import asyncio
import copy
import pickle
from contextlib import contextmanager, suppress
from typing import Any, AsyncGenerator, Dict, Iterator, List, Mapping, Optional, Union, cast
import cloudpickle
import psutil
import zmq
import zmq.asyncio
from loguru import logger
from zmq import Frame
from zmq.asyncio import Socket
from aphrodite import PoolingParams
from aphrodite.common.config import AphroditeConfig, DecodingConfig, ModelConfig
from aphrodite.common.envs import APHRODITE_RPC_TIMEOUT
from aphrodite.common.outputs import PoolingRequestOutput, RequestOutput
from aphrodite.common.sampling_params import SamplingParams
from aphrodite.utils import Device
from aphrodite.engine.multiprocessing import APHRODITE_RPC_SUCCESS_STR, ENGINE_DEAD_ERROR, IPC_DATA_EXT, IPC_HEALTH_EXT, IPC_INPUT_EXT, IPC_OUTPUT_EXT, RPC_REQUEST_T, RPCAbortRequest, RPCAdapterLoadedResponse, RPCError, RPCIsSleepingRequest, RPCIsSleepingResponse, RPCLoadAdapterRequest, RPCProcessRequest, RPCResetMultiModalCacheRequest, RPCResetPrefixCacheRequest, RPCSleepRequest, RPCStartupRequest, RPCStartupResponse, RPCUProfileRequest, RPCWakeUpRequest
from aphrodite.engine.protocol import EngineClient
from aphrodite.inputs import PromptType
from aphrodite.inputs.preprocess import InputPreprocessor
from aphrodite.lora.request import LoRARequest
from aphrodite.modeling.layers.sampler import SamplerOutput
from aphrodite.processing.scheduler import SchedulerOutputs
from aphrodite.transformers_utils.tokenizer_group import init_tokenizer_from_configs
class MQClientClosedError(Exception):
class MQAphroditeEngineClient(EngineClient):
    def __init__(self, ipc_path: str, engine_config: AphroditeConfig, engine_pid: int):
        self.context = zmq.asyncio.Context()
        self._errored_with: Optional[BaseException] = None
        self.aphrodite_config = engine_config
        self.model_config = engine_config.model_config
        self.decoding_config = engine_config.decoding_config
        if self.aphrodite_config.model_config.skip_tokenizer_init:
            self.tokenizer = None
        else:
            self.tokenizer = init_tokenizer_from_configs(model_config=self.model_config, scheduler_config=engine_config.scheduler_config, lora_config=engine_config.lora_config)
        self.input_preprocessor = InputPreprocessor(self.model_config, self.tokenizer)
        self.input_socket: Socket = self.context.socket(zmq.constants.PUSH)
        self.input_socket.connect(f'{ipc_path}{IPC_INPUT_EXT}')
        self.output_socket: Socket = self.context.socket(zmq.constants.PULL)
        self.output_socket.connect(f'{ipc_path}{IPC_OUTPUT_EXT}')
        self.heartbeat_socket: Socket = self.context.socket(zmq.constants.PULL)
        self.heartbeat_socket.connect(f'{ipc_path}{IPC_HEALTH_EXT}')
        self.data_ipc_path = f'{ipc_path}{IPC_DATA_EXT}'
        self.output_queues: Dict[str, asyncio.Queue] = {}
        self.output_loop: Optional[asyncio.Task] = None
        self.health_loop: Optional[asyncio.Task] = None
        self._engine_process = psutil.Process(engine_pid)
    @staticmethod
    def is_unsupported_config(aphrodite_config: AphroditeConfig):
        return aphrodite_config.parallel_config.pipeline_parallel_size > 1
    @contextmanager
    def get_data_socket(self) -> Iterator[Socket]:
        socket = self.context.socket(zmq.constants.DEALER)
        try:
            socket.connect(self.data_ipc_path)
            yield socket
        finally:
            socket.close(linger=0)
    async def run_heartbeat_loop(self, timeout: int):
        try:
            while True:
                if not self._engine_process.is_running() or self._engine_process.status() == psutil.STATUS_ZOMBIE:
                    self._set_errored(RuntimeError(f'Engine process (pid {self._engine_process.pid}) died.'))
                    break
                if await self.heartbeat_socket.poll(timeout=timeout):
                    await self._check_success(error_message='Heartbeat failed.', socket=self.heartbeat_socket)
                logger.debug('Heartbeat successful.')
        except asyncio.CancelledError:
            logger.debug('Shutting down MQAphroditeEngineClient check health loop.')
        except psutil.NoSuchProcess:
            self._set_errored(RuntimeError(f'Engine process (pid {self._engine_process.pid}) died.'))
        except Exception as e:
            self._set_errored(e)
    async def run_output_handler_loop(self):
        try:
            while True:
                while await self.output_socket.poll(timeout=APHRODITE_RPC_TIMEOUT) == 0:
                    logger.debug('Waiting for output from MQAphroditeEngine.')
                    if self.errored:
                        for queue_j in tuple(self.output_queues.values()):
                            queue_j.put_nowait(ENGINE_DEAD_ERROR(self._errored_with))
                        return
                message: Frame = await self.output_socket.recv(copy=False)
                request_outputs = pickle.loads(message.buffer)
                is_error = isinstance(request_outputs, (BaseException, RPCError))
                if is_error:
                    if isinstance(request_outputs, RPCError):
                        rpc_error: RPCError = request_outputs
                        request_id = rpc_error.request_id
                        exception = rpc_error.exception
                        is_engine_errored = rpc_error.is_engine_errored
                    else:
                        error: BaseException = request_outputs
                        logger.error('Received Exception {} rather than RPCError from MPAphroditeEngine. This should never happen.', error)
                        request_id = None
                        exception = error
                        is_engine_errored = True
                    if is_engine_errored and (not self._errored_with):
                        self._errored_with = exception
                        exception = self.dead_error
                    if request_id is None:
                        for queue_i in tuple(self.output_queues.values()):
                            queue_i.put_nowait(exception)
                    else:
                        queue = self.output_queues.get(request_id)
                        if queue is not None:
                            queue.put_nowait(exception)
                elif isinstance(request_outputs, (RPCAdapterLoadedResponse, RPCIsSleepingResponse)):
                    self._add_output(request_outputs)
                else:
                    for request_output in request_outputs:
                        self._add_output(request_output)
        except asyncio.CancelledError:
            logger.debug('Shutting down MQAphroditeEngineClient output handler.')
    def _add_output(self, request_output: Union[RequestOutput, RPCAdapterLoadedResponse, RPCIsSleepingResponse]):
        queue = self.output_queues.get(request_output.request_id)
        if queue is not None:
            queue.put_nowait(request_output)
    async def setup(self):
        if self.output_loop is None:
            self.output_loop = asyncio.create_task(self.run_output_handler_loop())
        with self.get_data_socket() as socket:
            response = await self._wait_for_server_rpc(socket)
            self.tracing_flag = response.tracing_enabled
            if self.health_loop is None:
                self.health_loop = asyncio.create_task(self.run_heartbeat_loop(timeout=APHRODITE_RPC_TIMEOUT))
    def close(self):
        self.context.destroy(linger=0)
        if self.health_loop is not None:
            self.health_loop.cancel()
        if self.output_loop is not None:
            self.output_loop.cancel()
    def _set_errored(self, e: BaseException):
        logger.exception(repr(e))
        if self._errored_with is None:
            self._errored_with = e
    @staticmethod
    async def _send_get_data_rpc_request(request: RPCStartupRequest, expected_type: Any, error_message: str, socket: Socket) -> Any:
        await socket.send_multipart((pickle.dumps(request),), copy=False)
        if await socket.poll(timeout=APHRODITE_RPC_TIMEOUT) == 0:
            raise TimeoutError(f"RPCServer didn't reply within {APHRODITE_RPC_TIMEOUT} ms")
        frame = await socket.recv(copy=False)
        data = pickle.loads(frame.buffer)
        if isinstance(data, BaseException):
            raise data
        elif not isinstance(data, expected_type):
            raise ValueError(error_message)
        return data
    @staticmethod
    async def _send_one_way_rpc_request(request: RPC_REQUEST_T, socket: Socket):
        if socket.closed:
            raise MQClientClosedError()
        await socket.send_multipart((pickle.dumps(request),))
    async def _await_ack(self, error_message: str, socket: Socket):
        if socket.closed:
            raise MQClientClosedError()
        if await socket.poll(timeout=APHRODITE_RPC_TIMEOUT) == 0:
            raise TimeoutError(f"MQAphroditeEngine didn't reply within {APHRODITE_RPC_TIMEOUT}ms")
        await self._check_success(error_message, socket)
    @staticmethod
    async def _check_success(error_message: str, socket: Socket):
        if socket.closed:
            raise MQClientClosedError()
        frame = await socket.recv(copy=False)
        response = pickle.loads(frame.buffer)
        if isinstance(response, BaseException):
            raise response
        elif not isinstance(response, str) or response != APHRODITE_RPC_SUCCESS_STR:
            raise ValueError(error_message)
    async def get_input_preprocessor(self) -> InputPreprocessor:
        return self.input_preprocessor
    async def get_tokenizer(self, lora_request: Optional[LoRARequest]=None):
        if self.tokenizer is None:
            return None
        else:
            return await self.tokenizer.get_lora_tokenizer_async(lora_request)
    async def get_aphrodite_config(self) -> AphroditeConfig:
        return self.aphrodite_config
    async def get_decoding_config(self) -> DecodingConfig:
        return self.decoding_config
    async def get_model_config(self) -> ModelConfig:
        return self.model_config
    async def is_tracing_enabled(self) -> bool:
        return self.tracing_flag
    async def _wait_for_server_rpc(self, socket: Socket) -> RPCStartupResponse:
        return await self._send_get_data_rpc_request(request=RPCStartupRequest.IS_SERVER_READY, expected_type=RPCStartupResponse, error_message='Unable to start RPC Server', socket=socket)
    async def abort(self, request_id: str):
        with suppress(MQClientClosedError):
            await self._send_one_way_rpc_request(request=RPCAbortRequest(request_id), socket=self.input_socket)
    async def do_log_stats(self, scheduler_outputs: Optional[SchedulerOutputs]=None, model_output: Optional[List[SamplerOutput]]=None) -> None:
        pass
    async def check_health(self):
        if self._errored_with is not None:
            raise self._errored_with
    @property
    def is_running(self) -> bool:
        return not self.errored
    @property
    def is_stopped(self) -> bool:
        return self.errored
    @property
    def errored(self) -> bool:
        return self._errored_with is not None
    @property
    def dead_error(self) -> BaseException:
        return ENGINE_DEAD_ERROR(self._errored_with)
    def generate(self, prompt: PromptType, sampling_params: SamplingParams, request_id: str, lora_request: Optional[LoRARequest]=None, trace_headers: Optional[Mapping[str, str]]=None, priority: int=0) -> AsyncGenerator[RequestOutput, None]:
        return cast(AsyncGenerator[RequestOutput, None], self._process_request(prompt, sampling_params, request_id, lora_request, trace_headers, priority))
    def encode(self, prompt: PromptType, pooling_params: PoolingParams, request_id: str, lora_request: Optional[LoRARequest]=None, trace_headers: Optional[Mapping[str, str]]=None, priority: int=0) -> AsyncGenerator[PoolingRequestOutput, None]:
        return cast(AsyncGenerator[PoolingRequestOutput, None], self._process_request(prompt, pooling_params, request_id, lora_request, trace_headers, priority=priority))
    async def _process_request(self, prompt: PromptType, params: Union[SamplingParams, PoolingParams], request_id: str, lora_request: Optional[LoRARequest]=None, trace_headers: Optional[Mapping[str, str]]=None, priority: int=0) -> Union[AsyncGenerator[RequestOutput, None], AsyncGenerator[PoolingRequestOutput, None]]:
        if self._errored_with is not None:
            raise ENGINE_DEAD_ERROR(self._errored_with)
        if request_id in self.output_queues:
            raise ValueError(f'Request {request_id} already exists')
        queue: asyncio.Queue[Union[RequestOutput, BaseException]] = asyncio.Queue()
        self.output_queues[request_id] = queue
        try:
            if isinstance(params, SamplingParams) and params.logits_processors:
                params = copy.copy(params)
                logits_processors = params.logits_processors
                params.logits_processors = None
                lp_bytes = cloudpickle.dumps(logits_processors)
            else:
                lp_bytes = None
            request_bytes = pickle.dumps(RPCProcessRequest(prompt=prompt, params=params, request_id=request_id, lora_request=lora_request, trace_headers=trace_headers, priority=priority))
            parts = (request_bytes, lp_bytes) if lp_bytes else (request_bytes,)
            await self.input_socket.send_multipart(parts, copy=False)
            finished = False
            try:
                while not finished:
                    request_output = await queue.get()
                    if isinstance(request_output, BaseException):
                        raise request_output
                    finished = request_output.finished
                    yield request_output
            finally:
                if not finished and (not self.errored):
                    await self.abort(request_id)
        finally:
            self.output_queues.pop(request_id)
    async def start_profile(self) -> None:
        await self._send_one_way_rpc_request(request=RPCUProfileRequest.START_PROFILE, socket=self.input_socket)
    async def stop_profile(self) -> None:
        await self._send_one_way_rpc_request(request=RPCUProfileRequest.STOP_PROFILE, socket=self.input_socket)
    async def reset_mm_cache(self) -> None:
        await self._send_one_way_rpc_request(request=RPCResetMultiModalCacheRequest.RESET, socket=self.input_socket)
    async def reset_prefix_cache(self, device: Optional[Device]=None) -> None:
        await self._send_one_way_rpc_request(request=RPCResetPrefixCacheRequest(device), socket=self.input_socket)
    async def sleep(self, level: int=1) -> None:
        return await self._send_one_way_rpc_request(request=RPCSleepRequest(level), socket=self.input_socket)
    async def wake_up(self, tags: Optional[list[str]]=None) -> None:
        return await self._send_one_way_rpc_request(request=RPCWakeUpRequest(tags), socket=self.input_socket)
    async def is_sleeping(self) -> bool:
        request = RPCIsSleepingRequest()
        queue: asyncio.Queue[Union[BaseException, RPCIsSleepingResponse]] = asyncio.Queue()
        self.output_queues[request.request_id] = queue
        request_bytes = pickle.dumps(request)
        await self.input_socket.send_multipart((request_bytes,), copy=False)
        request_output = await queue.get()
        self.output_queues.pop(request.request_id)
        if isinstance(request_output, BaseException):
            raise request_output
        return request_output.is_sleeping
    async def add_lora(self, lora_request: LoRARequest) -> None:
        request = RPCLoadAdapterRequest(lora_request)
        queue: asyncio.Queue[Union[None, BaseException]] = asyncio.Queue()
        self.output_queues[request.request_id] = queue
        request_bytes = pickle.dumps(request)
        await self.input_socket.send_multipart((request_bytes,), copy=False)
        request_output = await queue.get()
        self.output_queues.pop(request.request_id)
        if isinstance(request_output, BaseException):
            raise request_output