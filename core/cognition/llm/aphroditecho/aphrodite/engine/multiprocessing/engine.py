import pickle
import signal
from contextlib import contextmanager
from typing import Iterator, List, Optional, Union
import cloudpickle
import zmq
from loguru import logger
from aphrodite import AsyncEngineArgs, SamplingParams
from aphrodite.common.config import AphroditeConfig
from aphrodite.common.outputs import RequestOutput
from aphrodite.utils import deprecate_kwargs
from aphrodite.engine.aphrodite_engine import AphroditeEngine
from aphrodite.engine.multiprocessing import APHRODITE_RPC_SUCCESS_STR, ENGINE_DEAD_ERROR, IPC_DATA_EXT, IPC_HEALTH_EXT, IPC_INPUT_EXT, IPC_OUTPUT_EXT, REQUEST_OUTPUTS_T, RPCAbortRequest, RPCAdapterLoadedResponse, RPCError, RPCIsSleepingRequest, RPCIsSleepingResponse, RPCLoadAdapterRequest, RPCProcessRequest, RPCResetMultiModalCacheRequest, RPCResetPrefixCacheRequest, RPCSleepRequest, RPCStartupRequest, RPCStartupResponse, RPCUProfileRequest, RPCWakeUpRequest
from aphrodite.transformers_utils.config import maybe_register_config_serialize_by_value
from aphrodite.usage.usage_lib import UsageContext
from aphrodite.worker.model_runner_base import InputProcessingError
POLLING_TIMEOUT_MS = 10000
HEALTHY_RESPONSE = (pickle.dumps(APHRODITE_RPC_SUCCESS_STR),)
class MQAphroditeEngine:
    def __init__(self, ipc_path: str, use_async_sockets: bool, *args, log_requests: bool=True, **kwargs) -> None:
        kwargs['use_cached_outputs'] = True
        self.engine = AphroditeEngine(*args, **kwargs)
        self.log_requests = log_requests
        self.use_async_sockets = use_async_sockets
        if self.use_async_sockets:
            self.engine.process_request_outputs_callback = self._async_socket_engine_callback
        self.ctx = zmq.Context()
        self.input_socket = self.ctx.socket(zmq.constants.PULL)
        self.input_socket.bind(f'{ipc_path}{IPC_INPUT_EXT}')
        self.output_socket = self.ctx.socket(zmq.constants.PUSH)
        self.output_socket.bind(f'{ipc_path}{IPC_OUTPUT_EXT}')
        self.heartbeat_socket = self.ctx.socket(zmq.constants.PUSH)
        self.heartbeat_socket.bind(f'{ipc_path}{IPC_HEALTH_EXT}')
        self.data_ipc_path = f'{ipc_path}{IPC_DATA_EXT}'
        self._errored_with: Optional[BaseException] = None
    @property
    def dead_error(self) -> BaseException:
        if self._errored_with is not None:
            return ENGINE_DEAD_ERROR(self._errored_with)
        else:
            return ENGINE_DEAD_ERROR()
    @classmethod
    @deprecate_kwargs('disable_log_requests', additional_message='This argument will have no effect. Use `enable_log_requests` instead.')
    def from_aphrodite_config(cls, aphrodite_config: AphroditeConfig, usage_context: UsageContext, enable_log_requests: bool, disable_log_stats: bool, ipc_path: str, disable_log_requests: bool=True) -> 'MQAphroditeEngine':
        from aphrodite.plugins import load_general_plugins
        load_general_plugins()
        use_async_sockets = aphrodite_config.model_config.use_async_output_proc
        return cls(aphrodite_config=aphrodite_config, executor_class=AphroditeEngine._get_executor_cls(aphrodite_config), ipc_path=ipc_path, usage_context=usage_context, use_async_sockets=use_async_sockets, log_requests=enable_log_requests, log_stats=not disable_log_stats)
    @staticmethod
    def from_engine_args(engine_args: AsyncEngineArgs, usage_context: UsageContext, ipc_path: str):
        aphrodite_config = engine_args.create_engine_config(usage_context)
        return MQAphroditeEngine.from_aphrodite_config(ipc_path=ipc_path, aphrodite_config=aphrodite_config, usage_context=usage_context, enable_log_requests=engine_args.enable_log_requests, disable_log_stats=engine_args.disable_log_stats)
    def start(self):
        try:
            try:
                logger.debug('Starting Startup Loop.')
                self.run_startup_loop()
                logger.debug('Starting Engine Loop.')
                self.run_engine_loop()
            except Exception as e:
                logger.exception(repr(e))
        except KeyboardInterrupt:
            logger.debug('Shutting down MQAphroditeEngine.')
        finally:
            logger.debug('MQAphroditeEngine is shut down.')
            self.cleanup()
    def cleanup(self):
        self.ctx.destroy(linger=0)
        del self.engine
    @contextmanager
    def make_data_socket(self) -> Iterator[zmq.Socket]:
        socket = self.ctx.socket(zmq.constants.ROUTER)
        try:
            socket.bind(self.data_ipc_path)
            yield socket
        finally:
            socket.close(linger=0)
    def run_startup_loop(self) -> None:
        with self.make_data_socket() as socket:
            response: Union[RPCStartupResponse, BaseException]
            try:
                identity, message = socket.recv_multipart(copy=False)
                request: RPCStartupRequest = pickle.loads(message.buffer)
                if request == RPCStartupRequest.IS_SERVER_READY:
                    tracing_enabled = self.engine.is_tracing_enabled()
                    response = RPCStartupResponse(tracing_enabled=tracing_enabled)
            except Exception as e:
                response = e
            socket.send_multipart((identity, pickle.dumps(response)), copy=False)
    def run_engine_loop(self):
        while True:
            if not self.engine.has_unfinished_requests():
                while self.input_socket.poll(timeout=POLLING_TIMEOUT_MS) == 0:
                    self._health_check()
                    self.engine.do_log_stats()
                    logger.debug('Waiting for new requests in engine loop.')
            self.handle_new_input()
            request_outputs = self.engine_step()
            if not self.use_async_sockets:
                self._send_outputs(request_outputs)
    def engine_step(self) -> List[RequestOutput]:
        try:
            return self.engine.step()
        except SystemExit:
            raise
        except InputProcessingError as e:
            rpc_err = RPCError(request_id=e.request_id, is_engine_errored=False, exception=e.__cause__)
            self._send_outputs(rpc_err)
            return []
        except BaseException as e:
            self._set_errored(e)
            rpc_err = RPCError(request_id=None, is_engine_errored=True, exception=e)
            self._send_outputs(rpc_err)
            raise e
    def handle_new_input(self):
        try:
            while self.input_socket.poll(timeout=0) != 0:
                frames = self.input_socket.recv_multipart(copy=False)
                request = pickle.loads(frames[0].buffer)
                if isinstance(request, RPCProcessRequest):
                    if len(frames) > 1:
                        assert isinstance(request.params, SamplingParams)
                        lprocs = cloudpickle.loads(frames[1].buffer)
                        request.params.logits_processors = lprocs
                    self._handle_process_request(request)
                elif isinstance(request, RPCAbortRequest):
                    self._handle_abort_request(request)
                elif isinstance(request, RPCUProfileRequest):
                    if request == RPCUProfileRequest.START_PROFILE:
                        self.start_profile()
                    else:
                        self.stop_profile()
                elif isinstance(request, RPCLoadAdapterRequest):
                    self._handle_load_adapter_request(request)
                elif isinstance(request, RPCResetMultiModalCacheRequest):
                    self.reset_mm_cache()
                elif isinstance(request, RPCResetPrefixCacheRequest):
                    self.reset_prefix_cache()
                elif isinstance(request, RPCSleepRequest):
                    self.sleep(request.value)
                elif isinstance(request, RPCWakeUpRequest):
                    self.wake_up(request.tags)
                elif isinstance(request, RPCIsSleepingRequest):
                    self._handle_is_sleeping_request(request)
                else:
                    raise ValueError(f'Unknown RPCRequest Type: {type(request)}')
        except Exception as e:
            self._set_errored(e)
            self._send_unhealthy(e)
            raise e from None
    def _handle_process_request(self, request: RPCProcessRequest):
        request_id = request.request_id
        if self._errored_with is not None:
            rpc_err = RPCError(request_id=request_id, is_engine_errored=True, exception=ENGINE_DEAD_ERROR(self._errored_with))
            self._send_outputs(rpc_err)
        try:
            self.engine.add_request(request_id=request_id, prompt=request.prompt, params=request.params, lora_request=request.lora_request, trace_headers=request.trace_headers, priority=request.priority)
            if self.log_requests:
                logger.info('Added request {}.', request.request_id)
        except Exception as e:
            logger.debug('Failed to add request {} to engine. {}', request.request_id, e)
            is_errored = self._errored_with is not None
            rpc_err = RPCError(request_id=request_id, is_engine_errored=is_errored, exception=e)
            self._send_outputs(rpc_err)
            self.engine.abort_request(request_id)
    def _handle_abort_request(self, request: RPCAbortRequest):
        self.engine.abort_request(request.request_id)
        if self.log_requests:
            logger.info('Aborted request {}.', request.request_id)
    def _handle_load_adapter_request(self, request: RPCLoadAdapterRequest):
        try:
            self.engine.add_lora(request.lora_request)
        except BaseException as e:
            rpc_err = RPCError(request_id=request.request_id, is_engine_errored=False, exception=e)
            self._send_outputs(rpc_err)
            return
        self._send_outputs(RPCAdapterLoadedResponse(request_id=request.request_id))
    def _handle_is_sleeping_request(self, request: RPCIsSleepingRequest):
        is_sleeping = self.is_sleeping()
        self._send_outputs(RPCIsSleepingResponse(request_id=request.request_id, is_sleeping=is_sleeping))
    def _health_check(self):
        if self._errored_with is not None:
            self._send_unhealthy(self._errored_with)
        try:
            self.engine.check_health()
            self._send_healthy()
        except Exception as e:
            self._set_errored(e)
            self._send_unhealthy(e)
    def _send_outputs(self, outputs: REQUEST_OUTPUTS_T):
        if outputs:
            try:
                from ray.exceptions import RayTaskError
                if isinstance(outputs, RPCError) and isinstance(outputs.exception, RayTaskError):
                    outputs.exception = outputs.exception.cause
            except ImportError:
                pass
            output_bytes = pickle.dumps(outputs)
            self.output_socket.send_multipart((output_bytes,), copy=False)
    def _send_healthy(self):
        if not self.heartbeat_socket.closed:
            self.heartbeat_socket.send_multipart(HEALTHY_RESPONSE, copy=False)
    def _send_unhealthy(self, error: BaseException):
        if not self.heartbeat_socket.closed:
            error_bytes = pickle.dumps(error)
            self.heartbeat_socket.send_multipart((error_bytes,), copy=False)
    def _async_socket_engine_callback(self, request_outputs: REQUEST_OUTPUTS_T):
        self._send_outputs(request_outputs)
        self.handle_new_input()
    def _set_errored(self, e: BaseException):
        if self._errored_with is None:
            self._errored_with = e
    def start_profile(self) -> None:
        self.engine.start_profile()
    def stop_profile(self) -> None:
        self.engine.stop_profile()
    def reset_mm_cache(self) -> bool:
        return self.engine.reset_mm_cache()
    def reset_prefix_cache(self) -> bool:
        return self.engine.reset_prefix_cache()
    def sleep(self, level: int=1) -> None:
        self.engine.sleep(level)
    def wake_up(self, tags: Optional[list[str]]=None) -> None:
        self.engine.wake_up(tags)
    def is_sleeping(self) -> bool:
        return self.engine.is_sleeping()
def signal_handler(*_) -> None:
    raise KeyboardInterrupt('MQAphroditeEngine terminated')
def run_mp_engine(aphrodite_config: AphroditeConfig, usage_context: UsageContext, ipc_path: str, disable_log_stats: bool, enable_log_requests: bool, engine_alive):
    try:
        maybe_register_config_serialize_by_value()
        engine = MQAphroditeEngine.from_aphrodite_config(aphrodite_config=aphrodite_config, usage_context=usage_context, disable_log_stats=disable_log_stats, enable_log_requests=enable_log_requests, ipc_path=ipc_path)
        signal.signal(signal.SIGTERM, signal_handler)
        engine.start()
    except BaseException as e:
        logger.exception(e)
        engine_alive.value = False
        raise e from None