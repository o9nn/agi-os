import logging
import os
import queue
import signal
import threading
import time
from collections import deque
from collections.abc import Generator
from concurrent.futures import Future
from contextlib import ExitStack, contextmanager
from inspect import isclass, signature
from typing import Any, Callable, Optional, TypeVar, Union
import msgspec
import zmq
from loguru import logger
from aphrodite.common.config import AphroditeConfig, ParallelConfig
from aphrodite.distributed import stateless_destroy_torch_distributed_process_group
from aphrodite.logging_utils.dump_input import dump_engine_exception
from aphrodite.lora.request import LoRARequest
from aphrodite.tasks import POOLING_TASKS, SupportedTask
from aphrodite.transformers_utils.config import maybe_register_config_serialize_by_value
from aphrodite.utils import decorate_logs, make_zmq_socket, resolve_obj_by_qualname, set_process_title
from aphrodite.v1.core.kv_cache_utils import get_kv_cache_config, unify_kv_cache_configs
from aphrodite.v1.core.sched.interface import SchedulerInterface
from aphrodite.v1.core.sched.output import SchedulerOutput
from aphrodite.v1.core.sched.scheduler import Scheduler as V1Scheduler
from aphrodite.v1.engine import EngineCoreOutputs, EngineCoreRequest, EngineCoreRequestType, ReconfigureDistributedRequest, ReconfigureRankType, UtilityOutput, UtilityResult
from aphrodite.v1.engine.mm_input_cache import MirroredProcessingCache
from aphrodite.v1.engine.utils import EngineHandshakeMetadata, EngineZmqAddresses
from aphrodite.v1.executor.abstract import Executor
from aphrodite.v1.kv_cache_interface import KVCacheConfig
from aphrodite.v1.metrics.stats import SchedulerStats
from aphrodite.v1.outputs import ModelRunnerOutput
from aphrodite.v1.request import Request, RequestStatus
from aphrodite.v1.serial_utils import MsgpackDecoder, MsgpackEncoder
from aphrodite.v1.structured_output import StructuredOutputManager
from aphrodite.version import __version__ as APHRODITE_VERSION
POLLING_TIMEOUT_S = 2.5
HANDSHAKE_TIMEOUT_MINS = 5
_R = TypeVar('_R')
class EngineCore:
    def __init__(self, aphrodite_config: AphroditeConfig, executor_class: type[Executor], log_stats: bool, executor_fail_callback: Optional[Callable]=None):
        from aphrodite.plugins import load_general_plugins
        load_general_plugins()
        self.aphrodite_config = aphrodite_config
        logger.debug('Initializing a V1 LLM engine (v{}) with config: {}', APHRODITE_VERSION, aphrodite_config)
        self.log_stats = log_stats
        self.model_executor = executor_class(aphrodite_config)
        if executor_fail_callback is not None:
            self.model_executor.register_failure_callback(executor_fail_callback)
        self.available_gpu_memory_for_kv_cache = -1
        num_gpu_blocks, num_cpu_blocks, kv_cache_config = self._initialize_kv_caches(aphrodite_config)
        aphrodite_config.cache_config.num_gpu_blocks = num_gpu_blocks
        aphrodite_config.cache_config.num_cpu_blocks = num_cpu_blocks
        self.collective_rpc('initialize_cache', args=(num_gpu_blocks, num_cpu_blocks))
        self.structured_output_manager = StructuredOutputManager(aphrodite_config)
        if isinstance(aphrodite_config.scheduler_config.scheduler_cls, str):
            Scheduler = resolve_obj_by_qualname(aphrodite_config.scheduler_config.scheduler_cls)
        else:
            Scheduler = aphrodite_config.scheduler_config.scheduler_cls
        if Scheduler is not V1Scheduler:
            logger.warning('Using configured V1 scheduler class {}. This scheduler interface is not public and compatibility may not be maintained.', aphrodite_config.scheduler_config.scheduler_cls)
        if len(kv_cache_config.kv_cache_groups) == 0:
            logger.info('Disabling chunked prefill for model without KVCache')
            aphrodite_config.scheduler_config.chunked_prefill_enabled = False
        self.scheduler: SchedulerInterface = Scheduler(aphrodite_config=aphrodite_config, kv_cache_config=kv_cache_config, structured_output_manager=self.structured_output_manager, include_finished_set=aphrodite_config.parallel_config.data_parallel_size > 1, log_stats=self.log_stats)
        self.mm_input_cache_server = MirroredProcessingCache(aphrodite_config.model_config)
        self.batch_queue_size = self.model_executor.max_concurrent_batches
        self.batch_queue: Optional[queue.Queue[tuple[Future[ModelRunnerOutput], SchedulerOutput]]] = None
        if self.batch_queue_size > 1:
            logger.info('Batch queue is enabled with size {}', self.batch_queue_size)
            self.batch_queue = queue.Queue(self.batch_queue_size)
    def _initialize_kv_caches(self, aphrodite_config: AphroditeConfig) -> tuple[int, int, KVCacheConfig]:
        start = time.time()
        kv_cache_specs = self.model_executor.get_kv_cache_specs()
        has_kv_cache = any((kv_cache_spec for kv_cache_spec in kv_cache_specs))
        if has_kv_cache:
            if os.environ.get('APHRODITE_ELASTIC_EP_SCALE_UP_LAUNCH') == '1':
                dp_group = getattr(self, 'dp_group', None)
                assert dp_group is not None
                self.available_gpu_memory_for_kv_cache = ParallelConfig.sync_kv_cache_memory_size(dp_group, -1)
                available_gpu_memory = [self.available_gpu_memory_for_kv_cache] * len(kv_cache_specs)
            else:
                available_gpu_memory = self.model_executor.determine_available_memory()
                self.available_gpu_memory_for_kv_cache = available_gpu_memory[0]
        else:
            available_gpu_memory = [0] * len(kv_cache_specs)
        assert len(kv_cache_specs) == len(available_gpu_memory)
        kv_cache_configs = [get_kv_cache_config(aphrodite_config, kv_cache_spec_one_worker, available_gpu_memory_one_worker) for kv_cache_spec_one_worker, available_gpu_memory_one_worker in zip(kv_cache_specs, available_gpu_memory)]
        unify_kv_cache_configs(kv_cache_configs)
        assert all([cfg.num_blocks == kv_cache_configs[0].num_blocks for cfg in kv_cache_configs])
        num_gpu_blocks = kv_cache_configs[0].num_blocks
        num_cpu_blocks = 0
        scheduler_kv_cache_config = kv_cache_configs[0]
        self.model_executor.initialize_from_config(kv_cache_configs)
        elapsed = time.time() - start
        logger.info('init engine (profile, create kv cache, warmup model) took {:.2f} seconds', elapsed)
        return (num_gpu_blocks, num_cpu_blocks, scheduler_kv_cache_config)
    def get_supported_tasks(self) -> tuple[SupportedTask, ...]:
        return self.model_executor.supported_tasks
    def add_request(self, request: Request, request_wave: int=0):
        if not isinstance(request.request_id, str):
            raise TypeError(f'request_id must be a string, got {type(request.request_id)}')
        if (pooling_params := request.pooling_params):
            supported_pooling_tasks = [task for task in self.get_supported_tasks() if task in POOLING_TASKS]
            if pooling_params.task not in supported_pooling_tasks:
                raise ValueError(f'Unsupported task: {pooling_params.task!r} Supported tasks: {supported_pooling_tasks}')
        if request.kv_transfer_params is not None and (not self.scheduler.get_kv_connector()):
            logger.warning('Got kv_transfer_params, but no KVConnector found. Disabling KVTransfer for this request.')
        self.scheduler.add_request(request)
    def abort_requests(self, request_ids: list[str]):
        self.scheduler.finish_requests(request_ids, RequestStatus.FINISHED_ABORTED)
    def execute_model_with_error_logging(self, model_fn: Callable[[SchedulerOutput], ModelRunnerOutput], scheduler_output: SchedulerOutput) -> ModelRunnerOutput:
        try:
            return model_fn(scheduler_output)
        except Exception as err:
            dump_engine_exception(self.aphrodite_config, scheduler_output, self.scheduler.make_stats())
            raise err
    def step(self) -> tuple[dict[int, EngineCoreOutputs], bool]:
        if not self.scheduler.has_requests():
            return ({}, False)
        scheduler_output = self.scheduler.schedule()
        model_output = self.execute_model_with_error_logging(self.model_executor.execute_model, scheduler_output)
        engine_core_outputs = self.scheduler.update_from_output(scheduler_output, model_output)
        return (engine_core_outputs, scheduler_output.total_num_scheduled_tokens > 0)
    def step_with_batch_queue(self) -> tuple[Optional[dict[int, EngineCoreOutputs]], bool]:
        assert self.batch_queue is not None
        engine_core_outputs = None
        scheduler_output = None
        if not self.batch_queue.full():
            scheduler_output = self.scheduler.schedule()
            if scheduler_output.total_num_scheduled_tokens > 0:
                future = self.model_executor.execute_model(scheduler_output)
                self.batch_queue.put_nowait((future, scheduler_output))
        scheduled_batch = scheduler_output is not None and scheduler_output.total_num_scheduled_tokens > 0
        if not scheduled_batch and (not self.batch_queue.empty()):
            future, scheduler_output = self.batch_queue.get_nowait()
            model_output = self.execute_model_with_error_logging(lambda _: future.result(), scheduler_output)
            self.batch_queue.task_done()
            engine_core_outputs = self.scheduler.update_from_output(scheduler_output, model_output)
        return (engine_core_outputs, scheduled_batch)
    def shutdown(self):
        self.structured_output_manager.clear_backend()
        if self.model_executor:
            self.model_executor.shutdown()
        if self.scheduler:
            self.scheduler.shutdown()
    def profile(self, is_start: bool=True):
        self.model_executor.profile(is_start)
    def reset_mm_cache(self):
        if self.scheduler.has_unfinished_requests():
            logger.warning('Resetting the multi-modal cache when requests are in progress may lead to desynced internal caches.')
        self.mm_input_cache_server.reset()
    def reset_prefix_cache(self):
        self.scheduler.reset_prefix_cache()
    def sleep(self, level: int=1):
        self.model_executor.sleep(level)
    def wake_up(self, tags: Optional[list[str]]=None):
        self.model_executor.wake_up(tags)
    def is_sleeping(self) -> bool:
        return self.model_executor.is_sleeping
    def execute_dummy_batch(self):
        self.model_executor.collective_rpc('execute_dummy_batch')
    def add_lora(self, lora_request: LoRARequest) -> bool:
        return self.model_executor.add_lora(lora_request)
    def remove_lora(self, lora_id: int) -> bool:
        return self.model_executor.remove_lora(lora_id)
    def list_loras(self) -> set[int]:
        return self.model_executor.list_loras()
    def pin_lora(self, lora_id: int) -> bool:
        return self.model_executor.pin_lora(lora_id)
    def save_sharded_state(self, path: str, pattern: Optional[str]=None, max_size: Optional[int]=None) -> None:
        self.model_executor.save_sharded_state(path=path, pattern=pattern, max_size=max_size)
    def collective_rpc(self, method: Union[str, Callable[..., _R]], timeout: Optional[float]=None, args: tuple=(), kwargs: Optional[dict[str, Any]]=None) -> list[_R]:
        return self.model_executor.collective_rpc(method, timeout, args, kwargs)
    def save_tensorized_model(self, tensorizer_config) -> None:
        self.model_executor.save_tensorized_model(tensorizer_config=tensorizer_config)
    def preprocess_add_request(self, request: EngineCoreRequest) -> tuple[Request, int]:
        if request.mm_hashes is not None:
            assert request.mm_inputs is not None
            request.mm_inputs = self.mm_input_cache_server.get_and_update_p1(request.mm_inputs, request.mm_hashes)
        req = Request.from_engine_core_request(request)
        if req.use_structured_output:
            self.structured_output_manager.grammar_init(req)
        return (req, request.current_wave)
class EngineCoreProc(EngineCore):
    ENGINE_CORE_DEAD = b'ENGINE_CORE_DEAD'
    def __init__(self, aphrodite_config: AphroditeConfig, local_client: bool, handshake_address: str, executor_class: type[Executor], log_stats: bool, client_handshake_address: Optional[str]=None, engine_index: int=0):
        self.input_queue = queue.Queue[tuple[EngineCoreRequestType, Any]]()
        self.output_queue = queue.Queue[Union[tuple[int, EngineCoreOutputs], bytes]]()
        executor_fail_callback = lambda: self.input_queue.put_nowait((EngineCoreRequestType.EXECUTOR_FAILED, b''))
        self.engine_index = engine_index
        identity = self.engine_index.to_bytes(length=2, byteorder='little')
        self.engines_running = False
        with self._perform_handshakes(handshake_address, identity, local_client, aphrodite_config, client_handshake_address) as addresses:
            self.client_count = len(addresses.outputs)
            self.has_coordinator = addresses.coordinator_output is not None
            self.frontend_stats_publish_address = addresses.frontend_stats_publish_address
            logger.debug('Has DP Coordinator: {}, stats publish address: {}', self.has_coordinator, self.frontend_stats_publish_address)
            self.publish_dp_lb_stats = self.has_coordinator and (not aphrodite_config.parallel_config.data_parallel_external_lb)
            self._init_data_parallel(aphrodite_config)
            super().__init__(aphrodite_config, executor_class, log_stats, executor_fail_callback)
            ready_event = threading.Event()
            input_thread = threading.Thread(target=self.process_input_sockets, args=(addresses.inputs, addresses.coordinator_input, identity, ready_event), daemon=True)
            input_thread.start()
            self.output_thread = threading.Thread(target=self.process_output_sockets, args=(addresses.outputs, addresses.coordinator_output, self.engine_index), daemon=True)
            self.output_thread.start()
            while not ready_event.wait(timeout=10):
                if not input_thread.is_alive():
                    raise RuntimeError('Input socket thread died during startup')
                assert addresses.coordinator_input is not None
                logger.info('Waiting for READY message from DP Coordinator...')
        self.step_fn = self.step if self.batch_queue is None else self.step_with_batch_queue
    @contextmanager
    def _perform_handshakes(self, handshake_address: str, identity: bytes, local_client: bool, aphrodite_config: AphroditeConfig, client_handshake_address: Optional[str]) -> Generator[EngineZmqAddresses, None, None]:
        input_ctx = zmq.Context()
        is_local = local_client and client_handshake_address is None
        headless = not local_client
        handshake = self._perform_handshake(input_ctx, handshake_address, identity, is_local, headless, aphrodite_config, aphrodite_config.parallel_config)
        if client_handshake_address is None:
            with handshake as addresses:
                yield addresses
        else:
            assert local_client
            local_handshake = self._perform_handshake(input_ctx, client_handshake_address, identity, True, False, aphrodite_config)
            with handshake as addresses, local_handshake as client_addresses:
                addresses.inputs = client_addresses.inputs
                addresses.outputs = client_addresses.outputs
                yield addresses
        aphrodite_config.__post_init__()
    @contextmanager
    def _perform_handshake(self, ctx: zmq.Context, handshake_address: str, identity: bytes, local_client: bool, headless: bool, aphrodite_config: AphroditeConfig, parallel_config_to_update: Optional[ParallelConfig]=None) -> Generator[EngineZmqAddresses, None, None]:
        with make_zmq_socket(ctx, handshake_address, zmq.DEALER, identity=identity, linger=5000, bind=False) as handshake_socket:
            addresses = self.startup_handshake(handshake_socket, local_client, headless, parallel_config_to_update)
            yield addresses
            num_gpu_blocks = aphrodite_config.cache_config.num_gpu_blocks
            dp_stats_address = self.frontend_stats_publish_address
            handshake_socket.send(msgspec.msgpack.encode({'status': 'READY', 'local': local_client, 'headless': headless, 'num_gpu_blocks': num_gpu_blocks, 'dp_stats_address': dp_stats_address}))
    @staticmethod
    def startup_handshake(handshake_socket: zmq.Socket, local_client: bool, headless: bool, parallel_config: Optional[ParallelConfig]=None) -> EngineZmqAddresses:
        handshake_socket.send(msgspec.msgpack.encode({'status': 'HELLO', 'local': local_client, 'headless': headless}))
        logger.info('Waiting for init message from front-end.')
        if not handshake_socket.poll(timeout=HANDSHAKE_TIMEOUT_MINS * 60000):
            raise RuntimeError(f'Did not receive response from front-end process within {HANDSHAKE_TIMEOUT_MINS} minutes')
        init_bytes = handshake_socket.recv()
        init_message: EngineHandshakeMetadata = msgspec.msgpack.decode(init_bytes, type=EngineHandshakeMetadata)
        logger.debug('Received init message: {}', init_message)
        if parallel_config is not None:
            for key, value in init_message.parallel_config.items():
                setattr(parallel_config, key, value)
        return init_message.addresses
    @staticmethod
    def run_engine_core(*args, dp_rank: int=0, local_dp_rank: int=0, **kwargs):
        shutdown_requested = False
        maybe_register_config_serialize_by_value()
        def signal_handler(signum, frame):
            nonlocal shutdown_requested
            if not shutdown_requested:
                shutdown_requested = True
                raise SystemExit()
        signal.signal(signal.SIGTERM, signal_handler)
        signal.signal(signal.SIGINT, signal_handler)
        engine_core: Optional[EngineCoreProc] = None
        try:
            parallel_config: ParallelConfig = kwargs['aphrodite_config'].parallel_config
            if parallel_config.data_parallel_size > 1 or dp_rank > 0:
                set_process_title('DPEngineCore', str(dp_rank))
                decorate_logs()
                parallel_config.data_parallel_rank = dp_rank
                parallel_config.data_parallel_rank_local = local_dp_rank
                engine_core = DPEngineCoreProc(*args, **kwargs)
            else:
                set_process_title('EngineCore')
                decorate_logs()
                engine_core = EngineCoreProc(*args, **kwargs)
            engine_core.run_busy_loop()
        except SystemExit:
            logger.debug('EngineCore exiting.')
            raise
        except Exception as e:
            if engine_core is None:
                logger.exception('EngineCore failed to start.')
            else:
                logger.exception('EngineCore encountered a fatal error.')
                engine_core._send_engine_dead()
            raise e
        finally:
            if engine_core is not None:
                engine_core.shutdown()
    def _init_data_parallel(self, aphrodite_config: AphroditeConfig):
        pass
    def run_busy_loop(self):
        while True:
            self._process_input_queue()
            self._process_engine_step()
    def _process_input_queue(self):
        waited = False
        while not self.engines_running and (not self.scheduler.has_requests()):
            if logging.getLogger().isEnabledFor(logging.DEBUG) and self.input_queue.empty():
                logger.debug('EngineCore waiting for work.')
                waited = True
            req = self.input_queue.get()
            self._handle_client_request(*req)
        if waited:
            logger.debug('EngineCore loop active.')
        while not self.input_queue.empty():
            req = self.input_queue.get_nowait()
            self._handle_client_request(*req)
    def _process_engine_step(self) -> bool:
        outputs, model_executed = self.step_fn()
        for output in outputs.items() if outputs else ():
            self.output_queue.put_nowait(output)
        return model_executed
    def _handle_client_request(self, request_type: EngineCoreRequestType, request: Any) -> None:
        if request_type == EngineCoreRequestType.ADD:
            req, request_wave = request
            self.add_request(req, request_wave)
        elif request_type == EngineCoreRequestType.ABORT:
            self.abort_requests(request)
        elif request_type == EngineCoreRequestType.UTILITY:
            client_idx, call_id, method_name, args = request
            output = UtilityOutput(call_id)
            try:
                method = getattr(self, method_name)
                result = method(*self._convert_msgspec_args(method, args))
                output.result = UtilityResult(result)
            except BaseException as e:
                logger.exception('Invocation of {} method failed', method_name)
                output.failure_message = f'Call to {method_name} method failed: {str(e)}'
            self.output_queue.put_nowait((client_idx, EngineCoreOutputs(utility_output=output)))
        elif request_type == EngineCoreRequestType.EXECUTOR_FAILED:
            raise RuntimeError('Executor failed.')
        else:
            logger.error('Unrecognized input request type encountered: {}', request_type)
    @staticmethod
    def _convert_msgspec_args(method, args):
        if not args:
            return args
        arg_types = signature(method).parameters.values()
        assert len(args) <= len(arg_types)
        return tuple((msgspec.convert(v, type=p.annotation) if isclass(p.annotation) and issubclass(p.annotation, msgspec.Struct) and (not isinstance(v, p.annotation)) else v for v, p in zip(args, arg_types)))
    def _send_engine_dead(self):
        self.output_queue.put_nowait(EngineCoreProc.ENGINE_CORE_DEAD)
        self.output_thread.join(timeout=5.0)
        if self.output_thread.is_alive():
            logger.fatal('Aphrodite shutdown signal from EngineCore failed to send. Please report this issue.')
    def process_input_sockets(self, input_addresses: list[str], coord_input_address: Optional[str], identity: bytes, ready_event: threading.Event):
        add_request_decoder = MsgpackDecoder(EngineCoreRequest)
        generic_decoder = MsgpackDecoder()
        with ExitStack() as stack, zmq.Context() as ctx:
            input_sockets = [stack.enter_context(make_zmq_socket(ctx, input_address, zmq.DEALER, identity=identity, bind=False)) for input_address in input_addresses]
            if coord_input_address is None:
                coord_socket = None
            else:
                coord_socket = stack.enter_context(make_zmq_socket(ctx, coord_input_address, zmq.XSUB, identity=identity, bind=False))
                coord_socket.send(b'\x01')
            poller = zmq.Poller()
            for input_socket in input_sockets:
                input_socket.send(b'')
                poller.register(input_socket, zmq.POLLIN)
            if coord_socket is not None:
                assert coord_socket.recv() == b'READY'
                poller.register(coord_socket, zmq.POLLIN)
            ready_event.set()
            del ready_event
            while True:
                for input_socket, _ in poller.poll():
                    type_frame, *data_frames = input_socket.recv_multipart(copy=False)
                    request_type = EngineCoreRequestType(bytes(type_frame.buffer))
                    if request_type == EngineCoreRequestType.ADD:
                        request = add_request_decoder.decode(data_frames)
                        request = self.preprocess_add_request(request)
                    else:
                        request = generic_decoder.decode(data_frames)
                    self.input_queue.put_nowait((request_type, request))
    def process_output_sockets(self, output_paths: list[str], coord_output_path: Optional[str], engine_index: int):
        encoder = MsgpackEncoder()
        reuse_buffers: list[bytearray] = []
        pending = deque[tuple[zmq.MessageTracker, Any, bytearray]]()
        with ExitStack() as stack, zmq.Context() as ctx:
            sockets = [stack.enter_context(make_zmq_socket(ctx, output_path, zmq.PUSH, linger=4000)) for output_path in output_paths]
            coord_socket = stack.enter_context(make_zmq_socket(ctx, coord_output_path, zmq.PUSH, bind=False, linger=4000)) if coord_output_path is not None else None
            max_reuse_bufs = len(sockets) + 1
            while True:
                output = self.output_queue.get()
                if output == EngineCoreProc.ENGINE_CORE_DEAD:
                    for socket in sockets:
                        socket.send(output)
                    break
                assert not isinstance(output, bytes)
                client_index, outputs = output
                outputs.engine_index = engine_index
                if client_index == -1:
                    assert coord_socket is not None
                    coord_socket.send_multipart(encoder.encode(outputs))
                    continue
                while pending and pending[-1][0].done:
                    reuse_buffers.append(pending.pop()[2])
                buffer = reuse_buffers.pop() if reuse_buffers else bytearray()
                buffers = encoder.encode_into(outputs, buffer)
                tracker = sockets[client_index].send_multipart(buffers, copy=False, track=True)
                if not tracker.done:
                    ref = outputs if len(buffers) > 1 else None
                    pending.appendleft((tracker, ref, buffer))
                elif len(reuse_buffers) < max_reuse_bufs:
                    reuse_buffers.append(buffer)
class DPEngineCoreProc(EngineCoreProc):
    def __init__(self, aphrodite_config: AphroditeConfig, local_client: bool, handshake_address: str, executor_class: type[Executor], log_stats: bool, client_handshake_address: Optional[str]=None):
        self.step_counter = 0
        self.current_wave = 0
        self.last_counts = (0, 0)
        dp_rank = aphrodite_config.parallel_config.data_parallel_rank
        super().__init__(aphrodite_config, local_client, handshake_address, executor_class, log_stats, client_handshake_address, dp_rank)
    def _init_data_parallel(self, aphrodite_config: AphroditeConfig):
        dp_rank = aphrodite_config.parallel_config.data_parallel_rank
        dp_size = aphrodite_config.parallel_config.data_parallel_size
        local_dp_rank = aphrodite_config.parallel_config.data_parallel_rank_local
        assert dp_size > 1
        assert 0 <= local_dp_rank <= dp_rank < dp_size
        if aphrodite_config.kv_transfer_config is not None:
            aphrodite_config.kv_transfer_config.engine_id = f'{aphrodite_config.kv_transfer_config.engine_id}_dp{local_dp_rank}'
            logger.debug('Setting kv_transfer_config.engine_id to {}', aphrodite_config.kv_transfer_config.engine_id)
        self.dp_rank = dp_rank
        self.dp_group = aphrodite_config.parallel_config.stateless_init_dp_group()
    def shutdown(self):
        super().shutdown()
        if (dp_group := getattr(self, 'dp_group', None)):
            stateless_destroy_torch_distributed_process_group(dp_group)
    def add_request(self, request: Request, request_wave: int=0):
        if self.has_coordinator and request_wave != self.current_wave:
            if request_wave > self.current_wave:
                self.current_wave = request_wave
            elif not self.engines_running:
                self.output_queue.put_nowait((-1, EngineCoreOutputs(start_wave=self.current_wave)))
        super().add_request(request, request_wave)
    def _handle_client_request(self, request_type: EngineCoreRequestType, request: Any) -> None:
        if request_type == EngineCoreRequestType.START_DP_WAVE:
            new_wave, exclude_eng_index = request
            if exclude_eng_index != self.engine_index and new_wave >= self.current_wave:
                self.current_wave = new_wave
                if not self.engines_running:
                    logger.debug('EngineCore starting idle loop for wave {}.', new_wave)
                    self.engines_running = True
        else:
            super()._handle_client_request(request_type, request)
    def _maybe_publish_request_counts(self):
        if not self.publish_dp_lb_stats:
            return
        counts = self.scheduler.get_request_counts()
        if counts != self.last_counts:
            self.last_counts = counts
            stats = SchedulerStats(*counts, step_counter=self.step_counter, current_wave=self.current_wave)
            self.output_queue.put_nowait((-1, EngineCoreOutputs(scheduler_stats=stats)))
    def run_busy_loop(self):
        while True:
            self._process_input_queue()
            executed = self._process_engine_step()
            self._maybe_publish_request_counts()
            local_unfinished_reqs = self.scheduler.has_unfinished_requests()
            if not executed:
                if not local_unfinished_reqs and (not self.engines_running):
                    continue
                self.execute_dummy_batch()
            self.engines_running = self._has_global_unfinished_reqs(local_unfinished_reqs)
            if not self.engines_running:
                if self.dp_rank == 0 or not self.has_coordinator:
                    logger.debug('Wave {} finished, pausing engine loop.', self.current_wave)
                    client_index = -1 if self.has_coordinator else 0
                    self.output_queue.put_nowait((client_index, EngineCoreOutputs(wave_complete=self.current_wave)))
                self.current_wave += 1
                self.step_counter = 0
    def _has_global_unfinished_reqs(self, local_unfinished: bool) -> bool:
        self.step_counter += 1
        if self.step_counter % 32 != 0:
            return True
        return ParallelConfig.has_unfinished_dp(self.dp_group, local_unfinished)
    def reinitialize_distributed(self, reconfig_request: ReconfigureDistributedRequest) -> None:
        stateless_destroy_torch_distributed_process_group(self.dp_group)
        self.shutdown()
        parallel_config = self.aphrodite_config.parallel_config
        old_dp_size = parallel_config.data_parallel_size
        parallel_config.data_parallel_size = reconfig_request.new_data_parallel_size
        if reconfig_request.new_data_parallel_rank != -1:
            parallel_config.data_parallel_rank = reconfig_request.new_data_parallel_rank
        assert reconfig_request.new_data_parallel_rank_local == ReconfigureRankType.KEEP_CURRENT_RANK
        parallel_config.data_parallel_master_ip = reconfig_request.new_data_parallel_master_ip
        parallel_config.data_parallel_master_port = reconfig_request.new_data_parallel_master_port
        if reconfig_request.new_data_parallel_rank != -2:
            self.dp_rank = parallel_config.data_parallel_rank
            self.dp_group = parallel_config.stateless_init_dp_group()
        reconfig_request.new_data_parallel_master_port = parallel_config.data_parallel_master_port
        self.model_executor.reinitialize_distributed(reconfig_request)
        if reconfig_request.new_data_parallel_size > old_dp_size:
            assert self.available_gpu_memory_for_kv_cache > 0
            ParallelConfig.sync_kv_cache_memory_size(self.dp_group, self.available_gpu_memory_for_kv_cache)
            self.model_executor.collective_rpc('compile_or_warm_up_model')
        if reconfig_request.new_data_parallel_rank == ReconfigureRankType.SHUTDOWN_CURRENT_RANK:
            self.shutdown()
            logger.info('DPEngineCoreProc {} shutdown', self.dp_rank)
        else:
            logger.info('Distributed environment reinitialized for DP rank {}', self.dp_rank)
class DPEngineCoreActor(DPEngineCoreProc):
    def __init__(self, aphrodite_config: AphroditeConfig, local_client: bool, addresses: EngineZmqAddresses, executor_class: type[Executor], log_stats: bool, dp_rank: int=0, local_dp_rank: int=0):
        self.addresses = addresses
        aphrodite_config.parallel_config.data_parallel_rank = dp_rank
        aphrodite_config.parallel_config.data_parallel_rank_local = local_dp_rank
        self._set_cuda_visible_devices(aphrodite_config, local_dp_rank)
        super().__init__(aphrodite_config, local_client, '', executor_class, log_stats)
    def _set_cuda_visible_devices(self, aphrodite_config: AphroditeConfig, local_dp_rank: int):
        from aphrodite.platforms import current_platform
        device_control_env_var = current_platform.device_control_env_var
        world_size = aphrodite_config.parallel_config.world_size
        try:
            os.environ[device_control_env_var] = ','.join((str(current_platform.device_id_to_physical_device_id(i)) for i in range(local_dp_rank * world_size, (local_dp_rank + 1) * world_size)))
        except IndexError as e:
            raise Exception(f'Error setting {device_control_env_var}: local range: [{local_dp_rank * world_size}, {(local_dp_rank + 1) * world_size}) base value: "{os.getenv(device_control_env_var)}"') from e
    @contextmanager
    def _perform_handshakes(self, handshake_address: str, identity: bytes, local_client: bool, aphrodite_config: AphroditeConfig, client_handshake_address: Optional[str]):
        yield self.addresses
    def wait_for_init(self):
        pass
    def run(self):
        try:
            self.run_busy_loop()
        except SystemExit:
            logger.debug('EngineCore exiting.')
            raise
        except Exception:
            logger.exception('EngineCore encountered a fatal error.')
            raise
        finally:
            self.shutdown()