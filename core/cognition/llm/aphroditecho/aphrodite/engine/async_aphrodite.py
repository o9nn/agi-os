import asyncio
import time
import weakref
from functools import partial
from typing import Any, AsyncGenerator, Callable, Dict, Iterable, List, Mapping, Optional, Set, Tuple, Type, Union
from weakref import ReferenceType
from loguru import logger
import aphrodite.common.envs as envs
from aphrodite.common.config import AphroditeConfig, DecodingConfig, LoRAConfig, ModelConfig, ParallelConfig, SchedulerConfig
from aphrodite.common.outputs import PoolingRequestOutput, RequestOutput
from aphrodite.common.pooling_params import PoolingParams
from aphrodite.common.sampling_params import SamplingParams
from aphrodite.common.sequence import ExecuteModelRequest
from aphrodite.utils import Device, deprecate_kwargs, weak_bind
from aphrodite.engine.aphrodite_engine import AphroditeEngine, SchedulerOutputState
from aphrodite.engine.args_tools import AsyncEngineArgs
from aphrodite.engine.async_timeout import asyncio_timeout
from aphrodite.engine.metrics_types import StatLoggerBase
from aphrodite.engine.protocol import EngineClient
from aphrodite.executor.executor_base import ExecutorBase
from aphrodite.inputs import PromptType
from aphrodite.inputs.preprocess import InputPreprocessor
from aphrodite.lora.request import LoRARequest
from aphrodite.modeling.layers.sampler import SamplerOutput
from aphrodite.processing.scheduler import SchedulerOutputs
from aphrodite.transformers_utils.tokenizer import AnyTokenizer
from aphrodite.usage.usage_lib import UsageContext
ENGINE_ITERATION_TIMEOUT_S = envs.APHRODITE_ENGINE_ITERATION_TIMEOUT_S
class AsyncEngineDeadError(RuntimeError):
    pass
def _log_task_completion(task: asyncio.Task, error_callback: Callable[[Exception], None]) -> None:
    exception = None
    try:
        return_value = task.result()
        raise AssertionError(f'The engine background task should never finish without an exception. {return_value}')
    except asyncio.exceptions.CancelledError:
        logger.info('Engine is gracefully shutting down.')
    except Exception as e:
        exception = e
        logger.error('Engine background task failed', exc_info=e)
        error_callback(exception)
        raise AsyncEngineDeadError('Task finished unexpectedly. This should never happen! Please open an issue on GitHub. See stack trace above for the actual cause.') from e
STOP_ITERATION = Exception()
class AsyncStream:
    def __init__(self, request_id: str, cancel: Callable[[str], None]) -> None:
        self.request_id = request_id
        self._cancel = cancel
        self._queue: asyncio.Queue = asyncio.Queue()
        self._finished = False
    def put(self, item: Union[RequestOutput, PoolingRequestOutput, Exception]) -> None:
        if not self._finished:
            self._queue.put_nowait(item)
    def finish(self, exception: Optional[Union[BaseException, Type[BaseException]]]=None) -> None:
        if not self._finished:
            self._finished = True
            self._queue.put_nowait(exception if self._is_raisable(exception) else STOP_ITERATION)
    @property
    def finished(self) -> bool:
        return self._finished
    async def generator(self) -> AsyncGenerator[Union[RequestOutput, PoolingRequestOutput], None]:
        try:
            while True:
                result = await self._queue.get()
                if self._is_raisable(result):
                    if result == STOP_ITERATION:
                        return
                    raise result
                yield result
        except GeneratorExit:
            self._cancel(self.request_id)
            raise asyncio.CancelledError from None
    @staticmethod
    def _is_raisable(value: Any):
        return isinstance(value, BaseException) or (isinstance(value, type) and issubclass(value, BaseException))
class RequestTracker:
    def __init__(self) -> None:
        self._request_streams: Dict[str, AsyncStream] = {}
        self._aborted_requests: asyncio.Queue[str] = asyncio.Queue()
        self._new_requests: asyncio.Queue[Tuple[AsyncStream, dict]] = asyncio.Queue()
        self.new_requests_event = asyncio.Event()
    def __contains__(self, item):
        return item in self._request_streams
    def __len__(self) -> int:
        return len(self._request_streams)
    def propagate_exception(self, exc: Exception, request_id: Optional[str]=None) -> None:
        if request_id is not None:
            self.abort_request(request_id, exception=exc)
        else:
            for rid in tuple(self._request_streams.keys()):
                self.abort_request(rid, exception=exc)
    def process_request_output(self, request_output: Union[RequestOutput, PoolingRequestOutput], *, verbose: bool=False) -> None:
        request_id = request_output.request_id
        finished = request_output.finished
        if finished:
            stream = self._request_streams.pop(request_id, None)
        else:
            stream = self._request_streams.get(request_id)
        if stream is not None:
            stream.put(request_output)
            if finished:
                stream.finish()
        if verbose and finished:
            logger.info('Finished request {}.', request_id)
    def process_exception(self, request_id: str, exception: BaseException, *, verbose: bool=False) -> None:
        if verbose:
            logger.info('Finished request {}.', request_id)
        self.abort_request(request_id, exception=exception)
    def add_request(self, request_id: str, *, verbose: bool=False, **engine_add_request_kwargs) -> AsyncStream:
        if request_id in self._request_streams:
            raise KeyError(f'Request {request_id} already exists.')
        abort_request = partial(self.abort_request, verbose=verbose)
        stream = AsyncStream(request_id, abort_request)
        self._new_requests.put_nowait((stream, {'request_id': request_id, **engine_add_request_kwargs}))
        self.new_requests_event.set()
        if verbose:
            logger.info('Added request {}.', request_id)
        return stream
    def abort_request(self, request_id: str, *, exception: Optional[Union[BaseException, Type[BaseException]]]=None, verbose: bool=False) -> None:
        if verbose:
            logger.info('Aborted request {}.', request_id)
        self._aborted_requests.put_nowait(request_id)
        stream = self._request_streams.pop(request_id, None)
        if stream is not None:
            stream.finish(exception=exception)
    def get_new_and_aborted_requests(self) -> Tuple[List[Dict], Set[str]]:
        new_requests: List[Dict] = []
        finished_requests: Set[str] = set()
        while not self._aborted_requests.empty():
            request_id = self._aborted_requests.get_nowait()
            finished_requests.add(request_id)
        while not self._new_requests.empty():
            stream, new_request = self._new_requests.get_nowait()
            request_id = stream.request_id
            if request_id in finished_requests:
                stream.finish(asyncio.CancelledError)
                finished_requests.discard(request_id)
            else:
                self._request_streams[request_id] = stream
                new_requests.append(new_request)
        return (new_requests, finished_requests)
    async def wait_for_new_requests(self):
        if not self.has_new_requests():
            await self.new_requests_event.wait()
        self.new_requests_event.clear()
    def has_new_requests(self):
        return not self._new_requests.empty()
class _AsyncLLMEngine(AphroditeEngine):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
    async def step_async(self, virtual_engine: int) -> List[Union[RequestOutput, PoolingRequestOutput]]:
        cached_outputs = self.cached_scheduler_outputs[virtual_engine]
        seq_group_metadata_list = cached_outputs.seq_group_metadata_list
        scheduler_outputs = cached_outputs.scheduler_outputs
        allow_async_output_proc = cached_outputs.allow_async_output_proc
        ctx = self.scheduler_contexts[virtual_engine]
        ctx.request_outputs.clear()
        if not self._has_remaining_steps(seq_group_metadata_list):
            seq_group_metadata_list, scheduler_outputs, allow_async_output_proc = self.scheduler[virtual_engine].schedule()
            ctx.seq_group_metadata_list = seq_group_metadata_list
            ctx.scheduler_outputs = scheduler_outputs
            if not scheduler_outputs.is_empty():
                finished_requests_ids = self.scheduler[virtual_engine].get_and_reset_finished_requests_ids()
            if not allow_async_output_proc and len(ctx.output_queue) > 0:
                self._process_model_outputs(ctx=ctx)
            if self.scheduler_config.is_multi_step and scheduler_outputs.num_lookahead_slots > 0:
                self._cache_scheduler_outputs_for_multi_step(virtual_engine, seq_group_metadata_list, scheduler_outputs, allow_async_output_proc)
        else:
            finished_requests_ids = list()
        assert seq_group_metadata_list is not None
        assert scheduler_outputs is not None
        if not scheduler_outputs.is_empty():
            last_sampled_token_ids = self._get_last_sampled_token_ids(virtual_engine)
            execute_model_req = ExecuteModelRequest(seq_group_metadata_list=seq_group_metadata_list, blocks_to_swap_in=scheduler_outputs.blocks_to_swap_in, blocks_to_swap_out=scheduler_outputs.blocks_to_swap_out, blocks_to_copy=scheduler_outputs.blocks_to_copy, virtual_engine=virtual_engine, num_lookahead_slots=scheduler_outputs.num_lookahead_slots, running_queue_size=scheduler_outputs.running_queue_size, finished_requests_ids=finished_requests_ids, last_sampled_token_ids=last_sampled_token_ids)
            if allow_async_output_proc:
                execute_model_req.async_callback = self.async_callbacks[virtual_engine]
            outputs = await self.model_executor.execute_model_async(execute_model_req)
            if self.scheduler_config.is_multi_step:
                self._update_cached_scheduler_output(virtual_engine, outputs)
        else:
            if len(ctx.output_queue) > 0:
                self._process_model_outputs(ctx=ctx)
            outputs = []
        if self.scheduler_config.is_multi_step:
            for seq_group in seq_group_metadata_list:
                seq_group.finish_step()
        if not self._has_remaining_steps(seq_group_metadata_list):
            if self.scheduler_config.is_multi_step:
                self.cached_scheduler_outputs[virtual_engine] = SchedulerOutputState()
            is_first_step_output: bool = False if not seq_group_metadata_list else seq_group_metadata_list[0].state.num_steps == 1
            ctx.append_output(outputs=outputs, seq_group_metadata_list=seq_group_metadata_list, scheduler_outputs=scheduler_outputs, is_async=allow_async_output_proc, is_last_step=True, is_first_step_output=is_first_step_output)
            if outputs and allow_async_output_proc:
                assert len(outputs) == 1, 'Async postprocessor expects only a single output set'
                self._advance_to_next_step(outputs[0], seq_group_metadata_list, scheduler_outputs.scheduled_seq_groups)
            if not allow_async_output_proc:
                self._process_model_outputs(ctx=ctx)
                self.do_log_stats(scheduler_outputs, outputs)
                self.do_tracing(scheduler_outputs)
        else:
            return ctx.request_outputs
        if not self.has_unfinished_requests():
            if len(ctx.output_queue) > 0:
                self._process_model_outputs(ctx=ctx)
            assert len(ctx.output_queue) == 0
        return ctx.request_outputs
    async def stop_remote_worker_execution_loop_async(self) -> None:
        await self.model_executor.stop_remote_worker_execution_loop_async()
    async def get_tokenizer_async(self, lora_request: Optional[LoRARequest]=None) -> AnyTokenizer:
        return await self.get_tokenizer_group().get_lora_tokenizer_async(lora_request)
    async def add_request_async(self, request_id: str, prompt: PromptType, params: Union[SamplingParams, PoolingParams], arrival_time: Optional[float]=None, lora_request: Optional[LoRARequest]=None, trace_headers: Optional[Mapping[str, str]]=None, priority: int=0, data_parallel_rank: Optional[int]=None, tokenization_kwargs: Optional[dict[str, Any]]=None) -> None:
        if lora_request is not None and (not self.lora_config):
            raise ValueError(f'Got lora_request {lora_request} but LoRA is not enabled!')
        if priority != 0 and (not self.scheduler_config.policy == 'priority'):
            raise ValueError(f'Got priority {priority} but Priority scheduling is not enabled.')
        if arrival_time is None:
            arrival_time = time.time()
        if data_parallel_rank is not None:
            raise ValueError('Targeting data_parallel_rank only supported in v1 client.')
        if isinstance(prompt, dict) and prompt.get('prompt_embeds', None) is not None and (not prompt.get('prompt_token_ids', None)):
            prompt['prompt_token_ids'] = [0] * prompt['prompt_embeds'].shape[-2]
        processed_inputs = await self.input_preprocessor.preprocess_async(prompt, lora_request=lora_request, tokenization_kwargs=tokenization_kwargs)
        self._add_processed_request(request_id=request_id, processed_inputs=processed_inputs, params=params, arrival_time=arrival_time, lora_request=lora_request, trace_headers=trace_headers, priority=priority)
    async def check_health_async(self) -> None:
        self.model_executor.check_health()
    async def collective_rpc_async(self, method: str, timeout: Optional[float]=None, args: tuple=(), kwargs: Optional[dict]=None):
        raise NotImplementedError
class AsyncAphrodite(EngineClient):
    _engine_class: Type[_AsyncLLMEngine] = _AsyncLLMEngine
    def __init__(self, *args, log_requests: bool=True, start_engine_loop: bool=True, **kwargs) -> None:
        if envs.APHRODITE_USE_V1:
            raise ValueError('Using V0 AsyncAphrodite, but envs.APHRODITE_USE_V1=True. This should not happen. As a workaround, try using AsyncAphrodite.from_aphrodite_config(...) or explicitly set APHRODITE_USE_V1=0 or 1 and report this issue on Github.')
        self.log_requests = log_requests
        self.engine = self._engine_class(*args, **kwargs)
        self.use_process_request_outputs_callback = self.engine.model_config.use_async_output_proc
        if self.use_process_request_outputs_callback:
            self.engine.process_request_outputs_callback = weak_bind(self.process_request_outputs)
        self.background_loop: Optional[asyncio.Future] = None
        self._background_loop_unshielded: Optional[asyncio.Task] = None
        self.start_engine_loop = start_engine_loop
        self._errored_with: Optional[BaseException] = None
        self._request_tracker: RequestTracker
    def __del__(self):
        if (rt := getattr(self, 'request_tracker', None)):
            rt.new_requests_event.set()
    @classmethod
    def _get_executor_cls(cls, engine_config: AphroditeConfig) -> Type[ExecutorBase]:
        return AphroditeEngine._get_executor_cls(engine_config)
    @classmethod
    @deprecate_kwargs('disable_log_requests', additional_message='This argument will have no effect. Use `enable_log_requests` instead.')
    def from_aphrodite_config(cls, aphrodite_config: AphroditeConfig, start_engine_loop: bool=True, usage_context: UsageContext=UsageContext.ENGINE_CONTEXT, stat_loggers: Optional[dict[str, StatLoggerBase]]=None, enable_log_requests: bool=False, disable_log_stats: bool=False, disable_log_requests: bool=True) -> 'AsyncAphrodite':
        return cls(aphrodite_config=aphrodite_config, executor_class=cls._get_executor_cls(aphrodite_config), start_engine_loop=start_engine_loop, log_requests=enable_log_requests, log_stats=not disable_log_stats, usage_context=usage_context, stat_loggers=stat_loggers)
    @classmethod
    def from_engine_args(cls, engine_args: AsyncEngineArgs, start_engine_loop: bool=True, usage_context: UsageContext=UsageContext.ENGINE_CONTEXT, stat_loggers: Optional[Dict[str, StatLoggerBase]]=None) -> 'AsyncAphrodite':
        aphrodite_config = engine_args.create_engine_config(usage_context)
        async_engine_cls = cls
        if envs.APHRODITE_USE_V1:
            from aphrodite.v1.engine.async_llm import AsyncLLM as V1AsyncLLMEngine
            async_engine_cls = V1AsyncLLMEngine
        return async_engine_cls.from_aphrodite_config(aphrodite_config=aphrodite_config, start_engine_loop=start_engine_loop, usage_context=usage_context, stat_loggers=stat_loggers, disable_log_stats=engine_args.disable_log_stats, enable_log_requests=engine_args.enable_log_requests)
    @property
    def is_running(self) -> bool:
        return self.background_loop is not None and self._background_loop_unshielded is not None and (not self._background_loop_unshielded.done())
    @property
    def is_stopped(self) -> bool:
        return self.errored or (self.background_loop is not None and self._background_loop_unshielded is not None and self._background_loop_unshielded.done())
    @property
    def errored(self) -> bool:
        return self._errored_with is not None
    @property
    def dead_error(self) -> BaseException:
        return AsyncEngineDeadError('Background loop is not running. If it was running, inspect the output to find the stacktrace of the error that caused the background loop to stop (AsyncEngineDeadError).')
    def set_errored(self, exc: Exception) -> None:
        self._errored_with = exc
    def _error_callback(self, exc: Exception) -> None:
        self.set_errored(exc)
        self._request_tracker.propagate_exception(exc)
    async def get_input_preprocessor(self) -> InputPreprocessor:
        return self.engine.input_preprocessor
    async def get_tokenizer(self, lora_request: Optional[LoRARequest]=None) -> AnyTokenizer:
        return await self.engine.get_tokenizer_async(lora_request)
    def start_background_loop(self) -> None:
        if self.errored:
            raise AsyncEngineDeadError('Background loop has errored already.') from self._errored_with
        if self.is_running:
            raise RuntimeError('Background loop is already running.')
        self._request_tracker = RequestTracker()
        self._background_loop_unshielded = asyncio.get_event_loop().create_task(self.run_engine_loop(weakref.ref(self)))
        self._background_loop_unshielded.add_done_callback(partial(_log_task_completion, error_callback=self._error_callback))
        self.background_loop = asyncio.shield(self._background_loop_unshielded)
    def shutdown_background_loop(self) -> None:
        if self._background_loop_unshielded is not None:
            self._background_loop_unshielded.cancel()
            self._background_loop_unshielded = None
        self.background_loop = None
    async def engine_step(self, virtual_engine: int) -> bool:
        new_requests, aborted_requests = self._request_tracker.get_new_and_aborted_requests()
        for new_request in new_requests:
            try:
                await self.engine.add_request_async(**new_request)
            except ValueError as e:
                self._request_tracker.process_exception(new_request['request_id'], e, verbose=self.log_requests)
        if aborted_requests:
            await self._engine_abort(aborted_requests)
        request_outputs = await self.engine.step_async(virtual_engine)
        if not self.use_process_request_outputs_callback:
            all_finished = self.process_request_outputs(request_outputs)
        else:
            all_finished = all((request_output.finished for request_output in request_outputs))
        return not all_finished
    def process_request_outputs(self, request_outputs) -> bool:
        all_finished = True
        for request_output in request_outputs:
            self._request_tracker.process_request_output(request_output, verbose=self.log_requests)
            all_finished = all_finished and request_output.finished
        return all_finished
    async def _engine_abort(self, request_ids: Iterable[str]):
        self.engine.abort_request(request_ids)
    @staticmethod
    async def run_engine_loop(engine_ref: ReferenceType):
        engine: Optional[AsyncAphrodite] = engine_ref()
        if not engine:
            return
        pipeline_parallel_size = engine.engine.parallel_config.pipeline_parallel_size
        has_requests_in_progress = [False] * pipeline_parallel_size
        while True:
            if not any(has_requests_in_progress):
                logger.debug('Waiting for new requests...')
                await engine.engine.stop_remote_worker_execution_loop_async()
                request_tracker = engine._request_tracker
                del engine
                await asyncio.sleep(0)
                if engine_ref() is None:
                    return
                await request_tracker.wait_for_new_requests()
                engine = engine_ref()
                if not engine:
                    return
                logger.debug('Got new requests!')
                requests_in_progress = [asyncio.create_task(engine.engine_step(ve)) for ve in range(pipeline_parallel_size)]
                has_requests_in_progress = [True] * pipeline_parallel_size
            try:
                async with asyncio_timeout(ENGINE_ITERATION_TIMEOUT_S):
                    done, _ = await asyncio.wait(requests_in_progress, return_when=asyncio.FIRST_COMPLETED)
                    for _ in range(pipeline_parallel_size):
                        await asyncio.sleep(0)
                for task in done:
                    result = task.result()
                    virtual_engine = requests_in_progress.index(task)
                    has_unfinished_requests = engine.engine.has_unfinished_requests_for_virtual_engine(virtual_engine)
                    if result or has_unfinished_requests:
                        requests_in_progress[virtual_engine] = asyncio.create_task(engine.engine_step(virtual_engine))
                        has_requests_in_progress[virtual_engine] = True
                    else:
                        has_requests_in_progress[virtual_engine] = False
            except asyncio.TimeoutError as exc:
                logger.error('Engine iteration timed out. This should never happen!')
                engine.set_errored(exc)
                raise
            await asyncio.sleep(0)
    async def add_request(self, request_id: str, prompt: PromptType, params: Union[SamplingParams, PoolingParams], arrival_time: Optional[float]=None, lora_request: Optional[LoRARequest]=None, trace_headers: Optional[Mapping[str, str]]=None, priority: int=0, data_parallel_rank: Optional[int]=None, tokenization_kwargs: Optional[dict[str, Any]]=None) -> AsyncGenerator[Union[RequestOutput, PoolingRequestOutput], None]:
        if not self.is_running:
            if self.start_engine_loop:
                self.start_background_loop()
            else:
                raise AsyncEngineDeadError('Background loop is not running. If it was running, inspect the output to find the stacktrace of the error that caused the background loop to stop (AsyncEngineDeadError).')
        if priority != 0 and (not self.engine.scheduler_config.policy == 'priority'):
            raise ValueError(f'Got priority {priority} but Priority scheduling is not enabled.')
        stream = self._request_tracker.add_request(request_id, verbose=self.log_requests, prompt=prompt, params=params, arrival_time=arrival_time or time.time(), lora_request=lora_request, trace_headers=trace_headers, priority=priority, data_parallel_rank=data_parallel_rank, tokenization_kwargs=tokenization_kwargs)
        return stream.generator()
    async def generate(self, prompt: PromptType, sampling_params: SamplingParams, request_id: str, lora_request: Optional[LoRARequest]=None, trace_headers: Optional[Mapping[str, str]]=None, priority: int=0, data_parallel_rank: Optional[int]=None) -> AsyncGenerator[RequestOutput, None]:
        try:
            async for output in await self.add_request(request_id, prompt, sampling_params, lora_request=lora_request, trace_headers=trace_headers, priority=priority, data_parallel_rank=data_parallel_rank):
                yield AphroditeEngine.validate_output(output, RequestOutput)
        except asyncio.CancelledError:
            await self.abort(request_id)
            raise
    async def encode(self, prompt: PromptType, pooling_params: PoolingParams, request_id: str, lora_request: Optional[LoRARequest]=None, trace_headers: Optional[Mapping[str, str]]=None, priority: int=0, tokenization_kwargs: Optional[dict[str, Any]]=None) -> AsyncGenerator[PoolingRequestOutput, None]:
        try:
            async for output in await self.add_request(request_id, prompt, pooling_params, lora_request=lora_request, trace_headers=trace_headers, priority=priority, tokenization_kwargs=tokenization_kwargs):
                yield AphroditeEngine.validate_output(output, PoolingRequestOutput)
        except asyncio.CancelledError:
            await self.abort(request_id)
            raise
    async def abort(self, request_id: str) -> None:
        if not self.is_running:
            raise AsyncEngineDeadError('Background loop is not running. If it was running, inspect the output to find the stacktrace of the error that caused the background loop to stop (AsyncEngineDeadError).')
        return self._abort(request_id)
    def _abort(self, request_id: str) -> None:
        self._request_tracker.abort_request(request_id, exception=asyncio.CancelledError, verbose=self.log_requests)
    async def get_aphrodite_config(self) -> AphroditeConfig:
        return self.engine.get_aphrodite_config()
    async def get_model_config(self) -> ModelConfig:
        return self.engine.get_model_config()
    async def get_parallel_config(self) -> ParallelConfig:
        return self.engine.get_parallel_config()
    async def get_decoding_config(self) -> DecodingConfig:
        return self.engine.get_decoding_config()
    async def get_scheduler_config(self) -> SchedulerConfig:
        return self.engine.get_scheduler_config()
    async def get_lora_config(self) -> LoRAConfig:
        return self.engine.get_lora_config()
    async def do_log_stats(self, scheduler_outputs: Optional[SchedulerOutputs]=None, model_output: Optional[List[SamplerOutput]]=None) -> None:
        self.engine.do_log_stats()
    async def check_health(self) -> None:
        t = time.perf_counter()
        logger.debug('Starting health check...')
        if self.is_stopped:
            raise AsyncEngineDeadError('Background loop is stopped.')
        await self.engine.check_health_async()
        logger.debug('Health check took {:.2f}s', time.perf_counter() - t)
    async def is_tracing_enabled(self) -> bool:
        return self.engine.is_tracing_enabled()
    def add_logger(self, logger_name: str, logger: StatLoggerBase) -> None:
        self.engine.add_logger(logger_name=logger_name, logger=logger)
    def remove_logger(self, logger_name: str) -> None:
        self.engine.remove_logger(logger_name=logger_name)
    async def start_profile(self) -> None:
        self.engine.start_profile()
    async def stop_profile(self) -> None:
        self.engine.stop_profile()
    async def reset_mm_cache(self) -> None:
        self.engine.reset_mm_cache()
    async def reset_prefix_cache(self, device: Optional[Device]=None) -> None:
        self.engine.reset_prefix_cache(device)
    async def sleep(self, level: int=1) -> None:
        self.engine.sleep(level)
    async def wake_up(self, tags: Optional[list[str]]=None) -> None:
        self.engine.wake_up(tags)
    async def is_sleeping(self) -> bool:
        return self.engine.is_sleeping()
    async def add_lora(self, lora_request: LoRARequest) -> None:
        self.engine.add_lora(lora_request)
    async def collective_rpc(self, method: str, timeout: Optional[float]=None, args: tuple=(), kwargs: Optional[dict]=None):
        return await self.engine.collective_rpc_async(method, timeout, args, kwargs)
if envs.is_set('APHRODITE_USE_V1') and envs.APHRODITE_USE_V1:
    from aphrodite.v1.engine.async_llm import AsyncLLM
    AsyncAphrodite = AsyncLLM