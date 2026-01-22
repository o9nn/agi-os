import asyncio
import time
from collections.abc import AsyncGenerator, Mapping
from copy import copy
from typing import Any, Optional, Union
import numpy as np
from loguru import logger
import aphrodite.common.envs as envs
from aphrodite.common.config import AphroditeConfig, ModelConfig
from aphrodite.common.envs import APHRODITE_V1_OUTPUT_PROC_CHUNK_SIZE
from aphrodite.common.outputs import PoolingRequestOutput, RequestOutput
from aphrodite.common.pooling_params import PoolingParams
from aphrodite.common.sampling_params import SamplingParams
from aphrodite.engine.args_tools import AsyncEngineArgs
from aphrodite.engine.protocol import EngineClient
from aphrodite.inputs import PromptType
from aphrodite.inputs.preprocess import InputPreprocessor
from aphrodite.lora.request import LoRARequest
from aphrodite.multimodal import MULTIMODAL_REGISTRY, MultiModalRegistry
from aphrodite.tasks import SupportedTask
from aphrodite.transformers_utils.config import maybe_register_config_serialize_by_value
from aphrodite.transformers_utils.tokenizer import AnyTokenizer
from aphrodite.transformers_utils.tokenizer_group import init_tokenizer_from_configs
from aphrodite.usage.usage_lib import UsageContext
from aphrodite.utils import Device, cdiv, deprecate_kwargs
from aphrodite.v1.engine import EngineCoreRequest
from aphrodite.v1.engine.core_client import EngineCoreClient
from aphrodite.v1.engine.exceptions import EngineDeadError, EngineGenerateError
from aphrodite.v1.engine.output_processor import OutputProcessor, RequestOutputCollector
from aphrodite.v1.engine.parallel_sampling import ParentRequest
from aphrodite.v1.engine.processor import Processor
from aphrodite.v1.executor.abstract import Executor
from aphrodite.v1.metrics.loggers import StatLoggerFactory, StatLoggerManager
from aphrodite.v1.metrics.prometheus import shutdown_prometheus
from aphrodite.v1.metrics.stats import IterationStats
class AsyncLLM(EngineClient):
    def __init__(self, aphrodite_config: AphroditeConfig, executor_class: type[Executor], log_stats: bool, usage_context: UsageContext=UsageContext.ENGINE_CONTEXT, mm_registry: MultiModalRegistry=MULTIMODAL_REGISTRY, use_cached_outputs: bool=False, log_requests: bool=True, start_engine_loop: bool=True, stat_loggers: Optional[list[StatLoggerFactory]]=None, client_addresses: Optional[dict[str, str]]=None, client_count: int=1, client_index: int=0) -> None:
        if not envs.APHRODITE_USE_V1:
            raise ValueError('Using V1 AsyncLLMEngine, but envs.APHRODITE_USE_V1=False. This should not happen. As a workaround, try using AsyncLLMEngine.from_aphrodite_config(...) or explicitly set APHRODITE_USE_V1=0 or 1 and report this issue on Github.')
        maybe_register_config_serialize_by_value()
        self.model_config = aphrodite_config.model_config
        self.aphrodite_config = aphrodite_config
        self.log_requests = log_requests
        self.log_stats = log_stats
        if self.model_config.skip_tokenizer_init:
            self.tokenizer = None
        else:
            self.tokenizer = init_tokenizer_from_configs(model_config=aphrodite_config.model_config, scheduler_config=aphrodite_config.scheduler_config, lora_config=aphrodite_config.lora_config)
        self.processor = Processor(aphrodite_config=aphrodite_config, tokenizer=self.tokenizer, mm_registry=mm_registry)
        self.output_processor = OutputProcessor(self.tokenizer, log_stats=self.log_stats)
        self.engine_core = EngineCoreClient.make_async_mp_client(aphrodite_config=aphrodite_config, executor_class=executor_class, log_stats=self.log_stats, client_addresses=client_addresses, client_count=client_count, client_index=client_index)
        self.logger_manager: Optional[StatLoggerManager] = None
        if self.log_stats:
            self.logger_manager = StatLoggerManager(aphrodite_config=aphrodite_config, engine_idxs=self.engine_core.engine_ranks_managed, custom_stat_loggers=stat_loggers)
            self.logger_manager.log_engine_initialized()
        self.output_handler: Optional[asyncio.Task] = None
        try:
            asyncio.get_running_loop()
            self._run_output_handler()
        except RuntimeError:
            pass
    @classmethod
    @deprecate_kwargs('disable_log_requests', additional_message='This argument will have no effect. Use `enable_log_requests` instead.')
    def from_aphrodite_config(cls, aphrodite_config: AphroditeConfig, start_engine_loop: bool=True, usage_context: UsageContext=UsageContext.ENGINE_CONTEXT, stat_loggers: Optional[list[StatLoggerFactory]]=None, enable_log_requests: bool=False, disable_log_stats: bool=False, client_addresses: Optional[dict[str, str]]=None, client_count: int=1, client_index: int=0, disable_log_requests: bool=True) -> 'AsyncLLM':
        if not envs.APHRODITE_USE_V1:
            raise ValueError('Using V1 AsyncLLMEngine, but envs.APHRODITE_USE_V1=False. This should not happen. As a workaround, try using AsyncLLMEngine.from_aphrodite_config(...) or explicitly set APHRODITE_USE_V1=0 or 1 and report this issue on Github.')
        return cls(aphrodite_config=aphrodite_config, executor_class=Executor.get_class(aphrodite_config), start_engine_loop=start_engine_loop, stat_loggers=stat_loggers, log_requests=enable_log_requests, log_stats=not disable_log_stats, usage_context=usage_context, client_addresses=client_addresses, client_count=client_count, client_index=client_index)
    @classmethod
    def from_engine_args(cls, engine_args: AsyncEngineArgs, start_engine_loop: bool=True, usage_context: UsageContext=UsageContext.ENGINE_CONTEXT, stat_loggers: Optional[list[StatLoggerFactory]]=None) -> 'AsyncLLM':
        aphrodite_config = engine_args.create_engine_config(usage_context)
        executor_class = Executor.get_class(aphrodite_config)
        return cls(aphrodite_config=aphrodite_config, executor_class=executor_class, log_requests=engine_args.enable_log_requests, log_stats=not engine_args.disable_log_stats, start_engine_loop=start_engine_loop, usage_context=usage_context, stat_loggers=stat_loggers)
    def __del__(self):
        self.shutdown()
    def shutdown(self):
        shutdown_prometheus()
        if (engine_core := getattr(self, 'engine_core', None)):
            engine_core.shutdown()
        if (handler := getattr(self, 'output_handler', None)):
            handler.cancel()
    async def get_supported_tasks(self) -> tuple[SupportedTask, ...]:
        return await self.engine_core.get_supported_tasks_async()
    async def add_request(self, request_id: str, prompt: PromptType, params: Union[SamplingParams, PoolingParams], arrival_time: Optional[float]=None, lora_request: Optional[LoRARequest]=None, tokenization_kwargs: Optional[dict[str, Any]]=None, trace_headers: Optional[Mapping[str, str]]=None, priority: int=0, data_parallel_rank: Optional[int]=None) -> RequestOutputCollector:
        if self.errored:
            raise EngineDeadError()
        is_pooling = isinstance(params, PoolingParams)
        queue = RequestOutputCollector(output_kind=params.output_kind)
        prompt_str, request = self.processor.process_inputs(request_id, prompt, params, arrival_time, lora_request, tokenization_kwargs, trace_headers, priority, data_parallel_rank)
        if is_pooling or params.n == 1:
            await self._add_request(request, prompt_str, None, 0, queue)
            return queue
        parent_request = ParentRequest(request_id, params)
        for idx in range(params.n):
            request_id, params = parent_request.get_child_info(idx)
            child_request = request if idx == params.n - 1 else copy(request)
            child_request.request_id = request_id
            child_request.sampling_params = params
            await self._add_request(child_request, prompt_str, parent_request, idx, queue)
        return queue
    async def _add_request(self, request: EngineCoreRequest, prompt: Optional[str], parent_req: Optional[ParentRequest], index: int, queue: RequestOutputCollector):
        self.output_processor.add_request(request, prompt, parent_req, index, queue)
        await self.engine_core.add_request_async(request)
        if self.log_requests:
            logger.info('Added request {}.', request.request_id)
    async def generate(self, prompt: PromptType, sampling_params: SamplingParams, request_id: str, lora_request: Optional[LoRARequest]=None, trace_headers: Optional[Mapping[str, str]]=None, priority: int=0, data_parallel_rank: Optional[int]=None) -> AsyncGenerator[RequestOutput, None]:
        try:
            self._run_output_handler()
            q = await self.add_request(request_id, prompt, sampling_params, lora_request=lora_request, trace_headers=trace_headers, priority=priority, data_parallel_rank=data_parallel_rank)
            finished = False
            while not finished:
                out = q.get_nowait() or await q.get()
                finished = out.finished
                yield out
        except (asyncio.CancelledError, GeneratorExit):
            await self.abort(request_id)
            if self.log_requests:
                logger.info('Request {} aborted.', request_id)
            raise
        except EngineDeadError:
            if self.log_requests:
                logger.info('Request {} failed (engine dead).', request_id)
            raise
        except ValueError:
            if self.log_requests:
                logger.info('Request {} failed (bad request).', request_id)
            raise
        except Exception as e:
            await self.abort(request_id)
            if self.log_requests:
                logger.info('Request {} failed.', request_id)
            raise EngineGenerateError() from e
    def _run_output_handler(self):
        if self.output_handler is not None:
            return
        engine_core = self.engine_core
        output_processor = self.output_processor
        log_stats = self.log_stats
        logger_manager = self.logger_manager
        async def output_handler():
            try:
                while True:
                    outputs = await engine_core.get_output_async()
                    num_outputs = len(outputs.outputs)
                    iteration_stats = IterationStats() if log_stats and num_outputs else None
                    if num_outputs <= APHRODITE_V1_OUTPUT_PROC_CHUNK_SIZE:
                        slices = (outputs.outputs,)
                    else:
                        slices = np.array_split(outputs.outputs, cdiv(num_outputs, APHRODITE_V1_OUTPUT_PROC_CHUNK_SIZE))
                    for i, outputs_slice in enumerate(slices):
                        processed_outputs = output_processor.process_outputs(outputs_slice, outputs.timestamp, iteration_stats)
                        assert not processed_outputs.request_outputs
                        if i + 1 < len(slices):
                            await asyncio.sleep(0)
                        await engine_core.abort_requests_async(processed_outputs.reqs_to_abort)
                    if logger_manager:
                        logger_manager.record(engine_idx=outputs.engine_index, scheduler_stats=outputs.scheduler_stats, iteration_stats=iteration_stats)
            except Exception as e:
                logger.exception('AsyncLLM output_handler failed: {}', e)
                output_processor.propagate_error(e)
        self.output_handler = asyncio.create_task(output_handler())
    async def abort(self, request_id: str) -> None:
        request_ids = self.output_processor.abort_requests((request_id,))
        await self.engine_core.abort_requests_async(request_ids)
        if self.log_requests:
            logger.info('Aborted request {}.', request_id)
    async def encode(self, prompt: PromptType, pooling_params: PoolingParams, request_id: str, lora_request: Optional[LoRARequest]=None, trace_headers: Optional[Mapping[str, str]]=None, priority: int=0, tokenization_kwargs: Optional[dict[str, Any]]=None) -> AsyncGenerator[PoolingRequestOutput, None]:
        try:
            self._run_output_handler()
            q = await self.add_request(request_id, prompt, pooling_params, lora_request=lora_request, trace_headers=trace_headers, priority=priority, tokenization_kwargs=tokenization_kwargs)
            finished = False
            while not finished:
                out = q.get_nowait() or await q.get()
                assert isinstance(out, PoolingRequestOutput)
                finished = out.finished
                yield out
        except asyncio.CancelledError:
            await self.abort(request_id)
            if self.log_requests:
                logger.info('Request {} aborted.', request_id)
            raise
        except EngineDeadError:
            if self.log_requests:
                logger.info('Request {} failed (engine dead).', request_id)
            raise
        except ValueError:
            if self.log_requests:
                logger.info('Request {} failed (bad request).', request_id)
            raise
        except Exception as e:
            await self.abort(request_id)
            if self.log_requests:
                logger.info('Request {} failed.', request_id)
            raise EngineGenerateError() from e
    async def get_aphrodite_config(self) -> AphroditeConfig:
        return self.aphrodite_config
    async def get_model_config(self) -> ModelConfig:
        return self.model_config
    async def get_decoding_config(self):
        raise ValueError('Not Supported on V1 yet.')
    async def get_input_preprocessor(self) -> InputPreprocessor:
        return self.processor.input_preprocessor
    async def get_tokenizer(self, lora_request: Optional[LoRARequest]=None) -> AnyTokenizer:
        if self.tokenizer is None:
            raise ValueError('Unable to get tokenizer because skip_tokenizer_init is True')
        return self.tokenizer.get_lora_tokenizer(lora_request)
    async def is_tracing_enabled(self) -> bool:
        return False
    async def do_log_stats(self, scheduler_outputs=None, model_output=None) -> None:
        if self.logger_manager:
            self.logger_manager.log()
    async def check_health(self) -> None:
        logger.debug('Called check_health.')
        if self.errored:
            raise self.dead_error
    async def start_profile(self) -> None:
        await self.engine_core.profile_async(True)
    async def stop_profile(self) -> None:
        await self.engine_core.profile_async(False)
    async def reset_mm_cache(self) -> None:
        self.processor.mm_registry.reset_processor_cache()
        self.processor.mm_input_cache_client.reset()
        await self.engine_core.reset_mm_cache_async()
    async def reset_prefix_cache(self, device: Optional[Device]=None) -> None:
        if device == Device.CPU:
            raise ValueError('Not supported on CPU.')
        await self.engine_core.reset_prefix_cache_async()
    async def sleep(self, level: int=1) -> None:
        await self.engine_core.sleep_async(level)
    async def wake_up(self, tags: Optional[list[str]]=None) -> None:
        await self.engine_core.wake_up_async(tags)
    async def is_sleeping(self) -> bool:
        return await self.engine_core.is_sleeping_async()
    async def add_lora(self, lora_request: LoRARequest) -> bool:
        return await self.engine_core.add_lora_async(lora_request)
    async def remove_lora(self, lora_id: int) -> bool:
        return await self.engine_core.remove_lora_async(lora_id)
    async def list_loras(self) -> set[int]:
        return await self.engine_core.list_loras_async()
    async def pin_lora(self, lora_id: int) -> bool:
        return await self.engine_core.pin_lora_async(lora_id)
    async def collective_rpc(self, method: str, timeout: Optional[float]=None, args: tuple=(), kwargs: Optional[dict]=None):
        return await self.engine_core.collective_rpc_async(method, timeout, args, kwargs)
    async def wait_for_requests_to_drain(self, drain_timeout: int=300):
        start_time = time.time()
        while time.time() - start_time < drain_timeout:
            if not self.engine_core.dp_engines_running():
                logger.info('Engines are idle, requests have been drained')
                return
            logger.info('Engines are still running, waiting for requests to drain...')
            await asyncio.sleep(1)
        raise TimeoutError(f'Timeout reached after {drain_timeout} seconds waiting for requests to drain.')
    async def scale_elastic_ep(self, new_data_parallel_size: int, drain_timeout: int=300):
        old_data_parallel_size = self.aphrodite_config.parallel_config.data_parallel_size
        if old_data_parallel_size == new_data_parallel_size:
            logger.info('Data parallel size is already {}, skipping scale', new_data_parallel_size)
            return
        logger.info('Waiting for requests to drain before scaling up to {} engines...', new_data_parallel_size)
        await self.wait_for_requests_to_drain(drain_timeout)
        logger.info('Requests have been drained, proceeding with scale to {} engines', new_data_parallel_size)
        await self.engine_core.scale_elastic_ep(new_data_parallel_size)
        self.aphrodite_config.parallel_config.data_parallel_size = new_data_parallel_size
        if new_data_parallel_size > old_data_parallel_size and self.log_stats:
            self.logger_manager = StatLoggerManager(aphrodite_config=self.aphrodite_config, engine_idxs=list(range(new_data_parallel_size)), custom_stat_loggers=None)
    @property
    def is_running(self) -> bool:
        return self.output_handler is None or not self.output_handler.done()
    @property
    def is_stopped(self) -> bool:
        return self.errored
    @property
    def errored(self) -> bool:
        return self.engine_core.resources.engine_dead or not self.is_running
    @property
    def dead_error(self) -> BaseException:
        return EngineDeadError()