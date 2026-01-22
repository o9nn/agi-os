import time
from collections import Counter as collectionsCounter
from collections import deque
from contextlib import contextmanager
from dataclasses import dataclass
from functools import partial
from typing import TYPE_CHECKING, Any, Callable, ClassVar, Deque, Dict, Iterable, List, Literal, Mapping, NamedTuple, Optional
from typing import Sequence as GenericSequence
from typing import Set, Type, Union, cast
import torch
from loguru import logger
from typing_extensions import TypeVar
import aphrodite.common.envs as envs
from aphrodite.common.config import AphroditeConfig, DecodingConfig, LoRAConfig, ModelConfig, ObservabilityConfig, ParallelConfig, SchedulerConfig
from aphrodite.common.logger import setup_logger
from aphrodite.common.logits_processor import get_bad_words_logits_processors
from aphrodite.common.outputs import PoolingRequestOutput, RequestOutput, RequestOutputFactory
from aphrodite.common.pooling_params import PoolingParams
from aphrodite.common.sampling_params import RequestOutputKind, SamplingParams
from aphrodite.common.sequence import ExecuteModelRequest, ParallelSampleSequenceGroup, PoolingSequenceGroupOutput, Sequence, SequenceGroup, SequenceGroupBase, SequenceGroupMetadata, SequenceGroupOutput, SequenceStatus
from aphrodite.utils import Counter, Device, resolve_obj_by_qualname, weak_bind
from aphrodite.endpoints.openai.logits_processors import get_logits_processors as get_openai_logits_processors
from aphrodite.engine.args_tools import EngineArgs
from aphrodite.engine.metrics_types import StatLoggerBase, Stats
from aphrodite.engine.output_processor.interfaces import SequenceGroupOutputProcessor
from aphrodite.engine.output_processor.stop_checker import StopChecker
from aphrodite.engine.output_processor.util import create_output_by_sequence_group
from aphrodite.executor.executor_base import ExecutorBase
from aphrodite.inputs import ProcessorInputs, PromptType, SingletonInputs
from aphrodite.inputs.parse import split_enc_dec_inputs
from aphrodite.inputs.preprocess import InputPreprocessor
from aphrodite.lora.request import LoRARequest
from aphrodite.modeling.layers.sampler import SamplerOutput
from aphrodite.multimodal import MULTIMODAL_REGISTRY, MultiModalRegistry
from aphrodite.multimodal.processing import EncDecMultiModalProcessor
from aphrodite.processing.scheduler import ScheduledSequenceGroup, SchedulerOutputs
from aphrodite.tracing import SpanAttributes, SpanKind, extract_trace_context, init_tracer
from aphrodite.transformers_utils.detokenizer import Detokenizer
from aphrodite.transformers_utils.tokenizer import AnyTokenizer
from aphrodite.transformers_utils.tokenizer_group import TokenizerGroup, init_tokenizer_from_configs
from aphrodite.usage.usage_lib import UsageContext, is_usage_stats_enabled, usage_message
from aphrodite.version import __version__ as APHRODITE_VERSION
from aphrodite.worker.model_runner_base import InputProcessingError
_LOCAL_LOGGING_INTERVAL_SEC = 5
_O = TypeVar('_O', RequestOutput, PoolingRequestOutput)
_R = TypeVar('_R', default=Any)
@dataclass
class SchedulerOutputState:
    seq_group_metadata_list: Optional[List[SequenceGroupMetadata]] = None
    scheduler_outputs: Optional[SchedulerOutputs] = None
    allow_async_output_proc: bool = False
    last_output: Optional[SamplerOutput] = None
class OutputData(NamedTuple):
    outputs: List[SamplerOutput]
    seq_group_metadata_list: List[SequenceGroupMetadata]
    scheduler_outputs: SchedulerOutputs
    is_async: bool
    is_last_step: bool
    is_first_step_output: Optional[bool]
    skip: List[int]
class SchedulerContext:
    def __init__(self, multi_step_stream_outputs: bool=False):
        self.output_queue: Deque[OutputData] = deque()
        self.request_outputs: List[Union[RequestOutput, PoolingRequestOutput]] = []
        self.seq_group_metadata_list: Optional[List[SequenceGroupMetadata]] = None
        self.scheduler_outputs: Optional[SchedulerOutputs] = None
        self.multi_step_stream_outputs: bool = multi_step_stream_outputs
    def append_output(self, outputs: List[SamplerOutput], seq_group_metadata_list: List[SequenceGroupMetadata], scheduler_outputs: SchedulerOutputs, is_async: bool, is_last_step: bool, is_first_step_output: Optional[bool]):
        self.output_queue.append(OutputData(outputs=outputs, seq_group_metadata_list=seq_group_metadata_list, scheduler_outputs=scheduler_outputs, is_async=is_async, is_last_step=is_last_step, is_first_step_output=is_first_step_output, skip=[]))
class AphroditeEngine:
    DO_VALIDATE_OUTPUT: ClassVar[bool] = False
    'A flag to toggle whether to validate the type of request output.'
    @classmethod
    @contextmanager
    def enable_output_validation(cls):
        cls.DO_VALIDATE_OUTPUT = True
        yield
        cls.DO_VALIDATE_OUTPUT = False
    @classmethod
    def validate_output(cls, output: object, output_type: Type[_O]) -> _O:
        do_validate = cls.DO_VALIDATE_OUTPUT
        if (TYPE_CHECKING or do_validate) and (not isinstance(output, output_type)):
            raise TypeError(f'Expected output of type {output_type}, but found type {type(output)}')
        return cast(_O, output)
    @classmethod
    def validate_outputs(cls, outputs: GenericSequence[object], output_type: Type[_O]) -> List[_O]:
        do_validate = cls.DO_VALIDATE_OUTPUT
        outputs_: List[_O]
        if TYPE_CHECKING or do_validate:
            outputs_ = []
            for output in outputs:
                if not isinstance(output, output_type):
                    raise TypeError(f'Expected output of type {output_type}, but found type {type(output)}')
                outputs_.append(output)
        else:
            outputs_ = outputs
        return outputs_
    tokenizer: Optional[TokenizerGroup]
    def __init__(self, aphrodite_config: AphroditeConfig, executor_class: Type[ExecutorBase], log_stats: bool, usage_context: UsageContext=UsageContext.ENGINE_CONTEXT, stat_loggers: Optional[Dict[str, StatLoggerBase]]=None, mm_registry: MultiModalRegistry=MULTIMODAL_REGISTRY, use_cached_outputs: bool=False) -> None:
        if envs.APHRODITE_USE_V1:
            raise ValueError('Using V0 AphroditeEngine, but envs.APHRODITE_USE_V1=True. This should not happen. As a workaround, try using AphroditeEngine.from_aphrodite_config(...) or explicitly set APHRODITE_USE_V1=0 or 1 and report this issue on Github.')
        self.aphrodite_config = aphrodite_config
        self.model_config = aphrodite_config.model_config
        self.cache_config = aphrodite_config.cache_config
        self.lora_config = aphrodite_config.lora_config
        self.parallel_config = aphrodite_config.parallel_config
        self.scheduler_config = aphrodite_config.scheduler_config
        self.device_config = aphrodite_config.device_config
        self.speculative_config = aphrodite_config.speculative_config
        self.load_config = aphrodite_config.load_config
        self.decoding_config = aphrodite_config.decoding_config or DecodingConfig()
        self.observability_config = aphrodite_config.observability_config or ObservabilityConfig()
        logger.info('Initializing a V0 LLM engine (v{}) with config: {}, use_cached_outputs={}, ', APHRODITE_VERSION, aphrodite_config, use_cached_outputs)
        self.log_stats = log_stats
        self.use_cached_outputs = use_cached_outputs
        if self.model_config.skip_tokenizer_init:
            self.tokenizer = None
            self.detokenizer = None
            tokenizer_group = None
        else:
            self.tokenizer = self._init_tokenizer()
            self.detokenizer = Detokenizer(self.tokenizer)
            tokenizer_group = self.get_tokenizer_group()
        def get_tokenizer_for_seq(sequence: Sequence) -> AnyTokenizer:
            assert tokenizer_group, 'tokenizer_group cannot be None, make sure skip_tokenizer_init is False'
            return tokenizer_group.get_lora_tokenizer(sequence.lora_request)
        self.seq_counter = Counter()
        self.generation_config_fields = self.model_config.try_get_generation_config()
        self.input_preprocessor = InputPreprocessor(self.model_config, self.tokenizer, mm_registry)
        self.modeling = executor_class(aphrodite_config=aphrodite_config)
        if self.model_config.runner_type != 'pooling':
            self._initialize_kv_caches()
        if is_usage_stats_enabled():
            from aphrodite.modeling.model_loader import get_architecture_class_name
            usage_message.report_usage(get_architecture_class_name(self.model_config), usage_context, extra_kvs={'dtype': str(self.model_config.dtype), 'tensor_parallel_size': self.parallel_config.tensor_parallel_size, 'block_size': self.cache_config.block_size, 'gpu_memory_utilization': self.cache_config.gpu_memory_utilization, 'quantization': self.model_config.quantization, 'kv_cache_dtype': str(self.cache_config.cache_dtype), 'enable_lora': bool(self.lora_config), 'enable_prefix_caching': self.cache_config.enable_prefix_caching, 'enforce_eager': self.model_config.enforce_eager, 'disable_custom_all_reduce': self.parallel_config.disable_custom_all_reduce})
        self.cached_scheduler_outputs = [SchedulerOutputState() for _ in range(self.parallel_config.pipeline_parallel_size)]
        self.scheduler_contexts = [SchedulerContext(multi_step_stream_outputs=self.scheduler_config.multi_step_stream_outputs) for _ in range(self.parallel_config.pipeline_parallel_size)]
        if self.model_config.use_async_output_proc:
            process_model_outputs = weak_bind(self._process_model_outputs)
            self.async_callbacks = [partial(process_model_outputs, ctx=self.scheduler_contexts[v_id]) for v_id in range(self.parallel_config.pipeline_parallel_size)]
        else:
            self.async_callbacks = []
        self.process_request_outputs_callback: Optional[Callable] = None
        if isinstance(self.aphrodite_config.scheduler_config.scheduler_cls, str):
            Scheduler = resolve_obj_by_qualname(self.aphrodite_config.scheduler_config.scheduler_cls)
        else:
            Scheduler = self.aphrodite_config.scheduler_config.scheduler_cls
        self.scheduler = [Scheduler(self.scheduler_config, self.cache_config, self.lora_config, self.parallel_config.pipeline_parallel_size, self.async_callbacks[v_id] if self.model_config.use_async_output_proc else None) for v_id in range(self.parallel_config.pipeline_parallel_size)]
        if self.log_stats:
            if stat_loggers is not None:
                self.stat_loggers = stat_loggers
            else:
                from aphrodite.engine.metrics import LoggingStatLogger, PrometheusStatLogger
                self.stat_loggers = {'logging': LoggingStatLogger(local_interval=_LOCAL_LOGGING_INTERVAL_SEC, aphrodite_config=aphrodite_config), 'prometheus': PrometheusStatLogger(local_interval=_LOCAL_LOGGING_INTERVAL_SEC, labels=dict(model_name=self.model_config.served_model_name), aphrodite_config=aphrodite_config)}
                self.stat_loggers['prometheus'].info('cache_config', self.cache_config)
        self.tracer = None
        if self.observability_config.otlp_traces_endpoint:
            self.tracer = init_tracer('aphrodite.llm_engine', self.observability_config.otlp_traces_endpoint)
        self.output_processor = SequenceGroupOutputProcessor.create_output_processor(self.scheduler_config, self.detokenizer, self.scheduler, self.seq_counter, get_tokenizer_for_seq, stop_checker=StopChecker(self.scheduler_config.max_model_len, get_tokenizer_for_seq))
        self.seq_id_to_seq_group: Dict[str, SequenceGroupBase] = {}
        self._skip_scheduling_next_step = False
        self.reset_mm_cache()
    def _initialize_kv_caches(self) -> None:
        start = time.time()
        num_gpu_blocks, num_cpu_blocks = self.modeling.determine_num_available_blocks()
        if self.cache_config.num_gpu_blocks_override is not None:
            num_gpu_blocks_override = self.cache_config.num_gpu_blocks_override
            logger.info('Overriding num_gpu_blocks={} with num_gpu_blocks_override={}', num_gpu_blocks, num_gpu_blocks_override)
            num_gpu_blocks = num_gpu_blocks_override
        self.cache_config.num_gpu_blocks = num_gpu_blocks
        self.cache_config.num_cpu_blocks = num_cpu_blocks
        self.modeling.initialize_cache(num_gpu_blocks, num_cpu_blocks)
        elapsed = time.time() - start
        logger.info('init engine (profile, create kv cache, warmup model) took {:.2f} seconds', elapsed)
    @classmethod
    def _get_executor_cls(cls, engine_config: AphroditeConfig) -> Type[ExecutorBase]:
        distributed_executor_backend = engine_config.parallel_config.distributed_executor_backend
        if isinstance(distributed_executor_backend, type):
            if not issubclass(distributed_executor_backend, ExecutorBase):
                raise TypeError(f'distributed_executor_backend must be a subclass of ExecutorBase. Got {distributed_executor_backend}.')
            executor_class = distributed_executor_backend
        elif distributed_executor_backend == 'ray':
            from aphrodite.executor.ray_distributed_executor import RayDistributedExecutor
            executor_class = RayDistributedExecutor
        elif distributed_executor_backend == 'mp':
            from aphrodite.executor.mp_distributed_executor import MultiprocessingDistributedExecutor
            assert not envs.APHRODITE_USE_RAY_SPMD_WORKER, 'multiprocessing distributed executor backend does not support APHRODITE_USE_RAY_SPMD_WORKER=1'
            executor_class = MultiprocessingDistributedExecutor
        elif distributed_executor_backend == 'uni':
            from aphrodite.executor.uniproc_executor import UniProcExecutor
            executor_class = UniProcExecutor
        elif distributed_executor_backend == 'external_launcher':
            from aphrodite.executor.uniproc_executor import ExecutorWithExternalLauncher
            executor_class = ExecutorWithExternalLauncher
        else:
            raise ValueError(f'unrecognized distributed_executor_backend: {distributed_executor_backend}')
        return executor_class
    @classmethod
    def from_aphrodite_config(cls, aphrodite_config: AphroditeConfig, usage_context: UsageContext=UsageContext.ENGINE_CONTEXT, stat_loggers: Optional[Dict[str, StatLoggerBase]]=None, disable_log_stats: bool=False) -> 'AphroditeEngine':
        return cls(aphrodite_config=aphrodite_config, executor_class=cls._get_executor_cls(aphrodite_config), log_stats=not disable_log_stats, usage_context=usage_context, stat_loggers=stat_loggers)
    @classmethod
    def from_engine_args(cls, engine_args: EngineArgs, usage_context: UsageContext=UsageContext.ENGINE_CONTEXT, stat_loggers: Optional[Dict[str, StatLoggerBase]]=None) -> 'AphroditeEngine':
        aphrodite_config = engine_args.create_engine_config(usage_context)
        engine_cls = cls
        if envs.APHRODITE_USE_V1:
            from aphrodite.v1.engine.llm_engine import LLMEngine as V1AphroditeEngine
            engine_cls = V1AphroditeEngine
        return engine_cls.from_aphrodite_config(aphrodite_config=aphrodite_config, usage_context=usage_context, stat_loggers=stat_loggers, disable_log_stats=engine_args.disable_log_stats)
    def __reduce__(self):
        raise RuntimeError('AphroditeEngine should not be pickled!')
    def __del__(self):
        if (modeling := getattr(self, 'modeling', None)):
            modeling.shutdown()
    def get_tokenizer_group(self) -> TokenizerGroup:
        if self.tokenizer is None:
            raise ValueError('Unable to get tokenizer because skip_tokenizer_init is True')
        return self.tokenizer
    def get_tokenizer(self, lora_request: Optional[LoRARequest]=None) -> AnyTokenizer:
        return self.get_tokenizer_group().get_lora_tokenizer(lora_request)
    def _init_tokenizer(self) -> TokenizerGroup:
        return init_tokenizer_from_configs(model_config=self.model_config, scheduler_config=self.scheduler_config, lora_config=self.lora_config)
    def _verify_args(self) -> None:
        self.model_config.verify_with_parallel_config(self.parallel_config)
        self.cache_config.verify_with_parallel_config(self.parallel_config)
        if self.lora_config:
            self.lora_config.verify_with_model_config(self.model_config)
            self.lora_config.verify_with_scheduler_config(self.scheduler_config)
    def _add_processed_request(self, request_id: str, processed_inputs: ProcessorInputs, params: Union[SamplingParams, PoolingParams], arrival_time: float, lora_request: Optional[LoRARequest], trace_headers: Optional[Mapping[str, str]]=None, priority: int=0) -> Optional[SequenceGroup]:
        if isinstance(params, SamplingParams) and params.n > 1:
            ParallelSampleSequenceGroup.add_request(request_id, self, params, processed_inputs=processed_inputs, arrival_time=arrival_time, lora_request=lora_request, trace_headers=trace_headers, priority=priority)
            return None
        self._validate_model_inputs(processed_inputs, lora_request)
        block_size = self.cache_config.block_size
        seq_id = next(self.seq_counter)
        eos_token_id = self.input_preprocessor.get_eos_token_id(lora_request)
        encoder_inputs, decoder_inputs = split_enc_dec_inputs(processed_inputs)
        seq = Sequence(seq_id, decoder_inputs, block_size, eos_token_id, lora_request)
        encoder_seq = None if encoder_inputs is None else Sequence(seq_id, encoder_inputs, block_size, eos_token_id, lora_request)
        if isinstance(params, SamplingParams):
            seq_group = self._create_sequence_group_with_sampling(request_id, seq, params, arrival_time=arrival_time, lora_request=lora_request, trace_headers=trace_headers, encoder_seq=encoder_seq, priority=priority)
        elif isinstance(params, PoolingParams):
            seq_group = self._create_sequence_group_with_pooling(request_id, seq, params, arrival_time=arrival_time, lora_request=lora_request, encoder_seq=encoder_seq, priority=priority)
        else:
            raise ValueError('Either SamplingParams or PoolingParams must be provided.')
        costs = [scheduler.get_num_unfinished_seq_groups() for scheduler in self.scheduler]
        min_cost_scheduler = self.scheduler[costs.index(min(costs))]
        min_cost_scheduler.add_seq_group(seq_group)
        return seq_group
    def stop_remote_worker_execution_loop(self) -> None:
        self.modeling.stop_remote_worker_execution_loop()
    def add_request(self, request_id: str, prompt: PromptType, params: Union[SamplingParams, PoolingParams], arrival_time: Optional[float]=None, lora_request: Optional[LoRARequest]=None, tokenization_kwargs: Optional[dict[str, Any]]=None, trace_headers: Optional[Mapping[str, str]]=None, priority: int=0) -> None:
        if not isinstance(request_id, str):
            raise TypeError(f'request_id must be a string, got {type(request_id)}')
        if lora_request is not None and (not self.lora_config):
            raise ValueError(f'Got lora_request {lora_request} but LoRA is not enabled!')
        if priority != 0 and (not self.scheduler_config.policy == 'priority'):
            raise ValueError(f'Got priority {priority} but Priority scheduling is not enabled.')
        if isinstance(params, SamplingParams) and params.logits_processors and (self.scheduler_config.num_scheduler_steps > 1):
            raise ValueError('Logits processors are not supported in multi-step decoding')
        if arrival_time is None:
            arrival_time = time.time()
        if isinstance(prompt, dict) and prompt.get('prompt_embeds', None) is not None and (not prompt.get('prompt_token_ids', None)):
            seq_len = prompt['prompt_embeds'].shape[0]
            prompt['prompt_token_ids'] = [0] * seq_len
        processed_inputs = self.input_preprocessor.preprocess(prompt, tokenization_kwargs=tokenization_kwargs, lora_request=lora_request)
        self._add_processed_request(request_id=request_id, processed_inputs=processed_inputs, params=params, arrival_time=arrival_time, lora_request=lora_request, trace_headers=trace_headers, priority=priority)
    def _create_sequence_group_with_sampling(self, request_id: str, seq: Sequence, sampling_params: SamplingParams, arrival_time: float, lora_request: Optional[LoRARequest], trace_headers: Optional[Mapping[str, str]]=None, encoder_seq: Optional[Sequence]=None, priority: int=0) -> SequenceGroup:
        max_logprobs = self.get_model_config().max_logprobs
        if sampling_params.logprobs and sampling_params.logprobs > max_logprobs or (sampling_params.prompt_logprobs and sampling_params.prompt_logprobs > max_logprobs):
            raise ValueError(f'Cannot request more than {max_logprobs} logprobs.')
        sampling_params = self._build_logits_processors(sampling_params, lora_request)
        sampling_params = sampling_params.clone()
        sampling_params.update_from_generation_config(self.generation_config_fields, seq.eos_token_id)
        draft_size = 1
        if self.aphrodite_config.speculative_config is not None:
            draft_size = self.aphrodite_config.speculative_config.num_speculative_tokens + 1
        seq_group = SequenceGroup(request_id=request_id, seqs=[seq], arrival_time=arrival_time, sampling_params=sampling_params, lora_request=lora_request, trace_headers=trace_headers, encoder_seq=encoder_seq, priority=priority, draft_size=draft_size)
        return seq_group
    def _create_sequence_group_with_pooling(self, request_id: str, seq: Sequence, pooling_params: PoolingParams, arrival_time: float, lora_request: Optional[LoRARequest], encoder_seq: Optional[Sequence]=None, priority: int=0) -> SequenceGroup:
        pooling_params = pooling_params.clone()
        seq_group = SequenceGroup(request_id=request_id, seqs=[seq], arrival_time=arrival_time, lora_request=lora_request, pooling_params=pooling_params, encoder_seq=encoder_seq, priority=priority)
        return seq_group
    def abort_request(self, request_id: Union[str, Iterable[str]]) -> None:
        for scheduler in self.scheduler:
            scheduler.abort_seq_group(request_id, seq_id_to_seq_group=self.seq_id_to_seq_group)
    def get_aphrodite_config(self) -> AphroditeConfig:
        return self.aphrodite_config
    def get_model_config(self) -> ModelConfig:
        return self.model_config
    def get_parallel_config(self) -> ParallelConfig:
        return self.parallel_config
    def get_decoding_config(self) -> DecodingConfig:
        return self.decoding_config
    def get_scheduler_config(self) -> SchedulerConfig:
        return self.scheduler_config
    def get_lora_config(self) -> LoRAConfig:
        return self.lora_config
    def get_num_unfinished_requests(self) -> int:
        return sum((scheduler.get_num_unfinished_seq_groups() for scheduler in self.scheduler))
    def has_unfinished_requests(self) -> bool:
        return any((scheduler.has_unfinished_seqs() for scheduler in self.scheduler))
    def has_unfinished_requests_for_virtual_engine(self, virtual_engine: int) -> bool:
        return self.scheduler[virtual_engine].has_unfinished_seqs()
    def reset_mm_cache(self) -> bool:
        return self.input_preprocessor.mm_registry.reset_processor_cache()
    def reset_prefix_cache(self, device: Optional[Device]=None) -> bool:
        success = True
        for scheduler in self.scheduler:
            success = success and scheduler.reset_prefix_cache(device)
        return success
    @staticmethod
    def _process_sequence_group_outputs(seq_group: SequenceGroup, outputs: List[PoolingSequenceGroupOutput]) -> None:
        seq_group.pooled_data = outputs[0].data
        for seq in seq_group.get_seqs():
            seq.status = SequenceStatus.FINISHED_STOPPED
        return
    def _update_num_computed_tokens_for_multi_step_prefill(self, seq_group: SequenceGroup, seq_group_meta: SequenceGroupMetadata, is_first_step_output: Optional[bool]):
        assert self.scheduler_config.is_multi_step
        if not seq_group_meta.is_prompt:
            return
        do_update: bool = False
        if self.scheduler_config.chunked_prefill_enabled:
            do_update = is_first_step_output is None or is_first_step_output
        else:
            assert seq_group.state.num_steps == 1
            do_update = True
        if do_update:
            seq_group.update_num_computed_tokens(seq_group_meta.token_chunk_size)
    def _process_model_outputs(self, ctx: SchedulerContext, request_id: Optional[str]=None) -> None:
        now = time.time()
        if len(ctx.output_queue) == 0:
            return None
        if request_id:
            outputs, seq_group_metadata_list, scheduler_outputs, is_async, is_last_step, is_first_step_output, skip = ctx.output_queue[0]
        else:
            outputs, seq_group_metadata_list, scheduler_outputs, is_async, is_last_step, is_first_step_output, skip = ctx.output_queue.popleft()
        assert len(seq_group_metadata_list) == len(scheduler_outputs.scheduled_seq_groups)
        has_multiple_outputs: bool = len(outputs) > 1
        outputs_by_sequence_group: List[List[SequenceGroupOutput]]
        if has_multiple_outputs:
            assert self.scheduler_config.is_multi_step or self.speculative_config
            if self.scheduler_config.is_multi_step:
                outputs_by_sequence_group = create_output_by_sequence_group(outputs, len(seq_group_metadata_list))
            elif self.speculative_config:
                num_prefills = sum((sg.is_prompt for sg in seq_group_metadata_list))
                prefills, decodes = (outputs[:num_prefills], outputs[num_prefills:])
                outputs_by_sequence_group = create_output_by_sequence_group(decodes, num_seq_groups=len(seq_group_metadata_list) - num_prefills)
                outputs_by_sequence_group = [p.outputs for p in prefills] + outputs_by_sequence_group
            is_first_step_output = None
        else:
            outputs_by_sequence_group = outputs
        if request_id:
            indices = []
            for i, seq_group_meta in enumerate(seq_group_metadata_list):
                if seq_group_meta.request_id == request_id:
                    assert i not in skip
                    indices.append(i)
                    break
            if not indices:
                return
        else:
            indices = range(len(seq_group_metadata_list))
        finished_before: List[int] = []
        finished_now: List[int] = []
        for i in indices:
            if i in skip:
                continue
            seq_group_meta = seq_group_metadata_list[i]
            scheduled_seq_group = scheduler_outputs.scheduled_seq_groups[i]
            seq_group: SequenceGroup = scheduled_seq_group.seq_group
            if seq_group.is_finished():
                finished_before.append(i)
                continue
            output: List[SequenceGroupOutput]
            if has_multiple_outputs:
                output = outputs_by_sequence_group[i]
            else:
                output = [outputs_by_sequence_group[0][i]]
            if not is_async:
                if self.scheduler_config.is_multi_step:
                    self._update_num_computed_tokens_for_multi_step_prefill(seq_group, seq_group_meta, is_first_step_output)
                else:
                    seq_group.update_num_computed_tokens(seq_group_meta.token_chunk_size or 0)
            if outputs:
                for o in outputs:
                    if isinstance(o, SamplerOutput) and seq_group.metrics is not None:
                        if seq_group.metrics.model_forward_time is not None:
                            seq_group.metrics.model_forward_time += o.model_forward_time or 0
                        else:
                            seq_group.metrics.model_forward_time = o.model_forward_time
                        if seq_group.metrics.model_execute_time is not None:
                            seq_group.metrics.model_execute_time += o.model_execute_time or 0
                        else:
                            seq_group.metrics.model_execute_time = o.model_execute_time
            if self.model_config.runner_type == 'pooling':
                self._process_sequence_group_outputs(seq_group, output)
            else:
                self.output_processor.process_prompt_logprob(seq_group, output)
                if seq_group_meta.do_sample:
                    self.output_processor.process_outputs(seq_group, output, is_async)
            if seq_group.is_finished():
                finished_now.append(i)
        for i in finished_now:
            scheduled_seq_group = scheduler_outputs.scheduled_seq_groups[i]
            seq_group = scheduled_seq_group.seq_group
            seq_group.maybe_set_first_token_time(now)
            if not seq_group.is_prefill():
                seq_group.set_last_token_time(now)
            request_output = RequestOutputFactory.create(seq_group, self.seq_id_to_seq_group, use_cache=self.use_cached_outputs)
            if request_output:
                ctx.request_outputs.append(request_output)
        if request_id:
            assert len(indices) == 1
            skip.append(indices[0])
            if finished_now and self.process_request_outputs_callback is not None:
                self.process_request_outputs_callback(ctx.request_outputs)
                ctx.request_outputs.clear()
            return
        if finished_now:
            for scheduler in self.scheduler:
                scheduler.free_finished_seq_groups()
        if not is_last_step and (not ctx.multi_step_stream_outputs):
            if finished_now and self.process_request_outputs_callback is not None:
                self.process_request_outputs_callback(ctx.request_outputs)
                ctx.request_outputs.clear()
            return
        for i in indices:
            if i in skip or i in finished_before or i in finished_now:
                continue
            scheduled_seq_group = scheduler_outputs.scheduled_seq_groups[i]
            seq_group = scheduled_seq_group.seq_group
            seq_group.maybe_set_first_token_time(now)
            if not seq_group.is_prefill():
                seq_group.set_last_token_time(now)
            request_output = RequestOutputFactory.create(seq_group, self.seq_id_to_seq_group, use_cache=self.use_cached_outputs)
            if request_output:
                ctx.request_outputs.append(request_output)
        if not is_last_step and ctx.multi_step_stream_outputs:
            if self.process_request_outputs_callback is not None:
                self.process_request_outputs_callback(ctx.request_outputs)
                ctx.request_outputs.clear()
            return
        for seq_group in scheduler_outputs.ignored_seq_groups:
            params = seq_group.sampling_params
            if params is not None and params.output_kind == RequestOutputKind.DELTA and (not seq_group.is_finished()):
                continue
            request_output = RequestOutputFactory.create(seq_group, self.seq_id_to_seq_group, use_cache=self.use_cached_outputs)
            if request_output:
                ctx.request_outputs.append(request_output)
        if ctx.request_outputs and self.process_request_outputs_callback is not None:
            self.process_request_outputs_callback(ctx.request_outputs)
            ctx.request_outputs.clear()
        if is_async:
            self.do_log_stats(scheduler_outputs, outputs, finished_before, skip)
            self.do_tracing(scheduler_outputs, finished_before)
        return None
    def _advance_to_next_step(self, output: SamplerOutput, seq_group_metadata_list: List[SequenceGroupMetadata], scheduled_seq_groups: List[ScheduledSequenceGroup]) -> None:
        for seq_group_metadata, sequence_group_outputs, scheduled_seq_group in zip(seq_group_metadata_list, output, scheduled_seq_groups):
            seq_group = scheduled_seq_group.seq_group
            if seq_group.is_finished():
                continue
            if self.scheduler_config.is_multi_step:
                self._update_num_computed_tokens_for_multi_step_prefill(seq_group, seq_group_metadata, seq_group.state.num_steps == 1)
            else:
                token_chunk_size = seq_group_metadata.token_chunk_size if seq_group_metadata.token_chunk_size is not None else 0
                seq_group.update_num_computed_tokens(token_chunk_size)
            if seq_group_metadata.do_sample:
                assert len(sequence_group_outputs.samples) == 1, 'Async output processor expects a single sample (i.e sampling_params.n == 1)'
                sample = sequence_group_outputs.samples[0]
                assert len(seq_group.seqs) == 1
                seq = seq_group.seqs[0]
                if self.scheduler_config.is_multi_step:
                    is_prefill_append = seq.data.get_num_uncomputed_tokens() == 0
                    seq.append_token_id(sample.output_token, sample.logprobs, sample.output_embed)
                    if not is_prefill_append:
                        seq_group.update_num_computed_tokens(1)
                else:
                    seq.append_token_id(sample.output_token, sample.logprobs, sample.output_embed)
    def step(self) -> List[Union[RequestOutput, PoolingRequestOutput]]:
        if self.parallel_config.pipeline_parallel_size > 1:
            raise NotImplementedError('Pipeline parallelism is only supported through AsyncAphrodite as performance will be severely degraded otherwise.')
        virtual_engine = 0
        cached_outputs = self.cached_scheduler_outputs[virtual_engine]
        seq_group_metadata_list = cached_outputs.seq_group_metadata_list
        scheduler_outputs = cached_outputs.scheduler_outputs
        allow_async_output_proc = cached_outputs.allow_async_output_proc
        ctx = self.scheduler_contexts[virtual_engine]
        ctx.request_outputs.clear()
        if not self._has_remaining_steps(seq_group_metadata_list) and (not self._skip_scheduling_next_step):
            seq_group_metadata_list, scheduler_outputs, allow_async_output_proc = self.scheduler[virtual_engine].schedule()
            ctx.seq_group_metadata_list = seq_group_metadata_list
            ctx.scheduler_outputs = scheduler_outputs
            finished_requests_ids = self.scheduler[virtual_engine].get_and_reset_finished_requests_ids()
            for finished_request_id in finished_requests_ids:
                if finished_request_id in self.seq_id_to_seq_group:
                    del self.seq_id_to_seq_group[finished_request_id]
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
            execute_model_req = ExecuteModelRequest(seq_group_metadata_list=seq_group_metadata_list, blocks_to_swap_in=scheduler_outputs.blocks_to_swap_in, blocks_to_swap_out=scheduler_outputs.blocks_to_swap_out, blocks_to_copy=scheduler_outputs.blocks_to_copy, num_lookahead_slots=scheduler_outputs.num_lookahead_slots, running_queue_size=scheduler_outputs.running_queue_size, finished_requests_ids=finished_requests_ids, last_sampled_token_ids=last_sampled_token_ids)
            if allow_async_output_proc:
                execute_model_req.async_callback = self.async_callbacks[virtual_engine]
            try:
                outputs = self.modeling.execute_model(execute_model_req=execute_model_req)
                self._skip_scheduling_next_step = False
            except InputProcessingError as e:
                invalid_request_id = e.request_id
                self._abort_and_cache_schedule(request_id=invalid_request_id, virtual_engine=virtual_engine, seq_group_metadata_list=seq_group_metadata_list, scheduler_outputs=scheduler_outputs, allow_async_output_proc=allow_async_output_proc)
                raise
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
                self.cached_scheduler_outputs[0] = SchedulerOutputState()
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
            logger.debug('Stopping remote worker execution loop.')
            self.modeling.stop_remote_worker_execution_loop()
        return ctx.request_outputs
    def _abort_and_cache_schedule(self, request_id: str, virtual_engine: int, seq_group_metadata_list: List[SequenceGroupMetadata], scheduler_outputs: SchedulerOutputs, allow_async_output_proc: bool) -> None:
        self.abort_request(request_id)
        for i, metadata in enumerate(seq_group_metadata_list):
            if metadata.request_id == request_id:
                del seq_group_metadata_list[i]
                break
        for i, group in enumerate(scheduler_outputs.scheduled_seq_groups):
            if group.seq_group.request_id == request_id:
                del scheduler_outputs.scheduled_seq_groups[i]
                break
        if len(seq_group_metadata_list) > 0:
            self._skip_scheduling_next_step = True
            self._cache_scheduler_outputs_for_multi_step(virtual_engine=virtual_engine, scheduler_outputs=scheduler_outputs, seq_group_metadata_list=seq_group_metadata_list, allow_async_output_proc=allow_async_output_proc)
    def _has_remaining_steps(self, seq_group_metadata_list: Optional[List[SequenceGroupMetadata]]) -> bool:
        if not self.scheduler_config.is_multi_step or not seq_group_metadata_list:
            return False
        ref_remaining_steps = seq_group_metadata_list[0].state.remaining_steps
        if any([seq_group.state.remaining_steps != ref_remaining_steps for seq_group in seq_group_metadata_list[1:]]):
            raise AssertionError('All running sequence groups should have the same remaining steps.')
        return ref_remaining_steps > 0
    def _cache_scheduler_outputs_for_multi_step(self, virtual_engine: int, seq_group_metadata_list: Optional[List[SequenceGroupMetadata]], scheduler_outputs: SchedulerOutputs, allow_async_output_proc: bool) -> None:
        co = self.cached_scheduler_outputs[virtual_engine]
        co.seq_group_metadata_list = seq_group_metadata_list
        co.scheduler_outputs = scheduler_outputs
        co.allow_async_output_proc = allow_async_output_proc
        co.last_output = None
    def _update_cached_scheduler_output(self, virtual_engine: int, output: List[Optional[SamplerOutput]]) -> None:
        if self.parallel_config.pipeline_parallel_size > 1 and len(output) > 0 and (output[0] is not None):
            last_output = output[-1]
            assert last_output is not None
            assert last_output.sampled_token_ids_cpu is not None
            assert last_output.sampled_token_ids is None
            assert last_output.sampled_token_probs is None
            self.cached_scheduler_outputs[virtual_engine].last_output = last_output
    def _get_last_sampled_token_ids(self, virtual_engine: int) -> Optional[torch.Tensor]:
        cached_last_output = self.cached_scheduler_outputs[virtual_engine].last_output
        if self.scheduler_config.is_multi_step and self.parallel_config.pipeline_parallel_size > 1 and (cached_last_output is not None) and (cached_last_output.sampled_token_ids_cpu is not None):
            return cached_last_output.sampled_token_ids_cpu
        return None
    def add_logger(self, logger_name: str, logger: StatLoggerBase) -> None:
        if not self.log_stats:
            raise RuntimeError('Stat logging is disabled. Set `disable_log_stats=False` argument to enable.')
        if logger_name in self.stat_loggers:
            raise KeyError(f'Logger with name {logger_name} already exists.')
        self.stat_loggers[logger_name] = logger
    def remove_logger(self, logger_name: str) -> None:
        if not self.log_stats:
            raise RuntimeError('Stat logging is disabled. Set `disable_log_stats=False` argument to enable.')
        if logger_name not in self.stat_loggers:
            raise KeyError(f'Logger with name {logger_name} does not exist.')
        del self.stat_loggers[logger_name]
    def do_log_stats(self, scheduler_outputs: Optional[SchedulerOutputs]=None, model_output: Optional[List[SamplerOutput]]=None, finished_before: Optional[List[int]]=None, skip: Optional[List[int]]=None) -> None:
        if self.log_stats:
            stats = self._get_stats(scheduler_outputs, model_output, finished_before, skip)
            for logger in self.stat_loggers.values():
                logger.log(stats)
    def _get_stats(self, scheduler_outputs: Optional[SchedulerOutputs], model_output: Optional[List[SamplerOutput]]=None, finished_before: Optional[List[int]]=None, skip: Optional[List[int]]=None) -> Stats:
        now = time.time()
        num_running_sys = sum((len(scheduler.running) for scheduler in self.scheduler))
        num_swapped_sys = sum((len(scheduler.swapped) for scheduler in self.scheduler))
        num_waiting_sys = sum((len(scheduler.waiting) for scheduler in self.scheduler))
        num_total_gpu = self.cache_config.num_gpu_blocks
        gpu_cache_usage_sys = 0.0
        if num_total_gpu:
            num_free_gpu = sum((scheduler.block_manager.get_num_free_gpu_blocks() for scheduler in self.scheduler))
            gpu_cache_usage_sys = 1.0 - num_free_gpu / num_total_gpu
        num_total_cpu = self.cache_config.num_cpu_blocks
        cpu_cache_usage_sys = 0.0
        if num_total_cpu:
            num_free_cpu = sum((scheduler.block_manager.get_num_free_cpu_blocks() for scheduler in self.scheduler))
            cpu_cache_usage_sys = 1.0 - num_free_cpu / num_total_cpu
        cpu_prefix_cache_hit_rate = self.scheduler[0].get_prefix_cache_hit_rate(Device.CPU)
        gpu_prefix_cache_hit_rate = self.scheduler[0].get_prefix_cache_hit_rate(Device.GPU)
        if self.device_config.device_type == 'cpu':
            num_total_gpu, num_total_cpu = (num_total_cpu, num_total_gpu)
            gpu_cache_usage_sys, cpu_cache_usage_sys = (cpu_cache_usage_sys, gpu_cache_usage_sys)
            gpu_prefix_cache_hit_rate, cpu_prefix_cache_hit_rate = (cpu_prefix_cache_hit_rate, gpu_prefix_cache_hit_rate)
        num_prompt_tokens_iter = 0
        num_generation_tokens_iter = 0
        num_tokens_iter = 0
        time_to_first_tokens_iter: List[float] = []
        time_per_output_tokens_iter: List[float] = []
        num_preemption_iter = 0 if scheduler_outputs is None else scheduler_outputs.preempted
        time_e2e_requests: List[float] = []
        time_to_first_tokens_iter: List[float] = []
        time_queue_requests: List[float] = []
        time_inference_requests: List[float] = []
        time_prefill_requests: List[float] = []
        time_decode_requests: List[float] = []
        num_prompt_tokens_requests: List[int] = []
        num_generation_tokens_requests: List[int] = []
        n_requests: List[int] = []
        max_num_generation_tokens_requests: List[int] = []
        max_tokens_requests: List[int] = []
        finished_reason_requests: List[str] = []
        request_ids: List[str] = []
        running_lora_adapters = dict(collectionsCounter([running_request.lora_request.lora_name for scheduler in self.scheduler for running_request in scheduler.running if running_request.lora_request]))
        waiting_lora_adapters = dict(collectionsCounter([waiting_request.lora_request.lora_name for scheduler in self.scheduler for waiting_request in scheduler.waiting if waiting_request.lora_request]))
        max_lora_stat = '0'
        if self.lora_config:
            max_lora_stat = str(self.lora_config.max_loras)
        if scheduler_outputs is not None:
            actual_num_batched_tokens = scheduler_outputs.num_batched_tokens
            num_generation_tokens_from_prefill_groups = 0
            for idx, scheduled_seq_group in enumerate(scheduler_outputs.scheduled_seq_groups):
                if finished_before and idx in finished_before:
                    actual_num_batched_tokens -= 1
                    continue
                if skip and idx in skip:
                    continue
                group_was_prefill = idx < scheduler_outputs.num_prefill_groups
                seq_group = scheduled_seq_group.seq_group
                if group_was_prefill:
                    num_prompt_tokens_iter += scheduled_seq_group.token_chunk_size
                    if not seq_group.is_prefill():
                        latency = seq_group.get_last_token_latency()
                        time_to_first_tokens_iter.append(latency)
                        num_generation_tokens_from_prefill_groups += seq_group.num_seqs()
                else:
                    latency = seq_group.get_last_token_latency()
                    time_per_output_tokens_iter.append(latency)
                    if seq_group.state.current_step == 0:
                        actual_num_batched_tokens += seq_group.state.num_steps - 1
                    else:
                        actual_num_batched_tokens += seq_group.state.current_step - 1
                if seq_group.is_finished():
                    time_e2e_requests.append(now - seq_group.metrics.arrival_time)
                    if seq_group.metrics.first_token_time is not None:
                        ttft = seq_group.metrics.first_token_time - seq_group.metrics.arrival_time
                        time_to_first_tokens_iter.append(ttft)
                    if seq_group.metrics.first_scheduled_time is not None and seq_group.metrics.first_token_time is not None:
                        time_queue_requests.append(seq_group.metrics.first_scheduled_time - seq_group.metrics.arrival_time)
                        time_prefill_requests.append(seq_group.metrics.first_token_time - seq_group.metrics.first_scheduled_time)
                        time_decode_requests.append(now - seq_group.metrics.first_token_time)
                        time_inference_requests.append(now - seq_group.metrics.first_scheduled_time)
                    request_ids.append(seq_group.request_id)
                    num_prompt_tokens_requests.append(len(seq_group.prompt_token_ids))
                    num_generation_tokens_requests.extend([seq.get_output_len() for seq in seq_group.get_finished_seqs()])
                    max_num_generation_tokens_requests.append(max((seq.get_output_len() for seq in seq_group.get_seqs())))
                    if seq_group.sampling_params is not None:
                        n_requests.append(seq_group.sampling_params.n)
                        max_tokens_requests.append(seq_group.sampling_params.max_tokens)
                    finished_reason_requests.extend([SequenceStatus.get_finished_reason(seq.status) for seq in seq_group.get_finished_seqs()])
            num_generation_tokens_iter = actual_num_batched_tokens - num_prompt_tokens_iter + num_generation_tokens_from_prefill_groups
            num_tokens_iter = num_generation_tokens_iter + num_prompt_tokens_iter
        return Stats(now=now, num_running_sys=num_running_sys, num_swapped_sys=num_swapped_sys, num_waiting_sys=num_waiting_sys, gpu_cache_usage_sys=gpu_cache_usage_sys, cpu_cache_usage_sys=cpu_cache_usage_sys, cpu_prefix_cache_hit_rate=cpu_prefix_cache_hit_rate, gpu_prefix_cache_hit_rate=gpu_prefix_cache_hit_rate, num_prompt_tokens_iter=num_prompt_tokens_iter, num_generation_tokens_iter=num_generation_tokens_iter, num_tokens_iter=num_tokens_iter, time_to_first_tokens_iter=time_to_first_tokens_iter, time_per_output_tokens_iter=time_per_output_tokens_iter, num_preemption_iter=num_preemption_iter, time_e2e_requests=time_e2e_requests, time_queue_requests=time_queue_requests, time_inference_requests=time_inference_requests, time_prefill_requests=time_prefill_requests, time_decode_requests=time_decode_requests, num_prompt_tokens_requests=num_prompt_tokens_requests, num_generation_tokens_requests=num_generation_tokens_requests, max_num_generation_tokens_requests=max_num_generation_tokens_requests, n_requests=n_requests, max_tokens_requests=max_tokens_requests, finished_reason_requests=finished_reason_requests, max_lora=str(max_lora_stat), waiting_lora_adapters=list(waiting_lora_adapters.keys()), running_lora_adapters=list(running_lora_adapters.keys()))
    def add_lora(self, lora_request: LoRARequest) -> bool:
        return self.modeling.add_lora(lora_request)
    def remove_lora(self, lora_id: int) -> bool:
        return self.modeling.remove_lora(lora_id)
    def list_loras(self) -> Set[int]:
        return self.modeling.list_loras()
    def pin_lora(self, lora_id: int) -> bool:
        return self.modeling.pin_lora(lora_id)
    def start_profile(self) -> None:
        self.modeling.start_profile()
    def stop_profile(self) -> None:
        self.modeling.stop_profile()
    def sleep(self, level: int=1) -> None:
        assert self.aphrodite_config.model_config.enable_sleep_mode, 'Sleep mode is not enabled in the model config'
        self.modeling.sleep(level=level)
    def wake_up(self, tags: Optional[list[str]]=None) -> None:
        assert self.aphrodite_config.model_config.enable_sleep_mode, 'Sleep mode is not enabled in the model config'
        self.modeling.wake_up(tags)
    def is_sleeping(self) -> bool:
        return self.modeling.is_sleeping
    def check_health(self) -> None:
        self.modeling.check_health()
    def is_tracing_enabled(self) -> bool:
        return self.tracer is not None
    def do_tracing(self, scheduler_outputs: SchedulerOutputs, finished_before: Optional[List[int]]=None) -> None:
        if self.tracer is None:
            return
        for idx, scheduled_seq_group in enumerate(scheduler_outputs.scheduled_seq_groups):
            if finished_before and idx in finished_before:
                continue
            seq_group = scheduled_seq_group.seq_group
            if seq_group.is_finished():
                self.create_trace_span(seq_group)
    def create_trace_span(self, seq_group: SequenceGroup) -> None:
        if self.tracer is None or seq_group.sampling_params is None:
            return
        arrival_time_nano_seconds = int(seq_group.metrics.arrival_time * 1000000000.0)
        trace_context = extract_trace_context(seq_group.trace_headers)
        with self.tracer.start_as_current_span('llm_request', kind=SpanKind.SERVER, context=trace_context, start_time=arrival_time_nano_seconds) as seq_span:
            metrics = seq_group.metrics
            ttft = metrics.first_token_time - metrics.arrival_time if metrics.first_token_time is not None else None
            e2e_time = metrics.finished_time - metrics.arrival_time if metrics.finished_time is not None else None
            seq_span.set_attribute(SpanAttributes.GEN_AI_RESPONSE_MODEL, self.model_config.model)
            seq_span.set_attribute(SpanAttributes.GEN_AI_REQUEST_ID, seq_group.request_id)
            seq_span.set_attribute(SpanAttributes.GEN_AI_REQUEST_TEMPERATURE, seq_group.sampling_params.temperature)
            seq_span.set_attribute(SpanAttributes.GEN_AI_REQUEST_TOP_P, seq_group.sampling_params.top_p)
            seq_span.set_attribute(SpanAttributes.GEN_AI_REQUEST_MAX_TOKENS, seq_group.sampling_params.max_tokens)
            seq_span.set_attribute(SpanAttributes.GEN_AI_REQUEST_N, seq_group.sampling_params.n)
            seq_span.set_attribute(SpanAttributes.GEN_AI_USAGE_NUM_SEQUENCES, seq_group.num_seqs())
            seq_span.set_attribute(SpanAttributes.GEN_AI_USAGE_PROMPT_TOKENS, len(seq_group.prompt_token_ids))
            seq_span.set_attribute(SpanAttributes.GEN_AI_USAGE_COMPLETION_TOKENS, sum([seq.get_output_len() for seq in seq_group.get_finished_seqs()]))
            if metrics.time_in_queue is not None:
                seq_span.set_attribute(SpanAttributes.GEN_AI_LATENCY_TIME_IN_QUEUE, metrics.time_in_queue)
            if ttft is not None:
                seq_span.set_attribute(SpanAttributes.GEN_AI_LATENCY_TIME_TO_FIRST_TOKEN, ttft)
            if e2e_time is not None:
                seq_span.set_attribute(SpanAttributes.GEN_AI_LATENCY_E2E, e2e_time)
            if metrics.scheduler_time is not None:
                seq_span.set_attribute(SpanAttributes.GEN_AI_LATENCY_TIME_IN_SCHEDULER, metrics.scheduler_time)
            if metrics.model_forward_time is not None:
                seq_span.set_attribute(SpanAttributes.GEN_AI_LATENCY_TIME_IN_MODEL_FORWARD, metrics.model_forward_time / 1000.0)
            if metrics.model_execute_time is not None:
                seq_span.set_attribute(SpanAttributes.GEN_AI_LATENCY_TIME_IN_MODEL_EXECUTE, metrics.model_execute_time)
    def _validate_model_inputs(self, inputs: ProcessorInputs, lora_request: Optional[LoRARequest]):
        encoder_inputs, decoder_inputs = split_enc_dec_inputs(inputs)
        if encoder_inputs is not None:
            self._validate_model_input(encoder_inputs, lora_request, prompt_type='encoder')
        self._validate_model_input(decoder_inputs, lora_request, prompt_type='decoder')
    def _validate_model_input(self, prompt_inputs: SingletonInputs, lora_request: Optional[LoRARequest], *, prompt_type: Literal['encoder', 'decoder']):
        model_config = self.model_config
        tokenizer = None if self.tokenizer is None else self.tokenizer.get_lora_tokenizer(lora_request)
        prompt_ids = prompt_inputs.get('prompt_token_ids', [])
        if not prompt_ids:
            if prompt_type == 'encoder' and model_config.is_multimodal_model:
                pass
            elif prompt_inputs['type'] == 'embeds':
                pass
            else:
                raise ValueError(f'The {prompt_type} prompt cannot be empty')
        if tokenizer is not None:
            max_input_id = max(prompt_ids, default=0)
            if max_input_id > tokenizer.max_token_id:
                raise ValueError(f'Token id {max_input_id} is out of vocabulary')
        max_prompt_len = self.model_config.max_model_len
        if len(prompt_ids) > max_prompt_len:
            if prompt_type == 'encoder' and model_config.is_multimodal_model:
                mm_registry = self.input_preprocessor.mm_registry
                mm_processor = mm_registry.create_processor(model_config, tokenizer=tokenizer or object())
                assert isinstance(mm_processor, EncDecMultiModalProcessor)
                if mm_processor.pad_dummy_encoder_prompt:
                    return
            if model_config.is_multimodal_model:
                suggestion = 'Make sure that `max_model_len` is no smaller than the number of text tokens plus multimodal tokens. For image inputs, the number of image tokens depends on the number of images, and possibly their aspect ratios as well.'
            else:
                suggestion = 'Make sure that `max_model_len` is no smaller than the number of text tokens.'
            raise ValueError(f'The {prompt_type} prompt (length {len(prompt_ids)}) is longer than the maximum model length of {max_prompt_len}. {suggestion}')
    def _build_logits_processors(self, sampling_params: SamplingParams, lora_request: Optional[LoRARequest]) -> SamplingParams:
        logits_processors = []
        if sampling_params.logit_bias or sampling_params.allowed_token_ids:
            tokenizer = self.get_tokenizer(lora_request=lora_request)
            processors = get_openai_logits_processors(logit_bias=sampling_params.logit_bias, allowed_token_ids=sampling_params.allowed_token_ids, tokenizer=tokenizer)
            logits_processors.extend(processors)
            sampling_params.logit_bias = None
            sampling_params.allowed_token_ids = None
        if len(sampling_params.bad_words) > 0:
            tokenizer = self.get_tokenizer(lora_request)
            processors = get_bad_words_logits_processors(bad_words=sampling_params.bad_words, tokenizer=tokenizer)
            logits_processors.extend(processors)
        if logits_processors:
            if sampling_params.logits_processors is None:
                sampling_params.logits_processors = logits_processors
            else:
                sampling_params.logits_processors.extend(logits_processors)
        return sampling_params
    def collective_rpc(self, method: Union[str, Callable[..., _R]], timeout: Optional[float]=None, args: tuple=(), kwargs: Optional[dict[str, Any]]=None) -> list[_R]:
        return self.modeling.collective_rpc(method, timeout, args, kwargs)
if envs.is_set('APHRODITE_USE_V1') and envs.APHRODITE_USE_V1:
    from aphrodite.v1.engine.llm_engine import LLMEngine as V1AphroditeEngine
    AphroditeEngine = V1AphroditeEngine
setup_logger()