import itertools
from collections.abc import Sequence
from contextlib import contextmanager
from typing import TYPE_CHECKING, Any, Callable, ClassVar, Optional, Union, cast, overload
import cloudpickle
import torch.nn as nn
from loguru import logger
from pydantic import ValidationError
from tqdm.auto import tqdm
from typing_extensions import TypeVar, deprecated
import aphrodite.common.envs as envs
from aphrodite.common.beam_search import BeamSearchInstance, BeamSearchOutput, BeamSearchSequence, create_sort_beams_key_function
from aphrodite.common.config import CompilationConfig, ModelDType, TokenizerMode, is_init_field
from aphrodite.common.logger import log_once
from aphrodite.common.outputs import ClassificationRequestOutput, EmbeddingRequestOutput, PoolingRequestOutput, RequestOutput, ScoringRequestOutput
from aphrodite.common.pooling_params import PoolingParams
from aphrodite.common.sampling_params import BeamSearchParams, RequestOutputKind, SamplingParams
from aphrodite.endpoints.chat_utils import ChatCompletionMessageParam, ChatTemplateContentFormatOption, apply_hf_chat_template, apply_mistral_chat_template, parse_chat_messages, resolve_chat_template_content_format
from aphrodite.endpoints.score_utils import ScoreContentPartParam, ScoreMultiModalParam, _cosine_similarity, _validate_score_input_lens, get_score_prompt
from aphrodite.endpoints.utils import _validate_truncation_size, log_non_default_args
from aphrodite.engine.aphrodite_engine import AphroditeEngine
from aphrodite.engine.args_tools import ConvertOption, EngineArgs, HfOverrides, PoolerConfig, RunnerOption
from aphrodite.inputs import PromptType, SingletonPrompt, TextPrompt, TokensPrompt
from aphrodite.inputs.parse import parse_and_batch_prompt
from aphrodite.lora.request import LoRARequest
from aphrodite.quantization import QuantizationMethods
from aphrodite.tasks import PoolingTask
from aphrodite.transformers_utils.tokenizer import AnyTokenizer, MistralTokenizer, get_cached_tokenizer
from aphrodite.usage.usage_lib import UsageContext
from aphrodite.utils import Counter, Device, deprecate_kwargs, is_list_of
if TYPE_CHECKING:
    from aphrodite.v1.metrics.reader import Metric
_R = TypeVar('_R', default=Any)
class LLM:
    DEPRECATE_LEGACY: ClassVar[bool] = True
    'A flag to toggle whether to deprecate the legacy generate/encode API.'
    @classmethod
    @contextmanager
    def deprecate_legacy_api(cls):
        cls.DEPRECATE_LEGACY = True
        yield
        cls.DEPRECATE_LEGACY = False
    def __init__(self, model: str, *, runner: RunnerOption='auto', convert: ConvertOption='auto', tokenizer: Optional[str]=None, tokenizer_mode: TokenizerMode='auto', skip_tokenizer_init: bool=False, trust_remote_code: bool=False, allowed_local_media_path: str='', tensor_parallel_size: int=1, dtype: ModelDType='auto', quantization: Optional[QuantizationMethods]=None, revision: Optional[str]=None, tokenizer_revision: Optional[str]=None, seed: Optional[int]=None, gpu_memory_utilization: float=0.9, swap_space: float=4, cpu_offload_gb: float=0, enforce_eager: bool=False, max_seq_len_to_capture: int=8192, disable_custom_all_reduce: bool=False, disable_async_output_proc: bool=False, hf_token: Optional[Union[bool, str]]=None, hf_overrides: Optional[HfOverrides]=None, mm_processor_kwargs: Optional[dict[str, Any]]=None, override_pooler_config: Optional[PoolerConfig]=None, compilation_config: Optional[Union[int, dict[str, Any], CompilationConfig]]=None, **kwargs) -> None:
        if 'disable_log_stats' not in kwargs:
            kwargs['disable_log_stats'] = True
        if 'worker_cls' in kwargs:
            worker_cls = kwargs['worker_cls']
            if isinstance(worker_cls, type):
                kwargs['worker_cls'] = cloudpickle.dumps(worker_cls)
        if 'kv_transfer_config' in kwargs and isinstance(kwargs['kv_transfer_config'], dict):
            from aphrodite.common.config import KVTransferConfig
            raw_config_dict = kwargs['kv_transfer_config']
            try:
                kwargs['kv_transfer_config'] = KVTransferConfig(**raw_config_dict)
            except ValidationError as e:
                logger.error("Failed to convert 'kv_transfer_config' dict to KVTransferConfig object. Dict: {}. Error: {}", raw_config_dict, e)
                raise ValueError(f"Invalid 'kv_transfer_config' provided: {e}") from e
        if hf_overrides is None:
            hf_overrides = {}
        if compilation_config is not None:
            if isinstance(compilation_config, int):
                compilation_config_instance = CompilationConfig(level=compilation_config)
            elif isinstance(compilation_config, dict):
                predicate = lambda x: is_init_field(CompilationConfig, x[0])
                compilation_config_instance = CompilationConfig(**dict(filter(predicate, compilation_config.items())))
            else:
                compilation_config_instance = compilation_config
        else:
            compilation_config_instance = CompilationConfig()
        engine_args = EngineArgs(model=model, runner=runner, convert=convert, tokenizer=tokenizer, tokenizer_mode=tokenizer_mode, skip_tokenizer_init=skip_tokenizer_init, trust_remote_code=trust_remote_code, allowed_local_media_path=allowed_local_media_path, tensor_parallel_size=tensor_parallel_size, dtype=dtype, quantization=quantization, revision=revision, tokenizer_revision=tokenizer_revision, seed=seed, gpu_memory_utilization=gpu_memory_utilization, swap_space=swap_space, cpu_offload_gb=cpu_offload_gb, enforce_eager=enforce_eager, max_seq_len_to_capture=max_seq_len_to_capture, disable_custom_all_reduce=disable_custom_all_reduce, disable_async_output_proc=disable_async_output_proc, hf_token=hf_token, hf_overrides=hf_overrides, mm_processor_kwargs=mm_processor_kwargs, override_pooler_config=override_pooler_config, compilation_config=compilation_config_instance, **kwargs)
        log_non_default_args(engine_args)
        self.llm_engine = AphroditeEngine.from_engine_args(engine_args=engine_args, usage_context=UsageContext.LLM_CLASS)
        self.engine_class = type(self.llm_engine)
        self.request_counter = Counter()
        self.default_sampling_params: Union[dict[str, Any], None] = None
        if envs.APHRODITE_USE_V1:
            supported_tasks = self.llm_engine.get_supported_tasks()
        else:
            supported_tasks = self.llm_engine.model_config.supported_tasks
        logger.info('Supported_tasks: {}', supported_tasks)
        self.supported_tasks = supported_tasks
    def get_tokenizer(self, lora_request: Optional[LoRARequest]=None) -> AnyTokenizer:
        return self.llm_engine.get_tokenizer_group().get_lora_tokenizer(lora_request)
    def set_tokenizer(self, tokenizer: AnyTokenizer) -> None:
        tokenizer_group = self.llm_engine.get_tokenizer_group()
        if tokenizer.__class__.__name__.startswith('Cached'):
            tokenizer_group.tokenizer = tokenizer
        else:
            tokenizer_group.tokenizer = get_cached_tokenizer(tokenizer)
    def get_default_sampling_params(self) -> SamplingParams:
        if self.default_sampling_params is None:
            self.default_sampling_params = self.llm_engine.model_config.get_diff_sampling_param()
        if self.default_sampling_params:
            return SamplingParams.from_optional(**self.default_sampling_params)
        return SamplingParams()
    @overload
    def generate(self, prompts: Union[PromptType, Sequence[PromptType]], /, sampling_params: Optional[Union[SamplingParams, Sequence[SamplingParams]]]=None, *, use_tqdm: Union[bool, Callable[..., tqdm]]=True, lora_request: Optional[Union[list[LoRARequest], LoRARequest]]=None) -> list[RequestOutput]:
        ...
    @overload
    @deprecated("'prompt_token_ids' will become part of 'prompts'")
    def generate(self, prompts: str, sampling_params: Optional[Union[SamplingParams, list[SamplingParams]]]=None, prompt_token_ids: Optional[list[int]]=None, use_tqdm: Union[bool, Callable[..., tqdm]]=True, lora_request: Optional[Union[list[LoRARequest], LoRARequest]]=None) -> list[RequestOutput]:
        ...
    @overload
    @deprecated("'prompt_token_ids' will become part of 'prompts'")
    def generate(self, prompts: list[str], sampling_params: Optional[Union[SamplingParams, list[SamplingParams]]]=None, prompt_token_ids: Optional[list[list[int]]]=None, use_tqdm: Union[bool, Callable[..., tqdm]]=True, lora_request: Optional[Union[list[LoRARequest], LoRARequest]]=None) -> list[RequestOutput]:
        ...
    @overload
    @deprecated("'prompt_token_ids' will become part of 'prompts'")
    def generate(self, prompts: Optional[str]=None, sampling_params: Optional[Union[SamplingParams, list[SamplingParams]]]=None, *, prompt_token_ids: list[int], use_tqdm: Union[bool, Callable[..., tqdm]]=True, lora_request: Optional[Union[list[LoRARequest], LoRARequest]]=None) -> list[RequestOutput]:
        ...
    @overload
    @deprecated("'prompt_token_ids' will become part of 'prompts'")
    def generate(self, prompts: Optional[list[str]]=None, sampling_params: Optional[Union[SamplingParams, list[SamplingParams]]]=None, *, prompt_token_ids: list[list[int]], use_tqdm: Union[bool, Callable[..., tqdm]]=True, lora_request: Optional[Union[list[LoRARequest], LoRARequest]]=None) -> list[RequestOutput]:
        ...
    @overload
    @deprecated("'prompt_token_ids' will become part of 'prompts'")
    def generate(self, prompts: None, sampling_params: None, prompt_token_ids: Union[list[int], list[list[int]]], use_tqdm: Union[bool, Callable[..., tqdm]]=True, lora_request: Optional[Union[list[LoRARequest], LoRARequest]]=None) -> list[RequestOutput]:
        ...
    @deprecate_kwargs('prompt_token_ids', is_deprecated=lambda: LLM.DEPRECATE_LEGACY, additional_message="Please use the 'prompts' parameter instead.")
    def generate(self, prompts: Union[Union[PromptType, Sequence[PromptType]], Optional[Union[str, list[str]]]]=None, sampling_params: Optional[Union[SamplingParams, Sequence[SamplingParams]]]=None, prompt_token_ids: Optional[Union[list[int], list[list[int]]]]=None, use_tqdm: Union[bool, Callable[..., tqdm]]=True, lora_request: Optional[Union[list[LoRARequest], LoRARequest]]=None, priority: Optional[list[int]]=None) -> list[RequestOutput]:
        model_config = self.llm_engine.model_config
        runner_type = model_config.runner_type
        if runner_type != 'generate':
            raise ValueError('LLM.generate() is only supported for generative models. Try passing `--runner generate` to use the model as a generative model.')
        if prompt_token_ids is not None:
            parsed_prompts = self._convert_v1_inputs(prompts=cast(Optional[Union[str, list[str]]], prompts), prompt_token_ids=prompt_token_ids)
        else:
            parsed_prompts = cast(Union[PromptType, Sequence[PromptType]], prompts)
        if sampling_params is None:
            sampling_params = self.get_default_sampling_params()
        tokenization_kwargs: dict[str, Any] = {}
        truncate_prompt_tokens = None
        if isinstance(sampling_params, SamplingParams):
            truncate_prompt_tokens = sampling_params.truncate_prompt_tokens
        _validate_truncation_size(model_config.max_model_len, truncate_prompt_tokens, tokenization_kwargs)
        lora_request = self._get_modality_specific_lora_reqs(parsed_prompts, lora_request)
        self._validate_and_add_requests(prompts=parsed_prompts, params=sampling_params, use_tqdm=use_tqdm, lora_request=lora_request, tokenization_kwargs=tokenization_kwargs, priority=priority)
        outputs = self._run_engine(use_tqdm=use_tqdm)
        return self.engine_class.validate_outputs(outputs, RequestOutput)
    def _get_modality_specific_lora_reqs(self, parsed_prompts: Union[PromptType, Sequence[PromptType]], lora_request: Optional[Union[list[LoRARequest], LoRARequest]]):
        lora_config = self.llm_engine.aphrodite_config.lora_config
        if lora_config is None or not self.llm_engine.model_config.is_multimodal_model or (lora_config and lora_config.default_mm_loras is None):
            return lora_request
        if not isinstance(parsed_prompts, Sequence):
            parsed_prompts = [parsed_prompts]
        optional_loras = [lora_request] * len(parsed_prompts) if not isinstance(lora_request, Sequence) else lora_request
        return [self._resolve_single_prompt_mm_lora(parsed_prompt, opt_lora_req, lora_config.default_mm_loras) for parsed_prompt, opt_lora_req in zip(parsed_prompts, optional_loras)]
    def _resolve_single_prompt_mm_lora(self, parsed_prompt: PromptType, lora_request: Optional[LoRARequest], default_mm_loras: Optional[dict[str, str]]):
        if not default_mm_loras or not isinstance(parsed_prompt, dict) or 'multi_modal_data' not in parsed_prompt:
            return lora_request
        parsed_prompt = cast(Union[TextPrompt, TokensPrompt], parsed_prompt)
        intersection = set(parsed_prompt['multi_modal_data'].keys()).intersection(default_mm_loras.keys())
        if not intersection:
            return lora_request
        if len(intersection) > 1:
            logger.warning('Multiple modality specific loras were registered and would be used by a single prompt consuming several modalities;  currently we only support one lora per request; as such, lora(s) registered with modalities: {} will be skipped', intersection)
            return lora_request
        modality_name = intersection.pop()
        modality_lora_path = default_mm_loras[modality_name]
        modality_lora_id = sorted(default_mm_loras).index(modality_name) + 1
        if lora_request:
            if lora_request.lora_int_id != modality_lora_id:
                logger.warning('A modality with a registered lora and a lora_request with a different ID were provided; falling back to the lora_request as we only apply one LoRARequest per prompt')
            return lora_request
        return LoRARequest(modality_name, modality_lora_id, modality_lora_path)
    def collective_rpc(self, method: Union[str, Callable[..., _R]], timeout: Optional[float]=None, args: tuple=(), kwargs: Optional[dict[str, Any]]=None) -> list[_R]:
        return self.llm_engine.collective_rpc(method, timeout, args, kwargs)
    def apply_model(self, func: Callable[[nn.Module], _R]) -> list[_R]:
        executor = self.llm_engine.model_executor
        return executor.apply_model(func)
    def _get_beam_search_lora_requests(self, lora_request: Optional[Union[list[LoRARequest], LoRARequest]], prompts: list[Union[TokensPrompt, TextPrompt]]) -> list[Optional[LoRARequest]]:
        if isinstance(lora_request, Sequence) and len(lora_request) != len(prompts):
            raise ValueError('Lora request list should be the same length as the prompts')
        if lora_request is None or isinstance(lora_request, LoRARequest):
            return [lora_request] * len(prompts)
        raise TypeError(f'Invalid lora_request type {type(lora_request)}')
    def beam_search(self, prompts: list[Union[TokensPrompt, TextPrompt]], params: BeamSearchParams, lora_request: Optional[Union[list[LoRARequest], LoRARequest]]=None, use_tqdm: bool=False) -> list[BeamSearchOutput]:
        beam_width = params.beam_width
        max_tokens = params.max_tokens
        temperature = params.temperature
        ignore_eos = params.ignore_eos
        length_penalty = params.length_penalty
        lora_requests = self._get_beam_search_lora_requests(lora_request, prompts)
        tokenizer = self.get_tokenizer()
        sort_beams_key = create_sort_beams_key_function(tokenizer.eos_token_id, length_penalty)
        def create_tokens_prompt_from_beam(beam: BeamSearchSequence) -> TokensPrompt:
            token_prompt_kwargs: TokensPrompt = {'prompt_token_ids': beam.tokens}
            if beam.multi_modal_data is not None:
                token_prompt_kwargs['multi_modal_data'] = beam.multi_modal_data
            if beam.mm_processor_kwargs is not None:
                token_prompt_kwargs['mm_processor_kwargs'] = beam.mm_processor_kwargs
            return TokensPrompt(**token_prompt_kwargs)
        beam_search_params = SamplingParams(logprobs=2 * beam_width, max_tokens=1, temperature=temperature)
        instances: list[BeamSearchInstance] = []
        for lora_req, prompt in zip(lora_requests, prompts):
            mm_kwargs = {}
            if 'multi_modal_data' in prompt:
                mm_kwargs['multi_modal_data'] = prompt['multi_modal_data']
            if 'mm_processor_kwargs' in prompt:
                mm_kwargs['mm_processor_kwargs'] = prompt['mm_processor_kwargs']
            if 'prompt_token_ids' in prompt:
                prompt = cast(TokensPrompt, prompt)
                prompt_tokens = prompt['prompt_token_ids']
            else:
                prompt_tokens = tokenizer.encode(prompt['prompt'])
            instances.append(BeamSearchInstance(prompt_tokens, lora_request=lora_req, logprobs=None, **mm_kwargs))
        token_iter = range(max_tokens)
        if use_tqdm:
            token_iter = tqdm(token_iter, desc='Beam search', unit='token', unit_scale=False)
            logger.warning('The progress bar shows the upper bound on token steps and may finish early due to stopping conditions. It does not reflect instance-level progress.')
        for _ in token_iter:
            all_beams: list[BeamSearchSequence] = list(sum((instance.beams for instance in instances), []))
            pos = [0] + list(itertools.accumulate((len(instance.beams) for instance in instances)))
            instance_start_and_end: list[tuple[int, int]] = list(zip(pos[:-1], pos[1:]))
            if len(all_beams) == 0:
                break
            prompts_batch, lora_req_batch = zip(*[(create_tokens_prompt_from_beam(beam), beam.lora_request) for beam in all_beams])
            output = self.generate(prompts_batch, sampling_params=beam_search_params, use_tqdm=False, lora_request=lora_req_batch)
            for (start, end), instance in zip(instance_start_and_end, instances):
                instance_new_beams = []
                for i in range(start, end):
                    current_beam = all_beams[i]
                    result = output[i]
                    if result.outputs[0].logprobs is not None:
                        logprobs = result.outputs[0].logprobs[0]
                        for token_id, logprob_obj in logprobs.items():
                            new_beam = BeamSearchSequence(tokens=current_beam.tokens + [token_id], logprobs=current_beam.logprobs + [logprobs], lora_request=current_beam.lora_request, cum_logprob=current_beam.cum_logprob + logprob_obj.logprob, multi_modal_data=current_beam.multi_modal_data, mm_processor_kwargs=current_beam.mm_processor_kwargs)
                            if token_id == tokenizer.eos_token_id and (not ignore_eos):
                                instance.completed.append(new_beam)
                            else:
                                instance_new_beams.append(new_beam)
                sorted_beams = sorted(instance_new_beams, key=sort_beams_key, reverse=True)
                instance.beams = sorted_beams[:beam_width]
        outputs = []
        for instance in instances:
            instance.completed.extend(instance.beams)
            sorted_completed = sorted(instance.completed, key=sort_beams_key, reverse=True)
            best_beams = sorted_completed[:beam_width]
            for beam in best_beams:
                beam.text = tokenizer.decode(beam.tokens)
            outputs.append(BeamSearchOutput(sequences=best_beams))
        return outputs
    def chat(self, messages: Union[list[ChatCompletionMessageParam], list[list[ChatCompletionMessageParam]]], sampling_params: Optional[Union[SamplingParams, list[SamplingParams]]]=None, use_tqdm: Union[bool, Callable[..., tqdm]]=True, lora_request: Optional[LoRARequest]=None, chat_template: Optional[str]=None, chat_template_content_format: ChatTemplateContentFormatOption='auto', add_generation_prompt: bool=True, continue_final_message: bool=False, tools: Optional[list[dict[str, Any]]]=None, chat_template_kwargs: Optional[dict[str, Any]]=None, mm_processor_kwargs: Optional[dict[str, Any]]=None) -> list[RequestOutput]:
        list_of_messages: list[list[ChatCompletionMessageParam]]
        if is_list_of(messages, list):
            list_of_messages = cast(list[list[ChatCompletionMessageParam]], messages)
        else:
            list_of_messages = [cast(list[ChatCompletionMessageParam], messages)]
        tokenizer = self.get_tokenizer(lora_request)
        model_config = self.llm_engine.get_model_config()
        resolved_content_format = resolve_chat_template_content_format(chat_template, tools, chat_template_content_format, tokenizer, model_config=model_config)
        _chat_template_kwargs: dict[str, Any] = dict(chat_template=chat_template, add_generation_prompt=add_generation_prompt, continue_final_message=continue_final_message, tools=tools)
        _chat_template_kwargs.update(chat_template_kwargs or {})
        prompts: list[Union[TokensPrompt, TextPrompt]] = []
        for msgs in list_of_messages:
            conversation, mm_data = parse_chat_messages(msgs, model_config, tokenizer, content_format=resolved_content_format)
            if isinstance(tokenizer, MistralTokenizer):
                prompt_token_ids = apply_mistral_chat_template(tokenizer, messages=msgs, **_chat_template_kwargs)
            else:
                prompt_str = apply_hf_chat_template(tokenizer=tokenizer, conversation=conversation, model_config=model_config, **_chat_template_kwargs)
                prompt_token_ids = tokenizer.encode(prompt_str, add_special_tokens=False)
            prompt = TokensPrompt(prompt_token_ids=prompt_token_ids)
            if mm_data is not None:
                prompt['multi_modal_data'] = mm_data
            if mm_processor_kwargs is not None:
                prompt['mm_processor_kwargs'] = mm_processor_kwargs
            prompts.append(prompt)
        return self.generate(prompts, sampling_params=sampling_params, use_tqdm=use_tqdm, lora_request=lora_request)
    @overload
    def encode(self, prompts: Union[PromptType, Sequence[PromptType]], /, pooling_params: Optional[Union[PoolingParams, Sequence[PoolingParams]]]=None, *, truncate_prompt_tokens: Optional[int]=None, use_tqdm: Union[bool, Callable[..., tqdm]]=True, lora_request: Optional[Union[list[LoRARequest], LoRARequest]]=None, pooling_task: PoolingTask='encode', tokenization_kwargs: Optional[dict[str, Any]]=None) -> list[PoolingRequestOutput]:
        ...
    @overload
    @deprecated("'prompt_token_ids' will become part of 'prompts'")
    def encode(self, prompts: str, pooling_params: Optional[Union[PoolingParams, Sequence[PoolingParams]]]=None, prompt_token_ids: Optional[list[int]]=None, truncate_prompt_tokens: Optional[int]=None, use_tqdm: Union[bool, Callable[..., tqdm]]=True, lora_request: Optional[Union[list[LoRARequest], LoRARequest]]=None, pooling_task: PoolingTask='encode', tokenization_kwargs: Optional[dict[str, Any]]=None) -> list[PoolingRequestOutput]:
        ...
    @overload
    @deprecated("'prompt_token_ids' will become part of 'prompts'")
    def encode(self, prompts: list[str], pooling_params: Optional[Union[PoolingParams, Sequence[PoolingParams]]]=None, prompt_token_ids: Optional[list[list[int]]]=None, truncate_prompt_tokens: Optional[int]=None, use_tqdm: Union[bool, Callable[..., tqdm]]=True, lora_request: Optional[Union[list[LoRARequest], LoRARequest]]=None, pooling_task: PoolingTask='encode', tokenization_kwargs: Optional[dict[str, Any]]=None) -> list[PoolingRequestOutput]:
        ...
    @overload
    @deprecated("'prompt_token_ids' will become part of 'prompts'")
    def encode(self, prompts: Optional[str]=None, pooling_params: Optional[Union[PoolingParams, Sequence[PoolingParams]]]=None, *, prompt_token_ids: list[int], truncate_prompt_tokens: Optional[int]=None, use_tqdm: Union[bool, Callable[..., tqdm]]=True, lora_request: Optional[Union[list[LoRARequest], LoRARequest]]=None, pooling_task: PoolingTask='encode', tokenization_kwargs: Optional[dict[str, Any]]=None) -> list[PoolingRequestOutput]:
        ...
    @overload
    @deprecated("'prompt_token_ids' will become part of 'prompts'")
    def encode(self, prompts: Optional[list[str]]=None, pooling_params: Optional[Union[PoolingParams, Sequence[PoolingParams]]]=None, *, prompt_token_ids: list[list[int]], truncate_prompt_tokens: Optional[int]=None, use_tqdm: Union[bool, Callable[..., tqdm]]=True, lora_request: Optional[Union[list[LoRARequest], LoRARequest]]=None, pooling_task: PoolingTask='encode', tokenization_kwargs: Optional[dict[str, Any]]=None) -> list[PoolingRequestOutput]:
        ...
    @overload
    @deprecated("'prompt_token_ids' will become part of 'prompts'")
    def encode(self, prompts: None, pooling_params: None, prompt_token_ids: Union[list[int], list[list[int]]], truncate_prompt_tokens: Optional[int]=None, use_tqdm: Union[bool, Callable[..., tqdm]]=True, lora_request: Optional[Union[list[LoRARequest], LoRARequest]]=None, pooling_task: PoolingTask='encode', tokenization_kwargs: Optional[dict[str, Any]]=None) -> list[PoolingRequestOutput]:
        ...
    @deprecate_kwargs('prompt_token_ids', is_deprecated=lambda: LLM.DEPRECATE_LEGACY, additional_message="Please use the 'prompts' parameter instead.")
    def encode(self, prompts: Union[Union[PromptType, Sequence[PromptType]], Optional[Union[str, list[str]]]]=None, pooling_params: Optional[Union[PoolingParams, Sequence[PoolingParams]]]=None, prompt_token_ids: Optional[Union[list[int], list[list[int]]]]=None, truncate_prompt_tokens: Optional[int]=None, use_tqdm: Union[bool, Callable[..., tqdm]]=True, lora_request: Optional[Union[list[LoRARequest], LoRARequest]]=None, pooling_task: Optional[PoolingTask]=None, tokenization_kwargs: Optional[dict[str, Any]]=None) -> list[PoolingRequestOutput]:
        if pooling_task is None:
            if 'embed' in self.supported_tasks:
                pooling_task = 'embed'
            else:
                pooling_task = 'encode'
            log_once('WARNING', '`LLM.encode` is currently using `pooling_task = {}`.\nPlease use one of the more specific methods or set the task directly when using `LLM.encode`:\n  - For embeddings, use `LLM.embed(...)` or `pooling_task="embed"`.\n  - For classification logits, use `LLM.classify(...)` or `pooling_task="classify"`.\n  - For rewards, use `LLM.reward(...)` or `pooling_task="reward"`\n  - For similarity scores, use `LLM.score(...)`.', pooling_task)
        model_config = self.llm_engine.model_config
        runner_type = model_config.runner_type
        if runner_type != 'pooling':
            raise ValueError('LLM.encode() is only supported for pooling models. Try passing `--runner pooling` to use the model as a pooling model.')
        if prompt_token_ids is not None:
            parsed_prompts = self._convert_v1_inputs(prompts=cast(Optional[Union[str, list[str]]], prompts), prompt_token_ids=prompt_token_ids)
        else:
            parsed_prompts = cast(Union[PromptType, Sequence[PromptType]], prompts)
        if pooling_params is None:
            pooling_params = PoolingParams()
        if isinstance(pooling_params, PoolingParams):
            pooling_params.verify(pooling_task, model_config)
        else:
            for pooling_param in pooling_params:
                pooling_param.verify(pooling_task, model_config)
        if tokenization_kwargs is None:
            tokenization_kwargs = dict[str, Any]()
            _validate_truncation_size(model_config.max_model_len, truncate_prompt_tokens, tokenization_kwargs)
        self._validate_and_add_requests(prompts=parsed_prompts, params=pooling_params, use_tqdm=use_tqdm, lora_request=lora_request, tokenization_kwargs=tokenization_kwargs)
        outputs = self._run_engine(use_tqdm=use_tqdm)
        return self.engine_class.validate_outputs(outputs, PoolingRequestOutput)
    def embed(self, prompts: Union[PromptType, Sequence[PromptType]], /, *, truncate_prompt_tokens: Optional[int]=None, use_tqdm: Union[bool, Callable[..., tqdm]]=True, pooling_params: Optional[Union[PoolingParams, Sequence[PoolingParams]]]=None, lora_request: Optional[Union[list[LoRARequest], LoRARequest]]=None) -> list[EmbeddingRequestOutput]:
        if 'embed' not in self.supported_tasks:
            raise ValueError('Embedding API is not supported by this model. Try converting the model using `--convert embed`.')
        items = self.encode(prompts, truncate_prompt_tokens=truncate_prompt_tokens, use_tqdm=use_tqdm, pooling_params=pooling_params, lora_request=lora_request, pooling_task='embed')
        return [EmbeddingRequestOutput.from_base(item) for item in items]
    def classify(self, prompts: Union[PromptType, Sequence[PromptType]], /, *, use_tqdm: Union[bool, Callable[..., tqdm]]=True, pooling_params: Optional[Union[PoolingParams, Sequence[PoolingParams]]]=None, lora_request: Optional[Union[list[LoRARequest], LoRARequest]]=None) -> list[ClassificationRequestOutput]:
        if 'classify' not in self.supported_tasks:
            raise ValueError('Classification API is not supported by this model. Try converting the model using `--convert classify`.')
        items = self.encode(prompts, use_tqdm=use_tqdm, pooling_params=pooling_params, lora_request=lora_request, pooling_task='classify')
        return [ClassificationRequestOutput.from_base(item) for item in items]
    def reward(self, prompts: Union[PromptType, Sequence[PromptType]], /, *, truncate_prompt_tokens: Optional[int]=None, use_tqdm: Union[bool, Callable[..., tqdm]]=True, pooling_params: Optional[Union[PoolingParams, Sequence[PoolingParams]]]=None, lora_request: Optional[Union[list[LoRARequest], LoRARequest]]=None) -> list[PoolingRequestOutput]:
        return self.encode(prompts, use_tqdm=use_tqdm, lora_request=lora_request, pooling_params=pooling_params, truncate_prompt_tokens=truncate_prompt_tokens, pooling_task='encode')
    def _embedding_score(self, tokenizer: AnyTokenizer, text_1: list[Union[str, TextPrompt, TokensPrompt]], text_2: list[Union[str, TextPrompt, TokensPrompt]], truncate_prompt_tokens: Optional[int]=None, use_tqdm: Union[bool, Callable[..., tqdm]]=True, pooling_params: Optional[PoolingParams]=None, lora_request: Optional[Union[list[LoRARequest], LoRARequest]]=None) -> list[ScoringRequestOutput]:
        encoded_output: list[PoolingRequestOutput] = self.encode(text_1 + text_2, truncate_prompt_tokens=truncate_prompt_tokens, use_tqdm=use_tqdm, lora_request=lora_request, pooling_params=pooling_params, pooling_task='embed')
        encoded_output_1: list[PoolingRequestOutput] = encoded_output[0:len(text_1)]
        encoded_output_2: list[PoolingRequestOutput] = encoded_output[len(text_1):]
        if len(encoded_output_1) == 1:
            encoded_output_1 = encoded_output_1 * len(encoded_output_2)
        scores = _cosine_similarity(tokenizer=tokenizer, embed_1=encoded_output_1, embed_2=encoded_output_2)
        items = self.engine_class.validate_outputs(scores, PoolingRequestOutput)
        return [ScoringRequestOutput.from_base(item) for item in items]
    def _cross_encoding_score(self, tokenizer: AnyTokenizer, data_1: Union[list[str], list[ScoreContentPartParam]], data_2: Union[list[str], list[ScoreContentPartParam]], truncate_prompt_tokens: Optional[int]=None, use_tqdm: Union[bool, Callable[..., tqdm]]=True, pooling_params: Optional[PoolingParams]=None, lora_request: Optional[Union[list[LoRARequest], LoRARequest]]=None) -> list[ScoringRequestOutput]:
        model_config = self.llm_engine.model_config
        if isinstance(tokenizer, MistralTokenizer):
            raise ValueError('Score API is not supported for Mistral tokenizer')
        if len(data_1) == 1:
            data_1 = data_1 * len(data_2)
        if pooling_params is None:
            pooling_params = PoolingParams(task='score')
        model_config = self.llm_engine.model_config
        pooling_params.verify('score', model_config)
        tokenization_kwargs: dict[str, Any] = {}
        _validate_truncation_size(model_config.max_model_len, truncate_prompt_tokens, tokenization_kwargs)
        parsed_prompts = []
        input_pairs = [(t1, t2) for t1, t2 in zip(data_1, data_2)]
        if model_config.is_multimodal_model:
            for q, d in input_pairs:
                _, engine_prompt = get_score_prompt(model_config=model_config, data_1=q, data_2=d, tokenizer=tokenizer, tokenization_kwargs=tokenization_kwargs)
                parsed_prompts.append(engine_prompt)
        else:
            for q, t in input_pairs:
                if model_config.use_pad_token:
                    prompt_inputs = tokenizer(text=q, text_pair=t, **tokenization_kwargs)
                else:
                    prompt_inputs = tokenizer(text=q + t, **tokenization_kwargs)
                engine_prompt = TokensPrompt(prompt_token_ids=prompt_inputs['input_ids'], token_type_ids=prompt_inputs.get('token_type_ids'))
                parsed_prompts.append(engine_prompt)
        self._validate_and_add_requests(prompts=parsed_prompts, params=pooling_params, use_tqdm=use_tqdm, lora_request=lora_request)
        outputs = self._run_engine(use_tqdm=use_tqdm)
        items = self.engine_class.validate_outputs(outputs, PoolingRequestOutput)
        return [ScoringRequestOutput.from_base(item) for item in items]
    def score(self, data_1: Union[SingletonPrompt, Sequence[SingletonPrompt], ScoreMultiModalParam], data_2: Union[SingletonPrompt, Sequence[SingletonPrompt], ScoreMultiModalParam], /, *, truncate_prompt_tokens: Optional[int]=None, use_tqdm: Union[bool, Callable[..., tqdm]]=True, pooling_params: Optional[PoolingParams]=None, lora_request: Optional[Union[list[LoRARequest], LoRARequest]]=None) -> list[ScoringRequestOutput]:
        model_config = self.llm_engine.model_config
        runner_type = model_config.runner_type
        if runner_type != 'pooling':
            raise ValueError('LLM.score() is only supported for pooling models. Try passing `--runner pooling` to use the model as a pooling model.')
        supported_tasks = self.supported_tasks
        if all((t not in supported_tasks for t in ('embed', 'classify'))):
            raise ValueError('Score API is not supported by this model. Try converting the model using `--convert embed` or `--convert classify`.')
        if model_config.is_cross_encoder and getattr(model_config.hf_config, 'num_labels', 0) != 1:
            raise ValueError('Score API is only enabled for num_labels == 1.')
        tokenizer = self.get_tokenizer()
        if not model_config.is_multimodal_model:
            def check_data_type(data: Union[SingletonPrompt, Sequence[SingletonPrompt], ScoreMultiModalParam]):
                if isinstance(data, dict) and 'content' in data:
                    raise ValueError(f'ScoreMultiModalParam is not supported for {model_config.architecture}')
            check_data_type(data_1)
            check_data_type(data_2)
            def ensure_str(prompt: SingletonPrompt):
                if isinstance(prompt, dict):
                    if 'multi_modal_data' in prompt:
                        raise ValueError('Multi-modal prompt is not supported for scoring')
                    elif 'prompt_token_ids' in prompt:
                        prompt = tokenizer.decode(cast(TokensPrompt, prompt)['prompt_token_ids'])
                    elif 'prompt' in prompt:
                        prompt = cast(TextPrompt, prompt)['prompt']
                assert type(prompt) is str
                return prompt
            if isinstance(data_1, (str, dict)):
                data_1 = [data_1]
            data_1 = [ensure_str(t) for t in data_1]
            if isinstance(data_2, (str, dict)):
                data_2 = [data_2]
            data_2 = [ensure_str(t) for t in data_2]
        if isinstance(data_1, dict) and 'content' in data_1:
            data_1 = data_1.get('content')
        elif isinstance(data_1, str):
            data_1 = [data_1]
        if isinstance(data_2, dict) and 'content' in data_2:
            data_2 = data_2.get('content')
        elif isinstance(data_2, str):
            data_2 = [data_2]
        _validate_score_input_lens(data_1, data_2)
        if model_config.is_cross_encoder:
            return self._cross_encoding_score(tokenizer, data_1, data_2, truncate_prompt_tokens, use_tqdm, pooling_params, lora_request)
        else:
            return self._embedding_score(tokenizer, data_1, data_2, truncate_prompt_tokens, use_tqdm, pooling_params, lora_request)
    def start_profile(self) -> None:
        self.llm_engine.start_profile()
    def stop_profile(self) -> None:
        self.llm_engine.stop_profile()
    def reset_prefix_cache(self, device: Optional[Device]=None) -> bool:
        return self.llm_engine.reset_prefix_cache(device)
    def sleep(self, level: int=1):
        self.reset_prefix_cache()
        self.llm_engine.sleep(level=level)
    def wake_up(self, tags: Optional[list[str]]=None):
        self.llm_engine.wake_up(tags)
    def get_metrics(self) -> list['Metric']:
        from aphrodite.v1.engine.llm_engine import AphroditeEngine as V1LLMEngine
        assert isinstance(self.llm_engine, V1LLMEngine)
        return self.llm_engine.get_metrics()
    def _convert_v1_inputs(self, prompts: Optional[Union[str, list[str]]], prompt_token_ids: Optional[Union[list[int], list[list[int]]]]):
        if prompts is None and prompt_token_ids is None:
            raise ValueError('Either prompts or prompt_token_ids must be provided.')
        if prompts is not None and prompt_token_ids is not None and (len(prompts) != len(prompt_token_ids)):
            raise ValueError('The lengths of prompts and prompt_token_ids must be the same.')
        if prompts is not None:
            prompts = [p['content'] for p in parse_and_batch_prompt(prompts)]
        if prompt_token_ids is not None:
            prompt_token_ids = [p['content'] for p in parse_and_batch_prompt(prompt_token_ids)]
        if prompts is not None:
            num_requests = len(prompts)
        elif prompt_token_ids is not None:
            num_requests = len(prompt_token_ids)
        parsed_prompts: list[PromptType] = []
        for i in range(num_requests):
            item: PromptType
            if prompts is not None:
                item = TextPrompt(prompt=prompts[i])
            elif prompt_token_ids is not None:
                item = TokensPrompt(prompt_token_ids=prompt_token_ids[i])
            else:
                raise AssertionError
            parsed_prompts.append(item)
        return parsed_prompts
    def _validate_and_add_requests(self, prompts: Union[PromptType, Sequence[PromptType]], params: Union[SamplingParams, Sequence[SamplingParams], PoolingParams, Sequence[PoolingParams]], *, use_tqdm: Union[bool, Callable[..., tqdm]]=True, lora_request: Optional[Union[Sequence[LoRARequest], LoRARequest]], tokenization_kwargs: Optional[dict[str, Any]]=None, priority: Optional[list[int]]=None) -> None:
        if isinstance(prompts, (str, dict)):
            prompts = [prompts]
        num_requests = len(prompts)
        if isinstance(params, Sequence) and len(params) != num_requests:
            raise ValueError('The lengths of prompts and params must be the same.')
        if isinstance(lora_request, Sequence) and len(lora_request) != num_requests:
            raise ValueError('The lengths of prompts and lora_request must be the same.')
        for sp in params if isinstance(params, Sequence) else (params,):
            if isinstance(sp, SamplingParams):
                sp.output_kind = RequestOutputKind.FINAL_ONLY
        it = prompts
        if use_tqdm:
            tqdm_func = use_tqdm if callable(use_tqdm) else tqdm
            it = tqdm_func(it, desc='Adding requests')
        for i, prompt in enumerate(it):
            self._add_request(prompt, params[i] if isinstance(params, Sequence) else params, tokenization_kwargs=tokenization_kwargs, lora_request=lora_request[i] if isinstance(lora_request, Sequence) else lora_request, priority=priority[i] if priority else 0)
    def _add_request(self, prompt: PromptType, params: Union[SamplingParams, PoolingParams], tokenization_kwargs: Optional[dict[str, Any]]=None, lora_request: Optional[LoRARequest]=None, priority: int=0) -> None:
        request_id = str(next(self.request_counter))
        self.llm_engine.add_request(request_id, prompt, params, lora_request=lora_request, tokenization_kwargs=tokenization_kwargs, priority=priority)
    def _run_engine(self, *, use_tqdm: Union[bool, Callable[..., tqdm]]=True) -> list[Union[RequestOutput, PoolingRequestOutput]]:
        if use_tqdm:
            num_requests = self.llm_engine.get_num_unfinished_requests()
            tqdm_func = use_tqdm if callable(use_tqdm) else tqdm
            pbar = tqdm_func(total=num_requests, desc='Processed prompts', dynamic_ncols=True, postfix=f'est. speed input: {0:.2f} toks/s, output: {0:.2f} toks/s')
        outputs: list[Union[RequestOutput, PoolingRequestOutput]] = []
        total_in_toks = 0
        total_out_toks = 0
        while self.llm_engine.has_unfinished_requests():
            step_outputs = self.llm_engine.step()
            for output in step_outputs:
                if output.finished:
                    outputs.append(output)
                    if use_tqdm:
                        if isinstance(output, RequestOutput):
                            n = len(output.outputs)
                            assert output.prompt_token_ids is not None
                            total_in_toks += len(output.prompt_token_ids) * n
                            in_spd = total_in_toks / pbar.format_dict['elapsed']
                            total_out_toks += sum((len(stp.token_ids) for stp in output.outputs))
                            out_spd = total_out_toks / pbar.format_dict['elapsed']
                            pbar.postfix = f'est. speed input: {in_spd:.2f} toks/s, output: {out_spd:.2f} toks/s'
                            pbar.update(n)
                        else:
                            pbar.update(1)
                        if pbar.n == num_requests:
                            pbar.refresh()
        if use_tqdm:
            pbar.close()
        return sorted(outputs, key=lambda x: int(x.request_id))