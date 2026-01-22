import json
import time
from http import HTTPStatus
from typing import Annotated, Any, ClassVar, Dict, List, Literal, Optional, Union
import regex as re
import torch
from fastapi import HTTPException, UploadFile
from loguru import logger
from openai.types.chat.chat_completion_audio import ChatCompletionAudio as OpenAIChatCompletionAudio
from openai.types.chat.chat_completion_message import Annotation as OpenAIAnnotation
from openai.types.responses import ResponseInputParam, ResponseOutputItem, ResponseOutputMessage, ResponsePrompt, ResponseStatus, ResponseTextConfig
from openai.types.responses.response import ToolChoice
from openai.types.responses.tool import Tool
from openai.types.shared import Metadata, Reasoning
from pydantic import AliasChoices, BaseModel, ConfigDict, Field, TypeAdapter, ValidationInfo, field_validator, model_validator
from typing_extensions import TypeAlias
from aphrodite.common import envs
from aphrodite.common.pooling_params import PoolingParams
from aphrodite.common.sampling_params import BeamSearchParams, GuidedDecodingParams, RequestOutputKind, SamplingParams
from aphrodite.common.sequence import Logprob
from aphrodite.utils import random_uuid, resolve_obj_by_qualname
from aphrodite.endpoints.chat_utils import ChatCompletionMessageParam, random_tool_call_id
from aphrodite.endpoints.score_utils import ScoreContentPartParam, ScoreMultiModalParam
from aphrodite.transformers_utils.tokenizer import AnyTokenizer
_LONG_INFO = torch.iinfo(torch.long)
class OpenAIBaseModel(BaseModel):
    model_config = ConfigDict(extra='allow')
    field_names: ClassVar[Optional[set[str]]] = None
    @model_validator(mode='wrap')
    @classmethod
    def __log_extra_fields__(cls, data, handler):
        result = handler(data)
        if not isinstance(data, dict):
            return result
        field_names = cls.field_names
        if field_names is None:
            field_names = set()
            for field_name, field in cls.model_fields.items():
                field_names.add(field_name)
                if (alias := getattr(field, 'alias', None)):
                    field_names.add(alias)
            cls.field_names = field_names
        if any((k not in field_names for k in data)):
            logger.warning('The following fields were present in the request but ignored: {}', data.keys() - field_names)
        return result
class ErrorResponse(OpenAIBaseModel):
    object: str = 'error'
    message: str
    type: str
    param: Optional[str] = None
    code: int
class ModelPermission(OpenAIBaseModel):
    id: str = Field(default_factory=lambda: f'modelperm-{random_uuid()}')
    object: str = 'model_permission'
    created: int = Field(default_factory=lambda: int(time.time()))
    allow_create_engine: bool = False
    allow_sampling: bool = True
    allow_logprobs: bool = True
    allow_search_indices: bool = False
    allow_view: bool = True
    allow_fine_tuning: bool = False
    organization: str = '*'
    group: Optional[str] = None
    is_blocking: bool = False
class ModelCard(OpenAIBaseModel):
    id: str
    object: str = 'model'
    created: int = Field(default_factory=lambda: int(time.time()))
    owned_by: str = 'pygmalionai'
    root: Optional[str] = None
    parent: Optional[str] = None
    max_model_len: Optional[int] = None
    permission: list[ModelPermission] = Field(default_factory=list)
class ModelList(OpenAIBaseModel):
    object: str = 'list'
    data: list[ModelCard] = Field(default_factory=list)
class PromptTokenUsageInfo(OpenAIBaseModel):
    cached_tokens: Optional[int] = None
class UsageInfo(OpenAIBaseModel):
    prompt_tokens: int = 0
    total_tokens: int = 0
    completion_tokens: Optional[int] = 0
    prompt_tokens_details: Optional[PromptTokenUsageInfo] = None
class RequestResponseMetadata(BaseModel):
    request_id: str
    final_usage_info: Optional[UsageInfo] = None
class JsonSchemaResponseFormat(OpenAIBaseModel):
    name: str
    description: Optional[str] = None
    json_schema: Optional[dict[str, Any]] = Field(default=None, alias='schema')
    strict: Optional[bool] = None
class StructuralTag(OpenAIBaseModel):
    begin: str
    structural_tag_schema: Optional[dict[str, Any]] = Field(default=None, alias='schema')
    end: str
class StructuralTagResponseFormat(OpenAIBaseModel):
    type: Literal['structural_tag']
    structures: list[StructuralTag]
    triggers: list[str]
class ResponseFormat(OpenAIBaseModel):
    type: Literal['text', 'json_object', 'json_schema']
    json_schema: Optional[JsonSchemaResponseFormat] = None
AnyResponseFormat = Union[ResponseFormat, StructuralTagResponseFormat]
class StreamOptions(OpenAIBaseModel):
    include_usage: Optional[bool] = True
    continuous_usage_stats: Optional[bool] = False
class FunctionDefinition(OpenAIBaseModel):
    name: str
    description: Optional[str] = None
    parameters: Optional[dict[str, Any]] = None
class ChatCompletionToolsParam(OpenAIBaseModel):
    type: Literal['function'] = 'function'
    function: FunctionDefinition
class ChatCompletionNamedFunction(OpenAIBaseModel):
    name: str
class ChatCompletionNamedToolChoiceParam(OpenAIBaseModel):
    function: ChatCompletionNamedFunction
    type: Literal['function'] = 'function'
class LogitsProcessorConstructor(BaseModel):
    qualname: str
    args: Optional[list[Any]] = None
    kwargs: Optional[dict[str, Any]] = None
    model_config = ConfigDict(extra='forbid')
LogitsProcessors = list[Union[str, LogitsProcessorConstructor]]
def get_logits_processors(processors: Optional[LogitsProcessors], pattern: Optional[str]) -> Optional[list[Any]]:
    if processors and pattern:
        logits_processors = []
        for processor in processors:
            qualname = processor if isinstance(processor, str) else processor.qualname
            if not re.match(pattern, qualname):
                raise ValueError(f"Logits processor '{qualname}' is not allowed by this server. See --logits-processor-pattern engine argument for more information.")
            try:
                logits_processor = resolve_obj_by_qualname(qualname)
            except Exception as e:
                raise ValueError(f"Logits processor '{qualname}' could not be resolved: {e}") from e
            if isinstance(processor, LogitsProcessorConstructor):
                logits_processor = logits_processor(*(processor.args or []), **processor.kwargs or {})
            logits_processors.append(logits_processor)
        return logits_processors
    elif processors:
        raise ValueError('The `logits_processors` argument is not supported by this server. See --logits-processor-pattern engine argugment for more information.')
    return None
class ResponsesRequest(OpenAIBaseModel):
    background: Optional[bool] = False
    include: Optional[list[Literal['code_interpreter_call.outputs', 'computer_call_output.output.image_url', 'file_search_call.results', 'message.input_image.image_url', 'message.output_text.logprobs', 'reasoning.encrypted_content'],]] = None
    input: Union[str, ResponseInputParam]
    instructions: Optional[str] = None
    max_output_tokens: Optional[int] = None
    max_tool_calls: Optional[int] = None
    metadata: Optional[Metadata] = None
    model: Optional[str] = None
    parallel_tool_calls: Optional[bool] = True
    previous_response_id: Optional[str] = None
    prompt: Optional[ResponsePrompt] = None
    reasoning: Optional[Reasoning] = None
    service_tier: Literal['auto', 'default', 'flex', 'scale', 'priority'] = 'auto'
    store: Optional[bool] = True
    stream: Optional[bool] = False
    temperature: Optional[float] = None
    text: Optional[ResponseTextConfig] = None
    tool_choice: ToolChoice = 'auto'
    tools: list[Tool] = Field(default_factory=list)
    top_logprobs: Optional[int] = 0
    top_p: Optional[float] = None
    truncation: Optional[Literal['auto', 'disabled']] = 'disabled'
    user: Optional[str] = None
    request_id: str = Field(default_factory=lambda: f'resp_{random_uuid()}', description='The request_id related to this request. If the caller does not set it, a random_uuid will be generated. This id is used through out the inference process and return in response.')
    mm_processor_kwargs: Optional[dict[str, Any]] = Field(default=None, description='Additional kwargs to pass to the HF processor.')
    priority: int = Field(default=0, description='The priority of the request (lower means earlier handling; default: 0). Any priority other than 0 will raise an error if the served model does not use priority scheduling.')
    cache_salt: Optional[str] = Field(default=None, description='If specified, the prefix cache will be salted with the provided string to prevent an attacker to guess prompts in multi-user environments. The salt should be random, protected from access by 3rd parties, and long enough to be unpredictable (e.g., 43 characters base64-encoded, corresponding to 256 bit). Not supported by Aphrodite engine V0.')
    _DEFAULT_SAMPLING_PARAMS = {'temperature': 1.0, 'top_p': 1.0}
    def to_sampling_params(self, default_max_tokens: int, default_sampling_params: Optional[dict]=None) -> SamplingParams:
        if self.max_output_tokens is None:
            max_tokens = default_max_tokens
        else:
            max_tokens = min(self.max_output_tokens, default_max_tokens)
        default_sampling_params = default_sampling_params or {}
        if (temperature := self.temperature) is None:
            temperature = default_sampling_params.get('temperature', self._DEFAULT_SAMPLING_PARAMS['temperature'])
        if (top_p := self.top_p) is None:
            top_p = default_sampling_params.get('top_p', self._DEFAULT_SAMPLING_PARAMS['top_p'])
        stop_token_ids = default_sampling_params.get('stop_token_ids')
        guided_decoding = None
        if self.text is not None and self.text.format is not None:
            response_format = self.text.format
            if response_format.type == 'json_schema':
                guided_decoding = GuidedDecodingParams.from_optional(json=response_format.schema_)
            elif response_format.type == 'json_object':
                raise NotImplementedError('json_object is not supported')
        return SamplingParams.from_optional(temperature=temperature, top_p=top_p, max_tokens=max_tokens, logprobs=self.top_logprobs, stop_token_ids=stop_token_ids, output_kind=RequestOutputKind.DELTA if self.stream else RequestOutputKind.FINAL_ONLY, guided_decoding=guided_decoding)
    @model_validator(mode='before')
    def validate_background(cls, data):
        if not data.get('background'):
            return data
        if not data.get('store', True):
            raise ValueError('background can only be used when `store` is true')
        return data
    @model_validator(mode='before')
    def validate_prompt(cls, data):
        if data.get('prompt') is not None:
            raise ValueError('prompt template is not supported')
        return data
    @model_validator(mode='before')
    def check_cache_salt_support(cls, data):
        if data.get('cache_salt') is not None:
            if not envs.APHRODITE_USE_V1:
                raise ValueError("Parameter 'cache_salt' is not supported with this instance of Aphrodite, which uses engine V0.")
            if not isinstance(data['cache_salt'], str) or not data['cache_salt']:
                raise ValueError("Parameter 'cache_salt' must be a non-empty string if provided.")
        return data
class ChatCompletionRequest(OpenAIBaseModel):
    messages: list[ChatCompletionMessageParam]
    model: str
    frequency_penalty: Optional[float] = 0.0
    logit_bias: Optional[dict[str, float]] = None
    logprobs: Optional[bool] = False
    top_logprobs: Optional[int] = 0
    max_tokens: Optional[int] = Field(default=None, deprecated='max_tokens is deprecated in favor of the max_completion_tokens field')
    max_completion_tokens: Optional[int] = None
    n: Optional[int] = 1
    presence_penalty: Optional[float] = 0.0
    response_format: Optional[ResponseFormat] = None
    seed: Optional[int] = Field(None, ge=torch.iinfo(torch.long).min, le=torch.iinfo(torch.long).max)
    stop: Optional[Union[str, list[str]]] = []
    stream: Optional[bool] = False
    stream_options: Optional[StreamOptions] = None
    temperature: Optional[float] = 0.7
    top_p: Optional[float] = 1.0
    tools: Optional[list[ChatCompletionToolsParam]] = None
    tool_choice: Optional[Union[Literal['none'], Literal['auto'], ChatCompletionNamedToolChoiceParam]] = 'none'
    reasoning_effort: Optional[Literal['low', 'medium', 'high']] = None
    include_reasoning: bool = True
    parallel_tool_calls: Optional[bool] = False
    user: Optional[str] = None
    best_of: Optional[int] = None
    use_beam_search: Optional[bool] = False
    top_k: Optional[int] = -1
    min_p: Optional[float] = 0.0
    top_a: Optional[float] = 0.0
    tfs: Optional[float] = 1.0
    eta_cutoff: Optional[float] = 0.0
    epsilon_cutoff: Optional[float] = 0.0
    typical_p: Optional[float] = 1.0
    smoothing_factor: Optional[float] = 0.0
    smoothing_curve: Optional[float] = 1.0
    repetition_penalty: Optional[float] = 1.0
    no_repeat_ngram_size: Optional[int] = 0
    length_penalty: Optional[float] = 1.0
    early_stopping: Optional[bool] = False
    ignore_eos: Optional[bool] = False
    min_tokens: Optional[int] = 0
    stop_token_ids: Optional[list[int]] = []
    skip_special_tokens: Optional[bool] = True
    spaces_between_special_tokens: Optional[bool] = True
    truncate_prompt_tokens: Optional[Annotated[int, Field(ge=1)]] = None
    temperature_last: Optional[bool] = False
    prompt_logprobs: Optional[int] = None
    xtc_threshold: Optional[float] = 0.1
    xtc_probability: Optional[float] = 0.0
    dry_multiplier: Optional[float] = 0
    dry_base: Optional[float] = 1.75
    dry_allowed_length: Optional[int] = 2
    dry_sequence_breakers: Optional[list[str]] = Field(default=['\n', ':', '"', '*'])
    dry_range: Optional[int] = Field(default=0, validation_alias=AliasChoices('dry_range', 'dry_penalty_last_n'))
    dry_max_ngram: Optional[int] = 12
    dry_max_occurrences: Optional[int] = 8
    dry_early_exit_match_len: Optional[int] = 8
    dynatemp_min: Optional[float] = 0.0
    dynatemp_max: Optional[float] = 0.0
    dynatemp_exponent: Optional[float] = 1.0
    nsigma: Optional[float] = 0.0
    skew: Optional[float] = 0.0
    custom_token_bans: Optional[list[int]] = None
    token_ban_ranges: Optional[list[tuple[list[int], int, int]]] = None
    sampler_priority: Optional[Union[list[int], list[str]]] = Field(default=[], validation_alias=AliasChoices('sampler_priority', 'sampler_order'))
    allowed_token_ids: Optional[list[int]] = None
    bad_words: list[str] = Field(default_factory=list)
    echo: Optional[bool] = Field(default=False, description='If true, the new message will be prepended with the last message if they belong to the same role.')
    add_generation_prompt: Optional[bool] = Field(default=True, description='If true, the generation prompt will be added to the chat template. This is a parameter used by chat template in tokenizer config of the model.')
    continue_final_message: bool = Field(default=False, description='If this is set, the chat will be formatted so that the final message in the chat is open-ended, without any EOS tokens. The model will continue this message rather than starting a new one. This allows you to "prefill" part of the model\'s response for it. Cannot be used at the same time as `add_generation_prompt`.')
    add_special_tokens: Optional[bool] = Field(default=False, description='If true, special tokens (e.g. BOS) will be added to the prompt on top of what is added by the chat template. For most models, the chat template takes care of adding the special tokens so this should be set to False (as is the default).')
    documents: Optional[list[dict[str, str]]] = Field(default=None, description='A list of dicts representing documents that will be accessible to the model if it is performing RAG (retrieval-augmented generation). If the template does not support RAG, this argument will have no effect. We recommend that each document should be a dict containing "title" and "text" keys.')
    chat_template: Optional[str] = Field(default=None, description='A Jinja template to use for this conversion. As of transformers v4.44, default chat template is no longer allowed, so you must provide a chat template if the tokenizer does not define one.')
    chat_template_kwargs: Optional[dict[str, Any]] = Field(default=None, description='Additional keyword args to pass to the template renderer. Will be accessible by the chat template.')
    include_stop_str_in_output: Optional[bool] = Field(default=False, description='Whether to include the stop string in the output. This is only applied when the stop or stop_token_ids is set.')
    mm_processor_kwargs: Optional[dict[str, Any]] = Field(default=None, description='Additional kwargs to pass to the HF processor.')
    guided_json: Optional[Union[str, dict, BaseModel]] = Field(default=None, description='If specified, the output will follow the JSON schema.')
    guided_regex: Optional[str] = Field(default=None, description='If specified, the output will follow the regex pattern.')
    guided_choice: Optional[list[str]] = Field(default=None, description='If specified, the output will be exactly one of the choices.')
    guided_grammar: Optional[str] = Field(default=None, description='If specified, the output will follow the context free grammar.')
    structural_tag: Optional[str] = Field(default=None, description='If specified, the output will follow the structural tag schema.')
    guided_decoding_backend: Optional[str] = Field(default=None, description="If specified, will override the default guided decoding backend of the server for this specific request. If set, must be either 'outlines' / 'lm-format-enforcer'")
    guided_whitespace_pattern: Optional[str] = Field(default=None, description='If specified, will override the default whitespace pattern for guided json decoding.')
    priority: int = Field(default=0, description='The priority of the request (lower means earlier handling; default: 0). Any priority other than 0 will raise an error if the served model does not use priority scheduling.')
    request_id: str = Field(default_factory=lambda: f'{random_uuid()}', description='The request_id related to this request. If the caller does not set it, a random_uuid will be generated. This id is used through out the inference process and return in response.')
    kv_transfer_params: Optional[dict[str, Any]] = Field(default=None, description='KVTransfer parameters used for disaggregated serving.')
    aphrodite_xargs: Optional[dict[str, Union[str, int, float]]] = Field(default=None, description='Additional request parameters with string or numeric values, used by custom extensions.')
    _DEFAULT_SAMPLING_PARAMS: dict = {'repetition_penalty': 1.0, 'temperature': 1.0, 'top_p': 1.0, 'top_k': 0, 'min_p': 0.0}
    def to_beam_search_params(self, max_tokens: int, default_sampling_params: dict) -> BeamSearchParams:
        n = self.n if self.n is not None else 1
        if (temperature := self.temperature) is None:
            temperature = default_sampling_params.get('temperature', self._DEFAULT_SAMPLING_PARAMS['temperature'])
        return BeamSearchParams(beam_width=n, max_tokens=max_tokens, ignore_eos=self.ignore_eos, temperature=temperature, length_penalty=self.length_penalty, include_stop_str_in_output=self.include_stop_str_in_output)
    def to_sampling_params(self, tokenizer: AnyTokenizer, max_tokens: int, logits_processor_pattern: Optional[str], default_sampling_params: dict) -> SamplingParams:
        if (repetition_penalty := self.repetition_penalty) is None:
            default_sampling_params.get('repetition_penalty', self._DEFAULT_SAMPLING_PARAMS['repetition_penalty'])
        if (temperature := self.temperature) is None:
            default_sampling_params.get('temperature', self._DEFAULT_SAMPLING_PARAMS['temperature'])
        if (top_p := self.top_p) is None:
            default_sampling_params.get('top_p', self._DEFAULT_SAMPLING_PARAMS['top_p'])
        if (top_k := self.top_k) is None:
            default_sampling_params.get('top_k', self._DEFAULT_SAMPLING_PARAMS['top_k'])
        if (min_p := self.min_p) is None:
            default_sampling_params.get('min_p', self._DEFAULT_SAMPLING_PARAMS['min_p'])
        prompt_logprobs = self.prompt_logprobs
        if prompt_logprobs is None and self.echo:
            prompt_logprobs = self.top_logprobs
        guided_json_object = None
        if self.response_format is not None:
            if self.response_format.type == 'json_object':
                guided_json_object = True
            elif self.response_format.type == 'json_schema':
                json_schema = self.response_format.json_schema
                assert json_schema is not None
                self.guided_json = json_schema.json_schema
            elif self.response_format.type == 'structural_tag':
                structural_tag = self.response_format
                assert structural_tag is not None and isinstance(structural_tag, StructuralTagResponseFormat)
                s_tag_obj = structural_tag.model_dump(by_alias=True)
                self.structural_tag = json.dumps(s_tag_obj)
        guided_decoding = GuidedDecodingParams.from_optional(json=self._get_guided_json_from_tool() or self.guided_json, regex=self.guided_regex, choice=self.guided_choice, grammar=self.guided_grammar, json_object=guided_json_object, backend=self.guided_decoding_backend, whitespace_pattern=self.guided_whitespace_pattern, structural_tag=self.structural_tag)
        dry_sequence_breaker_ids = []
        if self.dry_sequence_breakers:
            for s in self.dry_sequence_breakers:
                token_id = tokenizer.encode(f'a{s}')[-1]
                dry_sequence_breaker_ids.append(token_id)
        extra_args: dict[str, Any] = self.aphrodite_xargs if self.aphrodite_xargs else {}
        if self.kv_transfer_params:
            extra_args['kv_transfer_params'] = self.kv_transfer_params
        return SamplingParams.from_optional(n=self.n, presence_penalty=self.presence_penalty, frequency_penalty=self.frequency_penalty, repetition_penalty=self.repetition_penalty, no_repeat_ngram_size=self.no_repeat_ngram_size, temperature=self.temperature, top_p=self.top_p, min_p=self.min_p, seed=self.seed, stop=self.stop, stop_token_ids=self.stop_token_ids, max_tokens=max_tokens, min_tokens=self.min_tokens, logprobs=self.top_logprobs if self.logprobs else None, prompt_logprobs=self.prompt_logprobs if self.prompt_logprobs else self.top_logprobs if self.echo else None, best_of=self.best_of, top_k=self.top_k, top_a=self.top_a, tfs=self.tfs, eta_cutoff=self.eta_cutoff, epsilon_cutoff=self.epsilon_cutoff, typical_p=self.typical_p, smoothing_factor=self.smoothing_factor, smoothing_curve=self.smoothing_curve, ignore_eos=self.ignore_eos, use_beam_search=self.use_beam_search, early_stopping=self.early_stopping, skip_special_tokens=self.skip_special_tokens, spaces_between_special_tokens=self.spaces_between_special_tokens, include_stop_str_in_output=self.include_stop_str_in_output, length_penalty=self.length_penalty, temperature_last=self.temperature_last, xtc_threshold=self.xtc_threshold, xtc_probability=self.xtc_probability, dry_multiplier=self.dry_multiplier, dry_base=self.dry_base, dry_allowed_length=self.dry_allowed_length, dry_sequence_breaker_ids=dry_sequence_breaker_ids, dry_range=self.dry_range, dry_max_ngram=self.dry_max_ngram, dry_max_occurrences=self.dry_max_occurrences, dry_early_exit_match_len=self.dry_early_exit_match_len, dynatemp_min=self.dynatemp_min, dynatemp_max=self.dynatemp_max, dynatemp_exponent=self.dynatemp_exponent, nsigma=self.nsigma, skew=self.skew, custom_token_bans=self.custom_token_bans, token_ban_ranges=self.token_ban_ranges, sampler_priority=self.sampler_priority, output_kind=RequestOutputKind.DELTA if self.stream else RequestOutputKind.FINAL_ONLY, guided_decoding=guided_decoding, logit_bias=self.logit_bias, bad_words=self.bad_words, allowed_token_ids=self.allowed_token_ids, extra_args=extra_args or None)
    def _get_guided_json_from_tool(self) -> Optional[Union[str, dict, BaseModel]]:
        if self.tool_choice == 'none' or self.tools is None:
            return None
        if type(self.tool_choice) is ChatCompletionNamedToolChoiceParam:
            tool_name = self.tool_choice.function.name
            tools = {tool.function.name: tool.function for tool in self.tools}
            if tool_name not in tools:
                raise ValueError(f"Tool '{tool_name}' has not been passed in `tools`.")
            tool = tools[tool_name]
            return tool.parameters
        if self.tool_choice == 'required':
            def get_tool_schema(tool: ChatCompletionToolsParam) -> dict:
                return {'properties': {'name': {'type': 'string', 'enum': [tool.function.name]}, 'parameters': tool.function.parameters if tool.function.parameters else {'type': 'object', 'properties': {}}}, 'required': ['name', 'parameters']}
            def get_tool_schema_defs(tools: list[ChatCompletionToolsParam]) -> dict:
                all_defs = dict[str, dict[str, Any]]()
                for tool in tools:
                    if tool.function.parameters is None:
                        continue
                    defs = tool.function.parameters.pop('$defs', {})
                    for def_name, def_schema in defs.items():
                        if def_name in all_defs and all_defs[def_name] != def_schema:
                            raise ValueError(f"Tool definition '{def_name}' has multiple schemas, which is not supported.")
                        else:
                            all_defs[def_name] = def_schema
                return all_defs
            json_schema = {'type': 'array', 'minItems': 1, 'items': {'type': 'object', 'anyOf': [get_tool_schema(tool) for tool in self.tools]}}
            json_schema_defs = get_tool_schema_defs(self.tools)
            if json_schema_defs:
                json_schema['$defs'] = json_schema_defs
            return json_schema
        return None
    @model_validator(mode='before')
    @classmethod
    def validate_stream_options(cls, values):
        if values.get('stream_options') is not None and (not values.get('stream')):
            raise ValueError('stream_options can only be set if stream is true')
        return values
    @model_validator(mode='before')
    @classmethod
    def check_logprobs(cls, data):
        if (prompt_logprobs := data.get('prompt_logprobs')) is not None:
            if data.get('stream') and prompt_logprobs > 0:
                raise ValueError('`prompt_logprobs` are not available when `stream=True`.')
            if prompt_logprobs < 0:
                raise ValueError('`prompt_logprobs` must be a positive value.')
        if (top_logprobs := data.get('top_logprobs')) is not None:
            if top_logprobs < 0:
                raise ValueError('`top_logprobs` must be a positive value.')
            if top_logprobs > 0 and (not data.get('logprobs')):
                raise ValueError('when using `top_logprobs`, `logprobs` must be set to true.')
        return data
    @model_validator(mode='before')
    @classmethod
    def check_guided_decoding_count(cls, data):
        if isinstance(data, ValueError):
            raise data
        def is_effectively_none(value):
            if value is None:
                return True
            if isinstance(value, str) and value.strip() == '':
                return True
            if isinstance(value, dict) and len(value) == 0:
                return True
            return False
        guide_count = sum(['guided_json' in data and (not is_effectively_none(data['guided_json'])), 'guided_regex' in data and (not is_effectively_none(data['guided_regex'])), 'guided_choice' in data and (not is_effectively_none(data['guided_choice'])), 'guided_grammar' in data and (not is_effectively_none(data['guided_grammar']))])
        if guide_count > 1:
            raise ValueError("You can only use one kind of guided decoding ('guided_json', 'guided_regex', 'guided_choice', or 'guided_grammar').")
        if guide_count > 1 and data.get('tool_choice', 'none') not in ('none', 'auto', 'required'):
            raise ValueError('You can only either use guided decoding or tools, not both.')
        return data
    @model_validator(mode='before')
    @classmethod
    def check_tool_usage(cls, data):
        if 'tool_choice' not in data and data.get('tools'):
            data['tool_choice'] = 'auto'
        if 'tool_choice' in data and data['tool_choice'] == 'none':
            return data
        if 'tool_choice' in data and data['tool_choice'] is not None:
            if 'tools' not in data or data['tools'] is None:
                raise ValueError('When using `tool_choice`, `tools` must be set.')
            if data['tool_choice'] not in ['auto', 'required'] and (not isinstance(data['tool_choice'], dict)):
                raise ValueError(f"""Invalid value for `tool_choice`: {data['tool_choice']}! Only named tools, "none", "auto" or "required" are supported.""")
            if data['tool_choice'] == 'required' and isinstance(data['tools'], list) and (len(data['tools']) == 0):
                data['tool_choice'] = 'none'
                del data['tools']
                return data
            correct_usage_message = 'Correct usage: `{"type": "function", "function": {"name": "my_function"}}`'
            if isinstance(data['tool_choice'], dict):
                valid_tool = False
                function = data['tool_choice'].get('function')
                if not isinstance(function, dict):
                    raise ValueError(f'Invalid value for `function`: `{function}` in `tool_choice`! {correct_usage_message}')
                if 'name' not in function:
                    raise ValueError(f'Expected field `name` in `function` in `tool_choice`! {correct_usage_message}')
                function_name = function['name']
                if not isinstance(function_name, str) or len(function_name) == 0:
                    raise ValueError(f'Invalid `name` in `function`: `{function_name}` in `tool_choice`! {correct_usage_message}')
                for tool in data['tools']:
                    if tool['function']['name'] == function_name:
                        valid_tool = True
                        break
                if not valid_tool:
                    raise ValueError('The tool specified in `tool_choice` does not match any of the specified `tools`')
        return data
    @model_validator(mode='before')
    @classmethod
    def check_generation_prompt(cls, data):
        if data.get('continue_final_message') and data.get('add_generation_prompt'):
            raise ValueError('Cannot set both `continue_final_message` and `add_generation_prompt` to True.')
        return data
    @model_validator(mode='before')
    @classmethod
    def check_cache_salt_support(cls, data):
        if data.get('cache_salt') is not None:
            if not envs.APHRODITE_USE_V1:
                raise ValueError("Parameter 'cache_salt' is not supported with this instance of Aphrodite, which uses engine V0.")
            if not isinstance(data['cache_salt'], str) or not data['cache_salt']:
                raise ValueError("Parameter 'cache_salt' must be a non-empty string if provided.")
        return data
class CompletionRequest(OpenAIBaseModel):
    model: str
    prompt: Optional[Union[list[int], list[list[int]], str, list[str]]] = None
    prompt_embeds: Optional[Union[bytes, list[bytes]]] = None
    best_of: Optional[int] = None
    echo: Optional[bool] = False
    frequency_penalty: Optional[float] = 0.0
    logit_bias: Optional[dict[str, float]] = None
    logprobs: Optional[int] = None
    max_tokens: Optional[int] = 16
    n: int = 1
    presence_penalty: Optional[float] = 0.0
    seed: Optional[int] = Field(None, ge=torch.iinfo(torch.long).min, le=torch.iinfo(torch.long).max)
    stop: Optional[Union[str, list[str]]] = []
    stream: Optional[bool] = False
    stream_options: Optional[StreamOptions] = None
    suffix: Optional[str] = None
    temperature: Optional[float] = 1.0
    top_p: Optional[float] = 1.0
    user: Optional[str] = None
    use_beam_search: Optional[bool] = False
    top_k: Optional[int] = -1
    min_p: Optional[float] = 0.0
    top_a: Optional[float] = 0.0
    tfs: Optional[float] = 1.0
    eta_cutoff: Optional[float] = 0.0
    epsilon_cutoff: Optional[float] = 0.0
    typical_p: Optional[float] = 1.0
    smoothing_factor: Optional[float] = 0.0
    smoothing_curve: Optional[float] = 1.0
    repetition_penalty: Optional[float] = 1.0
    no_repeat_ngram_size: Optional[int] = 0
    length_penalty: Optional[float] = 1.0
    early_stopping: Optional[bool] = False
    stop_token_ids: Optional[list[int]] = []
    ignore_eos: Optional[bool] = False
    min_tokens: Optional[int] = 0
    skip_special_tokens: Optional[bool] = True
    spaces_between_special_tokens: Optional[bool] = True
    truncate_prompt_tokens: Optional[Annotated[int, Field(ge=1)]] = None
    allowed_token_ids: Optional[list[int]] = None
    include_stop_str_in_output: Optional[bool] = False
    add_special_tokens: Optional[bool] = False
    temperature_last: Optional[bool] = False
    prompt_logprobs: Optional[int] = None
    xtc_threshold: Optional[float] = 0.1
    xtc_probability: Optional[float] = 0.0
    dry_multiplier: Optional[float] = 0
    dry_base: Optional[float] = 1.75
    dry_allowed_length: Optional[int] = 2
    dry_sequence_breakers: Optional[list[str]] = Field(default=['\n', ':', '"', '*'])
    dry_range: Optional[int] = Field(default=0, validation_alias=AliasChoices('dry_range', 'dry_penalty_last_n'))
    dry_max_ngram: Optional[int] = 12
    dry_max_occurrences: Optional[int] = 8
    dry_early_exit_match_len: Optional[int] = 8
    dynatemp_min: Optional[float] = 0.0
    dynatemp_max: Optional[float] = 0.0
    dynatemp_exponent: Optional[float] = 1.0
    nsigma: Optional[float] = 0.0
    skew: Optional[float] = 0.0
    custom_token_bans: Optional[list[int]] = None
    token_ban_ranges: Optional[list[tuple[list[int], int, int]]] = None
    sampler_priority: Optional[Union[list[int], list[str]]] = Field(default=[], validation_alias=AliasChoices('sampler_priority', 'sampler_order'))
    response_format: Optional[ResponseFormat] = Field(default=None, description="Similar to chat completion, this parameter specifies the format of output. Only {'type': 'json_object'}, {'type': 'json_schema'} or {'type': 'text' } is supported.")
    guided_json: Optional[Union[str, dict, BaseModel]] = Field(default=None, description='If specified, the output will follow the JSON schema.')
    guided_regex: Optional[str] = Field(default=None, description='If specified, the output will follow the regex pattern.')
    guided_choice: Optional[list[str]] = Field(default=None, description='If specified, the output will be exactly one of the choices.')
    guided_grammar: Optional[str] = Field(default=None, description='If specified, the output will follow the context free grammar.')
    guided_decoding_backend: Optional[str] = Field(default=None, description="If specified, will override the default guided decoding backend of the server for this specific request. If set, must be one of 'outlines' / 'lm-format-enforcer'")
    guided_whitespace_pattern: Optional[str] = Field(default=None, description='If specified, will override the default whitespace pattern for guided json decoding.')
    priority: int = Field(default=0, description='The priority of the request (lower means earlier handling; default: 0). Any priority other than 0 will raise an error if the served model does not use priority scheduling.')
    request_id: str = Field(default_factory=lambda: f'{random_uuid()}', description='The request_id related to this request. If the caller does not set it, a random_uuid will be generated. This id is used through out the inference process and return in response.')
    logits_processors: Optional[LogitsProcessors] = Field(default=None, description="A list of either qualified names of logits processors, or constructor objects, to apply when sampling. A constructor is a JSON object with a required 'qualname' field specifying the qualified name of the processor class/factory, and optional 'args' and 'kwargs' fields containing positional and keyword arguments. For example: {'qualname': 'my_module.MyLogitsProcessor', 'args': [1, 2], 'kwargs': {'param': 'value'}}.")
    return_tokens_as_token_ids: Optional[bool] = Field(default=None, description="If specified with 'logprobs', tokens are represented  as strings of the form 'token_id:{token_id}' so that tokens that are not JSON-encodable can be identified.")
    cache_salt: Optional[str] = Field(default=None, description='If specified, the prefix cache will be salted with the provided string to prevent an attacker to guess prompts in multi-user environments. The salt should be random, protected from access by 3rd parties, and long enough to be unpredictable (e.g., 43 characters base64-encoded, corresponding to 256 bit). Not supported by Aphrodite engine V0.')
    kv_transfer_params: Optional[dict[str, Any]] = Field(default=None, description='KVTransfer parameters used for disaggregated serving.')
    aphrodite_xargs: Optional[dict[str, Union[str, int, float]]] = Field(default=None, description='Additional request parameters with string or numeric values, used by custom extensions.')
    _DEFAULT_SAMPLING_PARAMS: dict = {'repetition_penalty': 1.0, 'temperature': 1.0, 'top_p': 1.0, 'top_k': 0, 'min_p': 0.0}
    def to_beam_search_params(self, max_tokens: int, default_sampling_params: Optional[dict]=None) -> BeamSearchParams:
        if default_sampling_params is None:
            default_sampling_params = {}
        n = self.n if self.n is not None else 1
        if (temperature := self.temperature) is None:
            temperature = default_sampling_params.get('temperature', 1.0)
        return BeamSearchParams(beam_width=n, max_tokens=max_tokens, ignore_eos=self.ignore_eos, temperature=temperature, length_penalty=self.length_penalty, include_stop_str_in_output=self.include_stop_str_in_output)
    def to_sampling_params(self, tokenizer: AnyTokenizer, max_tokens: int, logits_processor_pattern: Optional[str], default_sampling_params: Optional[dict]=None) -> SamplingParams:
        if default_sampling_params is None:
            default_sampling_params = {}
        if (repetition_penalty := self.repetition_penalty) is None:
            default_sampling_params.get('repetition_penalty', self._DEFAULT_SAMPLING_PARAMS['repetition_penalty'])
        if (temperature := self.temperature) is None:
            default_sampling_params.get('temperature', self._DEFAULT_SAMPLING_PARAMS['temperature'])
        if (top_p := self.top_p) is None:
            default_sampling_params.get('top_p', self._DEFAULT_SAMPLING_PARAMS['top_p'])
        if (top_k := self.top_k) is None:
            default_sampling_params.get('top_k', self._DEFAULT_SAMPLING_PARAMS['top_k'])
        if (min_p := self.min_p) is None:
            default_sampling_params.get('min_p', self._DEFAULT_SAMPLING_PARAMS['min_p'])
        prompt_logprobs = self.prompt_logprobs
        if prompt_logprobs is None and self.echo:
            prompt_logprobs = self.logprobs
        echo_without_generation = self.echo and self.max_tokens == 0
        guided_json_object = None
        if self.response_format is not None and self.response_format.type == 'json_object':
            guided_json_object = True
        guided_decoding = GuidedDecodingParams.from_optional(json=self.guided_json, regex=self.guided_regex, choice=self.guided_choice, grammar=self.guided_grammar, json_object=guided_json_object, backend=self.guided_decoding_backend, whitespace_pattern=self.guided_whitespace_pattern)
        dry_sequence_breaker_ids = []
        if self.dry_sequence_breakers:
            for s in self.dry_sequence_breakers:
                s = bytes(s, 'utf-8').decode('unicode_escape')
                token_id = tokenizer.encode(f'a{s}')[-1]
                dry_sequence_breaker_ids.append(token_id)
        extra_args: dict[str, Any] = self.aphrodite_xargs if self.aphrodite_xargs else {}
        if self.kv_transfer_params:
            extra_args['kv_transfer_params'] = self.kv_transfer_params
        return SamplingParams.from_optional(n=self.n, best_of=self.best_of, presence_penalty=self.presence_penalty, frequency_penalty=self.frequency_penalty, repetition_penalty=self.repetition_penalty, no_repeat_ngram_size=self.no_repeat_ngram_size, temperature=self.temperature, top_p=self.top_p, top_k=self.top_k, min_p=self.min_p, top_a=self.top_a, tfs=self.tfs, eta_cutoff=self.eta_cutoff, epsilon_cutoff=self.epsilon_cutoff, typical_p=self.typical_p, smoothing_factor=self.smoothing_factor, smoothing_curve=self.smoothing_curve, seed=self.seed, stop=self.stop, stop_token_ids=self.stop_token_ids, ignore_eos=self.ignore_eos, max_tokens=max_tokens if not echo_without_generation else 1, min_tokens=self.min_tokens, logprobs=self.logprobs, prompt_logprobs=self.prompt_logprobs if self.prompt_logprobs else self.logprobs if self.echo else None, use_beam_search=self.use_beam_search, early_stopping=self.early_stopping, skip_special_tokens=self.skip_special_tokens, spaces_between_special_tokens=self.spaces_between_special_tokens, include_stop_str_in_output=self.include_stop_str_in_output, length_penalty=self.length_penalty, truncate_prompt_tokens=self.truncate_prompt_tokens, temperature_last=self.temperature_last, xtc_threshold=self.xtc_threshold, xtc_probability=self.xtc_probability, dry_multiplier=self.dry_multiplier, dry_base=self.dry_base, dry_allowed_length=self.dry_allowed_length, dry_sequence_breaker_ids=dry_sequence_breaker_ids, dry_range=self.dry_range, dry_max_ngram=self.dry_max_ngram, dry_max_occurrences=self.dry_max_occurrences, dry_early_exit_match_len=self.dry_early_exit_match_len, dynatemp_min=self.dynatemp_min, dynatemp_max=self.dynatemp_max, dynatemp_exponent=self.dynatemp_exponent, nsigma=self.nsigma, skew=self.skew, custom_token_bans=self.custom_token_bans, token_ban_ranges=self.token_ban_ranges, sampler_priority=self.sampler_priority, output_kind=RequestOutputKind.DELTA if self.stream else RequestOutputKind.FINAL_ONLY, guided_decoding=guided_decoding, logit_bias=self.logit_bias, allowed_token_ids=self.allowed_token_ids, extra_args=extra_args or None)
    @model_validator(mode='before')
    @classmethod
    def check_guided_decoding_count(cls, data):
        if isinstance(data, ValueError):
            raise data
        def is_effectively_none(value):
            if value is None:
                return True
            if isinstance(value, str) and value.strip() == '':
                return True
            if isinstance(value, dict) and len(value) == 0:
                return True
            return False
        guide_count = sum(['guided_json' in data and (not is_effectively_none(data['guided_json'])), 'guided_regex' in data and (not is_effectively_none(data['guided_regex'])), 'guided_choice' in data and (not is_effectively_none(data['guided_choice'])), 'guided_grammar' in data and (not is_effectively_none(data['guided_grammar']))])
        if guide_count > 1:
            raise ValueError("You can only use one kind of guided decoding ('guided_json', 'guided_regex', 'guided_choice', or 'guided_grammar').")
        if guide_count > 1 and data.get('tool_choice', 'none') not in ('none', 'auto', 'required'):
            raise ValueError('You can only either use guided decoding or tools, not both.')
        return data
    @model_validator(mode='before')
    @classmethod
    def check_logprobs(cls, data):
        if (prompt_logprobs := data.get('prompt_logprobs')) is not None:
            if data.get('stream') and prompt_logprobs > 0:
                raise ValueError('`prompt_logprobs` are not available when `stream=True`.')
            if prompt_logprobs < 0:
                raise ValueError('`prompt_logprobs` must be a positive value.')
        if (logprobs := data.get('logprobs')) is not None and logprobs < 0:
            raise ValueError('`logprobs` must be a positive value.')
        return data
    @model_validator(mode='before')
    @classmethod
    def validate_stream_options(cls, data):
        if data.get('stream_options') and (not data.get('stream')):
            raise ValueError('Stream options can only be defined when `stream=True`.')
        return data
    @model_validator(mode='before')
    @classmethod
    def parse_dry_sequence_breakers(cls, data):
        if 'dry_sequence_breakers' in data:
            breakers = data['dry_sequence_breakers']
            if isinstance(breakers, str):
                try:
                    data['dry_sequence_breakers'] = json.loads(breakers)
                except json.JSONDecodeError as e:
                    raise ValueError(f'Invalid JSON for dry_sequence_breakers: {e}') from e
            is_list = isinstance(data['dry_sequence_breakers'], list)
            all_strings = all((isinstance(x, str) for x in data['dry_sequence_breakers']))
            if not is_list or not all_strings:
                raise ValueError('dry_sequence_breakers must be a list of strings or a JSON string representing a list of strings')
        return data
    @model_validator(mode='before')
    @classmethod
    def validate_prompt_and_prompt_embeds(cls, data):
        if data.get('prompt') is None and data.get('prompt_embeds') is None:
            raise ValueError('At least one of `prompt` or `prompt_embeds` must be set.')
        return data
    @model_validator(mode='before')
    @classmethod
    def check_cache_salt_support(cls, data):
        if data.get('cache_salt') is not None:
            if not envs.APHRODITE_USE_V1:
                raise ValueError("Parameter 'cache_salt' is not supported with this instance of Aphrodite, which uses engine V0.")
            if not isinstance(data['cache_salt'], str) or not data['cache_salt']:
                raise ValueError("Parameter 'cache_salt' must be a non-empty string if provided.")
        return data
class EmbeddingCompletionRequest(OpenAIBaseModel):
    model: Optional[str] = None
    input: Union[list[int], list[list[int]], str, list[str]]
    encoding_format: Literal['float', 'base64'] = 'float'
    dimensions: Optional[int] = None
    user: Optional[str] = None
    truncate_prompt_tokens: Optional[Annotated[int, Field(ge=-1)]] = None
    add_special_tokens: bool = Field(default=True, description='If true (the default), special tokens (e.g. BOS) will be added to the prompt.')
    priority: int = Field(default=0, description='The priority of the request (lower means earlier handling; default: 0). Any priority other than 0 will raise an error if the served model does not use priority scheduling.')
    request_id: str = Field(default_factory=lambda: f'{random_uuid()}', description='The request_id related to this request. If the caller does not set it, a random_uuid will be generated. This id is used through out the inference process and return in response.')
    normalize: Optional[bool] = None
    def to_pooling_params(self):
        return PoolingParams(dimensions=self.dimensions, normalize=self.normalize)
class EmbeddingChatRequest(OpenAIBaseModel):
    model: Optional[str] = None
    messages: list[ChatCompletionMessageParam]
    encoding_format: Literal['float', 'base64'] = 'float'
    dimensions: Optional[int] = None
    user: Optional[str] = None
    truncate_prompt_tokens: Optional[Annotated[int, Field(ge=-1)]] = None
    add_special_tokens: bool = Field(default=False, description='If true, special tokens (e.g. BOS) will be added to the prompt on top of what is added by the chat template. For most models, the chat template takes care of adding the special tokens so this should be set to false (as is the default).')
    chat_template: Optional[str] = Field(default=None, description='A Jinja template to use for this conversion. As of transformers v4.44, default chat template is no longer allowed, so you must provide a chat template if the tokenizer does not define one.')
    chat_template_kwargs: Optional[dict[str, Any]] = Field(default=None, description='Additional keyword args to pass to the template renderer. Will be accessible by the chat template.')
    mm_processor_kwargs: Optional[dict[str, Any]] = Field(default=None, description='Additional kwargs to pass to the HF processor.')
    priority: int = Field(default=0, description='The priority of the request (lower means earlier handling; default: 0). Any priority other than 0 will raise an error if the served model does not use priority scheduling.')
    request_id: str = Field(default_factory=lambda: f'{random_uuid()}', description='The request_id related to this request. If the caller does not set it, a random_uuid will be generated. This id is used through out the inference process and return in response.')
    normalize: Optional[bool] = None
    @model_validator(mode='before')
    @classmethod
    def check_generation_prompt(cls, data):
        if data.get('continue_final_message') and data.get('add_generation_prompt'):
            raise ValueError('Cannot set both `continue_final_message` and `add_generation_prompt` to True.')
        return data
    def to_pooling_params(self):
        return PoolingParams(dimensions=self.dimensions, normalize=self.normalize)
EmbeddingRequest = Union[EmbeddingCompletionRequest, EmbeddingChatRequest]
PoolingCompletionRequest = EmbeddingCompletionRequest
PoolingChatRequest = EmbeddingChatRequest
PoolingRequest = Union[PoolingCompletionRequest, PoolingChatRequest]
class ScoreRequest(OpenAIBaseModel):
    model: Optional[str] = None
    text_1: Union[list[str], str, ScoreMultiModalParam]
    text_2: Union[list[str], str, ScoreMultiModalParam]
    truncate_prompt_tokens: Optional[Annotated[int, Field(ge=-1)]] = None
    mm_processor_kwargs: Optional[dict[str, Any]] = Field(default=None, description='Additional kwargs to pass to the HF processor.')
    priority: int = Field(default=0, description='The priority of the request (lower means earlier handling; default: 0). Any priority other than 0 will raise an error if the served model does not use priority scheduling.')
    activation: Optional[bool] = None
    def to_pooling_params(self):
        return PoolingParams(activation=self.activation)
class RerankRequest(OpenAIBaseModel):
    model: Optional[str] = None
    query: Union[str, ScoreMultiModalParam]
    documents: Union[list[str], ScoreMultiModalParam]
    top_n: int = Field(default_factory=lambda: 0)
    truncate_prompt_tokens: Optional[Annotated[int, Field(ge=-1)]] = None
    mm_processor_kwargs: Optional[dict[str, Any]] = Field(default=None, description='Additional kwargs to pass to the HF processor.')
    priority: int = Field(default=0, description='The priority of the request (lower means earlier handling; default: 0). Any priority other than 0 will raise an error if the served model does not use priority scheduling.')
    activation: Optional[bool] = None
    def to_pooling_params(self):
        return PoolingParams(activation=self.activation)
class RerankDocument(BaseModel):
    text: Optional[str] = None
    multi_modal: Optional[ScoreContentPartParam] = None
class RerankResult(BaseModel):
    index: int
    document: RerankDocument
    relevance_score: float
class RerankUsage(BaseModel):
    total_tokens: int
class RerankResponse(OpenAIBaseModel):
    id: str
    model: str
    usage: RerankUsage
    results: list[RerankResult]
class CompletionLogProbs(OpenAIBaseModel):
    text_offset: list[int] = Field(default_factory=list)
    token_logprobs: list[Optional[float]] = Field(default_factory=list)
    tokens: list[str] = Field(default_factory=list)
    top_logprobs: list[Optional[dict[str, float]]] = Field(default_factory=list)
class CompletionResponseChoice(OpenAIBaseModel):
    index: int
    text: str
    logprobs: Optional[CompletionLogProbs] = None
    finish_reason: Optional[str] = None
    stop_reason: Optional[Union[int, str]] = Field(default=None, description='The stop string or token id that caused the completion to stop, None if the completion finished for some other reason including encountering the EOS token')
    prompt_logprobs: Optional[list[Optional[dict[int, Logprob]]]] = None
class CompletionResponse(OpenAIBaseModel):
    id: str = Field(default_factory=lambda: f'cmpl-{random_uuid()}')
    object: Literal['text_completion'] = 'text_completion'
    created: int = Field(default_factory=lambda: int(time.time()))
    model: str
    choices: list[CompletionResponseChoice]
    service_tier: Optional[Literal['auto', 'default', 'flex', 'scale', 'priority']] = None
    system_fingerprint: Optional[str] = None
    usage: UsageInfo
    kv_transfer_params: Optional[dict[str, Any]] = Field(default=None, description='KVTransfer parameters.')
class CompletionResponseStreamChoice(OpenAIBaseModel):
    index: int
    text: str
    logprobs: Optional[CompletionLogProbs] = None
    finish_reason: Optional[str] = None
    stop_reason: Optional[Union[int, str]] = Field(default=None, description='The stop string or token id that caused the completion to stop, None if the completion finished for some other reason including encountering the EOS token')
class CompletionStreamResponse(OpenAIBaseModel):
    id: str = Field(default_factory=lambda: f'cmpl-{random_uuid()}')
    object: str = 'text_completion'
    created: int = Field(default_factory=lambda: int(time.time()))
    model: str
    choices: list[CompletionResponseStreamChoice]
    usage: Optional[UsageInfo] = Field(default=None)
class EmbeddingResponseData(OpenAIBaseModel):
    index: int
    object: str = 'embedding'
    embedding: Union[list[float], str]
class EmbeddingResponse(OpenAIBaseModel):
    id: str = Field(default_factory=lambda: f'embd-{random_uuid()}')
    object: str = 'list'
    created: int = Field(default_factory=lambda: int(time.time()))
    model: str
    data: list[EmbeddingResponseData]
    usage: UsageInfo
class PoolingResponseData(OpenAIBaseModel):
    index: int
    object: str = 'pooling'
    data: Union[list[list[float]], list[float], str]
class PoolingResponse(OpenAIBaseModel):
    id: str = Field(default_factory=lambda: f'pool-{random_uuid()}')
    object: str = 'list'
    created: int = Field(default_factory=lambda: int(time.time()))
    model: str
    data: list[PoolingResponseData]
    usage: UsageInfo
class ScoreResponseData(OpenAIBaseModel):
    index: int
    object: str = 'score'
    score: float
class ScoreResponse(OpenAIBaseModel):
    id: str = Field(default_factory=lambda: f'embd-{random_uuid()}')
    object: str = 'list'
    created: int = Field(default_factory=lambda: int(time.time()))
    model: str
    data: list[ScoreResponseData]
    usage: UsageInfo
class ClassificationRequest(OpenAIBaseModel):
    model: Optional[str] = None
    input: Union[list[str], str]
    truncate_prompt_tokens: Optional[int] = None
    user: Optional[str] = None
    priority: int = Field(default=0, description='The priority of the request (lower means earlier handling; default: 0). Any priority other than 0 will raise an error if the served model does not use priority scheduling.')
    activation: Optional[bool] = None
    def to_pooling_params(self):
        return PoolingParams(activation=self.activation)
class ClassificationData(OpenAIBaseModel):
    index: int
    label: Optional[str]
    probs: list[float]
    num_classes: int
class ClassificationResponse(OpenAIBaseModel):
    id: str = Field(default_factory=lambda: f'classify-{random_uuid()}')
    object: str = 'list'
    created: int = Field(default_factory=lambda: int(time.time()))
    model: str
    data: list[ClassificationData]
    usage: UsageInfo
class FunctionCall(OpenAIBaseModel):
    name: str
    arguments: str
class ToolCall(OpenAIBaseModel):
    id: str = Field(default_factory=random_tool_call_id)
    type: Literal['function'] = 'function'
    function: FunctionCall
class DeltaFunctionCall(BaseModel):
    name: Optional[str] = None
    arguments: Optional[str] = None
class DeltaToolCall(OpenAIBaseModel):
    id: Optional[str] = None
    type: Optional[Literal['function']] = None
    index: int
    function: Optional[DeltaFunctionCall] = None
class ExtractedToolCallInformation(BaseModel):
    tools_called: bool
    tool_calls: list[ToolCall]
    content: Optional[str] = None
class ChatMessage(OpenAIBaseModel):
    role: str
    content: Optional[str] = None
    refusal: Optional[str] = None
    annotations: Optional[OpenAIAnnotation] = None
    audio: Optional[OpenAIChatCompletionAudio] = None
    function_call: Optional[FunctionCall] = None
    tool_calls: list[ToolCall] = Field(default_factory=list)
    reasoning_content: Optional[str] = None
class ChatCompletionLogProb(OpenAIBaseModel):
    token: str
    logprob: float = -9999.0
    bytes: Optional[list[int]] = None
class ChatCompletionLogProbsContent(ChatCompletionLogProb):
    field_names: ClassVar[Optional[set[str]]] = None
    top_logprobs: list[ChatCompletionLogProb] = Field(default_factory=list)
class ChatCompletionLogProbs(OpenAIBaseModel):
    content: Optional[list[ChatCompletionLogProbsContent]] = None
class ChatCompletionResponseChoice(OpenAIBaseModel):
    index: int
    message: ChatMessage
    logprobs: Optional[ChatCompletionLogProbs] = None
    finish_reason: Optional[str] = 'stop'
    stop_reason: Optional[Union[int, str]] = None
class ChatCompletionResponse(OpenAIBaseModel):
    id: str = Field(default_factory=lambda: f'chatcmpl-{random_uuid()}')
    object: Literal['chat.completion'] = 'chat.completion'
    created: int = Field(default_factory=lambda: int(time.time()))
    model: str
    choices: list[ChatCompletionResponseChoice]
    service_tier: Optional[Literal['auto', 'default', 'flex', 'scale', 'priority']] = None
    system_fingerprint: Optional[str] = None
    usage: UsageInfo
    prompt_logprobs: Optional[list[Optional[dict[int, Logprob]]]] = None
    kv_transfer_params: Optional[dict[str, Any]] = Field(default=None, description='KVTransfer parameters.')
class DeltaMessage(OpenAIBaseModel):
    role: Optional[str] = None
    content: Optional[str] = None
    reasoning_content: Optional[str] = None
    tool_calls: list[DeltaToolCall] = Field(default_factory=list)
class ChatCompletionResponseStreamChoice(OpenAIBaseModel):
    index: int
    delta: DeltaMessage
    logprobs: Optional[ChatCompletionLogProbs] = None
    finish_reason: Optional[str] = None
    stop_reason: Optional[Union[int, str]] = None
class ChatCompletionStreamResponse(OpenAIBaseModel):
    id: str = Field(default_factory=lambda: f'chatcmpl-{random_uuid()}')
    object: Literal['chat.completion.chunk'] = 'chat.completion.chunk'
    created: int = Field(default_factory=lambda: int(time.time()))
    model: str
    choices: list[ChatCompletionResponseStreamChoice]
    usage: Optional[UsageInfo] = Field(default=None)
class TranscriptionResponseStreamChoice(OpenAIBaseModel):
    delta: DeltaMessage
    finish_reason: Optional[str] = None
    stop_reason: Optional[Union[int, str]] = None
class TranscriptionStreamResponse(OpenAIBaseModel):
    id: str = Field(default_factory=lambda: f'trsc-{random_uuid()}')
    object: Literal['transcription.chunk'] = 'transcription.chunk'
    created: int = Field(default_factory=lambda: int(time.time()))
    model: str
    choices: list[TranscriptionResponseStreamChoice]
    usage: Optional[UsageInfo] = Field(default=None)
class ResponseReasoningItem(OpenAIBaseModel):
    id: str = Field(default_factory=lambda: f'rs_{random_uuid()}')
    text: str
    summary: list = Field(default_factory=list)
    type: Literal['reasoning'] = 'reasoning'
    encrypted_content: Optional[str] = None
    status: Optional[Literal['in_progress', 'completed', 'incomplete']]
class ResponsesResponse(OpenAIBaseModel):
    id: str = Field(default_factory=lambda: f'resp_{random_uuid()}')
    created_at: int = Field(default_factory=lambda: int(time.time()))
    instructions: Optional[str] = None
    metadata: Optional[Metadata] = None
    model: str
    object: Literal['response'] = 'response'
    output: list[Union[ResponseOutputMessage, ResponseReasoningItem]]
    parallel_tool_calls: bool
    temperature: float
    tool_choice: ToolChoice
    tools: list[Tool]
    top_p: float
    background: bool
    max_output_tokens: int
    max_tool_calls: Optional[int] = None
    previous_response_id: Optional[str] = None
    prompt: Optional[ResponsePrompt] = None
    reasoning: Optional[Reasoning] = None
    service_tier: Literal['auto', 'default', 'flex', 'scale', 'priority']
    status: ResponseStatus
    text: Optional[ResponseTextConfig] = None
    top_logprobs: int
    truncation: Literal['auto', 'disabled']
    usage: Optional[UsageInfo] = None
    user: Optional[str] = None
    @classmethod
    def from_request(cls, request: ResponsesRequest, sampling_params: SamplingParams, model_name: str, created_time: int, output: list[ResponseOutputItem], status: ResponseStatus, usage: Optional[UsageInfo]=None) -> 'ResponsesResponse':
        return cls(id=request.request_id, created_at=created_time, instructions=request.instructions, metadata=request.metadata, model=model_name, output=output, parallel_tool_calls=request.parallel_tool_calls, temperature=sampling_params.temperature, tool_choice=request.tool_choice, tools=request.tools, top_p=sampling_params.top_p, background=request.background, max_output_tokens=sampling_params.max_tokens, max_tool_calls=request.max_tool_calls, previous_response_id=request.previous_response_id, prompt=request.prompt, reasoning=request.reasoning, service_tier=request.service_tier, status=status, text=request.text, top_logprobs=sampling_params.logprobs, truncation=request.truncation, user=request.user, usage=usage)
BatchRequestInputBody = Union[ChatCompletionRequest, EmbeddingRequest, ScoreRequest, RerankRequest]
class BatchRequestInput(OpenAIBaseModel):
    custom_id: str
    method: str
    url: str
    body: BatchRequestInputBody
    @field_validator('body', mode='plain')
    @classmethod
    def check_type_for_url(cls, value: Any, info: ValidationInfo):
        url: str = info.data['url']
        if url == '/v1/chat/completions':
            return ChatCompletionRequest.model_validate(value)
        if url == '/v1/embeddings':
            return TypeAdapter(EmbeddingRequest).validate_python(value)
        if url.endswith('/score'):
            return ScoreRequest.model_validate(value)
        if url.endswith('/rerank'):
            return RerankRequest.model_validate(value)
        return TypeAdapter(BatchRequestInputBody).validate_python(value)
class BatchResponseData(OpenAIBaseModel):
    status_code: int = 200
    request_id: str
    body: Optional[Union[ChatCompletionResponse, EmbeddingResponse, ScoreResponse, RerankResponse]] = None
class BatchRequestOutput(OpenAIBaseModel):
    id: str
    custom_id: str
    response: Optional[BatchResponseData]
    error: Optional[Any]
class TokenizeCompletionRequest(OpenAIBaseModel):
    model: Optional[str] = None
    prompt: str
    add_special_tokens: bool = Field(default=True, description='If true (the default), special tokens (e.g. BOS) will be added to the prompt.')
    return_token_strs: Optional[bool] = Field(default=False, description='If true, also return the token strings corresponding to the token ids.')
class TokenizeChatRequest(OpenAIBaseModel):
    model: Optional[str] = None
    messages: list[ChatCompletionMessageParam]
    add_generation_prompt: bool = Field(default=True, description='If true, the generation prompt will be added to the chat template. This is a parameter used by chat template in tokenizer config of the model.')
    return_token_strs: Optional[bool] = Field(default=False, description='If true, also return the token strings corresponding to the token ids.')
    continue_final_message: bool = Field(default=False, description='If this is set, the chat will be formatted so that the final message in the chat is open-ended, without any EOS tokens. The model will continue this message rather than starting a new one. This allows you to "prefill" part of the model\'s response for it. Cannot be used at the same time as `add_generation_prompt`.')
    add_special_tokens: bool = Field(default=False, description='If true, special tokens (e.g. BOS) will be added to the prompt on top of what is added by the chat template. For most models, the chat template takes care of adding the special tokens so this should be set to false (as is the default).')
    chat_template: Optional[str] = Field(default=None, description='A Jinja template to use for this conversion. As of transformers v4.44, default chat template is no longer allowed, so you must provide a chat template if the tokenizer does not define one.')
    chat_template_kwargs: Optional[dict[str, Any]] = Field(default=None, description='Additional keyword args to pass to the template renderer. Will be accessible by the chat template.')
    mm_processor_kwargs: Optional[dict[str, Any]] = Field(default=None, description='Additional kwargs to pass to the HF processor.')
    tools: Optional[list[ChatCompletionToolsParam]] = Field(default=None, description='A list of tools the model may call.')
    @model_validator(mode='before')
    @classmethod
    def check_generation_prompt(cls, data):
        if data.get('continue_final_message') and data.get('add_generation_prompt'):
            raise ValueError('Cannot set both `continue_final_message` and `add_generation_prompt` to True.')
        return data
TokenizeRequest = Union[TokenizeCompletionRequest, TokenizeChatRequest]
class TokenizeResponse(OpenAIBaseModel):
    count: int
    max_model_len: int
    tokens: list[int]
    token_strs: Optional[list[str]] = None
class DetokenizeRequest(OpenAIBaseModel):
    model: Optional[str] = None
    tokens: list[int]
class DetokenizeResponse(OpenAIBaseModel):
    prompt: str
class TokenizerInfoResponse(OpenAIBaseModel):
    model_config = ConfigDict(extra='allow')
    tokenizer_class: str
class LoadLoRAAdapterRequest(BaseModel):
    lora_name: str
    lora_path: str
class UnloadLoRAAdapterRequest(BaseModel):
    lora_name: str
    lora_int_id: Optional[int] = Field(default=None)
class IncrementalUpdateRequest(BaseModel):
    parameter_name: str
    update_data: Any
    learning_rate: float = Field(default=0.01, ge=0.0, le=1.0)
    update_type: Literal['additive', 'multiplicative', 'replace'] = 'additive'
    metadata: Optional[Dict[str, Any]] = None
class ModelVersionRequest(BaseModel):
    description: str = Field(min_length=1, max_length=500)
class ModelRollbackRequest(BaseModel):
    version_id: str
class DynamicUpdateResponse(BaseModel):
    success: bool
    message: str
    data: Optional[Dict[str, Any]] = None
class ModelVersionInfo(BaseModel):
    version_id: str
    timestamp: float
    description: str
    is_active: bool
    performance_metrics: Dict[str, float]
class ModelVersionListResponse(BaseModel):
    versions: List[ModelVersionInfo]
    total_count: int
class ModelStatusResponse(BaseModel):
    current_version: Optional[str]
    total_versions: int
    total_updates: int
    config: Dict[str, Any]
    recent_performance: List[Dict[str, float]]
AudioResponseFormat: TypeAlias = Literal['json', 'text', 'srt', 'verbose_json', 'vtt']
class TranscriptionRequest(OpenAIBaseModel):
    file: UploadFile
    '\n    The audio file object (not file name) to transcribe, in one of these\n    formats: flac, mp3, mp4, mpeg, mpga, m4a, ogg, wav, or webm.\n    '
    model: Optional[str] = None
    'ID of the model to use.\n    '
    language: Optional[str] = None
    'The language of the input audio.\n\n    Supplying the input language in\n    [ISO-639-1](https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes) format\n    will improve accuracy and latency.\n    '
    prompt: str = Field(default='')
    "An optional text to guide the model's style or continue a previous audio\n    segment.\n\n    The [prompt](https://platform.openai.com/docs/guides/speech-to-text#prompting)\n    should match the audio language.\n    "
    response_format: AudioResponseFormat = Field(default='json')
    '\n    The format of the output, in one of these options: `json`, `text`, `srt`,\n    `verbose_json`, or `vtt`.\n    '
    timestamp_granularities: list[Literal['word', 'segment']] = Field(alias='timestamp_granularities[]', default=[])
    'The timestamp granularities to populate for this transcription.\n\n    `response_format` must be set `verbose_json` to use timestamp granularities.\n    Either or both of these options are supported: `word`, or `segment`. Note:\n    There is no additional latency for segment timestamps, but generating word\n    timestamps incurs additional latency.\n    '
    stream: Optional[bool] = False
    'When set, it will enable output to be streamed in a similar fashion\n    as the Chat Completion endpoint.\n    '
    stream_include_usage: Optional[bool] = False
    stream_continuous_usage_stats: Optional[bool] = False
    aphrodite_xargs: Optional[dict[str, Union[str, int, float]]] = Field(default=None, description='Additional request parameters with string or numeric values, used by custom extensions.')
    temperature: float = Field(default=0.0)
    'The sampling temperature, between 0 and 1.\n\n    Higher values like 0.8 will make the output more random, while lower values\n    like 0.2 will make it more focused / deterministic. If set to 0, the model\n    will use [log probability](https://en.wikipedia.org/wiki/Log_probability)\n    to automatically increase the temperature until certain thresholds are hit.\n    '
    top_p: Optional[float] = None
    'Enables nucleus (top-p) sampling, where tokens are selected from the\n    smallest possible set whose cumulative probability exceeds `p`.\n    '
    top_k: Optional[int] = None
    'Limits sampling to the `k` most probable tokens at each step.'
    min_p: Optional[float] = None
    'Filters out tokens with a probability lower than `min_p`, ensuring a\n    minimum likelihood threshold during sampling.\n    '
    seed: Optional[int] = Field(None, ge=_LONG_INFO.min, le=_LONG_INFO.max)
    'The seed to use for sampling.'
    frequency_penalty: Optional[float] = 0.0
    'The frequency penalty to use for sampling.'
    repetition_penalty: Optional[float] = None
    'The repetition penalty to use for sampling.'
    presence_penalty: Optional[float] = 0.0
    'The presence penalty to use for sampling.'
    _DEFAULT_SAMPLING_PARAMS: dict = {'repetition_penalty': 1.0, 'temperature': 1.0, 'top_p': 1.0, 'top_k': 0, 'min_p': 0.0}
    def to_sampling_params(self, default_max_tokens: int, default_sampling_params: Optional[dict]=None) -> SamplingParams:
        max_tokens = default_max_tokens
        if default_sampling_params is None:
            default_sampling_params = {}
        if (temperature := self.temperature) is None:
            temperature = default_sampling_params.get('temperature', self._DEFAULT_SAMPLING_PARAMS['temperature'])
        if (top_p := self.top_p) is None:
            top_p = default_sampling_params.get('top_p', self._DEFAULT_SAMPLING_PARAMS['top_p'])
        if (top_k := self.top_k) is None:
            top_k = default_sampling_params.get('top_k', self._DEFAULT_SAMPLING_PARAMS['top_k'])
        if (min_p := self.min_p) is None:
            min_p = default_sampling_params.get('min_p', self._DEFAULT_SAMPLING_PARAMS['min_p'])
        if (repetition_penalty := self.repetition_penalty) is None:
            repetition_penalty = default_sampling_params.get('repetition_penalty', self._DEFAULT_SAMPLING_PARAMS['repetition_penalty'])
        return SamplingParams.from_optional(temperature=temperature, max_tokens=max_tokens, seed=self.seed, top_p=top_p, top_k=top_k, min_p=min_p, frequency_penalty=self.frequency_penalty, repetition_penalty=repetition_penalty, presence_penalty=self.presence_penalty, output_kind=RequestOutputKind.DELTA if self.stream else RequestOutputKind.FINAL_ONLY, extra_args=self.aphrodite_xargs)
    @model_validator(mode='before')
    @classmethod
    def validate_transcription_request(cls, data):
        if isinstance(data.get('file'), str):
            raise HTTPException(status_code=HTTPStatus.UNPROCESSABLE_ENTITY, detail="Expected 'file' to be a file-like object, not 'str'.")
        stream_opts = ['stream_include_usage', 'stream_continuous_usage_stats']
        stream = data.get('stream', False)
        if any((bool(data.get(so, False)) for so in stream_opts)) and (not stream):
            raise ValueError('Stream options can only be defined when `stream=True`.')
        return data
class TranscriptionResponse(OpenAIBaseModel):
    text: str
    'The transcribed text.'
class TranscriptionWord(OpenAIBaseModel):
    end: float
    'End time of the word in seconds.'
    start: float
    'Start time of the word in seconds.'
    word: str
    'The text content of the word.'
class TranscriptionSegment(OpenAIBaseModel):
    id: int
    'Unique identifier of the segment.'
    avg_logprob: float
    'Average logprob of the segment.\n\n    If the value is lower than -1, consider the logprobs failed.\n    '
    compression_ratio: float
    'Compression ratio of the segment.\n\n    If the value is greater than 2.4, consider the compression failed.\n    '
    end: float
    'End time of the segment in seconds.'
    no_speech_prob: float
    'Probability of no speech in the segment.\n\n    If the value is higher than 1.0 and the `avg_logprob` is below -1, consider\n    this segment silent.\n    '
    seek: int
    'Seek offset of the segment.'
    start: float
    'Start time of the segment in seconds.'
    temperature: float
    'Temperature parameter used for generating the segment.'
    text: str
    'Text content of the segment.'
    tokens: list[int]
    'Array of token IDs for the text content.'
class TranscriptionResponseVerbose(OpenAIBaseModel):
    duration: str
    'The duration of the input audio.'
    language: str
    'The language of the input audio.'
    text: str
    'The transcribed text.'
    segments: Optional[list[TranscriptionSegment]] = None
    'Segments of the transcribed text and their corresponding details.'
    words: Optional[list[TranscriptionWord]] = None
    'Extracted words and their corresponding timestamps.'
class TranslationResponseStreamChoice(OpenAIBaseModel):
    delta: DeltaMessage
    finish_reason: Optional[str] = None
    stop_reason: Optional[Union[int, str]] = None
class TranslationStreamResponse(OpenAIBaseModel):
    id: str = Field(default_factory=lambda: f'trsl-{random_uuid()}')
    object: Literal['translation.chunk'] = 'translation.chunk'
    created: int = Field(default_factory=lambda: int(time.time()))
    model: str
    choices: list[TranslationResponseStreamChoice]
    usage: Optional[UsageInfo] = Field(default=None)
class TranslationRequest(OpenAIBaseModel):
    file: UploadFile
    '\n    The audio file object (not file name) to translate, in one of these\n    formats: flac, mp3, mp4, mpeg, mpga, m4a, ogg, wav, or webm.\n    '
    model: Optional[str] = None
    'ID of the model to use.\n    '
    prompt: str = Field(default='')
    "An optional text to guide the model's style or continue a previous audio\n    segment.\n    The [prompt](https://platform.openai.com/docs/guides/speech-to-text#prompting)\n    should match the audio language.\n    "
    response_format: AudioResponseFormat = Field(default='json')
    '\n    The format of the output, in one of these options: `json`, `text`, `srt`,\n    `verbose_json`, or `vtt`.\n    '
    temperature: float = Field(default=0.0)
    'The sampling temperature, between 0 and 1.\n    Higher values like 0.8 will make the output more random, while lower values\n    like 0.2 will make it more focused / deterministic. If set to 0, the model\n    will use [log probability](https://en.wikipedia.org/wiki/Log_probability)\n    to automatically increase the temperature until certain thresholds are hit.\n    '
    language: Optional[str] = None
    'The language of the input audio we translate from.\n    Supplying the input language in\n    [ISO-639-1](https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes) format\n    will improve accuracy.\n    '
    stream: Optional[bool] = False
    'Custom field not present in the original OpenAI definition. When set,\n    it will enable output to be streamed in a similar fashion as the Chat\n    Completion endpoint.\n    '
    stream_include_usage: Optional[bool] = False
    stream_continuous_usage_stats: Optional[bool] = False
    _DEFAULT_SAMPLING_PARAMS: dict = {'temperature': 0}
    def to_sampling_params(self, default_max_tokens: int, default_sampling_params: Optional[dict]=None) -> SamplingParams:
        max_tokens = default_max_tokens
        if default_sampling_params is None:
            default_sampling_params = {}
        if (temperature := self.temperature) is None:
            temperature = default_sampling_params.get('temperature', self._DEFAULT_SAMPLING_PARAMS['temperature'])
        return SamplingParams.from_optional(temperature=temperature, max_tokens=max_tokens, output_kind=RequestOutputKind.DELTA if self.stream else RequestOutputKind.FINAL_ONLY)
    @model_validator(mode='before')
    @classmethod
    def validate_stream_options(cls, data):
        stream_opts = ['stream_include_usage', 'stream_continuous_usage_stats']
        stream = data.get('stream', False)
        if any((bool(data.get(so, False)) for so in stream_opts)) and (not stream):
            raise ValueError('Stream options can only be defined when `stream=True`.')
        return data
class TranslationResponse(OpenAIBaseModel):
    text: str
    'The translated text.'
class TranslationWord(OpenAIBaseModel):
    end: float
    'End time of the word in seconds.'
    start: float
    'Start time of the word in seconds.'
    word: str
    'The text content of the word.'
class TranslationSegment(OpenAIBaseModel):
    id: int
    'Unique identifier of the segment.'
    avg_logprob: float
    'Average logprob of the segment.\n    If the value is lower than -1, consider the logprobs failed.\n    '
    compression_ratio: float
    'Compression ratio of the segment.\n    If the value is greater than 2.4, consider the compression failed.\n    '
    end: float
    'End time of the segment in seconds.'
    no_speech_prob: float
    'Probability of no speech in the segment.\n    If the value is higher than 1.0 and the `avg_logprob` is below -1, consider\n    this segment silent.\n    '
    seek: int
    'Seek offset of the segment.'
    start: float
    'Start time of the segment in seconds.'
    temperature: float
    'Temperature parameter used for generating the segment.'
    text: str
    'Text content of the segment.'
    tokens: list[int]
    'Array of token IDs for the text content.'
class TranslationResponseVerbose(OpenAIBaseModel):
    duration: str
    'The duration of the input audio.'
    language: str
    'The language of the input audio.'
    text: str
    'The translated text.'
    segments: Optional[list[TranslationSegment]] = None
    'Segments of the translated text and their corresponding details.'
    words: Optional[list[TranslationWord]] = None
    'Extracted words and their corresponding timestamps.'
class AnthropicContentBlock(OpenAIBaseModel):
    type: str
class AnthropicTextBlock(AnthropicContentBlock):
    type: Literal['text'] = 'text'
    text: str
class AnthropicImageSource(OpenAIBaseModel):
    type: Literal['base64'] = 'base64'
    media_type: Literal['image/jpeg', 'image/png', 'image/gif', 'image/webp']
    data: str
class AnthropicImageBlock(AnthropicContentBlock):
    type: Literal['image'] = 'image'
    source: AnthropicImageSource
class AnthropicToolUseBlock(AnthropicContentBlock):
    type: Literal['tool_use'] = 'tool_use'
    id: str
    name: str
    input: dict[str, Any]
class AnthropicToolResultBlock(AnthropicContentBlock):
    type: Literal['tool_result'] = 'tool_result'
    tool_use_id: str
    content: Optional[Union[str, list[Union[AnthropicTextBlock, AnthropicImageBlock]]]] = None
    is_error: Optional[bool] = False
class AnthropicThinkingBlock(AnthropicContentBlock):
    type: Literal['thinking'] = 'thinking'
    thinking: str
AnthropicContent = Union[AnthropicTextBlock, AnthropicImageBlock, AnthropicToolUseBlock, AnthropicToolResultBlock, AnthropicThinkingBlock]
class AnthropicMessage(OpenAIBaseModel):
    role: Literal['user', 'assistant']
    content: Union[str, list[AnthropicContent]]
class AnthropicTool(OpenAIBaseModel):
    name: str
    description: Optional[str] = None
    input_schema: dict[str, Any]
class AnthropicToolChoiceAuto(OpenAIBaseModel):
    type: Literal['auto'] = 'auto'
    disable_parallel_tool_use: Optional[bool] = False
class AnthropicToolChoiceAny(OpenAIBaseModel):
    type: Literal['any'] = 'any'
    disable_parallel_tool_use: Optional[bool] = False
class AnthropicToolChoiceNone(OpenAIBaseModel):
    type: Literal['none'] = 'none'
class AnthropicToolChoiceTool(OpenAIBaseModel):
    type: Literal['tool'] = 'tool'
    name: str
    disable_parallel_tool_use: Optional[bool] = False
AnthropicToolChoice = Union[AnthropicToolChoiceAuto, AnthropicToolChoiceAny, AnthropicToolChoiceNone, AnthropicToolChoiceTool]
class AnthropicThinkingConfig(OpenAIBaseModel):
    type: str
class AnthropicThinkingConfigEnabled(AnthropicThinkingConfig):
    type: Literal['enabled'] = 'enabled'
    budget_tokens: int = Field(ge=1024)
class AnthropicThinkingConfigDisabled(AnthropicThinkingConfig):
    type: Literal['disabled'] = 'disabled'
AnthropicThinking = Union[AnthropicThinkingConfigEnabled, AnthropicThinkingConfigDisabled]
class AnthropicMetadata(OpenAIBaseModel):
    user_id: Optional[str] = Field(None, max_length=256)
class AnthropicUsage(OpenAIBaseModel):
    input_tokens: int
    output_tokens: int
    cache_creation_input_tokens: Optional[int] = None
    cache_read_input_tokens: Optional[int] = None
class AnthropicMessagesRequest(OpenAIBaseModel):
    model: str = Field(min_length=1, max_length=256)
    messages: list[AnthropicMessage] = Field(max_items=100000)
    max_tokens: int = Field(ge=1)
    container: Optional[str] = None
    metadata: Optional[AnthropicMetadata] = None
    service_tier: Optional[Literal['auto', 'standard_only']] = None
    stop_sequences: Optional[list[str]] = Field(default_factory=list)
    stream: Optional[bool] = False
    system: Optional[Union[str, list[AnthropicTextBlock]]] = None
    temperature: Optional[float] = Field(None, ge=0.0, le=1.0)
    thinking: Optional[AnthropicThinking] = None
    tool_choice: Optional[AnthropicToolChoice] = None
    tools: Optional[list[AnthropicTool]] = None
    top_k: Optional[int] = Field(None, ge=0)
    top_p: Optional[float] = Field(None, ge=0.0, le=1.0)
    request_id: Optional[str] = None
    frequency_penalty: Optional[float] = 0.0
    presence_penalty: Optional[float] = 0.0
    seed: Optional[int] = None
class AnthropicMessagesResponse(OpenAIBaseModel):
    id: str
    type: Literal['message'] = 'message'
    role: Literal['assistant'] = 'assistant'
    content: list[AnthropicContent]
    model: str
    stop_reason: Optional[Literal['end_turn', 'max_tokens', 'stop_sequence', 'tool_use', 'pause_turn', 'refusal']]
    stop_sequence: Optional[str] = None
    usage: AnthropicUsage
    container: Optional[dict[str, Any]] = None
class AnthropicMessageStart(OpenAIBaseModel):
    type: Literal['message_start'] = 'message_start'
    message: AnthropicMessagesResponse
class AnthropicContentBlockStart(OpenAIBaseModel):
    type: Literal['content_block_start'] = 'content_block_start'
    index: int
    content_block: AnthropicContent
class AnthropicContentBlockDelta(OpenAIBaseModel):
    type: Literal['content_block_delta'] = 'content_block_delta'
    index: int
    delta: dict[str, Any]
class AnthropicContentBlockStop(OpenAIBaseModel):
    type: Literal['content_block_stop'] = 'content_block_stop'
    index: int
class AnthropicMessageDelta(OpenAIBaseModel):
    type: Literal['message_delta'] = 'message_delta'
    delta: dict[str, Any]
    usage: AnthropicUsage
class AnthropicMessageStop(OpenAIBaseModel):
    type: Literal['message_stop'] = 'message_stop'
class AnthropicPing(OpenAIBaseModel):
    type: Literal['ping'] = 'ping'
class AnthropicError(OpenAIBaseModel):
    type: Literal['error'] = 'error'
    error: dict[str, Any]
AnthropicStreamEvent = Union[AnthropicMessageStart, AnthropicContentBlockStart, AnthropicContentBlockDelta, AnthropicContentBlockStop, AnthropicMessageDelta, AnthropicMessageStop, AnthropicPing, AnthropicError]
class KAIGenerationInputSchema(BaseModel):
    genkey: Optional[str] = None
    prompt: str
    n: Optional[int] = 1
    max_context_length: int
    max_length: int
    rep_pen: Optional[float] = 1.0
    top_k: Optional[int] = 0
    top_a: Optional[float] = 0.0
    top_p: Optional[float] = 1.0
    min_p: Optional[float] = 0.0
    tfs: Optional[float] = 1.0
    eps_cutoff: Optional[float] = 0.0
    eta_cutoff: Optional[float] = 0.0
    typical: Optional[float] = 1.0
    temperature: Optional[float] = 1.0
    dynatemp_range: Optional[float] = 0.0
    dynatemp_exponent: Optional[float] = 1.0
    smoothing_factor: Optional[float] = 0.0
    smoothing_curve: Optional[float] = 1.0
    xtc_threshold: Optional[float] = 0.1
    xtc_probability: Optional[float] = 0.0
    use_default_badwordsids: Optional[bool] = None
    quiet: Optional[bool] = None
    sampler_seed: Optional[int] = None
    stop_sequence: Optional[list[str]] = None
    include_stop_str_in_output: Optional[bool] = False
    @model_validator(mode='before')
    def check_context(cls, values):
        assert values.get('max_length') <= values.get('max_context_length'), 'max_length must not be larger than max_context_length'
        return values