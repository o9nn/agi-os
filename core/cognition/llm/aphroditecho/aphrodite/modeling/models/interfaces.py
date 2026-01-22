from collections.abc import Iterable, Mapping, MutableSequence
from typing import TYPE_CHECKING, ClassVar, Literal, Optional, Protocol, Union, overload, runtime_checkable
import numpy as np
import torch
from loguru import logger
from torch import Tensor
from transformers.models.whisper.tokenization_whisper import LANGUAGES
from typing_extensions import Self, TypeIs
from aphrodite.common.config import ModelConfig, SpeechToTextConfig
from aphrodite.inputs import TokensPrompt
from aphrodite.inputs.data import PromptType
from aphrodite.quantization.base_config import QuantizationConfig
from aphrodite.utils import supports_kw
from .interfaces_base import is_pooling_model
if TYPE_CHECKING:
    from aphrodite.attention import AttentionMetadata
    from aphrodite.common.config import AphroditeConfig
    from aphrodite.common.sequence import IntermediateTensors
    from aphrodite.modeling.models.utils import WeightsMapper
MultiModalEmbeddings = Union[list[Tensor], Tensor, tuple[Tensor, ...]]
'\nThe output embeddings must be one of the following formats:\n\n- A list or tuple of 2D tensors, where each tensor corresponds to\n    each input multimodal data item (e.g, image).\n- A single 3D tensor, with the batch dimension grouping the 2D tensors.\n'
@runtime_checkable
class SupportsMultiModal(Protocol):
    supports_multimodal: ClassVar[Literal[True]] = True
    '\n    A flag that indicates this model supports multi-modal inputs.\n\n    Note:\n        There is no need to redefine this flag if this class is in the\n        MRO of your model class.\n    '
    @classmethod
    def get_placeholder_str(cls, modality: str, i: int) -> Optional[str]:
        ...
    def get_multimodal_embeddings(self, **kwargs: object) -> MultiModalEmbeddings:
        ...
    def get_language_model(self) -> torch.nn.Module:
        ...
    @overload
    def get_input_embeddings(self, input_ids: Tensor, multimodal_embeddings: Optional[MultiModalEmbeddings]=None, attn_metadata: Optional['AttentionMetadata']=None) -> Tensor:
        ...
    @overload
    def get_input_embeddings(self, input_ids: Tensor, multimodal_embeddings: Optional[MultiModalEmbeddings]=None) -> Tensor:
        ...
    def get_input_embeddings(self, input_ids: Tensor, multimodal_embeddings: Optional[MultiModalEmbeddings]=None, attn_metadata: Optional['AttentionMetadata']=None) -> Tensor:
        ...
@overload
def supports_multimodal(model: type[object]) -> TypeIs[type[SupportsMultiModal]]:
    ...
@overload
def supports_multimodal(model: object) -> TypeIs[SupportsMultiModal]:
    ...
def supports_multimodal(model: Union[type[object], object]) -> Union[TypeIs[type[SupportsMultiModal]], TypeIs[SupportsMultiModal]]:
    return getattr(model, 'supports_multimodal', False)
@runtime_checkable
class SupportsMultiModalWithRawInput(SupportsMultiModal, Protocol):
    supports_multimodal_raw_input: ClassVar[Literal[True]] = True
    '\n    A flag that indicates this model supports multi-modal inputs and processes\n    them in their raw form and not embeddings.\n\n    Note:\n        There is no need to redefine this flag if this class is in the\n        MRO of your model class.\n    '
@overload
def supports_multimodal_raw_input(model: object) -> TypeIs[SupportsMultiModalWithRawInput]:
    ...
@overload
def supports_multimodal_raw_input(model: type[object]) -> TypeIs[type[SupportsMultiModalWithRawInput]]:
    ...
def supports_multimodal_raw_input(model: Union[type[object], object]) -> Union[TypeIs[type[SupportsMultiModalWithRawInput]], TypeIs[SupportsMultiModalWithRawInput]]:
    return getattr(model, 'supports_multimodal_raw_input', False)
@runtime_checkable
class SupportsScoreTemplate(Protocol):
    supports_score_template: ClassVar[Literal[True]] = True
    '\n    A flag that indicates this model supports score template.\n\n    Note:\n        There is no need to redefine this flag if this class is in the\n        MRO of your model class.\n    '
    @classmethod
    def get_score_template(cls, query: str, document: str) -> Optional[str]:
        ...
    @classmethod
    def post_process_tokens(cls, prompt: TokensPrompt) -> None:
        ...
@overload
def supports_score_template(model: type[object]) -> TypeIs[type[SupportsScoreTemplate]]:
    ...
@overload
def supports_score_template(model: object) -> TypeIs[SupportsScoreTemplate]:
    ...
def supports_score_template(model: Union[type[object], object]) -> Union[TypeIs[type[SupportsScoreTemplate]], TypeIs[SupportsScoreTemplate]]:
    return getattr(model, 'supports_score_template', False)
@runtime_checkable
class SupportsLoRA(Protocol):
    supports_lora: ClassVar[Literal[True]] = True
    '\n    A flag that indicates this model supports LoRA.\n\n    Note:\n        There is no need to redefine this flag if this class is in the\n        MRO of your model class.\n    '
    embedding_modules: ClassVar[dict[str, str]] = {}
    embedding_padding_modules: ClassVar[list[str]] = []
    packed_modules_mapping: ClassVar[dict[str, list[str]]] = {}
@runtime_checkable
class _SupportsLoRAType(Protocol):
    supports_lora: Literal[True]
    packed_modules_mapping: dict[str, list[str]]
    embedding_modules: dict[str, str]
    embedding_padding_modules: list[str]
@overload
def supports_lora(model: type[object]) -> TypeIs[type[SupportsLoRA]]:
    ...
@overload
def supports_lora(model: object) -> TypeIs[SupportsLoRA]:
    ...
def supports_lora(model: Union[type[object], object]) -> Union[TypeIs[type[SupportsLoRA]], TypeIs[SupportsLoRA]]:
    result = _supports_lora(model)
    if not result:
        lora_attrs = ('packed_modules_mapping', 'embedding_modules', 'embedding_padding_modules')
        missing_attrs = tuple((attr for attr in lora_attrs if not hasattr(model, attr)))
        if getattr(model, 'supports_lora', False):
            if missing_attrs:
                logger.warning('The model ({}) sets `supports_lora=True`, but is missing LoRA-specific attributes: {}', model, missing_attrs)
        elif not missing_attrs:
            logger.warning('The model ({}) contains all LoRA-specific attributes, but does not set `supports_lora=True`.', model)
    return result
def _supports_lora(model: Union[type[object], object]) -> bool:
    if isinstance(model, type):
        return isinstance(model, _SupportsLoRAType)
    return isinstance(model, SupportsLoRA)
@runtime_checkable
class SupportsPP(Protocol):
    supports_pp: ClassVar[Literal[True]] = True
    '\n    A flag that indicates this model supports pipeline parallel.\n\n    Note:\n        There is no need to redefine this flag if this class is in the\n        MRO of your model class.\n    '
    def make_empty_intermediate_tensors(self, batch_size: int, dtype: torch.dtype, device: torch.device) -> 'IntermediateTensors':
        ...
    def forward(self, *, intermediate_tensors: Optional['IntermediateTensors']) -> Union[Tensor, 'IntermediateTensors']:
        ...
@runtime_checkable
class _SupportsPPType(Protocol):
    supports_pp: Literal[True]
    def make_empty_intermediate_tensors(self, batch_size: int, dtype: torch.dtype, device: torch.device) -> 'IntermediateTensors':
        ...
    def forward(self, *, intermediate_tensors: Optional['IntermediateTensors']) -> Union[Tensor, 'IntermediateTensors']:
        ...
@overload
def supports_pp(model: type[object]) -> TypeIs[type[SupportsPP]]:
    ...
@overload
def supports_pp(model: object) -> TypeIs[SupportsPP]:
    ...
def supports_pp(model: Union[type[object], object]) -> Union[bool, TypeIs[type[SupportsPP]], TypeIs[SupportsPP]]:
    supports_attributes = _supports_pp_attributes(model)
    supports_inspect = _supports_pp_inspect(model)
    if supports_attributes and (not supports_inspect):
        logger.warning('The model ({}) sets `supports_pp=True`, but does not accept `intermediate_tensors` in its `forward` method', model)
    if not supports_attributes:
        pp_attrs = ('make_empty_intermediate_tensors',)
        missing_attrs = tuple((attr for attr in pp_attrs if not hasattr(model, attr)))
        if getattr(model, 'supports_pp', False):
            if missing_attrs:
                logger.warning('The model ({}) sets `supports_pp=True`, but is missing PP-specific attributes: {}', model, missing_attrs)
        elif not missing_attrs:
            logger.warning('The model ({}) contains all PP-specific attributes, but does not set `supports_pp=True`.', model)
    return supports_attributes and supports_inspect
def _supports_pp_attributes(model: Union[type[object], object]) -> bool:
    if isinstance(model, type):
        return isinstance(model, _SupportsPPType)
    return isinstance(model, SupportsPP)
def _supports_pp_inspect(model: Union[type[object], object]) -> bool:
    model_forward = getattr(model, 'forward', None)
    if not callable(model_forward):
        return False
    return supports_kw(model_forward, 'intermediate_tensors')
@runtime_checkable
class HasInnerState(Protocol):
    has_inner_state: ClassVar[Literal[True]] = True
    '\n        A flag that indicates this model has inner state.\n        Models that has inner state usually need access to the scheduler_config\n        for max_num_seqs, etc. True for e.g. both Mamba and Jamba.\n    '
@overload
def has_inner_state(model: object) -> TypeIs[HasInnerState]:
    ...
@overload
def has_inner_state(model: type[object]) -> TypeIs[type[HasInnerState]]:
    ...
def has_inner_state(model: Union[type[object], object]) -> Union[TypeIs[type[HasInnerState]], TypeIs[HasInnerState]]:
    return getattr(model, 'has_inner_state', False)
@runtime_checkable
class IsAttentionFree(Protocol):
    is_attention_free: ClassVar[Literal[True]] = True
    '\n        A flag that indicates this model has no attention.\n        Used for block manager and attention backend selection.\n        True for Mamba but not Jamba.\n    '
@overload
def is_attention_free(model: object) -> TypeIs[IsAttentionFree]:
    ...
@overload
def is_attention_free(model: type[object]) -> TypeIs[type[IsAttentionFree]]:
    ...
def is_attention_free(model: Union[type[object], object]) -> Union[TypeIs[type[IsAttentionFree]], TypeIs[IsAttentionFree]]:
    return getattr(model, 'is_attention_free', False)
@runtime_checkable
class IsHybrid(Protocol):
    is_hybrid: ClassVar[Literal[True]] = True
    "\n        A flag that indicates this model has both mamba and attention blocks\n        , also indicates that the model's hf_config has \n        'layers_block_type' "
    @classmethod
    def get_mamba_state_shape_from_config(cls, aphrodite_config: 'AphroditeConfig', use_v1: bool=True) -> tuple[tuple[int, int], tuple[int, int, int]]:
        ...
@overload
def is_hybrid(model: object) -> TypeIs[IsHybrid]:
    ...
@overload
def is_hybrid(model: type[object]) -> TypeIs[type[IsHybrid]]:
    ...
def is_hybrid(model: Union[type[object], object]) -> Union[TypeIs[type[IsHybrid]], TypeIs[IsHybrid]]:
    return getattr(model, 'is_hybrid', False)
@runtime_checkable
class MixtureOfExperts(Protocol):
    expert_weights: MutableSequence[Iterable[Tensor]]
    '\n    Expert weights saved in this rank.\n\n    The first dimension is the layer, and the second dimension is different\n    parameters in the layer, e.g. up/down projection weights.\n    '
    num_moe_layers: int
    'Number of MoE layers in this model.'
    num_expert_groups: int
    'Number of expert groups in this model.'
    num_logical_experts: int
    'Number of logical experts in this model.'
    num_physical_experts: int
    'Number of physical experts in this model.'
    num_local_physical_experts: int
    'Number of local physical experts in this model.'
    num_routed_experts: int
    'Number of routed experts in this model.'
    num_shared_experts: int
    'Number of shared experts in this model.'
    num_redundant_experts: int
    'Number of redundant experts in this model.'
    def set_eplb_state(self, expert_load_view: Tensor, logical_to_physical_map: Tensor, logical_replica_count: Tensor) -> None:
        ...
    def update_physical_experts_metadata(self, num_physical_experts: int, num_local_physical_experts: int) -> None:
        ...
def is_mixture_of_experts(model: object) -> TypeIs[MixtureOfExperts]:
    return isinstance(model, MixtureOfExperts)
@runtime_checkable
class HasNoOps(Protocol):
    has_noops: ClassVar[Literal[True]] = True
@overload
def has_noops(model: object) -> TypeIs[HasNoOps]:
    ...
@overload
def has_noops(model: type[object]) -> TypeIs[type[HasNoOps]]:
    ...
def has_noops(model: Union[type[object], object]) -> Union[TypeIs[type[HasNoOps]], TypeIs[HasNoOps]]:
    return getattr(model, 'has_noops', False)
@runtime_checkable
class SupportsCrossEncoding(Protocol):
    supports_cross_encoding: ClassVar[Literal[True]] = True
@overload
def supports_cross_encoding(model: type[object]) -> TypeIs[type[SupportsCrossEncoding]]:
    ...
@overload
def supports_cross_encoding(model: object) -> TypeIs[SupportsCrossEncoding]:
    ...
def _supports_cross_encoding(model: Union[type[object], object]) -> Union[TypeIs[type[SupportsCrossEncoding]], TypeIs[SupportsCrossEncoding]]:
    return getattr(model, 'supports_cross_encoding', False)
def supports_cross_encoding(model: Union[type[object], object]) -> Union[TypeIs[type[SupportsCrossEncoding]], TypeIs[SupportsCrossEncoding]]:
    return is_pooling_model(model) and _supports_cross_encoding(model)
class SupportsQuant:
    hf_to_aphrodite_mapper: ClassVar[Optional['WeightsMapper']] = None
    packed_modules_mapping: ClassVar[Optional[dict[str, list[str]]]] = None
    quant_config: Optional[QuantizationConfig] = None
    def __new__(cls, *args, **kwargs) -> Self:
        instance = super().__new__(cls)
        quant_config = cls._find_quant_config(*args, **kwargs)
        if quant_config is not None:
            instance.quant_config = quant_config
            if (hf_to_aphrodite_mapper := instance.hf_to_aphrodite_mapper) is not None:
                instance.quant_config.apply_aphrodite_mapper(hf_to_aphrodite_mapper)
            if instance.packed_modules_mapping is not None:
                instance.quant_config.packed_modules_mapping.update(instance.packed_modules_mapping)
        return instance
    @staticmethod
    def _find_quant_config(*args, **kwargs) -> Optional[QuantizationConfig]:
        from aphrodite.common.config import AphroditeConfig
        args_values = list(args) + list(kwargs.values())
        for arg in args_values:
            if isinstance(arg, AphroditeConfig):
                return arg.quant_config
            if isinstance(arg, QuantizationConfig):
                return arg
        return None
@runtime_checkable
class SupportsTranscription(Protocol):
    supported_languages: ClassVar[Mapping[str, str]]
    supports_transcription: ClassVar[Literal[True]] = True
    supports_transcription_only: ClassVar[bool] = False
    '\n    Transcription models can opt out of text generation by setting this to\n    `True`.\n    '
    def __init_subclass__(cls, **kwargs):
        super().__init_subclass__(**kwargs)
        invalid = set(cls.supported_languages) - set(LANGUAGES.keys())
        if invalid:
            raise ValueError(f'{cls.__name__}.supported_languages contains invalid language codes: {sorted(invalid)}\n. Valid choices are: {sorted(LANGUAGES.keys())}')
    @classmethod
    def get_generation_prompt(cls, audio: np.ndarray, stt_config: SpeechToTextConfig, model_config: ModelConfig, language: Optional[str], task_type: str, request_prompt: str) -> PromptType:
        ...
    @classmethod
    def get_other_languages(cls) -> Mapping[str, str]:
        return {k: v for k, v in LANGUAGES.items() if k not in cls.supported_languages}
    @classmethod
    def validate_language(cls, language: Optional[str]) -> Optional[str]:
        if language is None or language in cls.supported_languages:
            return language
        elif language in cls.get_other_languages():
            logger.warning('Language {} is not natively supported by {}; results may be less accurate. Supported languages: {}', language, cls.__name__, list(cls.supported_languages.keys()))
            return language
        else:
            raise ValueError(f'Unsupported language: {language!r}.  Must be one of {list(cls.supported_languages.keys())}.')
    @classmethod
    def get_speech_to_text_config(cls, model_config: ModelConfig, task_type: Literal['transcribe', 'translate']) -> SpeechToTextConfig:
        ...
    @classmethod
    def get_num_audio_tokens(cls, audio_duration_s: float, stt_config: SpeechToTextConfig, model_config: ModelConfig) -> Optional[int]:
        return None
@overload
def supports_transcription(model: type[object]) -> TypeIs[type[SupportsTranscription]]:
    ...
@overload
def supports_transcription(model: object) -> TypeIs[SupportsTranscription]:
    ...
def supports_transcription(model: Union[type[object], object]) -> Union[TypeIs[type[SupportsTranscription]], TypeIs[SupportsTranscription]]:
    return getattr(model, 'supports_transcription', False)
@runtime_checkable
class SupportsV0Only(Protocol):
    supports_v0_only: ClassVar[Literal[True]] = True
@overload
def supports_v0_only(model: type[object]) -> TypeIs[type[SupportsV0Only]]:
    ...
@overload
def supports_v0_only(model: object) -> TypeIs[SupportsV0Only]:
    ...
def supports_v0_only(model: Union[type[object], object]) -> Union[TypeIs[type[SupportsV0Only]], TypeIs[SupportsV0Only]]:
    return getattr(model, 'supports_v0_only', False)