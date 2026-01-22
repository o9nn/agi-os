from collections.abc import Mapping
from dataclasses import dataclass
from typing import TYPE_CHECKING, Any, NamedTuple, Optional, Union
import torch
from transformers import BatchFeature, PretrainedConfig, ProcessorMixin
from typing_extensions import TypeVar
from aphrodite.common.jsontree import JSONTree, json_map_leaves
from aphrodite.common.logger import log_once
from aphrodite.utils import get_allowed_kwarg_only_overrides
from aphrodite.transformers_utils.processor import cached_processor_from_config
if TYPE_CHECKING:
    from aphrodite.common.config import ModelConfig
    from aphrodite.common.sequence import SequenceData
    from aphrodite.multimodal import MultiModalDataDict, MultiModalPlaceholderDict, MultiModalRegistry
    from aphrodite.transformers_utils.tokenizer import AnyTokenizer
else:
    ModelConfig = Any
    MultiModalDataDict = Any
    MultiModalPlaceholderDict = Any
    MultiModalRegistry = Any
    SequenceData = Any
    AnyTokenizer = Any
_T = TypeVar('_T')
_C = TypeVar('_C', bound=PretrainedConfig, default=PretrainedConfig)
_P = TypeVar('_P', bound=ProcessorMixin, default=ProcessorMixin)
@dataclass(frozen=True)
class InputContext:
    model_config: ModelConfig
    'The configuration of the model.'
    def get_hf_config(self, typ: Union[type[_C], tuple[type[_C], ...]]=PretrainedConfig, /) -> _C:
        hf_config = self.model_config.hf_config
        if not isinstance(hf_config, typ):
            raise TypeError(f'Invalid type of HuggingFace config. Expected type: {typ}, but found type: {type(hf_config)}')
        return hf_config
    def get_hf_image_processor_config(self) -> dict[str, Any]:
        return self.model_config.hf_image_processor_config
    def get_mm_config(self):
        mm_config = self.model_config.multimodal_config
        if mm_config is None:
            raise RuntimeError('Not a multimodal model')
        return mm_config
    def get_hf_processor(self, typ: Union[type[_P], tuple[type[_P], ...]]=ProcessorMixin, /, **kwargs: object) -> _P:
        return cached_processor_from_config(self.model_config, processor_cls=typ, **kwargs)
    def init_processor(self, typ: type[_T], /, **kwargs: object) -> _T:
        mm_config = self.model_config.get_multimodal_config()
        base_kwargs = mm_config.mm_processor_kwargs
        if base_kwargs is None:
            base_kwargs = {}
        merged_kwargs = {**base_kwargs, **kwargs}
        return typ(**merged_kwargs)
@dataclass(frozen=True)
class InputProcessingContext(InputContext):
    tokenizer: AnyTokenizer
    'The tokenizer used to tokenize the inputs.'
    def get_hf_processor(self, typ: Union[type[_P], tuple[type[_P], ...]]=ProcessorMixin, /, **kwargs: object) -> _P:
        return super().get_hf_processor(typ, tokenizer=self.tokenizer, **kwargs)
    def call_hf_processor(self, hf_processor: ProcessorMixin, data: Mapping[str, object], kwargs: Mapping[str, object]={}) -> Union[BatchFeature, JSONTree]:
        assert callable(hf_processor)
        mm_config = self.model_config.get_multimodal_config()
        merged_kwargs = mm_config.merge_mm_processor_kwargs(kwargs)
        allowed_kwargs = get_allowed_kwarg_only_overrides(hf_processor, merged_kwargs, requires_kw_only=False, allow_var_kwargs=True)
        def maybe_cast_dtype(x):
            if isinstance(x, torch.Tensor) and x.is_floating_point():
                return x.to(dtype=self.model_config.dtype)
            return x
        try:
            output = hf_processor(**data, **allowed_kwargs, return_tensors='pt')
            if isinstance(output, BatchFeature):
                cast_output = json_map_leaves(maybe_cast_dtype, output.data)
                return BatchFeature(cast_output)
            cast_output = json_map_leaves(maybe_cast_dtype, output)
            log_once('WARNING', f'{type(hf_processor).__name__} did not return `BatchFeature`. Make sure to match the behaviour of `ProcessorMixin` when implementing custom processors.')
            return cast_output
        except Exception as exc:
            msg = f'Failed to apply {type(hf_processor).__name__} on data={data} with kwargs={allowed_kwargs}'
            raise ValueError(msg) from exc
class DummyData(NamedTuple):
    seq_data: SequenceData
    multi_modal_data: Optional[MultiModalDataDict] = None
    multi_modal_placeholders: Optional[MultiModalPlaceholderDict] = None
class InputRegistry:
    def dummy_data_for_profiling(self, model_config: ModelConfig, seq_len: int, mm_registry: MultiModalRegistry, is_encoder_data: bool=False) -> DummyData:
        from aphrodite.common.sequence import SequenceData
        if not model_config.is_multimodal_model:
            seq_data = SequenceData.from_prompt_token_counts((0, seq_len))
            return DummyData(seq_data=seq_data)
        if is_encoder_data:
            enc_data = mm_registry.get_encoder_dummy_data(model_config, seq_len)
            seq_data = SequenceData.from_seqs(enc_data.prompt_token_ids)
            return DummyData(seq_data=seq_data)
        dec_data = mm_registry.get_decoder_dummy_data(model_config, seq_len)
        return DummyData(seq_data=SequenceData.from_seqs(dec_data.prompt_token_ids), multi_modal_data=dec_data.multi_modal_data, multi_modal_placeholders=dec_data.multi_modal_placeholders)