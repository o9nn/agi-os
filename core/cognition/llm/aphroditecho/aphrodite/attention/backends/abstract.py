from abc import ABC, abstractmethod
from contextlib import contextmanager
from dataclasses import dataclass, fields
from typing import TYPE_CHECKING, Any, Dict, Generic, List, Optional, Protocol, Set, Tuple, Type, TypeVar
import torch
from aphrodite.multimodal import MultiModalPlaceholderMap
if TYPE_CHECKING:
    from aphrodite.worker.model_runner_base import ModelRunnerBase, ModelRunnerInputBase, ModelRunnerInputBuilderBase
class AttentionType:
    DECODER = 'decoder'
    ENCODER = 'encoder'
    ENCODER_ONLY = 'encoder_only'
    ENCODER_DECODER = 'encoder_decoder'
class AttentionBackend(ABC):
    accept_output_buffer: bool = False
    @staticmethod
    @abstractmethod
    def get_name() -> str:
        raise NotImplementedError
    @staticmethod
    @abstractmethod
    def get_impl_cls() -> Type['AttentionImpl']:
        raise NotImplementedError
    @staticmethod
    @abstractmethod
    def get_metadata_cls() -> Type['AttentionMetadata']:
        raise NotImplementedError
    @staticmethod
    @abstractmethod
    def get_state_cls() -> Type['AttentionState']:
        raise NotImplementedError
    @classmethod
    def make_metadata(cls, *args, **kwargs) -> 'AttentionMetadata':
        return cls.get_metadata_cls()(*args, **kwargs)
    @staticmethod
    @abstractmethod
    def get_builder_cls() -> Type['AttentionMetadataBuilder']:
        raise NotImplementedError
    @staticmethod
    @abstractmethod
    def get_kv_cache_shape(num_blocks: int, block_size: int, num_kv_heads: int, head_size: int) -> Tuple[int, ...]:
        raise NotImplementedError
    @staticmethod
    def get_kv_cache_stride_order() -> Tuple[int, ...]:
        raise NotImplementedError
    @staticmethod
    @abstractmethod
    def swap_blocks(src_kv_cache: torch.Tensor, dst_kv_cache: torch.Tensor, src_to_dst: torch.Tensor) -> None:
        raise NotImplementedError
    @staticmethod
    @abstractmethod
    def copy_blocks(kv_caches: List[torch.Tensor], src_to_dists: torch.Tensor) -> None:
        raise NotImplementedError
    def advance_step(self, model_input: 'ModelRunnerInputBase', sampled_token_ids: Optional[torch.Tensor], block_size: int, num_seqs: int, num_queries: int) -> None:
        raise NotImplementedError
@dataclass
class AttentionMetadata:
    num_prefills: int
    num_prefill_tokens: int
    num_decode_tokens: int
    slot_mapping: torch.Tensor
    multi_modal_placeholder_index_maps: Optional[Dict[str, MultiModalPlaceholderMap.IndexMap]]
    enable_kv_scales_calculation: bool
    @property
    @abstractmethod
    def prefill_metadata(self) -> Optional['AttentionMetadata']:
        pass
    @property
    @abstractmethod
    def decode_metadata(self) -> Optional['AttentionMetadata']:
        pass
    def asdict_zerocopy(self, skip_fields: Optional[Set[str]]=None) -> Dict[str, Any]:
        if skip_fields is None:
            skip_fields = set()
        return {field.name: getattr(self, field.name) for field in fields(self) if field.name not in skip_fields}
T = TypeVar('T', bound=AttentionMetadata)
class AttentionState(ABC, Generic[T]):
    @abstractmethod
    def __init__(self, runner: 'ModelRunnerBase'):
        ...
    @abstractmethod
    @contextmanager
    def graph_capture(self, max_batch_size: int):
        yield
    @abstractmethod
    def graph_clone(self, batch_size: int) -> 'AttentionState[T]':
        ...
    @abstractmethod
    def graph_capture_get_metadata_for_batch(self, batch_size: int, is_encoder_decoder_model: bool=False) -> T:
        ...
    @abstractmethod
    def get_graph_input_buffers(self, attn_metadata: T, is_encoder_decoder_model: bool=False) -> Dict[str, Any]:
        ...
    @abstractmethod
    def prepare_graph_input_buffers(self, input_buffers: Dict[str, Any], attn_metadata: T, is_encoder_decoder_model: bool=False) -> None:
        ...
    @abstractmethod
    def begin_forward(self, model_input: 'ModelRunnerInputBase') -> None:
        ...
class AttentionMetadataBuilder(ABC, Generic[T]):
    @abstractmethod
    def __init__(self, input_builder: 'ModelRunnerInputBuilderBase') -> None:
        raise NotImplementedError
    @abstractmethod
    def prepare(self) -> None:
        raise NotImplementedError
    @abstractmethod
    def build(self, seq_lens: List[int], query_lens: List[int], cuda_graph_pad_size: int, batch_size: int) -> T:
        raise NotImplementedError
class AttentionLayer(Protocol):
    _q_scale: torch.Tensor
    _k_scale: torch.Tensor
    _v_scale: torch.Tensor
    _k_scale_float: float
    _v_scale_float: float
    _prob_scale: torch.Tensor
    def forward(self, query: torch.Tensor, key: torch.Tensor, value: torch.Tensor, kv_cache: torch.Tensor, attn_metadata: AttentionMetadata) -> torch.Tensor:
        ...
class AttentionImpl(ABC, Generic[T]):
    @abstractmethod
    def __init__(self, num_heads: int, head_size: int, scale: float, num_kv_heads: Optional[int]=None, alibi_slopes: Optional[List[float]]=None, sliding_window: Optional[int]=None, kv_cache_dtype: str='auto', blocksparse_params: Optional[Dict[str, Any]]=None, logits_soft_cap: Optional[float]=None, attn_type: str=AttentionType.DECODER) -> None:
        raise NotImplementedError
    @abstractmethod
    def forward(self, layer: AttentionLayer, query: torch.Tensor, key: torch.Tensor, value: torch.Tensor, kv_cache: torch.Tensor, attn_metadata: T, output: Optional[torch.Tensor]=None) -> torch.Tensor:
        raise NotImplementedError
class MLAAttentionImpl(AttentionImpl[T], Generic[T]):
    @abstractmethod
    def forward(self, layer: AttentionLayer, hidden_states_or_cq: torch.Tensor, kv_c_normed: torch.Tensor, k_pe: torch.Tensor, kv_cache: torch.Tensor, attn_metadata: T, output: Optional[torch.Tensor]=None) -> torch.Tensor:
        raise NotImplementedError
def is_quantized_kv_cache(kv_cache_dtype: str) -> bool:
    return kv_cache_dtype != 'auto'