import copy
from dataclasses import dataclass, fields
from math import prod
from typing import Optional
import torch
from typing_extensions import Self
from aphrodite.common.config import AphroditeConfig
from aphrodite.utils import cdiv, get_dtype_size
@dataclass(frozen=True)
class KVCacheSpec:
    block_size: int
    @property
    def page_size_bytes(self) -> int:
        raise NotImplementedError
    def max_memory_usage_bytes(self, aphrodite_config: AphroditeConfig) -> int:
        raise NotImplementedError
    @classmethod
    def merge(cls, specs: list[Self]) -> Self:
        assert all((spec == specs[0] for spec in specs[1:])), 'All layers in the same KV cache group must be the same.'
        return copy.deepcopy(specs[0])
@dataclass(frozen=True)
class AttentionSpec(KVCacheSpec):
    num_kv_heads: int
    head_size: int
    dtype: torch.dtype
    use_mla: bool
    @property
    def page_size_bytes(self) -> int:
        coef = 1 if self.use_mla else 2
        return coef * self.block_size * self.num_kv_heads * self.head_size * get_dtype_size(self.dtype)
@dataclass(frozen=True)
class FullAttentionSpec(AttentionSpec):
    sliding_window: Optional[int] = None
    attention_chunk_size: Optional[int] = None
    '\n    When hybrid allocator is disabled and the model contains both full \n    attention layers and sliding window attention layers, sliding \n    window attention are regarded as full attention in KV cache manager \n    (blocks are allocated for all tokens), while computed as sliding window \n    attention in model runner.\n    In this case, we use FullAttentionSpec and record the sliding window size.\n    Default to None for not using sliding window attention.\n    '
    def max_memory_usage_bytes(self, aphrodite_config: AphroditeConfig) -> int:
        max_model_len = aphrodite_config.model_config.max_model_len
        return cdiv(max_model_len, self.block_size) * self.page_size_bytes
    @classmethod
    def merge_window_sizes(cls, window_sizes: set[int]) -> Optional[int]:
        if len(window_sizes) == 0:
            return None
        elif len(window_sizes) == 1:
            return window_sizes.pop()
        else:
            raise ValueError('All attention layers in the same KV cache group must have the same window size.')
    @classmethod
    def merge(cls, specs: list[Self]) -> Self:
        assert all((isinstance(spec, FullAttentionSpec) for spec in specs)), 'All attention layers in the same KV cache group must be FullAttentionSpec.'
        sliding_window = set((spec.sliding_window for spec in specs if spec.sliding_window is not None))
        attention_chunk_size = set((spec.attention_chunk_size for spec in specs if spec.attention_chunk_size is not None))
        merged_spec = cls(block_size=specs[0].block_size, num_kv_heads=specs[0].num_kv_heads, head_size=specs[0].head_size, dtype=specs[0].dtype, use_mla=specs[0].use_mla, sliding_window=cls.merge_window_sizes(sliding_window), attention_chunk_size=cls.merge_window_sizes(attention_chunk_size))
        for spec in specs:
            for f in fields(AttentionSpec):
                assert getattr(spec, f.name) == getattr(merged_spec, f.name), 'All attention layers in the same KV cache group must have the same attention spec.'
        assert (merged_spec.sliding_window is not None) + (merged_spec.attention_chunk_size is not None) <= 1, 'Model with both sliding window layers and chunked local attention layers is not supported.'
        return merged_spec
@dataclass(frozen=True)
class ChunkedLocalAttentionSpec(AttentionSpec):
    attention_chunk_size: int
    def max_memory_usage_bytes(self, aphrodite_config: AphroditeConfig) -> int:
        max_model_len = aphrodite_config.model_config.max_model_len
        max_num_batched_tokens = aphrodite_config.scheduler_config.max_num_batched_tokens
        num_tokens = min(self.attention_chunk_size + max_num_batched_tokens, max_model_len)
        return cdiv(num_tokens, self.block_size) * self.page_size_bytes
@dataclass(frozen=True)
class SlidingWindowSpec(AttentionSpec):
    sliding_window: int
    def __post_init__(self):
        assert not self.use_mla, 'MLA is not supported for sliding window'
    def max_memory_usage_bytes(self, aphrodite_config: AphroditeConfig) -> int:
        max_model_len = aphrodite_config.model_config.max_model_len
        max_num_batched_tokens = aphrodite_config.scheduler_config.max_num_batched_tokens
        num_tokens = min(self.sliding_window - 1 + max_num_batched_tokens, max_model_len)
        return (cdiv(num_tokens, self.block_size) + 1) * self.page_size_bytes
@dataclass(frozen=True)
class MambaSpec(KVCacheSpec):
    shapes: tuple[tuple[int, ...], ...]
    dtype: torch.dtype
    page_size_padded: Optional[int] = None
    mamba_type: str = 'mamba2'
    @property
    def page_size_bytes(self) -> int:
        num_elements = sum((prod(shape) for shape in self.shapes))
        page_size = num_elements * get_dtype_size(self.dtype)
        if self.page_size_padded is not None:
            assert self.page_size_padded >= page_size
            return self.page_size_padded
        return page_size
    def max_memory_usage_bytes(self, aphrodite_config: AphroditeConfig) -> int:
        return self.page_size_bytes
@dataclass
class KVCacheTensor:
    size: int
    shared_by: list[str]
@dataclass
class KVCacheGroupSpec:
    layer_names: list[str]
    kv_cache_spec: KVCacheSpec
@dataclass
class KVCacheConfig:
    num_blocks: int
    'How should model runner initialize the KV cache tensors for each layer'
    kv_cache_tensors: list[KVCacheTensor]
    '\n    The kv cache groups of the model.\n    For models with only one type of attention, there is only one group that\n    contains all layers.\n    For models with multiple types of attention, there will be multiple groups,\n    see `_get_kv_cache_config_uniform_page_size` for more details.\n    '
    kv_cache_groups: list[KVCacheGroupSpec]