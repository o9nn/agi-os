from dataclasses import dataclass
from typing import List, Tuple, Type
import openvino as ov
import torch
from aphrodite.attention.backends.abstract import AttentionBackend, AttentionMetadata
from aphrodite.attention.backends.utils import CommonAttentionState
def copy_cache_block(src_tensor: ov.Tensor, dst_tensor: ov.Tensor, src_offset: int, dst_offset: int) -> None:
    def create_roi_tensor(tensor: ov.Tensor, block_number: int) -> ov.Tensor:
        roi_begin = ov.runtime.Coordinate([0, 0, 0, 0])
        roi_end = ov.runtime.Coordinate(tensor.get_shape())
        roi_begin[0] = block_number
        roi_end[0] = block_number + 1
        if isinstance(tensor, ov.Tensor):
            return ov.Tensor(tensor, roi_begin, roi_end)
        else:
            return ov.RemoteTensor(tensor, roi_begin, roi_end)
    src_roi_tensor = create_roi_tensor(src_tensor, src_offset)
    dst_roi_tensor = create_roi_tensor(dst_tensor, dst_offset)
    src_roi_tensor.copy_to(dst_roi_tensor)
class OpenVINOAttentionBackend(AttentionBackend):
    @staticmethod
    def get_name() -> str:
        return 'openvino'
    @staticmethod
    def get_impl_cls():
        raise NotImplementedError
    @staticmethod
    def make_metadata(*args, **kwargs) -> 'AttentionMetadata':
        raise NotImplementedError
    @staticmethod
    def get_state_cls() -> Type['CommonAttentionState']:
        return CommonAttentionState
    @staticmethod
    def make_openvino_metadata(*args, **kwargs) -> 'OpenVINOAttentionMetadata':
        return OpenVINOAttentionMetadata(*args, **kwargs)
    @staticmethod
    def get_kv_cache_shape(num_blocks: int, block_size: int, num_kv_heads: int, head_size: int) -> Tuple[int, ...]:
        return (2, num_blocks, num_kv_heads, block_size, head_size)
    @staticmethod
    def swap_blocks(src_tensor: ov.Tensor, dst_tensor: ov.Tensor, src_to_dists: List[Tuple[int, int]]) -> None:
        for src, dst in src_to_dists:
            copy_cache_block(src_tensor, dst_tensor, src, dst)
    @staticmethod
    def copy_blocks(kv_caches: List[Tuple[ov.Tensor, ov.Tensor]], src_to_dists: List[Tuple[int, int]]) -> None:
        for src, dst in src_to_dists:
            for key_cache, value_cache in kv_caches:
                copy_cache_block(key_cache, key_cache, src, dst)
                copy_cache_block(value_cache, value_cache, src, dst)
@dataclass
class OpenVINOAttentionMetadata:
    past_lens: torch.Tensor
    subsequence_begins: torch.Tensor
    block_indices: torch.Tensor
    block_indices_begins: torch.Tensor
    max_context_len: torch.Tensor