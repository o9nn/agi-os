from typing import Optional, Tuple
import torch
from aphrodite.platforms import current_platform
if current_platform.is_cuda():
    try:
        import aphrodite._flashmla_C
        _flashmla_C_AVAILABLE = True
    except ImportError:
        _flashmla_C_AVAILABLE = False
else:
    _flashmla_C_AVAILABLE = False
def is_flashmla_supported() -> Tuple[bool, Optional[str]]:
    if not current_platform.is_cuda():
        return (False, 'FlashMLA is only supported on CUDA devices.')
    if current_platform.get_device_capability()[0] != 9:
        return (False, 'FlashMLA is only supported on Hopper devices.')
    if not _flashmla_C_AVAILABLE:
        return (False, 'aphrodite._flashmla_C is not available, likely was not compiled due to insufficient nvcc version or a supported arch (only sm90a currently) was not in the list of target arches to compile for.')
    return (True, None)
def get_mla_metadata(cache_seqlens: torch.Tensor, num_heads_per_head_k: int, num_heads_k: int) -> Tuple[torch.Tensor, torch.Tensor]:
    return torch.ops._flashmla_C.get_mla_metadata(cache_seqlens, num_heads_per_head_k, num_heads_k)
def flash_mla_with_kvcache(q: torch.Tensor, k_cache: torch.Tensor, block_table: torch.Tensor, cache_seqlens: torch.Tensor, head_dim_v: int, tile_scheduler_metadata: torch.Tensor, num_splits: torch.Tensor, softmax_scale: Optional[float]=None, causal: bool=False) -> Tuple[torch.Tensor, torch.Tensor]:
    if softmax_scale is None:
        softmax_scale = q.shape[-1] ** (-0.5)
    out, softmax_lse = torch.ops._flashmla_C.fwd_kvcache_mla(q, k_cache, None, head_dim_v, cache_seqlens, block_table, softmax_scale, causal, tile_scheduler_metadata, num_splits)
    return (out, softmax_lse)