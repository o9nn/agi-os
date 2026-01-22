from typing import Optional, Union, Tuple, List
import torch
import torch.nn as nn
try:
    from . import _vllm_fa2_C
    FA2_UNAVAILABLE_REASON = None
    FA2_AVAILABLE = True
except ImportError as e:
    FA2_UNAVAILABLE_REASON = str(e)
    FA2_AVAILABLE = False
try:
    from . import _vllm_fa3_C
    FA3_UNAVAILABLE_REASON = None
    FA3_AVAILABLE = True
except ImportError as e:
    FA3_UNAVAILABLE_REASON = str(e)
    FA3_AVAILABLE = False
DEFAULT_FA_VERSION = 2
def _is_fa2_supported(device=None) -> Tuple[bool, Optional[str]]:
    if not FA2_AVAILABLE:
        return (False, f'FA2 is unavaible due to: {FA2_UNAVAILABLE_REASON}')
    if torch.cuda.get_device_capability(device)[0] < 8:
        return (False, 'FA2 is only supported on devices with compute capability >= 8')
    return (True, None)
def _is_fa3_supported(device=None) -> Tuple[bool, Optional[str]]:
    if not FA3_AVAILABLE:
        return (False, f'FA3 is unavaible due to: {FA3_UNAVAILABLE_REASON}')
    if torch.cuda.get_device_capability(device)[0] < 8 or torch.cuda.get_device_capability(device)[0] >= 10 or torch.cuda.get_device_capability(device) == (8, 6) or (torch.cuda.get_device_capability(device) == (8, 9)):
        return (False, 'FA3 is only supported on devices with compute capability >= 8 excluding 8.6 and 8.9 and Blackwell archs (>=10)')
    return (True, None)
def is_fa_version_supported(fa_version: int, device=None) -> bool:
    assert fa_version in [2, 3], f'Unsupported FA version: {fa_version}'
    if fa_version == 2:
        return _is_fa2_supported(device)[0]
    elif fa_version == 3:
        return _is_fa3_supported(device)[0]
def fa_version_unsupported_reason(fa_version: int, device=None) -> Optional[str]:
    assert fa_version in [2, 3], f'Unsupported FA version: {fa_version}'
    if fa_version == 2:
        return _is_fa2_supported(device)[1]
    elif fa_version == 3:
        return _is_fa3_supported(device)[1]
def maybe_contiguous(x):
    return x.contiguous() if x is not None and x.stride(-1) != 1 else x
def get_scheduler_metadata(batch_size, max_seqlen_q, max_seqlen_k, num_heads_q, num_heads_kv, headdim, cache_seqlens: torch.Tensor, qkv_dtype=torch.bfloat16, headdim_v=None, cu_seqlens_q: Optional[torch.Tensor]=None, cu_seqlens_k_new: Optional[torch.Tensor]=None, cache_leftpad: Optional[torch.Tensor]=None, page_size: Optional[int]=None, max_seqlen_k_new=0, causal=False, window_size=(-1, -1), has_softcap=False, num_splits=0, pack_gqa=None, sm_margin=0):
    cache_seqlens = maybe_contiguous(cache_seqlens)
    if headdim_v is None:
        headdim_v = headdim
    scheduler_metadata = torch.ops._vllm_fa3_C.get_scheduler_metadata(batch_size, max_seqlen_q, max_seqlen_k, num_heads_q, num_heads_kv, headdim, headdim_v, qkv_dtype, cache_seqlens, cu_seqlens_q, None, cu_seqlens_k_new, None, cache_leftpad, page_size, max_seqlen_k_new, causal, window_size[0], window_size[1], has_softcap, num_splits, pack_gqa, sm_margin)
    return scheduler_metadata
def flash_attn_varlen_func(q, k, v, max_seqlen_q, cu_seqlens_q, max_seqlen_k, cu_seqlens_k=None, seqused_k=None, q_v=None, dropout_p=0.0, softmax_scale=None, causal=False, window_size: Optional[List[int]]=None, softcap=0.0, alibi_slopes=None, deterministic=False, return_attn_probs=False, block_table=None, return_softmax_lse=False, out=None, scheduler_metadata=None, q_descale=None, k_descale=None, v_descale=None, num_splits: int=0, fa_version: int=DEFAULT_FA_VERSION, s_aux=None):
    assert cu_seqlens_k is not None or seqused_k is not None, 'cu_seqlens_k or seqused_k must be provided'
    assert cu_seqlens_k is None or seqused_k is None, 'cu_seqlens_k and seqused_k cannot be provided at the same time'
    assert block_table is None or seqused_k is not None, 'seqused_k must be provided if block_table is provided'
    if softmax_scale is None:
        softmax_scale = q.shape[-1] ** (-0.5)
    real_window_size: Tuple[int, int]
    if window_size is None:
        real_window_size = (-1, -1)
    else:
        assert len(window_size) == 2
        real_window_size = (window_size[0], window_size[1])
    q, k, v = [maybe_contiguous(x) for x in (q, k, v)]
    dummy_cu_seqlens_k = torch.empty_like(cu_seqlens_q)
    if fa_version == 2:
        if scheduler_metadata is not None and q_descale is not None and (k_descale is not None) and (v_descale is not None):
            raise NotImplementedError('FA2 does not support scheduler_metadata, q_descale, k_descale, v_descale')
        if s_aux is not None:
            raise NotImplementedError('FA2 does not support s_aux')
        if num_splits > 1:
            raise NotImplementedError('FA2 does not support num_splits > 1')
        out, softmax_lse = torch.ops._vllm_fa2_C.varlen_fwd(q, k, v, out, cu_seqlens_q, dummy_cu_seqlens_k if cu_seqlens_k is None else cu_seqlens_k, seqused_k, None, block_table, alibi_slopes, max_seqlen_q, max_seqlen_k, dropout_p, softmax_scale, False, causal, real_window_size[0], real_window_size[1], softcap, return_softmax_lse and dropout_p > 0, None)
    elif fa_version == 3:
        assert alibi_slopes is None, 'Alibi is not supported in FA3'
        out, softmax_lse, _, _ = torch.ops._vllm_fa3_C.fwd(q, k, v, None, None, q_v, out, cu_seqlens_q, cu_seqlens_k, None, None, seqused_k, max_seqlen_q, max_seqlen_k, block_table, None, None, None, None, None, q_descale, k_descale, v_descale, softmax_scale, causal, real_window_size[0], real_window_size[1], softcap, True, scheduler_metadata, num_splits, None, 0, s_aux)
    else:
        raise ValueError(f'Unsupported FA version: {fa_version}')
    return (out, softmax_lse) if return_softmax_lse else out
def flash_attn_with_kvcache(q, k_cache, v_cache, k=None, v=None, rotary_cos=None, rotary_sin=None, cache_seqlens: Optional[Union[int, torch.Tensor]]=None, cache_batch_idx: Optional[torch.Tensor]=None, cache_leftpad: Optional[torch.Tensor]=None, block_table: Optional[torch.Tensor]=None, softmax_scale=None, causal=False, window_size=(-1, -1), softcap=0.0, rotary_interleaved=True, alibi_slopes=None, num_splits=0, return_softmax_lse=False, *, out=None, scheduler_metadata=None, q_descale=None, k_descale=None, v_descale=None, fa_version: int=DEFAULT_FA_VERSION, s_aux=None):
    assert k_cache.stride(-1) == 1, 'k_cache must have contiguous last dimension'
    assert v_cache.stride(-1) == 1, 'v_cache must have contiguous last dimension'
    q, k, v = [maybe_contiguous(x) for x in (q, k, v)]
    if softmax_scale is None:
        softmax_scale = q.shape[-1] ** (-0.5)
    if cache_seqlens is not None and isinstance(cache_seqlens, int):
        cache_seqlens = torch.full((k_cache.shape[0],), cache_seqlens, dtype=torch.int32, device=k_cache.device)
        cache_seqlens = maybe_contiguous(cache_seqlens)
    cache_batch_idx = maybe_contiguous(cache_batch_idx)
    block_table = maybe_contiguous(block_table)
    if fa_version == 2:
        if scheduler_metadata is not None and q_descale is not None and (k_descale is not None) and (v_descale is not None):
            raise NotImplementedError('FA2 does not support scheduler_metadata, q_descale, k_descale, v_descale')
        if s_aux is not None:
            raise NotImplementedError('FA2 does not support s_aux')
        out, softmax_lse = torch.ops._vllm_fa2_C.fwd_kvcache(q, k_cache, v_cache, k, v, cache_seqlens, rotary_cos, rotary_sin, cache_batch_idx, cache_leftpad, block_table, alibi_slopes, out, softmax_scale, causal, window_size[0], window_size[1], softcap, rotary_interleaved, num_splits)
    elif fa_version == 3:
        assert alibi_slopes is None, 'Alibi is not supported in FA3'
        out, softmax_lse, _, _ = torch.ops._vllm_fa3_C.fwd(q, k_cache, v_cache, k, v, None, out, None, None, None, None, cache_seqlens, None, None, block_table, cache_batch_idx, None, None, None, None, q_descale, k_descale, v_descale, softmax_scale, causal, window_size[0], window_size[1], softcap, rotary_interleaved, scheduler_metadata, num_splits, None, 0, s_aux)
    else:
        raise ValueError(f'Unsupported FA version: {fa_version}')
    return (out, softmax_lse) if return_softmax_lse else out
def sparse_attn_func(q, k, v, block_count, block_offset, column_count, column_index, dropout_p=0.0, softmax_scale=None, causal=False, softcap=0.0, alibi_slopes=None, deterministic=False, return_attn_probs=False, *, return_softmax_lse=False, out=None):
    if softmax_scale is None:
        softmax_scale = q.shape[-1] ** (-0.5)
    q, k, v = [maybe_contiguous(x) for x in (q, k, v)]
    out, softmax_lse = torch.ops._vllm_fa2_C.fwd_sparse(q, k, v, block_count, block_offset, column_count, column_index, out, alibi_slopes, dropout_p, softmax_scale, causal, softcap, return_attn_probs and dropout_p > 0, None)
    return (out, softmax_lse) if return_softmax_lse else out
def sparse_attn_varlen_func(q, k, v, block_count, block_offset, column_count, column_index, cu_seqlens_q, cu_seqlens_k, max_seqlen_q, max_seqlen_k, dropout_p=0.0, softmax_scale=None, causal=False, softcap=0.0, alibi_slopes=None, deterministic=False, return_attn_probs=False, *, return_softmax_lse=False, out=None):
    if softmax_scale is None:
        softmax_scale = q.shape[-1] ** (-0.5)
    q, k, v = [maybe_contiguous(x) for x in (q, k, v)]
    out, softmax_lse = torch.ops._vllm_fa2_C.varlen_fwd_sparse(q, k, v, block_count, block_offset, column_count, column_index, out, cu_seqlens_q, cu_seqlens_k, None, alibi_slopes, max_seqlen_q, max_seqlen_k, dropout_p, softmax_scale, False, causal, softcap, return_attn_probs and dropout_p > 0, None)
    return (out, softmax_lse) if return_softmax_lse else out