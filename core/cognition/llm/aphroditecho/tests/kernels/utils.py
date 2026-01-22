import itertools
import random
import unittest
from collections.abc import Sequence
from numbers import Number
from typing import Any, NamedTuple, Optional, Union
import pytest
import torch
from torch._prims_common import TensorLikeType
from aphrodite.attention import AttentionBackend, AttentionMetadata, AttentionType
from aphrodite.modeling.layers.activation import SiluAndMul
from aphrodite.platforms.interface import _Backend
from aphrodite.common.utils import STR_BACKEND_ENV_VAR, STR_FLASH_ATTN_VAL, STR_XFORMERS_ATTN_VAL, make_tensor_with_pad
DEFAULT_OPCHECK_TEST_UTILS: tuple[str, ...] = ('test_schema', 'test_autograd_registration', 'test_faketensor')
ALL_OPCHECK_TEST_UTILS: tuple[str, ...] = ('test_schema', 'test_autograd_registration', 'test_faketensor', 'test_aot_dispatch_dynamic')
class QKVInputs(NamedTuple):
    query: torch.Tensor
    key: torch.Tensor
    value: torch.Tensor
    q_seq_lens: list[int]
    kv_seq_lens: list[int]
class QKVO(NamedTuple):
    qkv: QKVInputs
    ideal_output: torch.Tensor
class PackedQKVInputs(NamedTuple):
    query: torch.Tensor
    key: torch.Tensor
    value: torch.Tensor
    q_start_loc_list: Optional[list[int]]
    kv_start_loc_list: Optional[list[int]]
    q_seq_lens: Optional[list[int]]
    kv_seq_lens: Optional[list[int]]
class PackedQKVO(NamedTuple):
    packed_qkv: Optional[PackedQKVInputs]
    ideal_output: torch.Tensor
class KVMemoryMap(NamedTuple):
    block_tables: torch.Tensor
    slot_mapping: torch.Tensor
class PhaseTestParameters(NamedTuple):
    packed_qkvo: PackedQKVO
    kv_mmap: Optional[KVMemoryMap]
def maybe_make_int_tensor(_list: Optional[list[int]], device: Union[torch.device, str]) -> torch.Tensor:
    return None if _list is None else torch.tensor(_list, dtype=torch.int, device=device)
def maybe_make_long_tensor(_list: Optional[list[int]], device: Union[torch.device, str]) -> torch.Tensor:
    return None if _list is None else torch.tensor(_list, dtype=torch.long, device=device)
def maybe_max(_list: Optional[list]) -> Optional[Number]:
    return None if _list is None else max(_list)
def make_causal_mask(q_max_seq_len: int, kv_max_seq_len: int) -> torch.Tensor:
    mask = torch.triu(torch.ones(q_max_seq_len, kv_max_seq_len), diagonal=1)
    mask = mask.masked_fill(mask == 1, float('-inf')).masked_fill(mask == 0, 0.0)
    return mask
def override_backend_env_variable(mpatch: pytest.MonkeyPatch, backend_name: str) -> None:
    mpatch.setenv(STR_BACKEND_ENV_VAR, backend_name)
def ref_masked_attention(query: torch.Tensor, key: torch.Tensor, value: torch.Tensor, scale: float, custom_mask: Optional[torch.Tensor]=None, q_seq_lens: Optional[list]=None, kv_seq_lens: Optional[list]=None) -> torch.Tensor:
    assert q_seq_lens is not None
    assert kv_seq_lens is not None
    batch_size = query.shape[0]
    assert len(q_seq_lens) == batch_size
    assert len(kv_seq_lens) == batch_size
    attn_weights = scale * torch.einsum('bqhd,bkhd->bhqk', query, key).float()
    if q_seq_lens is not None or kv_seq_lens is not None:
        attn_mask = torch.zeros_like(attn_weights)
        if q_seq_lens is not None:
            for bdx, plen in enumerate(q_seq_lens):
                attn_mask[bdx, :, plen:, :] = -torch.inf
        if kv_seq_lens is not None:
            for bdx, plen in enumerate(kv_seq_lens):
                attn_mask[bdx, :, :, plen:] = -torch.inf
        attn_weights = attn_weights + attn_mask.float()
    if custom_mask is not None:
        attn_weights = attn_weights + custom_mask.float()
    attn_weights = torch.softmax(attn_weights, dim=-1).to(value.dtype)
    out = torch.einsum('bhqk,bkhd->bqhd', attn_weights, value)
    return out
def make_qkv(batch_size: int, max_q_seq_len: int, max_kv_seq_len: Optional[int], num_heads: int, head_size: int, device: Union[torch.device, str], force_kv_seq_lens: Optional[list[int]]=None, attn_type: AttentionType=AttentionType.ENCODER_DECODER, force_max_len: bool=False) -> tuple[QKVInputs, QKVInputs, QKVInputs]:
    if force_max_len:
        q_seq_lens = [max_q_seq_len for _ in range(batch_size)]
    else:
        q_seq_lens = [random.randint(2, max_q_seq_len) for _ in range(batch_size)]
    kv_seq_lens = None
    if force_kv_seq_lens is not None:
        kv_seq_lens = force_kv_seq_lens
    elif attn_type != AttentionType.ENCODER_DECODER:
        kv_seq_lens = q_seq_lens
    else:
        assert max_kv_seq_len is not None
        if force_max_len:
            kv_seq_lens = [max_kv_seq_len] * batch_size
        else:
            kv_seq_lens = [random.randint(2, max_kv_seq_len) for _ in range(batch_size)]
    query = torch.rand((batch_size, max_q_seq_len, num_heads, head_size)).to(device)
    key = torch.rand((batch_size, max_kv_seq_len, num_heads, head_size)).to(device)
    value = torch.rand((batch_size, max_kv_seq_len, num_heads, head_size)).to(device)
    prefill_query = torch.zeros((batch_size, max_q_seq_len, num_heads, head_size)).to(device)
    prefill_key = torch.zeros((batch_size, max_kv_seq_len, num_heads, head_size)).to(device)
    prefill_value = torch.zeros((batch_size, max_kv_seq_len, num_heads, head_size)).to(device)
    decode_query = torch.zeros((batch_size, 1, num_heads, head_size)).to(device)
    decode_key = torch.zeros((batch_size, 1, num_heads, head_size)).to(device)
    decode_value = torch.zeros((batch_size, 1, num_heads, head_size)).to(device)
    for bdx, (q_seq_len, kv_seq_len) in enumerate(zip(q_seq_lens, kv_seq_lens)):
        query[bdx, q_seq_len:, :, :] = 0
        key[bdx, kv_seq_len:, :, :] = 0
        value[bdx, kv_seq_len:, :, :] = 0
        prefill_query[bdx, 0:q_seq_len - 1, :, :] = query[bdx, 0:q_seq_len - 1, :, :]
        prefill_key[bdx, 0:kv_seq_len - 1, :, :] = key[bdx, 0:kv_seq_len - 1, :, :]
        prefill_value[bdx, 0:kv_seq_len - 1, :, :] = value[bdx, 0:kv_seq_len - 1, :, :]
        decode_query[bdx, :, :, :] = query[bdx, q_seq_len - 1:q_seq_len, :, :]
        decode_key[bdx, :, :, :] = key[bdx, kv_seq_len - 1:kv_seq_len, :, :]
        decode_value[bdx, :, :, :] = value[bdx, kv_seq_len - 1:kv_seq_len, :, :]
    prefill_q_seq_lens = [plen - 1 for plen in q_seq_lens]
    prefill_kv_seq_lens = [plen - 1 for plen in kv_seq_lens]
    decode_q_seq_lens = [1 for _ in q_seq_lens]
    decode_kv_seq_lens = [1 for _ in kv_seq_lens]
    return (QKVInputs(query, key, value, q_seq_lens, kv_seq_lens), QKVInputs(prefill_query, prefill_key, prefill_value, prefill_q_seq_lens, prefill_kv_seq_lens), QKVInputs(decode_query, decode_key, decode_value, decode_q_seq_lens, decode_kv_seq_lens))
def pack_tensor(unpacked_tensor: torch.Tensor, seq_lens: list[int], device: Union[torch.device, str]) -> tuple[torch.Tensor, list[int]]:
    num_tok = sum(seq_lens)
    num_heads = unpacked_tensor.shape[-2]
    head_size = unpacked_tensor.shape[-1]
    start_loc_list = [0] + list(itertools.accumulate(seq_lens))
    packed_tensor = torch.zeros((num_tok, num_heads, head_size), device=device)
    for bdx, (seq_len, start_loc) in enumerate(zip(seq_lens, start_loc_list)):
        packed_tensor[start_loc:start_loc + seq_len, :, :] = unpacked_tensor[bdx, :seq_len, :, :]
    return (packed_tensor, start_loc_list)
def pack_qkv(qkv: QKVInputs, device: Union[torch.device, str]) -> PackedQKVInputs:
    if qkv.query is None:
        packed_query = None
        q_start_loc_list = None
    else:
        packed_query, q_start_loc_list = pack_tensor(qkv.query, qkv.q_seq_lens, device=device)
    packed_key, kv_start_loc_list = pack_tensor(qkv.key, qkv.kv_seq_lens, device=device)
    packed_value, _ = pack_tensor(qkv.value, qkv.kv_seq_lens, device=device)
    return PackedQKVInputs(packed_query, packed_key, packed_value, q_start_loc_list, kv_start_loc_list, None if q_start_loc_list is None else qkv.q_seq_lens, qkv.kv_seq_lens)
def make_backend(backend_name: str) -> AttentionBackend:
    if backend_name == STR_XFORMERS_ATTN_VAL:
        from aphrodite.attention.backends.xformers import XFormersBackend
        return XFormersBackend()
    elif backend_name == STR_FLASH_ATTN_VAL:
        from aphrodite.attention.backends.flash_attn import FlashAttentionBackend
        return FlashAttentionBackend()
    raise AssertionError(f'Unrecognized backend_name {backend_name} for unit test')
def _make_metadata_tensors(seq_lens: Optional[list[int]], context_lens: Optional[list[int]], encoder_seq_lens: Optional[list[int]], device: Union[torch.device, str]) -> tuple[torch.Tensor, torch.Tensor, Any, Any, Optional[torch.Tensor], torch.Tensor, torch.Tensor, Optional[int]]:
    seq_lens_tensor = maybe_make_int_tensor(seq_lens, device)
    context_lens_tensor = maybe_make_int_tensor(context_lens, device)
    max_context_len = maybe_max(context_lens)
    max_seq_len = maybe_max(seq_lens)
    encoder_seq_lens_tensor = maybe_make_int_tensor(encoder_seq_lens, device)
    max_encoder_seq_len = None if encoder_seq_lens is None else max(encoder_seq_lens)
    seq_start_loc = None
    if seq_lens_tensor is not None:
        seq_start_loc = torch.zeros(seq_lens_tensor.shape[0] + 1, dtype=torch.int32, device=seq_lens_tensor.device)
        torch.cumsum(seq_lens_tensor, dim=0, dtype=seq_start_loc.dtype, out=seq_start_loc[1:])
    encoder_seq_start_loc = torch.zeros(encoder_seq_lens_tensor.shape[0] + 1, dtype=torch.int32, device=encoder_seq_lens_tensor.device)
    torch.cumsum(encoder_seq_lens_tensor, dim=0, dtype=encoder_seq_start_loc.dtype, out=encoder_seq_start_loc[1:])
    return (seq_lens_tensor, context_lens_tensor, max_context_len, max_seq_len, seq_start_loc, encoder_seq_lens_tensor, encoder_seq_start_loc, max_encoder_seq_len)
def make_kv_cache(num_blocks: int, num_heads: int, head_size: int, block_size: int, device: Union[torch.device, str], backend: str, default_val: float=0.0) -> torch.Tensor:
    if backend == 'XFORMERS':
        kv_cache = torch.rand((2, num_blocks, block_size * num_heads * head_size)).to(device)
    elif backend == 'FLASH_ATTN':
        kv_cache = torch.rand((2, num_blocks, block_size, num_heads, head_size)).to(device)
    else:
        raise ValueError(f"Unknown backend value: '{backend}'. Expected 'XFORMERS' or 'FLASH_ATTN'.")
    if default_val is not None:
        kv_cache[:, :, :] = default_val
    return kv_cache
def _num_tokens_to_min_blocks(num_tokens: int, block_size: int) -> int:
    return (num_tokens + block_size) // block_size
def make_empty_slot_mapping_tensor(device: Union[torch.device, str]):
    return maybe_make_long_tensor([], device)
def make_empty_block_tables_tensor(device: Union[torch.device, str]):
    return torch.tensor([], device=device)
def split_slot_mapping(slot_mapping_list: torch.Tensor, seq_lens: list[int], device: Union[torch.device, str]):
    prefill_slot_mapping = []
    decode_slot_mapping = []
    base_idx = 0
    for seq_len in seq_lens:
        prefill_slot_mapping.extend(slot_mapping_list[base_idx:base_idx + seq_len - 1])
        decode_slot_mapping.append(slot_mapping_list[base_idx + seq_len - 1])
        base_idx += seq_len
    return (maybe_make_long_tensor(prefill_slot_mapping, device), maybe_make_long_tensor(decode_slot_mapping, device))
def make_block_tables_slot_mapping(block_size: int, seq_lens: list[int], device: Union[torch.device, str], block_base_addr: int=0) -> tuple[torch.Tensor, list[int], int]:
    num_blocks_list = [_num_tokens_to_min_blocks(num_tokens, block_size) for num_tokens in seq_lens]
    max_block_table_len = max(num_blocks_list)
    block_table_pad_tokens = 10
    block_tables = []
    slot_mapping_list = []
    total_cache_blocks = sum(num_blocks_list)
    block_base_idx = block_base_addr + total_cache_blocks
    max_block_idx = block_base_idx
    for sdx, num_tokens in enumerate(seq_lens):
        num_blocks = num_blocks_list[sdx]
        block_table = list(range(block_base_idx, block_base_idx - num_blocks, -1))
        for idx in range(num_tokens):
            mapping_value = idx % block_size + block_table[idx // block_size] * block_size
            slot_mapping_list.append(mapping_value)
        block_base_idx -= num_blocks
        block_tables.append(block_table)
    block_tables_tensor = make_tensor_with_pad(block_tables, max_len=max_block_table_len + block_table_pad_tokens, pad=0, dtype=torch.int, device=device)
    return (block_tables_tensor, slot_mapping_list, max_block_idx)
def make_test_metadata(attn_backend: _Backend, is_prompt: bool, seq_lens: Optional[list[int]], decoder_test_params: Optional[PhaseTestParameters], device: Union[torch.device, str], encoder_test_params: Optional[PhaseTestParameters]=None, cross_test_params: Optional[PhaseTestParameters]=None) -> AttentionMetadata:
    kv_mmap = None if decoder_test_params is None else decoder_test_params.kv_mmap
    num_prefills_or_decodes = None if seq_lens is None else len(seq_lens)
    num_prefill_or_decode_tokens = None if seq_lens is None else sum(seq_lens) if is_prompt else len(seq_lens)
    context_lens = None
    if encoder_test_params is None:
        encoder_seq_lens = None
        num_encoder_tokens = None
    else:
        assert encoder_test_params.packed_qkvo.packed_qkv is not None
        encoder_seq_lens = encoder_test_params.packed_qkvo.packed_qkv.q_seq_lens
        num_encoder_tokens = None if encoder_seq_lens is None else sum(encoder_seq_lens)
    if cross_test_params is None:
        cross_kv_mmap = None
    else:
        cross_kv_mmap = cross_test_params.kv_mmap
    attn_backend_obj = make_backend(attn_backend.name)
    if is_prompt:
        num_prefills = num_prefills_or_decodes
        num_prefill_tokens = num_prefill_or_decode_tokens
        num_decode_tokens = 0
        seq_lens_tensor, context_lens_tensor, _, _, seq_start_loc, encoder_seq_lens_tensor, encoder_seq_start_loc, max_encoder_seq_len = _make_metadata_tensors(seq_lens, context_lens, encoder_seq_lens, device=device)
        return attn_backend_obj.make_metadata(num_prefills=num_prefills, slot_mapping=None if kv_mmap is None else kv_mmap.slot_mapping, multi_modal_placeholder_index_maps=None, enable_kv_scales_calculation=True, num_prefill_tokens=num_prefill_tokens, num_decode_tokens=num_decode_tokens, seq_lens=seq_lens, seq_lens_tensor=seq_lens_tensor, seq_start_loc=seq_start_loc, max_prefill_seq_len=None if seq_lens is None else max(seq_lens), max_decode_seq_len=0, context_lens_tensor=context_lens_tensor, block_tables=None if kv_mmap is None else kv_mmap.block_tables, use_cuda_graph=False, num_encoder_tokens=num_encoder_tokens, encoder_seq_lens=encoder_seq_lens, encoder_seq_lens_tensor=encoder_seq_lens_tensor, encoder_seq_start_loc=encoder_seq_start_loc, max_encoder_seq_len=max_encoder_seq_len, cross_slot_mapping=None if cross_kv_mmap is None else cross_kv_mmap.slot_mapping, cross_block_tables=None if cross_kv_mmap is None else cross_kv_mmap.block_tables)
    else:
        assert kv_mmap is not None
        assert num_prefill_or_decode_tokens is not None
        assert seq_lens is not None
        num_prefills = 0
        num_prefill_tokens = 0
        num_decode_tokens = num_prefill_or_decode_tokens
        seq_lens_tensor, context_lens_tensor, _, _, seq_start_loc, encoder_seq_lens_tensor, encoder_seq_start_loc, max_encoder_seq_len = _make_metadata_tensors(seq_lens, context_lens, encoder_seq_lens, device=device)
        return attn_backend_obj.make_metadata(num_prefills=num_prefills, slot_mapping=kv_mmap.slot_mapping, multi_modal_placeholder_index_maps=None, enable_kv_scales_calculation=True, num_prefill_tokens=num_prefill_tokens, num_decode_tokens=num_decode_tokens, seq_lens=seq_lens, seq_lens_tensor=seq_lens_tensor, seq_start_loc=seq_start_loc, max_prefill_seq_len=0, max_decode_seq_len=max(seq_lens), max_decode_query_len=1, context_lens_tensor=context_lens_tensor, block_tables=kv_mmap.block_tables, use_cuda_graph=False, num_encoder_tokens=num_encoder_tokens, encoder_seq_lens=encoder_seq_lens, encoder_seq_lens_tensor=encoder_seq_lens_tensor, encoder_seq_start_loc=encoder_seq_start_loc, max_encoder_seq_len=max_encoder_seq_len, cross_slot_mapping=None if cross_kv_mmap is None else cross_kv_mmap.slot_mapping, cross_block_tables=None if cross_kv_mmap is None else cross_kv_mmap.block_tables)
def assert_actual_matches_ideal(test_params: PhaseTestParameters, output_under_test: torch.Tensor, backend: str) -> None:
    ideal_output = test_params.packed_qkvo.ideal_output
    if backend == 'XFORMERS':
        torch.testing.assert_close(ideal_output, output_under_test.view_as(ideal_output))
    elif backend == 'FLASH_ATTN':
        torch.testing.assert_close(ideal_output, output_under_test.view_as(ideal_output), atol=0.01, rtol=0.016)
    else:
        raise ValueError(f"Unknown backend value: '{backend}'. Expected 'XFORMERS' or 'FLASH_ATTN'.")
def fp8_allclose(a: TensorLikeType, b: TensorLikeType, rtol: float=1e-05, atol: float=1e-08, equal_nan: bool=False) -> bool:
    torch._refs._check_close_args(name='torch.allclose', a=a, b=b, rtol=rtol, atol=atol)
    return bool(torch.all(torch.isclose(a.double(), b.double(), rtol=rtol, atol=atol, equal_nan=equal_nan)).item())
def stack_and_dev(tensors: list[torch.Tensor]):
    dev = tensors[0].device
    return torch.stack(tensors, dim=0).to(dev)
def compute_max_diff(output, output_ref):
    return torch.mean(torch.abs(output - output_ref)) / torch.mean(torch.abs(output_ref))
def torch_moe(a, w1, w2, score, topk, expert_map):
    B, D = a.shape
    a = a.view(B, -1, D).repeat(1, topk, 1).reshape(-1, D)
    out = torch.zeros(B * topk, w2.shape[1], dtype=a.dtype, device=a.device)
    score = torch.softmax(score, dim=-1, dtype=torch.float32)
    topk_weight, topk_ids = torch.topk(score, topk)
    topk_weight = topk_weight.view(-1)
    topk_ids = topk_ids.view(-1)
    if expert_map is not None:
        topk_ids = expert_map[topk_ids]
    for i in range(w1.shape[0]):
        mask = topk_ids == i
        if mask.sum():
            out[mask] = SiluAndMul()(a[mask] @ w1[i].transpose(0, 1)) @ w2[i].transpose(0, 1)
    return (out.view(B, -1, w2.shape[1]) * topk_weight.view(B, -1, 1).to(out.dtype)).sum(dim=1)
def torch_moe_single(a, w, score, topk):
    B, D = a.shape
    a = a.view(B, -1, D).repeat(1, topk, 1).reshape(-1, D)
    out = torch.zeros(B * topk, w.shape[1], dtype=a.dtype, device=a.device)
    score = torch.softmax(score, dim=-1, dtype=torch.float32)
    _, topk_ids = torch.topk(score, topk)
    topk_ids = topk_ids.view(-1)
    for i in range(w.shape[0]):
        mask = topk_ids == i
        if mask.sum():
            out[mask] = a[mask] @ w[i].transpose(0, 1)
    return out.view(B, -1, w.shape[1]).sum(dim=1)
def opcheck(op: Union[torch._ops.OpOverload, torch._ops.OpOverloadPacket, torch._library.custom_ops.CustomOpDef], args: tuple[Any, ...], kwargs: Optional[dict[str, Any]]=None, *, test_utils: Union[str, Sequence[str]]=ALL_OPCHECK_TEST_UTILS, raise_exception: bool=True, cond: bool=True) -> dict[str, str]:
    with unittest.mock.patch('torch.allclose', new=fp8_allclose):
        return torch.library.opcheck(op, args, kwargs, test_utils=test_utils, raise_exception=raise_exception) if cond else {}
def to_fp8(tensor: torch.Tensor):
    finfo = torch.finfo(torch.float8_e4m3fn)
    return torch.round(tensor.clamp(min=finfo.min, max=finfo.max)).to(dtype=torch.float8_e4m3fn)
def to_int8(tensor: torch.Tensor):
    return torch.round(tensor.clamp(min=-128, max=127)).to(dtype=torch.int8)
def baseline_scaled_mm(a: torch.Tensor, b: torch.Tensor, scale_a: torch.Tensor, scale_b: torch.Tensor, out_dtype: type[torch.dtype], bias: Optional[torch.Tensor]=None) -> torch.Tensor:
    def group_broadcast(t, shape):
        for i, s in enumerate(shape):
            if t.shape[i] != s and t.shape[i] != 1:
                assert s % t.shape[i] == 0
                t = t.unsqueeze(i + 1).expand(*t.shape[:i + 1], s // t.shape[i], *t.shape[i + 1:]).flatten(i, i + 1)
        return t
    scale_a = group_broadcast(scale_a, a.shape)
    scale_b = group_broadcast(scale_b, b.shape)
    output = torch.mm(scale_a * a.to(dtype=torch.float32), scale_b * b.to(dtype=torch.float32)).to(out_dtype)
    if bias is not None:
        output = output + bias
    return output