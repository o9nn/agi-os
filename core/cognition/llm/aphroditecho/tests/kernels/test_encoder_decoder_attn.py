from typing import NamedTuple, Optional
import pytest
import torch
from aphrodite.attention import Attention, AttentionBackend, AttentionMetadata, AttentionType
from aphrodite.attention.backends.utils import STR_NOT_IMPL_ENC_DEC_ROCM_HIP
from aphrodite.attention.selector import _Backend, global_force_attn_backend_context_manager
from aphrodite.common.utils import is_hip
from tests.kernels.utils import *
LIST_ENC_DEC_SUPPORTED_BACKENDS = [_Backend.XFORMERS]
HEAD_SIZES = [64, 256]
NUM_HEADS = [1, 16]
BATCH_SIZES = [1, 16]
BLOCK_SIZES = [16]
CUDA_DEVICE = 'cuda:0'
MAX_DEC_SEQ_LENS = [128]
MAX_ENC_SEQ_LENS = [128]
HEAD_SIZES_FOR_UNSUPP = [HEAD_SIZES[0]]
class TestPoint(NamedTuple):
    num_heads: int
    head_size: int
    backend_name: str
    batch_size: int
    block_size: int
    max_dec_seq_len: int
    max_enc_seq_len: int
    num_blocks: int
class TestResources(NamedTuple):
    scale: float
    attn_backend: AttentionBackend
    attn: Attention
    kv_cache: torch.Tensor
def _make_test_resources(test_pt: TestPoint) -> TestResources:
    scale = float(1.0 / test_pt.head_size ** 0.5)
    attn_backend = make_backend(test_pt.backend_name)
    attn = Attention(test_pt.num_heads, test_pt.head_size, scale=scale)
    if test_pt.num_blocks is None or test_pt.num_heads is None:
        return TestResources(scale, attn_backend, attn, torch.tensor([], dtype=torch.float32, device=CUDA_DEVICE))
    kv_cache = make_kv_cache(test_pt.num_blocks, test_pt.num_heads, test_pt.head_size, test_pt.block_size, device=CUDA_DEVICE)
    return TestResources(scale, attn_backend, attn, kv_cache)
def _encoder_attn_setup(test_pt: TestPoint, test_rsrcs: TestResources) -> PhaseTestParameters:
    num_heads, head_size, _, batch_size, _, _, max_q_seq_len, _ = test_pt
    scale = test_rsrcs.scale
    max_kv_seq_len = max_q_seq_len
    qkv_in, _, _ = make_qkv(batch_size, max_q_seq_len, max_kv_seq_len, num_heads, head_size, attn_type=AttentionType.ENCODER, device=CUDA_DEVICE)
    ideal_output = ref_masked_attention(qkv_in.query, qkv_in.key, qkv_in.value, scale=scale, q_seq_lens=qkv_in.q_seq_lens, kv_seq_lens=qkv_in.kv_seq_lens)
    packed_ideal_output, _ = pack_tensor(ideal_output, qkv_in.q_seq_lens, device=CUDA_DEVICE)
    packed_qkv = pack_qkv(qkv_in, device=CUDA_DEVICE)
    return PhaseTestParameters(PackedQKVO(packed_qkv, packed_ideal_output), None)
def _decoder_attn_setup(test_pt: TestPoint, test_rsrcs: TestResources, block_base_addr: int=0) -> Tuple[QKVInputs, PhaseTestParameters, PhaseTestParameters, int]:
    num_heads, head_size, _, batch_size, block_size, max_q_seq_len, _, _ = test_pt
    scale = test_rsrcs.scale
    max_kv_seq_len = max_q_seq_len
    qkv, prefill_qkv, decode_qkv = make_qkv(batch_size, max_q_seq_len, max_kv_seq_len, num_heads, head_size, attn_type=AttentionType.DECODER, device=CUDA_DEVICE)
    causal_mask = make_causal_mask(max_q_seq_len, max_kv_seq_len).to(CUDA_DEVICE)
    ideal_output = ref_masked_attention(qkv.query, qkv.key, qkv.value, scale=scale, custom_mask=causal_mask, q_seq_lens=qkv.q_seq_lens, kv_seq_lens=qkv.kv_seq_lens)
    prefill_ideal_output = torch.zeros_like(ideal_output)
    decode_ideal_output = torch.zeros_like(ideal_output[:, 0:1])
    for bdx, prefill_q_seq_len in enumerate(prefill_qkv.q_seq_lens):
        prefill_ideal_output[bdx, :prefill_q_seq_len] = ideal_output[bdx, :prefill_q_seq_len]
        decode_ideal_output[bdx, :] = ideal_output[bdx, prefill_q_seq_len:prefill_q_seq_len + 1]
    prefill_packed_ideal_output, _ = pack_tensor(prefill_ideal_output, prefill_qkv.q_seq_lens, device=CUDA_DEVICE)
    decode_packed_ideal_output, _ = pack_tensor(decode_ideal_output, [1 for _ in range(batch_size)], device=CUDA_DEVICE)
    prefill_block_tables = make_empty_block_tables_tensor(device=CUDA_DEVICE)
    decode_block_tables, slot_mapping_list, max_block_idx = make_block_tables_slot_mapping(block_size, qkv.q_seq_lens, device=CUDA_DEVICE, block_base_addr=block_base_addr)
    prefill_slot_mapping, decode_slot_mapping = split_slot_mapping(slot_mapping_list, qkv.q_seq_lens, device=CUDA_DEVICE)
    prefill_pckd_qkv = pack_qkv(prefill_qkv, device=CUDA_DEVICE)
    decode_pckd_qkv = pack_qkv(decode_qkv, device=CUDA_DEVICE)
    return (qkv, PhaseTestParameters(PackedQKVO(prefill_pckd_qkv, prefill_packed_ideal_output), KVMemoryMap(prefill_block_tables, prefill_slot_mapping)), PhaseTestParameters(PackedQKVO(decode_pckd_qkv, decode_packed_ideal_output), KVMemoryMap(decode_block_tables, decode_slot_mapping)), max_block_idx)
def _enc_dec_cross_attn_setup_reuses_query(decoder_qkv: QKVInputs, encoder_test_params: PhaseTestParameters, prefill_decoder_phase_test_params: PhaseTestParameters, test_pt: TestPoint, test_rsrcs: TestResources, block_base_addr: int=0) -> Tuple[PhaseTestParameters, PhaseTestParameters]:
    assert encoder_test_params.packed_qkvo.packed_qkv is not None
    assert prefill_decoder_phase_test_params.packed_qkvo.packed_qkv is not None
    num_heads, head_size, _, batch_size, block_size, max_decoder_seq_len, max_encoder_seq_len, _ = test_pt
    scale = test_rsrcs.scale
    decoder_query = decoder_qkv.query
    decoder_seq_lens = decoder_qkv.q_seq_lens
    encoder_seq_lens = encoder_test_params.packed_qkvo.packed_qkv.q_seq_lens
    prefill_q_seq_lens = prefill_decoder_phase_test_params.packed_qkvo.packed_qkv.q_seq_lens
    assert prefill_q_seq_lens is not None
    cross_kv, _, _ = make_qkv(batch_size, max_decoder_seq_len, max_encoder_seq_len, num_heads, head_size, force_kv_seq_lens=encoder_seq_lens, attn_type=AttentionType.ENCODER_DECODER, device=CUDA_DEVICE)
    ideal_output = ref_masked_attention(decoder_query, cross_kv.key, cross_kv.value, scale=scale, q_seq_lens=decoder_seq_lens, kv_seq_lens=cross_kv.kv_seq_lens)
    prefill_ideal_output = torch.zeros_like(ideal_output)
    decode_ideal_output = torch.zeros_like(ideal_output[:, 0:1])
    for bdx, prefill_q_seq_len in enumerate(prefill_q_seq_lens):
        prefill_ideal_output[bdx, :prefill_q_seq_len] = ideal_output[bdx, :prefill_q_seq_len]
        decode_ideal_output[bdx, :] = ideal_output[bdx, prefill_q_seq_len:prefill_q_seq_len + 1]
    prefill_packed_ideal_output, _ = pack_tensor(prefill_ideal_output, prefill_q_seq_lens, device=CUDA_DEVICE)
    decode_packed_ideal_output, _ = pack_tensor(decode_ideal_output, [1 for _ in range(batch_size)], device=CUDA_DEVICE)
    prefill_block_tables = make_empty_block_tables_tensor(device=CUDA_DEVICE)
    decode_slot_mapping = make_empty_slot_mapping_tensor(device=CUDA_DEVICE)
    decode_block_tables, prefill_slot_mapping_list, _ = make_block_tables_slot_mapping(block_size, cross_kv.kv_seq_lens, block_base_addr=block_base_addr, device=CUDA_DEVICE)
    prefill_slot_mapping = maybe_make_long_tensor(prefill_slot_mapping_list, device=CUDA_DEVICE)
    packed_cross_kv = pack_qkv(cross_kv, device=CUDA_DEVICE)
    return (PhaseTestParameters(PackedQKVO(packed_cross_kv, prefill_packed_ideal_output), KVMemoryMap(prefill_block_tables, prefill_slot_mapping)), PhaseTestParameters(PackedQKVO(None, decode_packed_ideal_output), KVMemoryMap(decode_block_tables, decode_slot_mapping)))
def _run_encoder_attention_test(attn: Attention, encoder_test_params: PhaseTestParameters, attn_metadata: AttentionMetadata) -> torch.Tensor:
    assert attn_metadata.num_decode_tokens == 0
    attn_type = AttentionType.ENCODER
    packed_qkv = encoder_test_params.packed_qkvo.packed_qkv
    assert packed_qkv is not None
    return attn.forward(packed_qkv.query, packed_qkv.key, packed_qkv.value, torch.tensor([], dtype=torch.float32, device=packed_qkv.query.device), attn_metadata, attn_type=attn_type)
def _run_decoder_self_attention_test(test_rsrcs: TestResources, decoder_test_params: PhaseTestParameters, attn_metadata: AttentionMetadata) -> torch.Tensor:
    attn_type = AttentionType.DECODER
    attn = test_rsrcs.attn
    kv_cache = test_rsrcs.kv_cache
    packed_qkv = decoder_test_params.packed_qkvo.packed_qkv
    assert packed_qkv is not None
    return attn.forward(packed_qkv.query, packed_qkv.key, packed_qkv.value, kv_cache, attn_metadata, attn_type=attn_type)
def _run_encoder_decoder_cross_attention_test(test_rsrcs: TestResources, decoder_test_params: PhaseTestParameters, cross_test_params: Optional[PhaseTestParameters], attn_metadata: AttentionMetadata) -> torch.Tensor:
    assert decoder_test_params.packed_qkvo.packed_qkv is not None
    attn_type = AttentionType.ENCODER_DECODER
    attn = test_rsrcs.attn
    kv_cache = test_rsrcs.kv_cache
    if cross_test_params is None:
        key = None
        value = None
    else:
        cross_pckd_qkv = cross_test_params.packed_qkvo.packed_qkv
        key = None if cross_pckd_qkv is None else cross_pckd_qkv.key
        value = None if cross_pckd_qkv is None else cross_pckd_qkv.value
    return attn.forward(decoder_test_params.packed_qkvo.packed_qkv.query, key, value, kv_cache, attn_metadata, attn_type=attn_type)
@pytest.mark.skipif(is_hip(), reason=STR_NOT_IMPL_ENC_DEC_ROCM_HIP)
@pytest.mark.parametrize('num_heads', NUM_HEADS)
@pytest.mark.parametrize('head_size', HEAD_SIZES)
@pytest.mark.parametrize('attn_backend', LIST_ENC_DEC_SUPPORTED_BACKENDS)
@pytest.mark.parametrize('batch_size', BATCH_SIZES)
@pytest.mark.parametrize('block_size', BLOCK_SIZES)
@pytest.mark.parametrize('max_dec_seq_len', MAX_DEC_SEQ_LENS)
@pytest.mark.parametrize('max_enc_seq_len', MAX_ENC_SEQ_LENS)
def test_encoder_only(num_heads: int, head_size: int, attn_backend: _Backend, batch_size: int, block_size: int, max_dec_seq_len: int, max_enc_seq_len: int):
    with global_force_attn_backend_context_manager(attn_backend):
        test_pt = TestPoint(num_heads, head_size, attn_backend.name, batch_size, block_size, max_dec_seq_len, max_enc_seq_len, 4096)
        test_rsrcs = _make_test_resources(test_pt)
        enc_test_params = _encoder_attn_setup(test_pt, test_rsrcs)
        prephase_attn_metadata: AttentionMetadata = make_test_metadata(test_rsrcs.attn_backend, True, None, decoder_test_params=None, encoder_test_params=enc_test_params, cross_test_params=None, device=CUDA_DEVICE)
        enc_pckd_act_out: torch.Tensor = _run_encoder_attention_test(test_rsrcs.attn, enc_test_params, prephase_attn_metadata)
        assert_actual_matches_ideal(enc_test_params, enc_pckd_act_out)
@pytest.mark.skipif(is_hip(), reason=STR_NOT_IMPL_ENC_DEC_ROCM_HIP)
@pytest.mark.parametrize('num_heads', NUM_HEADS)
@pytest.mark.parametrize('head_size', HEAD_SIZES)
@pytest.mark.parametrize('attn_backend', LIST_ENC_DEC_SUPPORTED_BACKENDS)
@pytest.mark.parametrize('batch_size', BATCH_SIZES)
@pytest.mark.parametrize('block_size', BLOCK_SIZES)
@pytest.mark.parametrize('max_dec_seq_len', MAX_DEC_SEQ_LENS)
@pytest.mark.parametrize('max_enc_seq_len', MAX_ENC_SEQ_LENS)
def test_e2e_enc_dec_attn(num_heads: int, head_size: int, attn_backend: _Backend, batch_size: int, block_size: int, max_dec_seq_len: int, max_enc_seq_len: int) -> None:
    with global_force_attn_backend_context_manager(attn_backend):
        test_pt = TestPoint(num_heads, head_size, attn_backend.name, batch_size, block_size, max_dec_seq_len, max_enc_seq_len, 4096)
        test_rsrcs = _make_test_resources(test_pt)
        enc_test_params = _encoder_attn_setup(test_pt, test_rsrcs)
        dec_qkv, prephase_dec_test_params, decphase_dec_test_params, cross_block_base_addr = _decoder_attn_setup(test_pt, test_rsrcs)
        prephase_cross_test_params, decphase_cross_test_params = _enc_dec_cross_attn_setup_reuses_query(dec_qkv, enc_test_params, prephase_dec_test_params, test_pt, test_rsrcs, block_base_addr=cross_block_base_addr)
        assert prephase_dec_test_params.packed_qkvo.packed_qkv is not None
        prephase_attn_metadata: AttentionMetadata = make_test_metadata(test_rsrcs.attn_backend, True, prephase_dec_test_params.packed_qkvo.packed_qkv.q_seq_lens, decoder_test_params=prephase_dec_test_params, encoder_test_params=enc_test_params, cross_test_params=prephase_cross_test_params, device=CUDA_DEVICE)
        enc_pckd_act_out = _run_encoder_attention_test(test_rsrcs.attn, enc_test_params, prephase_attn_metadata)
        assert_actual_matches_ideal(enc_test_params, enc_pckd_act_out)
        prephase_dec_pckd_act_out = _run_decoder_self_attention_test(test_rsrcs, prephase_dec_test_params, prephase_attn_metadata)
        assert_actual_matches_ideal(prephase_dec_test_params, prephase_dec_pckd_act_out)
        prephase_cross_pckd_act_out = _run_encoder_decoder_cross_attention_test(test_rsrcs, prephase_dec_test_params, prephase_cross_test_params, prephase_attn_metadata)
        assert_actual_matches_ideal(prephase_cross_test_params, prephase_cross_pckd_act_out)
        decphase_attn_metadata: AttentionMetadata = make_test_metadata(test_rsrcs.attn_backend, False, dec_qkv.q_seq_lens, decoder_test_params=decphase_dec_test_params, encoder_test_params=enc_test_params, cross_test_params=decphase_cross_test_params, device=CUDA_DEVICE)
        decphase_dec_pckd_act_out = _run_decoder_self_attention_test(test_rsrcs, decphase_dec_test_params, decphase_attn_metadata)
        assert_actual_matches_ideal(decphase_dec_test_params, decphase_dec_pckd_act_out)
        decphase_cross_pckd_act_out = _run_encoder_decoder_cross_attention_test(test_rsrcs, decphase_dec_test_params, None, decphase_attn_metadata)
        assert_actual_matches_ideal(decphase_cross_test_params, decphase_cross_pckd_act_out)