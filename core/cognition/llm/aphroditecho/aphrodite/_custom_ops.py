import contextlib
from typing import TYPE_CHECKING, Optional, Union
import torch
from loguru import logger
import aphrodite.common.envs as envs
from aphrodite.platforms import current_platform
from aphrodite.scalar_type import ScalarType
if not current_platform.is_tpu() and (not current_platform.is_xpu()):
    try:
        import aphrodite._C
    except ImportError as e:
        logger.warning('Failed to import from aphrodite._C with {!r}', e)
supports_moe_ops = False
with contextlib.suppress(ImportError):
    import aphrodite._moe_C
    supports_moe_ops = True
if TYPE_CHECKING:
    def register_fake(fn):
        return lambda name: fn
else:
    try:
        from torch.library import register_fake
    except ImportError:
        from torch.library import impl_abstract as register_fake
def silu_and_mul(out: torch.Tensor, x: torch.Tensor) -> None:
    torch.ops._C.silu_and_mul(out, x)
def gelu_and_mul(out: torch.Tensor, x: torch.Tensor) -> None:
    torch.ops._C.gelu_and_mul(out, x)
def gelu_tanh_and_mul(out: torch.Tensor, x: torch.Tensor) -> None:
    torch.ops._C.gelu_tanh_and_mul(out, x)
def gelu_fast(out: torch.Tensor, x: torch.Tensor) -> None:
    torch.ops._C.gelu_fast(out, x)
def gelu_new(out: torch.Tensor, x: torch.Tensor) -> None:
    torch.ops._C.gelu_new(out, x)
def gelu_quick(out: torch.Tensor, x: torch.Tensor) -> None:
    torch.ops._C.gelu_quick(out, x)
def fatrelu_and_mul(out: torch.Tensor, x: torch.Tensor, threshold: float) -> None:
    torch.ops._C.fatrelu_and_mul(out, x, threshold)
def paged_attention_v1(out: torch.Tensor, query: torch.Tensor, key_cache: torch.Tensor, value_cache: torch.Tensor, num_kv_heads: int, scale: float, block_tables: torch.Tensor, seq_lens: torch.Tensor, block_size: int, max_seq_len: int, alibi_slopes: Optional[torch.Tensor], kv_cache_dtype: str, k_scale: torch.Tensor, v_scale: torch.Tensor, tp_rank: int=0, blocksparse_local_blocks: int=0, blocksparse_vert_stride: int=0, blocksparse_block_size: int=64, blocksparse_head_sliding_step: int=0) -> None:
    torch.ops._C.paged_attention_v1(out, query, key_cache, value_cache, num_kv_heads, scale, block_tables, seq_lens, block_size, max_seq_len, alibi_slopes, kv_cache_dtype, k_scale, v_scale, tp_rank, blocksparse_local_blocks, blocksparse_vert_stride, blocksparse_block_size, blocksparse_head_sliding_step)
def paged_attention_v2(out: torch.Tensor, exp_sum: torch.Tensor, max_logits: torch.Tensor, tmp_out: torch.Tensor, query: torch.Tensor, key_cache: torch.Tensor, value_cache: torch.Tensor, num_kv_heads: int, scale: float, block_tables: torch.Tensor, seq_lens: torch.Tensor, block_size: int, max_seq_len: int, alibi_slopes: Optional[torch.Tensor], kv_cache_dtype: str, k_scale: torch.Tensor, v_scale: torch.Tensor, tp_rank: int=0, blocksparse_local_blocks: int=0, blocksparse_vert_stride: int=0, blocksparse_block_size: int=64, blocksparse_head_sliding_step: int=0) -> None:
    torch.ops._C.paged_attention_v2(out, exp_sum, max_logits, tmp_out, query, key_cache, value_cache, num_kv_heads, scale, block_tables, seq_lens, block_size, max_seq_len, alibi_slopes, kv_cache_dtype, k_scale, v_scale, tp_rank, blocksparse_local_blocks, blocksparse_vert_stride, blocksparse_block_size, blocksparse_head_sliding_step)
def paged_attention_rocm(out: torch.Tensor, exp_sum: torch.Tensor, max_logits: torch.Tensor, tmp_out: torch.Tensor, query: torch.Tensor, key_cache: torch.Tensor, value_cache: torch.Tensor, num_kv_heads: int, scale: float, block_tables: torch.Tensor, seq_lens: torch.Tensor, query_start_loc: Optional[torch.Tensor], block_size: int, max_seq_len: int, alibi_slopes: Optional[torch.Tensor], kv_cache_dtype: str, k_scale: torch.Tensor, v_scale: torch.Tensor, fp8_out_scale: Optional[torch.Tensor]=None) -> None:
    torch.ops._rocm_C.paged_attention(out, exp_sum, max_logits, tmp_out, query, key_cache, value_cache, num_kv_heads, scale, block_tables, seq_lens, query_start_loc, block_size, max_seq_len, alibi_slopes, kv_cache_dtype, k_scale, v_scale, fp8_out_scale)
def mla_decode_kvcache_cpu(out: torch.Tensor, query: torch.Tensor, kv_cache: torch.Tensor, scale: float, block_tables: torch.Tensor, seq_lens: torch.Tensor) -> None:
    torch.ops._C_cpu.mla_decode_kvcache(out, query, kv_cache, scale, block_tables, seq_lens)
def merge_attn_states(output: torch.Tensor, prefix_output: torch.Tensor, prefix_lse: torch.Tensor, suffix_output: torch.Tensor, suffix_lse: torch.Tensor, output_lse: Optional[torch.Tensor]=None) -> None:
    torch.ops._C.merge_attn_states(output, output_lse, prefix_output, prefix_lse, suffix_output, suffix_lse)
def convert_vertical_slash_indexes(q_seqlens: torch.Tensor, kv_seqlens: torch.Tensor, vertical_indexes: torch.Tensor, slash_indexes: torch.Tensor, context_size: int, block_size_M: int, block_size_N: int, causal: bool=True) -> tuple[torch.Tensor, torch.Tensor, torch.Tensor, torch.Tensor]:
    batch_size = slash_indexes.size(0)
    num_heads = slash_indexes.size(1)
    nnz_slash = slash_indexes.size(2)
    nnz_vertical = vertical_indexes.size(2)
    num_rows = (context_size + block_size_M - 1) // block_size_M
    block_count = torch.zeros(batch_size, num_heads, num_rows, dtype=q_seqlens.dtype, device=q_seqlens.device)
    block_offset = torch.zeros(batch_size, num_heads, num_rows, nnz_slash, dtype=q_seqlens.dtype, device=q_seqlens.device)
    column_count = torch.zeros(batch_size, num_heads, num_rows, dtype=q_seqlens.dtype, device=q_seqlens.device)
    column_index = torch.zeros(batch_size, num_heads, num_rows, nnz_vertical, dtype=q_seqlens.dtype, device=q_seqlens.device)
    torch.ops._C.convert_vertical_slash_indexes(block_count, block_offset, column_count, column_index, q_seqlens, kv_seqlens, vertical_indexes, slash_indexes, context_size, block_size_M, block_size_N, causal)
    return (block_count, block_offset, column_count, column_index)
def convert_vertical_slash_indexes_mergehead(q_seqlens: torch.Tensor, kv_seqlens: torch.Tensor, vertical_indexes: torch.Tensor, slash_indexes: torch.Tensor, vertical_indices_count: torch.Tensor, slash_indices_count: torch.Tensor, context_size: int, block_size_M: int, block_size_N: int, causal: bool=True) -> tuple[torch.Tensor, torch.Tensor, torch.Tensor, torch.Tensor]:
    batch_size = slash_indexes.size(0)
    num_heads = slash_indexes.size(1)
    nnz_slash = slash_indexes.size(2)
    nnz_vertical = vertical_indexes.size(2)
    num_rows = (context_size + block_size_M - 1) // block_size_M
    block_count = torch.empty(batch_size, num_heads, num_rows, dtype=q_seqlens.dtype, device=q_seqlens.device)
    block_offset = torch.empty(batch_size, num_heads, num_rows, nnz_slash, dtype=q_seqlens.dtype, device=q_seqlens.device)
    column_count = torch.empty(batch_size, num_heads, num_rows, dtype=q_seqlens.dtype, device=q_seqlens.device)
    column_index = torch.empty(batch_size, num_heads, num_rows, nnz_vertical, dtype=q_seqlens.dtype, device=q_seqlens.device)
    torch.ops._C.convert_vertical_slash_indexes_mergehead(block_count, block_offset, column_count, column_index, q_seqlens, kv_seqlens, vertical_indexes, slash_indexes, vertical_indices_count, slash_indices_count, context_size, block_size_M, block_size_N, causal)
    return (block_count, block_offset, column_count, column_index)
def rotary_embedding(positions: torch.Tensor, query: torch.Tensor, key: Optional[torch.Tensor], head_size: int, cos_sin_cache: torch.Tensor, is_neox: bool) -> None:
    torch.ops._C.rotary_embedding(positions, query, key, head_size, cos_sin_cache, is_neox)
def batched_rotary_embedding(positions: torch.Tensor, query: torch.Tensor, key: Optional[torch.Tensor], head_size: int, cos_sin_cache: torch.Tensor, is_neox: bool, rot_dim: int, cos_sin_cache_offsets: torch.Tensor) -> None:
    torch.ops._C.batched_rotary_embedding(positions, query, key, head_size, cos_sin_cache, is_neox, rot_dim, cos_sin_cache_offsets)
def rms_norm(out: torch.Tensor, input: torch.Tensor, weight: torch.Tensor, epsilon: float) -> None:
    input_contiguous = input.contiguous()
    torch.ops._C.rms_norm(out, input_contiguous, weight, epsilon)
def fused_add_rms_norm(input: torch.Tensor, residual: torch.Tensor, weight: torch.Tensor, epsilon: float) -> None:
    torch.ops._C.fused_add_rms_norm(input, residual, weight, epsilon)
def apply_repetition_penalties_torch(logits: torch.Tensor, prompt_mask: torch.Tensor, output_mask: torch.Tensor, repetition_penalties: torch.Tensor) -> None:
    repetition_penalties = repetition_penalties.unsqueeze(dim=1).repeat(1, logits.size(1))
    penalties = torch.where(prompt_mask | output_mask, repetition_penalties, 1.0)
    scaling = torch.where(logits > 0, 1.0 / penalties, penalties)
    logits *= scaling
def apply_repetition_penalties_cuda(logits: torch.Tensor, prompt_mask: torch.Tensor, output_mask: torch.Tensor, repetition_penalties: torch.Tensor) -> None:
    torch.ops._C.apply_repetition_penalties_(logits, prompt_mask, output_mask, repetition_penalties)
def apply_repetition_penalties(logits: torch.Tensor, prompt_mask: torch.Tensor, output_mask: torch.Tensor, repetition_penalties: torch.Tensor) -> None:
    if current_platform.is_cuda() and logits.is_contiguous():
        apply_repetition_penalties_cuda(logits, prompt_mask, output_mask, repetition_penalties)
    else:
        apply_repetition_penalties_torch(logits, prompt_mask, output_mask, repetition_penalties)
def advance_step_flashattn(num_seqs: int, num_queries: int, block_size: int, input_tokens: torch.Tensor, sampled_token_ids: torch.Tensor, input_positions: torch.Tensor, seq_lens: torch.Tensor, slot_mapping: torch.Tensor, block_tables: torch.Tensor) -> None:
    return torch.ops._C.advance_step_flashattn(num_seqs, num_queries, block_size, input_tokens, sampled_token_ids, input_positions, seq_lens, slot_mapping, block_tables)
def advance_step_flashinfer(num_seqs: int, num_queries: int, block_size: int, input_tokens: torch.Tensor, sampled_token_ids: torch.Tensor, input_positions: torch.Tensor, seq_lens: torch.Tensor, slot_mapping: torch.Tensor, block_tables: torch.Tensor, paged_kv_indices: torch.Tensor, paged_kv_indptr: torch.Tensor, paged_kv_last_page_len: torch.Tensor, block_table_bound: torch.Tensor) -> None:
    return torch.ops._C.advance_step_flashinfer(num_seqs, num_queries, block_size, input_tokens, sampled_token_ids, input_positions, seq_lens, slot_mapping, block_tables, paged_kv_indices, paged_kv_indptr, paged_kv_last_page_len, block_table_bound)
def rms_norm_dynamic_per_token_quant(input: torch.Tensor, weight: torch.Tensor, epsilon: float, quant_dtype: torch.dtype, scale_ub: Optional[torch.Tensor]=None, residual: Optional[torch.Tensor]=None) -> tuple[torch.Tensor, torch.Tensor]:
    output = torch.empty_like(input, dtype=quant_dtype)
    scales = torch.empty((input.numel() // input.shape[-1], 1), device=input.device, dtype=torch.float32)
    torch.ops._C.rms_norm_dynamic_per_token_quant(output, input, weight, scales, epsilon, scale_ub, residual)
    return (output, scales)
def awq_dequantize(qweight: torch.Tensor, scales: torch.Tensor, zeros: torch.Tensor, split_k_iters: int, thx: int, thy: int) -> torch.Tensor:
    if envs.APHRODITE_USE_TRITON_AWQ:
        from aphrodite.quantization.awq_triton import awq_dequantize_triton
        return awq_dequantize_triton(qweight, scales, zeros)
    return torch.ops._C.awq_dequantize(qweight, scales, zeros, split_k_iters, thx, thy)
def awq_gemm(input: torch.Tensor, qweight: torch.Tensor, qzeros: torch.Tensor, scales: torch.Tensor, split_k_iters: int) -> torch.Tensor:
    if envs.APHRODITE_USE_TRITON_AWQ:
        from aphrodite.quantization.awq_triton import awq_gemm_triton
        return awq_gemm_triton(input, qweight, qzeros, scales, split_k_iters)
    return torch.ops._C.awq_gemm(input, qweight, qzeros, scales, split_k_iters)
def gptq_gemm(a: torch.Tensor, b_q_weight: torch.Tensor, b_gptq_qzeros: torch.Tensor, b_gptq_scales: torch.Tensor, b_g_idx: torch.Tensor, use_exllama: bool, bit: int) -> torch.Tensor:
    return torch.ops._C.gptq_gemm(a, b_q_weight, b_gptq_qzeros, b_gptq_scales, b_g_idx, use_exllama, bit)
if hasattr(torch.ops._C, 'gptq_gemm'):
    @register_fake('_C::gptq_gemm')
    def _gptq_gemm_fake(a: torch.Tensor, b_q_weight: torch.Tensor, b_gptq_qzeros: torch.Tensor, b_gptq_scales: torch.Tensor, b_g_idx: torch.Tensor, use_exllama: bool, bit: int) -> torch.Tensor:
        return torch.empty((a.size(0), b_q_weight.size(1)), dtype=a.dtype, device=a.device)
def gptq_shuffle(q_weight: torch.Tensor, q_perm: torch.Tensor, bit: int) -> None:
    torch.ops._C.gptq_shuffle(q_weight, q_perm, bit)
def squeezellm_gemm(vec: torch.Tensor, mat: torch.Tensor, mul: torch.Tensor, lookup_table: torch.Tensor) -> None:
    torch.ops._C.squeezellm_gemm(vec, mat, mul, lookup_table)
def marlin_gemm(a: torch.Tensor, b_q_weight: torch.Tensor, b_scales: torch.Tensor, workspace: torch.Tensor, size_m: int, size_n: int, size_k: int) -> torch.Tensor:
    return torch.ops._C.marlin_gemm(a, b_q_weight, b_scales, workspace, size_m, size_n, size_k)
def gptq_marlin_24_gemm(a: torch.Tensor, b_q_weight: torch.Tensor, b_meta: torch.Tensor, b_scales: torch.Tensor, workspace: torch.Tensor, b_q_type: ScalarType, size_m: int, size_n: int, size_k: int) -> torch.Tensor:
    return torch.ops._C.gptq_marlin_24_gemm(a, b_q_weight, b_meta, b_scales, workspace, b_q_type.id, size_m, size_n, size_k)
if hasattr(torch.ops._C, 'gptq_marlin_24_gemm'):
    @register_fake('_C::gptq_marlin_24_gemm')
    def _gptq_marlin_24_gemm_fake(a: torch.Tensor, b_q_weight: torch.Tensor, b_meta: torch.Tensor, b_scales: torch.Tensor, workspace: torch.Tensor, b_q_type: ScalarType, size_m: torch.SymInt, size_n: torch.SymInt, size_k: torch.SymInt) -> torch.Tensor:
        return torch.empty((size_m, size_n), device=a.device, dtype=a.dtype)
    @register_fake('_C::gptq_marlin_gemm')
    def _gptq_marlin_gemm_fake(a: torch.Tensor, c: Optional[torch.Tensor], b_q_weight: torch.Tensor, b_scales: torch.Tensor, global_scale: Optional[torch.Tensor], b_zeros: Optional[torch.Tensor], g_idx: Optional[torch.Tensor], perm: Optional[torch.Tensor], workspace: torch.Tensor, b_q_type_id: int, size_m: torch.SymInt, size_n: torch.SymInt, size_k: torch.SymInt, is_k_full: bool=True, use_atomic_add: bool=False, use_fp32_reduce: bool=False, is_zp_float: bool=False) -> torch.Tensor:
        return torch.empty((size_m, size_n), device=a.device, dtype=a.dtype)
    @register_fake('_C::marlin_qqq_gemm')
    def _marlin_qqq_gemm_fake(a: torch.Tensor, b_q_weight: torch.Tensor, s_tok: torch.Tensor, s_ch: torch.Tensor, s_group: torch.Tensor, workspace: torch.Tensor, size_m: torch.SymInt, size_n: torch.SymInt, size_k: torch.SymInt) -> torch.Tensor:
        return torch.empty((size_m, size_n), dtype=torch.float16, device=a.device)
    @register_fake('_C::marlin_gemm')
    def _marlin_gemm_fake(a: torch.Tensor, b_q_weight: torch.Tensor, b_scales: torch.Tensor, workspace: torch.Tensor, size_m: torch.SymInt, size_n: torch.SymInt, size_k: torch.SymInt) -> torch.Tensor:
        return torch.empty((size_m, size_n), dtype=torch.float16, device=a.device)
    @register_fake('_C::awq_dequantize')
    def _awq_dequantize_fake(qweight: torch.Tensor, scales: torch.Tensor, zeros: torch.Tensor, split_k_iters: torch.SymInt, thx: int, thy: int) -> torch.Tensor:
        in_c = qweight.size(0)
        qout_c = qweight.size(1)
        out_c = qout_c * 8
        return torch.empty((in_c, out_c), dtype=scales.dtype, device=scales.device)
    @register_fake('_C::awq_gemm')
    def _awq_gemm_fake(input: torch.Tensor, qweight: torch.Tensor, qzeros: torch.Tensor, scales: torch.Tensor, split_k_iters: torch.SymInt) -> torch.Tensor:
        num_in_feats = input.size(0)
        return torch.empty((split_k_iters, num_in_feats, qweight.size(1) * 8), dtype=input.dtype, device=input.device).sum(0)
    @register_fake('_C::aqlm_gemm')
    def _aqlm_gemm_fake(input: torch.Tensor, codes: torch.Tensor, codebooks: torch.Tensor, scales: torch.Tensor, codebook_partition_sizes: list[int], bias: Optional[torch.Tensor]) -> torch.Tensor:
        out_features = codes.size(0) * codebooks.size(2)
        flat_input = input.reshape((-1, input.size(-1)))
        flat_output = torch.empty((flat_input.size(0), out_features), dtype=input.dtype, device=input.device)
        output_sizes = list(input.shape)
        output_sizes.pop()
        output_sizes.append(-1)
        return flat_output.reshape(tuple(output_sizes))
    @register_fake('_C::aqlm_dequant')
    def _aqlm_dequant_fake(codes: torch.Tensor, codebooks: torch.Tensor, codebook_partition_sizes: list[int]) -> torch.Tensor:
        in_features = codes.size(1) * 8
        out_features = codes.size(0)
        return torch.empty((out_features, in_features), dtype=codebooks.dtype, device=codebooks.device)
    @register_fake('_C::machete_mm')
    def machete_mm_fake(a: torch.Tensor, b_q: torch.Tensor, b_type: ScalarType, out_type: Optional[torch.dtype]=None, b_group_scales: Optional[torch.Tensor]=None, b_group_zeros: Optional[torch.Tensor]=None, b_group_size: Optional[int]=None, b_channel_scales: Optional[torch.Tensor]=None, a_token_scales: Optional[torch.Tensor]=None, schedule: Optional[str]=None) -> torch.Tensor:
        m = a.size(0)
        n = b_q.size(1)
        return torch.empty((m, n), device=a.device, dtype=a.dtype)
    @register_fake('_C::machete_prepack_B')
    def machete_prepack_B_fake(b_q_weight: torch.Tensor, a_type: torch.dtype, b_type: ScalarType, group_scales_type: Optional[torch.dtype]) -> torch.Tensor:
        return torch.empty_like(b_q_weight, memory_format=torch.contiguous_format)
if hasattr(torch.ops._C, 'allspark_w8a16_gemm'):
    @register_fake('_C::allspark_w8a16_gemm')
    def _allspark_w8a16_gemm_fake(a: torch.Tensor, b_qweight: torch.Tensor, b_scales: torch.Tensor, b_qzeros: Optional[torch.Tensor], n: torch.SymInt, group_size: torch.SymInt, sm_count: torch.SymInt, sm_version: torch.SymInt, CUBLAS_M_THRESHOLD: torch.SymInt, has_zp: bool, n32k16_reorder: bool) -> torch.Tensor:
        m = a.size(0)
        return torch.empty((m, n), device=a.device, dtype=a.dtype)
if hasattr(torch.ops._C, 'ggml_dequantize'):
    @register_fake('_C::ggml_dequantize')
    def _ggml_dequantize_fake(W: torch.Tensor, quant_type: int, m: torch.SymInt, n: torch.SymInt, dtype: Optional[torch.dtype]=None) -> torch.Tensor:
        return torch.empty((m, n), dtype=torch.float16, device=W.device)
    @register_fake('_C::ggml_mul_mat_vec_a8')
    def _ggml_mul_mat_vec_a8_fake(W: torch.Tensor, X: torch.Tensor, quant_type: int, row: torch.SymInt) -> torch.Tensor:
        return torch.empty((X.shape[0], row), dtype=X.dtype, device=W.device)
    @register_fake('_C::ggml_mul_mat_a8')
    def _ggml_mul_mat_a8_fake(W: torch.Tensor, X: torch.Tensor, quant_type: int, row: torch.SymInt) -> torch.Tensor:
        batch = X.size(0)
        return torch.empty((batch, row), dtype=X.dtype, device=W.device)
    @register_fake('_C::ggml_moe_a8')
    def _ggml_moe_a8_fake(X: torch.Tensor, W: torch.Tensor, sorted_token_ids: torch.Tensor, expert_ids: torch.Tensor, num_tokens_post_padded: torch.Tensor, quant_type: int, row: torch.SymInt, top_k: torch.SymInt, tokens: torch.SymInt) -> torch.Tensor:
        tokens = X.size(0)
        return torch.empty((tokens * top_k, row), dtype=torch.float16, device=W.device)
if hasattr(torch.ops._C, 'ggml_moe_a8_vec'):
    @register_fake('_C::ggml_moe_a8_vec')
    def _ggml_moe_a8_vec_fake(X: torch.Tensor, W: torch.Tensor, topk_ids: torch.Tensor, top_k: int, quant_type: int, row: torch.SymInt, tokens: torch.SymInt) -> torch.Tensor:
        tokens = X.size(0)
        return torch.empty((tokens * top_k, row), dtype=X.dtype, device=W.device)
if hasattr(torch.ops._C, 'vptq_gemm'):
    @register_fake('_C::vptq_gemm')
    def _vptq_gemm_fake(input: torch.Tensor, indices: torch.Tensor, codebooks: torch.Tensor, weight_scale: torch.Tensor, weight_bias: torch.Tensor, g_i_o: list[int], res: torch.Tensor, res_codebooks: torch.Tensor, oi: torch.Tensor, oc: torch.Tensor, invperm: torch.Tensor, bias: torch.Tensor) -> torch.Tensor:
        out_features = g_i_o[2]
        flat_input = input.reshape((-1, input.size(-1)))
        flat_output = torch.empty((flat_input.size(0), out_features), dtype=input.dtype, device=input.device)
        output_sizes = list(input.shape)
        output_sizes.pop()
        output_sizes.append(-1)
        return flat_output.reshape(tuple(output_sizes))
    @register_fake('_C::vptq_dequant')
    def _vptq_dequant_fake(indices: torch.Tensor, codebooks: torch.Tensor, weight_scale: torch.Tensor, weight_bias: torch.Tensor, g_i_o: list[int], res: torch.Tensor, res_codebooks: torch.Tensor, oi: torch.Tensor, oc: torch.Tensor, invperm: torch.Tensor) -> torch.Tensor:
        in_features = g_i_o[1]
        out_features = g_i_o[2]
        return torch.empty((out_features, in_features), dtype=codebooks.dtype, device=codebooks.device)
def cutlass_scaled_mm_supports_fp4(cuda_device_capability: int) -> bool:
    return torch.ops._C.cutlass_scaled_mm_supports_fp4(cuda_device_capability)
def cutlass_blockwise_scaled_grouped_mm(output: torch.Tensor, a: torch.Tensor, b: torch.Tensor, scales_a: torch.Tensor, scales_b: torch.Tensor, problem_sizes: torch.Tensor, expert_offsets: torch.Tensor):
    torch.ops._C.cutlass_blockwise_scaled_grouped_mm(output, a, b, scales_a, scales_b, problem_sizes, expert_offsets)
def cutlass_scaled_fp4_mm(a: torch.Tensor, b: torch.Tensor, block_scale_a: torch.Tensor, block_scale_b: torch.Tensor, alpha: torch.Tensor, out_dtype: torch.dtype) -> torch.Tensor:
    assert a.ndim == 2 and b.ndim == 2
    m, n = (a.shape[0], b.shape[0])
    out = torch.empty((m, n), dtype=out_dtype, device=a.device)
    torch.ops._C.cutlass_scaled_fp4_mm(out, a, b, block_scale_a, block_scale_b, alpha)
    return out
def cutlass_scaled_mm_supports_fp8(cuda_device_capability: int) -> bool:
    return torch.ops._C.cutlass_scaled_mm_supports_fp8(cuda_device_capability)
def cutlass_scaled_mm_supports_block_fp8(cuda_device_capability: int) -> bool:
    return torch.ops._C.cutlass_scaled_mm_supports_block_fp8(cuda_device_capability)
def cutlass_scaled_mm(a: torch.Tensor, b: torch.Tensor, scale_a: torch.Tensor, scale_b: torch.Tensor, out_dtype: torch.dtype, bias: Optional[torch.Tensor]=None) -> torch.Tensor:
    assert out_dtype is torch.bfloat16 or out_dtype is torch.float16
    assert bias is None or (bias.numel() == b.shape[1] and bias.dtype == out_dtype)
    target_shape = (*a.shape[:-1], b.shape[1])
    a = a.view(-1, a.shape[-1])
    cutlass_compatible_b = b.shape[0] % 16 == 0 and b.shape[1] % 16 == 0
    if current_platform.is_rocm() or not cutlass_compatible_b:
        from aphrodite.quantization.compressed_tensors.triton_scaled_mm import triton_scaled_mm
        out = triton_scaled_mm(a, b, scale_a, scale_b, out_dtype, bias)
    else:
        out = torch.empty((a.shape[0], b.shape[1]), dtype=out_dtype, device=a.device)
        torch.ops._C.cutlass_scaled_mm(out, a, b, scale_a, scale_b, bias)
    return out.view(*target_shape)
def cutlass_scaled_mm_azp(a: torch.Tensor, b: torch.Tensor, scale_a: torch.Tensor, scale_b: torch.Tensor, out_dtype: torch.dtype, azp_adj: torch.Tensor, azp: Optional[torch.Tensor]=None, bias: Optional[torch.Tensor]=None) -> torch.Tensor:
    assert b.shape[0] % 16 == 0 and b.shape[1] % 16 == 0
    assert out_dtype is torch.bfloat16 or out_dtype is torch.float16
    assert bias is None or (bias.numel() == b.shape[1] and bias.dtype == out_dtype)
    target_shape = (*a.shape[:-1], b.shape[1])
    a = a.view(-1, a.shape[-1])
    assert azp is None or azp.numel() == a.shape[0]
    out = torch.empty((a.shape[0], b.shape[1]), dtype=out_dtype, device=a.device)
    torch.ops._C.cutlass_scaled_mm_azp(out, a, b, scale_a, scale_b, azp_adj, azp, bias)
    return out.view(*target_shape)
def cutlass_sparse_scaled_mm_supported(cuda_device_capability: int) -> bool:
    return torch.ops._C.cutlass_sparse_scaled_mm_supported(cuda_device_capability)
def cutlass_group_gemm_supported(cuda_device_capability: int) -> bool:
    return torch.ops._C.cutlass_group_gemm_supported(cuda_device_capability)
def cutlass_sparse_compress(a: torch.Tensor) -> tuple[torch.Tensor, torch.Tensor]:
    assert a.dtype in [torch.int8, torch.float8_e4m3fn, torch.bfloat16, torch.float16]
    assert a.is_contiguous()
    elemsPerMetaElem = 4
    assert a.shape[1] % (2 * elemsPerMetaElem) == 0
    return torch.ops._C.cutlass_sparse_compress(a)
def cutlass_scaled_sparse_mm(a: torch.Tensor, bt_nzs: torch.Tensor, bt_meta: torch.Tensor, scale_a: torch.Tensor, scale_b: torch.Tensor, out_dtype: torch.dtype, bias: Optional[torch.Tensor]=None) -> torch.Tensor:
    assert bt_nzs.shape[0] % 16 == 0 and bt_nzs.shape[1] % 16 == 0
    assert out_dtype is torch.bfloat16 or out_dtype is torch.float16
    assert bias is None or (bias.shape[0] == bt_nzs.shape[0] and bias.dtype == out_dtype)
    m = a.shape[0]
    n = bt_nzs.shape[0]
    out = torch.empty((m, n), dtype=out_dtype, device=a.device)
    torch.ops._C.cutlass_scaled_sparse_mm(out, a, bt_nzs, bt_meta, scale_a, scale_b, bias)
    return out
def get_cutlass_moe_mm_data(topk_ids: torch.Tensor, expert_offsets: torch.Tensor, problem_sizes1: torch.Tensor, problem_sizes2: torch.Tensor, input_permutation: torch.Tensor, output_permutation: torch.Tensor, num_experts: int, n: int, k: int, blockscale_offsets: Optional[torch.Tensor]=None):
    return torch.ops._C.get_cutlass_moe_mm_data(topk_ids, expert_offsets, problem_sizes1, problem_sizes2, input_permutation, output_permutation, num_experts, n, k, blockscale_offsets)
def shuffle_rows(input_tensor: torch.Tensor, dst2src_map: torch.Tensor):
    num_tokens_permuted = dst2src_map.shape[0]
    output_tensor = torch.empty((num_tokens_permuted, input_tensor.shape[1]), device=input_tensor.device, dtype=input_tensor.dtype)
    torch.ops._moe_C.shuffle_rows(input_tensor, dst2src_map, output_tensor)
    return output_tensor
def get_cutlass_pplx_moe_mm_data(expert_offsets: torch.Tensor, problem_sizes1: torch.Tensor, problem_sizes2: torch.Tensor, expert_num_tokens: torch.Tensor, num_local_experts: int, padded_m: int, n: int, k: int):
    return torch.ops._C.get_cutlass_pplx_moe_mm_data(expert_offsets, problem_sizes1, problem_sizes2, expert_num_tokens, num_local_experts, padded_m, n, k)
def cutlass_moe_mm(out_tensors: torch.Tensor, a_tensors: torch.Tensor, b_tensors: torch.Tensor, a_scales: torch.Tensor, b_scales: torch.Tensor, expert_offsets: torch.Tensor, problem_sizes: torch.Tensor, a_strides: torch.Tensor, b_strides: torch.Tensor, c_strides: torch.Tensor, per_act_token: bool, per_out_ch: bool):
    return torch.ops._C.cutlass_moe_mm(out_tensors, a_tensors, b_tensors, a_scales, b_scales, expert_offsets, problem_sizes, a_strides, b_strides, c_strides, per_act_token, per_out_ch)
def cutlass_fp4_moe_mm(out_tensors: torch.Tensor, a_tensors: torch.Tensor, b_tensors: torch.Tensor, a_scales: torch.Tensor, b_scales: torch.Tensor, alphas: torch.Tensor, problem_sizes: torch.Tensor, expert_offsets: torch.Tensor, sf_offsets: torch.Tensor):
    return torch.ops._C.cutlass_fp4_group_mm(out_tensors, a_tensors, b_tensors, a_scales, b_scales, alphas, problem_sizes, expert_offsets, sf_offsets)
def aqlm_gemm(input: torch.Tensor, codes: torch.Tensor, codebooks: torch.Tensor, scales: torch.Tensor, codebook_partition_sizes: list[int], bias: Optional[torch.Tensor]) -> torch.Tensor:
    return torch.ops._C.aqlm_gemm(input, codes, codebooks, scales, codebook_partition_sizes, bias)
def aqlm_dequant(codes: torch.Tensor, codebooks: torch.Tensor, codebook_partition_sizes: list[int]) -> torch.Tensor:
    return torch.ops._C.aqlm_dequant(codes, codebooks, codebook_partition_sizes)
def vptq_gemm(input: torch.Tensor, indices: torch.Tensor, codebooks: torch.Tensor, weight_scale: torch.Tensor, weight_bias: torch.Tensor, g_i_o: list[int], res: torch.Tensor, res_codebooks: torch.Tensor, oi: torch.Tensor, oc: torch.Tensor, invperm: torch.Tensor, bias: torch.Tensor) -> torch.Tensor:
    return torch.ops._C.vptq_gemm(input, indices, codebooks, weight_scale, weight_bias, g_i_o, res, res_codebooks, oi, oc, invperm, bias)
def vptq_dequant(indices: torch.Tensor, codebooks: torch.Tensor, weight_scale: torch.Tensor, weight_bias: torch.Tensor, g_i_o: list[int], res: torch.Tensor, res_codebooks: torch.Tensor, oi: torch.Tensor, oc: torch.Tensor, invperm: torch.Tensor) -> torch.Tensor:
    return torch.ops._C.vptq_dequant(indices, codebooks, weight_scale, weight_bias, g_i_o, res, res_codebooks, oi, oc, invperm)
def gptq_marlin_repack(b_q_weight: torch.Tensor, perm: torch.Tensor, size_k: int, size_n: int, num_bits: int) -> torch.Tensor:
    return torch.ops._C.gptq_marlin_repack(b_q_weight, perm, size_k, size_n, num_bits)
def awq_marlin_repack(b_q_weight: torch.Tensor, size_k: int, size_n: int, num_bits: int) -> torch.Tensor:
    return torch.ops._C.awq_marlin_repack(b_q_weight, size_k, size_n, num_bits)
def gptq_marlin_moe_repack(b_q_weight: torch.Tensor, perm: torch.Tensor, size_k: int, size_n: int, num_bits: int) -> torch.Tensor:
    num_experts = b_q_weight.shape[0]
    assert size_k % 16 == 0
    output = torch.empty((num_experts, size_k // 16, size_n * (num_bits // 2)), device=b_q_weight.device, dtype=b_q_weight.dtype)
    for e in range(num_experts):
        output[e] = torch.ops._C.gptq_marlin_repack(b_q_weight[e], perm[e], size_k, size_n, num_bits)
    return output
def awq_marlin_moe_repack(b_q_weight: torch.Tensor, perm: torch.Tensor, size_k: int, size_n: int, num_bits: int) -> torch.Tensor:
    num_experts = b_q_weight.shape[0]
    assert size_k % 16 == 0
    output = torch.empty((num_experts, size_k // 16, size_n * (num_bits // 2)), device=b_q_weight.device, dtype=b_q_weight.dtype)
    for e in range(num_experts):
        output[e] = torch.ops._C.awq_marlin_repack(b_q_weight[e], size_k, size_n, num_bits)
    return output
def gptq_marlin_gemm(a: torch.Tensor, c: Optional[torch.Tensor], b_q_weight: torch.Tensor, b_scales: torch.Tensor, global_scale: Optional[torch.Tensor], b_zeros: Optional[torch.Tensor], g_idx: Optional[torch.Tensor], perm: Optional[torch.Tensor], workspace: torch.Tensor, b_q_type: ScalarType, size_m: int, size_n: int, size_k: int, is_k_full: bool=True, use_atomic_add: bool=False, use_fp32_reduce: bool=False, is_zp_float: bool=False) -> torch.Tensor:
    return torch.ops._C.gptq_marlin_gemm(a, c, b_q_weight, b_scales, global_scale, b_zeros, g_idx, perm, workspace, b_q_type.id, size_m, size_n, size_k, is_k_full, use_atomic_add, use_fp32_reduce, is_zp_float)
def machete_supported_schedules(a_type: torch.dtype, b_type: ScalarType, group_scales_type: Optional[torch.dtype], group_zeros_type: Optional[torch.dtype]=None, channel_scales_type: Optional[torch.dtype]=None, token_scales_type: Optional[torch.dtype]=None, out_type: Optional[torch.dtype]=None) -> list[str]:
    return torch.ops._C.machete_supported_schedules(a_type, b_type.id, group_scales_type, group_zeros_type, channel_scales_type, token_scales_type, out_type)
def machete_mm(a: torch.Tensor, b_q: torch.Tensor, b_type: ScalarType, out_type: Optional[torch.dtype]=None, b_group_scales: Optional[torch.Tensor]=None, b_group_zeros: Optional[torch.Tensor]=None, b_group_size: Optional[int]=None, b_channel_scales: Optional[torch.Tensor]=None, a_token_scales: Optional[torch.Tensor]=None, schedule: Optional[str]=None) -> torch.Tensor:
    return torch.ops._C.machete_mm(a, b_q, b_type.id, out_type, b_group_scales, b_group_zeros, b_group_size, b_channel_scales, a_token_scales, schedule)
def machete_prepack_B(b_q_weight: torch.Tensor, a_type: torch.dtype, b_type: ScalarType, group_scales_type: Optional[torch.dtype]) -> torch.Tensor:
    return torch.ops._C.machete_prepack_B(b_q_weight, a_type, b_type.id, group_scales_type)
if hasattr(torch.ops._C, 'permute_cols'):
    @register_fake('_C::permute_cols')
    def _permute_cols_fake(a: torch.Tensor, perm: torch.Tensor) -> torch.Tensor:
        return torch.empty_like(a)
def permute_cols(a: torch.Tensor, perm: torch.Tensor) -> torch.Tensor:
    return torch.ops._C.permute_cols(a, perm)
def scaled_fp4_quant(input: torch.Tensor, input_global_scale: torch.Tensor) -> tuple[torch.Tensor, torch.Tensor]:
    assert not current_platform.is_rocm()
    assert input.ndim >= 1, f'input.ndim needs to be >= 1, but got {input.ndim}.'
    other_dims = 1 if input.ndim == 1 else -1
    input = input.reshape(other_dims, input.shape[-1])
    m, n = input.shape
    block_size = 16
    device = input.device
    assert n % block_size == 0, f'last dim has to be multiple of 16, but got {n}.'
    assert input.dtype in (torch.float16, torch.bfloat16), f'input.dtype needs to be fp16 or bf16 but got {input.dtype}.'
    output = torch.empty((m, n // 2), device=device, dtype=torch.uint8)
    round_up = lambda x, y: (x + y - 1) // y * y
    rounded_m = round_up(m, 128)
    scale_n = n // block_size
    rounded_n = round_up(scale_n, 4)
    output_scale = torch.empty((rounded_m, rounded_n // 4), device=device, dtype=torch.int32)
    torch.ops._C.scaled_fp4_quant(output, input, output_scale, input_global_scale)
    output_scale = output_scale.view(torch.float8_e4m3fn)
    return (output, output_scale)
def scaled_fp4_experts_quant(input_tensor: torch.Tensor, input_global_scale: torch.Tensor, expert_offsets: torch.Tensor, blockscale_offsets: torch.Tensor, topk: int) -> tuple[torch.Tensor, torch.Tensor]:
    assert not current_platform.is_rocm()
    assert input_tensor.ndim == 2, f'input.ndim needs to be == 2, but got {input_tensor.ndim}.'
    MAX_TOKENS_PER_EXPERT = envs.APHRODITE_MAX_TOKENS_PER_EXPERT_FP4_MOE
    m_numtopk, k = input_tensor.shape
    assert m_numtopk <= MAX_TOKENS_PER_EXPERT * topk, f'm_numtopk must be less than MAX_TOKENS_PER_EXPERT({MAX_TOKENS_PER_EXPERT}) for cutlass_moe_fp4, observed m_numtopk = {m_numtopk}. Use APHRODITE_MAX_TOKENS_PER_EXPERT_FP4_MOE to set this value.'
    scales_k = k // 16
    padded_k = (scales_k + (4 - 1)) // 4
    output = torch.empty(m_numtopk, k // 2, device=input_tensor.device, dtype=torch.uint8)
    output_scales = torch.empty(MAX_TOKENS_PER_EXPERT * topk, padded_k, dtype=torch.int32, device=input_tensor.device)
    torch.ops._C.scaled_fp4_experts_quant(output, output_scales, input_tensor, input_global_scale, expert_offsets, blockscale_offsets)
    output_scales = output_scales.view(torch.float8_e4m3fn)
    return (output, output_scales)
def scaled_fp8_quant(input: torch.Tensor, scale: Optional[torch.Tensor]=None, num_token_padding: Optional[int]=None, scale_ub: Optional[torch.Tensor]=None, use_per_token_if_dynamic: bool=False, output: Optional[torch.Tensor]=None) -> tuple[torch.Tensor, torch.Tensor]:
    assert input.ndim == 2
    shape: Union[tuple[int, int], torch.Size] = input.shape
    out_dtype: torch.dtype = current_platform.fp8_dtype()
    if num_token_padding:
        shape = (max(num_token_padding, input.shape[0]), shape[1])
    if output is None:
        output = torch.empty(shape, device=input.device, dtype=out_dtype)
    else:
        assert num_token_padding is None, 'padding not supported if output passed in'
        assert output.dtype == out_dtype
    if scale is None:
        if use_per_token_if_dynamic:
            scale = torch.empty((shape[0], 1), device=input.device, dtype=torch.float32)
            torch.ops._C.dynamic_per_token_scaled_fp8_quant(output, input, scale, scale_ub)
        else:
            scale = torch.empty(1, device=input.device, dtype=torch.float32)
            torch.ops._C.dynamic_scaled_fp8_quant(output, input, scale)
    else:
        assert scale.numel() == 1, f'{scale.shape}'
        torch.ops._C.static_scaled_fp8_quant(output, input, scale)
    return (output, scale)
def allspark_repack_weight(qweight: torch.Tensor, scale: torch.Tensor, zero_point: Optional[torch.Tensor]=None, has_zp: bool=False) -> tuple[torch.Tensor, torch.Tensor, torch.Tensor]:
    K = qweight.shape[0]
    N = qweight.shape[1]
    N_32align = (N + 32 - 1) // 32 * 32
    qweight_reorder = torch.empty((N_32align, K), device=qweight.device, dtype=qweight.dtype)
    scale_reorder = torch.empty((1, N_32align), device=scale.device, dtype=scale.dtype)
    zero_point_reorder = None
    if has_zp:
        assert zero_point is not None, 'zero_point must be provided for asymmetric quantization.'
        zero_point_reorder = torch.empty((1, N_32align), device=zero_point.device, dtype=zero_point.dtype)
    torch.ops._C.rearrange_kn_weight_as_n32k16_order(qweight, scale, zero_point, has_zp, qweight_reorder, scale_reorder, zero_point_reorder, K, N, N_32align)
    return (qweight_reorder, scale_reorder, zero_point_reorder)
def allspark_w8a16_gemm(a: torch.Tensor, b_qweight: torch.Tensor, b_scales: torch.Tensor, b_qzeros: Optional[torch.Tensor], n: int, group_size: int, sm_count: int, sm_version: int, CUBLAS_M_THRESHOLD: int, has_zp: bool, n32k16_reorder: bool) -> torch.Tensor:
    return torch.ops._C.allspark_w8a16_gemm(a, b_qweight, b_scales, b_qzeros, n, group_size, sm_count, sm_version, CUBLAS_M_THRESHOLD, has_zp, n32k16_reorder)
def scaled_int8_quant(input: torch.Tensor, scale: Optional[torch.Tensor]=None, azp: Optional[torch.Tensor]=None, symmetric: bool=True) -> tuple[torch.Tensor, torch.Tensor, Optional[torch.Tensor]]:
    output = torch.empty_like(input, dtype=torch.int8)
    if scale is not None:
        assert symmetric == (azp is None), 'azp must only be provided for asymmetric quantization.'
        torch.ops._C.static_scaled_int8_quant(output, input, scale, azp)
        return (output, scale, azp)
    input_scales = torch.empty((input.numel() // input.shape[-1], 1), device=input.device, dtype=torch.float32)
    input_azp = None if symmetric else torch.empty_like(input_scales, dtype=torch.int32)
    torch.ops._C.dynamic_scaled_int8_quant(output, input.contiguous(), input_scales, input_azp)
    return (output, input_scales, input_azp)
def marlin_qqq_gemm(a: torch.Tensor, b_q_weight: torch.Tensor, s_tok: torch.Tensor, s_ch: torch.Tensor, s_group: torch.Tensor, workspace: torch.Tensor, size_m: int, size_n: int, size_k: int) -> torch.Tensor:
    return torch.ops._C.marlin_qqq_gemm(a, b_q_weight, s_tok, s_ch, s_group, workspace, size_m, size_n, size_k)
def ggml_dequantize(W: torch.Tensor, quant_type: int, m: int, n: int, dtype: Optional[torch.dtype]) -> torch.Tensor:
    return torch.ops._C.ggml_dequantize(W, quant_type, m, n, dtype)
def ggml_mul_mat_vec_a8(W: torch.Tensor, X: torch.Tensor, quant_type: int, row: int) -> torch.Tensor:
    return torch.ops._C.ggml_mul_mat_vec_a8(W, X, quant_type, row)
def ggml_mul_mat_a8(W: torch.Tensor, X: torch.Tensor, quant_type: int, row: int) -> torch.Tensor:
    return torch.ops._C.ggml_mul_mat_a8(W, X, quant_type, row)
def ggml_moe_a8(X: torch.Tensor, W: torch.Tensor, sorted_token_ids: torch.Tensor, expert_ids: torch.Tensor, num_tokens_post_padded: torch.Tensor, quant_type: int, row: int, top_k: int, tokens: int) -> torch.Tensor:
    return torch.ops._C.ggml_moe_a8(X, W, sorted_token_ids, expert_ids, num_tokens_post_padded, quant_type, row, top_k, tokens)
def ggml_moe_a8_vec(X: torch.Tensor, W: torch.Tensor, topk_ids: torch.Tensor, top_k: int, quant_type: int, row: torch.SymInt, tokens: torch.SymInt) -> torch.Tensor:
    return torch.ops._C.ggml_moe_a8_vec(X, W, topk_ids, top_k, quant_type, row, tokens)
def ggml_moe_get_block_size(quant_type: int) -> int:
    return torch.ops._C.ggml_moe_get_block_size(quant_type)
def fp_eXmY_linear_forward_cuda(exponent_bits: int, mantissa_bits: int, x: torch.Tensor, weights: torch.Tensor, scales: torch.Tensor, splitK: int) -> torch.Tensor:
    return torch.ops._C.fp_eXmY_linear_forward_cuda(exponent_bits, mantissa_bits, x, weights, scales, splitK)
def selective_scan_fwd(u: torch.Tensor, delta: torch.Tensor, A: torch.Tensor, B: torch.Tensor, C: torch.Tensor, D_: Optional[torch.Tensor], z_: Optional[torch.Tensor], delta_bias_: Optional[torch.Tensor], delta_softplus: bool, query_start_loc: Optional[torch.Tensor], cache_indices: Optional[torch.Tensor], has_initial_state: Optional[torch.Tensor], ssm_states: torch.Tensor, pad_slot_id: int):
    torch.ops._C.selective_scan_fwd(u, delta, A, B, C, D_, z_, delta_bias_, delta_softplus, query_start_loc, cache_indices, has_initial_state, ssm_states, pad_slot_id)
def LLMM1(a: torch.Tensor, b: torch.Tensor, rows_per_block: int) -> torch.Tensor:
    return torch.ops._rocm_C.LLMM1(a, b, rows_per_block)
def wvSplitK(a: torch.Tensor, b: torch.Tensor, cu_count: int) -> torch.Tensor:
    return torch.ops._rocm_C.wvSplitK(a, b, cu_count)
def wvSplitKQ(a: torch.Tensor, b: torch.Tensor, out_dtype: torch.dtype, scale_a: torch.Tensor, scale_b: torch.Tensor, cu_count: int) -> torch.Tensor:
    out = torch.empty((b.shape[0], a.shape[0]), dtype=out_dtype, device=b.device)
    torch.ops._rocm_C.wvSplitKQ(a, b, out, scale_a, scale_b, cu_count)
    return out
def moe_sum(input: torch.Tensor, output: torch.Tensor):
    torch.ops._moe_C.moe_sum(input, output)
def moe_align_block_size(topk_ids: torch.Tensor, num_experts: int, block_size: int, sorted_token_ids: torch.Tensor, experts_ids: torch.Tensor, num_tokens_post_pad: torch.Tensor) -> None:
    torch.ops._moe_C.moe_align_block_size(topk_ids, num_experts, block_size, sorted_token_ids, experts_ids, num_tokens_post_pad)
def moe_wna16_gemm(input: torch.Tensor, output: torch.Tensor, b_qweight: torch.Tensor, b_scales: torch.Tensor, b_qzeros: Optional[torch.Tensor], topk_weights: Optional[torch.Tensor], sorted_token_ids: torch.Tensor, experts_ids: torch.Tensor, num_tokens_post_pad: torch.Tensor, top_k: int, BLOCK_SIZE_M: int, BLOCK_SIZE_N: int, BLOCK_SIZE_K: int, bit: int) -> torch.Tensor:
    if not current_platform.is_cuda():
        raise NotImplementedError('The optimized moe_wna16_gemm kernel is only available on CUDA platforms')
    torch.ops._moe_C.moe_wna16_gemm(input, output, b_qweight, b_scales, b_qzeros, topk_weights, sorted_token_ids, experts_ids, num_tokens_post_pad, top_k, BLOCK_SIZE_M, BLOCK_SIZE_N, BLOCK_SIZE_K, bit)
def topk_softmax(topk_weights: torch.Tensor, topk_ids: torch.Tensor, token_expert_indices: torch.Tensor, gating_output: torch.Tensor) -> None:
    torch.ops._moe_C.topk_softmax(topk_weights, topk_ids, token_expert_indices, gating_output)
def moe_wna16_marlin_gemm(input: torch.Tensor, output: Optional[torch.Tensor], b_qweight: torch.Tensor, b_scales: torch.Tensor, global_scale: Optional[torch.Tensor], b_qzeros: Optional[torch.Tensor], g_idx: Optional[torch.Tensor], perm: Optional[torch.Tensor], workspace: torch.Tensor, sorted_token_ids: torch.Tensor, expert_ids: torch.Tensor, num_tokens_past_padded: torch.Tensor, topk_weights: torch.Tensor, moe_block_size: int, top_k: int, mul_topk_weights: bool, is_ep: bool, b_q_type: ScalarType, size_m: int, size_n: int, size_k: int, is_k_full: bool, use_atomic_add: bool, use_fp32_reduce: bool, is_zp_float: bool) -> torch.Tensor:
    return torch.ops._moe_C.moe_wna16_marlin_gemm(input, output, b_qweight, b_scales, global_scale, b_qzeros, g_idx, perm, workspace, sorted_token_ids, expert_ids, num_tokens_past_padded, topk_weights, moe_block_size, top_k, mul_topk_weights, is_ep, b_q_type.id, size_m, size_n, size_k, is_k_full, use_atomic_add, use_fp32_reduce, is_zp_float)
if supports_moe_ops and hasattr(torch.ops._moe_C, 'marlin_gemm_moe'):
    @register_fake('_moe_C::marlin_gemm_moe')
    def marlin_gemm_moe_fake(a: torch.Tensor, b_q_weights: torch.Tensor, sorted_ids: torch.Tensor, topk_weights: torch.Tensor, topk_ids: torch.Tensor, b_scales: torch.Tensor, b_zero_points: torch.Tensor, g_idx: torch.Tensor, perm: torch.Tensor, workspace: torch.Tensor, b_q_type: ScalarType, size_m: torch.SymInt, size_n: torch.SymInt, size_k: torch.SymInt, is_k_full: bool, num_experts: int, topk: int, moe_block_size: int, replicate_input: bool, apply_weights: bool) -> torch.Tensor:
        return torch.empty((size_m, topk, size_n), dtype=a.dtype, device=a.device)
    @register_fake('_moe_C::moe_wna16_marlin_gemm')
    def moe_wna16_marlin_gemm_fake(input: torch.Tensor, output: Optional[torch.Tensor], b_qweight: torch.Tensor, b_scales: torch.Tensor, b_qzeros: Optional[torch.Tensor], g_idx: Optional[torch.Tensor], perm: Optional[torch.Tensor], workspace: torch.Tensor, sorted_token_ids: torch.Tensor, expert_ids: torch.Tensor, num_tokens_past_padded: torch.Tensor, topk_weights: torch.Tensor, moe_block_size: int, top_k: int, mul_topk_weights: bool, is_ep: bool, b_q_type: ScalarType, size_m: int, size_n: int, size_k: int, is_k_full: bool, use_atomic_add: bool, use_fp32_reduce: bool, is_zp_float: bool) -> torch.Tensor:
        return torch.empty((size_m * top_k, size_n), dtype=input.dtype, device=input.device)
def reshape_and_cache(key: torch.Tensor, value: torch.Tensor, key_cache: torch.Tensor, value_cache: torch.Tensor, slot_mapping: torch.Tensor, kv_cache_dtype: str, k_scale: torch.Tensor, v_scale: torch.Tensor) -> None:
    torch.ops._C_cache_ops.reshape_and_cache(key, value, key_cache, value_cache, slot_mapping, kv_cache_dtype, k_scale, v_scale)
def reshape_and_cache_flash(key: torch.Tensor, value: torch.Tensor, key_cache: torch.Tensor, value_cache: torch.Tensor, slot_mapping: torch.Tensor, kv_cache_dtype: str, k_scale: torch.Tensor, v_scale: torch.Tensor) -> None:
    torch.ops._C_cache_ops.reshape_and_cache_flash(key, value, key_cache, value_cache, slot_mapping, kv_cache_dtype, k_scale, v_scale)
def concat_and_cache_mla(kv_c: torch.Tensor, k_pe: torch.Tensor, kv_cache: torch.Tensor, slot_mapping: torch.Tensor, kv_cache_dtype: str, scale: torch.Tensor) -> None:
    torch.ops._C_cache_ops.concat_and_cache_mla(kv_c, k_pe, kv_cache, slot_mapping, kv_cache_dtype, scale)
def copy_blocks(key_caches: list[torch.Tensor], value_caches: list[torch.Tensor], block_mapping: torch.Tensor) -> None:
    torch.ops._C_cache_ops.copy_blocks(key_caches, value_caches, block_mapping)
def copy_blocks_mla(kv_caches: list[torch.Tensor], block_mapping: torch.Tensor) -> None:
    torch.ops._C_cache_ops.copy_blocks_mla(kv_caches, block_mapping)
def swap_blocks(src: torch.Tensor, dst: torch.Tensor, block_mapping: torch.Tensor) -> None:
    torch.ops._C_cache_ops.swap_blocks(src, dst, block_mapping)
def convert_fp8(output: torch.Tensor, input: torch.Tensor, scale: float=1.0, kv_dtype: str='fp8') -> None:
    torch.ops._C_cache_ops.convert_fp8(output, input, scale, kv_dtype)
def gather_cache(src_cache: torch.Tensor, dst: torch.Tensor, block_table: torch.Tensor, cu_seq_lens: torch.Tensor, batch_size: int, seq_starts: Optional[torch.Tensor]=None) -> None:
    torch.ops._C_cache_ops.gather_cache(src_cache, dst, block_table, cu_seq_lens, batch_size, seq_starts)
def get_device_attribute(attribute: int, device: int) -> int:
    return torch.ops._C_cuda_utils.get_device_attribute(attribute, device)
def get_max_shared_memory_per_block_device_attribute(device: int) -> int:
    return torch.ops._C_cuda_utils.get_max_shared_memory_per_block_device_attribute(device)
def init_custom_ar(ipc_tensors: list[torch.Tensor], rank_data: torch.Tensor, rank: int, fully_connected: bool) -> int:
    return torch.ops._C_custom_ar.init_custom_ar(ipc_tensors, rank_data, rank, fully_connected)
def all_reduce(fa: int, inp: torch.Tensor, out: torch.Tensor, reg_buffer: int, reg_buffer_sz_bytes: int) -> None:
    torch.ops._C_custom_ar.all_reduce(fa, inp, out, reg_buffer, reg_buffer_sz_bytes)
def dispose(fa: int) -> None:
    torch.ops._C_custom_ar.dispose(fa)
def meta_size() -> int:
    return torch.ops._C_custom_ar.meta_size()
def register_buffer(fa: int, ipc_tensors: list[int]) -> None:
    return torch.ops._C_custom_ar.register_buffer(fa, ipc_tensors)
def get_graph_buffer_ipc_meta(fa: int) -> tuple[list[int], list[int]]:
    return torch.ops._C_custom_ar.get_graph_buffer_ipc_meta(fa)
def register_graph_buffers(fa: int, handles: list[list[int]], offsets: list[list[int]]) -> None:
    torch.ops._C_custom_ar.register_graph_buffers(fa, handles, offsets)
def allocate_shared_buffer_and_handle(size: int) -> tuple[int, torch.Tensor]:
    return torch.ops._C_custom_ar.allocate_shared_buffer_and_handle(size)
def open_mem_handle(mem_handle: torch.Tensor):
    return torch.ops._C_custom_ar.open_mem_handle(mem_handle)
def free_shared_buffer(ptr: int) -> None:
    torch.ops._C_custom_ar.free_shared_buffer(ptr)
def init_custom_qr(rank: int, world_size: int, qr_max_size: Optional[int]=None) -> int:
    return torch.ops._C_custom_ar.init_custom_qr(rank, world_size, qr_max_size)
def qr_destroy(fa: int) -> None:
    torch.ops._C_custom_ar.qr_destroy(fa)
def qr_all_reduce(fa: int, inp: torch.Tensor, out: torch.Tensor, quant_level: int, cast_bf2half: bool=False) -> None:
    torch.ops._C_custom_ar.qr_all_reduce(fa, inp, out, quant_level, cast_bf2half)
def qr_get_handle(fa: int) -> torch.Tensor:
    return torch.ops._C_custom_ar.qr_get_handle(fa)
def qr_open_handles(fa: int, handles: list[torch.Tensor]) -> None:
    return torch.ops._C_custom_ar.qr_open_handles(fa, handles)
def qr_max_size() -> int:
    return torch.ops._C_custom_ar.qr_max_size()
def get_flash_mla_metadata(cache_seqlens: torch.Tensor, num_heads_per_head_k: int, num_heads_k: int) -> tuple[torch.Tensor, torch.Tensor]:
    return torch.ops._C.get_flash_mla_metadata(cache_seqlens, num_heads_per_head_k, num_heads_k)
def flash_mla_with_kvcache(q: torch.Tensor, k_cache: torch.Tensor, block_table: torch.Tensor, cache_seqlens: torch.Tensor, head_dim_v: int, tile_scheduler_metadata: torch.Tensor, num_splits: torch.Tensor, softmax_scale: Optional[float]=None, causal: bool=False) -> tuple[torch.Tensor, torch.Tensor]:
    if softmax_scale is None:
        softmax_scale = q.shape[-1] ** (-0.5)
    out, softmax_lse = torch.ops._C.flash_mla_fwd_kvcache(q, k_cache, None, head_dim_v, cache_seqlens, block_table, softmax_scale, causal, tile_scheduler_metadata, num_splits)
    return (out, softmax_lse)
def cutlass_mla_decode(out: torch.Tensor, q_nope: torch.Tensor, q_pe: torch.Tensor, kv_c_and_k_pe_cache: torch.Tensor, seq_lens: torch.Tensor, page_table: torch.Tensor, scale: float) -> torch.Tensor:
    torch.ops._C.cutlass_mla_decode(out, q_nope, q_pe, kv_c_and_k_pe_cache, seq_lens, page_table, scale)
    return out
def sm100_cutlass_mla_decode(out: torch.Tensor, q_nope: torch.Tensor, q_pe: torch.Tensor, kv_c_and_k_pe_cache: torch.Tensor, seq_lens: torch.Tensor, page_table: torch.Tensor, workspace: torch.Tensor, scale: float, num_kv_splits: int) -> torch.Tensor:
    torch.ops._C.sm100_cutlass_mla_decode(out, q_nope, q_pe, kv_c_and_k_pe_cache, seq_lens, page_table, workspace, scale, num_kv_splits)
    return out
def sm100_cutlass_mla_get_workspace_size(max_seq_len: int, num_batches: int, sm_count: int, num_kv_splits: int) -> int:
    return torch.ops._C.sm100_cutlass_mla_get_workspace_size(max_seq_len, num_batches, sm_count, num_kv_splits)
if hasattr(torch.ops._C, 'weight_packed_linear'):
    @register_fake('_C::weight_packed_linear')
    def weight_packed_linear_fake(mat1: torch.Tensor, mat2: torch.Tensor, bias: Optional[torch.Tensor], is_vnni: bool) -> torch.Tensor:
        return torch.empty((mat1.size(0), mat2.size(0)), dtype=mat1.dtype, device=mat2.device)
if hasattr(torch.ops._C, 'fused_experts_cpu'):
    @register_fake('_C::fused_experts_cpu')
    def fused_experts_cpu_fake(hidden_states: torch.Tensor, w1: torch.Tensor, w2: torch.Tensor, topk_weights: torch.Tensor, topk_ids: torch.Tensor, inplace: bool, use_int8_w8a8: bool, use_fp8_w8a16: bool, w1_scale: Optional[torch.Tensor], w2_scale: Optional[torch.Tensor], block_size: Optional[list[int]], a1_scale: Optional[torch.Tensor], a2_scale: Optional[torch.Tensor], is_vnni: bool) -> torch.Tensor:
        return torch.empty_like(hidden_states)
if hasattr(torch.ops._C, 'int8_scaled_mm_with_quant'):
    @register_fake('_C::int8_scaled_mm_with_quant')
    def int8_scaled_mm_with_quant_fake(mat1: torch.Tensor, mat2: torch.Tensor, scales2: torch.Tensor, bias: Optional[torch.Tensor], out_dtype: torch.dtype, is_vnni: bool) -> torch.Tensor:
        M = mat1.size(0)
        N = mat2.size(0)
        return torch.empty((M, N), dtype=out_dtype)
def sampling_from_probs(probs: torch.Tensor, uniform_samplers: torch.Tensor, deterministic: bool=True, check_nan: bool=False) -> torch.Tensor:
    if check_nan and torch.any(torch.isnan(probs)):
        raise ValueError('NaN detected in probs')
    return torch.ops._C.sampling_from_probs(probs, uniform_samplers, deterministic)
def _to_tensor_scalar_tuple(x):
    if isinstance(x, torch.Tensor):
        return (x, 0)
    else:
        return (None, x)
def top_p_sampling_from_probs(probs: torch.Tensor, uniform_samples: torch.Tensor, top_p: Union[torch.Tensor, float], deterministic: bool=True, check_nan: bool=False) -> tuple[torch.Tensor, torch.Tensor]:
    if check_nan and torch.any(torch.isnan(probs)):
        raise ValueError('NaN detected in probs')
    return torch.ops._C.top_p_sampling_from_probs(probs, uniform_samples, *_to_tensor_scalar_tuple(top_p), deterministic)
def top_k_sampling_from_probs(probs: torch.Tensor, uniform_samples: torch.Tensor, top_k: Union[torch.Tensor, int], deterministic: bool=True, check_nan: bool=False) -> tuple[torch.Tensor, torch.Tensor]:
    if check_nan and torch.any(torch.isnan(probs)):
        raise ValueError('NaN detected in probs')
    return torch.ops._C.top_k_sampling_from_probs(probs, uniform_samples, *_to_tensor_scalar_tuple(top_k), deterministic)
def min_p_sampling_from_probs(probs: torch.Tensor, uniform_samples: torch.Tensor, min_p: Union[torch.Tensor, float], deterministic: bool=True, check_nan: bool=False) -> tuple[torch.Tensor, torch.Tensor]:
    if check_nan and torch.any(torch.isnan(probs)):
        raise ValueError('NaN detected in probs')
    return torch.ops._C.min_p_sampling_from_probs(probs, uniform_samples, *_to_tensor_scalar_tuple(min_p), deterministic)
def top_k_mask_logits(logits: torch.Tensor, top_k: Union[torch.Tensor, int]) -> torch.Tensor:
    return torch.ops._C.top_k_mask_logits(logits, *_to_tensor_scalar_tuple(top_k))
def top_p_renorm_prob(probs: torch.Tensor, top_p: Union[torch.Tensor, float]) -> torch.Tensor:
    return torch.ops._C.top_p_renorm_prob(probs, *_to_tensor_scalar_tuple(top_p))
def top_k_renorm_prob(probs: torch.Tensor, top_k: Union[torch.Tensor, int]) -> torch.Tensor:
    return torch.ops._C.top_k_renorm_prob(probs, *_to_tensor_scalar_tuple(top_k))
def top_k_top_p_sampling_from_logits(probs: torch.Tensor, uniform_samples: torch.Tensor, top_k: Union[torch.Tensor, int], top_p: Union[torch.Tensor, float], filter_apply_order: str='top_k_first', deterministic: bool=True, check_nan: bool=False) -> tuple[torch.Tensor, torch.Tensor]:
    if filter_apply_order == 'top_k_first':
        masked_logits = top_k_mask_logits(probs, top_k)
        probs = torch.softmax(masked_logits, dim=-1)
        return top_p_sampling_from_probs(probs, uniform_samples, top_p, deterministic, check_nan)
    elif filter_apply_order == 'joint':
        probs = torch.softmax(probs, dim=-1)
        if check_nan and torch.any(torch.isnan(probs)):
            raise ValueError('NaN detected in probs')
        return torch.ops._C.top_k_top_p_sampling_from_logits(probs, uniform_samples, *_to_tensor_scalar_tuple(top_k), *_to_tensor_scalar_tuple(top_p), deterministic)
    else:
        raise ValueError(f'Invalid filter_apply_order: {filter_apply_order}')
def top_k_top_p_sampling_from_probs(probs: torch.Tensor, uniform_samples: torch.Tensor, top_k: Union[torch.Tensor, int], top_p: Union[torch.Tensor, float], filter_apply_order: str='top_k_first', deterministic: bool=True, check_nan: bool=False) -> tuple[torch.Tensor, torch.Tensor]:
    if filter_apply_order == 'top_k_first':
        renorm_probs = top_k_renorm_prob(probs, top_k)
        return top_p_sampling_from_probs(renorm_probs, uniform_samples, top_p, deterministic, check_nan)
    elif filter_apply_order == 'joint':
        if check_nan and torch.any(torch.isnan(probs)):
            raise ValueError('NaN detected in probs')
        return torch.ops._C.top_k_top_p_sampling_from_probs(probs, uniform_samples, *_to_tensor_scalar_tuple(top_k), *_to_tensor_scalar_tuple(top_p), deterministic)
    else:
        raise ValueError(f'Invalid filter_apply_order: {filter_apply_order}')