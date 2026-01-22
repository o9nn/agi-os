#include "core/registration.h"
#include "rocm/ops.h"
TORCH_LIBRARY_EXPAND(TORCH_EXTENSION_NAME, rocm_ops) {
rocm_ops.def(
"LLMM1(Tensor in_a, Tensor in_b, int rows_per_block) -> "
"Tensor");
rocm_ops.impl("LLMM1", torch::kCUDA, &LLMM1);
rocm_ops.def(
"wvSplitK(Tensor in_a, Tensor in_b, int CuCount) -> "
"Tensor");
rocm_ops.impl("wvSplitK", torch::kCUDA, &wvSplitK);
rocm_ops.def(
"wvSplitKQ(Tensor in_a, Tensor in_b, Tensor! out_c, Tensor scale_a, "
"          Tensor scale_b, int CuCount) -> ()");
rocm_ops.impl("wvSplitKQ", torch::kCUDA, &wvSplitKQ);
rocm_ops.def(
"paged_attention(Tensor! out, Tensor exp_sums,"
"                Tensor max_logits, Tensor tmp_out,"
"                Tensor query, Tensor key_cache,"
"                Tensor value_cache, int num_kv_heads,"
"                float scale, Tensor block_tables,"
"                Tensor context_lens,"
"                Tensor? query_start_loc,"
"                int block_size,"
"                int max_context_len,"
"                Tensor? alibi_slopes,"
"                str kv_cache_dtype,"
"                Tensor k_scale, Tensor v_scale,"
"                Tensor? fp8_out_scale) -> ()");
rocm_ops.impl("paged_attention", torch::kCUDA, &paged_attention);
}
REGISTER_EXTENSION(TORCH_EXTENSION_NAME)