#ifndef MTMD_HELPER_H
#define MTMD_HELPER_H
#include "ggml.h"
#include "llama.h"
#include "mtmd.h"
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#ifdef __cplusplus
extern "C" {
#endif
MTMD_API mtmd_bitmap * mtmd_helper_bitmap_init_from_file(mtmd_context * ctx, const char * fname);
MTMD_API mtmd_bitmap * mtmd_helper_bitmap_init_from_buf(mtmd_context * ctx, const unsigned char * buf, size_t len);
MTMD_API size_t mtmd_helper_get_n_tokens(const mtmd_input_chunks * chunks);
MTMD_API llama_pos mtmd_helper_get_n_pos(const mtmd_input_chunks * chunks);
MTMD_API int32_t mtmd_helper_eval_chunks(mtmd_context * ctx,
struct llama_context * lctx,
const mtmd_input_chunks * chunks,
llama_pos n_past,
llama_seq_id seq_id,
int32_t n_batch,
bool logits_last,
llama_pos * new_n_past);
MTMD_API int32_t mtmd_helper_eval_chunk_single(mtmd_context * ctx,
struct llama_context * lctx,
const mtmd_input_chunk * chunk,
llama_pos n_past,
llama_seq_id seq_id,
int32_t n_batch,
bool logits_last,
llama_pos * new_n_past);
MTMD_API int32_t mtmd_helper_decode_image_chunk(mtmd_context * ctx,
struct llama_context * lctx,
const mtmd_input_chunk * chunk,
float * encoded_embd,
llama_pos n_past,
llama_seq_id seq_id,
int32_t n_batch,
llama_pos * new_n_past);
#ifdef __cplusplus
}
#endif
#endif