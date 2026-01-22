#ifndef MTMD_H
#define MTMD_H
#include "ggml.h"
#include "llama.h"
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#ifdef __cplusplus
#include <string>
#include <vector>
#include <cinttypes>
#include <memory>
#endif
#ifdef LLAMA_SHARED
# if defined(_WIN32) && !defined(__MINGW32__)
# ifdef LLAMA_BUILD
# define MTMD_API __declspec(dllexport)
# else
# define MTMD_API __declspec(dllimport)
# endif
# else
# define MTMD_API __attribute__ ((visibility ("default")))
# endif
#else
# define MTMD_API
#endif
#define MTMD_DEFAULT_IMAGE_MARKER "<__image__>"
#ifdef __cplusplus
extern "C" {
#endif
enum mtmd_input_chunk_type {
MTMD_INPUT_CHUNK_TYPE_TEXT,
MTMD_INPUT_CHUNK_TYPE_IMAGE,
MTMD_INPUT_CHUNK_TYPE_AUDIO,
};
struct mtmd_context;
struct mtmd_bitmap;
struct mtmd_image_tokens;
struct mtmd_input_chunk;
struct mtmd_input_chunks;
struct mtmd_input_text {
const char * text;
bool add_special;
bool parse_special;
};
typedef struct mtmd_context mtmd_context;
typedef struct mtmd_bitmap mtmd_bitmap;
typedef struct mtmd_image_tokens mtmd_image_tokens;
typedef struct mtmd_input_chunk mtmd_input_chunk;
typedef struct mtmd_input_chunks mtmd_input_chunks;
typedef struct mtmd_input_text mtmd_input_text;
struct mtmd_context_params {
bool use_gpu;
bool print_timings;
int n_threads;
enum ggml_log_level verbosity;
const char * image_marker;
const char * media_marker;
};
MTMD_API const char * mtmd_default_marker(void);
MTMD_API struct mtmd_context_params mtmd_context_params_default(void);
MTMD_API mtmd_context * mtmd_init_from_file(const char * mmproj_fname,
const struct llama_model * text_model,
const struct mtmd_context_params ctx_params);
MTMD_API void mtmd_free(mtmd_context * ctx);
MTMD_API bool mtmd_decode_use_non_causal(mtmd_context * ctx);
MTMD_API bool mtmd_decode_use_mrope(mtmd_context * ctx);
MTMD_API bool mtmd_support_vision(mtmd_context * ctx);
MTMD_API bool mtmd_support_audio(mtmd_context * ctx);
MTMD_API int mtmd_get_audio_bitrate(mtmd_context * ctx);
MTMD_API mtmd_bitmap * mtmd_bitmap_init (uint32_t nx, uint32_t ny, const unsigned char * data);
MTMD_API mtmd_bitmap * mtmd_bitmap_init_from_audio(size_t n_samples, const float * data);
MTMD_API uint32_t mtmd_bitmap_get_nx (const mtmd_bitmap * bitmap);
MTMD_API uint32_t mtmd_bitmap_get_ny (const mtmd_bitmap * bitmap);
MTMD_API const unsigned char * mtmd_bitmap_get_data (const mtmd_bitmap * bitmap);
MTMD_API size_t mtmd_bitmap_get_n_bytes(const mtmd_bitmap * bitmap);
MTMD_API bool mtmd_bitmap_is_audio (const mtmd_bitmap * bitmap);
MTMD_API void mtmd_bitmap_free (mtmd_bitmap * bitmap);
MTMD_API const char * mtmd_bitmap_get_id(const mtmd_bitmap * bitmap);
MTMD_API void mtmd_bitmap_set_id(mtmd_bitmap * bitmap, const char * id);
MTMD_API mtmd_input_chunks * mtmd_input_chunks_init(void);
MTMD_API size_t mtmd_input_chunks_size(const mtmd_input_chunks * chunks);
MTMD_API const mtmd_input_chunk * mtmd_input_chunks_get (const mtmd_input_chunks * chunks, size_t idx);
MTMD_API void mtmd_input_chunks_free(mtmd_input_chunks * chunks);
MTMD_API enum mtmd_input_chunk_type mtmd_input_chunk_get_type (const mtmd_input_chunk * chunk);
MTMD_API const llama_token * mtmd_input_chunk_get_tokens_text (const mtmd_input_chunk * chunk, size_t * n_tokens_output);
MTMD_API const mtmd_image_tokens * mtmd_input_chunk_get_tokens_image(const mtmd_input_chunk * chunk);
MTMD_API size_t mtmd_input_chunk_get_n_tokens (const mtmd_input_chunk * chunk);
MTMD_API const char * mtmd_input_chunk_get_id (const mtmd_input_chunk * chunk);
MTMD_API llama_pos mtmd_input_chunk_get_n_pos (const mtmd_input_chunk * chunk);
MTMD_API mtmd_input_chunk * mtmd_input_chunk_copy(const mtmd_input_chunk * chunk);
MTMD_API void mtmd_input_chunk_free(mtmd_input_chunk * chunk);
MTMD_API size_t mtmd_image_tokens_get_n_tokens(const mtmd_image_tokens * image_tokens);
MTMD_API size_t mtmd_image_tokens_get_nx (const mtmd_image_tokens * image_tokens);
MTMD_API size_t mtmd_image_tokens_get_ny (const mtmd_image_tokens * image_tokens);
MTMD_API const char * mtmd_image_tokens_get_id (const mtmd_image_tokens * image_tokens);
MTMD_API llama_pos mtmd_image_tokens_get_n_pos (const mtmd_image_tokens * image_tokens);
MTMD_API int32_t mtmd_tokenize(mtmd_context * ctx,
mtmd_input_chunks * output,
const mtmd_input_text * text,
const mtmd_bitmap ** bitmaps,
size_t n_bitmaps);
MTMD_API int32_t mtmd_encode(mtmd_context * ctx,
const mtmd_image_tokens * image_tokens);
MTMD_API int32_t mtmd_encode_chunk(mtmd_context * ctx,
const mtmd_input_chunk * chunk);
MTMD_API float * mtmd_get_output_embd(mtmd_context * ctx);
MTMD_API mtmd_input_chunks * mtmd_test_create_input_chunks(void);
#ifdef __cplusplus
}
#endif
#ifdef __cplusplus
namespace mtmd {
struct mtmd_context_deleter {
void operator()(mtmd_context * val) { mtmd_free(val); }
};
using context_ptr = std::unique_ptr<mtmd_context, mtmd_context_deleter>;
struct mtmd_bitmap_deleter {
void operator()(mtmd_bitmap * val) { mtmd_bitmap_free(val); }
};
using bitmap_ptr = std::unique_ptr<mtmd_bitmap, mtmd_bitmap_deleter>;
struct mtmd_input_chunks_deleter {
void operator()(mtmd_input_chunks * val) { mtmd_input_chunks_free(val); }
};
using input_chunks_ptr = std::unique_ptr<mtmd_input_chunks, mtmd_input_chunks_deleter>;
struct mtmd_input_chunk_deleter {
void operator()(mtmd_input_chunk * val) { mtmd_input_chunk_free(val); }
};
using input_chunk_ptr = std::unique_ptr<mtmd_input_chunk, mtmd_input_chunk_deleter>;
struct bitmap {
bitmap_ptr ptr;
bitmap() : ptr(nullptr) {}
bitmap(mtmd_bitmap * bitmap) : ptr(bitmap) {}
bitmap(bitmap && other) noexcept : ptr(std::move(other.ptr)) {}
bitmap(uint32_t nx, uint32_t ny, const unsigned char * data) {
ptr.reset(mtmd_bitmap_init(nx, ny, data));
}
~bitmap() = default;
uint32_t nx() { return mtmd_bitmap_get_nx(ptr.get()); }
uint32_t ny() { return mtmd_bitmap_get_ny(ptr.get()); }
const unsigned char * data() { return mtmd_bitmap_get_data(ptr.get()); }
size_t n_bytes() { return mtmd_bitmap_get_n_bytes(ptr.get()); }
std::string id() { return mtmd_bitmap_get_id(ptr.get()); }
void set_id(const char * id) { mtmd_bitmap_set_id(ptr.get(), id); }
};
struct bitmaps {
std::vector<bitmap> entries;
~bitmaps() = default;
std::vector<const mtmd_bitmap *> c_ptr() {
std::vector<const mtmd_bitmap *> res(entries.size());
for (size_t i = 0; i < entries.size(); i++) {
res[i] = entries[i].ptr.get();
}
return res;
}
};
struct input_chunks {
input_chunks_ptr ptr;
input_chunks() = default;
input_chunks(mtmd_input_chunks * chunks) : ptr(chunks) {}
~input_chunks() = default;
size_t size() { return mtmd_input_chunks_size(ptr.get()); }
const mtmd_input_chunk * operator[](size_t idx) {
return mtmd_input_chunks_get(ptr.get(), idx);
}
};
}
#endif
#endif