#pragma once
#include "ggml.h"
#include <stddef.h>
#include <stdint.h>
struct clip_ctx;
struct clip_image_size {
int width;
int height;
};
struct clip_image_f32;
struct clip_image_u8_batch;
struct clip_image_f32_batch;
enum clip_modality {
CLIP_MODALITY_VISION,
CLIP_MODALITY_AUDIO,
};
struct clip_context_params {
bool use_gpu;
enum ggml_log_level verbosity;
};
struct clip_init_result {
struct clip_ctx * ctx_v;
struct clip_ctx * ctx_a;
};
struct clip_init_result clip_init(const char * fname, struct clip_context_params ctx_params);
void clip_free(struct clip_ctx * ctx);
size_t clip_embd_nbytes(const struct clip_ctx * ctx);
size_t clip_embd_nbytes_by_img(const struct clip_ctx * ctx, int img_w, int img_h);
int32_t clip_get_image_size (const struct clip_ctx * ctx);
int32_t clip_get_patch_size (const struct clip_ctx * ctx);
int32_t clip_get_hidden_size(const struct clip_ctx * ctx);
const char * clip_patch_merge_type(const struct clip_ctx * ctx);
int clip_n_output_tokens(const struct clip_ctx * ctx, struct clip_image_f32 * img);
int clip_n_output_tokens_x(const struct clip_ctx * ctx, struct clip_image_f32 * img);
int clip_n_output_tokens_y(const struct clip_ctx * ctx, struct clip_image_f32 * img);
int clip_n_mmproj_embd(const struct clip_ctx * ctx);
struct clip_image_size * clip_image_size_init(void);
struct clip_image_u8 * clip_image_u8_init (void);
struct clip_image_f32 * clip_image_f32_init(void);
struct clip_image_f32_batch * clip_image_f32_batch_init(void);
unsigned char * clip_image_u8_get_data(struct clip_image_u8 * img, uint32_t * nx, uint32_t * ny);
void clip_image_size_free (struct clip_image_size * img_size);
void clip_image_u8_free (struct clip_image_u8 * img);
void clip_image_f32_free(struct clip_image_f32 * img);
void clip_image_u8_batch_free (struct clip_image_u8_batch * batch);
void clip_image_f32_batch_free(struct clip_image_f32_batch * batch);
size_t clip_image_f32_batch_n_images(const struct clip_image_f32_batch * batch);
size_t clip_image_f32_batch_nx(const struct clip_image_f32_batch * batch, int idx);
size_t clip_image_f32_batch_ny(const struct clip_image_f32_batch * batch, int idx);
struct clip_image_f32 * clip_image_f32_get_img(const struct clip_image_f32_batch * batch, int idx);
void clip_build_img_from_pixels(const unsigned char * rgb_pixels, int nx, int ny, struct clip_image_u8 * img);
bool clip_image_load_from_file(const char * fname, struct clip_image_u8 * img);
bool clip_image_load_from_bytes(const unsigned char * bytes, size_t bytes_length, struct clip_image_u8 * img);
bool clip_image_preprocess(struct clip_ctx * ctx, const struct clip_image_u8 * img, struct clip_image_f32_batch * res_imgs );
struct ggml_tensor * clip_get_newline_tensor(const struct clip_ctx * ctx);
bool clip_image_encode (struct clip_ctx * ctx, int n_threads, struct clip_image_f32 * img, float * vec);
bool clip_image_batch_encode(struct clip_ctx * ctx, int n_threads, const struct clip_image_f32_batch * imgs, float * vec);
int clip_is_minicpmv(const struct clip_ctx * ctx);
bool clip_is_glm(const struct clip_ctx * ctx);
bool clip_is_qwen2vl(const struct clip_ctx * ctx);
bool clip_is_llava(const struct clip_ctx * ctx);
bool clip_is_gemma3(const struct clip_ctx * ctx);
bool clip_encode_float_image (struct clip_ctx * ctx, int n_threads, float * img, int h, int w, float * vec);
void clip_image_f32_batch_add_mel(struct clip_image_f32_batch * batch, int n_mel, int n_frames, float * mel);
bool clip_has_vision_encoder(const struct clip_ctx * ctx);
bool clip_has_audio_encoder(const struct clip_ctx * ctx);
bool clip_has_whisper_encoder(const struct clip_ctx * ctx);