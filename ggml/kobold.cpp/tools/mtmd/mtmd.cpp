#include "clip.h"
#include "clip-impl.h"
#include "mtmd.h"
#include "mtmd-audio.h"
#include "llama.h"
#include <algorithm>
#include <cerrno>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <limits>
#include <vector>
struct mtmd_bitmap {
uint32_t nx;
uint32_t ny;
std::vector<unsigned char> data;
std::string id;
bool is_audio = false;
};
struct mtmd_image_tokens {
uint32_t nx;
uint32_t ny;
bool use_mrope_pos = false;
uint32_t n_tokens() const { return nx * ny; }
clip_image_f32_batch batch_f32;
std::string id;
mtmd_image_tokens clone() {
return mtmd_image_tokens{
nx,
ny,
use_mrope_pos,
batch_f32.clone(),
id
};
}
};
using mtmd_image_tokens_ptr = std::unique_ptr<mtmd_image_tokens>;
struct mtmd_audio_tokens {
uint32_t n_tokens;
clip_image_f32_batch batch_f32;
std::string id;
mtmd_audio_tokens clone() {
return mtmd_audio_tokens{
n_tokens,
batch_f32.clone(),
id
};
}
};
using mtmd_audio_tokens_ptr = std::unique_ptr<mtmd_audio_tokens>;
struct mtmd_input_chunk {
mtmd_input_chunk_type type;
std::vector<llama_token> tokens_text;
mtmd_image_tokens_ptr tokens_image;
mtmd_audio_tokens_ptr tokens_audio;
};
struct mtmd_input_chunks {
std::vector<mtmd_input_chunk> entries;
};
enum mtmd_slice_tmpl {
MTMD_SLICE_TMPL_NONE,
MTMD_SLICE_TMPL_MINICPMV_2_5,
MTMD_SLICE_TMPL_MINICPMV_2_6,
MTMD_SLICE_TMPL_LLAMA4,
};
const char * mtmd_default_marker() {
return "<__media__>";
}
mtmd_context_params mtmd_context_params_default() {
mtmd_context_params params;
params.use_gpu = true;
params.print_timings = true;
params.n_threads = 4;
params.verbosity = GGML_LOG_LEVEL_INFO;
params.image_marker = MTMD_DEFAULT_IMAGE_MARKER;
params.media_marker = mtmd_default_marker();
return params;
}
struct mtmd_context {
struct clip_ctx * ctx_v;
struct clip_ctx * ctx_a;
const struct llama_model * text_model;
std::vector<float> image_embd_v;
bool print_timings;
int n_threads;
std::string media_marker;
const int n_embd_text;
std::string img_beg;
std::string img_end;
std::string aud_beg;
std::string aud_end;
mtmd_slice_tmpl slice_tmpl    = MTMD_SLICE_TMPL_NONE;
llama_token tok_ov_img_start  = LLAMA_TOKEN_NULL;
llama_token tok_ov_img_end    = LLAMA_TOKEN_NULL;
llama_token tok_slices_start  = LLAMA_TOKEN_NULL;
llama_token tok_slices_end    = LLAMA_TOKEN_NULL;
llama_token tok_sli_img_start = LLAMA_TOKEN_NULL;
llama_token tok_sli_img_end   = LLAMA_TOKEN_NULL;
llama_token tok_sli_img_mid   = LLAMA_TOKEN_NULL;
llama_token tok_row_end       = LLAMA_TOKEN_NULL;
bool        tok_row_end_trail = false;
bool        ov_img_first      = false;
bool use_mrope = false;
whisper_preprocessor::whisper_filters w_filters;
mtmd_context(const char * mmproj_fname,
const llama_model * text_model,
const mtmd_context_params & ctx_params) :
text_model   (text_model),
print_timings(ctx_params.print_timings),
n_threads    (ctx_params.n_threads),
media_marker (ctx_params.media_marker),
n_embd_text  (llama_model_n_embd(text_model))
{
if (std::string(ctx_params.image_marker) != MTMD_DEFAULT_IMAGE_MARKER) {
throw std::runtime_error("custom image_marker is not supported anymore, use media_marker instead");
}
if (media_marker.empty()) {
throw std::runtime_error("media_marker must not be empty");
}
clip_context_params ctx_clip_params;
ctx_clip_params.use_gpu   = ctx_params.use_gpu;
ctx_clip_params.verbosity = ctx_params.verbosity;
auto res = clip_init(mmproj_fname, ctx_clip_params);
ctx_v = res.ctx_v;
ctx_a = res.ctx_a;
if (!ctx_v && !ctx_a) {
throw std::runtime_error(string_format("Failed to load CLIP model from %s\n", mmproj_fname));
}
if (ctx_v && ctx_a) {
int n_embd_v = clip_n_mmproj_embd(ctx_v);
int n_embd_a = clip_n_mmproj_embd(ctx_a);
if (n_embd_v != n_embd_a) {
throw std::runtime_error(string_format(
"mismatch between vision and audio mmproj (n_embd_v = %d, n_embd_a = %d)\n",
n_embd_v, n_embd_a));
}
}
int n_embd_clip = clip_n_mmproj_embd(ctx_v ? ctx_v : ctx_a);
if (n_embd_text != n_embd_clip) {
throw std::runtime_error(string_format(
"mismatch between text model (n_embd = %d) and mmproj (n_embd = %d)\n"
"hint: you may be using wrong mmproj\n",
n_embd_text, n_embd_clip));
}
if (ctx_v) {
init_vision();
}
if (ctx_a) {
init_audio();
}
}
void init_vision() {
GGML_ASSERT(ctx_v != nullptr);
use_mrope = clip_is_qwen2vl(ctx_v);
projector_type proj = clip_get_projector_type(ctx_v);
int minicpmv_version = clip_is_minicpmv(ctx_v);
if (minicpmv_version == 2) {
slice_tmpl        = MTMD_SLICE_TMPL_MINICPMV_2_5;
tok_ov_img_start  = lookup_token("<image>");
tok_ov_img_end    = lookup_token("</image>");
tok_slices_start  = lookup_token("<slice>");
tok_slices_end    = lookup_token("</slice>");
tok_sli_img_start = tok_ov_img_start;
tok_sli_img_end   = tok_ov_img_end;
tok_row_end       = lookup_token("\n");
tok_row_end_trail = false;
ov_img_first      = true;
} else if (minicpmv_version == 3 || minicpmv_version == 4 || minicpmv_version == 5) {
slice_tmpl        = MTMD_SLICE_TMPL_MINICPMV_2_6;
tok_ov_img_start  = lookup_token("<image>");
tok_ov_img_end    = lookup_token("</image>");
tok_sli_img_start = lookup_token("<slice>");
tok_sli_img_end   = lookup_token("</slice>");
tok_row_end       = lookup_token("\n");
tok_row_end_trail = false;
ov_img_first      = true;
} else if (minicpmv_version != 0) {
GGML_ASSERT(false && "unsupported minicpmv version");
} else if (proj == PROJECTOR_TYPE_LLAMA4) {
slice_tmpl        = MTMD_SLICE_TMPL_LLAMA4;
tok_ov_img_start  = lookup_token("<|image|>");
tok_sli_img_mid   = lookup_token("<|tile_x_separator|>");
tok_row_end       = lookup_token("<|tile_y_separator|>");
tok_row_end_trail = true;
ov_img_first      = false;
}
if (proj == PROJECTOR_TYPE_GEMMA3) {
img_beg = "<start_of_image>";
img_end = "<end_of_image>";
} else if (proj == PROJECTOR_TYPE_IDEFICS3) {
img_beg = "<fake_token_around_image><global-img>";
img_end = "<fake_token_around_image>";
} else if (proj == PROJECTOR_TYPE_PIXTRAL) {
img_end = "[IMG_END]";
} else if (proj == PROJECTOR_TYPE_QWEN2VL || proj == PROJECTOR_TYPE_QWEN25VL) {
img_beg = "<|vision_start|>";
img_end = "<|vision_end|>";
} else if (proj == PROJECTOR_TYPE_LLAMA4) {
img_beg = "<|image_start|>";
img_end = "<|image_end|>";
LOG_WRN("%s: llama 4 vision is known to have degraded quality:\n"
"    https:
} else if (proj == PROJECTOR_TYPE_INTERNVL) {
img_beg = "<img>";
img_end = "</img>";
}
}
void init_audio() {
GGML_ASSERT(ctx_a != nullptr);
projector_type proj = clip_get_projector_type(ctx_a);
if (clip_has_whisper_encoder(ctx_a)) {
w_filters = whisper_precalc_filters::get_128_bins();
}
LOG_WRN("%s: audio input is in experimental stage and may have reduced quality:\n"
"    https:
if (proj == PROJECTOR_TYPE_QWEN2A) {
aud_beg = "<|audio_bos|>";
aud_end = "<|audio_eos|>";
} else if (proj == PROJECTOR_TYPE_ULTRAVOX) {
aud_beg = "[BEGIN_AUDIO]";
}
}
clip_ctx * get_clip_ctx(const mtmd_input_chunk * chunk) const {
if (chunk->type == MTMD_INPUT_CHUNK_TYPE_IMAGE) {
return ctx_v;
} else if (chunk->type == MTMD_INPUT_CHUNK_TYPE_AUDIO) {
return ctx_a;
}
GGML_ABORT("unknown chunk type");
}
projector_type proj_type_v() const {
return ctx_v ? clip_get_projector_type(ctx_v) : PROJECTOR_TYPE_UNKNOWN;
}
projector_type proj_type_a() const {
return ctx_a ? clip_get_projector_type(ctx_a) : PROJECTOR_TYPE_UNKNOWN;
}
~mtmd_context() {
clip_free(ctx_a);
clip_free(ctx_v);
}
private:
llama_token lookup_token(const std::string & token_text) {
const llama_vocab * vocab = llama_model_get_vocab(text_model);
const int n_vocab = llama_vocab_n_tokens(vocab);
for (int i = 0; i < n_vocab; i++) {
if (token_to_piece(vocab, i, true) == token_text) {
return i;
}
}
return LLAMA_TOKEN_NULL;
}
std::string token_to_piece(const llama_vocab * vocab, llama_token token, bool special) {
std::string piece;
piece.resize(piece.capacity());
const int n_chars = llama_token_to_piece(vocab, token, &piece[0], piece.size(), 0, special);
if (n_chars < 0) {
piece.resize(-n_chars);
int check = llama_token_to_piece(vocab, token, &piece[0], piece.size(), 0, special);
GGML_ASSERT(check == -n_chars);
} else {
piece.resize(n_chars);
}
return piece;
}
};
mtmd_context * mtmd_init_from_file(const char * mmproj_fname,
const struct llama_model * text_model,
const struct mtmd_context_params ctx_params) {
try {
return new mtmd_context(mmproj_fname, text_model, ctx_params);
} catch (const std::exception & e) {
LOG_ERR("%s: error: %s\n", __func__, e.what());
return nullptr;
}
}
void mtmd_free(mtmd_context * ctx) {
if (ctx) {
delete ctx;
}
}
struct mtmd_tokenizer {
mtmd_context * ctx;
std::vector<const mtmd_bitmap *> bitmaps;
std::string input_text;
bool add_special;
bool parse_special;
const llama_vocab * vocab;
mtmd_input_chunks cur;
mtmd_tokenizer(mtmd_context * ctx,
const mtmd_input_text * text,
const mtmd_bitmap ** bitmaps,
size_t n_bitmaps) : ctx(ctx), bitmaps(bitmaps, bitmaps + n_bitmaps) {
add_special   = text->add_special;
parse_special = text->parse_special;
input_text    = text->text;
vocab         = llama_model_get_vocab(ctx->text_model);
string_replace_all(input_text, MTMD_DEFAULT_IMAGE_MARKER, ctx->media_marker);
}
int32_t tokenize(mtmd_input_chunks * output) {
cur.entries.clear();
std::vector<std::string> parts = split_text(input_text, ctx->media_marker);
size_t i_bm = 0;
for (auto & part : parts) {
if (part == ctx->media_marker) {
if (i_bm >= bitmaps.size()) {
LOG_ERR("%s: error: number of bitmaps (%zu) does not match number of markers (%zu)\n",
__func__, bitmaps.size(), parts.size() - 1);
return 1;
}
const mtmd_bitmap * bitmap = bitmaps[i_bm++];
int32_t res = add_media(bitmap);
if (res != 0) {
return res;
}
} else {
add_text(part, parse_special);
}
}
if (add_special && llama_vocab_get_add_bos(vocab)) {
if (!cur.entries.empty() && cur.entries[0].type == MTMD_INPUT_CHUNK_TYPE_TEXT) {
cur.entries[0].tokens_text.insert(cur.entries[0].tokens_text.begin(), llama_vocab_bos(vocab));
} else {
mtmd_input_chunk bos_chunk{
MTMD_INPUT_CHUNK_TYPE_TEXT,
{llama_vocab_bos(vocab)},
nullptr,
nullptr,
};
cur.entries.insert(cur.entries.begin(), std::move(bos_chunk));
}
}
if (add_special && llama_vocab_get_add_eos(vocab)) {
add_text({llama_vocab_eos(vocab)});
}
if (i_bm != bitmaps.size()) {
LOG_ERR("%s: error: number of bitmaps (%zu) does not match number of markers (%zu)\n",
__func__, bitmaps.size(), parts.size() - 1);
return 1;
}
*output = std::move(cur);
return 0;
}
void add_text(const std::string & txt, bool parse_special) {
LOG_DBG("%s: %s\n", __func__, txt.c_str());
auto tokens = mtmd_tokenize_text_internal(vocab, txt,  false, parse_special);
add_text(tokens);
}
void add_text(const std::vector<llama_token> & tokens) {
if (tokens.empty()) {
return;
}
if (!cur.entries.empty() && cur.entries.back().type == MTMD_INPUT_CHUNK_TYPE_TEXT) {
cur.entries.back().tokens_text.insert(
cur.entries.back().tokens_text.end(),
tokens.begin(),
tokens.end());
} else {
mtmd_input_chunk chunk{
MTMD_INPUT_CHUNK_TYPE_TEXT,
tokens,
nullptr,
nullptr,
};
cur.entries.emplace_back(std::move(chunk));
}
}
int32_t add_media(const mtmd_bitmap * bitmap) {
if (!bitmap->is_audio) {
if (!ctx->ctx_v) {
LOG_ERR("%s: error: model does not support vision input\n", __func__);
return 2;
}
if (!ctx->img_beg.empty()) {
add_text(ctx->img_beg, true);
}
clip_image_u8_ptr img_u8(clip_image_u8_init());
img_u8->nx = bitmap->nx;
img_u8->ny = bitmap->ny;
img_u8->buf.resize(bitmap->data.size());
std::memcpy(img_u8->buf.data(), bitmap->data.data(), img_u8->nx * img_u8->ny * 3);
clip_image_f32_batch batch_f32;
bool ok = clip_image_preprocess(ctx->ctx_v, img_u8.get(), &batch_f32);
if (!ok) {
LOG_ERR("Unable to preprocess image\n");
return 2;
}
if (
ctx->slice_tmpl == MTMD_SLICE_TMPL_MINICPMV_2_5
|| ctx->slice_tmpl == MTMD_SLICE_TMPL_MINICPMV_2_6
|| ctx->slice_tmpl == MTMD_SLICE_TMPL_LLAMA4
) {
const int n_col = batch_f32.grid_x;
const int n_row = batch_f32.grid_y;
auto chunks = split_batch_to_chunk(std::move(batch_f32), bitmap->id);
GGML_ASSERT(chunks.size() > 0);
auto ov_chunk = std::move(chunks.front());
chunks.erase(chunks.begin());
if (ctx->ov_img_first) {
if (ctx->tok_ov_img_start != LLAMA_TOKEN_NULL) {
add_text({ctx->tok_ov_img_start});
}
cur.entries.emplace_back(std::move(ov_chunk));
if (ctx->tok_ov_img_end != LLAMA_TOKEN_NULL) {
add_text({ctx->tok_ov_img_end});
}
}
if (!chunks.empty()) {
GGML_ASSERT((int)chunks.size() == n_row * n_col);
if (ctx->tok_slices_start != LLAMA_TOKEN_NULL) {
add_text({ctx->tok_slices_start});
}
for (int y = 0; y < n_row; y++) {
for (int x = 0; x < n_col; x++) {
const bool is_last_in_row = (x == n_col - 1);
if (ctx->tok_sli_img_start != LLAMA_TOKEN_NULL) {
add_text({ctx->tok_sli_img_start});
}
cur.entries.emplace_back(std::move(chunks[y * n_col + x]));
if (ctx->tok_sli_img_end != LLAMA_TOKEN_NULL) {
add_text({ctx->tok_sli_img_end});
}
if (!is_last_in_row && ctx->tok_sli_img_mid != LLAMA_TOKEN_NULL) {
add_text({ctx->tok_sli_img_mid});
}
}
if ((y != n_row - 1 || ctx->tok_row_end_trail) && ctx->tok_row_end != LLAMA_TOKEN_NULL) {
add_text({ctx->tok_row_end});
}
}
if (ctx->tok_slices_end != LLAMA_TOKEN_NULL) {
add_text({ctx->tok_slices_end});
}
}
if (!ctx->ov_img_first) {
if (ctx->tok_ov_img_start != LLAMA_TOKEN_NULL) {
add_text({ctx->tok_ov_img_start});
}
cur.entries.emplace_back(std::move(ov_chunk));
if (ctx->tok_ov_img_end != LLAMA_TOKEN_NULL) {
add_text({ctx->tok_ov_img_end});
}
}
} else {
size_t n_tokens = 0;
for (const auto & entry : batch_f32.entries) {
n_tokens += clip_n_output_tokens(ctx->ctx_v, entry.get());
}
mtmd_image_tokens_ptr image_tokens(new mtmd_image_tokens);
if (ctx->use_mrope) {
image_tokens->nx = clip_n_output_tokens_x(ctx->ctx_v, batch_f32.entries[0].get());
image_tokens->ny = clip_n_output_tokens_y(ctx->ctx_v, batch_f32.entries[0].get());
image_tokens->use_mrope_pos = true;
} else {
image_tokens->nx = n_tokens;
image_tokens->ny = 1;
}
image_tokens->batch_f32 = std::move(batch_f32);
image_tokens->id = bitmap->id;
LOG_DBG("image_tokens->nx = %d\n", image_tokens->nx);
LOG_DBG("image_tokens->ny = %d\n", image_tokens->ny);
LOG_DBG("batch_f32 size = %d\n", (int)image_tokens->batch_f32.entries.size());
mtmd_input_chunk chunk{
MTMD_INPUT_CHUNK_TYPE_IMAGE,
{},
std::move(image_tokens),
nullptr,
};
cur.entries.emplace_back(std::move(chunk));
}
if (!ctx->img_end.empty()) {
add_text(ctx->img_end, true);
}
} else {
if (!ctx->ctx_a) {
LOG_ERR("%s: error: model does not support audio input\n", __func__);
return 2;
}
if (bitmap->data.size() == 0) {
LOG_ERR("%s: error: empty audio data\n", __func__);
return 2;
}
if (!ctx->aud_beg.empty()) {
add_text(ctx->aud_beg, true);
}
GGML_ASSERT(ctx->w_filters.n_mel);
std::vector<whisper_preprocessor::whisper_mel> mel_spec_chunks;
const float * samples = (const float *)bitmap->data.data();
size_t n_samples = bitmap->data.size() / sizeof(float);
bool ok = whisper_preprocessor::preprocess_audio(samples, n_samples, ctx->w_filters, mel_spec_chunks);
if (!ok) {
LOG_ERR("Unable to preprocess audio\n");
return 2;
}
for (auto & mel_spec : mel_spec_chunks) {
clip_image_f32_ptr mel_f32(clip_image_f32_init());
mel_f32->nx  = mel_spec.n_len;
mel_f32->ny  = mel_spec.n_mel;
mel_f32->buf = std::move(mel_spec.data);
size_t n_tokens = clip_n_output_tokens(ctx->ctx_a, mel_f32.get());
clip_image_f32_batch batch_f32;
batch_f32.is_audio = true;
batch_f32.entries.push_back(std::move(mel_f32));
mtmd_audio_tokens_ptr audio_tokens(new mtmd_audio_tokens);
audio_tokens->n_tokens = n_tokens;
audio_tokens->batch_f32 = std::move(batch_f32);
audio_tokens->id = bitmap->id;
LOG_DBG("audio_tokens->n_tokens = %d\n", audio_tokens->n_tokens);
mtmd_input_chunk chunk{
MTMD_INPUT_CHUNK_TYPE_AUDIO,
{},
nullptr,
std::move(audio_tokens),
};
cur.entries.emplace_back(std::move(chunk));
}
if (!ctx->aud_end.empty()) {
add_text(ctx->aud_end, true);
}
}
return 0;
}
std::vector<mtmd_input_chunk> split_batch_to_chunk(clip_image_f32_batch && batch_f32, const std::string & id) {
std::vector<mtmd_input_chunk> chunks;
for (auto & entry : batch_f32.entries) {
mtmd_image_tokens_ptr image_tokens(new mtmd_image_tokens);
image_tokens->nx = clip_n_output_tokens(ctx->ctx_v, entry.get());
image_tokens->ny = 1;
image_tokens->batch_f32.entries.push_back(std::move(entry));
image_tokens->id = id;
mtmd_input_chunk chunk{
MTMD_INPUT_CHUNK_TYPE_IMAGE,
{},
std::move(image_tokens),
nullptr,
};
chunks.emplace_back(std::move(chunk));
}
return chunks;
}
static std::vector<std::string> split_text(const std::string & input, const std::string & delimiter) {
std::vector<std::string> result;
if (input.empty()) {
return result;
}
size_t start = 0;
size_t pos = 0;
while ((pos = input.find(delimiter, start)) != std::string::npos) {
if (pos > start) {
result.push_back(input.substr(start, pos - start));
}
result.push_back(delimiter);
start = pos + delimiter.length();
}
if (start < input.length()) {
result.push_back(input.substr(start));
}
return result;
}
static std::vector<llama_token> mtmd_tokenize_text_internal(
const struct llama_vocab * vocab,
const std::string & text,
bool   add_special,
bool   parse_special) {
int n_tokens = text.length() + 2 * add_special;
std::vector<llama_token> result(n_tokens);
n_tokens = llama_tokenize(vocab, text.data(), text.length(), result.data(), result.size(), add_special, parse_special);
if (n_tokens < 0) {
result.resize(-n_tokens);
int check = llama_tokenize(vocab, text.data(), text.length(), result.data(), result.size(), add_special, parse_special);
GGML_ASSERT(check == -n_tokens);
} else {
result.resize(n_tokens);
}
return result;
}
};
int32_t mtmd_tokenize(mtmd_context * ctx,
mtmd_input_chunks * output,
const mtmd_input_text * text,
const mtmd_bitmap ** bitmaps,
size_t n_bitmaps) {
mtmd_tokenizer tokenizer(ctx, text, bitmaps, n_bitmaps);
return tokenizer.tokenize(output);
}
int32_t mtmd_encode_chunk(mtmd_context * ctx, const mtmd_input_chunk * chunk) {
if (chunk->type == MTMD_INPUT_CHUNK_TYPE_TEXT) {
LOG_WRN("mtmd_encode_chunk has no effect for text chunks\n");
return 0;
} else if (chunk->type == MTMD_INPUT_CHUNK_TYPE_IMAGE) {
if (!ctx->ctx_v) {
LOG_ERR("%s: model does not support vision input\n", __func__);
return 1;
}
return mtmd_encode(ctx, chunk->tokens_image.get());
} else if (chunk->type == MTMD_INPUT_CHUNK_TYPE_AUDIO) {
if (!ctx->ctx_a) {
LOG_ERR("%s: model does not support audio input\n", __func__);
return 1;
}
int n_mmproj_embd = ctx->n_embd_text;
ctx->image_embd_v.resize(chunk->tokens_audio->n_tokens * n_mmproj_embd);
bool ok = clip_image_batch_encode(
ctx->ctx_a,
ctx->n_threads,
&chunk->tokens_audio->batch_f32,
ctx->image_embd_v.data());
return ok ? 0 : 1;
}
LOG_ERR("%s: unknown chunk type %d\n", __func__, (int)chunk->type);
return 1;
}
int32_t mtmd_encode(mtmd_context * ctx, const mtmd_image_tokens * image_tokens) {
clip_ctx * ctx_clip = ctx->ctx_v;
if (!ctx_clip) {
LOG_ERR("%s: this API does not support non-vision input, please use mtmd_encode_chunk instead\n", __func__);
return 1;
}
int n_mmproj_embd = clip_n_mmproj_embd(ctx_clip);
ctx->image_embd_v.resize(image_tokens->n_tokens() * n_mmproj_embd);
bool ok = false;
if (clip_is_llava(ctx_clip) || clip_is_minicpmv(ctx_clip) || clip_is_glm(ctx_clip)) {
const auto & entries = image_tokens->batch_f32.entries;
for (size_t i = 0; i < entries.size(); i++) {
int n_tokens_per_image = clip_n_output_tokens(ctx_clip, entries[i].get());
ok = clip_image_encode(
ctx_clip,
ctx->n_threads,
entries[i].get(),
ctx->image_embd_v.data() + i*n_mmproj_embd*n_tokens_per_image);
}
} else {
ok = clip_image_batch_encode(
ctx_clip,
ctx->n_threads,
&image_tokens->batch_f32,
ctx->image_embd_v.data());
}
return ok ? 0 : 1;
}
float * mtmd_get_output_embd(mtmd_context * ctx) {
return ctx->image_embd_v.data();
}
bool mtmd_decode_use_non_causal(mtmd_context * ctx) {
if (ctx->ctx_v && clip_get_projector_type(ctx->ctx_v) == PROJECTOR_TYPE_GEMMA3) {
return true;
}
return false;
}
bool mtmd_decode_use_mrope(mtmd_context * ctx) {
return ctx->use_mrope;
}
bool mtmd_support_vision(mtmd_context * ctx) {
return ctx->ctx_v != nullptr;
}
bool mtmd_support_audio(mtmd_context * ctx) {
return ctx->ctx_a != nullptr;
}
int mtmd_get_audio_bitrate(mtmd_context * ctx) {
if (!ctx->ctx_a) {
return -1;
}
return 16000;
}
mtmd_bitmap * mtmd_bitmap_init(uint32_t nx,
uint32_t ny,
const unsigned char * data) {
mtmd_bitmap * bitmap = new mtmd_bitmap;
bitmap->nx = nx;
bitmap->ny = ny;
size_t data_size = (size_t)nx * ny * 3;
bitmap->data.resize(data_size);
std::memcpy(bitmap->data.data(), data, data_size);
return bitmap;
}
mtmd_bitmap * mtmd_bitmap_init_from_audio(size_t n_samples,
const float * data) {
mtmd_bitmap * bitmap = new mtmd_bitmap;
bitmap->nx = n_samples;
bitmap->ny = 1;
bitmap->is_audio = true;
size_t data_size = n_samples * sizeof(float);
bitmap->data.resize(data_size);
std::memcpy(bitmap->data.data(), data, data_size);
return bitmap;
}
uint32_t mtmd_bitmap_get_nx(const mtmd_bitmap * bitmap) {
return bitmap->nx;
}
uint32_t mtmd_bitmap_get_ny(const mtmd_bitmap * bitmap) {
return bitmap->ny;
}
const unsigned char * mtmd_bitmap_get_data(const mtmd_bitmap * bitmap) {
return bitmap->data.data();
}
size_t mtmd_bitmap_get_n_bytes(const mtmd_bitmap * bitmap) {
return bitmap->data.size();
}
bool mtmd_bitmap_is_audio(const mtmd_bitmap * bitmap) {
return bitmap->is_audio;
}
const char * mtmd_bitmap_get_id(const mtmd_bitmap * bitmap) {
return bitmap->id.c_str();
}
void mtmd_bitmap_set_id(mtmd_bitmap * bitmap, const char * id) {
if (id) {
bitmap->id = std::string(id);
} else {
bitmap->id.clear();
}
}
void mtmd_bitmap_free(mtmd_bitmap * bitmap) {
if (bitmap) {
delete bitmap;
}
}
mtmd_input_chunks * mtmd_input_chunks_init() {
return new mtmd_input_chunks;
}
size_t mtmd_input_chunks_size(const mtmd_input_chunks * chunks) {
return chunks->entries.size();
}
const mtmd_input_chunk * mtmd_input_chunks_get(const mtmd_input_chunks * chunks, size_t idx) {
if (idx >= chunks->entries.size()) {
return nullptr;
}
return &chunks->entries[idx];
}
void mtmd_input_chunks_free(mtmd_input_chunks * chunks) {
if (chunks) {
delete chunks;
}
}
enum mtmd_input_chunk_type mtmd_input_chunk_get_type(const mtmd_input_chunk * chunk) {
return chunk->type;
}
const llama_token * mtmd_input_chunk_get_tokens_text(const mtmd_input_chunk * chunk, size_t * n_tokens_output) {
if (chunk->type == MTMD_INPUT_CHUNK_TYPE_TEXT) {
*n_tokens_output = chunk->tokens_text.size();
return chunk->tokens_text.data();
}
*n_tokens_output = 0;
return nullptr;
}
const mtmd_image_tokens * mtmd_input_chunk_get_tokens_image(const mtmd_input_chunk * chunk) {
if (chunk->type == MTMD_INPUT_CHUNK_TYPE_IMAGE) {
return chunk->tokens_image.get();
}
return nullptr;
}
size_t mtmd_input_chunk_get_n_tokens(const mtmd_input_chunk * chunk) {
if (chunk->type == MTMD_INPUT_CHUNK_TYPE_TEXT) {
return chunk->tokens_text.size();
} else if (chunk->type == MTMD_INPUT_CHUNK_TYPE_IMAGE) {
return mtmd_image_tokens_get_n_tokens(chunk->tokens_image.get());
} else if (chunk->type == MTMD_INPUT_CHUNK_TYPE_AUDIO) {
return chunk->tokens_audio->n_tokens;
} else {
GGML_ABORT("invalid chunk type");
}
}
llama_pos mtmd_input_chunk_get_n_pos(const mtmd_input_chunk * chunk) {
if (chunk->type == MTMD_INPUT_CHUNK_TYPE_TEXT) {
return chunk->tokens_text.size();
} else if (chunk->type == MTMD_INPUT_CHUNK_TYPE_IMAGE) {
return mtmd_image_tokens_get_n_pos(chunk->tokens_image.get());
} else if (chunk->type == MTMD_INPUT_CHUNK_TYPE_AUDIO) {
return chunk->tokens_audio->n_tokens;
} else {
GGML_ABORT("invalid chunk type");
}
}
const char * mtmd_input_chunk_get_id(const mtmd_input_chunk * chunk) {
if (chunk->type == MTMD_INPUT_CHUNK_TYPE_IMAGE) {
return chunk->tokens_image->id.c_str();
} else if (chunk->type == MTMD_INPUT_CHUNK_TYPE_AUDIO) {
return chunk->tokens_audio->id.c_str();
}
return nullptr;
}
mtmd_input_chunk * mtmd_input_chunk_copy(const mtmd_input_chunk * chunk) {
mtmd_input_chunk * copy = new mtmd_input_chunk{
chunk->type,
chunk->tokens_text,
nullptr,
nullptr,
};
if (chunk->tokens_image) {
copy->tokens_image = mtmd_image_tokens_ptr(new mtmd_image_tokens());
*copy->tokens_image = chunk->tokens_image->clone();
}
if (chunk->tokens_audio) {
copy->tokens_audio = mtmd_audio_tokens_ptr(new mtmd_audio_tokens());
*copy->tokens_audio = chunk->tokens_audio->clone();
}
return copy;
}
void mtmd_input_chunk_free(mtmd_input_chunk * chunk) {
if (chunk) {
delete chunk;
}
}
size_t mtmd_image_tokens_get_n_tokens(const mtmd_image_tokens * image_tokens) {
return image_tokens->n_tokens();
}
size_t mtmd_image_tokens_get_nx(const mtmd_image_tokens * image_tokens) {
return image_tokens->nx;
}
size_t mtmd_image_tokens_get_ny(const mtmd_image_tokens * image_tokens) {
return image_tokens->ny;
}
const char * mtmd_image_tokens_get_id(const mtmd_image_tokens * image_tokens) {
return image_tokens->id.c_str();
}
llama_pos mtmd_image_tokens_get_n_pos(const mtmd_image_tokens * image_tokens) {
if (image_tokens->use_mrope_pos) {
return 1;
}
return image_tokens->n_tokens();
}
mtmd_input_chunks * mtmd_test_create_input_chunks() {
mtmd_input_chunks * chunks = mtmd_input_chunks_init();
if (!chunks) {
return nullptr;
}
std::vector<llama_token> tokens_text = { 1, 2, 3, 4, 5 };
mtmd_input_chunk chunk_text{
MTMD_INPUT_CHUNK_TYPE_TEXT,
std::move(tokens_text),
nullptr,
nullptr,
};
chunks->entries.emplace_back(std::move(chunk_text));
mtmd_image_tokens_ptr image_tokens(new mtmd_image_tokens);
image_tokens->nx = 4;
image_tokens->ny = 4;
image_tokens->batch_f32.entries.resize(16);
image_tokens->id = "image_1";
mtmd_input_chunk chunk_image{
MTMD_INPUT_CHUNK_TYPE_IMAGE,
{},
std::move(image_tokens),
nullptr,
};
chunks->entries.emplace_back(std::move(chunk_image));
return chunks;
}