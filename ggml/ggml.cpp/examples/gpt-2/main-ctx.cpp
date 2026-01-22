#include "ggml.h"
#include "ggml-cpu.h"
#include "common.h"
#include "common-ggml.h"
#include <cassert>
#include <cmath>
#include <cstdio>
#include <cstring>
#include <fstream>
#include <map>
#include <string>
#include <vector>
#if defined(_MSC_VER)
#pragma warning(disable: 4244 4267)
#endif
struct gpt2_hparams {
int32_t n_vocab = 50257;
int32_t n_ctx = 1024;
int32_t n_embd = 768;
int32_t n_head = 12;
int32_t n_layer = 12;
int32_t ftype = 1;
float eps = 1e-5f;
};
struct gpt2_layer {
struct ggml_tensor * ln_1_g;
struct ggml_tensor * ln_1_b;
struct ggml_tensor * ln_2_g;
struct ggml_tensor * ln_2_b;
struct ggml_tensor * c_attn_attn_w;
struct ggml_tensor * c_attn_attn_b;
struct ggml_tensor * c_attn_proj_w;
struct ggml_tensor * c_attn_proj_b;
struct ggml_tensor * c_mlp_fc_w;
struct ggml_tensor * c_mlp_fc_b;
struct ggml_tensor * c_mlp_proj_w;
struct ggml_tensor * c_mlp_proj_b;
};
struct gpt2_model {
gpt2_hparams hparams;
struct ggml_tensor * ln_f_g;
struct ggml_tensor * ln_f_b;
struct ggml_tensor * wte;
struct ggml_tensor * wpe;
struct ggml_tensor * lm_head;
std::vector<gpt2_layer> layers;
struct ggml_tensor * memory_k;
struct ggml_tensor * memory_v;
struct ggml_context * ctx_w;
std::map<std::string, struct ggml_tensor *> tensors;
};
bool gpt2_model_load(const std::string & fname, gpt2_model & model, gpt_vocab & vocab) {
printf("%s: loading model from '%s'\n", __func__, fname.c_str());
auto fin = std::ifstream(fname, std::ios::binary);
if (!fin) {
fprintf(stderr, "%s: failed to open '%s'\n", __func__, fname.c_str());
return false;
}
{
uint32_t magic;
fin.read((char *) &magic, sizeof(magic));
if (magic != GGML_FILE_MAGIC) {
fprintf(stderr, "%s: invalid model file '%s' (bad magic)\n", __func__, fname.c_str());
return false;
}
}
{
auto & hparams = model.hparams;
fin.read((char *) &hparams.n_vocab, sizeof(hparams.n_vocab));
fin.read((char *) &hparams.n_ctx, sizeof(hparams.n_ctx));
fin.read((char *) &hparams.n_embd, sizeof(hparams.n_embd));
fin.read((char *) &hparams.n_head, sizeof(hparams.n_head));
fin.read((char *) &hparams.n_layer, sizeof(hparams.n_layer));
fin.read((char *) &hparams.ftype, sizeof(hparams.ftype));
const int32_t qntvr = hparams.ftype / GGML_QNT_VERSION_FACTOR;
printf("%s: n_vocab = %d\n", __func__, hparams.n_vocab);
printf("%s: n_ctx   = %d\n", __func__, hparams.n_ctx);
printf("%s: n_embd  = %d\n", __func__, hparams.n_embd);
printf("%s: n_head  = %d\n", __func__, hparams.n_head);
printf("%s: n_layer = %d\n", __func__, hparams.n_layer);
printf("%s: ftype   = %d\n", __func__, hparams.ftype);
printf("%s: qntvr   = %d\n", __func__, qntvr);
hparams.ftype %= GGML_QNT_VERSION_FACTOR;
}
{
int32_t n_vocab = 0;
fin.read((char *) &n_vocab, sizeof(n_vocab));
if (n_vocab != model.hparams.n_vocab) {
fprintf(stderr, "%s: invalid model file '%s' (bad vocab size %d != %d)\n",
__func__, fname.c_str(), n_vocab, model.hparams.n_vocab);
return false;
}
std::string word;
std::vector<char> buf(128);
for (int i = 0; i < n_vocab; i++) {
uint32_t len;
fin.read((char *) &len, sizeof(len));
buf.resize(len);
fin.read((char *) buf.data(), len);
word.assign(buf.data(), len);
vocab.token_to_id[word] = i;
vocab.id_to_token[i] = word;
}
}
ggml_type wtype = ggml_ftype_to_ggml_type((ggml_ftype) (model.hparams.ftype));
if (wtype == GGML_TYPE_COUNT) {
fprintf(stderr, "%s: invalid model file '%s' (bad ftype value %d)\n",
__func__, fname.c_str(), model.hparams.ftype);
return false;
}
auto & ctx = model.ctx_w;
size_t ctx_size = 0;
{
const auto & hparams = model.hparams;
const int n_embd = hparams.n_embd;
const int n_layer = hparams.n_layer;
const int n_ctx = hparams.n_ctx;
const int n_vocab = hparams.n_vocab;
ctx_size += ggml_row_size(GGML_TYPE_F32, n_embd);
ctx_size += ggml_row_size(GGML_TYPE_F32, n_embd);
ctx_size += ggml_row_size(wtype, n_vocab*n_embd);
ctx_size += ggml_row_size(GGML_TYPE_F32, n_ctx*n_embd);
ctx_size += ggml_row_size(wtype, n_vocab*n_embd);
ctx_size += n_layer*(ggml_row_size(GGML_TYPE_F32, n_embd));
ctx_size += n_layer*(ggml_row_size(GGML_TYPE_F32, n_embd));
ctx_size += n_layer*(ggml_row_size(GGML_TYPE_F32, n_embd));
ctx_size += n_layer*(ggml_row_size(GGML_TYPE_F32, n_embd));
ctx_size += n_layer*(ggml_row_size(wtype, 3*n_embd*n_embd));
ctx_size += n_layer*(ggml_row_size(GGML_TYPE_F32, 3*n_embd));
ctx_size += n_layer*(ggml_row_size(wtype, n_embd*n_embd));
ctx_size += n_layer*(ggml_row_size(GGML_TYPE_F32, n_embd));
ctx_size += n_layer*(ggml_row_size(wtype, 4*n_embd*n_embd));
ctx_size += n_layer*(ggml_row_size(GGML_TYPE_F32, 4*n_embd));
ctx_size += n_layer*(ggml_row_size(wtype, 4*n_embd*n_embd));
ctx_size += n_layer*(ggml_row_size(GGML_TYPE_F32, 4*n_embd));
ctx_size += n_ctx*n_layer*ggml_row_size(GGML_TYPE_F32, n_embd);
ctx_size += n_ctx*n_layer*ggml_row_size(GGML_TYPE_F32, n_embd);
ctx_size += (6 + 12*n_layer)*512;
printf("%s: ggml tensor size = %d bytes\n", __func__, (int) sizeof(ggml_tensor));
printf("%s: ggml ctx size = %6.2f MB\n", __func__, ctx_size/(1024.0*1024.0));
}
{
struct ggml_init_params params = {
ctx_size,
NULL,
false,
};
model.ctx_w = ggml_init(params);
if (!model.ctx_w) {
fprintf(stderr, "%s: ggml_init() failed\n", __func__);
return false;
}
}
{
const auto & hparams = model.hparams;
const int n_embd = hparams.n_embd;
const int n_layer = hparams.n_layer;
const int n_ctx = hparams.n_ctx;
const int n_vocab = hparams.n_vocab;
model.layers.resize(n_layer);
model.ln_f_g = ggml_new_tensor_1d(ctx, GGML_TYPE_F32, n_embd);
model.ln_f_b = ggml_new_tensor_1d(ctx, GGML_TYPE_F32, n_embd);
model.wte = ggml_new_tensor_2d(ctx, wtype, n_embd, n_vocab);
model.wpe = ggml_new_tensor_2d(ctx, GGML_TYPE_F32, n_embd, n_ctx);
model.lm_head = ggml_new_tensor_2d(ctx, wtype, n_embd, n_vocab);
model.tensors["model/ln_f/g"] = model.ln_f_g;
model.tensors["model/ln_f/b"] = model.ln_f_b;
model.tensors["model/wte"] = model.wte;
model.tensors["model/wpe"] = model.wpe;
model.tensors["model/lm_head"] = model.lm_head;
for (int i = 0; i < n_layer; ++i) {
auto & layer = model.layers[i];
layer.ln_1_g = ggml_new_tensor_1d(ctx, GGML_TYPE_F32, n_embd);
layer.ln_1_b = ggml_new_tensor_1d(ctx, GGML_TYPE_F32, n_embd);
layer.ln_2_g = ggml_new_tensor_1d(ctx, GGML_TYPE_F32, n_embd);
layer.ln_2_b = ggml_new_tensor_1d(ctx, GGML_TYPE_F32, n_embd);
layer.c_attn_attn_w = ggml_new_tensor_2d(ctx, wtype, n_embd, 3*n_embd);
layer.c_attn_attn_b = ggml_new_tensor_1d(ctx, GGML_TYPE_F32, 3*n_embd);
layer.c_attn_proj_w = ggml_new_tensor_2d(ctx, wtype, n_embd, n_embd);
layer.c_attn_proj_b = ggml_new_tensor_1d(ctx, GGML_TYPE_F32, n_embd);
layer.c_mlp_fc_w = ggml_new_tensor_2d(ctx, wtype, n_embd, 4*n_embd);
layer.c_mlp_fc_b = ggml_new_tensor_1d(ctx, GGML_TYPE_F32, 4*n_embd);
layer.c_mlp_proj_w = ggml_new_tensor_2d(ctx, wtype, 4*n_embd, n_embd);
layer.c_mlp_proj_b = ggml_new_tensor_1d(ctx, GGML_TYPE_F32, n_embd);
model.tensors["model/h" + std::to_string(i) + "/ln_1/g"] = layer.ln_1_g;
model.tensors["model/h" + std::to_string(i) + "/ln_1/b"] = layer.ln_1_b;
model.tensors["model/h" + std::to_string(i) + "/ln_2/g"] = layer.ln_2_g;
model.tensors["model/h" + std::to_string(i) + "/ln_2/b"] = layer.ln_2_b;
model.tensors["model/h" + std::to_string(i) + "/attn/c_attn/w"] = layer.c_attn_attn_w;
model.tensors["model/h" + std::to_string(i) + "/attn/c_attn/b"] = layer.c_attn_attn_b;
model.tensors["model/h" + std::to_string(i) + "/attn/c_proj/w"] = layer.c_attn_proj_w;
model.tensors["model/h" + std::to_string(i) + "/attn/c_proj/b"] = layer.c_attn_proj_b;
model.tensors["model/h" + std::to_string(i) + "/mlp/c_fc/w"] = layer.c_mlp_fc_w;
model.tensors["model/h" + std::to_string(i) + "/mlp/c_fc/b"] = layer.c_mlp_fc_b;
model.tensors["model/h" + std::to_string(i) + "/mlp/c_proj/w"] = layer.c_mlp_proj_w;
model.tensors["model/h" + std::to_string(i) + "/mlp/c_proj/b"] = layer.c_mlp_proj_b;
}
}
{
const auto & hparams = model.hparams;
const int n_embd = hparams.n_embd;
const int n_layer = hparams.n_layer;
const int n_ctx = hparams.n_ctx;
const int n_mem = n_layer*n_ctx;
const int n_elements = n_embd*n_mem;
model.memory_k = ggml_new_tensor_1d(ctx, GGML_TYPE_F32, n_elements);
model.memory_v = ggml_new_tensor_1d(ctx, GGML_TYPE_F32, n_elements);
const size_t memory_size = ggml_nbytes(model.memory_k) + ggml_nbytes(model.memory_v);
printf("%s: memory size = %8.2f MB, n_mem = %d\n", __func__, memory_size/1024.0/1024.0, n_mem);
}
{
size_t total_size = 0;
bool has_lm_head = false;
while (true) {
int32_t n_dims;
int32_t length;
int32_t ttype;
fin.read(reinterpret_cast<char *>(&n_dims), sizeof(n_dims));
fin.read(reinterpret_cast<char *>(&length), sizeof(length));
fin.read(reinterpret_cast<char *>(&ttype), sizeof(ttype));
if (fin.eof()) {
break;
}
int32_t nelements = 1;
int32_t ne[2] = { 1, 1 };
for (int i = 0; i < n_dims; ++i) {
fin.read(reinterpret_cast<char *>(&ne[i]), sizeof(ne[i]));
nelements *= ne[i];
}
std::string name(length, 0);
fin.read(&name[0], length);
if (model.tensors.find(name) == model.tensors.end()) {
fprintf(stderr, "%s: unknown tensor '%s' in model file\n", __func__, name.c_str());
return false;
}
auto tensor = model.tensors[name];
if (ggml_nelements(tensor) != nelements) {
fprintf(stderr, "%s: tensor '%s' has wrong size in model file\n", __func__, name.c_str());
return false;
}
if (tensor->ne[0] != ne[0] || tensor->ne[1] != ne[1]) {
fprintf(stderr, "%s: tensor '%s' has wrong shape in model file: got [%d, %d], expected [%d, %d]\n",
__func__, name.c_str(), (int) tensor->ne[0], (int) tensor->ne[1], ne[0], ne[1]);
return false;
}
if (0) {
printf("%24s - [%5d, %5d], type = %6s, %6.2f MB, %9zu bytes\n", name.c_str(), ne[0], ne[1], ggml_type_name(ggml_type(ttype)), ggml_nbytes(tensor)/1024.0/1024.0, ggml_nbytes(tensor));
}
const size_t bpe = ggml_type_size(ggml_type(ttype));
if ((nelements*bpe)/ggml_blck_size(tensor->type) != ggml_nbytes(tensor)) {
fprintf(stderr, "%s: tensor '%s' has wrong size in model file: got %zu, expected %zu\n",
__func__, name.c_str(), ggml_nbytes(tensor), nelements*bpe);
return false;
}
fin.read(reinterpret_cast<char *>(tensor->data), ggml_nbytes(tensor));
if (name == "model/wte" && has_lm_head == false) {
memcpy(model.lm_head->data, tensor->data, ggml_nbytes(tensor));
}
if (name == "model/lm_head") {
has_lm_head = true;
}
total_size += ggml_nbytes(tensor);
}
printf("%s: model size  = %8.2f MB\n", __func__, total_size/1024.0/1024.0);
}
fin.close();
return true;
}
bool gpt2_eval(
const gpt2_model & model,
const int n_threads,
const int n_past,
const std::vector<gpt_vocab::id> & embd_inp,
std::vector<float> & embd_w,
size_t & mem_per_token) {
const int N = embd_inp.size();
const auto & hparams = model.hparams;
const int n_embd = hparams.n_embd;
const int n_layer = hparams.n_layer;
const int n_ctx = hparams.n_ctx;
const int n_head = hparams.n_head;
const int n_vocab = hparams.n_vocab;
static size_t buf_size = 256u*1024*1024;
static void * buf = malloc(buf_size);
if (mem_per_token > 0 && mem_per_token*N > buf_size) {
const size_t buf_size_new = 1.1*(mem_per_token*N);
buf_size = buf_size_new;
buf = realloc(buf, buf_size);
if (buf == nullptr) {
fprintf(stderr, "%s: failed to allocate %zu bytes\n", __func__, buf_size);
return false;
}
}
struct ggml_init_params params = {
buf_size,
buf,
false,
};
struct ggml_context * ctx0 = ggml_init(params);
struct ggml_cgraph * gf = ggml_new_graph(ctx0);
struct ggml_tensor * embd = ggml_new_tensor_1d(ctx0, GGML_TYPE_I32, N);
memcpy(embd->data, embd_inp.data(), N*ggml_element_size(embd));
struct ggml_tensor * position = ggml_new_tensor_1d(ctx0, GGML_TYPE_I32, N);
for (int i = 0; i < N; ++i) {
((int32_t *) position->data)[i] = n_past + i;
}
struct ggml_tensor * inpL =
ggml_add(ctx0,
ggml_get_rows(ctx0, model.wte, embd),
ggml_get_rows(ctx0, model.wpe, position));
for (int il = 0; il < n_layer; ++il) {
struct ggml_tensor * cur;
{
cur = ggml_norm(ctx0, inpL, hparams.eps);
cur = ggml_add(ctx0,
ggml_mul(ctx0,
ggml_repeat(ctx0, model.layers[il].ln_1_g, cur),
cur),
ggml_repeat(ctx0, model.layers[il].ln_1_b, cur));
}
{
cur = ggml_mul_mat(ctx0,
model.layers[il].c_attn_attn_w,
cur);
cur = ggml_add(ctx0,
ggml_repeat(ctx0, model.layers[il].c_attn_attn_b, cur),
cur);
}
{
struct ggml_tensor * Qcur = ggml_view_2d(ctx0, cur, n_embd, N, cur->nb[1], 0*sizeof(float)*n_embd);
struct ggml_tensor * Kcur = ggml_view_2d(ctx0, cur, n_embd, N, cur->nb[1], 1*sizeof(float)*n_embd);
struct ggml_tensor * Vcur = ggml_view_2d(ctx0, cur, n_embd, N, cur->nb[1], 2*sizeof(float)*n_embd);
if (N >= 1) {
struct ggml_tensor * k = ggml_view_1d(ctx0, model.memory_k, N*n_embd, (ggml_element_size(model.memory_k)*n_embd)*(il*n_ctx + n_past));
struct ggml_tensor * v = ggml_view_1d(ctx0, model.memory_v, N*n_embd, (ggml_element_size(model.memory_v)*n_embd)*(il*n_ctx + n_past));
ggml_build_forward_expand(gf, ggml_cpy(ctx0, Kcur, k));
ggml_build_forward_expand(gf, ggml_cpy(ctx0, Vcur, v));
}
struct ggml_tensor * Q =
ggml_permute(ctx0,
ggml_cpy(ctx0,
Qcur,
ggml_new_tensor_3d(ctx0, GGML_TYPE_F32, n_embd/n_head, n_head, N)),
0, 2, 1, 3);
struct ggml_tensor * K =
ggml_permute(ctx0,
ggml_reshape_3d(ctx0,
ggml_view_1d(ctx0, model.memory_k, (n_past + N)*n_embd, il*n_ctx*ggml_element_size(model.memory_k)*n_embd),
n_embd/n_head, n_head, n_past + N),
0, 2, 1, 3);
struct ggml_tensor * KQ = ggml_mul_mat(ctx0, K, Q);
struct ggml_tensor * KQ_scaled = ggml_scale_inplace(ctx0, KQ, 1.0f/sqrt(float(n_embd)/n_head));
struct ggml_tensor * KQ_masked = ggml_diag_mask_inf_inplace(ctx0, KQ_scaled, n_past);
struct ggml_tensor * KQ_soft_max = ggml_soft_max_inplace(ctx0, KQ_masked);
struct ggml_tensor * V_trans =
ggml_cpy(ctx0,
ggml_permute(ctx0,
ggml_reshape_3d(ctx0,
ggml_view_1d(ctx0, model.memory_v, (n_past + N)*n_embd, il*n_ctx*ggml_element_size(model.memory_v)*n_embd),
n_embd/n_head, n_head, n_past + N),
1, 2, 0, 3),
ggml_new_tensor_3d(ctx0, model.memory_v->type, n_past + N, n_embd/n_head, n_head));
struct ggml_tensor * KQV = ggml_mul_mat(ctx0, V_trans, KQ_soft_max);
struct ggml_tensor * KQV_merged = ggml_permute(ctx0, KQV, 0, 2, 1, 3);
cur = ggml_cpy(ctx0,
KQV_merged,
ggml_new_tensor_2d(ctx0, GGML_TYPE_F32, n_embd, N));
}
{
cur = ggml_mul_mat(ctx0,
model.layers[il].c_attn_proj_w,
cur);
cur = ggml_add(ctx0,
ggml_repeat(ctx0, model.layers[il].c_attn_proj_b, cur),
cur);
}
cur = ggml_add(ctx0, cur, inpL);
struct ggml_tensor * inpFF = cur;
{
{
cur = ggml_norm(ctx0, inpFF, hparams.eps);
cur = ggml_add(ctx0,
ggml_mul(ctx0,
ggml_repeat(ctx0, model.layers[il].ln_2_g, cur),
cur),
ggml_repeat(ctx0, model.layers[il].ln_2_b, cur));
}
cur = ggml_mul_mat(ctx0,
model.layers[il].c_mlp_fc_w,
cur);
cur = ggml_add(ctx0,
ggml_repeat(ctx0, model.layers[il].c_mlp_fc_b, cur),
cur);
cur = ggml_gelu(ctx0, cur);
cur = ggml_mul_mat(ctx0,
model.layers[il].c_mlp_proj_w,
cur);
cur = ggml_add(ctx0,
ggml_repeat(ctx0, model.layers[il].c_mlp_proj_b, cur),
cur);
}
inpL = ggml_add(ctx0, cur, inpFF);
}
{
inpL = ggml_norm(ctx0, inpL, hparams.eps);
inpL = ggml_add(ctx0,
ggml_mul(ctx0,
ggml_repeat(ctx0, model.ln_f_g, inpL),
inpL),
ggml_repeat(ctx0, model.ln_f_b, inpL));
}
inpL = ggml_mul_mat(ctx0, model.lm_head, inpL);
ggml_build_forward_expand(gf, inpL);
ggml_graph_compute_with_ctx(ctx0, gf, n_threads);
embd_w.resize(n_vocab);
memcpy(embd_w.data(), (float *) ggml_get_data(inpL) + (n_vocab*(N-1)), sizeof(float)*n_vocab);
if (mem_per_token == 0) {
mem_per_token = ggml_used_mem(ctx0)/N;
}
ggml_free(ctx0);
return true;
}
int main(int argc, char ** argv) {
ggml_time_init();
const int64_t t_main_start_us = ggml_time_us();
gpt_params params;
params.model = "models/gpt-2-117M/ggml-model.bin";
if (gpt_params_parse(argc, argv, params) == false) {
return 1;
}
if (params.seed < 0) {
params.seed = time(NULL);
}
printf("%s: seed = %d\n", __func__, params.seed);
std::mt19937 rng(params.seed);
if (params.prompt.empty()) {
params.prompt = gpt_random_prompt(rng);
}
int64_t t_load_us = 0;
gpt_vocab vocab;
gpt2_model model;
{
const int64_t t_start_us = ggml_time_us();
if (!gpt2_model_load(params.model, model, vocab)) {
fprintf(stderr, "%s: failed to load model from '%s'\n", __func__, params.model.c_str());
return 1;
}
t_load_us = ggml_time_us() - t_start_us;
test_gpt_tokenizer(vocab, params.token_test);
}
int n_past = 0;
int64_t t_sample_us = 0;
int64_t t_predict_us = 0;
std::vector<float> logits;
std::vector<gpt_vocab::id> embd_inp = ::gpt_tokenize(vocab, params.prompt);
params.n_predict = std::min(params.n_predict, model.hparams.n_ctx - (int) embd_inp.size());
printf("%s: prompt: '%s'\n", __func__, params.prompt.c_str());
printf("%s: number of tokens in prompt = %zu, first 8 tokens: ", __func__, embd_inp.size());
for (int i = 0; i < std::min(8, (int) embd_inp.size()); i++) {
printf("%d ", embd_inp[i]);
}
printf("\n\n");
std::vector<gpt_vocab::id> embd;
size_t mem_per_token = 0;
gpt2_eval(model, params.n_threads, 0, { 0, 1, 2, 3 }, logits, mem_per_token);
for (size_t i = embd.size(); i < embd_inp.size() + params.n_predict; i++) {
if (embd.size() > 0) {
const int64_t t_start_us = ggml_time_us();
if (!gpt2_eval(model, params.n_threads, n_past, embd, logits, mem_per_token)) {
printf("Failed to predict\n");
return 1;
}
t_predict_us += ggml_time_us() - t_start_us;
}
n_past += embd.size();
embd.clear();
if (i >= embd_inp.size()) {
const int top_k = params.top_k;
const float top_p = params.top_p;
const float temp = params.temp;
const int n_vocab = model.hparams.n_vocab;
gpt_vocab::id id = 0;
{
const int64_t t_start_sample_us = ggml_time_us();
id = gpt_sample_top_k_top_p(vocab, logits.data() + (logits.size() - n_vocab), top_k, top_p, temp, rng);
t_sample_us += ggml_time_us() - t_start_sample_us;
}
embd.push_back(id);
} else {
for (size_t k = i; k < embd_inp.size(); k++) {
embd.push_back(embd_inp[k]);
if (int32_t(embd.size()) >= params.n_batch) {
break;
}
}
i += embd.size() - 1;
}
for (auto id : embd) {
printf("%s", vocab.id_to_token[id].c_str());
}
fflush(stdout);
if (embd.back() == 50256) {
break;
}
}
{
const int64_t t_main_end_us = ggml_time_us();
printf("\n\n");
printf("%s: mem per token = %8zu bytes\n", __func__, mem_per_token);
printf("%s:     load time = %8.2f ms\n", __func__, t_load_us/1000.0f);
printf("%s:   sample time = %8.2f ms\n", __func__, t_sample_us/1000.0f);
printf("%s:  predict time = %8.2f ms / %.2f ms per token\n", __func__, t_predict_us/1000.0f, t_predict_us/1000.0f/n_past);
printf("%s:    total time = %8.2f ms\n", __func__, (t_main_end_us - t_main_start_us)/1000.0f);
}
ggml_free(model.ctx_w);
return 0;
}