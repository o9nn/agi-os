#include "ggml.h"
#include "ggml-cpu.h"
#include "ggml-alloc.h"
#include "ggml-backend.h"
#ifdef GGML_USE_CUDA
#include "ggml-cuda.h"
#endif
#ifdef GGML_USE_METAL
#include "ggml-metal.h"
#endif
#ifdef GGML_USE_BLAS
#include "ggml-blas.h"
#endif
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
#define GPT2_MAX_NODES 4096
static void ggml_log_callback_default(ggml_log_level level, const char * text, void * user_data) {
(void) level;
(void) user_data;
fputs(text, stderr);
fflush(stderr);
}
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
std::vector<ggml_backend_t> backends;
std::vector<ggml_backend_buffer_t> buffers_w;
ggml_backend_buffer_t buffer_kv;
ggml_backend_buffer_t buffer_input;
std::map<std::string, struct ggml_tensor *> tensors;
struct ggml_tensor * embd;
struct ggml_tensor * position;
};
void init_backends(gpt2_model & model, const gpt_params & params) {
ggml_backend_t gpu_backend = NULL;
ggml_log_set(ggml_log_callback_default, nullptr);
#ifdef GGML_USE_CUDA
if (params.n_gpu_layers > 0) {
fprintf(stderr, "%s: using CUDA backend\n", __func__);
gpu_backend = ggml_backend_cuda_init(0);
if (!gpu_backend) {
fprintf(stderr, "%s: ggml_backend_cuda_init() failed\n", __func__);
}
}
#endif
#ifdef GGML_USE_METAL
if (params.n_gpu_layers > 0) {
fprintf(stderr, "%s: using Metal backend\n", __func__);
gpu_backend = ggml_backend_metal_init();
if (!gpu_backend) {
fprintf(stderr, "%s: ggml_backend_metal_init() failed\n", __func__);
}
}
#endif
if (gpu_backend) {
model.backends.push_back(gpu_backend);
}
#ifdef GGML_USE_BLAS
ggml_backend_t blas_backend = ggml_backend_blas_init();
if (!blas_backend) {
fprintf(stderr, "%s: failed to initialize BLAS backend\n", __func__);
} else {
ggml_backend_blas_set_n_threads(blas_backend, params.n_threads);
model.backends.push_back(blas_backend);
}
#endif
ggml_backend_t cpu_backend = ggml_backend_cpu_init();
ggml_backend_cpu_set_n_threads(cpu_backend, params.n_threads);
model.backends.push_back(cpu_backend);
}
bool gpt2_model_load(const std::string & fname, gpt2_model & model, gpt_vocab & vocab, const gpt_params & params) {
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
{
size_t n_tensors = 3 + 2 + 6 + 12*model.hparams.n_layer;
struct ggml_init_params params = {
ggml_tensor_overhead() * n_tensors,
NULL,
true,
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
init_backends(model, params);
ggml_backend_t backend_gpu = model.backends.front();
ggml_backend_t backend_cpu = model.backends.back();
std::map<std::string, ggml_backend_t> tensor_backends;
{
const int i_gpu_first_layer = model.hparams.n_layer - params.n_gpu_layers;
for (auto it : model.tensors) {
const std::string & name = it.first;
if (name == "model/wte" || name == "model/wpe") {
if (params.n_gpu_layers > model.hparams.n_layer) {
tensor_backends[name] = backend_gpu;
} else {
tensor_backends[name] = backend_cpu;
}
}
if (name == "model/ln_f/g" || name == "model/ln_f/b" || name == "model/lm_head") {
if (params.n_gpu_layers > 0) {
tensor_backends[name] = backend_gpu;
} else {
tensor_backends[name] = backend_cpu;
}
}
if (name.substr(0, 7) == "model/h") {
int layer = std::stoi(name.substr(7, 2));
if (layer >= i_gpu_first_layer) {
tensor_backends[name] = backend_gpu;
} else {
tensor_backends[name] = backend_cpu;
}
}
}
}
std::map<ggml_backend_t, ggml_tallocr> backend_buffers;
for (auto backend : model.backends) {
size_t size = 0;
for (auto it : model.tensors) {
if (tensor_backends[it.first] == backend) {
size += ggml_nbytes(it.second) + 512;
}
}
if (size > 0) {
printf("%s: %8s buffer size = %8.2f MB\n", __func__, ggml_backend_name(backend), size/1024.0/1024.0);
ggml_backend_buffer_t buffer = ggml_backend_alloc_buffer(backend, size);
ggml_backend_buffer_set_usage(buffer, GGML_BACKEND_BUFFER_USAGE_WEIGHTS);
model.buffers_w.push_back(buffer);
auto alloc = ggml_tallocr_new(buffer);
backend_buffers.insert(std::make_pair(backend, std::move(alloc)));
} else {
model.buffers_w.push_back(NULL);
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
ggml_set_name(model.memory_k, "model/memory_k");
ggml_set_name(model.memory_v, "model/memory_v");
const size_t memory_size = ggml_nbytes(model.memory_k) + ggml_nbytes(model.memory_v);
printf("%s: memory size = %8.2f MB, n_mem = %d\n", __func__, memory_size/1024.0/1024.0, n_mem);
ggml_backend_t backend_kv = params.n_gpu_layers >= hparams.n_layer/2 ? backend_gpu : backend_cpu;
printf("%s: backend_kv = %s\n", __func__, ggml_backend_name(backend_kv));
model.buffer_kv = ggml_backend_alloc_buffer(backend_kv, memory_size + 512*2);
{
ggml_tallocr alloc = ggml_tallocr_new(model.buffer_kv);
ggml_tallocr_alloc(&alloc, model.memory_k);
ggml_tallocr_alloc(&alloc, model.memory_v);
}
}
{
size_t total_size = 0;
bool has_lm_head = false;
std::vector<char> read_buf;
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
ggml_set_name(tensor, name.c_str());
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
ggml_backend_t backend = tensor_backends[name];
ggml_tallocr * alloc = &backend_buffers.find(backend)->second;
ggml_tallocr_alloc(alloc, tensor);
if (ggml_backend_is_cpu(backend)
#ifdef GGML_USE_METAL
|| ggml_backend_is_metal(backend)
#endif
) {
fin.read(reinterpret_cast<char *>(tensor->data), ggml_nbytes(tensor));
} else {
read_buf.resize(ggml_nbytes(tensor));
fin.read(read_buf.data(), ggml_nbytes(tensor));
ggml_backend_tensor_set(tensor, read_buf.data(), 0, ggml_nbytes(tensor));
}
if (name == "model/wte" && has_lm_head == false) {
ggml_tallocr * alloc_head = &backend_buffers.find(tensor_backends["model/lm_head"])->second;
ggml_tallocr_alloc(alloc_head, model.lm_head);
ggml_backend_tensor_copy(tensor, model.lm_head);
total_size += ggml_nbytes(model.lm_head);
}
if (name == "model/lm_head") {
has_lm_head = true;
}
total_size += ggml_nbytes(tensor);
}
printf("%s: model size  = %8.2f MB\n", __func__, total_size/1024.0/1024.0);
}
fin.close();
{
model.embd = ggml_new_tensor_1d(ctx, GGML_TYPE_I32, model.hparams.n_ctx);
model.position = ggml_new_tensor_1d(ctx, GGML_TYPE_I32, model.hparams.n_ctx);
ggml_set_name(model.embd, "in/embd");
ggml_set_name(model.position, "in/position");
size_t input_size = ggml_nbytes(model.embd) + ggml_nbytes(model.position);
ggml_backend_t backend_input = params.n_gpu_layers >= model.hparams.n_layer ? backend_gpu : backend_cpu;
model.buffer_input = ggml_backend_alloc_buffer(backend_input, input_size + 512*3);
printf("%s: backend_in = %s (%zu bytes)\n", __func__, ggml_backend_name(backend_input), input_size);
ggml_tallocr alloc = ggml_tallocr_new(model.buffer_input);
ggml_tallocr_alloc(&alloc, model.embd);
ggml_tallocr_alloc(&alloc, model.position);
}
return true;
}
struct ggml_cgraph * gpt2_graph(
const gpt2_model & model,
const int n_past,
const std::vector<gpt_vocab::id> & embd_inp) {
const int N = embd_inp.size();
const auto & hparams = model.hparams;
const int n_embd = hparams.n_embd;
const int n_layer = hparams.n_layer;
const int n_ctx = hparams.n_ctx;
const int n_head = hparams.n_head;
static size_t buf_size = ggml_tensor_overhead()*GPT2_MAX_NODES + ggml_graph_overhead_custom(GPT2_MAX_NODES, false);
static std::vector<uint8_t> buf(buf_size);
struct ggml_init_params params = {
buf_size,
buf.data(),
true,
};
struct ggml_context * ctx = ggml_init(params);
struct ggml_cgraph * gf = ggml_new_graph_custom(ctx, GPT2_MAX_NODES, false);
struct ggml_tensor * embd = ggml_view_1d(ctx, model.embd, N, 0);
ggml_backend_tensor_set(model.embd, embd_inp.data(), 0, N*ggml_element_size(embd));
struct ggml_tensor * position = ggml_view_1d(ctx, model.position, N, 0);
for (int i = 0; i < N; ++i) {
int32_t v = n_past + i;
ggml_backend_tensor_set(model.position, &v, i*sizeof(int32_t), sizeof(v));
}
const float KQ_scale = 1.0f/sqrtf(float(model.hparams.n_embd)/model.hparams.n_head);
struct ggml_tensor * inpL =
ggml_add(ctx,
ggml_get_rows(ctx, model.wte, embd),
ggml_get_rows(ctx, model.wpe, position));
ggml_set_name(inpL, "inpL");
ggml_set_name(inpL->src[0], "wte");
ggml_set_name(inpL->src[1], "wpe");
for (int il = 0; il < n_layer; ++il) {
struct ggml_tensor * cur;
{
cur = ggml_norm(ctx, inpL, hparams.eps);
ggml_format_name(cur, "l%d.norm", il);
cur = ggml_add(ctx,
ggml_mul(ctx,
cur,
model.layers[il].ln_1_g),
model.layers[il].ln_1_b);
ggml_format_name(cur, "l%d.ln_1_b", il);
ggml_format_name(cur->src[0], "l%d.ln_1_g", il);
}
{
cur = ggml_mul_mat(ctx,
model.layers[il].c_attn_attn_w,
cur);
ggml_format_name(cur, "l%d.attn_w", il);
cur = ggml_add(ctx,
cur,
model.layers[il].c_attn_attn_b);
ggml_format_name(cur, "l%d.attn_b", il);
}
{
struct ggml_tensor * Qcur = ggml_view_2d(ctx, cur, n_embd, N, cur->nb[1], 0*sizeof(float)*n_embd);
struct ggml_tensor * Kcur = ggml_view_2d(ctx, cur, n_embd, N, cur->nb[1], 1*sizeof(float)*n_embd);
struct ggml_tensor * Vcur = ggml_view_2d(ctx, cur, n_embd, N, cur->nb[1], 2*sizeof(float)*n_embd);
ggml_format_name(Qcur, "l%d.Qcur", il);
ggml_format_name(Kcur, "l%d.Kcur", il);
ggml_format_name(Vcur, "l%d.Vcur", il);
if (N >= 1) {
struct ggml_tensor * k = ggml_view_1d(ctx, model.memory_k, N*n_embd, (ggml_element_size(model.memory_k)*n_embd)*(il*n_ctx + n_past));
struct ggml_tensor * v = ggml_view_1d(ctx, model.memory_v, N*n_embd, (ggml_element_size(model.memory_v)*n_embd)*(il*n_ctx + n_past));
ggml_build_forward_expand(gf, ggml_cpy(ctx, Kcur, k));
ggml_build_forward_expand(gf, ggml_cpy(ctx, Vcur, v));
}
struct ggml_tensor * Q =
ggml_permute(ctx,
ggml_cont_3d(ctx, Qcur, n_embd/n_head, n_head, N),
0, 2, 1, 3);
ggml_format_name(Q, "l%d.Q", il);
struct ggml_tensor * K =
ggml_permute(ctx,
ggml_reshape_3d(ctx,
ggml_view_1d(ctx, model.memory_k, (n_past + N)*n_embd, il*n_ctx*ggml_element_size(model.memory_k)*n_embd),
n_embd/n_head, n_head, n_past + N),
0, 2, 1, 3);
ggml_format_name(K, "l%d.K", il);
struct ggml_tensor * KQ = ggml_mul_mat(ctx, K, Q);
ggml_format_name(KQ, "l%d.KQ", il);
struct ggml_tensor * KQ_scaled = ggml_scale(ctx, KQ, KQ_scale);
ggml_format_name(KQ_scaled, "l%d.KQ_scaled", il);
struct ggml_tensor * KQ_masked = ggml_diag_mask_inf(ctx, KQ_scaled, n_past);
ggml_format_name(KQ_masked, "l%d.KQ_masked", il);
struct ggml_tensor * KQ_soft_max = ggml_soft_max(ctx, KQ_masked);
ggml_format_name(KQ_soft_max, "l%d.KQ_soft_max", il);
struct ggml_tensor * V_trans =
ggml_cont_3d(ctx,
ggml_permute(ctx,
ggml_reshape_3d(ctx,
ggml_view_1d(ctx, model.memory_v, (n_past + N)*n_embd, il*n_ctx*ggml_element_size(model.memory_v)*n_embd),
n_embd/n_head, n_head, n_past + N),
1, 2, 0, 3),
n_past + N, n_embd/n_head, n_head);
struct ggml_tensor * KQV = ggml_mul_mat(ctx, V_trans, KQ_soft_max);
ggml_format_name(KQV, "l%d.KQV", il);
struct ggml_tensor * KQV_merged = ggml_permute(ctx, KQV, 0, 2, 1, 3);
ggml_format_name(KQV_merged, "l%d.KQV_merged", il);
cur = ggml_cont_2d(ctx, KQV_merged, n_embd, N);
ggml_format_name(cur, "l%d.KQV_merged_contiguous", il);
}
{
cur = ggml_mul_mat(ctx,
model.layers[il].c_attn_proj_w,
cur);
ggml_format_name(cur, "l%d.attn_proj_w", il);
cur = ggml_add(ctx,
cur,
model.layers[il].c_attn_proj_b);
ggml_format_name(cur, "l%d.attn_proj_b", il);
}
cur = ggml_add(ctx, cur, inpL);
ggml_format_name(cur, "l%d.add", il);
struct ggml_tensor * inpFF = cur;
{
{
cur = ggml_norm(ctx, inpFF, hparams.eps);
ggml_format_name(cur, "l%d.FFnorm", il);
cur = ggml_add(ctx,
ggml_mul(ctx,
cur,
model.layers[il].ln_2_g),
model.layers[il].ln_2_b);
ggml_format_name(cur, "l%d.ln_2_b", il);
ggml_format_name(cur->src[0], "l%d.ln_2_g", il);
}
cur = ggml_mul_mat(ctx,
model.layers[il].c_mlp_fc_w,
cur);
ggml_format_name(cur, "l%d.mlp_fc_w", il);
cur = ggml_add(ctx,
cur,
model.layers[il].c_mlp_fc_b);
ggml_format_name(cur, "l%d.mlp_fc_b", il);
cur = ggml_gelu(ctx, cur);
ggml_format_name(cur, "l%d.gelu", il);
cur = ggml_mul_mat(ctx,
model.layers[il].c_mlp_proj_w,
cur);
ggml_format_name(cur, "l%d.mlp_proj_w", il);
cur = ggml_add(ctx,
cur,
model.layers[il].c_mlp_proj_b);
ggml_format_name(cur, "l%d.mlp_proj_b", il);
}
inpL = ggml_add(ctx, cur, inpFF);
ggml_format_name(inpL, "l%d.add2", il);
}
{
inpL = ggml_norm(ctx, inpL, hparams.eps);
ggml_format_name(inpL, "out_norm");
inpL = ggml_add(ctx,
ggml_mul(ctx,
inpL,
model.ln_f_g),
model.ln_f_b);
ggml_format_name(inpL, "out_ln_f_b");
ggml_format_name(inpL->src[0], "out_ln_f_g");
}
inpL = ggml_mul_mat(ctx, model.lm_head, inpL);
ggml_format_name(inpL, "out_lm_head");
ggml_build_forward_expand(gf, inpL);
ggml_free(ctx);
return gf;
}
bool gpt2_eval(
const gpt2_model & model,
ggml_backend_sched_t sched,
const int n_past,
const std::vector<gpt_vocab::id> & embd_inp,
std::vector<float> & embd_w) {
const int N = embd_inp.size();
const auto & hparams = model.hparams;
const int n_vocab = hparams.n_vocab;
struct ggml_cgraph * gf = gpt2_graph(model, n_past, embd_inp);
ggml_backend_sched_reset(sched);
ggml_backend_sched_graph_compute(sched, gf);
struct ggml_tensor * inpL = ggml_graph_node(gf, -1);
embd_w.resize(n_vocab);
ggml_backend_tensor_get(inpL, embd_w.data(), (n_vocab*(N-1))*sizeof(float), sizeof(float)*n_vocab);
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
if (!gpt2_model_load(params.model, model, vocab, params)) {
fprintf(stderr, "%s: failed to load model from '%s'\n", __func__, params.model.c_str());
return 1;
}
t_load_us = ggml_time_us() - t_start_us;
test_gpt_tokenizer(vocab, params.token_test);
}
ggml_backend_sched_t sched;
{
sched = ggml_backend_sched_new(model.backends.data(), NULL, model.backends.size(), GPT2_MAX_NODES, false, true);
int n_tokens = std::min(model.hparams.n_ctx, params.n_batch);
int n_past = model.hparams.n_ctx - n_tokens;
struct ggml_cgraph * gf = gpt2_graph(model, n_past, std::vector<gpt_vocab::id>(n_tokens, 0));
ggml_backend_sched_reserve(sched, gf);
size_t mem_size = 0;
for (size_t i = 0; i < model.backends.size(); i++) {
size_t size = ggml_backend_sched_get_buffer_size(sched, model.backends[i]);
if (size > 0) {
mem_size += size;
printf("%s: %8s compute buffer size = %8.2f MB\n", __func__, ggml_backend_name(model.backends[i]), size/1024.0/1024.0);
}
}
printf("%s: total compute buffer size: %.2f MB\n", __func__, mem_size/1024.0/1024.0);
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
for (size_t i = embd.size(); i < embd_inp.size() + params.n_predict; i++) {
if (embd.size() > 0) {
const int64_t t_start_us = ggml_time_us();
if (!gpt2_eval(model, sched, n_past, embd, logits)) {
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
printf("%s:     load time = %8.2f ms\n", __func__, t_load_us/1000.0f);
printf("%s:   sample time = %8.2f ms\n", __func__, t_sample_us/1000.0f);
printf("%s:  predict time = %8.2f ms / %.2f ms per token\n", __func__, t_predict_us/1000.0f, t_predict_us/1000.0f/n_past);
printf("%s:    total time = %8.2f ms\n", __func__, (t_main_end_us - t_main_start_us)/1000.0f);
}
ggml_free(model.ctx_w);
ggml_backend_sched_free(sched);
ggml_backend_buffer_free(model.buffer_kv);
for (auto buf : model.buffers_w) {
ggml_backend_buffer_free(buf);
}
for (auto backend : model.backends) {
ggml_backend_free(backend);
}
return 0;
}