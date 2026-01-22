#include "ggml_v3.h"
#include "otherarch.h"
#include "utils.h"
#include <cassert>
#include <cmath>
#include <cstdio>
#include <cstring>
#include <fstream>
#include <map>
#include <string>
#include <vector>
#include <iostream>
#include <algorithm>
#include "model_adapter.h"
#ifdef GGML_USE_CUDA
#include "ggml_v3-cuda.h"
#endif
#if defined(GGML_USE_CLBLAST)
#include "ggml_v3-opencl.h"
#endif
ModelLoadResult gpt2_model_load(const std::string & fname, gpt2_model & model, gpt_vocab & vocab, FileFormat file_format, int gpulayers) {
printf("%s: loading model from '%s'\n", __func__, fname.c_str());
auto fin = std::ifstream(fname, std::ios::binary);
if (!fin) {
fprintf(stderr, "%s: failed to open '%s'\n", __func__, fname.c_str());
return ModelLoadResult::FAIL;
}
{
uint32_t magic;
fin.read((char *) &magic, sizeof(magic));
if (magic != 0x67676d6c) {
fprintf(stderr, "%s: invalid model file '%s' (bad magic)\n", __func__, fname.c_str());
return ModelLoadResult::FAIL;
}
}
int32_t origmaxctx = model.hparams.n_ctx;
{
auto & hparams = model.hparams;
fin.read((char *) &hparams.n_vocab, sizeof(hparams.n_vocab));
fin.read((char *) &hparams.n_ctx,   sizeof(hparams.n_ctx));
fin.read((char *) &hparams.n_embd,  sizeof(hparams.n_embd));
fin.read((char *) &hparams.n_head,  sizeof(hparams.n_head));
fin.read((char *) &hparams.n_layer, sizeof(hparams.n_layer));
fin.read((char *) &hparams.ftype,   sizeof(hparams.ftype));
const int32_t qntvr = hparams.ftype / GGML_V3_QNT_VERSION_FACTOR;
printf("%s: n_vocab = %d\n", __func__, hparams.n_vocab);
printf("%s: n_ctx   = %d (%d)\n", __func__, hparams.n_ctx,origmaxctx);
printf("%s: n_embd  = %d\n", __func__, hparams.n_embd);
printf("%s: n_head  = %d\n", __func__, hparams.n_head);
printf("%s: n_layer = %d\n", __func__, hparams.n_layer);
printf("%s: ftype   = %d\n", __func__, hparams.ftype);
printf("%s: qntvr   = %d\n", __func__, qntvr);
hparams.ftype %= GGML_V3_QNT_VERSION_FACTOR;
}
{
int32_t n_vocab = 0;
fin.read((char *) &n_vocab, sizeof(n_vocab));
if (n_vocab != model.hparams.n_vocab) {
fprintf(stderr, "%s: invalid model file '%s' (bad vocab size %d != %d)\n",
__func__, fname.c_str(), n_vocab, model.hparams.n_vocab);
return ModelLoadResult::FAIL;
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
for (const std::string & token : {
"<|system|>",
"<|user|>",
"<|assistant|>",
"<|end|>",
}) {
if (vocab.token_to_id.find(token) != vocab.token_to_id.end()) {
vocab.add_special_token(token);
}
}
}
ggml_v3_type wtype = ggml_v3_ftype_to_ggml_v3_type((ggml_v3_ftype) (model.hparams.ftype));
if (wtype == GGML_V3_TYPE_COUNT) {
fprintf(stderr, "%s: invalid model file '%s' (bad ftype value %d)\n",
__func__, fname.c_str(), model.hparams.ftype);
return ModelLoadResult::FAIL;
}
auto & ctx = model.ctx;
size_t ctx_size = 0;
{
const auto & hparams = model.hparams;
const int n_embd  = hparams.n_embd;
const int n_layer = hparams.n_layer;
const int n_ctx   = hparams.n_ctx;
const int n_vocab = hparams.n_vocab;
const int head_dim = n_embd / hparams.n_head;
const int kv_heads = hparams.n_head;
const int kv_dim   = kv_heads * head_dim;
ctx_size += n_embd*ggml_v3_type_sizef(GGML_V3_TYPE_F32);
ctx_size += n_embd*ggml_v3_type_sizef(GGML_V3_TYPE_F32);
ctx_size += n_vocab*n_embd*ggml_v3_type_sizef(wtype);
ctx_size +=   n_ctx*n_embd*ggml_v3_type_sizef(GGML_V3_TYPE_F32);
ctx_size += n_vocab*n_embd*ggml_v3_type_sizef(wtype);
ctx_size += n_layer*(n_embd*ggml_v3_type_sizef(GGML_V3_TYPE_F32));
ctx_size += n_layer*(n_embd*ggml_v3_type_sizef(GGML_V3_TYPE_F32));
ctx_size += n_layer*(n_embd*ggml_v3_type_sizef(GGML_V3_TYPE_F32));
ctx_size += n_layer*(n_embd*ggml_v3_type_sizef(GGML_V3_TYPE_F32));
ctx_size += n_layer*((n_embd + 2*kv_dim)*n_embd*ggml_v3_type_sizef(wtype));
ctx_size += n_layer*(       (n_embd + 2*kv_dim)*ggml_v3_type_sizef(GGML_V3_TYPE_F32));
ctx_size += n_layer*(n_embd*n_embd*ggml_v3_type_sizef(wtype));
ctx_size += n_layer*(       n_embd*ggml_v3_type_sizef(GGML_V3_TYPE_F32));
ctx_size += n_layer*(4*n_embd*n_embd*ggml_v3_type_sizef(wtype));
ctx_size += n_layer*(       4*n_embd*ggml_v3_type_sizef(GGML_V3_TYPE_F32));
ctx_size += n_layer*(4*n_embd*n_embd*ggml_v3_type_sizef(wtype));
ctx_size += n_layer*(         n_embd*ggml_v3_type_sizef(GGML_V3_TYPE_F32));
ctx_size += std::max(origmaxctx,n_ctx)*n_layer*n_embd*ggml_v3_type_sizef(GGML_V3_TYPE_F16);
ctx_size += std::max(origmaxctx,n_ctx)*n_layer*n_embd*ggml_v3_type_sizef(GGML_V3_TYPE_F16);
ctx_size += (6 + 12*n_layer)*1024;
printf("%s: ggml ctx size = %6.2f MB\n", __func__, ctx_size/(1024.0*1024.0));
}
{
struct ggml_v3_init_params params;
params.mem_size   = ctx_size;
params.mem_buffer = NULL;
params.no_alloc   = false;
model.ctx = ggml_v3_init(params);
if (!model.ctx) {
fprintf(stderr, "%s: ggml_v3_init() failed\n", __func__);
return ModelLoadResult::FAIL;
}
}
{
const auto & hparams = model.hparams;
const int n_embd  = hparams.n_embd;
const int n_layer = hparams.n_layer;
const int n_ctx   = hparams.n_ctx;
const int n_vocab = hparams.n_vocab;
const int head_dim = n_embd / hparams.n_head;
const int kv_heads = hparams.n_head;
const int kv_dim   = kv_heads * head_dim;
model.layers.resize(n_layer);
model.ln_f_g = ggml_v3_new_tensor_1d(ctx, GGML_V3_TYPE_F32, n_embd);
model.ln_f_b = ggml_v3_new_tensor_1d(ctx, GGML_V3_TYPE_F32, n_embd);
model.wte     = ggml_v3_new_tensor_2d(ctx, wtype,         n_embd, n_vocab);
model.wpe     = ggml_v3_new_tensor_2d(ctx, GGML_V3_TYPE_F32, n_embd, n_ctx);
model.lm_head = ggml_v3_new_tensor_2d(ctx, wtype,         n_embd, n_vocab);
model.tensors["model/ln_f/g"] = model.ln_f_g;
model.tensors["model/ln_f/b"] = model.ln_f_b;
model.tensors["model/wte"]     = model.wte;
model.tensors["model/wpe"]     = model.wpe;
model.tensors["model/lm_head"] = model.lm_head;
for (int i = 0; i < n_layer; ++i) {
auto & layer = model.layers[i];
layer.ln_1_g        = ggml_v3_new_tensor_1d(ctx, GGML_V3_TYPE_F32,   n_embd);
layer.ln_1_b        = ggml_v3_new_tensor_1d(ctx, GGML_V3_TYPE_F32,   n_embd);
layer.ln_2_g        = ggml_v3_new_tensor_1d(ctx, GGML_V3_TYPE_F32,   n_embd);
layer.ln_2_b        = ggml_v3_new_tensor_1d(ctx, GGML_V3_TYPE_F32,   n_embd);
layer.c_attn_attn_w = ggml_v3_new_tensor_2d(ctx, wtype,           n_embd, n_embd + 2*kv_dim);
layer.c_attn_attn_b = ggml_v3_new_tensor_1d(ctx, GGML_V3_TYPE_F32, n_embd + 2*kv_dim);
layer.c_attn_proj_w = ggml_v3_new_tensor_2d(ctx, wtype,           n_embd, n_embd);
layer.c_attn_proj_b = ggml_v3_new_tensor_1d(ctx, GGML_V3_TYPE_F32,   n_embd);
layer.c_mlp_fc_w    = ggml_v3_new_tensor_2d(ctx, wtype,           n_embd, 4*n_embd);
layer.c_mlp_fc_b    = ggml_v3_new_tensor_1d(ctx, GGML_V3_TYPE_F32, 4*n_embd);
layer.c_mlp_proj_w  = ggml_v3_new_tensor_2d(ctx, wtype,         4*n_embd, n_embd);
layer.c_mlp_proj_b  = ggml_v3_new_tensor_1d(ctx, GGML_V3_TYPE_F32,   n_embd);
model.tensors["model/h" + std::to_string(i) + "/ln_1/g"]        = layer.ln_1_g;
model.tensors["model/h" + std::to_string(i) + "/ln_1/b"]        = layer.ln_1_b;
model.tensors["model/h" + std::to_string(i) + "/ln_2/g"]        = layer.ln_2_g;
model.tensors["model/h" + std::to_string(i) + "/ln_2/b"]        = layer.ln_2_b;
model.tensors["model/h" + std::to_string(i) + "/attn/c_attn/w"] = layer.c_attn_attn_w;
model.tensors["model/h" + std::to_string(i) + "/attn/c_attn/b"] = layer.c_attn_attn_b;
model.tensors["model/h" + std::to_string(i) + "/attn/c_proj/w"] = layer.c_attn_proj_w;
model.tensors["model/h" + std::to_string(i) + "/attn/c_proj/b"] = layer.c_attn_proj_b;
model.tensors["model/h" + std::to_string(i) + "/mlp/c_fc/w"]    = layer.c_mlp_fc_w;
model.tensors["model/h" + std::to_string(i) + "/mlp/c_fc/b"]    = layer.c_mlp_fc_b;
model.tensors["model/h" + std::to_string(i) + "/mlp/c_proj/w"]  = layer.c_mlp_proj_w;
model.tensors["model/h" + std::to_string(i) + "/mlp/c_proj/b"]  = layer.c_mlp_proj_b;
}
}
{
const auto & hparams = model.hparams;
const int n_embd  = hparams.n_embd;
const int n_layer = hparams.n_layer;
const int n_ctx   = hparams.n_ctx;
const int n_mem      = n_layer*std::max(origmaxctx,n_ctx);
const int n_elements = n_embd*n_mem;
model.memory_k = ggml_v3_new_tensor_1d(ctx, GGML_V3_TYPE_F16, n_elements);
model.memory_v = ggml_v3_new_tensor_1d(ctx, GGML_V3_TYPE_F16, n_elements);
const size_t memory_size = ggml_v3_nbytes(model.memory_k) + ggml_v3_nbytes(model.memory_v);
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
fin.read(reinterpret_cast<char *>(&ttype),  sizeof(ttype));
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
if (model.tensors.find(name.data()) == model.tensors.end()) {
fprintf(stderr, "%s: unknown tensor '%s' in model file\n", __func__, name.data());
return ModelLoadResult::FAIL;
}
auto tensor = model.tensors[name.data()];
if (tensor->ne[0] != ne[0] || tensor->ne[1] != ne[1]) {
fprintf(stderr, "%s: tensor '%s' has wrong shape in model file: got [%d, %d], expected [%d, %d]\n",
__func__, name.data(), (int) tensor->ne[0], (int) tensor->ne[1], ne[0], ne[1]);
return ModelLoadResult::FAIL;
}
if (ggml_v3_nelements(tensor) != nelements) {
fprintf(stderr, "%s: tensor '%s' has wrong size in model file. got %d, expected %d\n",
__func__, name.data(), (int) ggml_v3_nelements(tensor), nelements);
return ModelLoadResult::FAIL;
}
if (0) {
printf("%24s - [%5d, %5d], type = %6s, %6.2f MB, %9zu bytes\n", name.data(), ne[0], ne[1], ggml_v3_type_name(ggml_v3_type(ttype)), ggml_v3_nbytes(tensor)/1024.0/1024.0, ggml_v3_nbytes(tensor));
}
const size_t bpe = ggml_v3_type_size(ggml_v3_type(ttype));
if ((nelements*bpe)/ggml_v3_blck_size(tensor->type) != ggml_v3_nbytes(tensor)) {
fprintf(stderr, "%s: tensor '%s' has wrong size in model file: got %zu, expected %zu\n",
__func__, name.data(), ggml_v3_nbytes(tensor), nelements*bpe);
return ModelLoadResult::FAIL;
}
fin.read(reinterpret_cast<char *>(tensor->data), ggml_v3_nbytes(tensor));
if (name == "model/wte" && has_lm_head == false) {
memcpy(model.lm_head->data, tensor->data, ggml_v3_nbytes(tensor));
}
if (name == "model/lm_head") {
has_lm_head = true;
}
total_size += ggml_v3_nbytes(tensor);
}
printf("%s: model size  = %8.2f MB\n", __func__, total_size/1024.0/1024.0);
}
fin.close();
#if defined(GGML_USE_CLBLAST) || defined(GGML_USE_CUDA)
if(gpulayers>0)
{
const auto & hparams = model.hparams;
size_t vram_total = 0;
const int n_gpu = std::min(gpulayers, int(hparams.n_layer));
#if defined(GGML_USE_CLBLAST)
fprintf(stderr, "%s: [opencl] offloading %d layers to GPU\n", __func__, n_gpu);
#else
fprintf(stderr, "%s: [CUDA] offloading %d layers to GPU\n", __func__, n_gpu);
#endif
for (int i = 0; i < n_gpu; ++i) {
const auto & layer = model.layers[i];
layer.c_attn_attn_w->backend = GGML_V3_BACKEND_GPU;
layer.c_attn_proj_w->backend = GGML_V3_BACKEND_GPU;
layer.c_mlp_fc_w->backend = GGML_V3_BACKEND_GPU;
layer.c_mlp_proj_w->backend = GGML_V3_BACKEND_GPU;
#if defined(GGML_USE_CLBLAST)
ggml_v3_cl_transform_tensor(layer.c_attn_attn_w->data,layer.c_attn_attn_w); vram_total += ggml_v3_nbytes(layer.c_attn_attn_w);
ggml_v3_cl_transform_tensor(layer.c_attn_proj_w->data,layer.c_attn_proj_w); vram_total += ggml_v3_nbytes(layer.c_attn_proj_w);
ggml_v3_cl_transform_tensor(layer.c_mlp_fc_w->data,layer.c_mlp_fc_w); vram_total += ggml_v3_nbytes(layer.c_mlp_fc_w);
ggml_v3_cl_transform_tensor(layer.c_mlp_proj_w->data,layer.c_mlp_proj_w); vram_total += ggml_v3_nbytes(layer.c_mlp_proj_w);
#else
ggml_v3_cuda_transform_tensor(layer.c_attn_attn_w->data,layer.c_attn_attn_w); vram_total += ggml_v3_nbytes(layer.c_attn_attn_w);
ggml_v3_cuda_transform_tensor(layer.c_attn_proj_w->data,layer.c_attn_proj_w); vram_total += ggml_v3_nbytes(layer.c_attn_proj_w);
ggml_v3_cuda_transform_tensor(layer.c_mlp_fc_w->data,layer.c_mlp_fc_w); vram_total += ggml_v3_nbytes(layer.c_mlp_fc_w);
ggml_v3_cuda_transform_tensor(layer.c_mlp_proj_w->data,layer.c_mlp_proj_w); vram_total += ggml_v3_nbytes(layer.c_mlp_proj_w);
#endif
}
#if defined(GGML_USE_CLBLAST)
fprintf(stderr, "%s: [opencl] total VRAM used: %zu MB\n", __func__, vram_total / 1024 / 1024);
#else
fprintf(stderr, "%s: [CUDA] total VRAM used: %zu MB\n", __func__, vram_total / 1024 / 1024);
#endif
}
#endif
return ModelLoadResult::SUCCESS;
}
bool gpt2_eval(
const gpt2_model & model,
const int n_threads,
const int n_past,
const std::vector<gpt_vocab::id> & embd_inp,
std::vector<float>         & embd_w,
size_t                     & mem_per_token,
bool use_scratch) {
const int N = embd_inp.size();
const auto & hparams = model.hparams;
const int n_embd  = hparams.n_embd;
const int n_layer = hparams.n_layer;
const int n_ctx   = hparams.n_ctx;
const int n_head  = hparams.n_head;
const int n_vocab = hparams.n_vocab;
static size_t buf_size = 256u*1024*1024;
static void * buf = malloc(buf_size);
static size_t scr0_size = (n_embd>2400?512u:256u)*1024*1024*(hparams.n_ctx>8192?2:1);
static size_t scr1_size = (n_embd>2400?512u:256u)*1024*1024;
static void * scr0 = malloc(scr0_size);
static void * scr1 = malloc(scr1_size);
if (mem_per_token > 0 && (mem_per_token*N*2 + 64u*1024*1024) > buf_size) {
const size_t buf_size_new = 320u*1024*1024 + 1.2*(mem_per_token*N);
if (buf_size_new > buf_size)
{
buf_size = buf_size_new;
buf = realloc(buf, buf_size);
if (buf == nullptr)
{
fprintf(stderr, "%s: failed to allocate %zu bytes. Try reducing batch size.\n", __func__, buf_size);
return false;
}
}
}
struct ggml_v3_init_params params;
params.mem_size   = buf_size;
params.mem_buffer = buf;
params.no_alloc   = false;
struct ggml_v3_context * ctx0 = ggml_v3_init(params);
struct ggml_v3_cgraph * gf = ggml_v3_new_graph_custom(ctx0, 8192, false);
struct ggml_v3_tensor * embd = ggml_v3_new_tensor_1d(ctx0, GGML_V3_TYPE_I32, N);
memcpy(embd->data, embd_inp.data(), N*ggml_v3_element_size(embd));
struct ggml_v3_tensor * position = ggml_v3_new_tensor_1d(ctx0, GGML_V3_TYPE_I32, N);
for (int i = 0; i < N; ++i) {
((int32_t *) position->data)[i] = n_past + i;
}
struct ggml_v3_tensor * inpL =
ggml_v3_add(ctx0,
ggml_v3_get_rows(ctx0, model.wte, embd),
ggml_v3_get_rows(ctx0, model.wpe, position));
for (int il = 0; il < n_layer; ++il) {
struct ggml_v3_tensor * cur;
if(use_scratch){
ggml_v3_set_scratch(ctx0, { 0, scr0_size, scr0, });
}
{
cur = ggml_v3_norm(ctx0, inpL, default_norm_eps);
cur = ggml_v3_add(ctx0,
ggml_v3_mul(ctx0,
ggml_v3_repeat(ctx0, model.layers[il].ln_1_g, cur),
cur),
ggml_v3_repeat(ctx0, model.layers[il].ln_1_b, cur));
}
{
cur = ggml_v3_mul_mat(ctx0,
model.layers[il].c_attn_attn_w,
cur);
cur = ggml_v3_add(ctx0,
ggml_v3_repeat(ctx0, model.layers[il].c_attn_attn_b, cur),
cur);
}
{
struct ggml_v3_tensor * Qcur = ggml_v3_view_2d(ctx0, cur, n_embd, N, cur->nb[1], 0*sizeof(float)*n_embd);
struct ggml_v3_tensor * Kcur = ggml_v3_view_2d(ctx0, cur, n_embd, N, cur->nb[1], 1*sizeof(float)*n_embd);
struct ggml_v3_tensor * Vcur = ggml_v3_view_2d(ctx0, cur, n_embd, N, cur->nb[1], 2*sizeof(float)*n_embd);
if (N >= 1) {
struct ggml_v3_tensor * k = ggml_v3_view_1d(ctx0, model.memory_k, N*n_embd, (ggml_v3_element_size(model.memory_k)*n_embd)*(il*n_ctx + n_past));
struct ggml_v3_tensor * v = ggml_v3_view_1d(ctx0, model.memory_v, N*n_embd, (ggml_v3_element_size(model.memory_v)*n_embd)*(il*n_ctx + n_past));
ggml_v3_build_forward_expand(gf, ggml_v3_cpy(ctx0, Kcur, k));
ggml_v3_build_forward_expand(gf, ggml_v3_cpy(ctx0, Vcur, v));
}
struct ggml_v3_tensor * Q =
ggml_v3_permute(ctx0,
ggml_v3_cpy(ctx0,
Qcur,
ggml_v3_new_tensor_3d(ctx0, GGML_V3_TYPE_F32, n_embd/n_head, n_head, N)),
0, 2, 1, 3);
struct ggml_v3_tensor * K =
ggml_v3_permute(ctx0,
ggml_v3_reshape_3d(ctx0,
ggml_v3_view_1d(ctx0, model.memory_k, (n_past + N)*n_embd, il*n_ctx*ggml_v3_element_size(model.memory_k)*n_embd),
n_embd/n_head, n_head, n_past + N),
0, 2, 1, 3);
struct ggml_v3_tensor * KQ = ggml_v3_mul_mat(ctx0, K, Q);
struct ggml_v3_tensor * KQ_scaled =
ggml_v3_scale_inplace(ctx0,
KQ,
1.0f/sqrt(float(n_embd)/n_head)
);
struct ggml_v3_tensor * KQ_masked = ggml_v3_diag_mask_inf_inplace(ctx0, KQ_scaled, n_past);
struct ggml_v3_tensor * KQ_soft_max = ggml_v3_soft_max_inplace(ctx0, KQ_masked);
struct ggml_v3_tensor * V_trans =
ggml_v3_cpy(ctx0,
ggml_v3_permute(ctx0,
ggml_v3_reshape_3d(ctx0,
ggml_v3_view_1d(ctx0, model.memory_v, (n_past + N)*n_embd, il*n_ctx*ggml_v3_element_size(model.memory_v)*n_embd),
n_embd/n_head, n_head, n_past + N),
1, 2, 0, 3),
ggml_v3_new_tensor_3d(ctx0, model.memory_v->type, n_past + N, n_embd/n_head, n_head));
struct ggml_v3_tensor * KQV = ggml_v3_mul_mat(ctx0, V_trans, KQ_soft_max);
struct ggml_v3_tensor * KQV_merged = ggml_v3_permute(ctx0, KQV, 0, 2, 1, 3);
cur = ggml_v3_cpy(ctx0,
KQV_merged,
ggml_v3_new_tensor_2d(ctx0, GGML_V3_TYPE_F32, n_embd, N));
}
{
cur = ggml_v3_mul_mat(ctx0,
model.layers[il].c_attn_proj_w,
cur);
cur = ggml_v3_add(ctx0,
ggml_v3_repeat(ctx0, model.layers[il].c_attn_proj_b, cur),
cur);
}
cur = ggml_v3_add(ctx0, cur, inpL);
struct ggml_v3_tensor * inpFF = cur;
if(use_scratch){
ggml_v3_set_scratch(ctx0, { 0, scr1_size, scr1, });
}
{
{
cur = ggml_v3_norm(ctx0, inpFF, default_norm_eps);
cur = ggml_v3_add(ctx0,
ggml_v3_mul(ctx0,
ggml_v3_repeat(ctx0, model.layers[il].ln_2_g, cur),
cur),
ggml_v3_repeat(ctx0, model.layers[il].ln_2_b, cur));
}
cur = ggml_v3_mul_mat(ctx0,
model.layers[il].c_mlp_fc_w,
cur);
cur = ggml_v3_add(ctx0,
ggml_v3_repeat(ctx0, model.layers[il].c_mlp_fc_b, cur),
cur);
cur = ggml_v3_gelu(ctx0, cur);
cur = ggml_v3_mul_mat(ctx0,
model.layers[il].c_mlp_proj_w,
cur);
cur = ggml_v3_add(ctx0,
ggml_v3_repeat(ctx0, model.layers[il].c_mlp_proj_b, cur),
cur);
}
inpL = ggml_v3_add(ctx0, cur, inpFF);
}
if(use_scratch){
ggml_v3_set_scratch(ctx0, { 0, scr0_size, scr0, });
}
{
inpL = ggml_v3_norm(ctx0, inpL, default_norm_eps);
inpL = ggml_v3_add(ctx0,
ggml_v3_mul(ctx0,
ggml_v3_repeat(ctx0, model.ln_f_g, inpL),
inpL),
ggml_v3_repeat(ctx0, model.ln_f_b, inpL));
}
if(use_scratch){
ggml_v3_set_scratch(ctx0, { 0, 0, nullptr, });
}
inpL = ggml_v3_mul_mat(ctx0, model.lm_head, inpL);
ggml_v3_build_forward_expand(gf, inpL);
kcpp_graph_compute_helper(gf, n_threads);
embd_w.resize(n_vocab);
memcpy(embd_w.data(), (float *) ggml_v3_get_data(inpL) + (n_vocab*(N-1)), sizeof(float)*n_vocab);
if (mem_per_token == 0) {
mem_per_token = ggml_v3_used_mem(ctx0)/N;
}
ggml_v3_free(ctx0);
return true;
}