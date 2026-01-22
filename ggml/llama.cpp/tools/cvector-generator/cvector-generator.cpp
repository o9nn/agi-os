#include "ggml.h"
#include "gguf.h"
#include "arg.h"
#include "common.h"
#include "llama.h"
#include "pca.hpp"
#include "mean.hpp"
#ifdef GGML_USE_CUDA
#include "ggml-cuda.h"
#endif
#ifdef GGML_USE_METAL
#include "ggml-metal.h"
#endif
#include <algorithm>
#include <climits>
#include <cstdio>
#include <cstring>
#include <fstream>
#include <iostream>
#include <string>
#include <tuple>
#include <vector>
template <class Iter>
static std::string tokens_to_str(llama_context * ctx, Iter begin, Iter end) {
std::string ret;
for (; begin != end; ++begin) {
ret += common_token_to_piece(ctx, *begin);
}
return ret;
}
static void print_usage(int, char ** argv) {
printf("\nexample usage:\n");
printf("\n    CPU only:   %s -m ./llama-3.Q4_K_M.gguf\n", argv[0]);
printf("\n    with GPU:   %s -m ./llama-3.Q4_K_M.gguf -ngl 99\n", argv[0]);
printf("\n    advanced:   %s -m ./llama-3.Q4_K_M.gguf -ngl 99 --pca-iter 2000 --pca-batch 100\n", argv[0]);
printf("\n    using mean: %s -m ./llama-3.Q4_K_M.gguf --method mean\n", argv[0]);
printf("\n");
}
struct callback_data {
ggml_context * ctx_ggml = nullptr;
int n_layers = 0;
int n_tokens = 0;
bool is_eval_pos = true;
std::vector<struct ggml_tensor *> v_pos;
std::vector<struct ggml_tensor *> v_neg;
std::vector<struct ggml_tensor *> v_diff_filtered;
void save_tensor_for_layer(struct ggml_tensor * t) {
GGML_ASSERT(t->type == GGML_TYPE_F32);
if (ctx_ggml == nullptr) {
struct ggml_init_params params_ggml = {
ggml_tensor_overhead() * n_layers * 3u,
NULL,
true,
};
ctx_ggml = ggml_init(params_ggml);
}
auto n_bytes = ggml_nbytes(t);
struct ggml_tensor * t_layer = ggml_new_tensor_2d(ctx_ggml, t->type, t->ne[0], t->ne[1]);
t_layer->data = malloc(n_bytes);
ggml_backend_tensor_get(t, t_layer->data, 0, n_bytes);
ggml_set_name(t_layer, ggml_get_name(t));
if (is_eval_pos) {
v_pos.push_back(t_layer);
} else {
v_neg.push_back(t_layer);
}
}
std::vector<struct ggml_tensor *> calc_diff() {
for (float il = 0; il < v_pos.size(); il++) {
float * a = (float *) v_pos[il]->data;
float * b = (float *) v_neg[il]->data;
size_t n_elem = ggml_nelements(v_pos[il]);
for (size_t j = 0; j < n_elem; j++) {
a[j] -= b[j];
}
auto diff_filtered = filter_nonzero_rows(v_pos[il]);
v_diff_filtered.push_back(diff_filtered);
}
return v_diff_filtered;
}
struct ggml_tensor * filter_nonzero_rows(struct ggml_tensor * a) {
auto is_row_all_zeros = [](struct ggml_tensor * t, int row, float eps) -> bool {
int n_cols = t->ne[0];
for (int col = 0; col < n_cols; ++col) {
if (ggml_get_f32_nd(t, col, row, 0, 0) > eps) {
return false;
}
}
return true;
};
std::vector<int> rows_to_copy;
for (int i_row = 0; i_row < a->ne[1]; i_row++) {
if (!is_row_all_zeros(a, i_row, 1e-6)) {
rows_to_copy.push_back(i_row);
}
}
int n_nonzero_rows = rows_to_copy.size();
int n_embd = a->ne[0];
GGML_ASSERT(n_nonzero_rows > 0);
struct ggml_tensor * diff_filtered = ggml_new_tensor_2d(
ctx_ggml, GGML_TYPE_F32, n_embd, n_nonzero_rows);
ggml_format_name(diff_filtered, "diff_filtered_%s", a->name);
diff_filtered->data = malloc(ggml_nbytes(diff_filtered));
for (int dest_row = 0; dest_row < n_nonzero_rows; dest_row++) {
int src_row = rows_to_copy[dest_row];
for (int i = 0; i < n_embd; i++) {
float src_elem = ggml_get_f32_nd(a, i, src_row, 0, 0);
ggml_set_f32_nd(diff_filtered, i, dest_row, 0, 0, src_elem);
}
}
return diff_filtered;
}
void reset() {
for (auto ptr : v_pos) free(ptr->data);
for (auto ptr : v_neg) free(ptr->data);
for (auto ptr : v_diff_filtered) free(ptr->data);
v_pos.clear();
v_neg.clear();
v_diff_filtered.clear();
if (ctx_ggml) {
ggml_free(ctx_ggml);
}
ctx_ggml = nullptr;
}
};
struct train_context {
ggml_context * ctx_ggml;
int n_embd;
int n_layers;
std::vector<std::string> positive_entries;
std::vector<std::string> negative_entries;
std::vector<struct ggml_tensor *> v_diff;
std::vector<struct ggml_tensor *> v_final;
std::vector<std::vector<uint8_t>> v_diff_tmp;
train_context(int n_embd_, int n_layers_) {
n_embd = n_embd_;
n_layers = n_layers_;
struct ggml_init_params params_ggml = {
ggml_tensor_overhead() * (n_layers - 1) * 2u,
NULL,
true,
};
ctx_ggml = ggml_init(params_ggml);
for (int il = 0; il < n_layers - 1; il++) {
std::vector<uint8_t> empty;
v_diff_tmp.push_back(empty);
auto t = ggml_new_tensor_1d(ctx_ggml, GGML_TYPE_F32, n_embd);
t->data = malloc(ggml_nbytes(t));
v_final.push_back(t);
}
}
void concat_diff_tmp(const std::vector<struct ggml_tensor *> & diff_filtered) {
GGML_ASSERT((int) diff_filtered.size() == n_layers - 1);
for (int il = 0; il < n_layers - 1; il++) {
auto t = diff_filtered[il];
auto & diff_tmp = v_diff_tmp[il];
size_t curr_size = diff_tmp.size();
diff_tmp.resize(curr_size + ggml_nbytes(t));
memcpy(diff_tmp.data() + curr_size, t->data, ggml_nbytes(t));
}
}
void build_v_diff(bool transpose) {
printf("build_v_diff\n");
for (int il = 0; il < n_layers - 1; il++) {
auto & diff_tmp = v_diff_tmp[il];
int n_elem = diff_tmp.size() / sizeof(float);
GGML_ASSERT(n_elem % n_embd == 0);
int n_rows = n_elem / n_embd;
struct ggml_tensor * diff = transpose
? ggml_new_tensor_2d(ctx_ggml, GGML_TYPE_F32, n_rows, n_embd)
: ggml_new_tensor_2d(ctx_ggml, GGML_TYPE_F32, n_embd, n_rows);
ggml_set_name(diff, (std::string("diff_") + std::to_string(il)).c_str());
diff->data = malloc(ggml_nbytes(diff));
if (transpose) {
float * arr = (float *) diff_tmp.data();
for (int ir = 0; ir < n_rows; ++ir) {
for (int ic = 0; ic < n_embd; ++ic) {
float f = arr[ir*n_embd + ic];
ggml_set_f32_nd(diff, ir, ic, 0, 0, f);
}
}
} else {
memcpy(diff->data, diff_tmp.data(), ggml_nbytes(diff));
}
v_diff.push_back(diff);
print_debug_tensor(diff);
diff_tmp.resize(0);
}
}
~train_context() {
for (auto ptr : v_final) free(ptr->data);
for (auto ptr : v_diff) free(ptr->data);
ggml_free(ctx_ggml);
}
};
struct tokenized_prompt {
std::vector<llama_token> tokens_pos;
std::vector<llama_token> tokens_neg;
size_t max_seq_len;
tokenized_prompt(llama_context * ctx, std::string pos, std::string neg) {
const llama_model * model = llama_get_model(ctx);
const llama_vocab * vocab = llama_model_get_vocab(model);
const bool add_bos = llama_vocab_get_add_bos(vocab);
tokens_pos = common_tokenize(ctx, pos, add_bos, true);
tokens_neg = common_tokenize(ctx, neg, add_bos, true);
max_seq_len = std::max(tokens_pos.size(), tokens_neg.size());
padding_seq(ctx, tokens_pos, max_seq_len);
padding_seq(ctx, tokens_neg, max_seq_len);
}
void padding_seq(llama_context * ctx, std::vector<llama_token> & tokens, size_t len) {
std::vector<llama_token> pad_tokens = common_tokenize(ctx, " ", false);
llama_token pad_tok = pad_tokens.back();
while (tokens.size() < len) {
tokens.push_back(pad_tok);
}
}
};
template <typename T>
static std::string to_string(const T & val) {
std::stringstream ss;
ss << val;
return ss.str();
}
static std::vector<std::string> ctrlvec_load_prompt_file(std::string path, bool skip_empty_lines) {
std::vector<std::string> output;
std::ifstream file(path);
if (!file.is_open()) {
fprintf(stderr, "error: unable to open file: %s\n", path.c_str());
exit(1);
}
std::string line;
while (std::getline(file, line)) {
bool is_skip = skip_empty_lines && line.empty();
if (!is_skip) {
string_process_escapes(line);
output.push_back(line);
}
}
file.close();
return output;
}
static bool cb_eval(struct ggml_tensor * t, bool ask, void * user_data) {
auto * cb_data = (callback_data *) user_data;
static const char * l_out_name = "l_out";
const bool is_l_out = strncmp(t->name, l_out_name, strlen(l_out_name)) == 0;
if (ask) {
return is_l_out;
}
if (!is_l_out || t->ne[1] != cb_data->n_tokens) {
return true;
}
cb_data->save_tensor_for_layer(t);
return true;
}
static bool get_hidden_layers(llama_context * ctx, std::vector<llama_token> & tokens) {
llama_memory_clear(llama_get_memory(ctx), true);
if (llama_decode(ctx, llama_batch_get_one(tokens.data(), tokens.size()))) {
fprintf(stderr, "%s : failed to eval\n", __func__);
return false;
}
return true;
}
static void export_gguf(const std::vector<struct ggml_tensor *> & v_ctrl, const std::string fname, const std::string model_hint) {
struct gguf_context * ctx = gguf_init_empty();
const std::string arch = "controlvector";
gguf_set_val_str(ctx, "general.architecture", arch.c_str());
gguf_set_val_str(ctx, (arch + ".model_hint").c_str(), model_hint.c_str());
gguf_set_val_i32(ctx, (arch + ".layer_count").c_str(), v_ctrl.size());
for (size_t i = 0; i < v_ctrl.size(); ++i) {
gguf_add_tensor(ctx, v_ctrl[i]);
print_debug_tensor(v_ctrl[i]);
printf("Added tensor: %s\n", v_ctrl[i]->name);
}
printf("%s: writing file...\n", __func__);
gguf_write_to_file(ctx, fname.c_str(), false);
printf("%s: wrote file '%s'\n", __func__, fname.c_str());
gguf_free(ctx);
}
static int prepare_entries(common_params & params, train_context & ctx_train) {
std::vector<std::string> positive_prompts = ctrlvec_load_prompt_file(params.cvector_positive_file, true);
std::vector<std::string> negative_prompts = ctrlvec_load_prompt_file(params.cvector_negative_file, true);
if (positive_prompts.size() != negative_prompts.size()) {
fprintf(stderr, "number of positive and negative prompts must be equal\n");
return 1;
}
if (positive_prompts.empty()) {
fprintf(stderr, "must provide at least one prompt pair\n");
return 1;
}
ctx_train.positive_entries = positive_prompts;
ctx_train.negative_entries = negative_prompts;
return 0;
}
int main(int argc, char ** argv) {
common_params params;
params.out_file = "control_vector.gguf";
if (!common_params_parse(argc, argv, params, LLAMA_EXAMPLE_CVECTOR_GENERATOR, print_usage)) {
return 1;
}
if (params.n_pca_iterations % params.n_pca_batch != 0) {
fprintf(stderr, "PCA iterations must by multiply of PCA batch size\n");
return 1;
}
callback_data cb_data;
params.cb_eval = cb_eval;
params.cb_eval_user_data = &cb_data;
params.warmup = false;
print_build_info();
llama_backend_init();
llama_numa_init(params.numa);
common_init_result llama_init = common_init_from_params(params);
llama_model * model = llama_init.model.get();
llama_context * ctx = llama_init.context.get();
int n_layers = llama_model_n_layer(model);
int n_embd = llama_model_n_embd(model);
char model_hint[128];
llama_model_meta_val_str(model, "general.architecture", model_hint, 128);
train_context ctx_train(n_embd, n_layers);
prepare_entries(params, ctx_train);
std::vector<tokenized_prompt> tokenized_prompts;
size_t n_total_tokens = 0;
for (size_t i = 0; i < ctx_train.positive_entries.size(); ++i) {
tokenized_prompt t(ctx, ctx_train.positive_entries[i], ctx_train.negative_entries[i]);
n_total_tokens += 2 * t.max_seq_len;
tokenized_prompts.push_back(std::move(t));
}
std::cout << "n_total_tokens: " << n_total_tokens << std::endl;
for(size_t i = 0; i < ctx_train.positive_entries.size(); ++i) {
bool success = false;
tokenized_prompt t = tokenized_prompts[i];
cb_data.n_layers = n_layers;
cb_data.n_tokens = t.max_seq_len;
printf("Evaluating prompt[%d/%d]: \"%s\" - \"%s\" (%d tokens)\n",
(int) i+1, (int) ctx_train.positive_entries.size(),
tokens_to_str(ctx, t.tokens_pos.cbegin(), t.tokens_pos.cend()).c_str(),
tokens_to_str(ctx, t.tokens_neg.cbegin(), t.tokens_neg.cend()).c_str(),
(int) t.max_seq_len);
cb_data.is_eval_pos = true;
success = get_hidden_layers(ctx, t.tokens_pos);
if (!success) break;
cb_data.is_eval_pos = false;
success = get_hidden_layers(ctx, t.tokens_neg);
if (!success) break;
auto v_diff_filtered = cb_data.calc_diff();
ctx_train.concat_diff_tmp(v_diff_filtered);
cb_data.reset();
}
printf("Done evaluate prompts, unload model...\n");
bool use_pca = params.cvector_dimre_method == DIMRE_METHOD_PCA;
ctx_train.build_v_diff(use_pca);
if (use_pca) {
PCA::pca_params pca_params;
pca_params.n_threads    = params.cpuparams.n_threads;
pca_params.n_batch      = params.n_pca_batch;
pca_params.n_iterations = params.n_pca_iterations;
PCA::run_pca(pca_params, ctx_train.v_diff, ctx_train.v_final);
} else {
mean::run(ctx_train.v_diff, ctx_train.v_final);
}
export_gguf(ctx_train.v_final, params.out_file, model_hint);
llama_backend_free();
return 0;
}