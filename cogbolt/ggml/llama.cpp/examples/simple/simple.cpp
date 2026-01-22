#include "llama.h"
#include <cstdio>
#include <cstring>
#include <string>
#include <vector>
static void print_usage(int, char ** argv) {
printf("\nexample usage:\n");
printf("\n    %s -m model.gguf [-n n_predict] [-ngl n_gpu_layers] [prompt]\n", argv[0]);
printf("\n");
}
int main(int argc, char ** argv) {
std::string model_path;
std::string prompt = "Hello my name is";
int ngl = 99;
int n_predict = 32;
{
int i = 1;
for (; i < argc; i++) {
if (strcmp(argv[i], "-m") == 0) {
if (i + 1 < argc) {
model_path = argv[++i];
} else {
print_usage(argc, argv);
return 1;
}
} else if (strcmp(argv[i], "-n") == 0) {
if (i + 1 < argc) {
try {
n_predict = std::stoi(argv[++i]);
} catch (...) {
print_usage(argc, argv);
return 1;
}
} else {
print_usage(argc, argv);
return 1;
}
} else if (strcmp(argv[i], "-ngl") == 0) {
if (i + 1 < argc) {
try {
ngl = std::stoi(argv[++i]);
} catch (...) {
print_usage(argc, argv);
return 1;
}
} else {
print_usage(argc, argv);
return 1;
}
} else {
break;
}
}
if (model_path.empty()) {
print_usage(argc, argv);
return 1;
}
if (i < argc) {
prompt = argv[i++];
for (; i < argc; i++) {
prompt += " ";
prompt += argv[i];
}
}
}
ggml_backend_load_all();
llama_model_params model_params = llama_model_default_params();
model_params.n_gpu_layers = ngl;
llama_model * model = llama_model_load_from_file(model_path.c_str(), model_params);
if (model == NULL) {
fprintf(stderr , "%s: error: unable to load model\n" , __func__);
return 1;
}
const llama_vocab * vocab = llama_model_get_vocab(model);
const int n_prompt = -llama_tokenize(vocab, prompt.c_str(), prompt.size(), NULL, 0, true, true);
std::vector<llama_token> prompt_tokens(n_prompt);
if (llama_tokenize(vocab, prompt.c_str(), prompt.size(), prompt_tokens.data(), prompt_tokens.size(), true, true) < 0) {
fprintf(stderr, "%s: error: failed to tokenize the prompt\n", __func__);
return 1;
}
llama_context_params ctx_params = llama_context_default_params();
ctx_params.n_ctx = n_prompt + n_predict - 1;
ctx_params.n_batch = n_prompt;
ctx_params.no_perf = false;
llama_context * ctx = llama_init_from_model(model, ctx_params);
if (ctx == NULL) {
fprintf(stderr , "%s: error: failed to create the llama_context\n" , __func__);
return 1;
}
auto sparams = llama_sampler_chain_default_params();
sparams.no_perf = false;
llama_sampler * smpl = llama_sampler_chain_init(sparams);
llama_sampler_chain_add(smpl, llama_sampler_init_greedy());
for (auto id : prompt_tokens) {
char buf[128];
int n = llama_token_to_piece(vocab, id, buf, sizeof(buf), 0, true);
if (n < 0) {
fprintf(stderr, "%s: error: failed to convert token to piece\n", __func__);
return 1;
}
std::string s(buf, n);
printf("%s", s.c_str());
}
llama_batch batch = llama_batch_get_one(prompt_tokens.data(), prompt_tokens.size());
const auto t_main_start = ggml_time_us();
int n_decode = 0;
llama_token new_token_id;
for (int n_pos = 0; n_pos + batch.n_tokens < n_prompt + n_predict; ) {
if (llama_decode(ctx, batch)) {
fprintf(stderr, "%s : failed to eval, return code %d\n", __func__, 1);
return 1;
}
n_pos += batch.n_tokens;
{
new_token_id = llama_sampler_sample(smpl, ctx, -1);
if (llama_vocab_is_eog(vocab, new_token_id)) {
break;
}
char buf[128];
int n = llama_token_to_piece(vocab, new_token_id, buf, sizeof(buf), 0, true);
if (n < 0) {
fprintf(stderr, "%s: error: failed to convert token to piece\n", __func__);
return 1;
}
std::string s(buf, n);
printf("%s", s.c_str());
fflush(stdout);
batch = llama_batch_get_one(&new_token_id, 1);
n_decode += 1;
}
}
printf("\n");
const auto t_main_end = ggml_time_us();
fprintf(stderr, "%s: decoded %d tokens in %.2f s, speed: %.2f t/s\n",
__func__, n_decode, (t_main_end - t_main_start) / 1000000.0f, n_decode / ((t_main_end - t_main_start) / 1000000.0f));
fprintf(stderr, "\n");
llama_perf_sampler_print(smpl);
llama_perf_context_print(ctx);
fprintf(stderr, "\n");
llama_sampler_free(smpl);
llama_free(ctx);
llama_model_free(model);
return 0;
}