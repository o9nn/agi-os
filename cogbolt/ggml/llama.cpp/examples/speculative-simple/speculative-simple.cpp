#include "arg.h"
#include "common.h"
#include "sampling.h"
#include "speculative.h"
#include "log.h"
#include "llama.h"
#include <cstdio>
#include <cstring>
#include <string>
#include <vector>
int main(int argc, char ** argv) {
common_params params;
if (!common_params_parse(argc, argv, params, LLAMA_EXAMPLE_SPECULATIVE)) {
return 1;
}
if (params.n_predict < -1) {
LOG_ERR("%s: --n-predict must be >= -1\n", __func__);
return 1;
}
common_init();
if (params.speculative.model.path.empty()) {
LOG_ERR("%s: --model-draft is required\n", __func__);
return 1;
}
llama_backend_init();
llama_numa_init(params.numa);
llama_model * model_tgt = NULL;
llama_context * ctx_tgt = NULL;
llama_context * ctx_dft = NULL;
common_init_result llama_init_tgt = common_init_from_params(params);
model_tgt = llama_init_tgt.model.get();
ctx_tgt   = llama_init_tgt.context.get();
const llama_vocab * vocab = llama_model_get_vocab(model_tgt);
params.devices      = params.speculative.devices;
params.model        = params.speculative.model;
params.n_ctx        = params.speculative.n_ctx;
params.n_batch      = params.speculative.n_ctx > 0 ? params.speculative.n_ctx : params.n_batch;
params.n_gpu_layers = params.speculative.n_gpu_layers;
if (params.speculative.cpuparams.n_threads > 0) {
params.cpuparams.n_threads = params.speculative.cpuparams.n_threads;
}
params.cpuparams_batch.n_threads = params.speculative.cpuparams_batch.n_threads;
common_init_result llama_init_dft = common_init_from_params(params);
ctx_dft   = llama_init_dft.context.get();
if (!common_speculative_are_compatible(ctx_tgt, ctx_dft)) {
LOG_INF("the draft model '%s' is not compatible with the target model '%s'. tokens will be translated between the draft and target models.\n", params.speculative.model.path.c_str(), params.model.path.c_str());
}
std::vector<llama_token> inp;
inp = common_tokenize(ctx_tgt, params.prompt, true, true);
if (llama_n_ctx(ctx_tgt) < (uint32_t) inp.size()) {
LOG_ERR("%s: the prompt exceeds the context size (%d tokens, ctx %d)\n", __func__, (int) inp.size(), llama_n_ctx(ctx_tgt));
return 1;
}
if (llama_n_batch(ctx_tgt) < (uint32_t) inp.size()) {
LOG_ERR("%s: the prompt exceeds the batch size (%d tokens, batch %d)\n", __func__, (int) inp.size(), llama_n_batch(ctx_tgt));
return 1;
}
LOG("\n\n");
for (auto id : inp) {
LOG("%s", common_token_to_piece(ctx_tgt, id).c_str());
}
int n_draft     = params.speculative.n_max;
int n_draft_min = params.speculative.n_min;
float p_min = params.speculative.p_min;
int n_predict = 0;
int n_drafted = 0;
int n_accept  = 0;
bool has_eos = false;
const auto t_enc_start = ggml_time_us();
struct common_sampler * smpl = common_sampler_init(model_tgt, params.sampling);
llama_decode(ctx_tgt, llama_batch_get_one(inp.data(), inp.size() - 1));
llama_token id_last = inp.back();
llama_tokens prompt_tgt(inp.begin(), inp.end() - 1);
prompt_tgt.reserve(llama_n_ctx(ctx_tgt));
int n_past = inp.size() - 1;
struct common_speculative_params params_spec;
params_spec.n_draft = n_draft;
params_spec.n_reuse = llama_n_ctx(ctx_dft) - n_draft;
params_spec.p_min   = p_min;
struct common_speculative * spec = common_speculative_init(ctx_tgt, ctx_dft);
for (auto &pair : params.speculative.replacements) {
common_speculative_add_replacement_tgt_dft(spec, pair.first.c_str(), pair.second.c_str());
}
llama_batch batch_tgt = llama_batch_init(llama_n_batch(ctx_tgt), 0, 1);
const auto t_enc_end = ggml_time_us();
const auto t_dec_start = ggml_time_us();
while (true) {
llama_tokens draft = common_speculative_gen_draft(spec, params_spec, prompt_tgt, id_last);
common_batch_clear(batch_tgt);
common_batch_add  (batch_tgt, id_last, n_past++, { 0 }, true);
{
if (draft.size() < (size_t) n_draft_min) {
draft.clear();
}
for (size_t i = 0; i < draft.size(); ++i) {
common_batch_add(batch_tgt, draft[i], n_past + i, { 0 }, true);
}
llama_decode(ctx_tgt, batch_tgt);
}
const auto ids = common_sampler_sample_and_accept_n(smpl, ctx_tgt, draft);
GGML_ASSERT(ids.size() > 0);
n_past    += ids.size() - 1;
n_drafted += draft.size();
n_accept  += ids.size() - 1;
n_predict += ids.size();
for (size_t i = 0; i < ids.size(); ++i) {
prompt_tgt.push_back(id_last);
id_last = ids[i];
if (llama_vocab_is_eog(vocab, id_last)) {
has_eos = true;
break;
}
const std::string token_str = common_token_to_piece(ctx_tgt, id_last);
if (params.use_color && i + 1 < ids.size()) {
LOG("\u001b[%dm%s\u001b[37m", (36 - 0 % 6), token_str.c_str());
} else {
LOG("%s", token_str.c_str());
}
}
LOG_DBG("accepted %d/%d draft tokens, the last target token is: (%d)\n", (int) ids.size() - 1, (int) draft.size(), id_last);
{
LOG_DBG("clear kv cache from any extra tokens, n_past = %d\n", n_past);
llama_memory_seq_rm(llama_get_memory(ctx_tgt), 0, n_past, -1);
}
if ((params.n_predict >= 0 && n_predict > params.n_predict) || has_eos) {
break;
}
}
auto t_dec_end = ggml_time_us();
const int n_input = inp.size();
LOG("\n\n");
LOG_INF("encoded %4d tokens in %8.3f seconds, speed: %8.3f t/s\n", n_input,   (t_enc_end - t_enc_start) / 1e6f, inp.size() / ((t_enc_end - t_enc_start) / 1e6f));
LOG_INF("decoded %4d tokens in %8.3f seconds, speed: %8.3f t/s\n", n_predict, (t_dec_end - t_dec_start) / 1e6f, n_predict  / ((t_dec_end - t_dec_start) / 1e6f));
LOG_INF("\n");
LOG_INF("n_draft   = %d\n", n_draft);
LOG_INF("n_predict = %d\n", n_predict);
LOG_INF("n_drafted = %d\n", n_drafted);
LOG_INF("n_accept  = %d\n", n_accept);
LOG_INF("accept    = %.3f%%\n", 100.0f * n_accept / n_drafted);
LOG_INF("\n");
LOG_INF("draft:\n\n");
llama_perf_context_print(ctx_dft);
LOG_INF("\n");
LOG_INF("target:\n\n");
common_perf_print(ctx_tgt, smpl);
common_sampler_free(smpl);
common_speculative_free(spec);
llama_backend_free();
LOG("\n\n");
return 0;
}