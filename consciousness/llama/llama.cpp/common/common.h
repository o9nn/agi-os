#pragma once
#include "llama-cpp.h"
#include <set>
#include <string>
#include <vector>
#include <sstream>
#ifdef _WIN32
#define DIRECTORY_SEPARATOR '\\'
#else
#define DIRECTORY_SEPARATOR '/'
#endif
#define die(msg) do { fputs("error: " msg "\n", stderr); exit(1); } while (0)
#define die_fmt(fmt, ...) do { fprintf(stderr, "error: " fmt "\n", __VA_ARGS__); exit(1); } while (0)
#define print_build_info() do { \
fprintf(stderr, "%s: build = %d (%s)\n", __func__, LLAMA_BUILD_NUMBER, LLAMA_COMMIT); \
fprintf(stderr, "%s: built with %s for %s\n", __func__, LLAMA_COMPILER, LLAMA_BUILD_TARGET); \
} while(0)
#define DEFAULT_MODEL_PATH "models/7B/ggml-model-f16.gguf"
struct common_adapter_lora_info {
std::string path;
float scale;
struct llama_adapter_lora * ptr;
};
using llama_tokens = std::vector<llama_token>;
extern int LLAMA_BUILD_NUMBER;
extern const char * LLAMA_COMMIT;
extern const char * LLAMA_COMPILER;
extern const char * LLAMA_BUILD_TARGET;
struct common_control_vector_load_info;
struct cpu_params {
int n_threads = -1;
bool cpumask[GGML_MAX_N_THREADS] = {false};
bool mask_valid = false;
enum ggml_sched_priority priority = GGML_SCHED_PRIO_NORMAL;
bool strict_cpu = false;
uint32_t poll = 50;
};
int32_t cpu_get_num_physical_cores();
int32_t cpu_get_num_math();
enum llama_example {
LLAMA_EXAMPLE_COMMON,
LLAMA_EXAMPLE_SPECULATIVE,
LLAMA_EXAMPLE_MAIN,
LLAMA_EXAMPLE_EMBEDDING,
LLAMA_EXAMPLE_PERPLEXITY,
LLAMA_EXAMPLE_RETRIEVAL,
LLAMA_EXAMPLE_PASSKEY,
LLAMA_EXAMPLE_IMATRIX,
LLAMA_EXAMPLE_BENCH,
LLAMA_EXAMPLE_SERVER,
LLAMA_EXAMPLE_CVECTOR_GENERATOR,
LLAMA_EXAMPLE_EXPORT_LORA,
LLAMA_EXAMPLE_LLAVA,
LLAMA_EXAMPLE_LOOKUP,
LLAMA_EXAMPLE_PARALLEL,
LLAMA_EXAMPLE_TTS,
LLAMA_EXAMPLE_COUNT,
};
enum common_sampler_type {
COMMON_SAMPLER_TYPE_NONE = 0,
COMMON_SAMPLER_TYPE_DRY = 1,
COMMON_SAMPLER_TYPE_TOP_K = 2,
COMMON_SAMPLER_TYPE_TOP_P = 3,
COMMON_SAMPLER_TYPE_MIN_P = 4,
COMMON_SAMPLER_TYPE_TYPICAL_P = 6,
COMMON_SAMPLER_TYPE_TEMPERATURE = 7,
COMMON_SAMPLER_TYPE_XTC = 8,
COMMON_SAMPLER_TYPE_INFILL = 9,
COMMON_SAMPLER_TYPE_PENALTIES = 10,
COMMON_SAMPLER_TYPE_TOP_N_SIGMA = 11,
};
enum dimre_method {
DIMRE_METHOD_PCA,
DIMRE_METHOD_MEAN,
};
enum common_conversation_mode {
COMMON_CONVERSATION_MODE_DISABLED = 0,
COMMON_CONVERSATION_MODE_ENABLED = 1,
COMMON_CONVERSATION_MODE_AUTO = 2,
};
enum common_grammar_trigger_type {
COMMON_GRAMMAR_TRIGGER_TYPE_TOKEN,
COMMON_GRAMMAR_TRIGGER_TYPE_WORD,
COMMON_GRAMMAR_TRIGGER_TYPE_PATTERN,
COMMON_GRAMMAR_TRIGGER_TYPE_PATTERN_START,
};
struct common_grammar_trigger {
common_grammar_trigger_type type;
std::string value;
llama_token token = LLAMA_TOKEN_NULL;
};
struct common_params_sampling {
uint32_t seed = LLAMA_DEFAULT_SEED;
int32_t n_prev = 64;
int32_t n_probs = 0;
int32_t min_keep = 0;
int32_t top_k = 40;
float top_p = 0.95f;
float min_p = 0.05f;
float xtc_probability = 0.00f;
float xtc_threshold = 0.10f;
float typ_p = 1.00f;
float temp = 0.80f;
float dynatemp_range = 0.00f;
float dynatemp_exponent = 1.00f;
int32_t penalty_last_n = 64;
float penalty_repeat = 1.00f;
float penalty_freq = 0.00f;
float penalty_present = 0.00f;
float dry_multiplier = 0.0f;
float dry_base = 1.75f;
int32_t dry_allowed_length = 2;
int32_t dry_penalty_last_n = -1;
int32_t mirostat = 0;
float top_n_sigma = -1.00f;
float mirostat_tau = 5.00f;
float mirostat_eta = 0.10f;
bool ignore_eos = false;
bool no_perf = false;
bool timing_per_token = false;
std::vector<std::string> dry_sequence_breakers = {"\n", ":", "\"", "*"};
std::vector<enum common_sampler_type> samplers = {
COMMON_SAMPLER_TYPE_PENALTIES,
COMMON_SAMPLER_TYPE_DRY,
COMMON_SAMPLER_TYPE_TOP_N_SIGMA,
COMMON_SAMPLER_TYPE_TOP_K,
COMMON_SAMPLER_TYPE_TYPICAL_P,
COMMON_SAMPLER_TYPE_TOP_P,
COMMON_SAMPLER_TYPE_MIN_P,
COMMON_SAMPLER_TYPE_XTC,
COMMON_SAMPLER_TYPE_TEMPERATURE,
};
std::string grammar;
bool grammar_lazy = false;
std::vector<common_grammar_trigger> grammar_triggers;
std::set<llama_token> preserved_tokens;
std::vector<llama_logit_bias> logit_bias;
std::string print() const;
};
struct common_params_model {
std::string path = "";
std::string url = "";
std::string hf_repo = "";
std::string hf_file = "";
};
struct common_params_speculative {
std::vector<ggml_backend_dev_t> devices;
int32_t n_ctx = 0;
int32_t n_max = 16;
int32_t n_min = 0;
int32_t n_gpu_layers = -1;
float p_split = 0.1f;
float p_min = 0.75f;
struct cpu_params cpuparams;
struct cpu_params cpuparams_batch;
struct common_params_model model;
};
struct common_params_vocoder {
struct common_params_model model;
std::string speaker_file = "";
bool use_guide_tokens = false;
};
enum common_reasoning_format {
COMMON_REASONING_FORMAT_NONE,
COMMON_REASONING_FORMAT_DEEPSEEK,
};
struct common_params {
int32_t n_predict = -1;
int32_t n_ctx = 4096;
int32_t n_batch = 2048;
int32_t n_ubatch = 512;
int32_t n_keep = 0;
int32_t n_chunks = -1;
int32_t n_parallel = 1;
int32_t n_sequences = 1;
int32_t grp_attn_n = 1;
int32_t grp_attn_w = 512;
int32_t n_print = -1;
float rope_freq_base = 0.0f;
float rope_freq_scale = 0.0f;
float yarn_ext_factor = -1.0f;
float yarn_attn_factor = 1.0f;
float yarn_beta_fast = 32.0f;
float yarn_beta_slow = 1.0f;
int32_t yarn_orig_ctx = 0;
float defrag_thold = 0.1f;
std::vector<ggml_backend_dev_t> devices;
int32_t n_gpu_layers = -1;
int32_t main_gpu = 0;
float tensor_split[128] = {0};
enum llama_split_mode split_mode = LLAMA_SPLIT_MODE_LAYER;
struct cpu_params cpuparams;
struct cpu_params cpuparams_batch;
ggml_backend_sched_eval_callback cb_eval = nullptr;
void * cb_eval_user_data = nullptr;
ggml_numa_strategy numa = GGML_NUMA_STRATEGY_DISABLED;
enum llama_rope_scaling_type rope_scaling_type = LLAMA_ROPE_SCALING_TYPE_UNSPECIFIED;
enum llama_pooling_type pooling_type = LLAMA_POOLING_TYPE_UNSPECIFIED;
enum llama_attention_type attention_type = LLAMA_ATTENTION_TYPE_UNSPECIFIED;
struct common_params_sampling sampling;
struct common_params_speculative speculative;
struct common_params_vocoder vocoder;
struct common_params_model model;
std::string model_alias = "";
std::string hf_token = "";
std::string prompt = "";
std::string system_prompt = "";
std::string prompt_file = "";
std::string path_prompt_cache = "";
std::string input_prefix = "";
std::string input_suffix = "";
std::string lookup_cache_static = "";
std::string lookup_cache_dynamic = "";
std::string logits_file = "";
std::vector<std::string> in_files;
std::vector<std::string> antiprompt;
std::vector<llama_model_kv_override> kv_overrides;
std::vector<llama_model_tensor_buft_override> tensor_buft_overrides;
bool lora_init_without_apply = false;
std::vector<common_adapter_lora_info> lora_adapters;
std::vector<common_control_vector_load_info> control_vectors;
int32_t verbosity = 0;
int32_t control_vector_layer_start = -1;
int32_t control_vector_layer_end = -1;
int32_t ppl_stride = 0;
int32_t ppl_output_type = 0;
bool hellaswag = false;
size_t hellaswag_tasks = 400;
bool winogrande = false;
size_t winogrande_tasks = 0;
bool multiple_choice = false;
size_t multiple_choice_tasks = 0;
bool kl_divergence = false;
bool usage = false;
bool completion = false;
bool use_color = false;
bool special = false;
bool interactive = false;
bool interactive_first = false;
bool prompt_cache_all = false;
bool prompt_cache_ro = false;
bool escape = true;
bool multiline_input = false;
bool simple_io = false;
bool cont_batching = true;
bool flash_attn = false;
bool no_perf = false;
bool ctx_shift = true;
bool input_prefix_bos = false;
bool use_mmap = true;
bool use_mlock = false;
bool verbose_prompt = false;
bool display_prompt = true;
bool dump_kv_cache = false;
bool no_kv_offload = false;
bool warmup = true;
bool check_tensors = false;
bool no_op_offload = false;
bool single_turn = false;
ggml_type cache_type_k = GGML_TYPE_F16;
ggml_type cache_type_v = GGML_TYPE_F16;
common_conversation_mode conversation_mode = COMMON_CONVERSATION_MODE_AUTO;
struct common_params_model mmproj;
bool mmproj_use_gpu = true;
bool no_mmproj = false;
std::vector<std::string> image;
bool embedding = false;
int32_t embd_normalize = 2;
std::string embd_out = "";
std::string embd_sep = "\n";
bool reranking = false;
int32_t port = 8080;
int32_t timeout_read = 600;
int32_t timeout_write = timeout_read;
int32_t n_threads_http = -1;
int32_t n_cache_reuse = 0;
std::string hostname = "127.0.0.1";
std::string public_path = "";
std::string chat_template = "";
bool use_jinja = false;
bool enable_chat_template = true;
common_reasoning_format reasoning_format = COMMON_REASONING_FORMAT_DEEPSEEK;
std::vector<std::string> api_keys;
std::string ssl_file_key = "";
std::string ssl_file_cert = "";
bool webui = true;
bool endpoint_slots = false;
bool endpoint_props = false;
bool endpoint_metrics = false;
bool log_json = false;
std::string slot_save_path;
float slot_prompt_similarity = 0.5f;
bool is_pp_shared = false;
std::vector<int32_t> n_pp;
std::vector<int32_t> n_tg;
std::vector<int32_t> n_pl;
std::vector<std::string> context_files;
int32_t chunk_size = 64;
std::string chunk_separator = "\n";
int32_t n_junk = 250;
int32_t i_pos = -1;
int32_t n_out_freq = 10;
int32_t n_save_freq = 0;
int32_t i_chunk = 0;
bool process_output = false;
bool compute_ppl = true;
bool parse_special = false;
int n_pca_batch = 100;
int n_pca_iterations = 1000;
dimre_method cvector_dimre_method = DIMRE_METHOD_PCA;
std::string cvector_positive_file = "tools/cvector-generator/positive.txt";
std::string cvector_negative_file = "tools/cvector-generator/negative.txt";
bool spm_infill = false;
bool batched_bench_output_jsonl = false;
std::string out_file;
};
void common_init();
std::string common_params_get_system_info(const common_params & params);
bool parse_cpu_range(const std::string & range, bool(&boolmask)[GGML_MAX_N_THREADS]);
bool parse_cpu_mask(const std::string & mask, bool(&boolmask)[GGML_MAX_N_THREADS]);
void postprocess_cpu_params(cpu_params & cpuparams, const cpu_params * role_model = nullptr);
bool set_process_priority(enum ggml_sched_priority prio);
#ifdef __GNUC__
# if defined(__MINGW32__) && !defined(__clang__)
# define LLAMA_COMMON_ATTRIBUTE_FORMAT(...) __attribute__((format(gnu_printf, __VA_ARGS__)))
# else
# define LLAMA_COMMON_ATTRIBUTE_FORMAT(...) __attribute__((format(printf, __VA_ARGS__)))
# endif
#else
# define LLAMA_COMMON_ATTRIBUTE_FORMAT(...)
#endif
LLAMA_COMMON_ATTRIBUTE_FORMAT(1, 2)
std::string string_format(const char * fmt, ...);
std::string string_strip(const std::string & str);
std::string string_get_sortable_timestamp();
std::string string_join(const std::vector<std::string> & values, const std::string & separator);
std::vector<std::string> string_split(const std::string & str, const std::string & delimiter);
std::string string_repeat(const std::string & str, size_t n);
void string_replace_all(std::string & s, const std::string & search, const std::string & replace);
std::string regex_escape(const std::string & s);
template<class T>
static std::vector<T> string_split(const std::string & str, char delim) {
static_assert(!std::is_same<T, std::string>::value, "Please use the specialized version for std::string");
std::vector<T> values;
std::istringstream str_stream(str);
std::string token;
while (std::getline(str_stream, token, delim)) {
T value;
std::istringstream token_stream(token);
token_stream >> value;
values.push_back(value);
}
return values;
}
template<>
std::vector<std::string> string_split<std::string>(const std::string & input, char separator)
{
std::vector<std::string> parts;
size_t begin_pos = 0;
size_t separator_pos = input.find(separator);
while (separator_pos != std::string::npos) {
std::string part = input.substr(begin_pos, separator_pos - begin_pos);
parts.emplace_back(part);
begin_pos = separator_pos + 1;
separator_pos = input.find(separator, begin_pos);
}
parts.emplace_back(input.substr(begin_pos, separator_pos - begin_pos));
return parts;
}
static bool string_starts_with(const std::string & str,
const std::string & prefix) {
return str.rfind(prefix, 0) == 0;
}
static bool string_ends_with(const std::string & str,
const std::string & suffix) {
return str.size() >= suffix.size() && str.compare(str.size()-suffix.size(), suffix.size(), suffix) == 0;
}
bool string_parse_kv_override(const char * data, std::vector<llama_model_kv_override> & overrides);
void string_process_escapes(std::string & input);
std::string string_from(bool value);
std::string string_from(const std::vector<int> & values);
std::string string_from(const struct llama_context * ctx, const std::vector<llama_token> & tokens);
std::string string_from(const struct llama_context * ctx, const struct llama_batch & batch);
bool fs_validate_filename(const std::string & filename);
bool fs_create_directory_with_parents(const std::string & path);
std::string fs_get_cache_directory();
std::string fs_get_cache_file(const std::string & filename);
struct common_init_result {
llama_model_ptr model;
llama_context_ptr context;
std::vector<llama_adapter_lora_ptr> lora;
};
struct common_init_result common_init_from_params(common_params & params);
struct llama_model_params common_model_params_to_llama ( common_params & params);
struct llama_context_params common_context_params_to_llama(const common_params & params);
struct ggml_threadpool_params ggml_threadpool_params_from_cpu_params(const cpu_params & params);
void common_set_adapter_lora(struct llama_context * ctx, std::vector<common_adapter_lora_info> & lora);
std::string get_model_endpoint();
void common_batch_clear(struct llama_batch & batch);
void common_batch_add(
struct llama_batch & batch,
llama_token id,
llama_pos pos,
const std::vector<llama_seq_id> & seq_ids,
bool logits);
size_t common_lcp(const llama_tokens & a, const llama_tokens & b);
size_t common_lcs(const llama_tokens & a, const llama_tokens & b);
std::vector<llama_token> common_tokenize(
const struct llama_context * ctx,
const std::string & text,
bool add_special,
bool parse_special = false);
std::vector<llama_token> common_tokenize(
const struct llama_vocab * vocab,
const std::string & text,
bool add_special,
bool parse_special = false);
std::string common_token_to_piece(
const struct llama_context * ctx,
llama_token token,
bool special = true);
std::string common_token_to_piece(
const struct llama_vocab * vocab,
llama_token token,
bool special = true);
std::string common_detokenize(
const struct llama_context * ctx,
const std::vector<llama_token> & tokens,
bool special = true);
std::string common_detokenize(
const struct llama_vocab * vocab,
const std::vector<llama_token> & tokens,
bool special = true);
void common_kv_cache_dump_view(const llama_kv_cache_view & view, int row_size = 80);
void common_kv_cache_dump_view_seqs(const llama_kv_cache_view & view, int row_size = 40);
void common_embd_normalize(const float * inp, float * out, int n, int embd_norm);
float common_embd_similarity_cos(const float * embd1, const float * embd2, int n);
struct common_control_vector_data {
int n_embd;
std::vector<float> data;
};
struct common_control_vector_load_info {
float strength;
std::string fname;
};
common_control_vector_data common_control_vector_load(const std::vector<common_control_vector_load_info> & load_infos);
namespace {
const char * const LLM_KV_SPLIT_NO = "split.no";
const char * const LLM_KV_SPLIT_COUNT = "split.count";
const char * const LLM_KV_SPLIT_TENSORS_COUNT = "split.tensors.count";
}
ggml_opt_dataset_t common_opt_dataset_init(struct llama_context * ctx, const std::vector<llama_token> & tokens, int64_t stride);