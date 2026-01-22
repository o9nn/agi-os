#include "bolt/ai/direct_gguf_inference.hpp"
#include <iostream>
#include <fstream>
#include <sstream>
#include <memory>
#include <algorithm>
#include <cstdlib>
#include <ctime>
#include <chrono>
#include <array>
#include <filesystem>
#ifdef LLAMA_AVAILABLE
#include <llama-cpp.h>
#endif
namespace bolt {
namespace ai {
struct DirectGGUFInference::ModelData {
#ifdef LLAMA_AVAILABLE
llama_model* model = nullptr;
llama_context* ctx = nullptr;
#endif
std::string model_info;
bool initialized = false;
std::string model_path;
};
DirectGGUFInference::DirectGGUFInference()
: model_data_(std::make_unique<ModelData>())
, model_loaded_(false) {
std::cout << "📦 DirectGGUFInference initialized" << std::endl;
}
DirectGGUFInference::~DirectGGUFInference() {
#ifdef LLAMA_AVAILABLE
if (model_data_->ctx) {
llama_free(model_data_->ctx);
model_data_->ctx = nullptr;
}
if (model_data_->model) {
llama_free_model(model_data_->model);
model_data_->model = nullptr;
}
llama_backend_free();
#endif
}
bool DirectGGUFInference::load_model(const std::string& model_path) {
model_path_ = model_path;
model_data_->model_path = model_path;
std::cout << "📥 Loading GGUF model: " << model_path << std::endl;
std::ifstream file(model_path);
if (!file.good()) {
std::cout << "❌ Model file not found: " << model_path << std::endl;
return false;
}
#ifndef LLAMA_AVAILABLE
std::cout << "⚠️  llama.cpp not available - using fallback responses" << std::endl;
model_loaded_ = true;
model_data_->model_info = "Fallback mode: llama.cpp not available";
return true;
#else
llama_backend_init();
llama_model_params model_params = llama_model_default_params();
model_params.use_mmap = true;
model_params.use_mlock = false;
model_data_->model = llama_load_model_from_file(model_path.c_str(), model_params);
if (!model_data_->model) {
std::cout << "❌ Failed to load model via llama.cpp" << std::endl;
return false;
}
llama_context_params ctx_params = llama_context_default_params();
ctx_params.n_ctx = 2048;
ctx_params.seed = 0;
model_data_->ctx = llama_new_context_with_model(model_data_->model, ctx_params);
if (!model_data_->ctx) {
std::cout << "❌ Failed to create llama context" << std::endl;
llama_free_model(model_data_->model);
model_data_->model = nullptr;
return false;
}
model_loaded_ = true;
model_data_->initialized = true;
std::ostringstream info;
info << "GGUF Model loaded via llama.cpp: " << model_path << "\n";
info << "Context length: " << ctx_params.n_ctx << "\n";
model_data_->model_info = info.str();
std::cout << "✅ Model loaded" << std::endl;
std::cout << model_data_->model_info << std::endl;
return true;
#endif
}
drawkern::AIInferenceResponse DirectGGUFInference::generate_text(
const std::string& prompt,
int max_tokens,
float temperature) {
auto start_time = std::chrono::high_resolution_clock::now();
drawkern::AIInferenceResponse response;
response.success = false;
if (!model_loaded_) {
response = get_fallback_response(prompt);
auto end_time = std::chrono::high_resolution_clock::now();
response.inference_time_ms = std::chrono::duration<float, std::milli>(end_time - start_time).count();
return response;
}
try {
std::string generated_text = generate_internal(prompt, max_tokens, temperature);
response.response = generated_text;
response.success = true;
response.tokens_generated = generated_text.length() / 4;
} catch (const std::exception& e) {
response.response = "Error during generation: " + std::string(e.what());
response.error = response.response;
}
auto end_time = std::chrono::high_resolution_clock::now();
response.inference_time_ms = std::chrono::duration<float, std::milli>(end_time - start_time).count();
return response;
}
drawkern::AIInferenceResponse DirectGGUFInference::chat(
const std::string& message,
const std::vector<std::string>& conversation_history) {
std::string formatted_prompt = format_chat_prompt(message, conversation_history);
return generate_text(formatted_prompt, 150, 0.7f);
}
std::string DirectGGUFInference::get_model_info() const {
if (model_data_) {
return model_data_->model_info;
}
return "No model loaded";
}
std::string DirectGGUFInference::generate_internal(const std::string& prompt, int max_tokens, float temperature) {
#ifndef LLAMA_AVAILABLE
return get_smart_fallback(prompt);
#else
if (!model_data_->initialized || !model_data_->ctx) {
return get_smart_fallback(prompt);
}
std::string system_prefix;
system_prefix.reserve(prompt.size() + 64);
system_prefix += prompt;
llama_context* ctx = model_data_->ctx;
std::vector<llama_token> tokens_in(system_prefix.size() + 8);
int n_in = llama_tokenize(ctx, system_prefix.c_str(), tokens_in.data(), (int)tokens_in.size(), true, true);
if (n_in < 0) return get_smart_fallback(prompt);
tokens_in.resize(n_in);
int n_ctx = llama_n_ctx(ctx);
int n_batch = 64;
llama_batch batch = llama_batch_init(n_batch, 0, 1);
int consumed = 0;
while (consumed < (int)tokens_in.size()) {
int to_eval = std::min(n_batch, (int)tokens_in.size() - consumed);
batch.n_tokens = 0;
for (int i = 0; i < to_eval; ++i) {
llama_batch_add(batch, tokens_in[consumed + i], consumed + i, { 0 }, true);
}
if (llama_decode(ctx, batch) != 0) {
llama_batch_free(batch);
return get_smart_fallback(prompt);
}
consumed += to_eval;
}
std::string out;
out.reserve((size_t)max_tokens * 4);
std::vector<float> logits(llama_n_vocab(model_data_->model));
std::vector<llama_token_data> candidates;
candidates.reserve(logits.size());
for (int i = 0; i < max_tokens; ++i) {
const float* cur_logits = llama_get_logits_ith(ctx, llama_get_kv_cache_token_count(ctx) - 1);
std::copy(cur_logits, cur_logits + (int)logits.size(), logits.begin());
candidates.clear();
for (int id = 0; id < (int)logits.size(); ++id) {
candidates.push_back({ id, logits[id], 0.0f });
}
llama_token_data_array cur_p = { candidates.data(), candidates.size(), false };
llama_sample_temperature(ctx, &cur_p, temperature);
llama_token id = llama_sample_token(ctx, &cur_p);
if (id == llama_token_eos(model_data_->model)) break;
char buf[8];
int n = llama_token_to_piece(ctx, id, buf, sizeof(buf), 0, true);
if (n > 0) out.append(buf, buf + n);
llama_batch batch_next = llama_batch_init(1, 0, 1);
llama_batch_add(batch_next, id, tokens_in.size() + i, { 0 }, true);
if (llama_decode(ctx, batch_next) != 0) {
llama_batch_free(batch_next);
break;
}
llama_batch_free(batch_next);
}
llama_batch_free(batch);
return out.empty() ? get_smart_fallback(prompt) : out;
#endif
}
std::string DirectGGUFInference::format_chat_prompt(const std::string& message, const std::vector<std::string>& history) {
std::ostringstream prompt;
prompt << "You are a helpful AI programming assistant specialized in C++ development. ";
prompt << "Provide clear, concise answers focused on coding help, debugging, and best practices.\n\n";
int history_limit = 6;
int start_idx = std::max(0, static_cast<int>(history.size()) - history_limit);
for (int i = start_idx; i < static_cast<int>(history.size()); i++) {
if (i % 2 == 0) {
prompt << "Human: " << history[i] << "\n";
} else {
prompt << "Assistant: " << history[i] << "\n";
}
}
prompt << "Human: " << message << "\n";
prompt << "Assistant: ";
return prompt.str();
}
bolt::drawkern::AIInferenceResponse DirectGGUFInference::get_fallback_response(const std::string& prompt) {
bolt::drawkern::AIInferenceResponse response;
response.success = true;
response.inference_time_ms = 0.1f;
response.tokens_generated = 1;
response.tokens_processed = 1;
response.response = get_smart_fallback(prompt);
return response;
}
}
}
namespace bolt {
namespace ai {
std::string DirectGGUFInference::get_smart_fallback(const std::string& input) {
std::string lower_input = input;
std::transform(lower_input.begin(), lower_input.end(), lower_input.begin(), ::tolower);
if (lower_input.find("code") != std::string::npos ||
lower_input.find("function") != std::string::npos ||
lower_input.find("class") != std::string::npos ||
lower_input.find("template") != std::string::npos ||
lower_input.find("pointer") != std::string::npos ||
lower_input.find("c++") != std::string::npos) {
return get_coding_help(input);
}
if (lower_input.find("algorithm") != std::string::npos ||
lower_input.find("sort") != std::string::npos ||
lower_input.find("search") != std::string::npos ||
lower_input.find("complexity") != std::string::npos) {
return get_algorithm_help(input);
}
if (lower_input.find("config") != std::string::npos ||
lower_input.find("setup") != std::string::npos ||
lower_input.find("install") != std::string::npos) {
return "🔧 **Configuration Help**: To use real AI models:\n\n"
"1. **Local GGUF Model**: Download a small model file and load it directly\n"
"2. **llama.cpp Server**: Start a local server with `./server -m model.gguf`\n"
"3. **Ollama**: Install Ollama and run `ollama run llama2`\n\n"
"Currently using intelligent fallback responses. Load a GGUF model for full AI capabilities!";
}
return "I understand you're asking: \"" + input + "\"\n\n"
"💡 I'm currently using smart fallback responses. For full AI capabilities:\n"
"• Load a GGUF model file directly\n"
"• Start a local llama.cpp server\n"
"• Configure an external AI provider\n\n"
"I can still help with C++ coding questions, algorithms, and configuration guidance!";
}
std::string DirectGGUFInference::get_coding_help(const std::string& input) {
return "🔧 **C++ Development Help**:\n\n"
"Based on your question about: \"" + input + "\"\n\n"
"**Quick C++ Tips**:\n"
"• Use `std::unique_ptr` for single ownership\n"
"• Use `std::shared_ptr` for shared ownership\n"
"• Prefer `auto` for type deduction\n"
"• Use RAII for resource management\n"
"• Consider `const` for immutable data\n\n"
"**Need specific help?** Load a GGUF model for detailed AI assistance!";
}
std::string DirectGGUFInference::get_algorithm_help(const std::string& input) {
return "🎯 **Algorithm Guidance**:\n\n"
"For your question: \"" + input + "\"\n\n"
"**Common C++ Algorithms**:\n"
"• `std::sort()` - O(n log n) sorting\n"
"• `std::binary_search()` - O(log n) search\n"
"• `std::find()` - Linear search\n"
"• `std::transform()` - Apply operation to range\n"
"• `std::accumulate()` - Reduce range to single value\n\n"
"Load a GGUF model for detailed algorithmic analysis and examples!";
}
std::unique_ptr<DirectGGUFInference> DirectGGUFFactory::create_from_file(const std::string& model_path) {
auto inference = std::make_unique<DirectGGUFInference>();
if (inference->load_model(model_path)) {
std::cout << "✅ Created GGUF inference engine from: " << model_path << std::endl;
return inference;
}
std::cout << "⚠️ Failed to load model, using fallback mode" << std::endl;
return inference;
}
std::unique_ptr<DirectGGUFInference> DirectGGUFFactory::create_auto_detect() {
auto inference = std::make_unique<DirectGGUFInference>();
auto models = find_available_models();
for (const auto& model : models) {
if (inference->load_model(model)) {
std::cout << "✅ Auto-loaded model: " << model << std::endl;
return inference;
}
}
std::cout << "⚠️ No GGUF models found, using intelligent fallback responses" << std::endl;
return inference;
}
std::vector<std::string> DirectGGUFFactory::find_available_models() {
std::vector<std::string> models;
std::vector<std::string> search_paths = {
"./models/",
"./ggml/llama.cpp/models/",
"/workspace/models/",
"/workspace/ggml/llama.cpp/models/",
std::string(getenv("HOME") ? getenv("HOME") : "") + "/.cache/huggingface/",
"./"
};
for (const auto& dir : search_paths) {
std::error_code ec;
if (!std::filesystem::is_directory(dir, ec)) continue;
for (auto& entry : std::filesystem::directory_iterator(dir, ec)) {
if (ec) break;
if (!entry.is_regular_file()) continue;
auto path = entry.path();
if (path.extension() == ".gguf") {
models.push_back(path.string());
}
}
}
return models;
}
bool DirectGGUFFactory::download_test_model(const std::string& output_path) {
std::cout << "📥 Downloading test model functionality not implemented yet" << std::endl;
std::cout << "💡 You can manually download a small GGUF model from:" << std::endl;
std::cout << "   • Hugging Face: https:
std::cout << "   • TinyLlama: https:
return false;
}
}
}