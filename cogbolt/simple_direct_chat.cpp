#include <iostream>
#include <string>
#include <vector>
#include <filesystem>
extern "C" {
#include "ggml/llama.cpp/ggml/include/ggml.h"
#include "ggml/llama.cpp/include/llama.h"
}
class DirectFileChat {
private:
llama_model* model = nullptr;
llama_context* ctx = nullptr;
std::string model_path;
public:
~DirectFileChat() {
if (ctx) llama_free(ctx);
if (model) llama_free_model(model);
llama_backend_free();
}
bool load_model() {
model_path = "/workspaces/bolt-cppml/models/TinyLlama-1.1B-Chat-v1.0-GGUF/tinyllama-1.1b-chat-v1.0.Q3_K_M.gguf";
if (!std::filesystem::exists(model_path)) {
std::cerr << "❌ Model file not found: " << model_path << std::endl;
return false;
}
std::cout << "📥 Loading model from file: " << model_path << std::endl;
std::cout << "📊 File size: " << (std::filesystem::file_size(model_path) / 1024 / 1024) << " MB" << std::endl;
llama_backend_init();
llama_model_params model_params = llama_model_default_params();
model_params.n_gpu_layers = 0;
model_params.use_mmap = true;
model = llama_load_model_from_file(model_path.c_str(), model_params);
if (!model) {
std::cerr << "❌ Failed to load model from file" << std::endl;
return false;
}
llama_context_params ctx_params = llama_context_default_params();
ctx_params.n_ctx = 512;
ctx_params.seed = 42;
ctx_params.n_threads = 4;
ctx = llama_new_context_with_model(model, ctx_params);
if (!ctx) {
std::cerr << "❌ Failed to create context" << std::endl;
return false;
}
std::cout << "✅ Model loaded directly from file!" << std::endl;
std::cout << "📋 Vocab size: " << llama_n_vocab(model) << std::endl;
std::cout << "📏 Context size: " << llama_n_ctx(ctx) << std::endl;
return true;
}
std::string chat(const std::string& input) {
std::string prompt = "<|user|>\n" + input + "\n<|assistant|>\n";
std::vector<llama_token> tokens;
tokens.resize(prompt.length() + 100);
int n_tokens = llama_tokenize(model, prompt.c_str(), prompt.length(), tokens.data(), tokens.size(), true, false);
if (n_tokens < 0) {
return "❌ Failed to tokenize input";
}
tokens.resize(n_tokens);
std::cout << "🔢 Input tokens: " << n_tokens << std::endl;
llama_kv_cache_clear(ctx);
if (llama_decode(ctx, llama_batch_get_one(tokens.data(), n_tokens, 0, 0))) {
return "❌ Failed to process prompt";
}
std::string response;
int max_tokens = 50;
std::cout << "🤖 Generating... ";
for (int i = 0; i < max_tokens; i++) {
auto* logits = llama_get_logits(ctx);
llama_token next_token = 0;
float max_logit = logits[0];
for (llama_token token = 1; token < llama_n_vocab(model); token++) {
if (logits[token] > max_logit) {
max_logit = logits[token];
next_token = token;
}
}
if (next_token == llama_token_eos(model)) {
break;
}
char text_buf[256];
int n_chars = llama_token_to_piece(model, next_token, text_buf, sizeof(text_buf), 0, false);
if (n_chars > 0) {
response.append(text_buf, n_chars);
std::cout << std::flush;
}
if (llama_decode(ctx, llama_batch_get_one(&next_token, 1, n_tokens + i, 0))) {
break;
}
}
std::cout << " Done!" << std::endl;
return response;
}
};
int main() {
std::cout << "🦙 Direct File Access Chat - No Server Required!" << std::endl;
std::cout << "=================================================" << std::endl;
DirectFileChat chat;
if (!chat.load_model()) {
std::cout << "❌ Failed to load model. Make sure TinyLlama is in the models directory." << std::endl;
return 1;
}
std::cout << "\n💬 Direct file chat ready! Type your messages (or 'quit' to exit):" << std::endl;
std::cout << "─────────────────────────────────────────────────────────────────" << std::endl;
std::string input;
while (true) {
std::cout << "\n👤 You: ";
if (!std::getline(std::cin, input) || input == "quit" || input == "exit") {
break;
}
if (input.empty()) continue;
std::cout << "🤖 AI: " << chat.chat(input) << std::endl;
std::cout << "─────────────────────────────────────────────────────────────────" << std::endl;
}
std::cout << "👋 Goodbye!" << std::endl;
return 0;
}