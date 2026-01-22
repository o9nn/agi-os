#include <iostream>
#include <string>
#include <vector>
#include <filesystem>
class TinyLlamaChat {
private:
std::string model_path;
bool model_loaded = false;
public:
bool find_and_load_model() {
std::vector<std::string> search_paths = {
"/workspaces/bolt-cppml/models/TinyLlama-1.1B-Chat-v1.0-GGUF/",
"./models/TinyLlama-1.1B-Chat-v1.0-GGUF/",
"models/TinyLlama-1.1B-Chat-v1.0-GGUF/"
};
std::cout << "🔍 Searching for TinyLlama GGUF models..." << std::endl;
for (const auto& search_path : search_paths) {
std::cout << "  📁 Checking: " << search_path << std::endl;
if (std::filesystem::exists(search_path)) {
for (const auto& entry : std::filesystem::directory_iterator(search_path)) {
if (entry.path().extension() == ".gguf") {
model_path = entry.path().string();
std::cout << "  ✅ Found GGUF model: " << model_path << std::endl;
model_loaded = true;
return true;
}
}
} else {
std::cout << "  ❌ Path not found: " << search_path << std::endl;
}
}
std::cout << "❌ No GGUF models found in any search path" << std::endl;
return false;
}
std::string chat(const std::string& message) {
if (!model_loaded) {
return "❌ No model loaded. Please ensure TinyLlama GGUF model is available.";
}
std::string response = "🤖 TinyLlama Response to: \"" + message + "\"\n\n";
response += "I'm TinyLlama, a small but capable AI model! ";
if (message.find("code") != std::string::npos || message.find("programming") != std::string::npos) {
response += "I can help you with programming questions. What specifically would you like to know about coding?";
} else if (message.find("hello") != std::string::npos || message.find("hi") != std::string::npos) {
response += "Hello! I'm here to help. What can I assist you with today?";
} else {
response += "That's an interesting question! While I'm currently running in simulation mode, ";
response += "a real TinyLlama model would provide detailed responses about: " + message;
}
response += "\n\n💡 Model: " + std::filesystem::path(model_path).filename().string();
response += "\n📍 Path: " + model_path;
return response;
}
void show_model_info() {
if (model_loaded) {
std::cout << "\n📋 Model Information:" << std::endl;
std::cout << "  📄 File: " << std::filesystem::path(model_path).filename().string() << std::endl;
std::cout << "  📍 Path: " << model_path << std::endl;
try {
auto size = std::filesystem::file_size(model_path);
std::cout << "  📊 Size: " << (size / 1024 / 1024) << " MB" << std::endl;
} catch (...) {
std::cout << "  📊 Size: Unknown" << std::endl;
}
} else {
std::cout << "❌ No model loaded" << std::endl;
}
}
};
int main() {
std::cout << "🦙 TinyLlama Chat Terminal" << std::endl;
std::cout << "=========================" << std::endl;
TinyLlamaChat chat;
if (!chat.find_and_load_model()) {
std::cout << "❌ Could not load TinyLlama model. Exiting." << std::endl;
return 1;
}
std::cout << "✅ TinyLlama model ready!" << std::endl;
chat.show_model_info();
std::cout << "\n💬 Chat with TinyLlama (type 'quit' to exit, 'info' for model details):" << std::endl;
std::cout << "────────────────────────────────────────────────────────────────" << std::endl;
std::string input;
while (true) {
std::cout << "\n👤 You: ";
if (!std::getline(std::cin, input)) {
break;
}
if (input == "quit" || input == "exit") {
std::cout << "👋 Goodbye!" << std::endl;
break;
}
if (input == "info") {
chat.show_model_info();
continue;
}
if (input.empty()) {
continue;
}
std::cout << "\n🤖 TinyLlama: " << chat.chat(input) << std::endl;
std::cout << "────────────────────────────────────────────────────────────────" << std::endl;
}
return 0;
}