#include "bolt/ai/bpe_tokenizer.hpp"
#include <iostream>
#include <algorithm>
#include <sstream>
namespace bolt {
bool BPETokenizer::loadVocabulary(const std::vector<std::string>& tokens,
const std::vector<float>& scores) {
if (tokens.empty()) {
std::cerr << "BPETokenizer: Empty token list" << std::endl;
return false;
}
vocab_size_ = tokens.size();
id_to_token_ = tokens;
token_to_id_.clear();
for (size_t i = 0; i < tokens.size(); i++) {
token_to_id_[tokens[i]] = i;
}
if (!scores.empty()) {
if (scores.size() != tokens.size()) {
std::cerr << "BPETokenizer: Token and score count mismatch" << std::endl;
return false;
}
token_scores_ = scores;
} else {
token_scores_.resize(tokens.size(), 0.0f);
}
identifySpecialTokens();
std::cout << "BPETokenizer: Loaded vocabulary with " << vocab_size_ << " tokens" << std::endl;
std::cout << "  BOS: " << bos_token_ << " (" << getTokenString(bos_token_) << ")" << std::endl;
std::cout << "  EOS: " << eos_token_ << " (" << getTokenString(eos_token_) << ")" << std::endl;
std::cout << "  PAD: " << pad_token_ << " (" << getTokenString(pad_token_) << ")" << std::endl;
return true;
}
void BPETokenizer::identifySpecialTokens() {
auto find_token = [this](const std::vector<std::string>& names) -> int {
for (const auto& name : names) {
auto it = token_to_id_.find(name);
if (it != token_to_id_.end()) {
return it->second;
}
}
return -1;
};
int bos = find_token({"<|startoftext|>", "<s>", "<BOS>", "<bos>", "[BOS]"});
if (bos >= 0) bos_token_ = bos;
int eos = find_token({"<|endoftext|>", "</s>", "<EOS>", "<eos>", "[EOS]"});
if (eos >= 0) eos_token_ = eos;
int pad = find_token({"<|pad|>", "<pad>", "<PAD>", "[PAD]"});
if (pad >= 0) pad_token_ = pad;
int unk = find_token({"<|unknown|>", "<unk>", "<UNK>", "[UNK]"});
if (unk >= 0) unk_token_ = unk;
}
std::vector<int> BPETokenizer::encode(const std::string& text) const {
if (text.empty()) {
return std::vector<int>();
}
return greedyEncode(text);
}
std::vector<int> BPETokenizer::greedyEncode(const std::string& text) const {
std::vector<int> result;
size_t pos = 0;
while (pos < text.length()) {
int best_token = unk_token_;
size_t best_len = 0;
for (size_t len = std::min(text.length() - pos, size_t(256)); len > 0; len--) {
std::string candidate = text.substr(pos, len);
auto it = token_to_id_.find(candidate);
if (it != token_to_id_.end()) {
best_token = it->second;
best_len = len;
break;
}
}
if (best_len == 0) {
std::string single_char = text.substr(pos, 1);
auto it = token_to_id_.find(single_char);
if (it != token_to_id_.end()) {
best_token = it->second;
best_len = 1;
} else {
best_token = unk_token_;
best_len = 1;
}
}
result.push_back(best_token);
pos += best_len;
}
return result;
}
std::string BPETokenizer::decode(const std::vector<int>& tokens) const {
std::string result;
for (int token_id : tokens) {
if (token_id == bos_token_ || token_id == eos_token_ || token_id == pad_token_) {
continue;
}
if (token_id < 0 || token_id >= vocab_size_) {
std::cerr << "BPETokenizer: Invalid token ID: " << token_id << std::endl;
continue;
}
result += id_to_token_[token_id];
}
return result;
}
std::string BPETokenizer::getTokenString(int token_id) const {
if (token_id >= 0 && token_id < vocab_size_) {
return id_to_token_[token_id];
}
return "<INVALID>";
}
int BPETokenizer::getTokenId(const std::string& token) const {
auto it = token_to_id_.find(token);
if (it != token_to_id_.end()) {
return it->second;
}
return unk_token_;
}
std::vector<std::string> BPETokenizer::splitToBytes(const std::string& text) const {
std::vector<std::string> result;
for (unsigned char c : text) {
result.push_back(std::string(1, c));
}
return result;
}
}