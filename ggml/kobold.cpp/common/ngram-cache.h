#pragma once
#include "llama.h"
#include <unordered_map>
#include <string>
#include <vector>
#define LLAMA_NGRAM_MIN 1
#define LLAMA_NGRAM_MAX 4
#define LLAMA_NGRAM_STATIC 2
struct common_ngram {
llama_token tokens[LLAMA_NGRAM_MAX];
common_ngram() {
for (int i = 0; i < LLAMA_NGRAM_MAX; ++i) {
tokens[i] = LLAMA_TOKEN_NULL;
}
}
common_ngram(const llama_token * input, const int ngram_size) {
for (int i = 0; i < LLAMA_NGRAM_MAX; ++i) {
tokens[i] = i < ngram_size ? input[i] : LLAMA_TOKEN_NULL;
}
}
bool operator==(const common_ngram & other) const {
for (int i = 0; i < LLAMA_NGRAM_MAX; ++i) {
if (tokens[i] != other.tokens[i]) {
return false;
}
}
return true;
}
};
struct common_token_hash_function {
size_t operator()(const llama_token token) const {
return token * 11400714819323198485llu;
}
};
struct common_ngram_hash_function {
size_t operator()(const common_ngram & ngram) const {
size_t hash = common_token_hash_function{}(ngram.tokens[0]);
for (int i = 1; i < LLAMA_NGRAM_MAX; ++i) {
hash ^= common_token_hash_function{}(ngram.tokens[i]);
}
return hash;
}
};
typedef std::unordered_map<llama_token, int32_t> common_ngram_cache_part;
typedef std::unordered_map<common_ngram, common_ngram_cache_part, common_ngram_hash_function> common_ngram_cache;
void common_ngram_cache_update(
common_ngram_cache & ngram_cache, int ngram_min, int ngram_max, std::vector<llama_token> & inp_data, int nnew, bool print_progress);
void common_ngram_cache_draft(
std::vector<llama_token> & inp, std::vector<llama_token> & draft, int n_draft, int ngram_min, int ngram_max,
common_ngram_cache & nc_context, common_ngram_cache & nc_dynamic, common_ngram_cache & nc_static);
void common_ngram_cache_save(common_ngram_cache & ngram_cache, std::string & filename);
common_ngram_cache common_ngram_cache_load(std::string & filename);
void common_ngram_cache_merge(common_ngram_cache & ngram_cache_target, common_ngram_cache & ngram_cache_add);