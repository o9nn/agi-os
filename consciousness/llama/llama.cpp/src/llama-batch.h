#pragma once
#include "llama.h"
#include <array>
#include <vector>
struct llama_ubatch {
bool equal_seqs;
uint32_t n_tokens;
uint32_t n_seq_tokens;
uint32_t n_seqs;
llama_token * token;
float * embd;
llama_pos * pos;
int32_t * n_seq_id;
llama_seq_id ** seq_id;
int8_t * output;
};
struct llama_sbatch_seq {
int32_t n_seq_id;
llama_seq_id * seq_id;
size_t offset;
size_t length;
};
struct llama_sbatch {
size_t n_tokens;
size_t n_embd;
bool logits_all;
std::vector<int64_t> ids;
std::vector<int64_t> out_ids;
std::vector<llama_sbatch_seq> seq;
const llama_batch * batch = nullptr;
std::vector<llama_token> ubatch_token;
std::vector<float> ubatch_embd;
std::vector<llama_pos> ubatch_pos;
std::vector<int32_t> ubatch_n_seq_id;
std::vector<llama_seq_id *> ubatch_seq_id;
std::vector<int8_t> ubatch_output;
llama_ubatch reserve_ubatch(size_t n_ubatch, bool has_embd = false);
void add_seq_to_ubatch(llama_ubatch & ubatch, llama_sbatch_seq & seq, size_t length);
llama_ubatch split_simple(size_t n_ubatch);
llama_ubatch split_equal(size_t n_ubatch);
llama_ubatch split_seq(size_t n_ubatch);
llama_sbatch() = default;
llama_sbatch(const llama_batch & batch, size_t n_embd, bool simple_split = false, bool logits_all = false);
};
struct llama_batch_allocr {
struct llama_batch batch;
std::array<llama_seq_id, 1> seq_id_0 = { 0 };
std::vector<llama_pos> pos;
std::vector<int32_t> n_seq_id;
std::vector<llama_seq_id *> seq_id;
std::vector<int8_t> logits;
llama_batch_allocr(struct llama_batch in_batch, llama_pos p0);
};