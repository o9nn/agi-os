#pragma once
#include "llama.h"
#include "common.h"
#include <string>
#include <vector>
struct common_sampler;
struct common_sampler * common_sampler_init(const struct llama_model * model, const struct common_params_sampling & params);
void common_sampler_free(struct common_sampler * gsmpl);
void common_sampler_accept(struct common_sampler * gsmpl, llama_token token, bool accept_grammar);
void common_sampler_reset (struct common_sampler * gsmpl);
struct common_sampler * common_sampler_clone (struct common_sampler * gsmpl);
void common_perf_print(const struct llama_context * ctx, const struct common_sampler * gsmpl);
llama_token common_sampler_sample(struct common_sampler * gsmpl, struct llama_context * ctx, int idx, bool grammar_first = false);
std::vector<llama_token> common_sampler_sample_and_accept_n(struct common_sampler * gsmpl, struct llama_context * ctx, const std::vector<int> & idxs, const llama_tokens & draft, bool grammar_first = false);
std::vector<llama_token> common_sampler_sample_and_accept_n(struct common_sampler * gsmpl, struct llama_context * ctx, const llama_tokens & draft, bool grammar_first = false);
uint32_t common_sampler_get_seed(const struct common_sampler * gsmpl);
llama_token_data_array * common_sampler_get_candidates(struct common_sampler * gsmpl);
llama_token common_sampler_last(const struct common_sampler * gsmpl);
std::string common_sampler_print(const struct common_sampler * gsmpl);
std::string common_sampler_prev_str(common_sampler * gsmpl, llama_context * ctx, int n);
char common_sampler_type_to_chr(enum common_sampler_type cnstr);
std::string common_sampler_type_to_str(enum common_sampler_type cnstr);
std::vector<enum common_sampler_type> common_sampler_types_from_names(const std::vector<std::string> & names, bool allow_alt_names);
std::vector<enum common_sampler_type> common_sampler_types_from_chars(const std::string & chars);
llama_sampler * llama_sampler_init_llg(const llama_vocab * vocab,
const char * grammar_kind, const char * grammar_data);