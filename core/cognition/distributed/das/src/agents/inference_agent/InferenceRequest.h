#pragma once
#include <map>
#include <string>
#include <vector>
#include "QueryAnswer.h"
using namespace std;
using namespace query_engine;
namespace inference_agent {
class InferenceRequest {
public:
InferenceRequest(string first_handle,
string second_handle,
int max_proof_length,
string context,
string max_answers = "1000000",
string update_attention_broker = "false");
InferenceRequest() = default;
~InferenceRequest();
virtual vector<string> query();
string get_id();
void set_id(string inference_request_id);
virtual string get_type();
virtual string get_max_proof_length();
virtual vector<string> get_distributed_inference_control_request();
virtual vector<string> get_correlation_query();
virtual void set_correlation_query(const string& query);
virtual map<string, QueryAnswerElement> get_correlation_query_constants();
virtual void set_correlation_query_constants(const string& constants);
virtual map<string, QueryAnswerElement> get_correlation_mapping();
virtual void set_correlation_mapping(const string& mapping);
virtual vector<string> get_update_attention_allocation_query();
virtual vector<vector<string>> get_requests();
virtual string get_direct_inference_hash() = 0;
string get_context();
void set_timeout(unsigned int timeout);
unsigned int get_timeout();
unsigned int get_repeat();
void set_repeat(unsigned int repeat);
void set_lca_max_results(unsigned long lca_max_results);
unsigned long get_lca_max_results();
unsigned int get_lca_max_repeats();
void set_lca_max_repeats(unsigned int lca_max_repeats);
bool get_lca_update_attention_broker();
void set_lca_update_attention_broker(bool lca_update_attention_broker);
bool get_sent_evolution_request();
void set_sent_evolution_request(bool sent_evolution_request);
bool get_full_evaluation();
void set_full_evaluation(bool full_evaluation);
protected:
string first_handle;
string second_handle;
int max_proof_length;
string context;
string inference_request_id;
string max_answers;
string update_attention_broker;
vector<string> correlation_query;
map<string, QueryAnswerElement> correlation_query_constants;
map<string, QueryAnswerElement> correlation_mapping;
unsigned long long timeout = 24 * 60 * 60;
unsigned int repeat = 5;
unsigned long lca_max_results = 100;
unsigned int lca_max_repeats = 1;
bool lca_update_attention_broker = false;
bool sent_evolution_request = false;
string command = "";
bool is_full_evaluation = false;
};
class ProofOfImplication : public InferenceRequest {
public:
ProofOfImplication(string first_handle, string second_handle, int max_proof_length, string context);
ProofOfImplication();
vector<string> query() override;
string get_type() override;
vector<vector<string>> get_requests() override;
string get_direct_inference_hash() override;
vector<string> get_update_attention_allocation_query() override;
private:
const string IMPLICATION_DEDUCTION_PROCESSOR = "IMPLICATION_DEDUCTION";
};
class ProofOfEquivalence : public InferenceRequest {
public:
ProofOfEquivalence(string first_handle, string second_handle, int max_proof_length, string context);
ProofOfEquivalence();
vector<string> query() override;
string get_type() override;
vector<vector<string>> get_requests() override;
string get_direct_inference_hash() override;
vector<string> get_update_attention_allocation_query() override;
private:
const string EQUIVALENCE_DEDUCTION_PROCESSOR = "EQUIVALENCE_DEDUCTION";
};
}