#pragma once
#include <memory>
#include <mutex>
#include <string>
#include <thread>
#include <unordered_map>
#include <vector>
#include "InferenceProxy.h"
#include "InferenceRequest.h"
#include "InferenceRequestValidator.h"
#include "LCAQueue.h"
#include "LinkCreationRequestProxy.h"
#include "QueryEvolutionProxy.h"
using namespace distributed_algorithm_node;
using namespace link_creation_agent;
using namespace evolution;
using namespace std;
namespace inference_agent {
class InferenceAgent {
public:
InferenceAgent();
~InferenceAgent();
void run();
void stop();
void process_inference_request(const vector<string>& request, const string& request_id);
void process_inference_request(shared_ptr<InferenceProxy> proxy);
void process_inference_abort_request(shared_ptr<InferenceRequest> inference_request);
private:
void send_link_creation_request(shared_ptr<InferenceRequest> inference_request);
void send_distributed_inference_control_request(shared_ptr<InferenceRequest> inference_request);
void send_update_attention_allocation_request(shared_ptr<InferenceRequest> inference_request);
shared_ptr<InferenceRequest> build_inference_request(const vector<string>& request);
bool is_lca_requests_finished(shared_ptr<InferenceRequest> inference_request);
shared_ptr<InferenceRequest> get_inference_request(const string& request_id);
void process_evolution_requests();
void process_lca_requests();
void process_direct_link_inference(shared_ptr<InferenceRequest> inference_request);
InferenceRequestValidator inference_request_validator;
vector<string> get_link_creation_request();
int max_proof_length_limit = 10;
thread* agent_thread = nullptr;
bool is_stoping = false;
mutex agent_mutex;
unsigned long long inference_request_id = 0;
Queue<shared_ptr<InferenceRequest>> inference_request_queue;
vector<shared_ptr<InferenceRequest>> inference_requests;
unordered_map<string, vector<shared_ptr<LinkCreationRequestProxy>>> link_creation_proxy_map;
unordered_map<string, shared_ptr<QueryEvolutionProxy>> evolution_proxy_map;
unordered_map<string, shared_ptr<InferenceProxy>> inference_proxy_map;
unordered_map<string, unsigned long long> inference_timeout_map;
static const string PROOF_OF_IMPLICATION;
static const string PROOF_OF_EQUIVALENCE;
};
}