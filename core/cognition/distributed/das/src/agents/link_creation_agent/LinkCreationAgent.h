#pragma once
#include <mutex>
#include <string>
#include <thread>
#include <vector>
#include "LinkCreationAgentRequest.h"
#include "LinkCreationRequestProxy.h"
#include "LinkCreationService.h"
#include "PatternMatchingQueryProxy.h"
#include "ServiceBusSingleton.h"
using namespace std;
using namespace query_engine;
namespace link_creation_agent {
class LinkCreationAgent {
public:
LinkCreationAgent(int request_interval,
int thread_count,
int default_timeout,
string buffer_file_path,
string metta_file_path,
bool save_links_to_metta_file,
bool save_links_to_db);
LinkCreationAgent() = default;
~LinkCreationAgent();
void run();
shared_ptr<LinkCreationAgentRequest> create_request(shared_ptr<LinkCreationRequestProxy> proxy);
void process_request(shared_ptr<LinkCreationRequestProxy> proxy);
void abort_request(const string& request_id);
private:
shared_ptr<PatternMatchingQueryProxy> query(shared_ptr<LinkCreationAgentRequest> lca_request);
void save_buffer();
void load_buffer();
void stop();
string config_path;
int requests_interval_seconds;
int link_creation_agent_thread_count;
int query_timeout_seconds;
string query_agent_client_id;
string query_agent_server_id;
string link_creation_agent_server_id;
string das_agent_client_id;
string das_agent_server_id;
string requests_buffer_file;
string metta_file_path = ".";
bool save_links_to_db = false;
bool save_links_to_metta_file = true;
unsigned int query_agent_client_start_port;
unsigned int query_agent_client_end_port;
LinkCreationService* service;
map<string, shared_ptr<LinkCreationAgentRequest>> request_buffer;
unordered_map<string, shared_ptr<LinkCreationRequestProxy>> link_creation_proxy_map;
unordered_map<string, shared_ptr<PatternMatchingQueryProxy>>
pattern_query_proxy_map;
Queue<shared_ptr<LinkCreationAgentRequest>> requests_queue;
thread* agent_thread;
mutex agent_mutex;
bool is_stoping = false;
bool is_running = false;
int loop_interval = 100;
};
}