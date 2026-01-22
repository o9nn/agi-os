#pragma once
#include <mutex>
#include <set>
#include <shared_mutex>
#include "EquivalenceProcessor.h"
#include "ImplicationProcessor.h"
#include "LCAQueue.h"
#include "LinkCreationAgentRequest.h"
#include "LinkProcessor.h"
#include "MettaTemplateProcessor.h"
#include "PatternMatchingQueryProxy.h"
#include "QueryAnswer.h"
#include "ServiceBusSingleton.h"
#include "TemplateProcessor.h"
#include "ThreadPool.h"
using namespace query_engine;
using namespace std;
namespace link_creation_agent {
class LinkCreationService
{
public:
LinkCreationService(int thread_count);
void process_request(shared_ptr<PatternMatchingQueryProxy> proxy,
shared_ptr<LinkCreationAgentRequest> request);
void set_timeout(int timeout);
void set_metta_file_path(string metta_file_path);
void set_save_links_to_metta_file(bool save_links_to_metta_file) {
this->save_links_to_metta_file = save_links_to_metta_file;
}
void set_save_links_to_db(bool save_links_to_db) { this->save_links_to_db = save_links_to_db; }
~LinkCreationService();
private:
ThreadPool thread_pool;
set<string> processed_link_handles;
string metta_file_path;
shared_mutex m_mutex;
shared_ptr<LinkTemplateProcessor> link_template_processor;
shared_ptr<ImplicationProcessor> implication_processor;
shared_ptr<EquivalenceProcessor> equivalence_processor;
shared_ptr<MettaTemplateProcessor> metta_link_processor;
shared_ptr<mutex> query_agent_mutex;
Queue<tuple<string, shared_ptr<Link>>> link_creation_queue;
bool is_stoping = false;
thread create_link_thread;
set<string> metta_expression_set;
bool save_links_to_metta_file = false;
bool save_links_to_db = false;
int timeout = 300 * 1000;
class ProcessorTypeValue : public HandleTrie::TrieValue {
public:
set<ProcessorType> processor_types;
ProcessorTypeValue() {}
void merge(TrieValue* other) {}
};
void create_links();
vector<shared_ptr<Link>> process_query_answer(shared_ptr<QueryAnswer> query_answer,
vector<string> params,
vector<string> link_template);
string query_answer_hash(shared_ptr<QueryAnswer> query_answer);
};
}