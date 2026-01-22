#pragma once
#include <mutex>
#include "AtomDBSingleton.h"
#include "BaseProxy.h"
#include "Message.h"
#include "QueryAnswer.h"
#include "SharedQueue.h"
using namespace std;
using namespace service_bus;
using namespace query_engine;
using namespace agents;
using namespace atomdb;
namespace agents {
class BaseQueryProxy : public BaseProxy {
protected:
BaseQueryProxy();
BaseQueryProxy(const vector<string>& tokens, const string& context);
public:
static string ANSWER_BUNDLE;
static string ABORT;
static string FINISHED;
static string UNIQUE_ASSIGNMENT_FLAG;
static string ATTENTION_UPDATE_FLAG;
static string MAX_BUNDLE_SIZE;
static string MAX_ANSWERS;
static string
USE_LINK_TEMPLATE_CACHE;
static string POPULATE_METTA_MAPPING;
static string USE_METTA_AS_QUERY_TOKENS;
virtual ~BaseQueryProxy();
virtual shared_ptr<QueryAnswer> pop();
unsigned int get_count();
void set_count(unsigned int count);
virtual void tokenize(vector<string>& output);
virtual bool finished();
virtual void untokenize(vector<string>& tokens);
virtual void push(shared_ptr<QueryAnswer> answer);
void flush_answer_bundle();
void query_processing_finished();
const string& get_context();
const vector<string>& get_query_tokens();
virtual string to_string();
void populate_metta_mapping(QueryAnswer* answer);
virtual bool from_remote_peer(const string& command, const vector<string>& args) override;
void answer_bundle(const vector<string>& args);
void query_answers_finished(const vector<string>& args);
virtual void pack_command_line_args() = 0;
private:
void init();
void recursive_metta_mapping(string handle, map<string, string>& table);
mutex api_mutex;
SharedQueue answer_queue;
unsigned int answer_count;
string context;
vector<string> query_tokens;
vector<string> answer_bundle_vector;
shared_ptr<AtomDB> atomdb;
};
}