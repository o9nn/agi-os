#pragma once
#include <fstream>
#include <mutex>
#include <set>
#include <string>
#include <vector>
#include "BaseQueryProxy.h"
#include "QueryAnswer.h"
using namespace std;
using namespace service_bus;
using namespace query_engine;
using namespace agents;
namespace context_broker {
class ContextBrokerProxy : public BaseQueryProxy {
public:
static string ATTENTION_BROKER_SET_PARAMETERS;
static string ATTENTION_BROKER_SET_PARAMETERS_FINISHED;
static string CONTEXT_CREATED;
static string USE_CACHE;
static string ENFORCE_CACHE_RECREATION;
static string INITIAL_RENT_RATE;
static string INITIAL_SPREADING_RATE_LOWERBOUND;
static string INITIAL_SPREADING_RATE_UPPERBOUND;
static double DEFAULT_RENT_RATE;
static double DEFAULT_SPREADING_RATE_LOWERBOUND;
static double DEFAULT_SPREADING_RATE_UPPERBOUND;
ContextBrokerProxy();
ContextBrokerProxy(const string& name,
const vector<string>& query,
const vector<pair<QueryAnswerElement, QueryAnswerElement>>& determiner_schema,
const vector<QueryAnswerElement>& stimulus_schema);
ContextBrokerProxy(const string& name,
const vector<string>& query,
const string& determiner_schema,
const string& stimulus_schema);
virtual ~ContextBrokerProxy();
bool is_context_created();
virtual bool from_remote_peer(const string& command, const vector<string>& args) override;
void attention_broker_set_parameters(double rent_rate,
double spreading_rate_lowerbound,
double spreading_rate_upperbound);
bool attention_broker_set_parameters_finished();
const string& get_name();
const string& get_key();
const vector<pair<QueryAnswerElement, QueryAnswerElement>>& get_determiner_schema();
const vector<QueryAnswerElement>& get_stimulus_schema();
const string& get_cache_file_name();
bool get_use_cache();
bool get_enforce_cache_recreation();
double get_initial_rent_rate();
double get_initial_spreading_rate_lowerbound();
double get_initial_spreading_rate_upperbound();
virtual void tokenize(vector<string>& output) override;
virtual void untokenize(vector<string>& tokens) override;
virtual string to_string() override;
virtual void pack_command_line_args() override;
map<string, unsigned int> to_stimulate;
vector<vector<string>> determiner_request;
bool update_attention_broker_parameters;
double rent_rate;
double spreading_rate_lowerbound;
double spreading_rate_upperbound;
private:
void init(const string& name);
void set_default_query_parameters();
mutex api_mutex;
string name;
string key;
string cache_file_name;
vector<pair<QueryAnswerElement, QueryAnswerElement>> determiner_schema;
vector<QueryAnswerElement> stimulus_schema;
bool context_created;
bool ongoing_attention_broker_set_parameters;
};
}