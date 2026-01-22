#pragma once
#include <mutex>
#include <set>
#include <vector>
#include "BaseQueryProxy.h"
#include "FitnessFunction.h"
using namespace std;
using namespace service_bus;
using namespace query_engine;
using namespace agents;
using namespace fitness_functions;
namespace evolution {
class QueryEvolutionProxy : public BaseQueryProxy {
public:
static string EVAL_FITNESS;
static string EVAL_FITNESS_RESPONSE;
static string POPULATION_SIZE;
static string MAX_GENERATIONS;
static string ELITISM_RATE;
static string SELECTION_RATE;
static string TOTAL_ATTENTION_TOKENS;
QueryEvolutionProxy();
QueryEvolutionProxy(
const vector<string>& tokens,
const vector<vector<string>>& correlation_queries,
const vector<map<string, QueryAnswerElement>>& correlation_replacements,
const vector<pair<QueryAnswerElement, QueryAnswerElement>>& correlation_mappings,
const string& context,
const string& fitness_function_tag,
const shared_ptr<FitnessFunction> fitness_function = shared_ptr<FitnessFunction>(nullptr));
virtual ~QueryEvolutionProxy();
virtual void pack_command_line_args();
virtual void tokenize(vector<string>& output);
virtual void untokenize(vector<string>& tokens);
float compute_fitness(shared_ptr<QueryAnswer> answer);
bool stop_criteria_met();
void new_population_sampled(vector<std::pair<shared_ptr<QueryAnswer>, float>>& population);
virtual string to_string();
bool is_fitness_function_remote();
void remote_fitness_evaluation(const vector<string>& answer_bundle);
vector<float> get_remotely_evaluated_fitness();
bool remote_fitness_evaluation_finished();
const vector<vector<string>>& get_correlation_queries();
const vector<map<string, QueryAnswerElement>>& get_correlation_replacements();
const vector<pair<QueryAnswerElement, QueryAnswerElement>>& get_correlation_mappings();
virtual bool from_remote_peer(const string& command, const vector<string>& args) override;
void eval_fitness(const vector<string>& args);
void eval_fitness_response(const vector<string>& args);
private:
void set_default_query_parameters();
void set_fitness_function_tag(const string& tag);
void init();
shared_ptr<FitnessFunction> fitness_function_object;
mutex api_mutex;
string fitness_function_tag;
float best_reported_fitness;
unsigned int num_generations;
vector<vector<string>> correlation_queries;
vector<map<string, QueryAnswerElement>> correlation_replacements;
vector<pair<QueryAnswerElement, QueryAnswerElement>> correlation_mappings;
bool ongoing_remote_fitness_evaluation;
vector<float> remote_fitness_evaluation_result;
};
}