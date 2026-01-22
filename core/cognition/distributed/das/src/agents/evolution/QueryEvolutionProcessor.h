#pragma once
#include <map>
#include <memory>
#include <thread>
#include "AtomSpace.h"
#include "BusCommandProcessor.h"
#include "PatternMatchingQueryProxy.h"
#include "QueryEvolutionProxy.h"
#include "StoppableThread.h"
using namespace std;
using namespace service_bus;
using namespace atom_space;
namespace evolution {
class QueryEvolutionProcessor : public BusCommandProcessor {
public:
QueryEvolutionProcessor();
~QueryEvolutionProcessor();
virtual shared_ptr<BusCommandProxy> factory_empty_proxy();
virtual void run_command(shared_ptr<BusCommandProxy> proxy);
protected:
void select_best_individuals(shared_ptr<QueryEvolutionProxy> proxy,
vector<std::pair<shared_ptr<QueryAnswer>, float>>& population,
vector<std::pair<shared_ptr<QueryAnswer>, float>>& selected);
void select_one_by_tournament(shared_ptr<QueryEvolutionProxy> proxy,
vector<std::pair<shared_ptr<QueryAnswer>, float>>& population,
vector<std::pair<shared_ptr<QueryAnswer>, float>>& selected);
void apply_elitism(shared_ptr<QueryEvolutionProxy> proxy,
vector<std::pair<shared_ptr<QueryAnswer>, float>>& population,
vector<std::pair<shared_ptr<QueryAnswer>, float>>& selected);
void evolve_query(shared_ptr<StoppableThread> monitor, shared_ptr<QueryEvolutionProxy> proxy);
void sample_population(shared_ptr<StoppableThread> monitor,
shared_ptr<QueryEvolutionProxy> proxy,
vector<std::pair<shared_ptr<QueryAnswer>, float>>& population);
void update_attention_allocation(shared_ptr<QueryEvolutionProxy> proxy,
vector<std::pair<shared_ptr<QueryAnswer>, float>>& selected);
private:
shared_ptr<PatternMatchingQueryProxy> issue_sampling_query(shared_ptr<QueryEvolutionProxy> proxy);
shared_ptr<PatternMatchingQueryProxy> issue_correlation_query(shared_ptr<QueryEvolutionProxy> proxy,
vector<string> query_tokens);
void correlate_similar(shared_ptr<QueryEvolutionProxy> proxy,
shared_ptr<QueryAnswer> correlation_query_answer);
void stimulate(shared_ptr<QueryEvolutionProxy> proxy,
vector<std::pair<shared_ptr<QueryAnswer>, float>>& selected);
void thread_process_one_query(shared_ptr<StoppableThread>, shared_ptr<QueryEvolutionProxy> proxy);
void remove_query_thread(const string& stoppable_thread_id);
map<string, shared_ptr<StoppableThread>> query_threads;
mutex query_threads_mutex;
shared_ptr<QueryEvolutionProxy> proxy;
set<string> visited_individuals;
AtomSpace atom_space;
unsigned int generation_count;
};
}