#ifndef _MOSES_MOSES_PARAMS_H
#define _MOSES_MOSES_PARAMS_H
#include <atomic>
#include <map>
#include <boost/program_options/variables_map.hpp>
namespace opencog {
namespace moses {
typedef std::map<std::string, unsigned> jobs_t;
struct moses_parameters
{
moses_parameters(const boost::program_options::variables_map& _vm =
boost::program_options::variables_map(),
const jobs_t& _jobs = jobs_t(),
bool _local = true,
int _max_evals = 10000,
int _max_gens = -1,
score_t _max_score = 0,
int _max_cnd_output = -1) :
local(_local), mpi(false), force_feed(false), jobs(_jobs), vm(_vm),
max_evals(_max_evals), max_gens(_max_gens), max_score(_max_score),
max_time(INT_MAX), max_cnd_output(_max_cnd_output)
{}
bool local;
bool mpi;
bool force_feed;
jobs_t jobs;
boost::program_options::variables_map vm;
int max_evals;
int max_gens;
score_t max_score;
time_t max_time;
int max_cnd_output;
};
struct moses_statistics
{
moses_statistics() : n_evals(0), n_expansions(0), elapsed_secs(0)
{}
std::atomic<int> n_evals;
std::atomic<int> n_expansions;
double elapsed_secs;
};
}
}
#endif