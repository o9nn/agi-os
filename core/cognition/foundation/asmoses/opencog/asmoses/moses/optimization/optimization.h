#ifndef _MOSES_OPTIMIZATION_H
#define _MOSES_OPTIMIZATION_H
#include <opencog/util/oc_assert.h>
#include "opencog/asmoses/moses/representation/instance_scorer.h"
#include "opencog/asmoses/moses/representation/instance_set.h"
namespace opencog { namespace moses {
double information_theoretic_bits(const field_set& fs);
static const std::string un("un");
static const std::string sa("sa");
static const std::string hc("hc");
static const std::string ps("ps");
struct optim_parameters
{
optim_parameters(const std::string& _opt_algo = hc,
double _pop_size_ratio = 20,
score_t _terminate_if_gte = 0,
size_t _max_dist = 4,
score_t _min_score_improv = 0.5);
unsigned pop_size(const field_set& fs) const;
unsigned max_gens_improv(const field_set& fs) const;
unsigned rtr_window_size(const field_set& fs) const;
unsigned max_distance(const field_set& fs) const;
void set_min_score_improv(score_t s);
score_t min_score_improv() const;
bool score_improved(score_t best_score, score_t prev_hi) const;
std::string opt_algo;
double term_improv;
double window_size_pop;
double window_size_len;
double pop_size_ratio;
score_t terminate_if_gte;
size_t max_dist;
private:
score_t min_score_improvement;
};
struct optim_stats
{
optim_stats()
: nsteps(0), demeID(0), total_steps(0), total_evals(0),
field_set_size(0), over_budget(false)
#ifdef GATHER_STATS
, hiscore(0.0), hicount(0.0),
num_improved(0.0), count_improved(0.0)
#endif
{}
unsigned nsteps;
demeID_t demeID;
unsigned total_steps;
unsigned total_evals;
unsigned field_set_size;
bool over_budget;
#ifdef GATHER_STATS
vector<double> scores;
vector<double> counts;
double hiscore;
double hicount;
double num_improved;
double count_improved;
#endif
};
void print_stats_header (optim_stats *, bool diversity_enabled);
typedef instance_set<composite_score> deme_t;
typedef scored_instance<composite_score> deme_inst_t;
struct optimizer_base : optim_stats
{
optimizer_base(const optim_parameters& op = optim_parameters())
: opt_params(op) {}
virtual void operator()(deme_t& deme,
const iscorer_base& iscorer,
unsigned max_evals,
time_t max_time) = 0;
virtual ~optimizer_base() {}
optim_parameters opt_params;
};
}
}
#endif