#ifndef _MOSES_HILL_CLIMBING_H
#define _MOSES_HILL_CLIMBING_H
#include <opencog/util/oc_assert.h>
#include "../representation/instance_set.h"
#include "optimization.h"
namespace opencog { namespace moses {
struct hc_parameters
{
hc_parameters(bool widen = false,
bool step = false,
bool cross = true,
unsigned max_evals = 20000,
double _fraction_of_nn = 2.0)
: widen_search(widen),
single_step(step),
crossover(cross),
crossover_pop_size(120),
crossover_min_neighbors(400),
max_nn_evals (max_evals),
fraction_of_nn(_fraction_of_nn),
score_range(5.0),
max_allowed_instances(10000),
resize_to_fit_ram(false)
{
OC_ASSERT(0.0 < fraction_of_nn);
}
bool widen_search;
bool single_step;
bool crossover;
unsigned crossover_pop_size;
unsigned crossover_min_neighbors;
unsigned max_nn_evals;
double fraction_of_nn;
score_t score_range;
size_t max_allowed_instances;
bool resize_to_fit_ram;
std::string prefix_stat_deme;
};
struct hill_climbing : optimizer_base
{
hill_climbing(const optim_parameters& op = optim_parameters(),
const hc_parameters& hc = hc_parameters())
: optimizer_base(op), hc_params(hc), _total_RAM_bytes(getTotalRAM())
{}
protected:
void log_stats_legend();
size_t estimate_neighborhood(size_t distance, const field_set& fields);
size_t n_new_instances(size_t distance, unsigned max_evals,
size_t current_number_of_evals,
size_t total_number_of_neighbors);
size_t cross_top_one(deme_t& deme,
size_t deme_size,
size_t num_to_make,
size_t sample_start,
size_t sample_size,
const instance& base);
size_t cross_top_two(deme_t& deme,
size_t deme_size,
size_t num_to_make,
size_t sample_start,
size_t sample_size,
const instance& base);
size_t cross_top_three(deme_t& deme,
size_t deme_size,
size_t num_to_make,
size_t sample_start,
size_t sample_size,
const instance& base);
size_t crossover(deme_t& deme, size_t deme_size,
size_t max_number_of_new_instance,
size_t sample_start, size_t sample_size,
const instance& base);
bool resize_deme(deme_t& deme, score_t score_cutoff);
size_t resize_by_score(deme_t& deme, score_t score_cutoff);
public:
void operator()(deme_t& deme,
const instance& init_inst,
const iscorer_base& iscorer,
unsigned max_evals,
time_t max_time);
void operator()(deme_t& deme,
const iscorer_base& iscorer,
unsigned max_evals,
time_t max_time)
{
instance init_inst(deme.fields().packed_width());
operator()(deme, init_inst, iscorer, max_evals, max_time);
}
protected:
const hc_parameters hc_params;
const uint64_t _total_RAM_bytes;
size_t _instance_bytes;
};
}
}
#endif