#ifndef _MOSES_STAR_ANNEAL_H
#define _MOSES_STAR_ANNEAL_H
#include <opencog/util/oc_assert.h>
#include "../representation/instance_set.h"
#include "optimization.h"
namespace opencog { namespace moses {
struct sa_parameters
{
sa_parameters() :
init_temp(30),
min_temp(0),
temp_step_size(0.5),
accept_prob_temp_intensity(0.5),
max_new_instances(100) {}
double init_temp;
double min_temp;
double temp_step_size;
double accept_prob_temp_intensity;
size_t max_new_instances;
};
struct simulated_annealing : optimizer_base
{
typedef score_t energy_t;
simulated_annealing(const optim_parameters& op = optim_parameters(),
const sa_parameters& sa = sa_parameters())
: optimizer_base(op), sa_params(sa) {}
double accept_probability(energy_t energy_new, energy_t energy_old,
double temperature)
{
if (energy_new < energy_old)
return 1.0;
else
return std::exp((energy_old - energy_new) / temperature);
}
double cooling_schedule(double t)
{
OC_ASSERT(t > 0, "t should greater than 0");
return (double) sa_params.init_temp / (1.0 + t);
}
energy_t energy(score_t sc)
{
return -sc;
}
energy_t energy(const deme_inst_t& inst)
{
return energy(inst.second.get_score());
}
unsigned dist_temp(double current_temp)
{
return (unsigned)( ((current_temp - sa_params.min_temp)
/(sa_params.init_temp - sa_params.min_temp))
*
(max_distance - 1) + 1 );
}
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
const instance init_inst(deme.fields().packed_width());
operator()(deme, init_inst, iscorer, max_evals, max_time);
}
sa_parameters sa_params;
protected:
unsigned max_distance;
};
}
}
#endif