#ifndef _MOSES_UNIVARIATE_H
#define _MOSES_UNIVARIATE_H
#include "opencog/asmoses/moses/representation/instance_set.h"
#include "optimization.h"
namespace opencog { namespace moses {
struct eda_parameters
{
eda_parameters() :
selection(2),
selection_ratio(1),
replacement_ratio(0.5),
model_complexity(1)
{}
bool is_tournament_selection() {
return selection > 1;
}
bool is_truncation_selection() {
return selection <= 1;
}
double selection;
double selection_ratio;
double replacement_ratio;
double model_complexity;
};
struct univariate_optimization : optimizer_base
{
univariate_optimization(const optim_parameters& op = optim_parameters(),
const eda_parameters& ep = eda_parameters())
: optimizer_base(op), eda_params(ep) {}
void operator()(deme_t& deme,
const iscorer_base& iscorer,
unsigned max_evals,
time_t max_time);
eda_parameters eda_params;
};
}
}
#endif