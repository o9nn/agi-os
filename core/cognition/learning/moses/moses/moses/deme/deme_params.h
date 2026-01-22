#ifndef _OPENCOG_DEME_PARAMETERS_H
#define _OPENCOG_DEME_PARAMETERS_H
#include "feature_selector.h"
#include "../representation/representation.h"
namespace opencog { namespace moses {
static const operator_set empty_ignore_ops = operator_set();
struct deme_parameters
{
deme_parameters(bool _reduce_all = true,
const operator_set& _ignore_ops = empty_ignore_ops,
const combo_tree_ns_set* _perceptions = NULL,
const combo_tree_ns_set* _actions = NULL,
const feature_selector* _fstor = NULL) :
reduce_all(_reduce_all),
ignore_ops(_ignore_ops),
perceptions(_perceptions),
actions(_actions),
fstor(_fstor),
linear_contin(true)
{}
int max_candidates_per_deme;
bool reduce_all;
operator_set ignore_ops;
const combo_tree_ns_set* perceptions;
const combo_tree_ns_set* actions;
const feature_selector* fstor;
bool linear_contin;
float perm_ratio;
};
}
}
#endif