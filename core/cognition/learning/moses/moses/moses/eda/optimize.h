#ifndef _EDA_OPTIMIZE_H
#define _EDA_OPTIMIZE_H
#include <opencog/util/Logger.h>
#include <opencog/util/oc_omp.h>
#include "../representation/instance_set.h"
namespace opencog {
namespace moses {
template <typename ScoreT,
typename ScoringPolicy,
typename TerminationPolicy,
typename SelectionPolicy,
typename StructureLearningPolicy,
typename ProbsLearningPolicy,
typename ReplacementPolicy,
typename LoggingPolicy>
int optimize(instance_set<ScoreT>& current,
int n_select,
int n_generate,
int max_gens,
const ScoringPolicy& score,
const TerminationPolicy& termination_criterion,
const SelectionPolicy& select,
const StructureLearningPolicy& learn_structure,
const ProbsLearningPolicy& learn_probs,
const ReplacementPolicy& replace,
LoggingPolicy& write_log)
{
logger().debug("Probabilistic Learning Optimization");
typedef typename StructureLearningPolicy::model_type model_type;
logger().debug("Evaluate the initial population (%u individuals)",
current.size());
OMP_ALGO::transform(current.begin(), current.end(),
current.begin_scores(),
std::bind(std::cref(score), std::placeholders::_1));
int generation = 0;
for (; generation < max_gens
&& !termination_criterion(current.begin(), current.end());
++generation)
{
write_log(current.begin(), current.end(),
current.fields(), generation);
logger().debug("Select %d promising instances for model building",
n_select);
std::vector<scored_instance<ScoreT> > promising(n_select);
select(current.begin(), current.end(), promising.begin(), n_select);
logger().debug("Build probabilistic model");
model_type model(current.fields(), promising.begin(),
promising.end());
learn_structure(current.fields(), promising.begin(),
promising.end(), model);
learn_probs(current.fields(), promising.begin(),
promising.end(), model);
logger().debug("Sample and evaluate %d new candidates"
" according to the model", n_generate);
instance_set<ScoreT> new_instances(n_generate, current.fields());
for (auto& inst : new_instances)
inst = model();
OMP_ALGO::transform(new_instances.begin(), new_instances.end(),
new_instances.begin_scores(),
std::bind(std::cref(score), std::placeholders::_1));
logger().debug("Replace the new candidates");
replace(new_instances.begin(), new_instances.end(),
current.begin(), current.end());
}
write_log(current.begin(), current.end(),
current.fields(), generation);
return current.size() + generation * n_generate;
}
}
}
#endif