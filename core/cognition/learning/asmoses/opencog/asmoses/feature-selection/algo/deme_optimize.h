#ifndef _OPENCOG_FEATURE_SELECTION_DEME_OPTIMIZE_H
#define _OPENCOG_FEATURE_SELECTION_DEME_OPTIMIZE_H
#include <boost/range/algorithm/sort.hpp>
#include <opencog/asmoses/moses/optimization/optimization.h>
#include <opencog/asmoses/moses/representation/field_set.h>
#include <opencog/asmoses/moses/representation/instance_set.h>
#include <opencog/asmoses/data/table/table.h>
#include "opencog/asmoses/feature-selection/main/feature-selection.h"
namespace opencog {
using namespace moses;
using namespace combo;
instance initial_instance(const feature_selection_parameters& fs_params,
const field_set& fields,
const string_seq& labels);
template<typename Optimize, typename Scorer>
feature_set_pop optimize_deme_select_feature_sets(const field_set& fields,
instance_set<composite_score>& deme,
instance& init_inst,
Optimize& optimize, const Scorer& scorer,
const feature_selection_parameters& fs_params)
{
optimize(deme, init_inst, scorer, fs_params.hc_max_evals, fs_params.max_time);
feature_set_pop fs_pop;
for (const auto& inst : deme) {
feature_set_pop::value_type p(select_tag()(inst).get_score(),
get_feature_set(fields, inst));
if (std::find(fs_pop.begin(), fs_pop.end(), p) == fs_pop.end())
fs_pop.insert(p);
}
{
std::stringstream ss;
ss << "Selected feature set has composite score: ";
if (deme.n_evals > 0)
ss << fs_pop.begin()->first;
else
ss << "Unknown";
logger().info(ss.str());
}
{
logger().info("Total number of evaluations performed: %u", deme.n_evals);
logger().info("Actual number of evaluations to reach the best feature set: %u",
deme.n_best_evals);
}
return fs_pop;
}
template<typename Optimize, typename Scorer>
feature_set_pop create_deme_select_feature_sets(const CompressedTable& ctable,
Optimize& optimize,
const Scorer& scorer,
const feature_selection_parameters& fs_params)
{
arity_t arity = ctable.get_arity();
field_set fields(field_set::disc_spec(2), arity);
instance_set<composite_score> deme(fields);
instance init_inst = initial_instance(fs_params, fields, ctable.get_input_labels());
typedef deme_based_scorer<Scorer> DBScorer;
DBScorer db_sc(scorer, fields);
if(fs_params.hc_cache_size > 0) {
typedef iscorer_cache<DBScorer> ScorerCache;
ScorerCache sc_cache(fs_params.hc_cache_size, db_sc);
feature_set_pop sf_pop =
optimize_deme_select_feature_sets(fields, deme, init_inst, optimize,
sc_cache, fs_params);
logger().info("Number of cache misses = %u", sc_cache.get_misses());
return sf_pop;
} else {
return optimize_deme_select_feature_sets(fields, deme, init_inst, optimize,
db_sc, fs_params);
}
}
template<typename Optimize>
feature_set_pop moses_select_feature_sets(const CompressedTable& ctable,
Optimize& optimize,
const feature_selection_parameters& fs_params)
{
fs_scorer<std::set<arity_t>> fs_sc(ctable, fs_params);
return create_deme_select_feature_sets(ctable, optimize, fs_sc, fs_params);
}
}
#endif