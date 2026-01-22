#ifndef _OPENCOG_FEATURE_SELECTION_SIMPLE_ALGO_H
#define _OPENCOG_FEATURE_SELECTION_SIMPLE_ALGO_H
#include <mutex>
#include <opencog/util/numeric.h>
#include <opencog/util/oc_omp.h>
#include "opencog/asmoses/feature-selection/main/feature-selection.h"
namespace opencog {
feature_set_pop simple_select_feature_sets(const CompressedTable& ctable,
const feature_selection_parameters& fs_params);
template<typename FeatureSet>
struct ScoredFeatureSetGreater
{
typedef std::pair<double, FeatureSet> ScoredFeatureSet;
ScoredFeatureSetGreater()
{
seed = randGen().randint();
}
bool operator()(const ScoredFeatureSet& x, const ScoredFeatureSet& y) const
{
if (x.first > y.first) return true;
if (x.first < y.first) return false;
arity_t ix = *x.second.begin();
arity_t ox = ix ^ seed;
arity_t iy = *y.second.begin();
arity_t oy = iy ^ seed;
return ox > oy;
}
typedef ScoredFeatureSet first_argument_type;
typedef ScoredFeatureSet second_argument_type;
typedef bool result_type;
private:
arity_t seed;
};
template<typename Scorer, typename FeatureSet>
FeatureSet simple_selection(const FeatureSet& features,
const Scorer& scorer,
size_t num_desired,
bool use_exp_distrib,
double threshold)
{
typedef std::pair<double, FeatureSet> ScoredFeatureSet;
std::set<ScoredFeatureSet, ScoredFeatureSetGreater<FeatureSet>> sorted_flist;
std::vector<FeatureSet> singletons;
for (auto feat : features)
singletons.push_back(FeatureSet({feat}));
std::mutex sfl_mutex;
OMP_ALGO::for_each(singletons.begin(), singletons.end(),
[&](const FeatureSet& singleton) {
double sc = scorer(singleton);
if (threshold <= sc) {
std::unique_lock<std::mutex> lock(sfl_mutex);
sorted_flist.insert({sc, singleton});
}
});
FeatureSet final;
if (use_exp_distrib)
{
double x = 1.0 - 1.0 / ((double) num_desired + 1);
double xn = 1.0;
for (auto pr = sorted_flist.begin(); pr != sorted_flist.end(); pr++) {
if (randGen().randdouble() < xn)
{
final.insert(*pr->second.begin());
num_desired --;
if (num_desired <= 0) break;
}
xn *= x;
}
} else {
for (auto pr = sorted_flist.begin(); pr != sorted_flist.end(); pr++) {
final.insert(*pr->second.begin());
num_desired --;
if (num_desired <= 0) break;
}
}
return final;
}
}
#endif