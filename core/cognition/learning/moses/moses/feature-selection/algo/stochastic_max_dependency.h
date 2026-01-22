#ifndef _OPENCOG_FEATURE_MAX_MI_ALGO_H
#define _OPENCOG_FEATURE_MAX_MI_ALGO_H
#include <functional>
#include <opencog/util/numeric.h>
#include <opencog/util/lru_cache.h>
#include <opencog/util/algorithm.h>
#include <opencog/util/functional.h>
#include <opencog/util/oc_omp.h>
#include "../main/feature-selection.h"
namespace opencog {
feature_set_pop smd_select_feature_sets(const CTable& ctable,
const feature_selection_parameters& fs_params);
template<typename Scorer>
feature_set_pop stochastic_max_dependency_selection(const feature_set& features,
const feature_set& init_features,
const Scorer& scorer,
unsigned num_features,
double threshold = 0.0,
unsigned top_size = 100)
{
if (logger().is_debug_enabled()) {
logger().debug() << "Call stochastic_max_dependency_selection(num_features="
<< num_features
<< ", threshold=" << threshold
<< ", top_size=" << top_size
<<")";
}
typedef typename feature_set::value_type feature_id;
typedef feature_set_pop ranks_t;
typedef boost::shared_mutex shared_mutex;
typedef boost::unique_lock<shared_mutex> unique_lock;
shared_mutex mutex;
double init_sc = scorer(init_features);
std::vector<std::pair<double, feature_set>> tops{{init_sc, init_features}};
double previous_high_score = init_sc;
if (features.size() < num_features)
num_features = features.size();
std::vector<feature_id> shuffle(features.begin(), features.end());
auto shr = [&](ptrdiff_t i) { return randGen().randint(i); };
random_shuffle(shuffle.begin(), shuffle.end(), shr);
ranks_t ranks;
for (unsigned i = init_features.size() + 1; i <= num_features; ++i) {
ranks.clear();
for (const auto& pr : tops) {
const feature_set &fs = pr.second;
OMP_ALGO::for_each(shuffle.cbegin(), shuffle.cend(),
[&](feature_id fid) {
if (fs.end() == fs.find(fid)) {
feature_set prod = fs;
prod.insert(fid);
double sc = scorer(prod);
if (logger().is_fine_enabled())
ostream_container(logger().fine()
<< "feature set ",
prod, ",", "{", "}")
<< " is scored " << sc;
std::pair<double, feature_set> pdf(sc, prod);
unique_lock lock(mutex);
ranks.insert(pdf);
}
});
}
tops.clear();
auto rb = ranks.begin();
auto re = std::next(rb, std::min(top_size, (unsigned)ranks.size()));
tops.insert(tops.begin(), rb, re);
OC_ASSERT (!ranks.empty(), "Fatal Error: no ranked feature sets");
double high_score = ranks.begin()->first;
logger().debug("SMD: featureset size=%d highest score=%f", i, high_score);
if (high_score - previous_high_score < threshold) {
logger().debug("SMD: terminate, no improvment in score");
break;
}
previous_high_score = high_score;
}
OC_ASSERT(!tops.empty(), "top is empty, there must be a bug");
if (logger().is_debug_enabled()) {
std::stringstream ss;
ss << "Exit stochastic_max_dependency_selection(), selected: ";
ostream_container(ss, ranks.begin()->second);
ss << " Score = " << ranks.begin()->first;
logger().debug() << ss.str();
}
return ranks;
}
}
#endif