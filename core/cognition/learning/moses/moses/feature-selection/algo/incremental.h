#ifndef _OPENCOG_FEATURE_SELECTION_INCREMENTAL_ALGO_H
#define _OPENCOG_FEATURE_SELECTION_INCREMENTAL_ALGO_H
#include <functional>
#include <boost/range/algorithm/set_algorithm.hpp>
#include <boost/range/algorithm/max_element.hpp>
#include <opencog/util/algorithm.h>
#include <opencog/util/functional.h>
#include <opencog/util/lru_cache.h>
#include <opencog/util/numeric.h>
#include <opencog/util/oc_omp.h>
#include "../main/feature-selection.h"
namespace opencog {
feature_set_pop incremental_select_feature_sets(const CTable& ctable,
const feature_selection_parameters& fs_params);
template<typename Scorer, typename FeatureSet>
FeatureSet incremental_selection(const FeatureSet& features,
const Scorer& scorer,
double threshold,
unsigned max_interaction_terms = 1,
double red_threshold = -1.0)
{
FeatureSet rel;
FeatureSet res;
typedef boost::shared_mutex shared_mutex;
typedef boost::unique_lock<shared_mutex> unique_lock;
shared_mutex mutex;
#if DEBUG
for (unsigned i = 1; i <= max_interaction_terms; ++i) {
std::set<FeatureSet> ps = powerset(features, i, true);
typename std::set<FeatureSet>::const_iterator psit;
for (const FeatureSet& fs : ps) {
printContainer(fs, ", ", "fs=[", "]");
std::cout << "\t" << scorer(fs) << std::endl;
}
std::cout << "============================" << std::endl;
}
#endif
for (unsigned i = 1; i <= max_interaction_terms; ++i) {
struct timeval start;
gettimeofday(&start, NULL);
FeatureSet tf = opencog::set_difference(features, rel);
std::set<FeatureSet> fss = powerset(tf, i, true);
logger().debug("Iteration %d feature set size=%d powerset size=%d",
i, tf.size(), fss.size());
rel.clear();
auto fss_view = random_access_view(fss);
auto filter_relevant = [&](const FeatureSet* fs) {
if (scorer(*fs) > threshold) {
unique_lock lock(mutex);
rel.insert(fs->begin(), fs->end());
}};
OMP_ALGO::for_each(fss_view.begin(), fss_view.end(), filter_relevant);
logger().debug("Iteration %d relevant features=%d",
i, rel.size());
if (0.0 < red_threshold) {
std::set<FeatureSet> nrfss = powerset(rel, i+1, true);
FeatureSet red;
auto filter_redundant = [&](const FeatureSet* fs) {
{
FeatureSet rfs = redundant_features(*fs, scorer,
threshold
* red_threshold);
unique_lock lock(mutex);
red.insert(rfs.begin(), rfs.end());
}};
auto nrfss_view = random_access_view(nrfss);
OMP_ALGO::for_each(nrfss_view.begin(), nrfss_view.end(),
filter_redundant);
logger().debug("Iteration %d redundant features=%d",
i, red.size());
boost::set_difference(rel, red, std::inserter(res, res.begin()));
} else {
res.insert(rel.begin(), rel.end());
}
logger().debug("Iteration %d finished with %d features\n",
i, res.size());
struct timeval stop, elapsed;
gettimeofday(&stop, NULL);
timersub(&stop, &start, &elapsed);
logger().debug("Elapsed time %d seconds\n", elapsed.tv_sec);
double rate = 1.0e6 * elapsed.tv_sec + elapsed.tv_usec;
rate /= (double) res.size();
rate /= (double) res.size();
logger().debug("Rate %f microseconds per feature squared\n", rate);
}
if (logger().is_info_enabled()) {
std::stringstream ss;
ss << "Exit incremental_selection(), selected: ";
ostream_container(ss, res);
logger().info() << ss.str();
}
return res;
}
template<typename Scorer, typename FeatureSet>
FeatureSet cached_incremental_selection(const FeatureSet& features,
const Scorer& scorer,
double threshold,
unsigned max_interaction_terms = 1,
double red_threshold = -1.0)
{
logger().debug() << "cached_incremental_selection(), num feats="
<< features.size();
prr_cache_threaded<Scorer> scorer_cache(std::pow((double)features.size(),
(int)max_interaction_terms),
scorer);
return incremental_selection(features, scorer_cache, threshold,
max_interaction_terms, red_threshold);
}
template<typename Scorer, typename FeatureSet>
FeatureSet adaptive_incremental_selection(const FeatureSet& features,
const Scorer& scorer,
unsigned features_size_target,
unsigned max_interaction_terms = 1,
double red_threshold = -1.0,
double min = 0, double max = 1,
double epsilon = 0.001)
{
double mean = (min+max)/2;
if (logger().is_debug_enabled()) {
logger().debug() << "Call adaptive_incremental_selection(size="
<< features_size_target
<< ", terms=" << max_interaction_terms
<< ", red thresh=" << red_threshold
<< ", min=" << min
<< ", max=" << max
<< ", epsi=" << epsilon
<<") so selection-thres=" << mean;
}
FeatureSet res = incremental_selection(features, scorer, mean,
max_interaction_terms, red_threshold);
unsigned rsize = res.size();
logger().debug("Selected %d features", rsize);
if (is_within(min, max, epsilon) || rsize == features_size_target)
return res;
else {
double nmin = rsize < features_size_target? min : mean;
double nmax = rsize < features_size_target? mean : max;
return adaptive_incremental_selection(features, scorer,
features_size_target,
max_interaction_terms,
red_threshold,
nmin, nmax, epsilon);
}
}
template<typename Scorer, typename FeatureSet>
FeatureSet cached_adaptive_incremental_selection(const FeatureSet& features,
const Scorer& scorer,
unsigned features_size_target,
unsigned max_interaction_terms = 1,
double red_threshold = -1.0,
double min = 0, double max = 1,
double epsilon = 0.01)
{
logger().debug() << "cached_adaptive_incremental_selection(),"
<< " target=" << features_size_target
<< " iterms=" << max_interaction_terms
<< " num feats=" << features.size();
prr_cache_threaded<Scorer> scorer_cache(std::pow((double)features.size(),
(int)max_interaction_terms),
scorer);
FeatureSet f = adaptive_incremental_selection(features, scorer_cache,
features_size_target,
max_interaction_terms,
red_threshold,
min, max, epsilon);
return f;
}
template<typename Scorer, typename FeatureSet>
FeatureSet redundant_features(const FeatureSet& features, const Scorer& scorer,
double threshold)
{
typedef FeatureSet FS;
for (unsigned i = 1; i < features.size(); i++) {
auto sfs = powerset(features, i, true);
auto mit = boost::max_element(sfs, [&](const FS& fsl, const FS& fsr) {
return scorer(fsl) < scorer(fsr); });
if (scorer(features) - scorer(*mit) < threshold)
return set_difference(features, *mit);
}
return FS();
}
}
#endif