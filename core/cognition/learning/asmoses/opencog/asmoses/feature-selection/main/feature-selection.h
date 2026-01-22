#ifndef _OPENCOG_FEATURE_SELECTION_H
#define _OPENCOG_FEATURE_SELECTION_H
#include <boost/range/algorithm/sort.hpp>
#include "opencog/asmoses/feature-selection/scorers/mutual_info.h"
#include "opencog/asmoses/feature-selection/scorers/moses_optim.h"
#include "opencog/asmoses/feature-selection/scorers/moses_matrix.h"
namespace opencog {
static const std::string mi="mi";
static const std::string pre="pre";
struct feature_selection_parameters
{
feature_selection_parameters() :
algorithm("simple"), scorer(mi),
target_size(1), exp_distrib(false), threshold(0.0),
jobs(1),
subsampling_ratio(1.0),
inc_target_size_epsilon(1.0e-10),
inc_red_intensity(-1.0),
inc_interaction_terms(1),
smd_top_size(100),
hc_max_evals(10000),
max_time(INT_MAX),
hc_max_score(1.0e50),
hc_cache_size(1000),
hc_fraction_of_remaining(1.0),
hc_crossover(true),
hc_crossover_pop_size(300),
hc_crossover_min_neighbors(1000),
hc_widen_search(true),
hc_fraction_of_nn(2.0),
mi_confi(50.0)
{}
std::string algorithm;
std::string scorer;
std::string input_file;
std::string target_feature_str;
std::string timestamp_feature_str;
string_seq ignore_features_str;
string_seq force_features_str;
string_seq initial_features;
std::string output_file;
unsigned target_size;
bool exp_distrib;
double threshold;
unsigned jobs;
float subsampling_ratio;
double inc_target_size_epsilon;
double inc_red_intensity;
unsigned inc_interaction_terms;
unsigned smd_top_size;
unsigned int hc_max_evals;
time_t max_time;
double hc_max_score;
unsigned long hc_cache_size;
double hc_fraction_of_remaining;
bool hc_crossover;
unsigned hc_crossover_pop_size;
unsigned hc_crossover_min_neighbors;
bool hc_widen_search;
float hc_fraction_of_nn;
double mi_confi;
double pre_penalty;
double pre_min_activation;
double pre_max_activation;
bool pre_positive;
};
typedef std::set<arity_t> feature_set;
typedef std::multimap<double, feature_set, std::greater<double>> feature_set_pop;
void write_results(const Table& table,
const feature_selection_parameters& fs_params);
feature_set initial_features(const string_seq& labels,
const feature_selection_parameters& fs_params);
template<typename DBScorer>
struct iscorer_cache : public iscorer_base
{
iscorer_cache(size_t n, const DBScorer& sc) :
_cache(n, sc) {}
result_type operator()(const argument_type& x) const
{
return _cache.operator()(x);
}
unsigned get_misses() const { return _cache.get_misses(); }
unsigned get_hits() const { return _cache.get_hits(); }
prr_cache_threaded<DBScorer> _cache;
};
template<typename FeatureSet>
struct fs_scorer
{
fs_scorer(const CompressedTable& ctable,
const feature_selection_parameters& fs_params)
: _ptr_mi_scorer(nullptr), _ptr_pre_scorer(nullptr)
{
if (fs_params.scorer == mi) {
_ptr_mi_scorer =
new MICScorerCompressedTable<FeatureSet>(ctable, fs_params.mi_confi);
} else if (fs_params.scorer == pre) {
_ptr_pre_scorer =
new pre_scorer<FeatureSet>(ctable, fs_params.mi_confi,
fs_params.pre_penalty,
fs_params.pre_min_activation,
fs_params.pre_max_activation,
fs_params.pre_positive);
} else {
OC_ASSERT(false, "Unknown feature selection scorer %s",
fs_params.scorer.c_str());
}
}
~fs_scorer() {
delete _ptr_mi_scorer;
delete _ptr_pre_scorer;
}
double operator()(const FeatureSet& fs) const
{
if (_ptr_mi_scorer)
return _ptr_mi_scorer->operator()(fs);
else if (_ptr_pre_scorer)
return _ptr_pre_scorer->operator()(fs);
else {
OC_ASSERT(false);
return 0.0;
}
}
protected:
MICScorerCompressedTable<FeatureSet>* _ptr_mi_scorer;
pre_scorer<FeatureSet>* _ptr_pre_scorer;
};
feature_set_pop select_feature_sets(const CompressedTable& ctable,
const feature_selection_parameters& fs_params);
feature_set select_features(const CompressedTable& ctable,
const feature_selection_parameters& fs_params);
feature_set select_features(const Table& table,
const feature_selection_parameters& fs_params);
void feature_selection(const Table& table,
const feature_selection_parameters& fs_params);
}
namespace std
{
template<>
struct hash<opencog::feature_set>
{
size_t operator()(const opencog::feature_set& fs) const noexcept
{
size_t hsh = 0;
for (int ii: fs)
hsh ^= std::hash<opencog::arity_t>{}(ii)
+ 0x9e3779b9 + (hsh << 6) + (hsh >> 2);
return hsh;
}
};
}
#endif