#ifndef _OPENCOG_FEATURE_SELECTOR_H
#define _OPENCOG_FEATURE_SELECTOR_H
#include <opencog/asmoses/data/table/table.h>
#include <opencog/asmoses/combo/combo/vertex.h>
#include <opencog/asmoses/feature-selection/main/feature-selection.h>
namespace opencog {
namespace moses {
struct feature_selector_parameters
{
feature_selector_parameters() :
increase_target_size(true),
ignore_xmplr_features(true),
restrict_incorrect(true),
restrict_true(false),
init_xmplr_features(false),
xmplr_as_feature(false),
subsampling_ratio(1.0),
subsampling_by_time(false),
n_demes(1),
diversity_pressure(0.0),
diversity_cap(0),
diversity_interaction(0)
{}
feature_selection_parameters fs_params;
bool increase_target_size;
bool ignore_xmplr_features;
std::set<arity_t> ignored_features;
bool restrict_incorrect;
bool restrict_true;
bool prune_xmplr;
bool init_xmplr_features;
bool xmplr_as_feature;
double subsampling_ratio;
bool subsampling_by_time;
unsigned n_demes;
double diversity_pressure;
size_t diversity_cap;
int diversity_interaction;
bool diversity_jaccard;
std::map<std::string,float> enforce_features;
};
typedef std::multimap<composite_score,
feature_set,
std::greater<composite_score>> csc_feature_set_pop;
struct feature_selector
{
feature_selector(const combo::CompressedTable& ctable,
const feature_selector_parameters& festor_params);
feature_selector(const combo::Table& table,
const feature_selector_parameters& festor_params);
feature_set_pop operator()(const combo::combo_tree& xmplr);
feature_set sample_enforced_features() const;
feature_selector_parameters params;
const combo::CompressedTable _ctable;
protected:
void preprocess_params(const combo::combo_tree& xmplr);
combo::CompressedTable build_fs_ctable(const combo::combo_tree& xmplr) const;
feature_set_pop select_top_feature_sets(const feature_set_pop& fss) const;
void remove_useless_features(feature_set_pop& fss) const;
csc_feature_set_pop rank_feature_sets(const feature_set_pop& fs_pop) const;
void log_stats_top_feature_sets(const feature_set_pop& top_fs) const;
double mi(const feature_set& fs_l, const feature_set& fs_r) const;
};
}
}
#endif