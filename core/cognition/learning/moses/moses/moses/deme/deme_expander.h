#ifndef _OPENCOG_DEME_EXPANDER_H
#define _OPENCOG_DEME_EXPANDER_H
#include <vector>
#include <boost/ptr_container/ptr_vector.hpp>
#include "../optimization/optimization.h"
#include "../scoring/behave_cscore.h"
#include "../metapopulation/metapop_params.h"
#include "deme_params.h"
namespace opencog {
namespace moses {
struct deme_expander
{
deme_expander(const type_tree& type_signature,
const reduct::rule& si_ca,
const reduct::rule& si_kb,
behave_cscore& sc,
optimizer_base& opt,
const deme_parameters& pa = deme_parameters(),
const subsample_deme_filter_parameters& fp = subsample_deme_filter_parameters());
~deme_expander() {}
bool create_demes(const combo_tree& exemplar, int n_expansions = 0);
void optimize_demes(int max_evals, time_t max_time);
void free_demes();
unsigned total_evals();
boost::ptr_vector<representation> _reps;
std::vector<std::vector<deme_t>> _demes;
optimizer_base &_optimize;
protected:
std::vector<std::set<TTable::value_type>> subsample_by_time() const;
std::vector<std::set<unsigned>> subsample_by_row() const;
void create_demeIDs(int n_expansions);
bool create_representations(const combo_tree& exemplar);
string_seq fs_to_names(const feature_set& fs, const string_seq& ilabels) const;
void log_selected_feature_sets(const feature_set_pop& sf_pop,
const feature_set& xmplr_features,
const string_seq& ilabels) const;
combo_tree prune_xmplr(const combo_tree& xmplr,
const feature_set& selected_features) const;
const combo::type_tree& _type_sig;
const reduct::rule& simplify_candidate;
const reduct::rule& simplify_knob_building;
std::function<ptrdiff_t(ptrdiff_t)> random_shuffle_gen;
public:
behave_cscore& _cscorer;
protected:
std::vector<std::set<arity_t>> _ignore_cols_seq;
std::vector<demeID_t> _demeIDs;
const deme_parameters& _params;
const subsample_deme_filter_parameters& _filter_params;
};
}
}
#endif