#ifndef _OPENCOG_METAPOPULATION_H
#define _OPENCOG_METAPOPULATION_H
#include <atomic>
#include <limits>
#include <mutex>
#include <unordered_map>
#include <unordered_set>
#include <boost/accumulators/accumulators.hpp>
#include <boost/accumulators/statistics/count.hpp>
#include <boost/accumulators/statistics/mean.hpp>
#include <boost/accumulators/statistics/max.hpp>
#include <boost/logic/tribool.hpp>
#include <boost/ptr_container/ptr_vector.hpp>
#include <opencog/util/boost_ext/accumulators/statistics/geometric_mean_mirror.h>
#include <moses/comboreduct/combo/combo.h>
#include "../optimization/optimization.h"
#include "../scoring/behave_cscore.h"
#include "metapop_params.h"
#include "ensemble.h"
#define EVALUATED_ALL_AVAILABLE 1234567
class metapopulationUTest;
namespace opencog {
namespace moses {
using combo::combo_tree;
class metapopulation
{
void init(const std::vector<combo_tree>& exemplars);
public:
metapopulation(const std::vector<combo_tree>& bases,
behave_cscore& sc,
const metapop_parameters& pa = metapop_parameters(),
const subsample_deme_filter_parameters& subp = subsample_deme_filter_parameters());
metapopulation(const combo_tree& base,
behave_cscore& sc,
const metapop_parameters& pa = metapop_parameters(),
const subsample_deme_filter_parameters& subp = subsample_deme_filter_parameters());
~metapopulation() {}
const scored_combo_tree_set& best_candidates() const;
const ensemble& get_ensemble() const { return _ensemble; }
composite_score best_composite_score() const;
const combo_tree& best_tree() const;
score_t best_score() const {
return best_composite_score().get_score();
}
behave_cscore& get_cscorer() const { return _cscorer; }
public:
scored_combo_tree_ptr_set::const_iterator select_exemplar();
const scored_combo_tree_ptr_set& get_trees() const { return _scored_trees; }
scored_combo_tree_ptr_set::const_iterator begin() const { return _scored_trees.begin(); }
scored_combo_tree_ptr_set::const_iterator end() const { return _scored_trees.end(); }
bool empty() const { return _scored_trees.empty(); }
size_t size() const { return _scored_trees.size(); }
void clear() { _scored_trees.clear(); }
public:
void merge_candidates(scored_combo_tree_set& candidates);
bool merge_demes(std::vector<std::vector<deme_t>>& demes,
const boost::ptr_vector<representation>& reps);
void update_best_candidates(const scored_combo_tree_set& candidates);
private:
void rescore();
void resize_metapop();
scored_combo_tree_set get_new_candidates(const scored_combo_tree_set&);
void trim_down_deme(deme_t& deme) const;
void deme_to_trees(deme_t&, const representation&,
scored_combo_tree_set&);
score_t useful_score_range() const
{
return _params.complexity_temperature * 30.0 / 100.0;
}
private:
typedef diversity_parameters::dp_t dp_t;
dp_t distort_dp(dp_t dp) const {
return pow(dp, _params.diversity.exponent);
}
dp_t aggregated_dps(dp_t ddp_sum, unsigned N) const {
return pow(ddp_sum / N, 1.0 / _params.diversity.exponent);
}
void set_diversity();
public:
struct diversity_stats
{
double count;
double mean;
double std;
double min;
double max;
};
diversity_stats gather_diversity_stats(int n);
bool diversity_enabled() const {
return _params.diversity.enabled();
}
private:
struct cached_dst
{
cached_dst(const diversity_parameters& dparams)
: _dparams(dparams), misses(0), hits(0) {}
typedef std::set<const scored_combo_tree*> ptr_pair;
dp_t operator()(const scored_combo_tree* cl,
const scored_combo_tree* cr);
void erase_ptr_seq(std::vector<scored_combo_tree*> ptr_seq);
diversity_stats gather_stats() const;
boost::shared_mutex mutex;
const diversity_parameters& _dparams;
std::atomic<unsigned> misses, hits;
std::unordered_map<ptr_pair, dp_t, boost::hash<ptr_pair>> cache;
};
cached_dst _cached_dst;
public:
const cached_dst& get_cached_dst() const {
return _cached_dst;
}
private:
friend class ::metapopulationUTest;
typedef std::pair<scored_combo_tree_set,
scored_combo_tree_set> scored_combo_tree_set_pair;
typedef std::vector<const scored_combo_tree*> scored_combo_tree_ptr_vec;
typedef scored_combo_tree_ptr_vec::iterator scored_combo_tree_ptr_vec_it;
typedef scored_combo_tree_ptr_vec::const_iterator scored_combo_tree_ptr_vec_cit;
typedef std::pair<scored_combo_tree_ptr_vec,
scored_combo_tree_ptr_vec> scored_combo_tree_ptr_vec_pair;
static scored_combo_tree_set to_set(const scored_combo_tree_ptr_vec& bcv);
void remove_dominated(scored_combo_tree_set& bcs, unsigned jobs = 1);
static scored_combo_tree_ptr_vec_pair
inline split(const scored_combo_tree_ptr_vec& bcv)
{
scored_combo_tree_ptr_vec_cit middle = bcv.begin() + bcv.size() / 2;
return make_pair(scored_combo_tree_ptr_vec(bcv.begin(), middle),
scored_combo_tree_ptr_vec(middle, bcv.end()));
}
static scored_combo_tree_set
get_nondominated_iter(const scored_combo_tree_set& bcs);
scored_combo_tree_ptr_vec
get_nondominated_rec(const scored_combo_tree_ptr_vec& bcv,
unsigned jobs = 1);
scored_combo_tree_set_pair
get_nondominated_disjoint(const scored_combo_tree_set& bcs1,
const scored_combo_tree_set& bcs2,
unsigned jobs = 1);
scored_combo_tree_ptr_vec_pair
get_nondominated_disjoint_rec(const scored_combo_tree_ptr_vec& bcv1,
const scored_combo_tree_ptr_vec& bcv2,
unsigned jobs = 1);
void merge_nondominated(const scored_combo_tree_set& bcs, unsigned jobs = 1);
static boost::logic::tribool dominates(const behavioral_score& x,
const behavioral_score& y);
private:
void sort_demes(std::vector<std::vector<deme_t>>& ss_demes);
void keep_top_unique_candidates(
std::vector<std::vector<deme_t>>& all_demes,
const boost::ptr_vector<representation>& reps);
bool ss_score_dev_filter(const representation& rep,
const std::vector<deme_t>& ss_demes) const;
float ss_average_agreement(const representation& rep,
std::vector<deme_t>& ss_demes);
typedef boost::accumulators::accumulator_set
<double,
boost::accumulators::stats<boost::accumulators::tag::count,
boost::accumulators::tag::mean,
boost::accumulators::tag::geometric_mean_mirror,
boost::accumulators::tag::max>> tanimoto_acc_t;
void recompute_scores_over_whole_dataset(
std::vector<std::vector<deme_t>>& ss_demes,
const boost::ptr_vector<representation>& reps);
std::vector<bool> ss_filter(
const std::vector<std::vector<deme_t>>& all_demes,
const boost::ptr_vector<representation>& reps) const;
void ss_tanimoto_stats(const std::vector<combo_tree>& trs,
tanimoto_acc_t& acc) const;
void ss_tanimoto_stats(const representation& rep,
const std::vector<deme_t>& ss_demes,
tanimoto_acc_t& acc) const;
bool ss_tanimoto_filter(const representation& rep,
const std::vector<deme_t>& ss_demes) const;
public:
void log_best_candidates() const;
std::ostream& ostream_metapop(std::ostream&, int n = INT_MAX) const;
private:
void log_selected_exemplar(scored_combo_tree_ptr_set::const_iterator);
protected:
const metapop_parameters& _params;
const subsample_deme_filter_parameters& _filter_params;
behave_cscore& _cscorer;
scored_combo_tree_ptr_set _scored_trees;
static const unsigned _min_pool_size = 250;
size_t _merge_count;
composite_score _best_cscore;
scored_combo_tree_set _best_candidates;
typedef std::unordered_map<scored_combo_tree, unsigned,
scored_combo_tree_hash,
scored_combo_tree_equal> scored_tree_counter;
scored_tree_counter _visited_exemplars;
bool has_been_visited(const scored_combo_tree&) const;
std::mutex _merge_mutex;
ensemble _ensemble;
};
}
}
#endif