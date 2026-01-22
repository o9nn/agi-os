#ifndef _OPENCOG_METAPOP_PARAMETERS_H
#define _OPENCOG_METAPOP_PARAMETERS_H
#include "ensemble_params.h"
#include "../moses/types.h"
namespace opencog { namespace moses {
struct diversity_parameters
{
typedef score_t dp_t;
diversity_parameters();
dp_t pressure;
bool autoscale;
dp_t exponent;
bool normalize;
enum dst_enum_t { p_norm, tanimoto, angular };
void set_dst(dst_enum_t de, dp_t p = 0.0 );
std::function<dp_t(const std::vector<score_t>&,
const std::vector<score_t>&)> dst;
enum dst2dp_enum_t { inverse, complement, pthpower };
dst2dp_enum_t dst2dp_type;
void set_dst2dp(dst2dp_enum_t d2de);
std::function<dp_t(dp_t)> dst2dp;
bool enabled() const;
};
struct subsample_deme_filter_parameters
{
subsample_deme_filter_parameters(unsigned _n_subsample_demes = 0,
unsigned _n_top_candidates = 1) :
by_time(true),
contiguous_time(true),
n_subsample_demes(_n_subsample_demes),
n_top_candidates(_n_top_candidates),
n_tuples(UINT_MAX) {}
bool by_time;
bool contiguous_time;
unsigned n_subsample_demes;
unsigned n_top_candidates;
unsigned n_tuples;
float std_dev_threshold;
float tanimoto_mean_threshold,
tanimoto_geo_mean_threshold,
tanimoto_max_threshold;
unsigned n_best_bfdemes;
float tanimoto_mean_weight,
tanimoto_geo_mean_weight,
tanimoto_max_weight;
unsigned n_subsample_fitnesses;
float low_dev_pressure;
};
struct metapop_parameters
{
metapop_parameters(int _max_candidates_per_deme = -1,
int _revisit = 0,
score_t _complexity_temperature = 6.0f,
unsigned _jobs = 1,
diversity_parameters _diversity = diversity_parameters()) :
max_candidates_per_deme(_max_candidates_per_deme),
revisit(_revisit),
do_boosting(false),
discard_dominated(false),
keep_bscore(false),
complexity_temperature(_complexity_temperature),
cap_coef(50.0),
jobs(_jobs),
diversity(_diversity),
merge_callback(NULL),
callback_user_data(NULL)
{}
int max_candidates_per_deme;
int revisit;
bool do_boosting;
bool discard_dominated;
bool keep_bscore;
score_t complexity_temperature;
double cap_coef;
unsigned jobs;
diversity_parameters diversity;
ensemble_parameters ensemble_params;
bool (*merge_callback)(scored_combo_tree_set&, void*);
void *callback_user_data;
};
}
}
#endif