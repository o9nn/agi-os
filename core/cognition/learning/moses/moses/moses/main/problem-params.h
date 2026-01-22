#ifndef _OPENCOG_MOSES_PROBLEM_PARAMS_H
#define _OPENCOG_MOSES_PROBLEM_PARAMS_H
#include <string>
#include <vector>
#include <boost/program_options.hpp>
#include <moses/comboreduct/combo/vertex.h>
#include <moses/comboreduct/reduct/reduct.h>
#include <moses/comboreduct/table/table.h>
#include <moses/moses/main/problem.h>
#include <moses/moses/metapopulation/metapop_params.h>
#include <moses/moses/moses/moses_main.h>
#include <moses/moses/moses/moses_params.h>
#include <moses/moses/optimization/optimization.h>
#include <moses/moses/optimization/hill-climbing.h>
#include <moses/moses/optimization/particle-swarm.h>
namespace opencog { namespace moses {
struct problem_params : public option_base
{
problem_params();
void add_options(boost::program_options::options_description&);
void parse_options(boost::program_options::variables_map&);
std::vector<std::string> jobs_str;
unsigned min_pool;
bool enable_mpi;
unsigned long rand_seed;
std::string problem;
const unsigned int default_nsamples;
int nsamples;
double min_rand_input;
double max_rand_input;
bool balance;
unsigned long max_evals;
time_t max_time;
int max_gens;
std::string log_level;
std::string log_file;
long result_count;
bool output_score;
bool output_cscore;
bool output_bscore;
bool output_only_best;
bool output_eval_number;
bool output_with_labels;
bool output_deme_id;
std::string output_format_str;
std::string output_file;
int reduct_candidate_effort;
int reduct_knob_building_effort;
std::vector<std::string> include_only_ops_str;
std::vector<std::string> ignore_ops_str;
vertex_set ignore_ops;
std::vector<std::string> exemplars_str;
std::vector<combo_tree> exemplars;
int max_candidates_per_deme;
int revisit;
bool reduce_all;
bool linear_regression;
bool discard_dominated;
double noise;
score_t complexity_temperature;
score_t complexity_ratio;
double cap_coef;
unsigned cache_size;
double perm_ratio;
bool boosting;
int num_to_promote;
bool exact_experts;
double expalpha;
double bias_scale;
score_t diversity_pressure;
bool diversity_autoscale;
score_t diversity_exponent;
bool diversity_normalize;
std::string diversity_dst;
score_t diversity_p_norm;
std::string diversity_dst2dp;
std::string opt_algo;
double pop_size_ratio;
score_t max_score;
size_t max_dist;
bool weighted_accuracy;
std::vector<contin_t> discretize_thresholds;
score_t time_dispersion_pressure;
score_t time_dispersion_exponent;
bool time_bscore;
string time_bscore_granularity_str;
TemporalGranularity time_bscore_granularity;
bool hc_widen_search;
bool hc_single_step;
bool hc_crossover;
unsigned hc_crossover_pop_size;
unsigned hc_crossover_min_neighbors;
bool hc_resize_to_fit_ram;
unsigned hc_max_nn;
double hc_frac_of_nn;
unsigned ps_max_particles;
unsigned contin_depth;
bool use_well_enough;
score_t hardness;
bool pre_positive;
bool gen_best_tree;
bool it_abs_err;
unsigned ss_n_subsample_demes,
ss_n_top_candidates,
ss_n_tuples,
ss_n_best_bfdemes;
float ss_std_dev_threshold,
ss_tanimoto_mean_threshold,
ss_tanimoto_geo_mean_threshold,
ss_tanimoto_max_threshold,
ss_tanimoto_mean_weight,
ss_tanimoto_geo_mean_weight,
ss_tanimoto_max_weight;
bool ss_by_time,
ss_contiguous_time;
unsigned ss_n_subsample_fitnesses;
float ss_low_dev_pressure;
bool enable_feature_selection;
std::string fs_focus;
std::string fs_seed;
feature_selector_parameters festor_params;
feature_selection_parameters& fs_params;
std::string fs_enforce_features_filename;
reduct::rule* bool_reduct;
reduct::rule* bool_reduct_rep;
reduct::rule* contin_reduct;
optim_parameters opt_params;
hc_parameters hc_params;
ps_parameters ps_params;
moses_parameters moses_params;
deme_parameters deme_params;
subsample_deme_filter_parameters filter_params;
metapop_parameters meta_params;
metapop_printer mmr_pa;
protected:
const unsigned int max_filename_size;
reduct::logical_reduction lr;
private:
std::vector<std::string> col_labels;
};
}
}
#endif