#ifndef _PRECISION_BSCORE_H
#define _PRECISION_BSCORE_H
#include "scoring_base.h"
#include "time_dispersion.h"
namespace opencog
{
namespace moses
{
using combo::CompressedTable;
using combo::count_t;
using combo::multi_type_seq;
using combo::type_node;
struct precision_bscore : public bscore_ctable_time_dispersion
{
precision_bscore(const CompressedTable &_ctable,
float activation_pressure = 1.0f,
float min_activation = 0.5f,
float max_activation = 1.0f,
bool positive = true,
float dispersion_pressure = 0.0f,
float dispersion_exponent = 1.0f,
bool exact_experts = true,
double bias_scale = 1.0,
bool time_bscore = false,
TemporalGranularity granularity = TemporalGranularity::day,
bool disable_debug_log = false);
behavioral_score operator()(const combo_tree &tr) const;
behavioral_score operator()(const scored_combo_tree_set &) const;
score_t get_error(const combo_tree &) const;
behavioral_score operator()(const Handle &handle) const;
score_t get_error(const Handle &) const;
behavioral_score best_possible_bscore() const;
behavioral_score worst_possible_bscore() const;
score_t min_improv() const;
void set_complexity_coef(score_t complexity_ratio);
void set_complexity_coef(unsigned alphabet_size, float stddev);
void reset_weights();
void update_weights(const std::vector<double> &);
combo_tree gen_canonical_best_candidate() const;
protected:
score_t min_activation, max_activation;
score_t activation_pressure;
bool positive;
double bias_scale;
double wnorm;
bool exact_experts;
bool time_bscore;
type_node output_type;
behavioral_score do_score(std::function<bool(const multi_type_seq &)>) const;
private:
vertex _target, _neg_target;
bool _disable_debug_log;
score_t get_activation_penalty(score_t activation) const;
score_t sum_outputs(const CompressedTable::counter_t &) const;
behavioral_score exact_selection(const scored_combo_tree_set &) const;
behavioral_score bias_selection(const scored_combo_tree_set &) const;
};
struct precision_conj_bscore : public bscore_base
{
precision_conj_bscore(const CompressedTable &_ctable, float hardness,
bool positive = true);
behavioral_score operator()(const combo_tree& tr) const;
behavioral_score operator()(const Handle& handle) const;
behavioral_score best_possible_bscore() const;
score_t min_improv() const;
virtual void set_complexity_coef(score_t complexity_ratio);
virtual void set_complexity_coef(unsigned alphabet_size, float stddev);
protected:
const CompressedTable &ctable;
size_t ctable_usize;
float hardness;
bool positive;
private:
std::function<score_t(const CompressedTable::counter_t &)> sum_outputs;
};
}
}
#endif