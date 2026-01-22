#ifndef _BSCORES_H
#define _BSCORES_H
#include <iostream>
#include <fstream>
#include <functional>
#include <vector>
#include <boost/accumulators/accumulators.hpp>
#include <boost/accumulators/statistics/stats.hpp>
#include <boost/accumulators/statistics/weighted_skewness.hpp>
#include <moses/comboreduct/table/table.h>
#include "scoring_base.h"
#include "../moses/types.h"
namespace opencog { namespace moses {
using namespace combo;
struct logical_bscore : public bscore_base
{
template<typename Func>
logical_bscore(const Func& func, int a)
: _target(func, a), _arity(a)
{
_size = _target.size();
reset_weights();
}
logical_bscore(const combo_tree& tr, int a)
: _target(tr, a), _arity(a)
{
_size = _target.size();
reset_weights();
}
behavioral_score operator()(const combo_tree&) const;
behavioral_score operator()(const scored_combo_tree_set&) const;
behavioral_score best_possible_bscore() const;
behavioral_score worst_possible_bscore() const;
score_t get_error(const behavioral_score&) const;
score_t min_improv() const;
protected:
complete_truth_table _target;
int _arity;
};
struct discretize_contin_bscore : public bscore_base
{
discretize_contin_bscore(const OTable& ot, const ITable& it,
const std::vector<contin_t>& thres,
bool weighted_average);
behavioral_score operator()(const combo_tree& tr) const;
behavioral_score best_possible_bscore() const;
score_t min_improv() const;
protected:
OTable target;
ITable cit;
std::vector<contin_t> thresholds;
bool weighted_accuracy;
size_t class_idx(contin_t v) const;
size_t class_idx_within(contin_t v, size_t l_idx, size_t u_idx) const;
std::vector<size_t> classes;
std::vector<score_t> weights;
};
struct contin_bscore : public bscore_base
{
enum err_function_type {
squared_error,
abs_error
};
void init(err_function_type eft = squared_error)
{
switch (eft) {
case squared_error:
err_func = [](contin_t y1, contin_t y2) { return sq(y1 - y2); };
break;
case abs_error:
err_func = [](contin_t y1, contin_t y2) { return std::abs(y1 - y2); };
break;
default:
OC_ASSERT(false);
}
};
template<typename Scoring>
contin_bscore(const Scoring& score, const ITable& r,
err_function_type eft = squared_error)
: target(score, r), cti(r)
{
init(eft);
_size = r.size();
}
contin_bscore(const OTable& t, const ITable& r,
err_function_type eft = squared_error)
: target(t), cti(r)
{
init(eft);
_size = r.size();
}
contin_bscore(const Table& table,
err_function_type eft = squared_error)
: target(table.otable), cti(table.itable) {
init(eft);
_size = table.size();
}
behavioral_score operator()(const combo_tree& tr) const;
behavioral_score best_possible_bscore() const;
score_t min_improv() const;
virtual void set_complexity_coef(unsigned alphabet_size, float stddev);
using bscore_base::set_complexity_coef;
protected:
OTable target;
ITable cti;
private:
std::function<score_t(contin_t, contin_t)> err_func;
};
struct ctruth_table_bscore : public bscore_ctable_base
{
ctruth_table_bscore(const CTable& ctt)
: bscore_ctable_base(ctt)
{
_size = _wrk_ctable.size();
reset_weights();
set_best_possible_bscore();
}
behavioral_score operator()(const combo_tree& tr) const;
behavioral_score operator()(const scored_combo_tree_set&) const;
behavioral_score best_possible_bscore() const;
behavioral_score worst_possible_bscore() const;
score_t min_improv() const;
protected:
mutable behavioral_score _best_possible_score;
void set_best_possible_bscore() const;
};
struct enum_table_bscore : public bscore_base
{
enum_table_bscore(const CTable& ctt) : _ctable(ctt)
{ _size = _ctable.size(); }
behavioral_score operator()(const combo_tree& tr) const;
behavioral_score best_possible_bscore() const;
virtual score_t min_improv() const;
protected:
CTable _ctable;
};
struct enum_filter_bscore : public enum_table_bscore
{
enum_filter_bscore(const CTable& ctt)
: enum_table_bscore(ctt), punish(1.0)
{}
behavioral_score operator()(const combo_tree& tr) const;
score_t punish;
};
struct enum_graded_bscore : public enum_table_bscore
{
enum_graded_bscore(const CTable& ctt)
: enum_table_bscore(ctt), grading(0.9)
{}
behavioral_score operator()(const combo_tree&) const;
virtual score_t min_improv() const;
virtual complexity_t get_complexity(const combo_tree&) const;
score_t grading;
protected:
score_t graded_complexity(combo_tree::iterator) const;
};
struct enum_effective_bscore : public enum_graded_bscore
{
enum_effective_bscore(const CTable& ctt)
: enum_graded_bscore(ctt), _ctable_usize(ctt.uncompressed_size())
{ _size = _ctable_usize; }
behavioral_score operator()(const combo_tree& tr) const;
protected:
size_t _ctable_usize;
};
struct interesting_predicate_bscore : public bscore_base
{
typedef score_t weight_t;
typedef Counter<contin_t, contin_t> counter_t;
typedef Counter<contin_t, contin_t> pdf_t;
typedef boost::accumulators::accumulator_set<contin_t,
boost::accumulators::stats<
boost::accumulators::tag::weighted_skewness
>, contin_t> accumulator_t;
interesting_predicate_bscore(const CTable& ctable,
weight_t kld_weight = 1.0,
weight_t skewness_weight = 1.0,
weight_t stdU_weight = 1.0,
weight_t skew_U_weight = 1.0,
score_t min_activation = 0.0,
score_t max_activation = 1.0,
score_t penalty = 1.0,
bool positive = true,
bool abs_skewness = false,
bool decompose_kld = false);
behavioral_score operator()(const combo_tree& tr) const;
behavioral_score best_possible_bscore() const;
score_t min_improv() const;
virtual void set_complexity_coef(unsigned alphabet_size, float p);
using bscore_base::set_complexity_coef;
protected:
counter_t _counter;
pdf_t _pdf;
mutable KLDS<contin_t> _klds;
CTable _ctable;
contin_t _skewness;
weight_t _kld_w;
weight_t _skewness_w;
bool _abs_skewness;
weight_t _stdU_w;
weight_t _skew_U_w;
score_t _min_activation, _max_activation;
score_t _penalty;
bool _positive;
bool _decompose_kld;
private:
score_t get_activation_penalty(score_t activation) const;
};
struct cluster_bscore : public bscore_base
{
cluster_bscore(const ITable&);
behavioral_score operator()(const combo_tree& tr) const;
behavioral_score best_possible_bscore() const;
score_t min_improv() const;
protected:
ITable _itable;
};
}
}
#endif