#ifndef _MOSES_SCORING_BASE_H
#define _MOSES_SCORING_BASE_H
#include "opencog/asmoses/moses/moses/types.h"
#include <opencog/asmoses/combo/combo/vertex.h>
#include <opencog/asmoses/data/table/table.h>
namespace opencog
{
namespace moses
{
using combo::combo_tree;
using combo::arity_t;
using combo::count_t;
using combo::CompressedTable;
using combo::TTable;
score_t discrete_complexity_coef(unsigned alphabet_size, double p);
score_t contin_complexity_coef(unsigned alphabet_size, double stdev);
struct bscore_base
{
typedef behavioral_score result_type;
bscore_base() : _return_weighted_score(false), _complexity_coef(0.0), _size(0) {};
virtual ~bscore_base()
{};
virtual behavioral_score operator()(const combo_tree &) const = 0;
virtual behavioral_score operator()(const Handle &) const;
virtual behavioral_score operator()(const scored_combo_tree_set &) const;
virtual size_t size() const
{ return _size; }
virtual behavioral_score best_possible_bscore() const = 0;
virtual behavioral_score worst_possible_bscore() const;
virtual combo_tree gen_canonical_best_candidate() const;
virtual score_t min_improv() const
{ return 0.0; }
void use_weighted_scores()
{ _return_weighted_score = true; }
virtual score_t sum_bscore(const behavioral_score &) const;
virtual void reset_weights();
virtual void update_weights(const std::vector<double> &);
virtual score_t get_error(const behavioral_score &) const;
virtual score_t get_error(const combo_tree &) const;
virtual void ignore_cols(const std::set<arity_t> &) const
{}
virtual void ignore_rows(const std::set<unsigned> &) const
{}
virtual void ignore_rows_at_times(const std::set<TTable::value_type> &) const
{}
virtual unsigned get_ctable_usize() const
{
OC_ASSERT(false, "You must implement me in the derived class");
return 0U;
}
virtual const CompressedTable &get_ctable() const
{
static const CompressedTable empty_ctable;
OC_ASSERT(false, "You must implement me in the derived class");
return empty_ctable;
}
virtual complexity_t get_complexity(const combo_tree &tr) const
{
return tree_complexity(tr);
}
virtual complexity_t get_complexity(const scored_combo_tree_set &) const;
virtual complexity_t get_complexity(const Handle &handle) const
{
return atomese_complexity(handle);
}
virtual score_t get_complexity_coef() const
{ return _complexity_coef; }
virtual void set_complexity_coef(score_t complexity_ratio);
virtual void set_complexity_coef(unsigned alphabet_size, float p);
protected:
mutable bool _return_weighted_score;
score_t _complexity_coef;
mutable size_t _size;
std::vector<double> _weights;
};
struct bscore_ctable_base : public bscore_base
{
bscore_ctable_base(const CompressedTable &);
void ignore_cols(const std::set<arity_t> &) const;
void ignore_rows(const std::set<unsigned> &) const;
void ignore_rows_at_times(const std::set<TTable::value_type> &) const;
unsigned get_ctable_usize() const;
const CompressedTable &get_ctable() const;
protected:
const CompressedTable &_orig_ctable;
mutable CompressedTable _wrk_ctable;
mutable CompressedTable _all_rows_wrk_ctable;
mutable size_t _ctable_usize;
mutable count_t _ctable_weight;
void recompute_weight() const;
};
static inline combo_tree conjunction_from_truth_row(const combo::builtin_seq& row)
{
combo_tree tr;
auto head = tr.set_head(combo::id::logical_and);
arity_t idx = 1;
for (const combo::builtin cell : row) {
combo::argument arg(cell == combo::id::logical_true ? idx++ : -idx++);
tr.append_child(head, arg);
}
return tr;
}
static inline void log_candidate_bscore(const combo_tree &tr,
const behavioral_score &bs)
{
if (logger().is_fine_enabled())
logger().fine() << "Evaluate candidate: " << tr << "\n"
<< "\tBScore size=" << bs.size()
<< " bscore: " << bs;
}
static inline void log_candidate_bscore(const Handle &handle,
const behavioral_score &bs)
{
if (logger().is_fine_enabled())
logger().fine() << "Evaluate candidate: " << handle->to_string() << "\n"
<< "\tBScore size=" << bs.size()
<< " bscore: " << bs;
}
}
}
#endif