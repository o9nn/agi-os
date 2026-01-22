#ifndef _MOSES_SS_BSCORE_H
#define _MOSES_SS_BSCORE_H
#include "scoring_base.h"
namespace opencog { namespace moses {
using combo::combo_tree;
using combo::arity_t;
using combo::CTable;
using combo::TTable;
struct ss_bscore : public bscore_base
{
ss_bscore(const bscore_base& bscorer, unsigned n_subsamples = 0,
float low_dev_pressure = 0.0, bool by_time = true);
behavioral_score operator()(const combo_tree& tr) const;
behavioral_score best_possible_bscore() const;
score_t min_improv() const;
void ignore_cols(const std::set<arity_t>&) const;
void ignore_rows(const std::set<unsigned>&) const;
void ignore_rows_at_times(const std::set<TTable::value_type>&) const;
unsigned get_ctable_usize() const;
const CTable& get_ctable() const;
protected:
const bscore_base& _bscorer;
unsigned _n_subsamples;
float _low_dev_pressure;
bool _by_time;
std::set<TTable::value_type> _timestamps;
};
}
}
#endif