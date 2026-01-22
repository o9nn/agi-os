#ifndef _SELECT_BSCORE_H
#define _SELECT_BSCORE_H
#include <moses/comboreduct/table/table.h>
#include "scoring_base.h"
namespace opencog { namespace moses {
using combo::CTable;
using combo::count_t;
struct select_bscore : public bscore_ctable_base
{
select_bscore(const CTable& ctable,
double lower_percentile = 0.8f,
double upper_percentile = 0.9f,
double hardness = 1.0f,
bool positive = true);
behavioral_score operator()(const combo_tree& tr) const;
behavioral_score operator()(const scored_combo_tree_set&) const;
score_t get_error(const behavioral_score&) const;
behavioral_score best_possible_bscore() const;
behavioral_score worst_possible_bscore() const;
score_t min_improv() const;
void reset_weights();
void update_weights(const std::vector<double>&);
protected:
void set_best_possible_bscore();
behavioral_score _best_possible_score;
double _effective_length;
bool _positive;
score_t _lower_bound;
score_t _upper_bound;
};
}
}
#endif