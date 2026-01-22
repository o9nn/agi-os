#ifndef _DISCRIMINATING_BSCORE_H
#define _DISCRIMINATING_BSCORE_H
#include <moses/comboreduct/table/table.h>
#include "scoring_base.h"
namespace opencog { namespace moses {
using combo::CTable;
using combo::type_node;
struct discriminator
{
discriminator(const CTable&);
struct d_counts {
d_counts();
score_t true_positive_sum;
score_t false_positive_sum;
score_t positive_count;
score_t true_negative_sum;
score_t false_negative_sum;
score_t negative_count;
};
d_counts count(const combo_tree&) const;
std::vector<d_counts> counts(const combo_tree&) const;
protected:
const CTable& _ctable;
type_node _output_type;
score_t _true_total;
score_t _false_total;
std::function<score_t(const CTable::counter_t&)> sum_true;
std::function<score_t(const CTable::counter_t&)> sum_false;
};
struct discriminating_bscore : public bscore_ctable_base, discriminator
{
discriminating_bscore(const CTable& _ctable,
float min_threshold = 0.5f,
float max_threshold = 1.0,
float hardness = 1.0f);
virtual behavioral_score best_possible_bscore() const;
virtual score_t min_improv() const;
virtual void set_complexity_coef(score_t complexity_ratio);
virtual void set_complexity_coef(unsigned alphabet_size, float stddev);
protected:
/
score_t get_threshold_penalty(score_t) const;
size_t _ctable_usize;
score_t _max_output;
score_t _min_output;
float _min_threshold;
float _max_threshold;
float _hardness;
bool _full_bscore;
};
struct recall_bscore : public discriminating_bscore
{
recall_bscore(const CTable& _ctable,
float min_precision = 0.8f,
float max_precision = 1.0f,
float hardness = 1.0f);
behavioral_score operator()(const combo_tree& tr) const;
protected:
virtual score_t get_fixed(score_t pos, score_t neg, unsigned cnt) const;
virtual score_t get_variable(score_t pos, score_t neg, unsigned cnt) const;
};
struct prerec_bscore : public discriminating_bscore
{
prerec_bscore(const CTable& _ctable,
float min_recall = 0.5f,
float max_recall = 1.0f,
float hardness = 1.0f);
behavioral_score operator()(const combo_tree& tr) const;
protected:
virtual score_t get_fixed(score_t pos, score_t neg, unsigned cnt) const;
virtual score_t get_variable(score_t pos, score_t neg, unsigned cnt) const;
};
struct bep_bscore : public discriminating_bscore
{
bep_bscore(const CTable& _ctable,
float min_diff = 0.0f,
float max_diff = 0.5f,
float hardness = 1.0f);
behavioral_score operator()(const combo_tree& tr) const;
protected:
virtual score_t get_fixed(score_t pos, score_t neg, unsigned cnt) const;
virtual score_t get_variable(score_t pos, score_t neg, unsigned cnt) const;
};
struct f_one_bscore : public discriminating_bscore
{
f_one_bscore(const CTable& _ctable);
behavioral_score operator()(const combo_tree& tr) const;
protected:
virtual score_t get_fixed(score_t pos, score_t neg, unsigned cnt) const;
virtual score_t get_variable(score_t pos, score_t neg, unsigned cnt) const;
};
}
}
#endif