#ifndef _OPENCOG_ENSEMBLE_H
#define _OPENCOG_ENSEMBLE_H
#include "ensemble_params.h"
#include "opencog/asmoses/moses/moses/types.h"
#include "opencog/asmoses/moses/scoring/behave_cscore.h"
#include "opencog/asmoses/moses/scoring/scoring_base.h"
namespace opencog
{
namespace moses
{
class ensemble
{
public:
ensemble(behave_cscore&,
const ensemble_parameters& ep = ensemble_parameters());
void add_candidates(scored_combo_tree_set&);
const scored_combo_tree_set& get_ensemble() const
{
return _scored_trees;
}
const combo::combo_tree& get_weighted_tree() const;
score_t flat_score() const;
private:
const ensemble_parameters& _params;
bscore_base& _bscorer;
double _tolerance;
double _bias;
std::vector<double> _row_bias;
scored_combo_tree_set _scored_trees;
mutable combo_tree _weighted_tree;
void add_adaboost(scored_combo_tree_set&);
void add_expert(scored_combo_tree_set&);
const combo::combo_tree& get_adaboost_tree() const;
const combo::combo_tree& get_exact_tree() const;
const combo::combo_tree& get_expert_tree() const;
};
}};
#endif