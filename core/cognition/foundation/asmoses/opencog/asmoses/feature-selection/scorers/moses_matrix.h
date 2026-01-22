#ifndef _OPENCOG_FS_SCORERS_MATRIX_H
#define _OPENCOG_FS_SCORERS_MATRIX_H
#include <opencog/util/numeric.h>
#include <opencog/asmoses/data/table/table.h>
#include <opencog/asmoses/moses/scoring/precision_bscore.h>
#include "fs_scorer_base.h"
namespace opencog {
using namespace moses;
using combo::CompressedTable;
template<typename FeatureSet>
struct pre_scorer : public fs_scorer_base<FeatureSet>
{
typedef fs_scorer_base<FeatureSet> super;
pre_scorer(const CompressedTable& ctable,
double confi = 100,
float penalty = 1.0f,
float min_activation = 0.5f,
float max_activation = 1.0f,
bool positive = true)
: super(ctable, confi), _penalty(penalty),
_min_activation(min_activation), _max_activation(max_activation),
_positive(positive) {}
double operator()(const FeatureSet& fs) const
{
CompressedTable filtered_ctable = super::_ctable.filtered(fs);
precision_bscore sc(filtered_ctable, _penalty,
_min_activation, _max_activation,
_positive,
0.0f,
1.0f,
true,
1.0,
false,
TemporalGranularity::day,
true
);
double precision = boost::accumulate(sc.best_possible_bscore(), 0.0);
double cfdence = super::confidence(fs.size());
logger().fine("pre_scorer precision = %g, confidence = %g",
precision, cfdence);
return precision * cfdence;
}
protected:
double _penalty, _min_activation, _max_activation;
bool _positive;
};
}
#endif