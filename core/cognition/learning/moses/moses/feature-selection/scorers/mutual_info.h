#ifndef _OPENCOG_FEATURE_SCORERS_MI_H
#define _OPENCOG_FEATURE_SCORERS_MI_H
#include <opencog/util/numeric.h>
#include <moses/comboreduct/table/table.h>
#include "fs_scorer_base.h"
namespace opencog {
using namespace combo;
template<typename FeatureSet>
struct MutualInformation
{
typedef FeatureSet argument_type;
typedef double result_type;
MutualInformation(const CTable& ctable)
: _ctable(ctable) {}
double operator()(const FeatureSet& features) const
{
return mutualInformation(_ctable, features);
}
protected:
const CTable& _ctable;
};
template<typename FeatureSet>
struct MICScorer
{
MICScorer(const ITable& it, const OTable& ot,
double confi = 100)
: _it(it), _ot(ot), _confi(confi) {}
double operator()(const FeatureSet& fs) const
{
double MI = mutualInformation(_it, _ot, fs);
double confidence = _it.size()/(_it.size() + exp(-_confi*fs.size()));
return MI * confidence;
}
const ITable& _it;
const OTable& _ot;
double _confi;
};
template<typename FeatureSet>
struct MICScorerCTable : public fs_scorer_base<FeatureSet>
{
typedef fs_scorer_base<FeatureSet> super;
MICScorerCTable(const CTable& ctable, double confi = 100)
: super(ctable, confi) {}
double operator()(const FeatureSet& fs) const
{
double MI = mutualInformation(super::_ctable, fs);
double confidence = super::confidence(fs.size());
logger().fine("MICScorerCTable MI = %g, confidence = %g",
MI, confidence);
return MI * confidence;
}
};
}
#endif