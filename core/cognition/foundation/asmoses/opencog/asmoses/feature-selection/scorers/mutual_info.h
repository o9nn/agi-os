#ifndef _OPENCOG_FEATURE_SCORERS_MI_H
#define _OPENCOG_FEATURE_SCORERS_MI_H
#include <opencog/util/numeric.h>
#include <opencog/asmoses/data/table/table.h>
#include "fs_scorer_base.h"
namespace opencog {
using namespace combo;
template<typename FeatureSet>
struct MutualInformation
{
MutualInformation(const CompressedTable& ctable)
: _ctable(ctable) {}
typedef FeatureSet argument_type;
typedef double result_type;
double operator()(const FeatureSet& features) const
{
return mutualInformation(_ctable, features);
}
protected:
const CompressedTable& _ctable;
};
template<typename FeatureSet>
struct MICScorer
{
typedef FeatureSet argument_type;
typedef double result_type;
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
struct MICScorerCompressedTable : public fs_scorer_base<FeatureSet>
{
typedef fs_scorer_base<FeatureSet> super;
MICScorerCompressedTable(const CompressedTable& ctable, double confi = 100)
: super(ctable, confi) {}
double operator()(const FeatureSet& fs) const
{
double MI = mutualInformation(super::_ctable, fs);
double confidence = super::confidence(fs.size());
logger().fine("MICScorerCompressedTable MI = %g, confidence = %g",
MI, confidence);
return MI * confidence;
}
};
}
#endif