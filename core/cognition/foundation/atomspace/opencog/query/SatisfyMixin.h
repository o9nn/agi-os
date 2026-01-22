#ifndef _OPENCOG_SATISFY_MIXIN_H
#define _OPENCOG_SATISFY_MIXIN_H
#include "PatternMatchCallback.h"
namespace opencog {
class SatisfyMixin:
public virtual PatternMatchCallback
{
bool cartesian_product(const HandleSeq& virtuals,
const PatternTermSeq& absents,
const GroundingMap& var_gnds,
const GroundingMap& term_gnds,
GroundingMapSeqSeq comp_var_gnds,
GroundingMapSeqSeq comp_term_gnds);
public:
virtual bool satisfy(const PatternLinkPtr&);
};
};
#endif