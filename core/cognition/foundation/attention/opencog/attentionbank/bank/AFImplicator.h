#ifndef _OPENCOG_AF_IMPLICATOR_H
#define _OPENCOG_AF_IMPLICATOR_H
#include <opencog/attentionbank/bank/AttentionalFocusCB.h>
#include <opencog/query/RewriteMixin.h>
#include <opencog/query/InitiateSearchMixin.h>
#include <opencog/query/SatisfyMixin.h>
namespace opencog {
class AFImplicator:
public AttentionalFocusCB,
public InitiateSearchMixin,
public RewriteMixin,
public SatisfyMixin
{
public:
AFImplicator(AtomSpace* asp, ContainerValuePtr cvp) :
AttentionalFocusCB(asp),
InitiateSearchMixin(asp),
RewriteMixin(asp, cvp)
{}
virtual void set_pattern(const Variables& vars,
const Pattern& pat)
{
InitiateSearchMixin::set_pattern(vars, pat);
AttentionalFocusCB::set_pattern(vars, pat);
}
};
Handle af_bindlink(AtomSpace*, const Handle&);
};
#endif