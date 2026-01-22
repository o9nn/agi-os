#ifndef ASMOSES_MPULSELINK_H
#define ASMOSES_MPULSELINK_H
#include <opencog/atoms/core/FunctionLink.h>
#include <opencog/atoms/core/ScopeLink.h>
#include <opencog/atoms/core/Quotation.h>
#include <opencog/asmoses/atomese/atom_types/atom_types.h>
namespace opencog
{
class MpulseLink : public FunctionLink
{
public:
MpulseLink(const HandleSeq&, Type= MPULSE_LINK);
virtual ValuePtr execute(AtomSpace*, bool);
static Handle factory(const Handle&);
void init(void);
};
typedef std::shared_ptr<MpulseLink> MpulseLinkPtr;
static inline MpulseLinkPtr MpulseLinkCast(const Handle& h)
{
AtomPtr a(h);
return std::dynamic_pointer_cast<MpulseLink>(a);
}
static inline MpulseLinkPtr MpulseLinkCast(AtomPtr a)
{ return std::dynamic_pointer_cast<MpulseLink>(a); }
#define createMpulseLink std::make_shared<MpulseLink>
}
#endif