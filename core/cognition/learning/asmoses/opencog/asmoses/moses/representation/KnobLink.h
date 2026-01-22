#ifndef ASMOSES_KNOBLINK_H
#define ASMOSES_KNOBLINK_H
#include <opencog/atoms/core/FunctionLink.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/value/Value.h>
#include <opencog/atoms/base/Link.h>
namespace opencog
{
class KnobLink : public FunctionLink
{
protected:
Handle _equiv;
void init();
public:
KnobLink(const HandleSeq&&, Type=KNOB_LINK);
virtual ValuePtr execute(AtomSpace*, bool silent=false) override;
static Handle factory(const Handle&);
bool is_component() {return _is_component;}
private:
bool _is_component;
};
typedef std::shared_ptr<KnobLink> KnobLinkPtr;
static inline KnobLinkPtr KnobLinkCast(const Handle& h)
{ return std::dynamic_pointer_cast<KnobLink>(h); }
static inline KnobLinkPtr KnobLinkCast(AtomPtr a)
{ return std::dynamic_pointer_cast<KnobLink>(a); }
#define createKnobLink std::make_shared<KnobLink>
}
#endif