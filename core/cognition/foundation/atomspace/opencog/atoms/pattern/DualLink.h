#ifndef _OPENCOG_DUAL_LINK_H
#define _OPENCOG_DUAL_LINK_H
#include <opencog/atoms/pattern/PatternLink.h>
namespace opencog
{
class DualLink : public PatternLink
{
protected:
void init(void);
public:
DualLink(const HandleSeq&&, Type=DUAL_LINK);
DualLink(const DualLink&) = delete;
DualLink& operator=(const DualLink&) = delete;
virtual bool is_executable() const { return true; }
virtual ValuePtr execute(AtomSpace*, bool silent=false);
static Handle factory(const Handle&);
};
LINK_PTR_DECL(DualLink)
#define createDualLink CREATE_DECL(DualLink)
}
#endif