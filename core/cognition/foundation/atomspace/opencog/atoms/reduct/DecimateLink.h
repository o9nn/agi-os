#ifndef _OPENCOG_DECIMATE_LINK_H
#define _OPENCOG_DECIMATE_LINK_H
#include <opencog/atoms/base/Link.h>
namespace opencog
{
class DecimateLink : public Link
{
protected:
void init(void);
ValuePtr do_execute(const std::vector<bool>&, const ValuePtr&);
public:
DecimateLink(const Handle&, const Handle&);
DecimateLink(const HandleSeq&&, Type=DECIMATE_LINK);
DecimateLink(const DecimateLink&) = delete;
DecimateLink& operator=(const DecimateLink&) = delete;
virtual bool is_executable() const { return true; }
virtual ValuePtr execute(AtomSpace*, bool);
static Handle factory(const Handle&);
};
LINK_PTR_DECL(DecimateLink)
#define createDecimateLink CREATE_DECL(DecimateLink)
}
#endif