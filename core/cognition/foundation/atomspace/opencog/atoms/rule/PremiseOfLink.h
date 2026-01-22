#ifndef _OPENCOG_PREMISE_OF_LINK_H
#define _OPENCOG_PREMISE_OF_LINK_H
#include <opencog/atoms/rule/VardeclOfLink.h>
namespace opencog
{
class PremiseOfLink : public VardeclOfLink
{
private:
void init(void);
protected:
Handle _premise;
public:
PremiseOfLink(const HandleSeq&&, Type=PREMISE_OF_LINK);
PremiseOfLink(const PremiseOfLink&) = delete;
PremiseOfLink& operator=(const PremiseOfLink&) = delete;
virtual ValuePtr execute(AtomSpace*, bool);
static Handle factory(const Handle&);
};
LINK_PTR_DECL(PremiseOfLink)
#define createPremiseOfLink CREATE_DECL(PremiseOfLink)
}
#endif