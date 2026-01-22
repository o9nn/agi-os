#ifndef _OPENCOG_MAX_LINK_H
#define _OPENCOG_MAX_LINK_H
#include <opencog/atoms/reduct/NumericFunctionLink.h>
namespace opencog
{
class MaxLink : public NumericFunctionLink
{
protected:
void init(void);
public:
MaxLink(const HandleSeq&&, Type=MAX_LINK);
MaxLink(const MaxLink&) = delete;
MaxLink& operator=(const MaxLink&) = delete;
virtual ValuePtr execute(AtomSpace*, bool);
static Handle factory(const Handle&);
};
LINK_PTR_DECL(MaxLink)
#define createMaxLink CREATE_DECL(MaxLink)
}
#endif