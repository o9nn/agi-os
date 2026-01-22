#ifndef _OPENCOG_NUMBER_OF_LINK_H
#define _OPENCOG_NUMBER_OF_LINK_H
#include <opencog/atoms/core/FunctionLink.h>
namespace opencog
{
class NumberOfLink : public FunctionLink
{
private:
void init(void);
public:
NumberOfLink(const HandleSeq&&, Type=NUMBER_OF_LINK);
NumberOfLink(const Handle&);
NumberOfLink(const NumberOfLink&) = delete;
NumberOfLink& operator=(const NumberOfLink&) = delete;
virtual ValuePtr execute(AtomSpace*, bool);
static Handle factory(const Handle&);
};
LINK_PTR_DECL(NumberOfLink)
#define createNumberOfLink CREATE_DECL(NumberOfLink)
}
#endif