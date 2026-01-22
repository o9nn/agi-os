#ifndef _OPENCOG_STRING_OF_LINK_H
#define _OPENCOG_STRING_OF_LINK_H
#include <opencog/atoms/core/FunctionLink.h>
namespace opencog
{
class StringOfLink : public FunctionLink
{
private:
void init(void);
public:
StringOfLink(const HandleSeq&&, Type=STRING_OF_LINK);
StringOfLink(const Handle&, const Handle&);
StringOfLink(const StringOfLink&) = delete;
StringOfLink& operator=(const StringOfLink&) = delete;
virtual ValuePtr execute(AtomSpace*, bool);
static Handle factory(const Handle&);
};
LINK_PTR_DECL(StringOfLink)
#define createStringOfLink CREATE_DECL(StringOfLink)
}
#endif