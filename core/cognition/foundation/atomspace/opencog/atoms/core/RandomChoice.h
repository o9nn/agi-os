#ifndef _OPENCOG_RANDOM_CHOICE_LINK_H
#define _OPENCOG_RANDOM_CHOICE_LINK_H
#include <opencog/atoms/core/FunctionLink.h>
namespace opencog
{
class RandomChoiceLink : public FunctionLink
{
public:
RandomChoiceLink(const HandleSeq&&, Type=RANDOM_CHOICE_LINK);
RandomChoiceLink(const RandomChoiceLink&) = delete;
RandomChoiceLink& operator=(const RandomChoiceLink&) = delete;
virtual ValuePtr execute(AtomSpace*, bool);
static Handle factory(const Handle&);
};
LINK_PTR_DECL(RandomChoiceLink)
#define createRandomChoiceLink CREATE_DECL(RandomChoiceLink)
}
#endif