#ifndef _OPENCOG_FETCH_VALUE_OF_LINK_H
#define _OPENCOG_FETCH_VALUE_OF_LINK_H
#include <opencog/atoms/flow/ValueOfLink.h>
namespace opencog
{
class FetchValueOfLink : public ValueOfLink
{
private:
void init(void);
public:
FetchValueOfLink(const HandleSeq&&, Type = FETCH_VALUE_OF_LINK);
FetchValueOfLink(const FetchValueOfLink&) = delete;
FetchValueOfLink& operator=(const FetchValueOfLink&) = delete;
virtual ValuePtr execute(AtomSpace*, bool);
static Handle factory(const Handle&);
};
LINK_PTR_DECL(FetchValueOfLink)
#define createFetchValueOfLink CREATE_DECL(FetchValueOfLink)
}
extern "C" {
void opencog_persist_flow_init(void);
};
#endif