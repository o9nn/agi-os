#ifndef _OPENCOG_STORE_VALUE_OF_LINK_H
#define _OPENCOG_STORE_VALUE_OF_LINK_H
#include <opencog/atoms/flow/ValueOfLink.h>
namespace opencog
{
class StoreValueOfLink : public ValueOfLink
{
private:
void init(void);
public:
StoreValueOfLink(const HandleSeq&&, Type = STORE_VALUE_OF_LINK);
StoreValueOfLink(const StoreValueOfLink&) = delete;
StoreValueOfLink& operator=(const StoreValueOfLink&) = delete;
virtual ValuePtr execute(AtomSpace*, bool);
static Handle factory(const Handle&);
};
LINK_PTR_DECL(StoreValueOfLink)
#define createStoreValueOfLink CREATE_DECL(StoreValueOfLink)
}
#endif