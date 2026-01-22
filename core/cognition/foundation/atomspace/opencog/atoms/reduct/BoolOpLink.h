#ifndef _OPENCOG_BOOL_OP_LINK_H
#define _OPENCOG_BOOL_OP_LINK_H
#include <opencog/atoms/base/Link.h>
namespace opencog
{
class BoolOpLink : public Link
{
protected:
void init(void);
public:
BoolOpLink(const HandleSeq&&, Type);
BoolOpLink(const BoolOpLink&) = delete;
BoolOpLink& operator=(const BoolOpLink&) = delete;
virtual bool is_executable() const { return true; }
virtual ValuePtr execute(AtomSpace*, bool);
static Handle factory(const Handle&);
};
LINK_PTR_DECL(BoolOpLink)
#define createBoolOpLink CREATE_DECL(BoolOpLink)
}
#endif