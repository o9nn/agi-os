#ifndef _OPENCOG_COND_LINK_H
#define _OPENCOG_COND_LINK_H
#include <opencog/atoms/core/FunctionLink.h>
namespace opencog
{
class CondLink : public FunctionLink
{
protected:
HandleSeq conds;
HandleSeq exps;
Handle default_exp;
void init(void);
public:
CondLink(const HandleSeq&&, Type=COND_LINK);
CondLink(const CondLink&) = delete;
CondLink& operator=(const CondLink&) = delete;
virtual ValuePtr execute(AtomSpace*, bool);
static Handle factory(const Handle&);
};
LINK_PTR_DECL(CondLink)
#define createCondLink CREATE_DECL(CondLink)
}
#endif