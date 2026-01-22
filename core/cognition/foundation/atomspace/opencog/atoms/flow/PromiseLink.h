#ifndef _OPENCOG_PROMISE_LINK_H
#define _OPENCOG_PROMISE_LINK_H
#include <opencog/atoms/base/Link.h>
namespace opencog
{
class PromiseLink : public Link
{
private:
void init(void);
Type _future_type;
HandleSeq _args;
public:
PromiseLink(const HandleSeq&&, Type=PROMISE_LINK);
PromiseLink(const Handle&);
PromiseLink(const Handle&, const Handle&);
PromiseLink(const PromiseLink&) = delete;
PromiseLink& operator=(const PromiseLink&) = delete;
virtual bool is_executable() const { return true; }
virtual ValuePtr execute(AtomSpace*, bool);
static Handle factory(const Handle&);
};
LINK_PTR_DECL(PromiseLink)
#define createPromiseLink CREATE_DECL(PromiseLink)
}
#endif