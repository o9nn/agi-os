#ifndef _OPENCOG_PUT_LINK_H
#define _OPENCOG_PUT_LINK_H
#include <opencog/atoms/core/PrenexLink.h>
namespace opencog
{
class PutLink : public PrenexLink
{
protected:
Handle _arguments;
void init(void);
void static_typecheck_arguments(void);
Handle do_reduce(void) const;
public:
PutLink(const HandleSeq&&, Type=PUT_LINK);
PutLink(const PutLink&) = delete;
PutLink& operator=(const PutLink&) = delete;
virtual ~PutLink() {}
Handle get_arguments() { return _arguments; }
virtual bool is_executable() const { return true; }
virtual ValuePtr execute(AtomSpace*, bool silent=false);
static Handle factory(const Handle&);
};
LINK_PTR_DECL(PutLink)
#define createPutLink CREATE_DECL(PutLink)
}
#endif