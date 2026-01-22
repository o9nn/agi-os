#ifndef _OPENCOG_ELEMENT_OF_LINK_H
#define _OPENCOG_ELEMENT_OF_LINK_H
#include <opencog/atoms/base/Link.h>
namespace opencog
{
class ElementOfLink : public Link
{
protected:
void init(void);
ValuePtr do_execute(const std::vector<double>&, const ValuePtr&);
public:
ElementOfLink(const Handle&, const Handle&);
ElementOfLink(const HandleSeq&&, Type=ELEMENT_OF_LINK);
ElementOfLink(const ElementOfLink&) = delete;
ElementOfLink& operator=(const ElementOfLink&) = delete;
virtual bool is_executable() const { return true; }
virtual ValuePtr execute(AtomSpace*, bool);
static Handle factory(const Handle&);
};
LINK_PTR_DECL(ElementOfLink)
#define createElementOfLink CREATE_DECL(ElementOfLink)
}
#endif