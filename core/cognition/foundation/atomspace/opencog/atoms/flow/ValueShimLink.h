#ifndef _OPENCOG_VALUE_SHIM_LINK_H
#define _OPENCOG_VALUE_SHIM_LINK_H
#include <opencog/atoms/base/Link.h>
namespace opencog
{
class ValueShimLink : public Link
{
private:
ValuePtr val;
public:
ValueShimLink(Type=VALUE_SHIM_LINK);
ValueShimLink(const HandleSeq&, Type=VALUE_SHIM_LINK);
ValueShimLink(const ValuePtr& v) : Link(VALUE_SHIM_LINK), val(v) {}
ValueShimLink(const ValueShimLink&) = delete;
ValueShimLink& operator=(const ValueShimLink&) = delete;
void set_value(const ValuePtr& v) { val = v; }
virtual ValuePtr execute(AtomSpace*, bool) { return val; }
virtual bool is_executable() const { return true; }
virtual void setAtomSpace(AtomSpace *);
virtual std::string to_string(const std::string& = "") const;
virtual std::string to_short_string(const std::string& = "") const;
static Handle factory(const Handle&);
};
LINK_PTR_DECL(ValueShimLink)
#define createValueShimLink CREATE_DECL(ValueShimLink)
}
#endif