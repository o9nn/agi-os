#ifndef _OPENCOG_LINK_VALUE_H
#define _OPENCOG_LINK_VALUE_H
#include <vector>
#include <opencog/atoms/value/Value.h>
#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/atom_types/atom_types.h>
namespace opencog
{
class LinkValue
: public Value
{
friend class TransposeColumn;
protected:
mutable std::vector<ValuePtr> _value;
virtual void update() const {}
LinkValue(Type t) : Value(t) {}
public:
LinkValue(void)
: Value(LINK_VALUE) {}
LinkValue(const ValuePtr& vp)
: Value(LINK_VALUE) { _value.push_back(vp); }
LinkValue(const ValueSeq& vlist)
: Value(LINK_VALUE), _value(vlist) {}
LinkValue(ValueSeq&& vlist)
: Value(LINK_VALUE), _value(std::move(vlist)) {}
LinkValue(Type t, const ValueSeq& vlist)
: Value(t), _value(vlist) {}
LinkValue(Type t, ValueSeq&& vlist)
: Value(t), _value(std::move(vlist)) {}
LinkValue(Type t, const ValueSet& vset)
: Value(t)
{ for (const ValuePtr& v: vset) _value.emplace_back(v); }
LinkValue(Type t, const HandleSeq& hseq)
: Value(t)
{ for (const Handle& h: hseq) _value.emplace_back(h); }
LinkValue(Type t, const HandleSet& hset)
: Value(t)
{ for (const Handle& h: hset) _value.emplace_back(h); }
LinkValue(const ValueSet& vset)
: Value(LINK_VALUE)
{ for (const ValuePtr& v: vset) _value.emplace_back(v); }
LinkValue(const HandleSeq& hseq)
: Value(LINK_VALUE)
{ for (const Handle& h: hseq) _value.emplace_back(h); }
LinkValue(const HandleSet& hset)
: Value(LINK_VALUE)
{ for (const Handle& h: hset) _value.emplace_back(h); }
virtual ~LinkValue() {}
const ValueSeq& value() const { update(); return _value; }
HandleSeq to_handle_seq(void) const;
HandleSet to_handle_set(void) const;
size_t size() const { return _value.size(); }
virtual std::string to_string(const std::string& indent = "") const;
virtual std::string to_short_string(const std::string& indent = "") const;
virtual bool operator==(const Value&) const;
};
VALUE_PTR_DECL(LinkValue);
CREATE_VALUE_DECL(LinkValue);
}
#endif