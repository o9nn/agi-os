#ifndef _OPENCOG_VALUE_H
#define _OPENCOG_VALUE_H
#include <memory>
#include <string>
#include <opencog/util/empty_string.h>
#include <opencog/atoms/atom_types/types.h>
#include <opencog/atoms/atom_types/NameServer.h>
#include <opencog/atoms/base/Handle.h>
namespace opencog
{
class Value;
typedef std::shared_ptr<Value> ValuePtr;
class Value
: public std::enable_shared_from_this<Value>
{
protected:
Type _type;
public:
Value(Type t) : _type(t) {}
virtual ~Value() {}
inline Type get_type() const { return _type; }
virtual bool is_atom() const { return false; }
virtual bool is_node() const { return false; }
virtual bool is_link() const { return false; }
virtual bool is_unordered_link() const { return false; }
virtual size_t size() const { return 0; }
bool is_type(Type t, bool subclass = true) const
{
Type at(get_type());
if (not subclass) return t == at;
return nameserver().isA(at, t);
}
virtual std::string to_string(const std::string& indent) const = 0;
virtual std::string to_short_string(const std::string& indent) const
{ return to_string(indent); }
std::string to_string() const { return to_string(""); }
std::string to_short_string() const { return to_short_string(""); }
virtual bool operator==(const Value&) const = 0;
bool operator!=(const Value& other) const
{ return not operator==(other); }
};
#define SAFE_UPDATE(PRTSTR,PRINTER) \
try { \
update(); \
{ PRINTER; } \
} catch (const StandardException& ex) { \
PRTSTR += " \""; \
PRTSTR += ex.what(); \
PRTSTR += "\""; \
}
typedef std::vector<ValuePtr> ValueSeq;
typedef std::set<ValuePtr> ValueSet;
typedef std::map<Handle, ValuePtr> ValueMap;
std::string oc_to_string(const ValuePtr& vp,
const std::string& indent=empty_string);
std::string oc_to_string(const ValueSeq& vs,
const std::string& indent=empty_string);
std::string oc_to_string(const ValueMap& vs,
const std::string& indent=empty_string);
class Atom;
template<typename T, typename ... Args>
static inline
typename std::enable_if<
std::is_base_of<Value, T>::value && !std::is_base_of<Atom, T>::value,
std::shared_ptr<T> >::type
createValue(Args&&... args) {
return std::make_shared<T>(std::forward<Args>(args)...);
}
#define VALUE_PTR_DECL(CNAME) \
typedef std::shared_ptr<CNAME> CNAME##Ptr; \
static inline CNAME##Ptr CNAME##Cast(const ValuePtr& a) \
{ return std::dynamic_pointer_cast<CNAME>(a); } \
static inline const ValuePtr ValueCast(const CNAME##Ptr& fv) \
{ return std::shared_ptr<Value>(fv, (Value*) fv.get()); }
#define CREATE_VALUE_DECL(CNAME) \
template<typename ... Type> \
static inline std::shared_ptr<CNAME> create##CNAME(Type&&... args) \
{ return std::make_shared<CNAME>(std::forward<Type>(args)...); }
}
namespace std
{
template<typename Out>
Out& operator<<(Out& out, const opencog::ValuePtr& pa)
{
out << pa->to_string("");
return out;
}
}
#endif