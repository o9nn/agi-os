#ifndef _OPENCOG_TYPE_CHOICE_H
#define _OPENCOG_TYPE_CHOICE_H
#include <opencog/atoms/base/Link.h>
namespace opencog
{
typedef std::pair<size_t, size_t> GlobInterval;
class TypeChoice;
LINK_PTR_DECL(TypeChoice)
typedef std::set<TypeChoicePtr> TypeChoiceSet;
class TypeChoice : public Link
{
protected:
TypeSet _simple_typeset;
HandleSet _deep_typeset;
TypeChoiceSet _sect_typeset;
GlobInterval _glob_interval;
bool _is_untyped;
void init(bool);
bool pre_analyze(bool);
void analyze(Handle);
void post_analyze(bool);
GlobInterval make_interval(const HandleSeq&);
bool is_nonglob_type(const ValuePtr&) const;
ContentHash compute_hash() const;
public:
TypeChoice(const HandleSeq&&, Type=TYPE_CHOICE, bool=false);
TypeChoice(const TypeChoice&) = delete;
TypeChoice& operator=(const TypeChoice&) = delete;
TypeSet get_simple_typeset(void) const { return _simple_typeset; }
HandleSet get_deep_typeset(void) const { return _deep_typeset; }
GlobInterval get_glob_interval(void) const
{ return _glob_interval; }
static const GlobInterval default_interval(bool);
static bool is_empty(const GlobInterval&);
bool is_simple(void) const;
bool is_globby(void) const;
bool is_lower_bound(size_t) const;
bool is_upper_bound(size_t) const;
bool is_type(const ValuePtr&) const;
bool is_type(Type) const;
bool is_untyped(bool) const;
bool is_equal(const TypeChoice&) const;
bool operator==(const Atom&) const;
std::string to_string(const std::string& indent) const;
using Atom::to_string;
static Handle factory(const Handle&);
};
#define createTypeChoice CREATE_DECL(TypeChoice)
std::string oc_to_string(const TypeChoiceSet&,
const std::string& indent=empty_string);
}
#endif