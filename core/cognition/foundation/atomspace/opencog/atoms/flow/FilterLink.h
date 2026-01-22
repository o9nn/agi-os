#ifndef _OPENCOG_FILTER_LINK_H
#define _OPENCOG_FILTER_LINK_H
#include <opencog/atoms/core/FunctionLink.h>
#include <opencog/atoms/core/ScopeLink.h>
#include <opencog/atoms/core/Quotation.h>
namespace opencog
{
class FilterLink : public FunctionLink
{
protected:
ScopeLinkPtr _pattern;
const Variables* _mvars;
const HandleSet* _varset;
HandleSet _globby_terms;
HandleSeq _rewrite;
void init(void);
FilterLink(Type, const Handle&);
bool extract(const Handle&, const ValuePtr&, ValueMap&,
AtomSpace*, bool,
Quotation quotation=Quotation()) const;
ValuePtr rewrite_one(const ValuePtr&, AtomSpace*, bool) const;
template<typename VECT>
bool glob_compare(const HandleSeq&, const VECT&,
ValueMap&, AtomSpace*, bool, Quotation,
ValuePtr (*)(const VECT&&),
size_t, size_t) const;
mutable bool _recursive_glob;
public:
FilterLink(const HandleSeq&&, Type=FILTER_LINK);
FilterLink(const Handle& pattern, const Handle& term);
FilterLink(const FilterLink&) = delete;
FilterLink operator=(const FilterLink&) = delete;
virtual ValuePtr execute(AtomSpace*, bool);
static Handle factory(const Handle&);
};
LINK_PTR_DECL(FilterLink);
#define createFilterLink CREATE_DECL(FilterLink)
}
#endif