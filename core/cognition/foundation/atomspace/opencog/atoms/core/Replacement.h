#ifndef _OPENCOG_REPLACEMENT_H
#define _OPENCOG_REPLACEMENT_H
#include <map>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/core/Quotation.h>
namespace opencog
{
struct Replacement
{
typedef std::map<Handle, unsigned int> IndexMap;
static Handle replace_nocheck(const Handle&, const HandleMap&,
bool do_exec = false);
protected:
static Handle substitute_scoped(Handle, const HandleSeq&,
const IndexMap&,
bool do_exec,
Quotation quotation=Quotation());
static bool must_alpha_convert(const Handle& scope, const HandleSeq& args);
static bool must_alpha_hide(const Handle& scope, const IndexMap& index_map);
static IndexMap alpha_hide(const Handle& scope, const IndexMap& index_map);
};
}
#endif