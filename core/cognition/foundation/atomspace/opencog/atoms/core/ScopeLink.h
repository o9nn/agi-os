#ifndef _OPENCOG_SCOPE_LINK_H
#define _OPENCOG_SCOPE_LINK_H
#include <opencog/atoms/core/Quotation.h>
#include <opencog/atoms/core/VariableList.h>
namespace opencog
{
class ScopeLink : public Link
{
protected:
Handle _vardecl;
Handle _body;
Variables _variables;
bool _quoted;
void init(void);
void extract_variables(const HandleSeq& oset);
void init_scoped_variables(const Handle& vardecl);
bool skip_init(Type);
virtual ContentHash compute_hash() const;
ContentHash scope_hash(const FreeVariables::IndexMap& index) const;
ContentHash term_hash(const Handle&,
const FreeVariables::IndexMap& index,
Quotation quotation = Quotation()) const;
public:
ScopeLink(const HandleSeq&&, Type=SCOPE_LINK);
ScopeLink(const Handle& varcdecls, const Handle& body);
ScopeLink(const ScopeLink&) = delete;
ScopeLink& operator=(const ScopeLink&) = delete;
const Variables& get_variables(void) const { return _variables; }
const Handle& get_vardecl(void) const { return _vardecl; }
const Handle& get_body(void) const { return _body; }
void trim(const HandleSeq&);
Handle alpha_convert() const;
Handle alpha_convert(const HandleSeq& vars) const;
Handle alpha_convert(const HandleMap& vsmap) const;
bool is_equal(const Handle&, bool silent=false) const;
virtual bool operator==(const Atom&) const;
virtual bool operator!=(const Atom&) const;
static Handle factory(const Handle&);
};
LINK_PTR_DECL(ScopeLink)
#define createScopeLink CREATE_DECL(ScopeLink)
}
#endif