#ifndef _OPENCOG_REWRITE_LINK_H
#define _OPENCOG_REWRITE_LINK_H
#include <opencog/atoms/core/ScopeLink.h>
namespace opencog
{
class RewriteLink : public ScopeLink
{
protected:
bool _silent;
void init(void);
Handle substitute_vardecl(const HandleMap& vm) const;
static Handle substitute_vardecl(const Handle& vardecl,
const HandleMap& vm);
HandleSeq substitute_bodies(const Handle& nvardecl,
const HandleMap& vm) const;
Handle substitute_body(const Handle& nvardecl,
const Handle& body,
const HandleMap& vm) const;
static bool is_bound_to_ancestor(const Variables& variables,
const Handle& local_scope);
static bool is_scope_bound_to_ancestor(const Variables& variables,
const Handle& h);
static bool is_logical_connector(Type);
static bool is_logical_connector(const Handle&);
public:
RewriteLink(const HandleSeq&&, Type=REWRITE_LINK);
RewriteLink(const Handle& varcdecls, const Handle& body);
RewriteLink(const RewriteLink &) = delete;
RewriteLink& operator=(const RewriteLink &) = delete;
void make_silent(bool s) { _silent = s; }
virtual Handle beta_reduce(const HandleMap& vm) const;
virtual Handle beta_reduce(const HandleSeq& arguments) const;
HandleSeq beta_reduce_bodies(const Handle& nvardecl,
const HandleMap& vm) const;
Handle consume_quotations() const;
static Handle consume_quotations(const Variables& variables, const Handle& h,
Quotation quotation,
bool& needless_quotation,
bool clause_root);
static HandleSeq consume_quotations(const Variables& variables,
const HandleSeq& hs,
Quotation quotation,
bool& needless_quotation,
bool clause_root);
static Handle consume_quotations_mere_rec(const Variables& variables,
const Handle& h,
Quotation quotation,
bool& needless_quotation,
bool clause_root);
static Handle factory(const Handle&);
};
LINK_PTR_DECL(RewriteLink)
#define createRewriteLink CREATE_DECL(RewriteLink)
}
#endif