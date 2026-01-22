#ifndef _OPENCOG_TERM_MATCH_MIXIN_H
#define _OPENCOG_TERM_MATCH_MIXIN_H
#include <opencog/atoms/atom_types/types.h>
#include <opencog/atoms/core/Quotation.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/query/PatternMatchCallback.h>
namespace opencog {
class TermMatchMixin : public virtual PatternMatchCallback
{
public:
TermMatchMixin(AtomSpace*);
~TermMatchMixin();
virtual bool node_match(const Handle&, const Handle&);
virtual bool variable_match(const Handle&, const Handle&);
virtual bool scope_match(const Handle&, const Handle&);
virtual bool link_match(const PatternTermPtr&, const Handle&);
virtual bool post_link_match(const Handle&, const Handle&);
virtual void post_link_mismatch(const Handle&, const Handle&);
virtual bool clause_match(const Handle&, const Handle&,
const GroundingMap&);
virtual bool optional_clause_match(const Handle& pattrn,
const Handle& grnd,
const GroundingMap&);
virtual bool always_clause_match(const Handle& pattrn,
const Handle& grnd,
const GroundingMap&);
virtual IncomingSet get_incoming_set(const Handle&, Type);
virtual Handle get_link(const Handle&, Type, HandleSeq&&);
virtual bool evaluate_sentence(const Handle& pat, const GroundingMap& gnds)
{ return eval_sentence(pat, gnds); }
virtual const TypeSet& get_connectives(void)
{
return _connectives;
}
bool crisp_truth_from_tv(const TruthValuePtr& tvp)
{ return tvp->get_mean() >= 0.5; }
bool optionals_present(void) { return _optionals_present; }
protected:
NameServer& _nameserver;
bool is_self_ground(const Handle&, const Handle&,
const GroundingMap&, const HandleSet&,
Quotation quotation=Quotation());
const Variables* _pat_bound_vars;
const Variables* _gnd_bound_vars;
AtomSpace* _temp_aspace;
TypeSet _connectives;
bool eval_term(const Handle& pat, const GroundingMap& gnds);
bool eval_sentence(const Handle& pat, const GroundingMap& gnds);
bool _optionals_present = false;
AtomSpace* _as;
};
}
#endif