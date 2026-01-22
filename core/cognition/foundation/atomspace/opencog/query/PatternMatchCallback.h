#ifndef _OPENCOG_PATTERN_MATCH_CALLBACK_H
#define _OPENCOG_PATTERN_MATCH_CALLBACK_H
#include <map>
#include <set>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/pattern/PatternLink.h>
#include <opencog/atoms/pattern/PatternTerm.h>
namespace opencog {
class PatternMatchCallback
{
public:
virtual ~PatternMatchCallback() {};
virtual bool node_match(const Handle& patt_node,
const Handle& grnd_node) = 0;
virtual bool variable_match(const Handle& patt_node,
const Handle& grnd_node) = 0;
virtual bool scope_match(const Handle& patt_node,
const Handle& grnd_node) = 0;
virtual bool link_match(const PatternTermPtr& patt_link,
const Handle& grnd_link) = 0;
virtual bool post_link_match(const Handle& patt_link,
const Handle& grnd_link)
{
return true;
}
virtual void post_link_mismatch(const Handle& patt_link,
const Handle& grnd_link)
{}
virtual bool fuzzy_match(const Handle& ph, const Handle& gh)
{
return false;
}
virtual bool evaluate_sentence(const Handle& eval,
const GroundingMap& gnds) = 0;
virtual bool clause_match(const Handle& pattrn_link_h,
const Handle& grnd_link_h,
const GroundingMap& term_gnds)
{
if (pattrn_link_h == grnd_link_h) return false;
return true;
}
virtual bool optional_clause_match(const Handle& pattrn,
const Handle& grnd,
const GroundingMap& term_gnds) = 0;
virtual bool always_clause_match(const Handle& pattrn,
const Handle& grnd,
const GroundingMap& term_gnds) = 0;
virtual bool propose_grounding(const GroundingMap &var_soln,
const GroundingMap &term_soln) = 0;
virtual bool propose_grouping(const GroundingMap &var_soln,
const GroundingMap &term_soln,
const GroundingMap &grouping)
{
return propose_grounding(var_soln, term_soln);
}
virtual IncomingSet get_incoming_set(const Handle& h, Type t)
{
return h->getIncomingSetByType(t);
}
virtual Handle get_link(const Handle& hg,
Type t, HandleSeq&& oset) = 0;
virtual const TypeSet& get_connectives(void)
{ static const TypeSet _empty; return _empty; }
virtual bool start_search(void) { return false; }
virtual bool perform_search(PatternMatchCallback&) = 0;
virtual bool search_finished(bool done) { return done; }
virtual void next_connections(const GroundingMap& var_grounding) = 0;
virtual bool get_next_clause(PatternTermPtr& clause,
PatternTermPtr& joint) = 0;
virtual void push(void) {}
virtual void pop(void) {}
const Variables* _variables = nullptr;
const Pattern* _pattern = nullptr;
virtual void set_pattern(const Variables& vars,
const Pattern& pat)
{
_variables = &vars;
_pattern = &pat;
}
virtual bool satisfy(const PatternLinkPtr&) = 0;
};
#ifdef USE_THREADED_PATTERN_ENGINE
#define DECLARE_PE_MUTEX std::mutex _mtx;
#define LOCK_PE_MUTEX std::lock_guard<std::mutex> lck(_mtx);
#else
#define DECLARE_PE_MUTEX
#define LOCK_PE_MUTEX
#endif
}
#endif