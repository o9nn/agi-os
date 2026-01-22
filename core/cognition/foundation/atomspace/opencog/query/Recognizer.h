#ifndef _OPENCOG_RECOGNIZER_H
#define _OPENCOG_RECOGNIZER_H
#include <opencog/query/TermMatchMixin.h>
#include <opencog/query/SatisfyMixin.h>
namespace opencog {
class Recognizer :
public TermMatchMixin,
public SatisfyMixin
{
private:
bool match = false;
protected:
DECLARE_PE_MUTEX;
PatternTermPtr _root;
PatternTermPtr _starter_term;
size_t _cnt;
bool do_search(PatternMatchCallback&, const Handle&);
bool loose_match(const Handle&, const Handle&);
public:
HandleSet _rules;
Recognizer(AtomSpace* as) :
TermMatchMixin(as),
_cnt(0)
{}
virtual bool node_match(const Handle&, const Handle&);
virtual bool link_match(const PatternTermPtr&, const Handle&);
virtual bool fuzzy_match(const Handle&, const Handle&);
virtual bool propose_grounding(const GroundingMap &var_soln,
const GroundingMap &term_soln);
virtual bool perform_search(PatternMatchCallback&);
virtual void next_connections(const GroundingMap&);
virtual bool get_next_clause(PatternTermPtr&, PatternTermPtr&);
};
}
#endif