#ifndef _ATTENTIONAL_FOCUS_CB_H
#define _ATTENTIONAL_FOCUS_CB_H
#include <opencog/query/TermMatchMixin.h>
namespace opencog {
class AttentionalFocusCB: public TermMatchMixin
{
public:
AttentionalFocusCB(AtomSpace*);
bool node_match(const Handle&, const Handle&);
bool link_match(const PatternTermPtr&, const Handle&);
IncomingSet get_incoming_set(const Handle&, Type);
};
}
#endif