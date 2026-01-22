#ifndef _OPENCOG_JOIN_LINK_H
#define _OPENCOG_JOIN_LINK_H
#include <opencog/atoms/core/PrenexLink.h>
#include <opencog/atoms/value/QueueValue.h>
namespace opencog
{
class JoinCallback
{
public:
virtual ~JoinCallback() {}
virtual IncomingSet get_incoming_set(const Handle&) = 0;
};
class JoinLink : public PrenexLink
{
protected:
void init(void);
void validate(void);
HandleSet _const_terms;
Handle _meet;
void setup_meet(void);
size_t _jsize;
size_t _vsize;
Handle _top_var;
HandleSeq _top_clauses;
bool _need_top_map;
void setup_top_clauses(void);
HandleSeq _top_types;
void setup_top_types(void);
struct Traverse
{
JoinCallback *jcb;
HandleSet containers;
HandleMap replace_map;
HandleSetSeq join_map;
HandleSeqMap top_map;
};
HandleSet principals(AtomSpace*, Traverse&) const;
void principal_filter(Traverse&, HandleSet&, const Handle&) const;
void principal_filter_map(Traverse&, const HandleSeq&,
HandleSet&, const Handle&) const;
HandleSet upper_set(AtomSpace*, bool, Traverse&) const;
HandleSet supremum(AtomSpace*, bool, Traverse&) const;
HandleSet constrain(AtomSpace*, bool, Traverse&) const;
void fixup_replacements(Traverse&) const;
HandleSet replace(const Traverse&) const;
void find_top(Traverse&, const Handle&) const;
HandleSet container(AtomSpace*, JoinCallback*, bool) const;
virtual QueueValuePtr do_execute(AtomSpace*,
JoinCallback*, bool silent);
public:
JoinLink(const HandleSeq&&, Type=JOIN_LINK);
JoinLink(const JoinLink&) = delete;
JoinLink operator=(const JoinLink&) = delete;
virtual bool is_executable() const { return true; }
virtual ValuePtr execute(AtomSpace*, bool);
ValuePtr execute_cb(AtomSpace*, JoinCallback*);
static Handle factory(const Handle&);
};
LINK_PTR_DECL(JoinLink)
#define createJoinLink CREATE_DECL(JoinLink)
}
#endif