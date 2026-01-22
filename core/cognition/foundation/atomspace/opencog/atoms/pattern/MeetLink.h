#ifndef _OPENCOG_MEET_LINK_H
#define _OPENCOG_MEET_LINK_H
#include <opencog/atoms/pattern/PatternLink.h>
#include <opencog/atoms/value/ContainerValue.h>
namespace opencog
{
class MeetLink : public PatternLink
{
protected:
void init(void);
virtual ContainerValuePtr do_execute(AtomSpace*, bool silent);
public:
MeetLink(const HandleSeq&&, Type=MEET_LINK);
MeetLink(const MeetLink&) = delete;
MeetLink operator=(const MeetLink&) = delete;
virtual bool is_executable() const { return true; }
virtual ValuePtr execute(AtomSpace*, bool silent=false);
static Handle factory(const Handle&);
};
LINK_PTR_DECL(MeetLink)
#define createMeetLink CREATE_DECL(MeetLink)
}
#endif