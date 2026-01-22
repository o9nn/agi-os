#ifndef _OPENCOG_GET_LINK_H
#define _OPENCOG_GET_LINK_H
#include <opencog/atoms/pattern/MeetLink.h>
namespace opencog
{
class GetLink : public MeetLink
{
protected:
void init(void);
public:
GetLink(const HandleSeq&&, Type=GET_LINK);
GetLink(const GetLink&) = delete;
GetLink operator=(const GetLink&) = delete;
virtual ValuePtr execute(AtomSpace*, bool silent=false);
static Handle factory(const Handle&);
};
LINK_PTR_DECL(GetLink)
#define createGetLink CREATE_DECL(GetLink)
}
#endif