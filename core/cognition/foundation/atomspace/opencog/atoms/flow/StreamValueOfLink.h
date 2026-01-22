#ifndef _OPENCOG_STREAM_VALUE_OF_LINK_H
#define _OPENCOG_STREAM_VALUE_OF_LINK_H
#include <opencog/atoms/flow/ValueOfLink.h>
namespace opencog
{
class StreamValueOfLink : public ValueOfLink
{
public:
StreamValueOfLink(const HandleSeq&&, Type=STREAM_VALUE_OF_LINK);
StreamValueOfLink(const StreamValueOfLink&) = delete;
StreamValueOfLink& operator=(const StreamValueOfLink&) = delete;
virtual ValuePtr execute(AtomSpace*, bool);
static Handle factory(const Handle&);
};
LINK_PTR_DECL(StreamValueOfLink)
#define createStreamValueOfLink CREATE_DECL(StreamValueOfLink)
}
#endif