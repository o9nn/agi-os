#ifndef _OPENCOG_FUTURE_STREAM_H
#define _OPENCOG_FUTURE_STREAM_H
#include <opencog/atoms/base/Handle.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/value/LinkValue.h>
namespace opencog
{
class FutureStream
: public LinkValue
{
protected:
FutureStream(Type t) : LinkValue(t) {}
void init(void);
virtual void update() const;
HandleSeq _formula;
AtomSpace* _as;
public:
FutureStream(const Handle&);
FutureStream(const HandleSeq&&);
FutureStream(const ValueSeq&);
virtual ~FutureStream() {}
virtual std::string to_string(const std::string& indent = "") const;
virtual bool operator==(const Value&) const;
};
VALUE_PTR_DECL(FutureStream);
CREATE_VALUE_DECL(FutureStream);
}
#endif