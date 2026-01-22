#ifndef _OPENCOG_RANDOM_STREAM_H
#define _OPENCOG_RANDOM_STREAM_H
#include <opencog/atoms/value/FloatValue.h>
namespace opencog
{
class RandomStream
: public FloatValue
{
protected:
RandomStream(Type t) : FloatValue(t) {}
int _len;
virtual void update() const;
public:
RandomStream(int=1);
virtual ~RandomStream() {}
virtual std::string to_string(const std::string& indent = "") const;
};
VALUE_PTR_DECL(RandomStream);
CREATE_VALUE_DECL(RandomStream);
}
#endif