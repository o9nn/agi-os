#ifndef _OPENCOG_SECTION_VALUE_H
#define _OPENCOG_SECTION_VALUE_H
#include <opencog/atoms/value/LinkValue.h>
namespace opencog
{
class SectionValue
: public LinkValue
{
protected:
SectionValue(Type t) : LinkValue(t) {}
public:
SectionValue(const ValueSeq& vlist);
SectionValue(ValueSeq&& vlist);
virtual ~SectionValue() {}
};
VALUE_PTR_DECL(SectionValue);
CREATE_VALUE_DECL(SectionValue);
}
#endif