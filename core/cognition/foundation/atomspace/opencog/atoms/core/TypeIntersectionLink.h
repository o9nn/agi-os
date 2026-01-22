#ifndef _OPENCOG_TYPE_INTERSECTION_H
#define _OPENCOG_TYPE_INTERSECTION_H
#include <opencog/atoms/core/TypeChoice.h>
namespace opencog
{
class TypeIntersectionLink : public TypeChoice
{
protected:
void init(bool);
void analyze(Handle, bool&);
public:
TypeIntersectionLink(const HandleSeq&&, Type=TYPE_INTERSECTION_LINK, bool=false);
TypeIntersectionLink(const TypeIntersectionLink&) = delete;
TypeIntersectionLink& operator=(const TypeIntersectionLink&) = delete;
static Handle factory(const Handle&);
};
LINK_PTR_DECL(TypeIntersectionLink)
#define createTypeIntersectionLink CREATE_DECL(TypeIntersectionLink)
}
#endif