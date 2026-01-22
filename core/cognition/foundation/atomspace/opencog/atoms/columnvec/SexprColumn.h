#ifndef _OPENCOG_SEXPR_COLUMN_H
#define _OPENCOG_SEXPR_COLUMN_H
#include <opencog/atoms/base/Link.h>
namespace opencog
{
class SexprColumn : public Link
{
protected:
ValuePtr do_execute(AtomSpace*, bool);
public:
SexprColumn(const HandleSeq&&, Type = SEXPR_COLUMN);
SexprColumn(const SexprColumn&) = delete;
SexprColumn& operator=(const SexprColumn&) = delete;
virtual bool is_executable() const { return true; }
virtual ValuePtr execute(AtomSpace*, bool);
static Handle factory(const Handle&);
};
LINK_PTR_DECL(SexprColumn)
#define createSexprColumn CREATE_DECL(SexprColumn)
}
#endif