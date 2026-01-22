#ifndef _OPENCOG_FLOAT_COLUMN_H
#define _OPENCOG_FLOAT_COLUMN_H
#include <opencog/atoms/base/Link.h>
namespace opencog
{
class FloatColumn : public Link
{
protected:
ValuePtr do_execute(AtomSpace*, bool);
ValuePtr do_handle_loop(AtomSpace*, bool, const HandleSeq&);
public:
FloatColumn(const HandleSeq&&, Type = FLOAT_COLUMN);
FloatColumn(const FloatColumn&) = delete;
FloatColumn& operator=(const FloatColumn&) = delete;
virtual bool is_executable() const { return true; }
virtual ValuePtr execute(AtomSpace*, bool);
static Handle factory(const Handle&);
};
LINK_PTR_DECL(FloatColumn)
#define createFloatColumn CREATE_DECL(FloatColumn)
}
#endif