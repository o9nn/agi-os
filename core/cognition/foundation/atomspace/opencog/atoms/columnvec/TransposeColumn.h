#ifndef _OPENCOG_TRANSPOSE_COLUMN_H
#define _OPENCOG_TRANSPOSE_COLUMN_H
#include <opencog/atoms/base/Link.h>
namespace opencog
{
class TransposeColumn : public Link
{
protected:
ValuePtr do_execute(AtomSpace*, bool);
ValuePtr do_handle_loop(AtomSpace*, bool, const HandleSeq&);
ValuePtr do_value_loop(AtomSpace*, bool, const ValueSeq&);
ValuePtr do_direct_loop(AtomSpace*, bool, const ValueSeq&);
public:
TransposeColumn(const HandleSeq&&, Type = TRANSPOSE_COLUMN);
TransposeColumn(const TransposeColumn&) = delete;
TransposeColumn& operator=(const TransposeColumn&) = delete;
virtual bool is_executable() const { return true; }
virtual ValuePtr execute(AtomSpace*, bool);
static Handle factory(const Handle&);
};
LINK_PTR_DECL(TransposeColumn)
#define createTransposeColumn CREATE_DECL(TransposeColumn)
}
#endif