#ifndef _OPENCOG_FUNCTION_LINK_H
#define _OPENCOG_FUNCTION_LINK_H
#include <opencog/atoms/core/FreeLink.h>
namespace opencog
{
class FunctionLink : public FreeLink
{
protected:
static void check_type(Type t);
void init(void);
public:
FunctionLink(const HandleSeq&&, Type = FUNCTION_LINK);
FunctionLink(const FunctionLink&) = delete;
FunctionLink& operator=(const FunctionLink&) = delete;
virtual ~FunctionLink() {}
virtual bool is_executable(void) const { return true; }
static ValuePtr get_value(AtomSpace*, bool, ValuePtr);
static Handle factory(const Handle&);
};
LINK_PTR_DECL(FunctionLink)
#define createFunctionLink CREATE_DECL(FunctionLink)
}
#endif