#ifndef _PROLOG_ECODE_H
#define _PROLOG_ECODE_H
#include <string>
#include <opencog/atoms/base/Handle.h>
namespace opencog
{
class Prolog
{
public:
static HandleSeq parse(const std::string&,
size_t& l, size_t& r);
static Handle get_next_expr(const std::string&,
size_t& l, size_t& r);
static std::string prt_datalog(const Handle&, bool=false);
};
}
#endif