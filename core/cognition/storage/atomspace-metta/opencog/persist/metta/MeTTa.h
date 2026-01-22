#ifndef _METTA_ECODE_H
#define _METTA_ECODE_H
#include <string>
#include <opencog/atoms/base/Handle.h>
namespace opencog
{
class MeTTa
{
public:
static Handle next_expr(const std::string&,
size_t& l, size_t& r);
static std::string prt_metta(const Handle&);
};
}
#endif