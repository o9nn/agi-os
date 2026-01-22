#ifndef _OPENCOG_REQUEST_CLASS_INFO_H
#define _OPENCOG_REQUEST_CLASS_INFO_H
#include <string>
namespace opencog
{
struct RequestClassInfo : public ClassInfo
{
std::string description;
std::string help;
bool is_shell;
bool hidden;
RequestClassInfo() : is_shell(false), hidden(false) {};
RequestClassInfo(const char* i, const char *d, const char* h,
bool s = false, bool hide = false)
: ClassInfo(i), description(d), help(h), is_shell(s), hidden(hide) {};
RequestClassInfo(const std::string& i,
const std::string& d,
const std::string& h,
bool s = false,
bool hide = false)
: ClassInfo(i), description(d), help(h), is_shell(s), hidden(hide) {};
};
}
#endif