#ifndef _OPENCOG_MISC_H
#define _OPENCOG_MISC_H
#include <iterator>
#include <functional>
#include <string>
#ifndef WIN32
#include <cxxabi.h>
#endif
namespace opencog
{
unsigned int bitcount(unsigned long n);
template <typename _OutputIterator>
void tokenize(const std::string& str,
_OutputIterator tokens,
const std::string& delimiters = " ")
{
std::string::size_type lastPos = str.find_first_not_of(delimiters, 0);
std::string::size_type pos = str.find_first_of(delimiters, lastPos);
while (std::string::npos != pos || std::string::npos != lastPos) {
*(tokens++) = str.substr(lastPos, pos - lastPos);
lastPos = str.find_first_not_of(delimiters, pos);
pos = str.find_first_of(delimiters, lastPos);
}
}
template<typename _T>
struct safe_deleter
{
void operator()(_T*& __ptr) {
if (__ptr) {
delete __ptr;
__ptr = 0;
}
}
};
#ifndef WIN32
std::string demangle(const std::string& mangled);
#endif
}
#endif