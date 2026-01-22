#ifndef __EXCEPTION_H
#define __EXCEPTION_H 1
#pragma GCC system_header
#pragma GCC visibility push(default)
#include <bits/c++config.h>
extern "C++" {
namespace std
{
class exception
{
public:
exception() _GLIBCXX_NOTHROW { }
virtual ~exception() _GLIBCXX_TXN_SAFE_DYN _GLIBCXX_NOTHROW;
#if __cplusplus >= 201103L
exception(const exception&) = default;
exception& operator=(const exception&) = default;
exception(exception&&) = default;
exception& operator=(exception&&) = default;
#endif
virtual const char*
what() const _GLIBCXX_TXN_SAFE_DYN _GLIBCXX_NOTHROW;
};
}
}
#pragma GCC visibility pop
#endif