#ifndef _GLIBCXX_CXX_IO_H
#define _GLIBCXX_CXX_IO_H 1
#include <cstdio>
#include <bits/gthr.h>
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
#ifdef __GTHREAD_LEGACY_MUTEX_T
typedef __GTHREAD_LEGACY_MUTEX_T __c_lock;
#else
typedef __gthread_mutex_t __c_lock;
#endif
typedef FILE __c_file;
_GLIBCXX_END_NAMESPACE_VERSION
}
#endif