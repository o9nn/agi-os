#ifndef _MEMORYFWD_H
#define _MEMORYFWD_H 1
#pragma GCC system_header
#include <bits/c++config.h>
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
#if _GLIBCXX_HOSTED
template<typename>
class allocator;
template<>
class allocator<void>;
#endif
#if __cplusplus >= 201103L
template<typename, typename>
struct uses_allocator;
template<typename>
struct allocator_traits;
#endif
_GLIBCXX_END_NAMESPACE_VERSION
}
#endif