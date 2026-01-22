#ifndef _GLIBCXX_CXX_ALLOCATOR_H
#define _GLIBCXX_CXX_ALLOCATOR_H 1
#include <ext/new_allocator.h>
#if __cplusplus >= 201103L
namespace std
{
template<typename _Tp>
using __allocator_base = __gnu_cxx::new_allocator<_Tp>;
}
#else
# define __allocator_base __gnu_cxx::new_allocator
#endif
#ifndef _GLIBCXX_SANITIZE_STD_ALLOCATOR
# if defined(__SANITIZE_ADDRESS__)
# define _GLIBCXX_SANITIZE_STD_ALLOCATOR 1
# elif defined __has_feature
# if __has_feature(address_sanitizer)
# define _GLIBCXX_SANITIZE_STD_ALLOCATOR 1
# endif
# endif
#endif
#endif