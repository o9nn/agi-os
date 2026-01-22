#ifndef _HASH_BYTES_H
#define _HASH_BYTES_H 1
#pragma GCC system_header
#include <bits/c++config.h>
namespace std
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
size_t
_Hash_bytes(const void* __ptr, size_t __len, size_t __seed);
size_t
_Fnv_hash_bytes(const void* __ptr, size_t __len, size_t __seed);
_GLIBCXX_END_NAMESPACE_VERSION
}
#endif