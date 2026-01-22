#ifndef _FUNCTIONAL_HASH_H
#define _FUNCTIONAL_HASH_H 1
#pragma GCC system_header
#include <type_traits>
#include <bits/hash_bytes.h>
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
template<typename _Result, typename _Arg>
struct __hash_base
{
typedef _Result     result_type _GLIBCXX17_DEPRECATED;
typedef _Arg      argument_type _GLIBCXX17_DEPRECATED;
};
template<typename _Tp>
struct hash;
template<typename _Tp, typename = void>
struct __poison_hash
{
static constexpr bool __enable_hash_call = false;
private:
__poison_hash(__poison_hash&&);
~__poison_hash();
};
template<typename _Tp>
struct __poison_hash<_Tp, __void_t<decltype(hash<_Tp>()(declval<_Tp>()))>>
{
static constexpr bool __enable_hash_call = true;
};
template<typename _Tp, bool = is_enum<_Tp>::value>
struct __hash_enum
{
private:
__hash_enum(__hash_enum&&);
~__hash_enum();
};
template<typename _Tp>
struct __hash_enum<_Tp, true> : public __hash_base<size_t, _Tp>
{
size_t
operator()(_Tp __val) const noexcept
{
using __type = typename underlying_type<_Tp>::type;
return hash<__type>{}(static_cast<__type>(__val));
}
};
template<typename _Tp>
struct hash : __hash_enum<_Tp>
{ };
template<typename _Tp>
struct hash<_Tp*> : public __hash_base<size_t, _Tp*>
{
size_t
operator()(_Tp* __p) const noexcept
{ return reinterpret_cast<size_t>(__p); }
};
#define _Cxx_hashtable_define_trivial_hash(_Tp) 	\
template<>						\
struct hash<_Tp> : public __hash_base<size_t, _Tp>  \
{                                                   \
size_t                                            \
operator()(_Tp __val) const noexcept              \
{ return static_cast<size_t>(__val); }            \
};
_Cxx_hashtable_define_trivial_hash(bool)
_Cxx_hashtable_define_trivial_hash(char)
_Cxx_hashtable_define_trivial_hash(signed char)
_Cxx_hashtable_define_trivial_hash(unsigned char)
_Cxx_hashtable_define_trivial_hash(wchar_t)
#ifdef _GLIBCXX_USE_CHAR8_T
_Cxx_hashtable_define_trivial_hash(char8_t)
#endif
_Cxx_hashtable_define_trivial_hash(char16_t)
_Cxx_hashtable_define_trivial_hash(char32_t)
_Cxx_hashtable_define_trivial_hash(short)
_Cxx_hashtable_define_trivial_hash(int)
_Cxx_hashtable_define_trivial_hash(long)
_Cxx_hashtable_define_trivial_hash(long long)
_Cxx_hashtable_define_trivial_hash(unsigned short)
_Cxx_hashtable_define_trivial_hash(unsigned int)
_Cxx_hashtable_define_trivial_hash(unsigned long)
_Cxx_hashtable_define_trivial_hash(unsigned long long)
#ifdef __GLIBCXX_TYPE_INT_N_0
__extension__
_Cxx_hashtable_define_trivial_hash(__GLIBCXX_TYPE_INT_N_0)
__extension__
_Cxx_hashtable_define_trivial_hash(__GLIBCXX_TYPE_INT_N_0 unsigned)
#endif
#ifdef __GLIBCXX_TYPE_INT_N_1
__extension__
_Cxx_hashtable_define_trivial_hash(__GLIBCXX_TYPE_INT_N_1)
__extension__
_Cxx_hashtable_define_trivial_hash(__GLIBCXX_TYPE_INT_N_1 unsigned)
#endif
#ifdef __GLIBCXX_TYPE_INT_N_2
__extension__
_Cxx_hashtable_define_trivial_hash(__GLIBCXX_TYPE_INT_N_2)
__extension__
_Cxx_hashtable_define_trivial_hash(__GLIBCXX_TYPE_INT_N_2 unsigned)
#endif
#ifdef __GLIBCXX_TYPE_INT_N_3
__extension__
_Cxx_hashtable_define_trivial_hash(__GLIBCXX_TYPE_INT_N_3)
__extension__
_Cxx_hashtable_define_trivial_hash(__GLIBCXX_TYPE_INT_N_3 unsigned)
#endif
#undef _Cxx_hashtable_define_trivial_hash
struct _Hash_impl
{
static size_t
hash(const void* __ptr, size_t __clength,
size_t __seed = static_cast<size_t>(0xc70f6907UL))
{ return _Hash_bytes(__ptr, __clength, __seed); }
template<typename _Tp>
static size_t
hash(const _Tp& __val)
{ return hash(&__val, sizeof(__val)); }
template<typename _Tp>
static size_t
__hash_combine(const _Tp& __val, size_t __hash)
{ return hash(&__val, sizeof(__val), __hash); }
};
struct _Fnv_hash_impl
{
static size_t
hash(const void* __ptr, size_t __clength,
size_t __seed = static_cast<size_t>(2166136261UL))
{ return _Fnv_hash_bytes(__ptr, __clength, __seed); }
template<typename _Tp>
static size_t
hash(const _Tp& __val)
{ return hash(&__val, sizeof(__val)); }
template<typename _Tp>
static size_t
__hash_combine(const _Tp& __val, size_t __hash)
{ return hash(&__val, sizeof(__val), __hash); }
};
template<>
struct hash<float> : public __hash_base<size_t, float>
{
size_t
operator()(float __val) const noexcept
{
return __val != 0.0f ? std::_Hash_impl::hash(__val) : 0;
}
};
template<>
struct hash<double> : public __hash_base<size_t, double>
{
size_t
operator()(double __val) const noexcept
{
return __val != 0.0 ? std::_Hash_impl::hash(__val) : 0;
}
};
template<>
struct hash<long double>
: public __hash_base<size_t, long double>
{
_GLIBCXX_PURE size_t
operator()(long double __val) const noexcept;
};
#if __cplusplus >= 201703L
template<>
struct hash<nullptr_t> : public __hash_base<size_t, nullptr_t>
{
size_t
operator()(nullptr_t) const noexcept
{ return 0; }
};
#endif
template<typename _Hash>
struct __is_fast_hash : public std::true_type
{ };
template<>
struct __is_fast_hash<hash<long double>> : public std::false_type
{ };
_GLIBCXX_END_NAMESPACE_VERSION
}
#endif