#ifndef _UNIQUE_PTR_H
#define _UNIQUE_PTR_H 1
#include <bits/c++config.h>
#include <debug/assertions.h>
#include <type_traits>
#include <tuple>
#include <bits/stl_function.h>
#include <bits/functional_hash.h>
#if __cplusplus >= 202002L
# include <compare>
# if _GLIBCXX_HOSTED
#  include <ostream>
# endif
#endif
#if __cplusplus > 202002L && defined(__cpp_constexpr_dynamic_alloc)
# define __cpp_lib_constexpr_memory 202202L
#elif __cplusplus > 201703L
# define __cpp_lib_constexpr_memory 201811L
#endif
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
#if _GLIBCXX_USE_DEPRECATED
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"
template<typename> class auto_ptr;
#pragma GCC diagnostic pop
#endif
template<typename _Tp>
struct default_delete
{
constexpr default_delete() noexcept = default;
template<typename _Up,
typename = _Require<is_convertible<_Up*, _Tp*>>>
_GLIBCXX23_CONSTEXPR
default_delete(const default_delete<_Up>&) noexcept { }
_GLIBCXX23_CONSTEXPR
void
operator()(_Tp* __ptr) const
{
static_assert(!is_void<_Tp>::value,
"can't delete pointer to incomplete type");
static_assert(sizeof(_Tp)>0,
"can't delete pointer to incomplete type");
delete __ptr;
}
};
template<typename _Tp>
struct default_delete<_Tp[]>
{
public:
constexpr default_delete() noexcept = default;
template<typename _Up,
typename = _Require<is_convertible<_Up(*)[], _Tp(*)[]>>>
_GLIBCXX23_CONSTEXPR
default_delete(const default_delete<_Up[]>&) noexcept { }
template<typename _Up>
_GLIBCXX23_CONSTEXPR
typename enable_if<is_convertible<_Up(*)[], _Tp(*)[]>::value>::type
operator()(_Up* __ptr) const
{
static_assert(sizeof(_Tp)>0,
"can't delete pointer to incomplete type");
delete [] __ptr;
}
};
template <typename _Tp, typename _Dp>
class __uniq_ptr_impl
{
template <typename _Up, typename _Ep, typename = void>
struct _Ptr
{
using type = _Up*;
};
template <typename _Up, typename _Ep>
struct
_Ptr<_Up, _Ep, __void_t<typename remove_reference<_Ep>::type::pointer>>
{
using type = typename remove_reference<_Ep>::type::pointer;
};
public:
using _DeleterConstraint = enable_if<
__and_<__not_<is_pointer<_Dp>>,
is_default_constructible<_Dp>>::value>;
using pointer = typename _Ptr<_Tp, _Dp>::type;
static_assert( !is_rvalue_reference<_Dp>::value,
"unique_ptr's deleter type must be a function object type"
" or an lvalue reference type" );
__uniq_ptr_impl() = default;
_GLIBCXX23_CONSTEXPR
__uniq_ptr_impl(pointer __p) : _M_t() { _M_ptr() = __p; }
template<typename _Del>
_GLIBCXX23_CONSTEXPR
__uniq_ptr_impl(pointer __p, _Del&& __d)
: _M_t(__p, std::forward<_Del>(__d)) { }
_GLIBCXX23_CONSTEXPR
__uniq_ptr_impl(__uniq_ptr_impl&& __u) noexcept
: _M_t(std::move(__u._M_t))
{ __u._M_ptr() = nullptr; }
_GLIBCXX23_CONSTEXPR
__uniq_ptr_impl& operator=(__uniq_ptr_impl&& __u) noexcept
{
reset(__u.release());
_M_deleter() = std::forward<_Dp>(__u._M_deleter());
return *this;
}
_GLIBCXX23_CONSTEXPR
pointer&   _M_ptr() noexcept { return std::get<0>(_M_t); }
_GLIBCXX23_CONSTEXPR
pointer    _M_ptr() const noexcept { return std::get<0>(_M_t); }
_GLIBCXX23_CONSTEXPR
_Dp&       _M_deleter() noexcept { return std::get<1>(_M_t); }
_GLIBCXX23_CONSTEXPR
const _Dp& _M_deleter() const noexcept { return std::get<1>(_M_t); }
_GLIBCXX23_CONSTEXPR
void reset(pointer __p) noexcept
{
const pointer __old_p = _M_ptr();
_M_ptr() = __p;
if (__old_p)
_M_deleter()(__old_p);
}
_GLIBCXX23_CONSTEXPR
pointer release() noexcept
{
pointer __p = _M_ptr();
_M_ptr() = nullptr;
return __p;
}
_GLIBCXX23_CONSTEXPR
void
swap(__uniq_ptr_impl& __rhs) noexcept
{
using std::swap;
swap(this->_M_ptr(), __rhs._M_ptr());
swap(this->_M_deleter(), __rhs._M_deleter());
}
private:
tuple<pointer, _Dp> _M_t;
};
template <typename _Tp, typename _Dp,
bool = is_move_constructible<_Dp>::value,
bool = is_move_assignable<_Dp>::value>
struct __uniq_ptr_data : __uniq_ptr_impl<_Tp, _Dp>
{
using __uniq_ptr_impl<_Tp, _Dp>::__uniq_ptr_impl;
__uniq_ptr_data(__uniq_ptr_data&&) = default;
__uniq_ptr_data& operator=(__uniq_ptr_data&&) = default;
};
template <typename _Tp, typename _Dp>
struct __uniq_ptr_data<_Tp, _Dp, true, false> : __uniq_ptr_impl<_Tp, _Dp>
{
using __uniq_ptr_impl<_Tp, _Dp>::__uniq_ptr_impl;
__uniq_ptr_data(__uniq_ptr_data&&) = default;
__uniq_ptr_data& operator=(__uniq_ptr_data&&) = delete;
};
template <typename _Tp, typename _Dp>
struct __uniq_ptr_data<_Tp, _Dp, false, true> : __uniq_ptr_impl<_Tp, _Dp>
{
using __uniq_ptr_impl<_Tp, _Dp>::__uniq_ptr_impl;
__uniq_ptr_data(__uniq_ptr_data&&) = delete;
__uniq_ptr_data& operator=(__uniq_ptr_data&&) = default;
};
template <typename _Tp, typename _Dp>
struct __uniq_ptr_data<_Tp, _Dp, false, false> : __uniq_ptr_impl<_Tp, _Dp>
{
using __uniq_ptr_impl<_Tp, _Dp>::__uniq_ptr_impl;
__uniq_ptr_data(__uniq_ptr_data&&) = delete;
__uniq_ptr_data& operator=(__uniq_ptr_data&&) = delete;
};
template <typename _Tp, typename _Dp = default_delete<_Tp>>
class unique_ptr
{
template <typename _Up>
using _DeleterConstraint =
typename __uniq_ptr_impl<_Tp, _Up>::_DeleterConstraint::type;
__uniq_ptr_data<_Tp, _Dp> _M_t;
public:
using pointer	  = typename __uniq_ptr_impl<_Tp, _Dp>::pointer;
using element_type  = _Tp;
using deleter_type  = _Dp;
private:
template<typename _Up, typename _Ep>
using __safe_conversion_up = __and_<
is_convertible<typename unique_ptr<_Up, _Ep>::pointer, pointer>,
__not_<is_array<_Up>>
>;
public:
template<typename _Del = _Dp, typename = _DeleterConstraint<_Del>>
constexpr unique_ptr() noexcept
: _M_t()
{ }
template<typename _Del = _Dp, typename = _DeleterConstraint<_Del>>
_GLIBCXX23_CONSTEXPR
explicit
unique_ptr(pointer __p) noexcept
: _M_t(__p)
{ }
template<typename _Del = deleter_type,
typename = _Require<is_copy_constructible<_Del>>>
_GLIBCXX23_CONSTEXPR
unique_ptr(pointer __p, const deleter_type& __d) noexcept
: _M_t(__p, __d) { }
template<typename _Del = deleter_type,
typename = _Require<is_move_constructible<_Del>>>
_GLIBCXX23_CONSTEXPR
unique_ptr(pointer __p,
__enable_if_t<!is_lvalue_reference<_Del>::value,
_Del&&> __d) noexcept
: _M_t(__p, std::move(__d))
{ }
template<typename _Del = deleter_type,
typename _DelUnref = typename remove_reference<_Del>::type>
_GLIBCXX23_CONSTEXPR
unique_ptr(pointer,
__enable_if_t<is_lvalue_reference<_Del>::value,
_DelUnref&&>) = delete;
template<typename _Del = _Dp, typename = _DeleterConstraint<_Del>>
constexpr unique_ptr(nullptr_t) noexcept
: _M_t()
{ }
unique_ptr(unique_ptr&&) = default;
template<typename _Up, typename _Ep, typename = _Require<
__safe_conversion_up<_Up, _Ep>,
__conditional_t<is_reference<_Dp>::value,
is_same<_Ep, _Dp>,
is_convertible<_Ep, _Dp>>>>
_GLIBCXX23_CONSTEXPR
unique_ptr(unique_ptr<_Up, _Ep>&& __u) noexcept
: _M_t(__u.release(), std::forward<_Ep>(__u.get_deleter()))
{ }
#if _GLIBCXX_USE_DEPRECATED
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"
template<typename _Up, typename = _Require<
is_convertible<_Up*, _Tp*>, is_same<_Dp, default_delete<_Tp>>>>
unique_ptr(auto_ptr<_Up>&& __u) noexcept;
#pragma GCC diagnostic pop
#endif
#if __cplusplus > 202002L && __cpp_constexpr_dynamic_alloc
constexpr
#endif
~unique_ptr() noexcept
{
static_assert(__is_invocable<deleter_type&, pointer>::value,
"unique_ptr's deleter must be invocable with a pointer");
auto& __ptr = _M_t._M_ptr();
if (__ptr != nullptr)
get_deleter()(std::move(__ptr));
__ptr = pointer();
}
unique_ptr& operator=(unique_ptr&&) = default;
template<typename _Up, typename _Ep>
_GLIBCXX23_CONSTEXPR
typename enable_if< __and_<
__safe_conversion_up<_Up, _Ep>,
is_assignable<deleter_type&, _Ep&&>
>::value,
unique_ptr&>::type
operator=(unique_ptr<_Up, _Ep>&& __u) noexcept
{
reset(__u.release());
get_deleter() = std::forward<_Ep>(__u.get_deleter());
return *this;
}
_GLIBCXX23_CONSTEXPR
unique_ptr&
operator=(nullptr_t) noexcept
{
reset();
return *this;
}
_GLIBCXX23_CONSTEXPR
typename add_lvalue_reference<element_type>::type
operator*() const noexcept(noexcept(*std::declval<pointer>()))
{
__glibcxx_assert(get() != pointer());
return *get();
}
_GLIBCXX23_CONSTEXPR
pointer
operator->() const noexcept
{
_GLIBCXX_DEBUG_PEDASSERT(get() != pointer());
return get();
}
_GLIBCXX23_CONSTEXPR
pointer
get() const noexcept
{ return _M_t._M_ptr(); }
_GLIBCXX23_CONSTEXPR
deleter_type&
get_deleter() noexcept
{ return _M_t._M_deleter(); }
_GLIBCXX23_CONSTEXPR
const deleter_type&
get_deleter() const noexcept
{ return _M_t._M_deleter(); }
_GLIBCXX23_CONSTEXPR
explicit operator bool() const noexcept
{ return get() == pointer() ? false : true; }
_GLIBCXX23_CONSTEXPR
pointer
release() noexcept
{ return _M_t.release(); }
_GLIBCXX23_CONSTEXPR
void
reset(pointer __p = pointer()) noexcept
{
static_assert(__is_invocable<deleter_type&, pointer>::value,
"unique_ptr's deleter must be invocable with a pointer");
_M_t.reset(std::move(__p));
}
_GLIBCXX23_CONSTEXPR
void
swap(unique_ptr& __u) noexcept
{
static_assert(__is_swappable<_Dp>::value, "deleter must be swappable");
_M_t.swap(__u._M_t);
}
unique_ptr(const unique_ptr&) = delete;
unique_ptr& operator=(const unique_ptr&) = delete;
};
template<typename _Tp, typename _Dp>
class unique_ptr<_Tp[], _Dp>
{
template <typename _Up>
using _DeleterConstraint =
typename __uniq_ptr_impl<_Tp, _Up>::_DeleterConstraint::type;
__uniq_ptr_data<_Tp, _Dp> _M_t;
template<typename _Up>
using __is_derived_Tp
= __and_< is_base_of<_Tp, _Up>,
__not_<is_same<__remove_cv_t<_Tp>, __remove_cv_t<_Up>>> >;
public:
using pointer	  = typename __uniq_ptr_impl<_Tp, _Dp>::pointer;
using element_type  = _Tp;
using deleter_type  = _Dp;
template<typename _Up, typename _Ep,
typename _UPtr = unique_ptr<_Up, _Ep>,
typename _UP_pointer = typename _UPtr::pointer,
typename _UP_element_type = typename _UPtr::element_type>
using __safe_conversion_up = __and_<
is_array<_Up>,
is_same<pointer, element_type*>,
is_same<_UP_pointer, _UP_element_type*>,
is_convertible<_UP_element_type(*)[], element_type(*)[]>
>;
template<typename _Up>
using __safe_conversion_raw = __and_<
__or_<__or_<is_same<_Up, pointer>,
is_same<_Up, nullptr_t>>,
__and_<is_pointer<_Up>,
is_same<pointer, element_type*>,
is_convertible<
typename remove_pointer<_Up>::type(*)[],
element_type(*)[]>
>
>
>;
template<typename _Del = _Dp, typename = _DeleterConstraint<_Del>>
constexpr unique_ptr() noexcept
: _M_t()
{ }
template<typename _Up,
typename _Vp = _Dp,
typename = _DeleterConstraint<_Vp>,
typename = typename enable_if<
__safe_conversion_raw<_Up>::value, bool>::type>
_GLIBCXX23_CONSTEXPR
explicit
unique_ptr(_Up __p) noexcept
: _M_t(__p)
{ }
template<typename _Up, typename _Del = deleter_type,
typename = _Require<__safe_conversion_raw<_Up>,
is_copy_constructible<_Del>>>
_GLIBCXX23_CONSTEXPR
unique_ptr(_Up __p, const deleter_type& __d) noexcept
: _M_t(__p, __d) { }
template<typename _Up, typename _Del = deleter_type,
typename = _Require<__safe_conversion_raw<_Up>,
is_move_constructible<_Del>>>
_GLIBCXX23_CONSTEXPR
unique_ptr(_Up __p,
__enable_if_t<!is_lvalue_reference<_Del>::value,
_Del&&> __d) noexcept
: _M_t(std::move(__p), std::move(__d))
{ }
template<typename _Up, typename _Del = deleter_type,
typename _DelUnref = typename remove_reference<_Del>::type,
typename = _Require<__safe_conversion_raw<_Up>>>
unique_ptr(_Up,
__enable_if_t<is_lvalue_reference<_Del>::value,
_DelUnref&&>) = delete;
unique_ptr(unique_ptr&&) = default;
template<typename _Del = _Dp, typename = _DeleterConstraint<_Del>>
constexpr unique_ptr(nullptr_t) noexcept
: _M_t()
{ }
template<typename _Up, typename _Ep, typename = _Require<
__safe_conversion_up<_Up, _Ep>,
__conditional_t<is_reference<_Dp>::value,
is_same<_Ep, _Dp>,
is_convertible<_Ep, _Dp>>>>
_GLIBCXX23_CONSTEXPR
unique_ptr(unique_ptr<_Up, _Ep>&& __u) noexcept
: _M_t(__u.release(), std::forward<_Ep>(__u.get_deleter()))
{ }
#if __cplusplus > 202002L && __cpp_constexpr_dynamic_alloc
constexpr
#endif
~unique_ptr()
{
auto& __ptr = _M_t._M_ptr();
if (__ptr != nullptr)
get_deleter()(__ptr);
__ptr = pointer();
}
unique_ptr&
operator=(unique_ptr&&) = default;
template<typename _Up, typename _Ep>
_GLIBCXX23_CONSTEXPR
typename
enable_if<__and_<__safe_conversion_up<_Up, _Ep>,
is_assignable<deleter_type&, _Ep&&>
>::value,
unique_ptr&>::type
operator=(unique_ptr<_Up, _Ep>&& __u) noexcept
{
reset(__u.release());
get_deleter() = std::forward<_Ep>(__u.get_deleter());
return *this;
}
_GLIBCXX23_CONSTEXPR
unique_ptr&
operator=(nullptr_t) noexcept
{
reset();
return *this;
}
_GLIBCXX23_CONSTEXPR
typename std::add_lvalue_reference<element_type>::type
operator[](size_t __i) const
{
__glibcxx_assert(get() != pointer());
return get()[__i];
}
_GLIBCXX23_CONSTEXPR
pointer
get() const noexcept
{ return _M_t._M_ptr(); }
_GLIBCXX23_CONSTEXPR
deleter_type&
get_deleter() noexcept
{ return _M_t._M_deleter(); }
_GLIBCXX23_CONSTEXPR
const deleter_type&
get_deleter() const noexcept
{ return _M_t._M_deleter(); }
_GLIBCXX23_CONSTEXPR
explicit operator bool() const noexcept
{ return get() == pointer() ? false : true; }
_GLIBCXX23_CONSTEXPR
pointer
release() noexcept
{ return _M_t.release(); }
template <typename _Up,
typename = _Require<
__or_<is_same<_Up, pointer>,
__and_<is_same<pointer, element_type*>,
is_pointer<_Up>,
is_convertible<
typename remove_pointer<_Up>::type(*)[],
element_type(*)[]
>
>
>
>>
_GLIBCXX23_CONSTEXPR
void
reset(_Up __p) noexcept
{ _M_t.reset(std::move(__p)); }
_GLIBCXX23_CONSTEXPR
void reset(nullptr_t = nullptr) noexcept
{ reset(pointer()); }
_GLIBCXX23_CONSTEXPR
void
swap(unique_ptr& __u) noexcept
{
static_assert(__is_swappable<_Dp>::value, "deleter must be swappable");
_M_t.swap(__u._M_t);
}
unique_ptr(const unique_ptr&) = delete;
unique_ptr& operator=(const unique_ptr&) = delete;
};
template<typename _Tp, typename _Dp>
inline
#if __cplusplus > 201402L || !defined(__STRICT_ANSI__)
_GLIBCXX23_CONSTEXPR
typename enable_if<__is_swappable<_Dp>::value>::type
#else
void
#endif
swap(unique_ptr<_Tp, _Dp>& __x,
unique_ptr<_Tp, _Dp>& __y) noexcept
{ __x.swap(__y); }
#if __cplusplus > 201402L || !defined(__STRICT_ANSI__)
template<typename _Tp, typename _Dp>
typename enable_if<!__is_swappable<_Dp>::value>::type
swap(unique_ptr<_Tp, _Dp>&,
unique_ptr<_Tp, _Dp>&) = delete;
#endif
template<typename _Tp, typename _Dp,
typename _Up, typename _Ep>
_GLIBCXX_NODISCARD _GLIBCXX23_CONSTEXPR
inline bool
operator==(const unique_ptr<_Tp, _Dp>& __x,
const unique_ptr<_Up, _Ep>& __y)
{ return __x.get() == __y.get(); }
template<typename _Tp, typename _Dp>
_GLIBCXX_NODISCARD _GLIBCXX23_CONSTEXPR
inline bool
operator==(const unique_ptr<_Tp, _Dp>& __x, nullptr_t) noexcept
{ return !__x; }
#ifndef __cpp_lib_three_way_comparison
template<typename _Tp, typename _Dp>
_GLIBCXX_NODISCARD
inline bool
operator==(nullptr_t, const unique_ptr<_Tp, _Dp>& __x) noexcept
{ return !__x; }
template<typename _Tp, typename _Dp,
typename _Up, typename _Ep>
_GLIBCXX_NODISCARD
inline bool
operator!=(const unique_ptr<_Tp, _Dp>& __x,
const unique_ptr<_Up, _Ep>& __y)
{ return __x.get() != __y.get(); }
template<typename _Tp, typename _Dp>
_GLIBCXX_NODISCARD
inline bool
operator!=(const unique_ptr<_Tp, _Dp>& __x, nullptr_t) noexcept
{ return (bool)__x; }
template<typename _Tp, typename _Dp>
_GLIBCXX_NODISCARD
inline bool
operator!=(nullptr_t, const unique_ptr<_Tp, _Dp>& __x) noexcept
{ return (bool)__x; }
#endif
template<typename _Tp, typename _Dp,
typename _Up, typename _Ep>
_GLIBCXX_NODISCARD _GLIBCXX23_CONSTEXPR
inline bool
operator<(const unique_ptr<_Tp, _Dp>& __x,
const unique_ptr<_Up, _Ep>& __y)
{
typedef typename
std::common_type<typename unique_ptr<_Tp, _Dp>::pointer,
typename unique_ptr<_Up, _Ep>::pointer>::type _CT;
return std::less<_CT>()(__x.get(), __y.get());
}
template<typename _Tp, typename _Dp>
_GLIBCXX_NODISCARD _GLIBCXX23_CONSTEXPR
inline bool
operator<(const unique_ptr<_Tp, _Dp>& __x, nullptr_t)
{
return std::less<typename unique_ptr<_Tp, _Dp>::pointer>()(__x.get(),
nullptr);
}
template<typename _Tp, typename _Dp>
_GLIBCXX_NODISCARD _GLIBCXX23_CONSTEXPR
inline bool
operator<(nullptr_t, const unique_ptr<_Tp, _Dp>& __x)
{
return std::less<typename unique_ptr<_Tp, _Dp>::pointer>()(nullptr,
__x.get());
}
template<typename _Tp, typename _Dp,
typename _Up, typename _Ep>
_GLIBCXX_NODISCARD _GLIBCXX23_CONSTEXPR
inline bool
operator<=(const unique_ptr<_Tp, _Dp>& __x,
const unique_ptr<_Up, _Ep>& __y)
{ return !(__y < __x); }
template<typename _Tp, typename _Dp>
_GLIBCXX_NODISCARD _GLIBCXX23_CONSTEXPR
inline bool
operator<=(const unique_ptr<_Tp, _Dp>& __x, nullptr_t)
{ return !(nullptr < __x); }
template<typename _Tp, typename _Dp>
_GLIBCXX_NODISCARD _GLIBCXX23_CONSTEXPR
inline bool
operator<=(nullptr_t, const unique_ptr<_Tp, _Dp>& __x)
{ return !(__x < nullptr); }
template<typename _Tp, typename _Dp,
typename _Up, typename _Ep>
_GLIBCXX_NODISCARD _GLIBCXX23_CONSTEXPR
inline bool
operator>(const unique_ptr<_Tp, _Dp>& __x,
const unique_ptr<_Up, _Ep>& __y)
{ return (__y < __x); }
template<typename _Tp, typename _Dp>
_GLIBCXX_NODISCARD _GLIBCXX23_CONSTEXPR
inline bool
operator>(const unique_ptr<_Tp, _Dp>& __x, nullptr_t)
{
return std::less<typename unique_ptr<_Tp, _Dp>::pointer>()(nullptr,
__x.get());
}
template<typename _Tp, typename _Dp>
_GLIBCXX_NODISCARD _GLIBCXX23_CONSTEXPR
inline bool
operator>(nullptr_t, const unique_ptr<_Tp, _Dp>& __x)
{
return std::less<typename unique_ptr<_Tp, _Dp>::pointer>()(__x.get(),
nullptr);
}
template<typename _Tp, typename _Dp,
typename _Up, typename _Ep>
_GLIBCXX_NODISCARD _GLIBCXX23_CONSTEXPR
inline bool
operator>=(const unique_ptr<_Tp, _Dp>& __x,
const unique_ptr<_Up, _Ep>& __y)
{ return !(__x < __y); }
template<typename _Tp, typename _Dp>
_GLIBCXX_NODISCARD _GLIBCXX23_CONSTEXPR
inline bool
operator>=(const unique_ptr<_Tp, _Dp>& __x, nullptr_t)
{ return !(__x < nullptr); }
template<typename _Tp, typename _Dp>
_GLIBCXX_NODISCARD inline bool
operator>=(nullptr_t, const unique_ptr<_Tp, _Dp>& __x)
{ return !(nullptr < __x); }
#ifdef __cpp_lib_three_way_comparison
template<typename _Tp, typename _Dp, typename _Up, typename _Ep>
requires three_way_comparable_with<typename unique_ptr<_Tp, _Dp>::pointer,
typename unique_ptr<_Up, _Ep>::pointer>
_GLIBCXX23_CONSTEXPR
inline
compare_three_way_result_t<typename unique_ptr<_Tp, _Dp>::pointer,
typename unique_ptr<_Up, _Ep>::pointer>
operator<=>(const unique_ptr<_Tp, _Dp>& __x,
const unique_ptr<_Up, _Ep>& __y)
{ return compare_three_way()(__x.get(), __y.get()); }
template<typename _Tp, typename _Dp>
requires three_way_comparable<typename unique_ptr<_Tp, _Dp>::pointer>
_GLIBCXX23_CONSTEXPR
inline
compare_three_way_result_t<typename unique_ptr<_Tp, _Dp>::pointer>
operator<=>(const unique_ptr<_Tp, _Dp>& __x, nullptr_t)
{
using pointer = typename unique_ptr<_Tp, _Dp>::pointer;
return compare_three_way()(__x.get(), static_cast<pointer>(nullptr));
}
#endif
template<typename _Up, typename _Ptr = typename _Up::pointer,
bool = __poison_hash<_Ptr>::__enable_hash_call>
struct __uniq_ptr_hash
#if ! _GLIBCXX_INLINE_VERSION
: private __poison_hash<_Ptr>
#endif
{
size_t
operator()(const _Up& __u) const
noexcept(noexcept(std::declval<hash<_Ptr>>()(std::declval<_Ptr>())))
{ return hash<_Ptr>()(__u.get()); }
};
template<typename _Up, typename _Ptr>
struct __uniq_ptr_hash<_Up, _Ptr, false>
: private __poison_hash<_Ptr>
{ };
template<typename _Tp, typename _Dp>
struct hash<unique_ptr<_Tp, _Dp>>
: public __hash_base<size_t, unique_ptr<_Tp, _Dp>>,
public __uniq_ptr_hash<unique_ptr<_Tp, _Dp>>
{ };
#if __cplusplus >= 201402L && _GLIBCXX_HOSTED
#define __cpp_lib_make_unique 201304L
namespace __detail
{
template<typename _Tp>
struct _MakeUniq
{ typedef unique_ptr<_Tp> __single_object; };
template<typename _Tp>
struct _MakeUniq<_Tp[]>
{ typedef unique_ptr<_Tp[]> __array; };
template<typename _Tp, size_t _Bound>
struct _MakeUniq<_Tp[_Bound]>
{ struct __invalid_type { }; };
template<typename _Tp>
using __unique_ptr_t = typename _MakeUniq<_Tp>::__single_object;
template<typename _Tp>
using __unique_ptr_array_t = typename _MakeUniq<_Tp>::__array;
template<typename _Tp>
using __invalid_make_unique_t = typename _MakeUniq<_Tp>::__invalid_type;
}
template<typename _Tp, typename... _Args>
_GLIBCXX23_CONSTEXPR
inline __detail::__unique_ptr_t<_Tp>
make_unique(_Args&&... __args)
{ return unique_ptr<_Tp>(new _Tp(std::forward<_Args>(__args)...)); }
template<typename _Tp>
_GLIBCXX23_CONSTEXPR
inline __detail::__unique_ptr_array_t<_Tp>
make_unique(size_t __num)
{ return unique_ptr<_Tp>(new remove_extent_t<_Tp>[__num]()); }
template<typename _Tp, typename... _Args>
__detail::__invalid_make_unique_t<_Tp>
make_unique(_Args&&...) = delete;
#if __cplusplus > 201703L
template<typename _Tp>
_GLIBCXX23_CONSTEXPR
inline __detail::__unique_ptr_t<_Tp>
make_unique_for_overwrite()
{ return unique_ptr<_Tp>(new _Tp); }
template<typename _Tp>
_GLIBCXX23_CONSTEXPR
inline __detail::__unique_ptr_array_t<_Tp>
make_unique_for_overwrite(size_t __num)
{ return unique_ptr<_Tp>(new remove_extent_t<_Tp>[__num]); }
template<typename _Tp, typename... _Args>
__detail::__invalid_make_unique_t<_Tp>
make_unique_for_overwrite(_Args&&...) = delete;
#endif
#endif
#if __cplusplus > 201703L && __cpp_concepts && _GLIBCXX_HOSTED
template<typename _CharT, typename _Traits, typename _Tp, typename _Dp>
inline basic_ostream<_CharT, _Traits>&
operator<<(basic_ostream<_CharT, _Traits>& __os,
const unique_ptr<_Tp, _Dp>& __p)
requires requires { __os << __p.get(); }
{
__os << __p.get();
return __os;
}
#endif
#if __cplusplus >= 201703L
namespace __detail::__variant
{
template<typename> struct _Never_valueless_alt;
template<typename _Tp, typename _Del>
struct _Never_valueless_alt<std::unique_ptr<_Tp, _Del>>
: std::true_type
{ };
}
#endif
_GLIBCXX_END_NAMESPACE_VERSION
}
#endif