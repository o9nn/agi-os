#ifndef _SHARED_PTR_H
#define _SHARED_PTR_H 1
#include <iosfwd>
#include <bits/shared_ptr_base.h>
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
template<typename _Ch, typename _Tr, typename _Tp, _Lock_policy _Lp>
inline std::basic_ostream<_Ch, _Tr>&
operator<<(std::basic_ostream<_Ch, _Tr>& __os,
const __shared_ptr<_Tp, _Lp>& __p)
{
__os << __p.get();
return __os;
}
template<typename _Del, typename _Tp, _Lock_policy _Lp>
inline _Del*
get_deleter(const __shared_ptr<_Tp, _Lp>& __p) noexcept
{
#if __cpp_rtti
return static_cast<_Del*>(__p._M_get_deleter(typeid(_Del)));
#else
return 0;
#endif
}
template<typename _Del, typename _Tp>
inline _Del*
get_deleter(const shared_ptr<_Tp>& __p) noexcept
{
#if __cpp_rtti
return static_cast<_Del*>(__p._M_get_deleter(typeid(_Del)));
#else
return 0;
#endif
}
#if __cpp_concepts && __cpp_lib_type_trait_variable_templates
template<typename _Tp>
requires (!is_array_v<_Tp>)
using _NonArray = _Tp;
#else
template<typename _Tp>
using _NonArray = __enable_if_t<!is_array<_Tp>::value, _Tp>;
#endif
#if __cpp_lib_shared_ptr_arrays >= 201707L
#if __cpp_concepts
template<typename _Tp>
requires is_array_v<_Tp> && (extent_v<_Tp> == 0)
using _UnboundedArray = _Tp;
#else
template<typename _Tp>
using _UnboundedArray
= __enable_if_t<__is_array_unknown_bounds<_Tp>::value, _Tp>;
#endif
#if __cpp_concepts
template<typename _Tp>
requires (extent_v<_Tp> != 0)
using _BoundedArray = _Tp;
#else
template<typename _Tp>
using _BoundedArray
= __enable_if_t<__is_array_known_bounds<_Tp>::value, _Tp>;
#endif
#if __cpp_lib_smart_ptr_for_overwrite
#if __cpp_concepts
template<typename _Tp>
requires (!is_array_v<_Tp>) || (extent_v<_Tp> != 0)
using _NotUnboundedArray = _Tp;
#else
template<typename _Tp>
using _NotUnboundedArray
= __enable_if_t<!__is_array_unknown_bounds<_Tp>::value, _Tp>;
#endif
#endif
#endif
template<typename _Tp>
class shared_ptr : public __shared_ptr<_Tp>
{
template<typename... _Args>
using _Constructible = typename enable_if<
is_constructible<__shared_ptr<_Tp>, _Args...>::value
>::type;
template<typename _Arg>
using _Assignable = typename enable_if<
is_assignable<__shared_ptr<_Tp>&, _Arg>::value, shared_ptr&
>::type;
public:
using element_type = typename __shared_ptr<_Tp>::element_type;
#if __cplusplus >= 201703L
# define __cpp_lib_shared_ptr_weak_type 201606L
using weak_type = weak_ptr<_Tp>;
#endif
constexpr shared_ptr() noexcept : __shared_ptr<_Tp>() { }
shared_ptr(const shared_ptr&) noexcept = default;
template<typename _Yp, typename = _Constructible<_Yp*>>
explicit
shared_ptr(_Yp* __p) : __shared_ptr<_Tp>(__p) { }
template<typename _Yp, typename _Deleter,
typename = _Constructible<_Yp*, _Deleter>>
shared_ptr(_Yp* __p, _Deleter __d)
: __shared_ptr<_Tp>(__p, std::move(__d)) { }
template<typename _Deleter>
shared_ptr(nullptr_t __p, _Deleter __d)
: __shared_ptr<_Tp>(__p, std::move(__d)) { }
template<typename _Yp, typename _Deleter, typename _Alloc,
typename = _Constructible<_Yp*, _Deleter, _Alloc>>
shared_ptr(_Yp* __p, _Deleter __d, _Alloc __a)
: __shared_ptr<_Tp>(__p, std::move(__d), std::move(__a)) { }
template<typename _Deleter, typename _Alloc>
shared_ptr(nullptr_t __p, _Deleter __d, _Alloc __a)
: __shared_ptr<_Tp>(__p, std::move(__d), std::move(__a)) { }
template<typename _Yp>
shared_ptr(const shared_ptr<_Yp>& __r, element_type* __p) noexcept
: __shared_ptr<_Tp>(__r, __p) { }
#if __cplusplus > 201703L
template<typename _Yp>
shared_ptr(shared_ptr<_Yp>&& __r, element_type* __p) noexcept
: __shared_ptr<_Tp>(std::move(__r), __p) { }
#endif
template<typename _Yp,
typename = _Constructible<const shared_ptr<_Yp>&>>
shared_ptr(const shared_ptr<_Yp>& __r) noexcept
: __shared_ptr<_Tp>(__r) { }
shared_ptr(shared_ptr&& __r) noexcept
: __shared_ptr<_Tp>(std::move(__r)) { }
template<typename _Yp, typename = _Constructible<shared_ptr<_Yp>>>
shared_ptr(shared_ptr<_Yp>&& __r) noexcept
: __shared_ptr<_Tp>(std::move(__r)) { }
template<typename _Yp, typename = _Constructible<const weak_ptr<_Yp>&>>
explicit shared_ptr(const weak_ptr<_Yp>& __r)
: __shared_ptr<_Tp>(__r) { }
#if _GLIBCXX_USE_DEPRECATED
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"
template<typename _Yp, typename = _Constructible<auto_ptr<_Yp>>>
shared_ptr(auto_ptr<_Yp>&& __r);
#pragma GCC diagnostic pop
#endif
template<typename _Yp, typename _Del,
typename = _Constructible<unique_ptr<_Yp, _Del>>>
shared_ptr(unique_ptr<_Yp, _Del>&& __r)
: __shared_ptr<_Tp>(std::move(__r)) { }
#if __cplusplus <= 201402L && _GLIBCXX_USE_DEPRECATED
template<typename _Yp, typename _Del,
_Constructible<unique_ptr<_Yp, _Del>, __sp_array_delete>* = 0>
shared_ptr(unique_ptr<_Yp, _Del>&& __r)
: __shared_ptr<_Tp>(std::move(__r), __sp_array_delete()) { }
#endif
constexpr shared_ptr(nullptr_t) noexcept : shared_ptr() { }
shared_ptr& operator=(const shared_ptr&) noexcept = default;
template<typename _Yp>
_Assignable<const shared_ptr<_Yp>&>
operator=(const shared_ptr<_Yp>& __r) noexcept
{
this->__shared_ptr<_Tp>::operator=(__r);
return *this;
}
#if _GLIBCXX_USE_DEPRECATED
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"
template<typename _Yp>
_Assignable<auto_ptr<_Yp>>
operator=(auto_ptr<_Yp>&& __r)
{
this->__shared_ptr<_Tp>::operator=(std::move(__r));
return *this;
}
#pragma GCC diagnostic pop
#endif
shared_ptr&
operator=(shared_ptr&& __r) noexcept
{
this->__shared_ptr<_Tp>::operator=(std::move(__r));
return *this;
}
template<class _Yp>
_Assignable<shared_ptr<_Yp>>
operator=(shared_ptr<_Yp>&& __r) noexcept
{
this->__shared_ptr<_Tp>::operator=(std::move(__r));
return *this;
}
template<typename _Yp, typename _Del>
_Assignable<unique_ptr<_Yp, _Del>>
operator=(unique_ptr<_Yp, _Del>&& __r)
{
this->__shared_ptr<_Tp>::operator=(std::move(__r));
return *this;
}
private:
template<typename _Alloc, typename... _Args>
shared_ptr(_Sp_alloc_shared_tag<_Alloc> __tag, _Args&&... __args)
: __shared_ptr<_Tp>(__tag, std::forward<_Args>(__args)...)
{ }
template<typename _Yp, typename _Alloc, typename... _Args>
friend shared_ptr<_NonArray<_Yp>>
allocate_shared(const _Alloc&, _Args&&...);
template<typename _Yp, typename... _Args>
friend shared_ptr<_NonArray<_Yp>>
make_shared(_Args&&...);
#if __cpp_lib_shared_ptr_arrays >= 201707L
template<typename _Alloc, typename _Init = const remove_extent_t<_Tp>*>
shared_ptr(const _Sp_counted_array_base<_Alloc>& __a,
_Init __init = nullptr)
: __shared_ptr<_Tp>(__a, __init)
{ }
template<typename _Yp, typename _Alloc>
friend shared_ptr<_UnboundedArray<_Yp>>
allocate_shared(const _Alloc&, size_t);
template<typename _Yp>
friend shared_ptr<_UnboundedArray<_Yp>>
make_shared(size_t);
template<typename _Yp, typename _Alloc>
friend shared_ptr<_UnboundedArray<_Yp>>
allocate_shared(const _Alloc&, size_t, const remove_extent_t<_Yp>&);
template<typename _Yp>
friend shared_ptr<_UnboundedArray<_Yp>>
make_shared(size_t, const remove_extent_t<_Yp>&);
template<typename _Yp, typename _Alloc>
friend shared_ptr<_BoundedArray<_Yp>>
allocate_shared(const _Alloc&);
template<typename _Yp>
friend shared_ptr<_BoundedArray<_Yp>>
make_shared();
template<typename _Yp, typename _Alloc>
friend shared_ptr<_BoundedArray<_Yp>>
allocate_shared(const _Alloc&, const remove_extent_t<_Yp>&);
template<typename _Yp>
friend shared_ptr<_BoundedArray<_Yp>>
make_shared(const remove_extent_t<_Yp>&);
#if __cpp_lib_smart_ptr_for_overwrite
template<typename _Yp, typename _Alloc>
friend shared_ptr<_NotUnboundedArray<_Yp>>
allocate_shared_for_overwrite(const _Alloc&);
template<typename _Yp>
friend shared_ptr<_NotUnboundedArray<_Yp>>
make_shared_for_overwrite();
template<typename _Yp, typename _Alloc>
friend shared_ptr<_UnboundedArray<_Yp>>
allocate_shared_for_overwrite(const _Alloc&, size_t);
template<typename _Yp>
friend shared_ptr<_UnboundedArray<_Yp>>
make_shared_for_overwrite(size_t);
#endif
#endif
shared_ptr(const weak_ptr<_Tp>& __r, std::nothrow_t) noexcept
: __shared_ptr<_Tp>(__r, std::nothrow) { }
friend class weak_ptr<_Tp>;
};
#if __cpp_deduction_guides >= 201606
template<typename _Tp>
shared_ptr(weak_ptr<_Tp>) ->  shared_ptr<_Tp>;
template<typename _Tp, typename _Del>
shared_ptr(unique_ptr<_Tp, _Del>) ->  shared_ptr<_Tp>;
#endif
template<typename _Tp, typename _Up>
_GLIBCXX_NODISCARD inline bool
operator==(const shared_ptr<_Tp>& __a, const shared_ptr<_Up>& __b) noexcept
{ return __a.get() == __b.get(); }
template<typename _Tp>
_GLIBCXX_NODISCARD inline bool
operator==(const shared_ptr<_Tp>& __a, nullptr_t) noexcept
{ return !__a; }
#ifdef __cpp_lib_three_way_comparison
template<typename _Tp, typename _Up>
inline strong_ordering
operator<=>(const shared_ptr<_Tp>& __a,
const shared_ptr<_Up>& __b) noexcept
{ return compare_three_way()(__a.get(), __b.get()); }
template<typename _Tp>
inline strong_ordering
operator<=>(const shared_ptr<_Tp>& __a, nullptr_t) noexcept
{
using pointer = typename shared_ptr<_Tp>::element_type*;
return compare_three_way()(__a.get(), static_cast<pointer>(nullptr));
}
#else
template<typename _Tp>
_GLIBCXX_NODISCARD inline bool
operator==(nullptr_t, const shared_ptr<_Tp>& __a) noexcept
{ return !__a; }
template<typename _Tp, typename _Up>
_GLIBCXX_NODISCARD inline bool
operator!=(const shared_ptr<_Tp>& __a, const shared_ptr<_Up>& __b) noexcept
{ return __a.get() != __b.get(); }
template<typename _Tp>
_GLIBCXX_NODISCARD inline bool
operator!=(const shared_ptr<_Tp>& __a, nullptr_t) noexcept
{ return (bool)__a; }
template<typename _Tp>
_GLIBCXX_NODISCARD inline bool
operator!=(nullptr_t, const shared_ptr<_Tp>& __a) noexcept
{ return (bool)__a; }
template<typename _Tp, typename _Up>
_GLIBCXX_NODISCARD inline bool
operator<(const shared_ptr<_Tp>& __a, const shared_ptr<_Up>& __b) noexcept
{
using _Tp_elt = typename shared_ptr<_Tp>::element_type;
using _Up_elt = typename shared_ptr<_Up>::element_type;
using _Vp = typename common_type<_Tp_elt*, _Up_elt*>::type;
return less<_Vp>()(__a.get(), __b.get());
}
template<typename _Tp>
_GLIBCXX_NODISCARD inline bool
operator<(const shared_ptr<_Tp>& __a, nullptr_t) noexcept
{
using _Tp_elt = typename shared_ptr<_Tp>::element_type;
return less<_Tp_elt*>()(__a.get(), nullptr);
}
template<typename _Tp>
_GLIBCXX_NODISCARD inline bool
operator<(nullptr_t, const shared_ptr<_Tp>& __a) noexcept
{
using _Tp_elt = typename shared_ptr<_Tp>::element_type;
return less<_Tp_elt*>()(nullptr, __a.get());
}
template<typename _Tp, typename _Up>
_GLIBCXX_NODISCARD inline bool
operator<=(const shared_ptr<_Tp>& __a, const shared_ptr<_Up>& __b) noexcept
{ return !(__b < __a); }
template<typename _Tp>
_GLIBCXX_NODISCARD inline bool
operator<=(const shared_ptr<_Tp>& __a, nullptr_t) noexcept
{ return !(nullptr < __a); }
template<typename _Tp>
_GLIBCXX_NODISCARD inline bool
operator<=(nullptr_t, const shared_ptr<_Tp>& __a) noexcept
{ return !(__a < nullptr); }
template<typename _Tp, typename _Up>
_GLIBCXX_NODISCARD inline bool
operator>(const shared_ptr<_Tp>& __a, const shared_ptr<_Up>& __b) noexcept
{ return (__b < __a); }
template<typename _Tp>
_GLIBCXX_NODISCARD inline bool
operator>(const shared_ptr<_Tp>& __a, nullptr_t) noexcept
{ return nullptr < __a; }
template<typename _Tp>
_GLIBCXX_NODISCARD inline bool
operator>(nullptr_t, const shared_ptr<_Tp>& __a) noexcept
{ return __a < nullptr; }
template<typename _Tp, typename _Up>
_GLIBCXX_NODISCARD inline bool
operator>=(const shared_ptr<_Tp>& __a, const shared_ptr<_Up>& __b) noexcept
{ return !(__a < __b); }
template<typename _Tp>
_GLIBCXX_NODISCARD inline bool
operator>=(const shared_ptr<_Tp>& __a, nullptr_t) noexcept
{ return !(__a < nullptr); }
template<typename _Tp>
_GLIBCXX_NODISCARD inline bool
operator>=(nullptr_t, const shared_ptr<_Tp>& __a) noexcept
{ return !(nullptr < __a); }
#endif
template<typename _Tp>
inline void
swap(shared_ptr<_Tp>& __a, shared_ptr<_Tp>& __b) noexcept
{ __a.swap(__b); }
template<typename _Tp, typename _Up>
inline shared_ptr<_Tp>
static_pointer_cast(const shared_ptr<_Up>& __r) noexcept
{
using _Sp = shared_ptr<_Tp>;
return _Sp(__r, static_cast<typename _Sp::element_type*>(__r.get()));
}
template<typename _Tp, typename _Up>
inline shared_ptr<_Tp>
const_pointer_cast(const shared_ptr<_Up>& __r) noexcept
{
using _Sp = shared_ptr<_Tp>;
return _Sp(__r, const_cast<typename _Sp::element_type*>(__r.get()));
}
template<typename _Tp, typename _Up>
inline shared_ptr<_Tp>
dynamic_pointer_cast(const shared_ptr<_Up>& __r) noexcept
{
using _Sp = shared_ptr<_Tp>;
if (auto* __p = dynamic_cast<typename _Sp::element_type*>(__r.get()))
return _Sp(__r, __p);
return _Sp();
}
#if __cplusplus >= 201703L
template<typename _Tp, typename _Up>
inline shared_ptr<_Tp>
reinterpret_pointer_cast(const shared_ptr<_Up>& __r) noexcept
{
using _Sp = shared_ptr<_Tp>;
return _Sp(__r, reinterpret_cast<typename _Sp::element_type*>(__r.get()));
}
#if __cplusplus > 201703L
template<typename _Tp, typename _Up>
inline shared_ptr<_Tp>
static_pointer_cast(shared_ptr<_Up>&& __r) noexcept
{
using _Sp = shared_ptr<_Tp>;
return _Sp(std::move(__r),
static_cast<typename _Sp::element_type*>(__r.get()));
}
template<typename _Tp, typename _Up>
inline shared_ptr<_Tp>
const_pointer_cast(shared_ptr<_Up>&& __r) noexcept
{
using _Sp = shared_ptr<_Tp>;
return _Sp(std::move(__r),
const_cast<typename _Sp::element_type*>(__r.get()));
}
template<typename _Tp, typename _Up>
inline shared_ptr<_Tp>
dynamic_pointer_cast(shared_ptr<_Up>&& __r) noexcept
{
using _Sp = shared_ptr<_Tp>;
if (auto* __p = dynamic_cast<typename _Sp::element_type*>(__r.get()))
return _Sp(std::move(__r), __p);
return _Sp();
}
template<typename _Tp, typename _Up>
inline shared_ptr<_Tp>
reinterpret_pointer_cast(shared_ptr<_Up>&& __r) noexcept
{
using _Sp = shared_ptr<_Tp>;
return _Sp(std::move(__r),
reinterpret_cast<typename _Sp::element_type*>(__r.get()));
}
#endif
#endif
template<typename _Tp>
class weak_ptr : public __weak_ptr<_Tp>
{
template<typename _Arg>
using _Constructible = typename enable_if<
is_constructible<__weak_ptr<_Tp>, _Arg>::value
>::type;
template<typename _Arg>
using _Assignable = typename enable_if<
is_assignable<__weak_ptr<_Tp>&, _Arg>::value, weak_ptr&
>::type;
public:
constexpr weak_ptr() noexcept = default;
template<typename _Yp,
typename = _Constructible<const shared_ptr<_Yp>&>>
weak_ptr(const shared_ptr<_Yp>& __r) noexcept
: __weak_ptr<_Tp>(__r) { }
weak_ptr(const weak_ptr&) noexcept = default;
template<typename _Yp, typename = _Constructible<const weak_ptr<_Yp>&>>
weak_ptr(const weak_ptr<_Yp>& __r) noexcept
: __weak_ptr<_Tp>(__r) { }
weak_ptr(weak_ptr&&) noexcept = default;
template<typename _Yp, typename = _Constructible<weak_ptr<_Yp>>>
weak_ptr(weak_ptr<_Yp>&& __r) noexcept
: __weak_ptr<_Tp>(std::move(__r)) { }
weak_ptr&
operator=(const weak_ptr& __r) noexcept = default;
template<typename _Yp>
_Assignable<const weak_ptr<_Yp>&>
operator=(const weak_ptr<_Yp>& __r) noexcept
{
this->__weak_ptr<_Tp>::operator=(__r);
return *this;
}
template<typename _Yp>
_Assignable<const shared_ptr<_Yp>&>
operator=(const shared_ptr<_Yp>& __r) noexcept
{
this->__weak_ptr<_Tp>::operator=(__r);
return *this;
}
weak_ptr&
operator=(weak_ptr&& __r) noexcept = default;
template<typename _Yp>
_Assignable<weak_ptr<_Yp>>
operator=(weak_ptr<_Yp>&& __r) noexcept
{
this->__weak_ptr<_Tp>::operator=(std::move(__r));
return *this;
}
shared_ptr<_Tp>
lock() const noexcept
{ return shared_ptr<_Tp>(*this, std::nothrow); }
};
#if __cpp_deduction_guides >= 201606
template<typename _Tp>
weak_ptr(shared_ptr<_Tp>) ->  weak_ptr<_Tp>;
#endif
template<typename _Tp>
inline void
swap(weak_ptr<_Tp>& __a, weak_ptr<_Tp>& __b) noexcept
{ __a.swap(__b); }
template<typename _Tp = void>
struct owner_less;
template<>
struct owner_less<void> : _Sp_owner_less<void, void>
{ };
template<typename _Tp>
struct owner_less<shared_ptr<_Tp>>
: public _Sp_owner_less<shared_ptr<_Tp>, weak_ptr<_Tp>>
{ };
template<typename _Tp>
struct owner_less<weak_ptr<_Tp>>
: public _Sp_owner_less<weak_ptr<_Tp>, shared_ptr<_Tp>>
{ };
template<typename _Tp>
class enable_shared_from_this
{
protected:
constexpr enable_shared_from_this() noexcept { }
enable_shared_from_this(const enable_shared_from_this&) noexcept { }
enable_shared_from_this&
operator=(const enable_shared_from_this&) noexcept
{ return *this; }
~enable_shared_from_this() { }
public:
shared_ptr<_Tp>
shared_from_this()
{ return shared_ptr<_Tp>(this->_M_weak_this); }
shared_ptr<const _Tp>
shared_from_this() const
{ return shared_ptr<const _Tp>(this->_M_weak_this); }
#if __cplusplus > 201402L || !defined(__STRICT_ANSI__)
#define __cpp_lib_enable_shared_from_this 201603L
weak_ptr<_Tp>
weak_from_this() noexcept
{ return this->_M_weak_this; }
weak_ptr<const _Tp>
weak_from_this() const noexcept
{ return this->_M_weak_this; }
#endif
private:
template<typename _Tp1>
void
_M_weak_assign(_Tp1* __p, const __shared_count<>& __n) const noexcept
{ _M_weak_this._M_assign(__p, __n); }
friend const enable_shared_from_this*
__enable_shared_from_this_base(const __shared_count<>&,
const enable_shared_from_this* __p)
{ return __p; }
template<typename, _Lock_policy>
friend class __shared_ptr;
mutable weak_ptr<_Tp>  _M_weak_this;
};
template<typename _Tp, typename _Alloc, typename... _Args>
inline shared_ptr<_NonArray<_Tp>>
allocate_shared(const _Alloc& __a, _Args&&... __args)
{
return shared_ptr<_Tp>(_Sp_alloc_shared_tag<_Alloc>{__a},
std::forward<_Args>(__args)...);
}
template<typename _Tp, typename... _Args>
inline shared_ptr<_NonArray<_Tp>>
make_shared(_Args&&... __args)
{
using _Alloc = allocator<void>;
_Alloc __a;
return shared_ptr<_Tp>(_Sp_alloc_shared_tag<_Alloc>{__a},
std::forward<_Args>(__args)...);
}
#if __cpp_lib_shared_ptr_arrays >= 201707L
template<typename _Tp, typename _Alloc = allocator<void>>
auto
__make_shared_arr_tag(size_t __n, const _Alloc& __a = _Alloc()) noexcept
{
using _Up = remove_all_extents_t<_Tp>;
using _UpAlloc = __alloc_rebind<_Alloc, _Up>;
size_t __s = sizeof(remove_extent_t<_Tp>) / sizeof(_Up);
if (__builtin_mul_overflow(__s, __n, &__n))
std::__throw_bad_array_new_length();
return _Sp_counted_array_base<_UpAlloc>{_UpAlloc(__a), __n};
}
template<typename _Tp, typename _Alloc>
inline shared_ptr<_UnboundedArray<_Tp>>
allocate_shared(const _Alloc& __a, size_t __n)
{
return shared_ptr<_Tp>(std::__make_shared_arr_tag<_Tp>(__n, __a));
}
template<typename _Tp>
inline shared_ptr<_UnboundedArray<_Tp>>
make_shared(size_t __n)
{
return shared_ptr<_Tp>(std::__make_shared_arr_tag<_Tp>(__n));
}
template<typename _Tp, typename _Alloc>
inline shared_ptr<_UnboundedArray<_Tp>>
allocate_shared(const _Alloc& __a, size_t __n,
const remove_extent_t<_Tp>& __u)
{
return shared_ptr<_Tp>(std::__make_shared_arr_tag<_Tp>(__n, __a),
std::__addressof(__u));
}
template<typename _Tp>
inline shared_ptr<_UnboundedArray<_Tp>>
make_shared(size_t __n, const remove_extent_t<_Tp>& __u)
{
return shared_ptr<_Tp>(std::__make_shared_arr_tag<_Tp>(__n),
std::__addressof(__u));
}
template<typename _Tp, typename _Alloc = allocator<void>>
auto
__make_shared_arrN_tag(const _Alloc& __a = _Alloc()) noexcept
{
using _Up = remove_all_extents_t<_Tp>;
using _UpAlloc = __alloc_rebind<_Alloc, _Up>;
size_t __n = sizeof(_Tp) / sizeof(_Up);
return _Sp_counted_array_base<_UpAlloc>{_UpAlloc(__a), __n};
}
template<typename _Tp, typename _Alloc>
inline shared_ptr<_BoundedArray<_Tp>>
allocate_shared(const _Alloc& __a)
{
return shared_ptr<_Tp>(std::__make_shared_arrN_tag<_Tp>(__a));
}
template<typename _Tp>
inline shared_ptr<_BoundedArray<_Tp>>
make_shared()
{
return shared_ptr<_Tp>(std::__make_shared_arrN_tag<_Tp>());
}
template<typename _Tp, typename _Alloc>
inline shared_ptr<_BoundedArray<_Tp>>
allocate_shared(const _Alloc& __a, const remove_extent_t<_Tp>& __u)
{
return shared_ptr<_Tp>(std::__make_shared_arrN_tag<_Tp>(__a),
std::__addressof(__u));
}
template<typename _Tp>
inline shared_ptr<_BoundedArray<_Tp>>
make_shared(const remove_extent_t<_Tp>& __u)
{
return shared_ptr<_Tp>(std::__make_shared_arrN_tag<_Tp>(),
std::__addressof(__u));
}
#if __cpp_lib_smart_ptr_for_overwrite
template<typename _Tp, typename _Alloc>
inline shared_ptr<_NotUnboundedArray<_Tp>>
allocate_shared_for_overwrite(const _Alloc& __a)
{
if constexpr (is_array_v<_Tp>)
return shared_ptr<_Tp>(std::__make_shared_arrN_tag<_Tp>(__a),
_Sp_overwrite_tag{});
else
{
using _Alloc2 = __alloc_rebind<_Alloc, _Sp_overwrite_tag>;
_Alloc2 __a2 = __a;
return shared_ptr<_Tp>(_Sp_alloc_shared_tag<_Alloc2>{__a2});
}
}
template<typename _Tp>
inline shared_ptr<_NotUnboundedArray<_Tp>>
make_shared_for_overwrite()
{
if constexpr (is_array_v<_Tp>)
return shared_ptr<_Tp>(std::__make_shared_arrN_tag<_Tp>(),
_Sp_overwrite_tag{});
else
{
using _Alloc = allocator<_Sp_overwrite_tag>;
return shared_ptr<_Tp>(_Sp_alloc_shared_tag<_Alloc>{{}});
}
}
template<typename _Tp, typename _Alloc>
inline shared_ptr<_UnboundedArray<_Tp>>
allocate_shared_for_overwrite(const _Alloc& __a, size_t __n)
{
return shared_ptr<_Tp>(std::__make_shared_arr_tag<_Tp>(__n, __a),
_Sp_overwrite_tag{});
}
template<typename _Tp>
inline shared_ptr<_UnboundedArray<_Tp>>
make_shared_for_overwrite(size_t __n)
{
return shared_ptr<_Tp>(std::__make_shared_arr_tag<_Tp>(__n),
_Sp_overwrite_tag{});
}
#endif
#endif
template<typename _Tp>
struct hash<shared_ptr<_Tp>>
: public __hash_base<size_t, shared_ptr<_Tp>>
{
size_t
operator()(const shared_ptr<_Tp>& __s) const noexcept
{
return std::hash<typename shared_ptr<_Tp>::element_type*>()(__s.get());
}
};
#if __cplusplus >= 201703L
namespace __detail::__variant
{
template<typename> struct _Never_valueless_alt;
template<typename _Tp>
struct _Never_valueless_alt<std::shared_ptr<_Tp>>
: std::true_type
{ };
template<typename _Tp>
struct _Never_valueless_alt<std::weak_ptr<_Tp>>
: std::true_type
{ };
}
#endif
_GLIBCXX_END_NAMESPACE_VERSION
}
#endif