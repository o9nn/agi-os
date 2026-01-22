#ifndef _NEW_ALLOCATOR_H
#define _NEW_ALLOCATOR_H 1
#include <bits/c++config.h>
#include <new>
#include <bits/functexcept.h>
#include <bits/move.h>
#if __cplusplus >= 201103L
#include <type_traits>
#endif
namespace __gnu_cxx _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
template<typename _Tp>
class new_allocator
{
public:
typedef _Tp        value_type;
typedef std::size_t     size_type;
typedef std::ptrdiff_t  difference_type;
#if __cplusplus <= 201703L
typedef _Tp*       pointer;
typedef const _Tp* const_pointer;
typedef _Tp&       reference;
typedef const _Tp& const_reference;
template<typename _Tp1>
struct rebind
{ typedef new_allocator<_Tp1> other; };
#endif
#if __cplusplus >= 201103L
typedef std::true_type propagate_on_container_move_assignment;
#endif
_GLIBCXX20_CONSTEXPR
new_allocator() _GLIBCXX_USE_NOEXCEPT { }
_GLIBCXX20_CONSTEXPR
new_allocator(const new_allocator&) _GLIBCXX_USE_NOEXCEPT { }
template<typename _Tp1>
_GLIBCXX20_CONSTEXPR
new_allocator(const new_allocator<_Tp1>&) _GLIBCXX_USE_NOEXCEPT { }
#if __cplusplus <= 201703L
~new_allocator() _GLIBCXX_USE_NOEXCEPT { }
pointer
address(reference __x) const _GLIBCXX_NOEXCEPT
{ return std::__addressof(__x); }
const_pointer
address(const_reference __x) const _GLIBCXX_NOEXCEPT
{ return std::__addressof(__x); }
#endif
_GLIBCXX_NODISCARD _Tp*
allocate(size_type __n, const void* = static_cast<const void*>(0))
{
#if __cplusplus >= 201103L
static_assert(sizeof(_Tp) != 0, "cannot allocate incomplete types");
#endif
if (__builtin_expect(__n > this->_M_max_size(), false))
{
if (__n > (std::size_t(-1) / sizeof(_Tp)))
std::__throw_bad_array_new_length();
std::__throw_bad_alloc();
}
#if __cpp_aligned_new
if (alignof(_Tp) > __STDCPP_DEFAULT_NEW_ALIGNMENT__)
{
std::align_val_t __al = std::align_val_t(alignof(_Tp));
return static_cast<_Tp*>(::operator new(__n * sizeof(_Tp), __al));
}
#endif
return static_cast<_Tp*>(::operator new(__n * sizeof(_Tp)));
}
void
deallocate(_Tp* __p, size_type __t __attribute__ ((__unused__)))
{
#if __cpp_aligned_new
if (alignof(_Tp) > __STDCPP_DEFAULT_NEW_ALIGNMENT__)
{
::operator delete(__p,
# if __cpp_sized_deallocation
__t * sizeof(_Tp),
# endif
std::align_val_t(alignof(_Tp)));
return;
}
#endif
::operator delete(__p
#if __cpp_sized_deallocation
, __t * sizeof(_Tp)
#endif
);
}
#if __cplusplus <= 201703L
size_type
max_size() const _GLIBCXX_USE_NOEXCEPT
{ return _M_max_size(); }
#if __cplusplus >= 201103L
template<typename _Up, typename... _Args>
void
construct(_Up* __p, _Args&&... __args)
noexcept(std::is_nothrow_constructible<_Up, _Args...>::value)
{ ::new((void *)__p) _Up(std::forward<_Args>(__args)...); }
template<typename _Up>
void
destroy(_Up* __p)
noexcept(std::is_nothrow_destructible<_Up>::value)
{ __p->~_Up(); }
#else
void
construct(pointer __p, const _Tp& __val)
{ ::new((void *)__p) _Tp(__val); }
void
destroy(pointer __p) { __p->~_Tp(); }
#endif
#endif
template<typename _Up>
friend _GLIBCXX20_CONSTEXPR bool
operator==(const new_allocator&, const new_allocator<_Up>&)
_GLIBCXX_NOTHROW
{ return true; }
#if __cpp_impl_three_way_comparison < 201907L
template<typename _Up>
friend _GLIBCXX20_CONSTEXPR bool
operator!=(const new_allocator&, const new_allocator<_Up>&)
_GLIBCXX_NOTHROW
{ return false; }
#endif
private:
_GLIBCXX_CONSTEXPR size_type
_M_max_size() const _GLIBCXX_USE_NOEXCEPT
{
#if __PTRDIFF_MAX__ < __SIZE_MAX__
return std::size_t(__PTRDIFF_MAX__) / sizeof(_Tp);
#else
return std::size_t(-1) / sizeof(_Tp);
#endif
}
};
_GLIBCXX_END_NAMESPACE_VERSION
}
#endif