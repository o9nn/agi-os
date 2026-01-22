#ifndef _ALLOCATED_PTR_H
#define _ALLOCATED_PTR_H 1
#if __cplusplus < 201103L
# include <bits/c++0xwarning.h>
#else
# include <type_traits>
# include <bits/ptr_traits.h>
# include <bits/alloc_traits.h>
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
template<typename _Alloc>
struct __allocated_ptr
{
using pointer = typename allocator_traits<_Alloc>::pointer;
using value_type = typename allocator_traits<_Alloc>::value_type;
__allocated_ptr(_Alloc& __a, pointer __ptr) noexcept
: _M_alloc(std::__addressof(__a)), _M_ptr(__ptr)
{ }
template<typename _Ptr,
typename _Req = _Require<is_same<_Ptr, value_type*>>>
__allocated_ptr(_Alloc& __a, _Ptr __ptr)
: _M_alloc(std::__addressof(__a)),
_M_ptr(pointer_traits<pointer>::pointer_to(*__ptr))
{ }
__allocated_ptr(__allocated_ptr&& __gd) noexcept
: _M_alloc(__gd._M_alloc), _M_ptr(__gd._M_ptr)
{ __gd._M_ptr = nullptr; }
~__allocated_ptr()
{
if (_M_ptr != nullptr)
std::allocator_traits<_Alloc>::deallocate(*_M_alloc, _M_ptr, 1);
}
__allocated_ptr&
operator=(std::nullptr_t) noexcept
{
_M_ptr = nullptr;
return *this;
}
value_type* get() { return std::__to_address(_M_ptr); }
private:
_Alloc* _M_alloc;
pointer _M_ptr;
};
template<typename _Alloc>
__allocated_ptr<_Alloc>
__allocate_guarded(_Alloc& __a)
{
return { __a, std::allocator_traits<_Alloc>::allocate(__a, 1) };
}
_GLIBCXX_END_NAMESPACE_VERSION
}
#endif
#endif