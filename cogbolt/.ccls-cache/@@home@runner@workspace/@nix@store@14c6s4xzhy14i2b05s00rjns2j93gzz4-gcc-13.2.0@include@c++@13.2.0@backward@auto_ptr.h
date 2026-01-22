#ifndef _BACKWARD_AUTO_PTR_H
#define _BACKWARD_AUTO_PTR_H 1
#include <bits/c++config.h>
#include <debug/debug.h>
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
template<typename _Tp1>
struct auto_ptr_ref
{
_Tp1* _M_ptr;
explicit
auto_ptr_ref(_Tp1* __p): _M_ptr(__p) { }
} _GLIBCXX11_DEPRECATED;
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"
template<typename _Tp>
class auto_ptr
{
private:
_Tp* _M_ptr;
public:
typedef _Tp element_type;
explicit
auto_ptr(element_type* __p = 0) throw() : _M_ptr(__p) { }
auto_ptr(auto_ptr& __a) throw() : _M_ptr(__a.release()) { }
template<typename _Tp1>
auto_ptr(auto_ptr<_Tp1>& __a) throw() : _M_ptr(__a.release()) { }
auto_ptr&
operator=(auto_ptr& __a) throw()
{
reset(__a.release());
return *this;
}
template<typename _Tp1>
auto_ptr&
operator=(auto_ptr<_Tp1>& __a) throw()
{
reset(__a.release());
return *this;
}
~auto_ptr() { delete _M_ptr; }
element_type&
operator*() const throw()
{
__glibcxx_assert(_M_ptr != 0);
return *_M_ptr;
}
element_type*
operator->() const throw()
{
__glibcxx_assert(_M_ptr != 0);
return _M_ptr;
}
element_type*
get() const throw() { return _M_ptr; }
element_type*
release() throw()
{
element_type* __tmp = _M_ptr;
_M_ptr = 0;
return __tmp;
}
void
reset(element_type* __p = 0) throw()
{
if (__p != _M_ptr)
{
delete _M_ptr;
_M_ptr = __p;
}
}
auto_ptr(auto_ptr_ref<element_type> __ref) throw()
: _M_ptr(__ref._M_ptr) { }
auto_ptr&
operator=(auto_ptr_ref<element_type> __ref) throw()
{
if (__ref._M_ptr != this->get())
{
delete _M_ptr;
_M_ptr = __ref._M_ptr;
}
return *this;
}
template<typename _Tp1>
operator auto_ptr_ref<_Tp1>() throw()
{ return auto_ptr_ref<_Tp1>(this->release()); }
template<typename _Tp1>
operator auto_ptr<_Tp1>() throw()
{ return auto_ptr<_Tp1>(this->release()); }
} _GLIBCXX11_DEPRECATED_SUGGEST("std::unique_ptr");
template<>
class auto_ptr<void>
{
public:
typedef void element_type;
} _GLIBCXX11_DEPRECATED;
#if __cplusplus >= 201103L
#if _GLIBCXX_HOSTED
template<_Lock_policy _Lp>
template<typename _Tp>
inline
__shared_count<_Lp>::__shared_count(std::auto_ptr<_Tp>&& __r)
: _M_pi(new _Sp_counted_ptr<_Tp*, _Lp>(__r.get()))
{ __r.release(); }
template<typename _Tp, _Lock_policy _Lp>
template<typename _Tp1, typename>
inline
__shared_ptr<_Tp, _Lp>::__shared_ptr(std::auto_ptr<_Tp1>&& __r)
: _M_ptr(__r.get()), _M_refcount()
{
__glibcxx_function_requires(_ConvertibleConcept<_Tp1*, _Tp*>)
static_assert( sizeof(_Tp1) > 0, "incomplete type" );
_Tp1* __tmp = __r.get();
_M_refcount = __shared_count<_Lp>(std::move(__r));
_M_enable_shared_from_this_with(__tmp);
}
template<typename _Tp>
template<typename _Tp1, typename>
inline
shared_ptr<_Tp>::shared_ptr(std::auto_ptr<_Tp1>&& __r)
: __shared_ptr<_Tp>(std::move(__r)) { }
#endif
template<typename _Tp, typename _Dp>
template<typename _Up, typename>
inline
unique_ptr<_Tp, _Dp>::unique_ptr(auto_ptr<_Up>&& __u) noexcept
: _M_t(__u.release(), deleter_type()) { }
#endif
#pragma GCC diagnostic pop
_GLIBCXX_END_NAMESPACE_VERSION
}
#endif