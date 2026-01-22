#ifndef _STL_TEMPBUF_H
#define _STL_TEMPBUF_H 1
#include <new>
#include <bits/exception_defines.h>
#include <bits/stl_construct.h>
#include <bits/stl_pair.h>
#include <ext/numeric_traits.h>
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
namespace __detail
{
template<typename _Tp>
inline void
__return_temporary_buffer(_Tp* __p,
size_t __len __attribute__((__unused__)))
{
#if __cpp_sized_deallocation
::operator delete(__p, __len * sizeof(_Tp));
#else
::operator delete(__p);
#endif
}
}
template<typename _Tp>
_GLIBCXX17_DEPRECATED
pair<_Tp*, ptrdiff_t>
get_temporary_buffer(ptrdiff_t __len) _GLIBCXX_NOEXCEPT
{
const ptrdiff_t __max =
__gnu_cxx::__numeric_traits<ptrdiff_t>::__max / sizeof(_Tp);
if (__len > __max)
__len = __max;
while (__len > 0)
{
_Tp* __tmp = static_cast<_Tp*>(::operator new(__len * sizeof(_Tp),
std::nothrow));
if (__tmp != 0)
return std::pair<_Tp*, ptrdiff_t>(__tmp, __len);
__len = __len == 1 ? 0 : ((__len + 1) / 2);
}
return std::pair<_Tp*, ptrdiff_t>(static_cast<_Tp*>(0), 0);
}
template<typename _Tp>
inline void
return_temporary_buffer(_Tp* __p)
{ ::operator delete(__p); }
template<typename _ForwardIterator, typename _Tp>
class _Temporary_buffer
{
__glibcxx_class_requires(_ForwardIterator, _ForwardIteratorConcept)
public:
typedef _Tp value_type;
typedef value_type* pointer;
typedef pointer iterator;
typedef ptrdiff_t size_type;
protected:
size_type _M_original_len;
size_type _M_len;
pointer _M_buffer;
public:
size_type
size() const
{ return _M_len; }
size_type
requested_size() const
{ return _M_original_len; }
iterator
begin()
{ return _M_buffer; }
iterator
end()
{ return _M_buffer + _M_len; }
_Temporary_buffer(_ForwardIterator __seed, size_type __original_len);
~_Temporary_buffer()
{
std::_Destroy(_M_buffer, _M_buffer + _M_len);
std::__detail::__return_temporary_buffer(_M_buffer, _M_len);
}
private:
_Temporary_buffer(const _Temporary_buffer&);
void
operator=(const _Temporary_buffer&);
};
template<bool>
struct __uninitialized_construct_buf_dispatch
{
template<typename _Pointer, typename _ForwardIterator>
static void
__ucr(_Pointer __first, _Pointer __last,
_ForwardIterator __seed)
{
if (__first == __last)
return;
_Pointer __cur = __first;
__try
{
std::_Construct(std::__addressof(*__first),
_GLIBCXX_MOVE(*__seed));
_Pointer __prev = __cur;
++__cur;
for(; __cur != __last; ++__cur, ++__prev)
std::_Construct(std::__addressof(*__cur),
_GLIBCXX_MOVE(*__prev));
*__seed = _GLIBCXX_MOVE(*__prev);
}
__catch(...)
{
std::_Destroy(__first, __cur);
__throw_exception_again;
}
}
};
template<>
struct __uninitialized_construct_buf_dispatch<true>
{
template<typename _Pointer, typename _ForwardIterator>
static void
__ucr(_Pointer, _Pointer, _ForwardIterator) { }
};
template<typename _Pointer, typename _ForwardIterator>
inline void
__uninitialized_construct_buf(_Pointer __first, _Pointer __last,
_ForwardIterator __seed)
{
typedef typename std::iterator_traits<_Pointer>::value_type
_ValueType;
std::__uninitialized_construct_buf_dispatch<
__has_trivial_constructor(_ValueType)>::
__ucr(__first, __last, __seed);
}
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"
template<typename _ForwardIterator, typename _Tp>
_Temporary_buffer<_ForwardIterator, _Tp>::
_Temporary_buffer(_ForwardIterator __seed, size_type __original_len)
: _M_original_len(__original_len), _M_len(0), _M_buffer(0)
{
std::pair<pointer, size_type> __p(
std::get_temporary_buffer<value_type>(_M_original_len));
if (__p.first)
{
__try
{
std::__uninitialized_construct_buf(__p.first, __p.first + __p.second,
__seed);
_M_buffer = __p.first;
_M_len = __p.second;
}
__catch(...)
{
std::__detail::__return_temporary_buffer(__p.first, __p.second);
__throw_exception_again;
}
}
}
#pragma GCC diagnostic pop
_GLIBCXX_END_NAMESPACE_VERSION
}
#endif