#ifndef _BASIC_STRING_H
#define _BASIC_STRING_H 1
#pragma GCC system_header
#include <ext/atomicity.h>
#include <ext/alloc_traits.h>
#include <debug/debug.h>
#if __cplusplus >= 201103L
#include <initializer_list>
#endif
#if __cplusplus >= 201703L
# include <string_view>
#endif
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
#ifdef __cpp_lib_is_constant_evaluated
# define __cpp_lib_constexpr_string 201811L
#elif __cplusplus >= 201703L
# define __cpp_lib_constexpr_string 201611L
#elif __cplusplus > 201703L
#endif
#if _GLIBCXX_USE_CXX11_ABI
_GLIBCXX_BEGIN_NAMESPACE_CXX11
template<typename _CharT, typename _Traits, typename _Alloc>
class basic_string
{
typedef typename __gnu_cxx::__alloc_traits<_Alloc>::template
rebind<_CharT>::other _Char_alloc_type;
typedef __gnu_cxx::__alloc_traits<_Char_alloc_type> _Alloc_traits;
public:
typedef _Traits					traits_type;
typedef typename _Traits::char_type		value_type;
typedef _Char_alloc_type				allocator_type;
typedef typename _Alloc_traits::size_type		size_type;
typedef typename _Alloc_traits::difference_type	difference_type;
typedef typename _Alloc_traits::reference		reference;
typedef typename _Alloc_traits::const_reference	const_reference;
typedef typename _Alloc_traits::pointer		pointer;
typedef typename _Alloc_traits::const_pointer	const_pointer;
typedef __gnu_cxx::__normal_iterator<pointer, basic_string>  iterator;
typedef __gnu_cxx::__normal_iterator<const_pointer, basic_string>
const_iterator;
typedef std::reverse_iterator<const_iterator>	const_reverse_iterator;
typedef std::reverse_iterator<iterator>		reverse_iterator;
static const size_type	npos = static_cast<size_type>(-1);
protected:
#if __cplusplus < 201103L
typedef iterator __const_iterator;
#else
typedef const_iterator __const_iterator;
#endif
private:
#if __cplusplus >= 201703L
typedef basic_string_view<_CharT, _Traits> __sv_type;
template<typename _Tp, typename _Res>
using _If_sv = enable_if_t<
__and_<is_convertible<const _Tp&, __sv_type>,
__not_<is_convertible<const _Tp*, const basic_string*>>,
__not_<is_convertible<const _Tp&, const _CharT*>>>::value,
_Res>;
static __sv_type
_S_to_string_view(__sv_type __svt) noexcept
{ return __svt; }
struct __sv_wrapper
{
explicit __sv_wrapper(__sv_type __sv) noexcept : _M_sv(__sv) { }
__sv_type _M_sv;
};
explicit
basic_string(__sv_wrapper __svw, const _Alloc& __a)
: basic_string(__svw._M_sv.data(), __svw._M_sv.size(), __a) { }
#endif
struct _Alloc_hider : allocator_type
{
#if __cplusplus < 201103L
_Alloc_hider(pointer __dat, const _Alloc& __a = _Alloc())
: allocator_type(__a), _M_p(__dat) { }
#else
_Alloc_hider(pointer __dat, const _Alloc& __a)
: allocator_type(__a), _M_p(__dat) { }
_Alloc_hider(pointer __dat, _Alloc&& __a = _Alloc())
: allocator_type(std::move(__a)), _M_p(__dat) { }
#endif
pointer _M_p;
};
_Alloc_hider	_M_dataplus;
size_type		_M_string_length;
enum { _S_local_capacity = 15 / sizeof(_CharT) };
union
{
_CharT           _M_local_buf[_S_local_capacity + 1];
size_type        _M_allocated_capacity;
};
void
_M_data(pointer __p)
{ _M_dataplus._M_p = __p; }
void
_M_length(size_type __length)
{ _M_string_length = __length; }
pointer
_M_data() const
{ return _M_dataplus._M_p; }
pointer
_M_local_data()
{
#if __cplusplus >= 201103L
return std::pointer_traits<pointer>::pointer_to(*_M_local_buf);
#else
return pointer(_M_local_buf);
#endif
}
const_pointer
_M_local_data() const
{
#if __cplusplus >= 201103L
return std::pointer_traits<const_pointer>::pointer_to(*_M_local_buf);
#else
return const_pointer(_M_local_buf);
#endif
}
void
_M_capacity(size_type __capacity)
{ _M_allocated_capacity = __capacity; }
void
_M_set_length(size_type __n)
{
_M_length(__n);
traits_type::assign(_M_data()[__n], _CharT());
}
bool
_M_is_local() const
{ return _M_data() == _M_local_data(); }
pointer
_M_create(size_type&, size_type);
void
_M_dispose()
{
if (!_M_is_local())
_M_destroy(_M_allocated_capacity);
}
void
_M_destroy(size_type __size) throw()
{ _Alloc_traits::deallocate(_M_get_allocator(), _M_data(), __size + 1); }
template<typename _InIterator>
void
_M_construct_aux(_InIterator __beg, _InIterator __end,
std::__false_type)
{
typedef typename iterator_traits<_InIterator>::iterator_category _Tag;
_M_construct(__beg, __end, _Tag());
}
template<typename _Integer>
void
_M_construct_aux(_Integer __beg, _Integer __end, std::__true_type)
{ _M_construct_aux_2(static_cast<size_type>(__beg), __end); }
void
_M_construct_aux_2(size_type __req, _CharT __c)
{ _M_construct(__req, __c); }
template<typename _InIterator>
void
_M_construct(_InIterator __beg, _InIterator __end)
{
typedef typename std::__is_integer<_InIterator>::__type _Integral;
_M_construct_aux(__beg, __end, _Integral());
}
template<typename _InIterator>
void
_M_construct(_InIterator __beg, _InIterator __end,
std::input_iterator_tag);
template<typename _FwdIterator>
void
_M_construct(_FwdIterator __beg, _FwdIterator __end,
std::forward_iterator_tag);
void
_M_construct(size_type __req, _CharT __c);
allocator_type&
_M_get_allocator()
{ return _M_dataplus; }
const allocator_type&
_M_get_allocator() const
{ return _M_dataplus; }
private:
#ifdef _GLIBCXX_DISAMBIGUATE_REPLACE_INST
template<typename _Tp, bool _Requires =
!__are_same<_Tp, _CharT*>::__value
&& !__are_same<_Tp, const _CharT*>::__value
&& !__are_same<_Tp, iterator>::__value
&& !__are_same<_Tp, const_iterator>::__value>
struct __enable_if_not_native_iterator
{ typedef basic_string& __type; };
template<typename _Tp>
struct __enable_if_not_native_iterator<_Tp, false> { };
#endif
size_type
_M_check(size_type __pos, const char* __s) const
{
if (__pos > this->size())
__throw_out_of_range_fmt(__N("%s: __pos (which is %zu) > "
"this->size() (which is %zu)"),
__s, __pos, this->size());
return __pos;
}
void
_M_check_length(size_type __n1, size_type __n2, const char* __s) const
{
if (this->max_size() - (this->size() - __n1) < __n2)
__throw_length_error(__N(__s));
}
size_type
_M_limit(size_type __pos, size_type __off) const _GLIBCXX_NOEXCEPT
{
const bool __testoff =  __off < this->size() - __pos;
return __testoff ? __off : this->size() - __pos;
}
bool
_M_disjunct(const _CharT* __s) const _GLIBCXX_NOEXCEPT
{
return (less<const _CharT*>()(__s, _M_data())
|| less<const _CharT*>()(_M_data() + this->size(), __s));
}
static void
_S_copy(_CharT* __d, const _CharT* __s, size_type __n)
{
if (__n == 1)
traits_type::assign(*__d, *__s);
else
traits_type::copy(__d, __s, __n);
}
static void
_S_move(_CharT* __d, const _CharT* __s, size_type __n)
{
if (__n == 1)
traits_type::assign(*__d, *__s);
else
traits_type::move(__d, __s, __n);
}
static void
_S_assign(_CharT* __d, size_type __n, _CharT __c)
{
if (__n == 1)
traits_type::assign(*__d, __c);
else
traits_type::assign(__d, __n, __c);
}
template<class _Iterator>
static void
_S_copy_chars(_CharT* __p, _Iterator __k1, _Iterator __k2)
{
for (; __k1 != __k2; ++__k1, (void)++__p)
traits_type::assign(*__p, *__k1);
}
static void
_S_copy_chars(_CharT* __p, iterator __k1, iterator __k2) _GLIBCXX_NOEXCEPT
{ _S_copy_chars(__p, __k1.base(), __k2.base()); }
static void
_S_copy_chars(_CharT* __p, const_iterator __k1, const_iterator __k2)
_GLIBCXX_NOEXCEPT
{ _S_copy_chars(__p, __k1.base(), __k2.base()); }
static void
_S_copy_chars(_CharT* __p, _CharT* __k1, _CharT* __k2) _GLIBCXX_NOEXCEPT
{ _S_copy(__p, __k1, __k2 - __k1); }
static void
_S_copy_chars(_CharT* __p, const _CharT* __k1, const _CharT* __k2)
_GLIBCXX_NOEXCEPT
{ _S_copy(__p, __k1, __k2 - __k1); }
static int
_S_compare(size_type __n1, size_type __n2) _GLIBCXX_NOEXCEPT
{
const difference_type __d = difference_type(__n1 - __n2);
if (__d > __gnu_cxx::__numeric_traits<int>::__max)
return __gnu_cxx::__numeric_traits<int>::__max;
else if (__d < __gnu_cxx::__numeric_traits<int>::__min)
return __gnu_cxx::__numeric_traits<int>::__min;
else
return int(__d);
}
void
_M_assign(const basic_string&);
void
_M_mutate(size_type __pos, size_type __len1, const _CharT* __s,
size_type __len2);
void
_M_erase(size_type __pos, size_type __n);
public:
basic_string()
_GLIBCXX_NOEXCEPT_IF(is_nothrow_default_constructible<_Alloc>::value)
: _M_dataplus(_M_local_data())
{ _M_set_length(0); }
explicit
basic_string(const _Alloc& __a) _GLIBCXX_NOEXCEPT
: _M_dataplus(_M_local_data(), __a)
{ _M_set_length(0); }
basic_string(const basic_string& __str)
: _M_dataplus(_M_local_data(),
_Alloc_traits::_S_select_on_copy(__str._M_get_allocator()))
{ _M_construct(__str._M_data(), __str._M_data() + __str.length()); }
basic_string(const basic_string& __str, size_type __pos,
const _Alloc& __a = _Alloc())
: _M_dataplus(_M_local_data(), __a)
{
const _CharT* __start = __str._M_data()
+ __str._M_check(__pos, "basic_string::basic_string");
_M_construct(__start, __start + __str._M_limit(__pos, npos));
}
basic_string(const basic_string& __str, size_type __pos,
size_type __n)
: _M_dataplus(_M_local_data())
{
const _CharT* __start = __str._M_data()
+ __str._M_check(__pos, "basic_string::basic_string");
_M_construct(__start, __start + __str._M_limit(__pos, __n));
}
basic_string(const basic_string& __str, size_type __pos,
size_type __n, const _Alloc& __a)
: _M_dataplus(_M_local_data(), __a)
{
const _CharT* __start
= __str._M_data() + __str._M_check(__pos, "string::string");
_M_construct(__start, __start + __str._M_limit(__pos, __n));
}
basic_string(const _CharT* __s, size_type __n,
const _Alloc& __a = _Alloc())
: _M_dataplus(_M_local_data(), __a)
{ _M_construct(__s, __s + __n); }
#if __cpp_deduction_guides && ! defined _GLIBCXX_DEFINING_STRING_INSTANTIATIONS
template<typename = _RequireAllocator<_Alloc>>
#endif
basic_string(const _CharT* __s, const _Alloc& __a = _Alloc())
: _M_dataplus(_M_local_data(), __a)
{
const _CharT* __end = __s ? __s + traits_type::length(__s)
: reinterpret_cast<const _CharT*>(__alignof__(_CharT));
_M_construct(__s, __end, random_access_iterator_tag());
}
#if __cpp_deduction_guides && ! defined _GLIBCXX_DEFINING_STRING_INSTANTIATIONS
template<typename = _RequireAllocator<_Alloc>>
#endif
basic_string(size_type __n, _CharT __c, const _Alloc& __a = _Alloc())
: _M_dataplus(_M_local_data(), __a)
{ _M_construct(__n, __c); }
#if __cplusplus >= 201103L
basic_string(basic_string&& __str) noexcept
: _M_dataplus(_M_local_data(), std::move(__str._M_get_allocator()))
{
if (__str._M_is_local())
{
traits_type::copy(_M_local_buf, __str._M_local_buf,
_S_local_capacity + 1);
}
else
{
_M_data(__str._M_data());
_M_capacity(__str._M_allocated_capacity);
}
_M_length(__str.length());
__str._M_data(__str._M_local_data());
__str._M_set_length(0);
}
basic_string(initializer_list<_CharT> __l, const _Alloc& __a = _Alloc())
: _M_dataplus(_M_local_data(), __a)
{ _M_construct(__l.begin(), __l.end()); }
basic_string(const basic_string& __str, const _Alloc& __a)
: _M_dataplus(_M_local_data(), __a)
{ _M_construct(__str.begin(), __str.end()); }
basic_string(basic_string&& __str, const _Alloc& __a)
noexcept(_Alloc_traits::_S_always_equal())
: _M_dataplus(_M_local_data(), __a)
{
if (__str._M_is_local())
{
traits_type::copy(_M_local_buf, __str._M_local_buf,
_S_local_capacity + 1);
_M_length(__str.length());
__str._M_set_length(0);
}
else if (_Alloc_traits::_S_always_equal()
|| __str.get_allocator() == __a)
{
_M_data(__str._M_data());
_M_length(__str.length());
_M_capacity(__str._M_allocated_capacity);
__str._M_data(__str._M_local_buf);
__str._M_set_length(0);
}
else
_M_construct(__str.begin(), __str.end());
}
#endif
#if __cplusplus >= 201103L
template<typename _InputIterator,
typename = std::_RequireInputIter<_InputIterator>>
#else
template<typename _InputIterator>
#endif
basic_string(_InputIterator __beg, _InputIterator __end,
const _Alloc& __a = _Alloc())
: _M_dataplus(_M_local_data(), __a)
{ _M_construct(__beg, __end); }
#if __cplusplus >= 201703L
template<typename _Tp,
typename = enable_if_t<is_convertible_v<const _Tp&, __sv_type>>>
basic_string(const _Tp& __t, size_type __pos, size_type __n,
const _Alloc& __a = _Alloc())
: basic_string(_S_to_string_view(__t).substr(__pos, __n), __a) { }
template<typename _Tp, typename = _If_sv<_Tp, void>>
explicit
basic_string(const _Tp& __t, const _Alloc& __a = _Alloc())
: basic_string(__sv_wrapper(_S_to_string_view(__t)), __a) { }
#endif
~basic_string()
{ _M_dispose(); }
basic_string&
operator=(const basic_string& __str)
{
return this->assign(__str);
}
basic_string&
operator=(const _CharT* __s)
{ return this->assign(__s); }
basic_string&
operator=(_CharT __c)
{
this->assign(1, __c);
return *this;
}
#if __cplusplus >= 201103L
basic_string&
operator=(basic_string&& __str)
noexcept(_Alloc_traits::_S_nothrow_move())
{
if (!_M_is_local() && _Alloc_traits::_S_propagate_on_move_assign()
&& !_Alloc_traits::_S_always_equal()
&& _M_get_allocator() != __str._M_get_allocator())
{
_M_destroy(_M_allocated_capacity);
_M_data(_M_local_data());
_M_set_length(0);
}
std::__alloc_on_move(_M_get_allocator(), __str._M_get_allocator());
if (__str._M_is_local())
{
if (__builtin_expect(std::__addressof(__str) != this, true))
{
if (__str.size())
this->_S_copy(_M_data(), __str._M_data(), __str.size());
_M_set_length(__str.size());
}
}
else if (_Alloc_traits::_S_propagate_on_move_assign()
|| _Alloc_traits::_S_always_equal()
|| _M_get_allocator() == __str._M_get_allocator())
{
pointer __data = nullptr;
size_type __capacity;
if (!_M_is_local())
{
if (_Alloc_traits::_S_always_equal())
{
__data = _M_data();
__capacity = _M_allocated_capacity;
}
else
_M_destroy(_M_allocated_capacity);
}
_M_data(__str._M_data());
_M_length(__str.length());
_M_capacity(__str._M_allocated_capacity);
if (__data)
{
__str._M_data(__data);
__str._M_capacity(__capacity);
}
else
__str._M_data(__str._M_local_buf);
}
else
assign(__str);
__str.clear();
return *this;
}
basic_string&
operator=(initializer_list<_CharT> __l)
{
this->assign(__l.begin(), __l.size());
return *this;
}
#endif
#if __cplusplus >= 201703L
template<typename _Tp>
_If_sv<_Tp, basic_string&>
operator=(const _Tp& __svt)
{ return this->assign(__svt); }
operator __sv_type() const noexcept
{ return __sv_type(data(), size()); }
#endif
iterator
begin() _GLIBCXX_NOEXCEPT
{ return iterator(_M_data()); }
const_iterator
begin() const _GLIBCXX_NOEXCEPT
{ return const_iterator(_M_data()); }
iterator
end() _GLIBCXX_NOEXCEPT
{ return iterator(_M_data() + this->size()); }
const_iterator
end() const _GLIBCXX_NOEXCEPT
{ return const_iterator(_M_data() + this->size()); }
reverse_iterator
rbegin() _GLIBCXX_NOEXCEPT
{ return reverse_iterator(this->end()); }
const_reverse_iterator
rbegin() const _GLIBCXX_NOEXCEPT
{ return const_reverse_iterator(this->end()); }
reverse_iterator
rend() _GLIBCXX_NOEXCEPT
{ return reverse_iterator(this->begin()); }
const_reverse_iterator
rend() const _GLIBCXX_NOEXCEPT
{ return const_reverse_iterator(this->begin()); }
#if __cplusplus >= 201103L
const_iterator
cbegin() const noexcept
{ return const_iterator(this->_M_data()); }
const_iterator
cend() const noexcept
{ return const_iterator(this->_M_data() + this->size()); }
const_reverse_iterator
crbegin() const noexcept
{ return const_reverse_iterator(this->end()); }
const_reverse_iterator
crend() const noexcept
{ return const_reverse_iterator(this->begin()); }
#endif
public:
size_type
size() const _GLIBCXX_NOEXCEPT
{ return _M_string_length; }
size_type
length() const _GLIBCXX_NOEXCEPT
{ return _M_string_length; }
size_type
max_size() const _GLIBCXX_NOEXCEPT
{ return (_Alloc_traits::max_size(_M_get_allocator()) - 1) / 2; }
void
resize(size_type __n, _CharT __c);
void
resize(size_type __n)
{ this->resize(__n, _CharT()); }
#if __cplusplus >= 201103L
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"
void
shrink_to_fit() noexcept
{ reserve(); }
#pragma GCC diagnostic pop
#endif
size_type
capacity() const _GLIBCXX_NOEXCEPT
{
return _M_is_local() ? size_type(_S_local_capacity)
: _M_allocated_capacity;
}
void
reserve(size_type __res_arg);
#if __cplusplus > 201703L
[[deprecated("use shrink_to_fit() instead")]]
#endif
void
reserve();
void
clear() _GLIBCXX_NOEXCEPT
{ _M_set_length(0); }
_GLIBCXX_NODISCARD bool
empty() const _GLIBCXX_NOEXCEPT
{ return this->size() == 0; }
const_reference
operator[] (size_type __pos) const _GLIBCXX_NOEXCEPT
{
__glibcxx_assert(__pos <= size());
return _M_data()[__pos];
}
reference
operator[](size_type __pos)
{
__glibcxx_assert(__pos <= size());
_GLIBCXX_DEBUG_PEDASSERT(__cplusplus >= 201103L || __pos < size());
return _M_data()[__pos];
}
const_reference
at(size_type __n) const
{
if (__n >= this->size())
__throw_out_of_range_fmt(__N("basic_string::at: __n "
"(which is %zu) >= this->size() "
"(which is %zu)"),
__n, this->size());
return _M_data()[__n];
}
reference
at(size_type __n)
{
if (__n >= size())
__throw_out_of_range_fmt(__N("basic_string::at: __n "
"(which is %zu) >= this->size() "
"(which is %zu)"),
__n, this->size());
return _M_data()[__n];
}
#if __cplusplus >= 201103L
reference
front() noexcept
{
__glibcxx_assert(!empty());
return operator[](0);
}
const_reference
front() const noexcept
{
__glibcxx_assert(!empty());
return operator[](0);
}
reference
back() noexcept
{
__glibcxx_assert(!empty());
return operator[](this->size() - 1);
}
const_reference
back() const noexcept
{
__glibcxx_assert(!empty());
return operator[](this->size() - 1);
}
#endif
basic_string&
operator+=(const basic_string& __str)
{ return this->append(__str); }
basic_string&
operator+=(const _CharT* __s)
{ return this->append(__s); }
basic_string&
operator+=(_CharT __c)
{
this->push_back(__c);
return *this;
}
#if __cplusplus >= 201103L
basic_string&
operator+=(initializer_list<_CharT> __l)
{ return this->append(__l.begin(), __l.size()); }
#endif
#if __cplusplus >= 201703L
template<typename _Tp>
_If_sv<_Tp, basic_string&>
operator+=(const _Tp& __svt)
{ return this->append(__svt); }
#endif
basic_string&
append(const basic_string& __str)
{ return _M_append(__str._M_data(), __str.size()); }
basic_string&
append(const basic_string& __str, size_type __pos, size_type __n = npos)
{ return _M_append(__str._M_data()
+ __str._M_check(__pos, "basic_string::append"),
__str._M_limit(__pos, __n)); }
basic_string&
append(const _CharT* __s, size_type __n)
{
__glibcxx_requires_string_len(__s, __n);
_M_check_length(size_type(0), __n, "basic_string::append");
return _M_append(__s, __n);
}
basic_string&
append(const _CharT* __s)
{
__glibcxx_requires_string(__s);
const size_type __n = traits_type::length(__s);
_M_check_length(size_type(0), __n, "basic_string::append");
return _M_append(__s, __n);
}
basic_string&
append(size_type __n, _CharT __c)
{ return _M_replace_aux(this->size(), size_type(0), __n, __c); }
#if __cplusplus >= 201103L
basic_string&
append(initializer_list<_CharT> __l)
{ return this->append(__l.begin(), __l.size()); }
#endif
#if __cplusplus >= 201103L
template<class _InputIterator,
typename = std::_RequireInputIter<_InputIterator>>
#else
template<class _InputIterator>
#endif
basic_string&
append(_InputIterator __first, _InputIterator __last)
{ return this->replace(end(), end(), __first, __last); }
#if __cplusplus >= 201703L
template<typename _Tp>
_If_sv<_Tp, basic_string&>
append(const _Tp& __svt)
{
__sv_type __sv = __svt;
return this->append(__sv.data(), __sv.size());
}
template<typename _Tp>
_If_sv<_Tp, basic_string&>
append(const _Tp& __svt, size_type __pos, size_type __n = npos)
{
__sv_type __sv = __svt;
return _M_append(__sv.data()
+ std::__sv_check(__sv.size(), __pos, "basic_string::append"),
std::__sv_limit(__sv.size(), __pos, __n));
}
#endif
void
push_back(_CharT __c)
{
const size_type __size = this->size();
if (__size + 1 > this->capacity())
this->_M_mutate(__size, size_type(0), 0, size_type(1));
traits_type::assign(this->_M_data()[__size], __c);
this->_M_set_length(__size + 1);
}
basic_string&
assign(const basic_string& __str)
{
#if __cplusplus >= 201103L
if (_Alloc_traits::_S_propagate_on_copy_assign())
{
if (!_Alloc_traits::_S_always_equal() && !_M_is_local()
&& _M_get_allocator() != __str._M_get_allocator())
{
if (__str.size() <= _S_local_capacity)
{
_M_destroy(_M_allocated_capacity);
_M_data(_M_local_data());
_M_set_length(0);
}
else
{
const auto __len = __str.size();
auto __alloc = __str._M_get_allocator();
auto __ptr = _Alloc_traits::allocate(__alloc, __len + 1);
_M_destroy(_M_allocated_capacity);
_M_data(__ptr);
_M_capacity(__len);
_M_set_length(__len);
}
}
std::__alloc_on_copy(_M_get_allocator(), __str._M_get_allocator());
}
#endif
this->_M_assign(__str);
return *this;
}
#if __cplusplus >= 201103L
basic_string&
assign(basic_string&& __str)
noexcept(_Alloc_traits::_S_nothrow_move())
{
return *this = std::move(__str);
}
#endif
basic_string&
assign(const basic_string& __str, size_type __pos, size_type __n = npos)
{ return _M_replace(size_type(0), this->size(), __str._M_data()
+ __str._M_check(__pos, "basic_string::assign"),
__str._M_limit(__pos, __n)); }
basic_string&
assign(const _CharT* __s, size_type __n)
{
__glibcxx_requires_string_len(__s, __n);
return _M_replace(size_type(0), this->size(), __s, __n);
}
basic_string&
assign(const _CharT* __s)
{
__glibcxx_requires_string(__s);
return _M_replace(size_type(0), this->size(), __s,
traits_type::length(__s));
}
basic_string&
assign(size_type __n, _CharT __c)
{ return _M_replace_aux(size_type(0), this->size(), __n, __c); }
#if __cplusplus >= 201103L
template<class _InputIterator,
typename = std::_RequireInputIter<_InputIterator>>
#else
template<class _InputIterator>
#endif
basic_string&
assign(_InputIterator __first, _InputIterator __last)
{ return this->replace(begin(), end(), __first, __last); }
#if __cplusplus >= 201103L
basic_string&
assign(initializer_list<_CharT> __l)
{ return this->assign(__l.begin(), __l.size()); }
#endif
#if __cplusplus >= 201703L
template<typename _Tp>
_If_sv<_Tp, basic_string&>
assign(const _Tp& __svt)
{
__sv_type __sv = __svt;
return this->assign(__sv.data(), __sv.size());
}
template<typename _Tp>
_If_sv<_Tp, basic_string&>
assign(const _Tp& __svt, size_type __pos, size_type __n = npos)
{
__sv_type __sv = __svt;
return _M_replace(size_type(0), this->size(),
__sv.data()
+ std::__sv_check(__sv.size(), __pos, "basic_string::assign"),
std::__sv_limit(__sv.size(), __pos, __n));
}
#endif
#if __cplusplus >= 201103L
iterator
insert(const_iterator __p, size_type __n, _CharT __c)
{
_GLIBCXX_DEBUG_PEDASSERT(__p >= begin() && __p <= end());
const size_type __pos = __p - begin();
this->replace(__p, __p, __n, __c);
return iterator(this->_M_data() + __pos);
}
#else
void
insert(iterator __p, size_type __n, _CharT __c)
{	this->replace(__p, __p, __n, __c);  }
#endif
#if __cplusplus >= 201103L
template<class _InputIterator,
typename = std::_RequireInputIter<_InputIterator>>
iterator
insert(const_iterator __p, _InputIterator __beg, _InputIterator __end)
{
_GLIBCXX_DEBUG_PEDASSERT(__p >= begin() && __p <= end());
const size_type __pos = __p - begin();
this->replace(__p, __p, __beg, __end);
return iterator(this->_M_data() + __pos);
}
#else
template<class _InputIterator>
void
insert(iterator __p, _InputIterator __beg, _InputIterator __end)
{ this->replace(__p, __p, __beg, __end); }
#endif
#if __cplusplus >= 201103L
iterator
insert(const_iterator __p, initializer_list<_CharT> __l)
{ return this->insert(__p, __l.begin(), __l.end()); }
#ifdef _GLIBCXX_DEFINING_STRING_INSTANTIATIONS
void
insert(iterator __p, initializer_list<_CharT> __l)
{
_GLIBCXX_DEBUG_PEDASSERT(__p >= begin() && __p <= end());
this->insert(__p - begin(), __l.begin(), __l.size());
}
#endif
#endif
basic_string&
insert(size_type __pos1, const basic_string& __str)
{ return this->replace(__pos1, size_type(0),
__str._M_data(), __str.size()); }
basic_string&
insert(size_type __pos1, const basic_string& __str,
size_type __pos2, size_type __n = npos)
{ return this->replace(__pos1, size_type(0), __str._M_data()
+ __str._M_check(__pos2, "basic_string::insert"),
__str._M_limit(__pos2, __n)); }
basic_string&
insert(size_type __pos, const _CharT* __s, size_type __n)
{ return this->replace(__pos, size_type(0), __s, __n); }
basic_string&
insert(size_type __pos, const _CharT* __s)
{
__glibcxx_requires_string(__s);
return this->replace(__pos, size_type(0), __s,
traits_type::length(__s));
}
basic_string&
insert(size_type __pos, size_type __n, _CharT __c)
{ return _M_replace_aux(_M_check(__pos, "basic_string::insert"),
size_type(0), __n, __c); }
iterator
insert(__const_iterator __p, _CharT __c)
{
_GLIBCXX_DEBUG_PEDASSERT(__p >= begin() && __p <= end());
const size_type __pos = __p - begin();
_M_replace_aux(__pos, size_type(0), size_type(1), __c);
return iterator(_M_data() + __pos);
}
#if __cplusplus >= 201703L
template<typename _Tp>
_If_sv<_Tp, basic_string&>
insert(size_type __pos, const _Tp& __svt)
{
__sv_type __sv = __svt;
return this->insert(__pos, __sv.data(), __sv.size());
}
template<typename _Tp>
_If_sv<_Tp, basic_string&>
insert(size_type __pos1, const _Tp& __svt,
size_type __pos2, size_type __n = npos)
{
__sv_type __sv = __svt;
return this->replace(__pos1, size_type(0),
__sv.data()
+ std::__sv_check(__sv.size(), __pos2, "basic_string::insert"),
std::__sv_limit(__sv.size(), __pos2, __n));
}
#endif
basic_string&
erase(size_type __pos = 0, size_type __n = npos)
{
_M_check(__pos, "basic_string::erase");
if (__n == npos)
this->_M_set_length(__pos);
else if (__n != 0)
this->_M_erase(__pos, _M_limit(__pos, __n));
return *this;
}
iterator
erase(__const_iterator __position)
{
_GLIBCXX_DEBUG_PEDASSERT(__position >= begin()
&& __position < end());
const size_type __pos = __position - begin();
this->_M_erase(__pos, size_type(1));
return iterator(_M_data() + __pos);
}
iterator
erase(__const_iterator __first, __const_iterator __last)
{
_GLIBCXX_DEBUG_PEDASSERT(__first >= begin() && __first <= __last
&& __last <= end());
const size_type __pos = __first - begin();
if (__last == end())
this->_M_set_length(__pos);
else
this->_M_erase(__pos, __last - __first);
return iterator(this->_M_data() + __pos);
}
#if __cplusplus >= 201103L
void
pop_back() noexcept
{
__glibcxx_assert(!empty());
_M_erase(size() - 1, 1);
}
#endif
basic_string&
replace(size_type __pos, size_type __n, const basic_string& __str)
{ return this->replace(__pos, __n, __str._M_data(), __str.size()); }
basic_string&
replace(size_type __pos1, size_type __n1, const basic_string& __str,
size_type __pos2, size_type __n2 = npos)
{ return this->replace(__pos1, __n1, __str._M_data()
+ __str._M_check(__pos2, "basic_string::replace"),
__str._M_limit(__pos2, __n2)); }
basic_string&
replace(size_type __pos, size_type __n1, const _CharT* __s,
size_type __n2)
{
__glibcxx_requires_string_len(__s, __n2);
return _M_replace(_M_check(__pos, "basic_string::replace"),
_M_limit(__pos, __n1), __s, __n2);
}
basic_string&
replace(size_type __pos, size_type __n1, const _CharT* __s)
{
__glibcxx_requires_string(__s);
return this->replace(__pos, __n1, __s, traits_type::length(__s));
}
basic_string&
replace(size_type __pos, size_type __n1, size_type __n2, _CharT __c)
{ return _M_replace_aux(_M_check(__pos, "basic_string::replace"),
_M_limit(__pos, __n1), __n2, __c); }
basic_string&
replace(__const_iterator __i1, __const_iterator __i2,
const basic_string& __str)
{ return this->replace(__i1, __i2, __str._M_data(), __str.size()); }
basic_string&
replace(__const_iterator __i1, __const_iterator __i2,
const _CharT* __s, size_type __n)
{
_GLIBCXX_DEBUG_PEDASSERT(begin() <= __i1 && __i1 <= __i2
&& __i2 <= end());
return this->replace(__i1 - begin(), __i2 - __i1, __s, __n);
}
basic_string&
replace(__const_iterator __i1, __const_iterator __i2, const _CharT* __s)
{
__glibcxx_requires_string(__s);
return this->replace(__i1, __i2, __s, traits_type::length(__s));
}
basic_string&
replace(__const_iterator __i1, __const_iterator __i2, size_type __n,
_CharT __c)
{
_GLIBCXX_DEBUG_PEDASSERT(begin() <= __i1 && __i1 <= __i2
&& __i2 <= end());
return _M_replace_aux(__i1 - begin(), __i2 - __i1, __n, __c);
}
#if __cplusplus >= 201103L
template<class _InputIterator,
typename = std::_RequireInputIter<_InputIterator>>
basic_string&
replace(const_iterator __i1, const_iterator __i2,
_InputIterator __k1, _InputIterator __k2)
{
_GLIBCXX_DEBUG_PEDASSERT(begin() <= __i1 && __i1 <= __i2
&& __i2 <= end());
__glibcxx_requires_valid_range(__k1, __k2);
return this->_M_replace_dispatch(__i1, __i2, __k1, __k2,
std::__false_type());
}
#else
template<class _InputIterator>
#ifdef _GLIBCXX_DISAMBIGUATE_REPLACE_INST
typename __enable_if_not_native_iterator<_InputIterator>::__type
#else
basic_string&
#endif
replace(iterator __i1, iterator __i2,
_InputIterator __k1, _InputIterator __k2)
{
_GLIBCXX_DEBUG_PEDASSERT(begin() <= __i1 && __i1 <= __i2
&& __i2 <= end());
__glibcxx_requires_valid_range(__k1, __k2);
typedef typename std::__is_integer<_InputIterator>::__type _Integral;
return _M_replace_dispatch(__i1, __i2, __k1, __k2, _Integral());
}
#endif
basic_string&
replace(__const_iterator __i1, __const_iterator __i2,
_CharT* __k1, _CharT* __k2)
{
_GLIBCXX_DEBUG_PEDASSERT(begin() <= __i1 && __i1 <= __i2
&& __i2 <= end());
__glibcxx_requires_valid_range(__k1, __k2);
return this->replace(__i1 - begin(), __i2 - __i1,
__k1, __k2 - __k1);
}
basic_string&
replace(__const_iterator __i1, __const_iterator __i2,
const _CharT* __k1, const _CharT* __k2)
{
_GLIBCXX_DEBUG_PEDASSERT(begin() <= __i1 && __i1 <= __i2
&& __i2 <= end());
__glibcxx_requires_valid_range(__k1, __k2);
return this->replace(__i1 - begin(), __i2 - __i1,
__k1, __k2 - __k1);
}
basic_string&
replace(__const_iterator __i1, __const_iterator __i2,
iterator __k1, iterator __k2)
{
_GLIBCXX_DEBUG_PEDASSERT(begin() <= __i1 && __i1 <= __i2
&& __i2 <= end());
__glibcxx_requires_valid_range(__k1, __k2);
return this->replace(__i1 - begin(), __i2 - __i1,
__k1.base(), __k2 - __k1);
}
basic_string&
replace(__const_iterator __i1, __const_iterator __i2,
const_iterator __k1, const_iterator __k2)
{
_GLIBCXX_DEBUG_PEDASSERT(begin() <= __i1 && __i1 <= __i2
&& __i2 <= end());
__glibcxx_requires_valid_range(__k1, __k2);
return this->replace(__i1 - begin(), __i2 - __i1,
__k1.base(), __k2 - __k1);
}
#if __cplusplus >= 201103L
basic_string& replace(const_iterator __i1, const_iterator __i2,
initializer_list<_CharT> __l)
{ return this->replace(__i1, __i2, __l.begin(), __l.size()); }
#endif
#if __cplusplus >= 201703L
template<typename _Tp>
_If_sv<_Tp, basic_string&>
replace(size_type __pos, size_type __n, const _Tp& __svt)
{
__sv_type __sv = __svt;
return this->replace(__pos, __n, __sv.data(), __sv.size());
}
template<typename _Tp>
_If_sv<_Tp, basic_string&>
replace(size_type __pos1, size_type __n1, const _Tp& __svt,
size_type __pos2, size_type __n2 = npos)
{
__sv_type __sv = __svt;
return this->replace(__pos1, __n1,
__sv.data()
+ std::__sv_check(__sv.size(), __pos2, "basic_string::replace"),
std::__sv_limit(__sv.size(), __pos2, __n2));
}
template<typename _Tp>
_If_sv<_Tp, basic_string&>
replace(const_iterator __i1, const_iterator __i2, const _Tp& __svt)
{
__sv_type __sv = __svt;
return this->replace(__i1 - begin(), __i2 - __i1, __sv);
}
#endif
private:
template<class _Integer>
basic_string&
_M_replace_dispatch(const_iterator __i1, const_iterator __i2,
_Integer __n, _Integer __val, __true_type)
{ return _M_replace_aux(__i1 - begin(), __i2 - __i1, __n, __val); }
template<class _InputIterator>
basic_string&
_M_replace_dispatch(const_iterator __i1, const_iterator __i2,
_InputIterator __k1, _InputIterator __k2,
__false_type);
basic_string&
_M_replace_aux(size_type __pos1, size_type __n1, size_type __n2,
_CharT __c);
basic_string&
_M_replace(size_type __pos, size_type __len1, const _CharT* __s,
const size_type __len2);
basic_string&
_M_append(const _CharT* __s, size_type __n);
public:
size_type
copy(_CharT* __s, size_type __n, size_type __pos = 0) const;
void
swap(basic_string& __s) _GLIBCXX_NOEXCEPT;
const _CharT*
c_str() const _GLIBCXX_NOEXCEPT
{ return _M_data(); }
const _CharT*
data() const _GLIBCXX_NOEXCEPT
{ return _M_data(); }
#if __cplusplus >= 201703L
_CharT*
data() noexcept
{ return _M_data(); }
#endif
allocator_type
get_allocator() const _GLIBCXX_NOEXCEPT
{ return _M_get_allocator(); }
size_type
find(const _CharT* __s, size_type __pos, size_type __n) const
_GLIBCXX_NOEXCEPT;
size_type
find(const basic_string& __str, size_type __pos = 0) const
_GLIBCXX_NOEXCEPT
{ return this->find(__str.data(), __pos, __str.size()); }
#if __cplusplus >= 201703L
template<typename _Tp>
_If_sv<_Tp, size_type>
find(const _Tp& __svt, size_type __pos = 0) const
noexcept(is_same<_Tp, __sv_type>::value)
{
__sv_type __sv = __svt;
return this->find(__sv.data(), __pos, __sv.size());
}
#endif
size_type
find(const _CharT* __s, size_type __pos = 0) const _GLIBCXX_NOEXCEPT
{
__glibcxx_requires_string(__s);
return this->find(__s, __pos, traits_type::length(__s));
}
size_type
find(_CharT __c, size_type __pos = 0) const _GLIBCXX_NOEXCEPT;
size_type
rfind(const basic_string& __str, size_type __pos = npos) const
_GLIBCXX_NOEXCEPT
{ return this->rfind(__str.data(), __pos, __str.size()); }
#if __cplusplus >= 201703L
template<typename _Tp>
_If_sv<_Tp, size_type>
rfind(const _Tp& __svt, size_type __pos = npos) const
noexcept(is_same<_Tp, __sv_type>::value)
{
__sv_type __sv = __svt;
return this->rfind(__sv.data(), __pos, __sv.size());
}
#endif
size_type
rfind(const _CharT* __s, size_type __pos, size_type __n) const
_GLIBCXX_NOEXCEPT;
size_type
rfind(const _CharT* __s, size_type __pos = npos) const
{
__glibcxx_requires_string(__s);
return this->rfind(__s, __pos, traits_type::length(__s));
}
size_type
rfind(_CharT __c, size_type __pos = npos) const _GLIBCXX_NOEXCEPT;
size_type
find_first_of(const basic_string& __str, size_type __pos = 0) const
_GLIBCXX_NOEXCEPT
{ return this->find_first_of(__str.data(), __pos, __str.size()); }
#if __cplusplus >= 201703L
template<typename _Tp>
_If_sv<_Tp, size_type>
find_first_of(const _Tp& __svt, size_type __pos = 0) const
noexcept(is_same<_Tp, __sv_type>::value)
{
__sv_type __sv = __svt;
return this->find_first_of(__sv.data(), __pos, __sv.size());
}
#endif
size_type
find_first_of(const _CharT* __s, size_type __pos, size_type __n) const
_GLIBCXX_NOEXCEPT;
size_type
find_first_of(const _CharT* __s, size_type __pos = 0) const
_GLIBCXX_NOEXCEPT
{
__glibcxx_requires_string(__s);
return this->find_first_of(__s, __pos, traits_type::length(__s));
}
size_type
find_first_of(_CharT __c, size_type __pos = 0) const _GLIBCXX_NOEXCEPT
{ return this->find(__c, __pos); }
size_type
find_last_of(const basic_string& __str, size_type __pos = npos) const
_GLIBCXX_NOEXCEPT
{ return this->find_last_of(__str.data(), __pos, __str.size()); }
#if __cplusplus >= 201703L
template<typename _Tp>
_If_sv<_Tp, size_type>
find_last_of(const _Tp& __svt, size_type __pos = npos) const
noexcept(is_same<_Tp, __sv_type>::value)
{
__sv_type __sv = __svt;
return this->find_last_of(__sv.data(), __pos, __sv.size());
}
#endif
size_type
find_last_of(const _CharT* __s, size_type __pos, size_type __n) const
_GLIBCXX_NOEXCEPT;
size_type
find_last_of(const _CharT* __s, size_type __pos = npos) const
_GLIBCXX_NOEXCEPT
{
__glibcxx_requires_string(__s);
return this->find_last_of(__s, __pos, traits_type::length(__s));
}
size_type
find_last_of(_CharT __c, size_type __pos = npos) const _GLIBCXX_NOEXCEPT
{ return this->rfind(__c, __pos); }
size_type
find_first_not_of(const basic_string& __str, size_type __pos = 0) const
_GLIBCXX_NOEXCEPT
{ return this->find_first_not_of(__str.data(), __pos, __str.size()); }
#if __cplusplus >= 201703L
template<typename _Tp>
_If_sv<_Tp, size_type>
find_first_not_of(const _Tp& __svt, size_type __pos = 0) const
noexcept(is_same<_Tp, __sv_type>::value)
{
__sv_type __sv = __svt;
return this->find_first_not_of(__sv.data(), __pos, __sv.size());
}
#endif
size_type
find_first_not_of(const _CharT* __s, size_type __pos,
size_type __n) const _GLIBCXX_NOEXCEPT;
size_type
find_first_not_of(const _CharT* __s, size_type __pos = 0) const
_GLIBCXX_NOEXCEPT
{
__glibcxx_requires_string(__s);
return this->find_first_not_of(__s, __pos, traits_type::length(__s));
}
size_type
find_first_not_of(_CharT __c, size_type __pos = 0) const
_GLIBCXX_NOEXCEPT;
size_type
find_last_not_of(const basic_string& __str, size_type __pos = npos) const
_GLIBCXX_NOEXCEPT
{ return this->find_last_not_of(__str.data(), __pos, __str.size()); }
#if __cplusplus >= 201703L
template<typename _Tp>
_If_sv<_Tp, size_type>
find_last_not_of(const _Tp& __svt, size_type __pos = npos) const
noexcept(is_same<_Tp, __sv_type>::value)
{
__sv_type __sv = __svt;
return this->find_last_not_of(__sv.data(), __pos, __sv.size());
}
#endif
size_type
find_last_not_of(const _CharT* __s, size_type __pos,
size_type __n) const _GLIBCXX_NOEXCEPT;
size_type
find_last_not_of(const _CharT* __s, size_type __pos = npos) const
_GLIBCXX_NOEXCEPT
{
__glibcxx_requires_string(__s);
return this->find_last_not_of(__s, __pos, traits_type::length(__s));
}
size_type
find_last_not_of(_CharT __c, size_type __pos = npos) const
_GLIBCXX_NOEXCEPT;
basic_string
substr(size_type __pos = 0, size_type __n = npos) const
{ return basic_string(*this,
_M_check(__pos, "basic_string::substr"), __n); }
int
compare(const basic_string& __str) const
{
const size_type __size = this->size();
const size_type __osize = __str.size();
const size_type __len = std::min(__size, __osize);
int __r = traits_type::compare(_M_data(), __str.data(), __len);
if (!__r)
__r = _S_compare(__size, __osize);
return __r;
}
#if __cplusplus >= 201703L
template<typename _Tp>
_If_sv<_Tp, int>
compare(const _Tp& __svt) const
noexcept(is_same<_Tp, __sv_type>::value)
{
__sv_type __sv = __svt;
const size_type __size = this->size();
const size_type __osize = __sv.size();
const size_type __len = std::min(__size, __osize);
int __r = traits_type::compare(_M_data(), __sv.data(), __len);
if (!__r)
__r = _S_compare(__size, __osize);
return __r;
}
template<typename _Tp>
_If_sv<_Tp, int>
compare(size_type __pos, size_type __n, const _Tp& __svt) const
noexcept(is_same<_Tp, __sv_type>::value)
{
__sv_type __sv = __svt;
return __sv_type(*this).substr(__pos, __n).compare(__sv);
}
template<typename _Tp>
_If_sv<_Tp, int>
compare(size_type __pos1, size_type __n1, const _Tp& __svt,
size_type __pos2, size_type __n2 = npos) const
noexcept(is_same<_Tp, __sv_type>::value)
{
__sv_type __sv = __svt;
return __sv_type(*this)
.substr(__pos1, __n1).compare(__sv.substr(__pos2, __n2));
}
#endif
int
compare(size_type __pos, size_type __n, const basic_string& __str) const;
int
compare(size_type __pos1, size_type __n1, const basic_string& __str,
size_type __pos2, size_type __n2 = npos) const;
int
compare(const _CharT* __s) const _GLIBCXX_NOEXCEPT;
int
compare(size_type __pos, size_type __n1, const _CharT* __s) const;
int
compare(size_type __pos, size_type __n1, const _CharT* __s,
size_type __n2) const;
#if __cplusplus > 201703L
bool
starts_with(basic_string_view<_CharT, _Traits> __x) const noexcept
{ return __sv_type(this->data(), this->size()).starts_with(__x); }
bool
starts_with(_CharT __x) const noexcept
{ return __sv_type(this->data(), this->size()).starts_with(__x); }
bool
starts_with(const _CharT* __x) const noexcept
{ return __sv_type(this->data(), this->size()).starts_with(__x); }
bool
ends_with(basic_string_view<_CharT, _Traits> __x) const noexcept
{ return __sv_type(this->data(), this->size()).ends_with(__x); }
bool
ends_with(_CharT __x) const noexcept
{ return __sv_type(this->data(), this->size()).ends_with(__x); }
bool
ends_with(const _CharT* __x) const noexcept
{ return __sv_type(this->data(), this->size()).ends_with(__x); }
#endif
#if __cplusplus > 202002L
bool
contains(basic_string_view<_CharT, _Traits> __x) const noexcept
{ return __sv_type(this->data(), this->size()).contains(__x); }
bool
contains(_CharT __x) const noexcept
{ return __sv_type(this->data(), this->size()).contains(__x); }
bool
contains(const _CharT* __x) const noexcept
{ return __sv_type(this->data(), this->size()).contains(__x); }
#endif
template<typename, typename, typename> friend class basic_stringbuf;
};
_GLIBCXX_END_NAMESPACE_CXX11
#else
template<typename _CharT, typename _Traits, typename _Alloc>
class basic_string
{
typedef typename __gnu_cxx::__alloc_traits<_Alloc>::template
rebind<_CharT>::other _CharT_alloc_type;
typedef __gnu_cxx::__alloc_traits<_CharT_alloc_type> _CharT_alloc_traits;
public:
typedef _Traits					    traits_type;
typedef typename _Traits::char_type		    value_type;
typedef _Alloc					    allocator_type;
typedef typename _CharT_alloc_traits::size_type	    size_type;
typedef typename _CharT_alloc_traits::difference_type difference_type;
#if __cplusplus < 201103L
typedef typename _CharT_alloc_type::reference	    reference;
typedef typename _CharT_alloc_type::const_reference   const_reference;
#else
typedef value_type&				    reference;
typedef const value_type&				    const_reference;
#endif
typedef typename _CharT_alloc_traits::pointer	    pointer;
typedef typename _CharT_alloc_traits::const_pointer   const_pointer;
typedef __gnu_cxx::__normal_iterator<pointer, basic_string>  iterator;
typedef __gnu_cxx::__normal_iterator<const_pointer, basic_string>
const_iterator;
typedef std::reverse_iterator<const_iterator>	const_reverse_iterator;
typedef std::reverse_iterator<iterator>		    reverse_iterator;
protected:
typedef iterator __const_iterator;
private:
struct _Rep_base
{
size_type		_M_length;
size_type		_M_capacity;
_Atomic_word		_M_refcount;
};
struct _Rep : _Rep_base
{
typedef typename __gnu_cxx::__alloc_traits<_Alloc>::template
rebind<char>::other _Raw_bytes_alloc;
static const size_type	_S_max_size;
static const _CharT	_S_terminal;
static size_type _S_empty_rep_storage[];
static _Rep&
_S_empty_rep() _GLIBCXX_NOEXCEPT
{
void* __p = reinterpret_cast<void*>(&_S_empty_rep_storage);
return *reinterpret_cast<_Rep*>(__p);
}
bool
_M_is_leaked() const _GLIBCXX_NOEXCEPT
{
#if defined(__GTHREADS)
return __atomic_load_n(&this->_M_refcount, __ATOMIC_RELAXED) < 0;
#else
return this->_M_refcount < 0;
#endif
}
bool
_M_is_shared() const _GLIBCXX_NOEXCEPT
{
#if defined(__GTHREADS)
return __atomic_load_n(&this->_M_refcount, __ATOMIC_ACQUIRE) > 0;
#else
return this->_M_refcount > 0;
#endif
}
void
_M_set_leaked() _GLIBCXX_NOEXCEPT
{ this->_M_refcount = -1; }
void
_M_set_sharable() _GLIBCXX_NOEXCEPT
{ this->_M_refcount = 0; }
void
_M_set_length_and_sharable(size_type __n) _GLIBCXX_NOEXCEPT
{
#if _GLIBCXX_FULLY_DYNAMIC_STRING == 0
if (__builtin_expect(this != &_S_empty_rep(), false))
#endif
{
this->_M_set_sharable();
this->_M_length = __n;
traits_type::assign(this->_M_refdata()[__n], _S_terminal);
}
}
_CharT*
_M_refdata() throw()
{ return reinterpret_cast<_CharT*>(this + 1); }
_CharT*
_M_grab(const _Alloc& __alloc1, const _Alloc& __alloc2)
{
return (!_M_is_leaked() && __alloc1 == __alloc2)
? _M_refcopy() : _M_clone(__alloc1);
}
static _Rep*
_S_create(size_type, size_type, const _Alloc&);
void
_M_dispose(const _Alloc& __a) _GLIBCXX_NOEXCEPT
{
#if _GLIBCXX_FULLY_DYNAMIC_STRING == 0
if (__builtin_expect(this != &_S_empty_rep(), false))
#endif
{
_GLIBCXX_SYNCHRONIZATION_HAPPENS_BEFORE(&this->_M_refcount);
if (__gnu_cxx::__exchange_and_add_dispatch(&this->_M_refcount,
-1) <= 0)
{
_GLIBCXX_SYNCHRONIZATION_HAPPENS_AFTER(&this->_M_refcount);
_M_destroy(__a);
}
}
}
void
_M_destroy(const _Alloc&) throw();
_CharT*
_M_refcopy() throw()
{
#if _GLIBCXX_FULLY_DYNAMIC_STRING == 0
if (__builtin_expect(this != &_S_empty_rep(), false))
#endif
__gnu_cxx::__atomic_add_dispatch(&this->_M_refcount, 1);
return _M_refdata();
}
_CharT*
_M_clone(const _Alloc&, size_type __res = 0);
};
struct _Alloc_hider : _Alloc
{
_Alloc_hider(_CharT* __dat, const _Alloc& __a) _GLIBCXX_NOEXCEPT
: _Alloc(__a), _M_p(__dat) { }
_CharT* _M_p;
};
public:
static const size_type	npos = static_cast<size_type>(-1);
private:
mutable _Alloc_hider	_M_dataplus;
_CharT*
_M_data() const _GLIBCXX_NOEXCEPT
{ return  _M_dataplus._M_p; }
_CharT*
_M_data(_CharT* __p) _GLIBCXX_NOEXCEPT
{ return (_M_dataplus._M_p = __p); }
_Rep*
_M_rep() const _GLIBCXX_NOEXCEPT
{ return &((reinterpret_cast<_Rep*> (_M_data()))[-1]); }
iterator
_M_ibegin() const _GLIBCXX_NOEXCEPT
{ return iterator(_M_data()); }
iterator
_M_iend() const _GLIBCXX_NOEXCEPT
{ return iterator(_M_data() + this->size()); }
void
_M_leak()
{
if (!_M_rep()->_M_is_leaked())
_M_leak_hard();
}
size_type
_M_check(size_type __pos, const char* __s) const
{
if (__pos > this->size())
__throw_out_of_range_fmt(__N("%s: __pos (which is %zu) > "
"this->size() (which is %zu)"),
__s, __pos, this->size());
return __pos;
}
void
_M_check_length(size_type __n1, size_type __n2, const char* __s) const
{
if (this->max_size() - (this->size() - __n1) < __n2)
__throw_length_error(__N(__s));
}
size_type
_M_limit(size_type __pos, size_type __off) const _GLIBCXX_NOEXCEPT
{
const bool __testoff =  __off < this->size() - __pos;
return __testoff ? __off : this->size() - __pos;
}
bool
_M_disjunct(const _CharT* __s) const _GLIBCXX_NOEXCEPT
{
return (less<const _CharT*>()(__s, _M_data())
|| less<const _CharT*>()(_M_data() + this->size(), __s));
}
static void
_M_copy(_CharT* __d, const _CharT* __s, size_type __n) _GLIBCXX_NOEXCEPT
{
if (__n == 1)
traits_type::assign(*__d, *__s);
else
traits_type::copy(__d, __s, __n);
}
static void
_M_move(_CharT* __d, const _CharT* __s, size_type __n) _GLIBCXX_NOEXCEPT
{
if (__n == 1)
traits_type::assign(*__d, *__s);
else
traits_type::move(__d, __s, __n);
}
static void
_M_assign(_CharT* __d, size_type __n, _CharT __c) _GLIBCXX_NOEXCEPT
{
if (__n == 1)
traits_type::assign(*__d, __c);
else
traits_type::assign(__d, __n, __c);
}
template<class _Iterator>
static void
_S_copy_chars(_CharT* __p, _Iterator __k1, _Iterator __k2)
{
for (; __k1 != __k2; ++__k1, (void)++__p)
traits_type::assign(*__p, *__k1);
}
static void
_S_copy_chars(_CharT* __p, iterator __k1, iterator __k2) _GLIBCXX_NOEXCEPT
{ _S_copy_chars(__p, __k1.base(), __k2.base()); }
static void
_S_copy_chars(_CharT* __p, const_iterator __k1, const_iterator __k2)
_GLIBCXX_NOEXCEPT
{ _S_copy_chars(__p, __k1.base(), __k2.base()); }
static void
_S_copy_chars(_CharT* __p, _CharT* __k1, _CharT* __k2) _GLIBCXX_NOEXCEPT
{ _M_copy(__p, __k1, __k2 - __k1); }
static void
_S_copy_chars(_CharT* __p, const _CharT* __k1, const _CharT* __k2)
_GLIBCXX_NOEXCEPT
{ _M_copy(__p, __k1, __k2 - __k1); }
static int
_S_compare(size_type __n1, size_type __n2) _GLIBCXX_NOEXCEPT
{
const difference_type __d = difference_type(__n1 - __n2);
if (__d > __gnu_cxx::__numeric_traits<int>::__max)
return __gnu_cxx::__numeric_traits<int>::__max;
else if (__d < __gnu_cxx::__numeric_traits<int>::__min)
return __gnu_cxx::__numeric_traits<int>::__min;
else
return int(__d);
}
void
_M_mutate(size_type __pos, size_type __len1, size_type __len2);
void
_M_leak_hard();
static _Rep&
_S_empty_rep() _GLIBCXX_NOEXCEPT
{ return _Rep::_S_empty_rep(); }
#if __cplusplus >= 201703L
typedef basic_string_view<_CharT, _Traits> __sv_type;
template<typename _Tp, typename _Res>
using _If_sv = enable_if_t<
__and_<is_convertible<const _Tp&, __sv_type>,
__not_<is_convertible<const _Tp*, const basic_string*>>,
__not_<is_convertible<const _Tp&, const _CharT*>>>::value,
_Res>;
static __sv_type
_S_to_string_view(__sv_type __svt) noexcept
{ return __svt; }
struct __sv_wrapper
{
explicit __sv_wrapper(__sv_type __sv) noexcept : _M_sv(__sv) { }
__sv_type _M_sv;
};
explicit
basic_string(__sv_wrapper __svw, const _Alloc& __a)
: basic_string(__svw._M_sv.data(), __svw._M_sv.size(), __a) { }
#endif
public:
basic_string()
#if _GLIBCXX_FULLY_DYNAMIC_STRING == 0
_GLIBCXX_NOEXCEPT
: _M_dataplus(_S_empty_rep()._M_refdata(), _Alloc())
#else
: _M_dataplus(_S_construct(size_type(), _CharT(), _Alloc()), _Alloc())
#endif
{ }
explicit
basic_string(const _Alloc& __a)
: _M_dataplus(_S_construct(size_type(), _CharT(), __a), __a)
{ }
basic_string(const basic_string& __str)
: _M_dataplus(__str._M_rep()->_M_grab(_Alloc(__str.get_allocator()),
__str.get_allocator()),
__str.get_allocator())
{ }
basic_string(const basic_string& __str, size_type __pos,
const _Alloc& __a = _Alloc());
basic_string(const basic_string& __str, size_type __pos,
size_type __n);
basic_string(const basic_string& __str, size_type __pos,
size_type __n, const _Alloc& __a);
basic_string(const _CharT* __s, size_type __n,
const _Alloc& __a = _Alloc())
: _M_dataplus(_S_construct(__s, __s + __n, __a), __a)
{ }
#if __cpp_deduction_guides && ! defined _GLIBCXX_DEFINING_STRING_INSTANTIATIONS
template<typename = _RequireAllocator<_Alloc>>
#endif
basic_string(const _CharT* __s, const _Alloc& __a = _Alloc())
: _M_dataplus(_S_construct(__s, __s ? __s + traits_type::length(__s) :
__s + npos, __a), __a)
{ }
basic_string(size_type __n, _CharT __c, const _Alloc& __a = _Alloc())
: _M_dataplus(_S_construct(__n, __c, __a), __a)
{ }
#if __cplusplus >= 201103L
basic_string(basic_string&& __str)
#if _GLIBCXX_FULLY_DYNAMIC_STRING == 0
noexcept
#endif
: _M_dataplus(std::move(__str._M_dataplus))
{
#if _GLIBCXX_FULLY_DYNAMIC_STRING == 0
__str._M_data(_S_empty_rep()._M_refdata());
#else
__str._M_data(_S_construct(size_type(), _CharT(), get_allocator()));
#endif
}
basic_string(initializer_list<_CharT> __l, const _Alloc& __a = _Alloc())
: _M_dataplus(_S_construct(__l.begin(), __l.end(), __a), __a)
{ }
basic_string(const basic_string& __str, const _Alloc& __a)
: _M_dataplus(__str._M_rep()->_M_grab(__a, __str.get_allocator()), __a)
{ }
basic_string(basic_string&& __str, const _Alloc& __a)
: _M_dataplus(__str._M_data(), __a)
{
if (__a == __str.get_allocator())
{
#if _GLIBCXX_FULLY_DYNAMIC_STRING == 0
__str._M_data(_S_empty_rep()._M_refdata());
#else
__str._M_data(_S_construct(size_type(), _CharT(), __a));
#endif
}
else
_M_dataplus._M_p = _S_construct(__str.begin(), __str.end(), __a);
}
#endif
template<class _InputIterator>
basic_string(_InputIterator __beg, _InputIterator __end,
const _Alloc& __a = _Alloc())
: _M_dataplus(_S_construct(__beg, __end, __a), __a)
{ }
#if __cplusplus >= 201703L
template<typename _Tp,
typename = enable_if_t<is_convertible_v<const _Tp&, __sv_type>>>
basic_string(const _Tp& __t, size_type __pos, size_type __n,
const _Alloc& __a = _Alloc())
: basic_string(_S_to_string_view(__t).substr(__pos, __n), __a) { }
template<typename _Tp, typename = _If_sv<_Tp, void>>
explicit
basic_string(const _Tp& __t, const _Alloc& __a = _Alloc())
: basic_string(__sv_wrapper(_S_to_string_view(__t)), __a) { }
#endif
~basic_string() _GLIBCXX_NOEXCEPT
{ _M_rep()->_M_dispose(this->get_allocator()); }
basic_string&
operator=(const basic_string& __str)
{ return this->assign(__str); }
basic_string&
operator=(const _CharT* __s)
{ return this->assign(__s); }
basic_string&
operator=(_CharT __c)
{
this->assign(1, __c);
return *this;
}
#if __cplusplus >= 201103L
basic_string&
operator=(basic_string&& __str)
_GLIBCXX_NOEXCEPT_IF(allocator_traits<_Alloc>::is_always_equal::value)
{
this->swap(__str);
return *this;
}
basic_string&
operator=(initializer_list<_CharT> __l)
{
this->assign(__l.begin(), __l.size());
return *this;
}
#endif
#if __cplusplus >= 201703L
template<typename _Tp>
_If_sv<_Tp, basic_string&>
operator=(const _Tp& __svt)
{ return this->assign(__svt); }
operator __sv_type() const noexcept
{ return __sv_type(data(), size()); }
#endif
iterator
begin()
{
_M_leak();
return iterator(_M_data());
}
const_iterator
begin() const _GLIBCXX_NOEXCEPT
{ return const_iterator(_M_data()); }
iterator
end()
{
_M_leak();
return iterator(_M_data() + this->size());
}
const_iterator
end() const _GLIBCXX_NOEXCEPT
{ return const_iterator(_M_data() + this->size()); }
reverse_iterator
rbegin()
{ return reverse_iterator(this->end()); }
const_reverse_iterator
rbegin() const _GLIBCXX_NOEXCEPT
{ return const_reverse_iterator(this->end()); }
reverse_iterator
rend()
{ return reverse_iterator(this->begin()); }
const_reverse_iterator
rend() const _GLIBCXX_NOEXCEPT
{ return const_reverse_iterator(this->begin()); }
#if __cplusplus >= 201103L
const_iterator
cbegin() const noexcept
{ return const_iterator(this->_M_data()); }
const_iterator
cend() const noexcept
{ return const_iterator(this->_M_data() + this->size()); }
const_reverse_iterator
crbegin() const noexcept
{ return const_reverse_iterator(this->end()); }
const_reverse_iterator
crend() const noexcept
{ return const_reverse_iterator(this->begin()); }
#endif
public:
size_type
size() const _GLIBCXX_NOEXCEPT
{ return _M_rep()->_M_length; }
size_type
length() const _GLIBCXX_NOEXCEPT
{ return _M_rep()->_M_length; }
size_type
max_size() const _GLIBCXX_NOEXCEPT
{ return _Rep::_S_max_size; }
void
resize(size_type __n, _CharT __c);
void
resize(size_type __n)
{ this->resize(__n, _CharT()); }
#if __cplusplus >= 201103L
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"
void
shrink_to_fit() noexcept
{ reserve(); }
#pragma GCC diagnostic pop
#endif
size_type
capacity() const _GLIBCXX_NOEXCEPT
{ return _M_rep()->_M_capacity; }
void
reserve(size_type __res_arg);
#if __cplusplus > 201703L
[[deprecated("use shrink_to_fit() instead")]]
#endif
void
reserve();
#if _GLIBCXX_FULLY_DYNAMIC_STRING == 0
void
clear() _GLIBCXX_NOEXCEPT
{
if (_M_rep()->_M_is_shared())
{
_M_rep()->_M_dispose(this->get_allocator());
_M_data(_S_empty_rep()._M_refdata());
}
else
_M_rep()->_M_set_length_and_sharable(0);
}
#else
void
clear()
{ _M_mutate(0, this->size(), 0); }
#endif
_GLIBCXX_NODISCARD bool
empty() const _GLIBCXX_NOEXCEPT
{ return this->size() == 0; }
const_reference
operator[] (size_type __pos) const _GLIBCXX_NOEXCEPT
{
__glibcxx_assert(__pos <= size());
return _M_data()[__pos];
}
reference
operator[](size_type __pos)
{
__glibcxx_assert(__pos <= size());
_GLIBCXX_DEBUG_PEDASSERT(__cplusplus >= 201103L || __pos < size());
_M_leak();
return _M_data()[__pos];
}
const_reference
at(size_type __n) const
{
if (__n >= this->size())
__throw_out_of_range_fmt(__N("basic_string::at: __n "
"(which is %zu) >= this->size() "
"(which is %zu)"),
__n, this->size());
return _M_data()[__n];
}
reference
at(size_type __n)
{
if (__n >= size())
__throw_out_of_range_fmt(__N("basic_string::at: __n "
"(which is %zu) >= this->size() "
"(which is %zu)"),
__n, this->size());
_M_leak();
return _M_data()[__n];
}
#if __cplusplus >= 201103L
reference
front()
{
__glibcxx_assert(!empty());
return operator[](0);
}
const_reference
front() const noexcept
{
__glibcxx_assert(!empty());
return operator[](0);
}
reference
back()
{
__glibcxx_assert(!empty());
return operator[](this->size() - 1);
}
const_reference
back() const noexcept
{
__glibcxx_assert(!empty());
return operator[](this->size() - 1);
}
#endif
basic_string&
operator+=(const basic_string& __str)
{ return this->append(__str); }
basic_string&
operator+=(const _CharT* __s)
{ return this->append(__s); }
basic_string&
operator+=(_CharT __c)
{
this->push_back(__c);
return *this;
}
#if __cplusplus >= 201103L
basic_string&
operator+=(initializer_list<_CharT> __l)
{ return this->append(__l.begin(), __l.size()); }
#endif
#if __cplusplus >= 201703L
template<typename _Tp>
_If_sv<_Tp, basic_string&>
operator+=(const _Tp& __svt)
{ return this->append(__svt); }
#endif
basic_string&
append(const basic_string& __str);
basic_string&
append(const basic_string& __str, size_type __pos, size_type __n = npos);
basic_string&
append(const _CharT* __s, size_type __n);
basic_string&
append(const _CharT* __s)
{
__glibcxx_requires_string(__s);
return this->append(__s, traits_type::length(__s));
}
basic_string&
append(size_type __n, _CharT __c);
#if __cplusplus >= 201103L
basic_string&
append(initializer_list<_CharT> __l)
{ return this->append(__l.begin(), __l.size()); }
#endif
template<class _InputIterator>
basic_string&
append(_InputIterator __first, _InputIterator __last)
{ return this->replace(_M_iend(), _M_iend(), __first, __last); }
#if __cplusplus >= 201703L
template<typename _Tp>
_If_sv<_Tp, basic_string&>
append(const _Tp& __svt)
{
__sv_type __sv = __svt;
return this->append(__sv.data(), __sv.size());
}
template<typename _Tp>
_If_sv<_Tp, basic_string&>
append(const _Tp& __svt, size_type __pos, size_type __n = npos)
{
__sv_type __sv = __svt;
return append(__sv.data()
+ std::__sv_check(__sv.size(), __pos, "basic_string::append"),
std::__sv_limit(__sv.size(), __pos, __n));
}
#endif
void
push_back(_CharT __c)
{
const size_type __len = 1 + this->size();
if (__len > this->capacity() || _M_rep()->_M_is_shared())
this->reserve(__len);
traits_type::assign(_M_data()[this->size()], __c);
_M_rep()->_M_set_length_and_sharable(__len);
}
basic_string&
assign(const basic_string& __str);
#if __cplusplus >= 201103L
basic_string&
assign(basic_string&& __str)
noexcept(allocator_traits<_Alloc>::is_always_equal::value)
{
this->swap(__str);
return *this;
}
#endif
basic_string&
assign(const basic_string& __str, size_type __pos, size_type __n = npos)
{ return this->assign(__str._M_data()
+ __str._M_check(__pos, "basic_string::assign"),
__str._M_limit(__pos, __n)); }
basic_string&
assign(const _CharT* __s, size_type __n);
basic_string&
assign(const _CharT* __s)
{
__glibcxx_requires_string(__s);
return this->assign(__s, traits_type::length(__s));
}
basic_string&
assign(size_type __n, _CharT __c)
{ return _M_replace_aux(size_type(0), this->size(), __n, __c); }
template<class _InputIterator>
basic_string&
assign(_InputIterator __first, _InputIterator __last)
{ return this->replace(_M_ibegin(), _M_iend(), __first, __last); }
#if __cplusplus >= 201103L
basic_string&
assign(initializer_list<_CharT> __l)
{ return this->assign(__l.begin(), __l.size()); }
#endif
#if __cplusplus >= 201703L
template<typename _Tp>
_If_sv<_Tp, basic_string&>
assign(const _Tp& __svt)
{
__sv_type __sv = __svt;
return this->assign(__sv.data(), __sv.size());
}
template<typename _Tp>
_If_sv<_Tp, basic_string&>
assign(const _Tp& __svt, size_type __pos, size_type __n = npos)
{
__sv_type __sv = __svt;
return assign(__sv.data()
+ std::__sv_check(__sv.size(), __pos, "basic_string::assign"),
std::__sv_limit(__sv.size(), __pos, __n));
}
#endif
void
insert(iterator __p, size_type __n, _CharT __c)
{	this->replace(__p, __p, __n, __c);  }
template<class _InputIterator>
void
insert(iterator __p, _InputIterator __beg, _InputIterator __end)
{ this->replace(__p, __p, __beg, __end); }
#if __cplusplus >= 201103L
void
insert(iterator __p, initializer_list<_CharT> __l)
{
_GLIBCXX_DEBUG_PEDASSERT(__p >= _M_ibegin() && __p <= _M_iend());
this->insert(__p - _M_ibegin(), __l.begin(), __l.size());
}
#endif
basic_string&
insert(size_type __pos1, const basic_string& __str)
{ return this->insert(__pos1, __str, size_type(0), __str.size()); }
basic_string&
insert(size_type __pos1, const basic_string& __str,
size_type __pos2, size_type __n = npos)
{ return this->insert(__pos1, __str._M_data()
+ __str._M_check(__pos2, "basic_string::insert"),
__str._M_limit(__pos2, __n)); }
basic_string&
insert(size_type __pos, const _CharT* __s, size_type __n);
basic_string&
insert(size_type __pos, const _CharT* __s)
{
__glibcxx_requires_string(__s);
return this->insert(__pos, __s, traits_type::length(__s));
}
basic_string&
insert(size_type __pos, size_type __n, _CharT __c)
{ return _M_replace_aux(_M_check(__pos, "basic_string::insert"),
size_type(0), __n, __c); }
iterator
insert(iterator __p, _CharT __c)
{
_GLIBCXX_DEBUG_PEDASSERT(__p >= _M_ibegin() && __p <= _M_iend());
const size_type __pos = __p - _M_ibegin();
_M_replace_aux(__pos, size_type(0), size_type(1), __c);
_M_rep()->_M_set_leaked();
return iterator(_M_data() + __pos);
}
#if __cplusplus >= 201703L
template<typename _Tp>
_If_sv<_Tp, basic_string&>
insert(size_type __pos, const _Tp& __svt)
{
__sv_type __sv = __svt;
return this->insert(__pos, __sv.data(), __sv.size());
}
template<typename _Tp>
_If_sv<_Tp, basic_string&>
insert(size_type __pos1, const _Tp& __svt,
size_type __pos2, size_type __n = npos)
{
__sv_type __sv = __svt;
return this->replace(__pos1, size_type(0), __sv.data()
+ std::__sv_check(__sv.size(), __pos2, "basic_string::insert"),
std::__sv_limit(__sv.size(), __pos2, __n));
}
#endif
basic_string&
erase(size_type __pos = 0, size_type __n = npos)
{
_M_mutate(_M_check(__pos, "basic_string::erase"),
_M_limit(__pos, __n), size_type(0));
return *this;
}
iterator
erase(iterator __position)
{
_GLIBCXX_DEBUG_PEDASSERT(__position >= _M_ibegin()
&& __position < _M_iend());
const size_type __pos = __position - _M_ibegin();
_M_mutate(__pos, size_type(1), size_type(0));
_M_rep()->_M_set_leaked();
return iterator(_M_data() + __pos);
}
iterator
erase(iterator __first, iterator __last);
#if __cplusplus >= 201103L
void
pop_back()
{
__glibcxx_assert(!empty());
erase(size() - 1, 1);
}
#endif
basic_string&
replace(size_type __pos, size_type __n, const basic_string& __str)
{ return this->replace(__pos, __n, __str._M_data(), __str.size()); }
basic_string&
replace(size_type __pos1, size_type __n1, const basic_string& __str,
size_type __pos2, size_type __n2 = npos)
{ return this->replace(__pos1, __n1, __str._M_data()
+ __str._M_check(__pos2, "basic_string::replace"),
__str._M_limit(__pos2, __n2)); }
basic_string&
replace(size_type __pos, size_type __n1, const _CharT* __s,
size_type __n2);
basic_string&
replace(size_type __pos, size_type __n1, const _CharT* __s)
{
__glibcxx_requires_string(__s);
return this->replace(__pos, __n1, __s, traits_type::length(__s));
}
basic_string&
replace(size_type __pos, size_type __n1, size_type __n2, _CharT __c)
{ return _M_replace_aux(_M_check(__pos, "basic_string::replace"),
_M_limit(__pos, __n1), __n2, __c); }
basic_string&
replace(iterator __i1, iterator __i2, const basic_string& __str)
{ return this->replace(__i1, __i2, __str._M_data(), __str.size()); }
basic_string&
replace(iterator __i1, iterator __i2, const _CharT* __s, size_type __n)
{
_GLIBCXX_DEBUG_PEDASSERT(_M_ibegin() <= __i1 && __i1 <= __i2
&& __i2 <= _M_iend());
return this->replace(__i1 - _M_ibegin(), __i2 - __i1, __s, __n);
}
basic_string&
replace(iterator __i1, iterator __i2, const _CharT* __s)
{
__glibcxx_requires_string(__s);
return this->replace(__i1, __i2, __s, traits_type::length(__s));
}
basic_string&
replace(iterator __i1, iterator __i2, size_type __n, _CharT __c)
{
_GLIBCXX_DEBUG_PEDASSERT(_M_ibegin() <= __i1 && __i1 <= __i2
&& __i2 <= _M_iend());
return _M_replace_aux(__i1 - _M_ibegin(), __i2 - __i1, __n, __c);
}
template<class _InputIterator>
basic_string&
replace(iterator __i1, iterator __i2,
_InputIterator __k1, _InputIterator __k2)
{
_GLIBCXX_DEBUG_PEDASSERT(_M_ibegin() <= __i1 && __i1 <= __i2
&& __i2 <= _M_iend());
__glibcxx_requires_valid_range(__k1, __k2);
typedef typename std::__is_integer<_InputIterator>::__type _Integral;
return _M_replace_dispatch(__i1, __i2, __k1, __k2, _Integral());
}
basic_string&
replace(iterator __i1, iterator __i2, _CharT* __k1, _CharT* __k2)
{
_GLIBCXX_DEBUG_PEDASSERT(_M_ibegin() <= __i1 && __i1 <= __i2
&& __i2 <= _M_iend());
__glibcxx_requires_valid_range(__k1, __k2);
return this->replace(__i1 - _M_ibegin(), __i2 - __i1,
__k1, __k2 - __k1);
}
basic_string&
replace(iterator __i1, iterator __i2,
const _CharT* __k1, const _CharT* __k2)
{
_GLIBCXX_DEBUG_PEDASSERT(_M_ibegin() <= __i1 && __i1 <= __i2
&& __i2 <= _M_iend());
__glibcxx_requires_valid_range(__k1, __k2);
return this->replace(__i1 - _M_ibegin(), __i2 - __i1,
__k1, __k2 - __k1);
}
basic_string&
replace(iterator __i1, iterator __i2, iterator __k1, iterator __k2)
{
_GLIBCXX_DEBUG_PEDASSERT(_M_ibegin() <= __i1 && __i1 <= __i2
&& __i2 <= _M_iend());
__glibcxx_requires_valid_range(__k1, __k2);
return this->replace(__i1 - _M_ibegin(), __i2 - __i1,
__k1.base(), __k2 - __k1);
}
basic_string&
replace(iterator __i1, iterator __i2,
const_iterator __k1, const_iterator __k2)
{
_GLIBCXX_DEBUG_PEDASSERT(_M_ibegin() <= __i1 && __i1 <= __i2
&& __i2 <= _M_iend());
__glibcxx_requires_valid_range(__k1, __k2);
return this->replace(__i1 - _M_ibegin(), __i2 - __i1,
__k1.base(), __k2 - __k1);
}
#if __cplusplus >= 201103L
basic_string& replace(iterator __i1, iterator __i2,
initializer_list<_CharT> __l)
{ return this->replace(__i1, __i2, __l.begin(), __l.end()); }
#endif
#if __cplusplus >= 201703L
template<typename _Tp>
_If_sv<_Tp, basic_string&>
replace(size_type __pos, size_type __n, const _Tp& __svt)
{
__sv_type __sv = __svt;
return this->replace(__pos, __n, __sv.data(), __sv.size());
}
template<typename _Tp>
_If_sv<_Tp, basic_string&>
replace(size_type __pos1, size_type __n1, const _Tp& __svt,
size_type __pos2, size_type __n2 = npos)
{
__sv_type __sv = __svt;
return this->replace(__pos1, __n1,
__sv.data()
+ std::__sv_check(__sv.size(), __pos2, "basic_string::replace"),
std::__sv_limit(__sv.size(), __pos2, __n2));
}
template<typename _Tp>
_If_sv<_Tp, basic_string&>
replace(const_iterator __i1, const_iterator __i2, const _Tp& __svt)
{
__sv_type __sv = __svt;
return this->replace(__i1 - begin(), __i2 - __i1, __sv);
}
#endif
private:
template<class _Integer>
basic_string&
_M_replace_dispatch(iterator __i1, iterator __i2, _Integer __n,
_Integer __val, __true_type)
{ return _M_replace_aux(__i1 - _M_ibegin(), __i2 - __i1, __n, __val); }
template<class _InputIterator>
basic_string&
_M_replace_dispatch(iterator __i1, iterator __i2, _InputIterator __k1,
_InputIterator __k2, __false_type);
basic_string&
_M_replace_aux(size_type __pos1, size_type __n1, size_type __n2,
_CharT __c);
basic_string&
_M_replace_safe(size_type __pos1, size_type __n1, const _CharT* __s,
size_type __n2);
template<class _InIterator>
static _CharT*
_S_construct_aux(_InIterator __beg, _InIterator __end,
const _Alloc& __a, __false_type)
{
typedef typename iterator_traits<_InIterator>::iterator_category _Tag;
return _S_construct(__beg, __end, __a, _Tag());
}
template<class _Integer>
static _CharT*
_S_construct_aux(_Integer __beg, _Integer __end,
const _Alloc& __a, __true_type)
{ return _S_construct_aux_2(static_cast<size_type>(__beg),
__end, __a); }
static _CharT*
_S_construct_aux_2(size_type __req, _CharT __c, const _Alloc& __a)
{ return _S_construct(__req, __c, __a); }
template<class _InIterator>
static _CharT*
_S_construct(_InIterator __beg, _InIterator __end, const _Alloc& __a)
{
typedef typename std::__is_integer<_InIterator>::__type _Integral;
return _S_construct_aux(__beg, __end, __a, _Integral());
}
template<class _InIterator>
static _CharT*
_S_construct(_InIterator __beg, _InIterator __end, const _Alloc& __a,
input_iterator_tag);
template<class _FwdIterator>
static _CharT*
_S_construct(_FwdIterator __beg, _FwdIterator __end, const _Alloc& __a,
forward_iterator_tag);
static _CharT*
_S_construct(size_type __req, _CharT __c, const _Alloc& __a);
public:
size_type
copy(_CharT* __s, size_type __n, size_type __pos = 0) const;
void
swap(basic_string& __s)
_GLIBCXX_NOEXCEPT_IF(allocator_traits<_Alloc>::is_always_equal::value);
const _CharT*
c_str() const _GLIBCXX_NOEXCEPT
{ return _M_data(); }
const _CharT*
data() const _GLIBCXX_NOEXCEPT
{ return _M_data(); }
#if __cplusplus >= 201703L
_CharT*
data() noexcept
{
_M_leak();
return _M_data();
}
#endif
allocator_type
get_allocator() const _GLIBCXX_NOEXCEPT
{ return _M_dataplus; }
size_type
find(const _CharT* __s, size_type __pos, size_type __n) const
_GLIBCXX_NOEXCEPT;
size_type
find(const basic_string& __str, size_type __pos = 0) const
_GLIBCXX_NOEXCEPT
{ return this->find(__str.data(), __pos, __str.size()); }
size_type
find(const _CharT* __s, size_type __pos = 0) const _GLIBCXX_NOEXCEPT
{
__glibcxx_requires_string(__s);
return this->find(__s, __pos, traits_type::length(__s));
}
size_type
find(_CharT __c, size_type __pos = 0) const _GLIBCXX_NOEXCEPT;
#if __cplusplus >= 201703L
template<typename _Tp>
_If_sv<_Tp, size_type>
find(const _Tp& __svt, size_type __pos = 0) const
noexcept(is_same<_Tp, __sv_type>::value)
{
__sv_type __sv = __svt;
return this->find(__sv.data(), __pos, __sv.size());
}
#endif
size_type
rfind(const basic_string& __str, size_type __pos = npos) const
_GLIBCXX_NOEXCEPT
{ return this->rfind(__str.data(), __pos, __str.size()); }
size_type
rfind(const _CharT* __s, size_type __pos, size_type __n) const
_GLIBCXX_NOEXCEPT;
size_type
rfind(const _CharT* __s, size_type __pos = npos) const _GLIBCXX_NOEXCEPT
{
__glibcxx_requires_string(__s);
return this->rfind(__s, __pos, traits_type::length(__s));
}
size_type
rfind(_CharT __c, size_type __pos = npos) const _GLIBCXX_NOEXCEPT;
#if __cplusplus >= 201703L
template<typename _Tp>
_If_sv<_Tp, size_type>
rfind(const _Tp& __svt, size_type __pos = npos) const
noexcept(is_same<_Tp, __sv_type>::value)
{
__sv_type __sv = __svt;
return this->rfind(__sv.data(), __pos, __sv.size());
}
#endif
size_type
find_first_of(const basic_string& __str, size_type __pos = 0) const
_GLIBCXX_NOEXCEPT
{ return this->find_first_of(__str.data(), __pos, __str.size()); }
size_type
find_first_of(const _CharT* __s, size_type __pos, size_type __n) const
_GLIBCXX_NOEXCEPT;
size_type
find_first_of(const _CharT* __s, size_type __pos = 0) const
_GLIBCXX_NOEXCEPT
{
__glibcxx_requires_string(__s);
return this->find_first_of(__s, __pos, traits_type::length(__s));
}
size_type
find_first_of(_CharT __c, size_type __pos = 0) const _GLIBCXX_NOEXCEPT
{ return this->find(__c, __pos); }
#if __cplusplus >= 201703L
template<typename _Tp>
_If_sv<_Tp, size_type>
find_first_of(const _Tp& __svt, size_type __pos = 0) const
noexcept(is_same<_Tp, __sv_type>::value)
{
__sv_type __sv = __svt;
return this->find_first_of(__sv.data(), __pos, __sv.size());
}
#endif
size_type
find_last_of(const basic_string& __str, size_type __pos = npos) const
_GLIBCXX_NOEXCEPT
{ return this->find_last_of(__str.data(), __pos, __str.size()); }
size_type
find_last_of(const _CharT* __s, size_type __pos, size_type __n) const
_GLIBCXX_NOEXCEPT;
size_type
find_last_of(const _CharT* __s, size_type __pos = npos) const
_GLIBCXX_NOEXCEPT
{
__glibcxx_requires_string(__s);
return this->find_last_of(__s, __pos, traits_type::length(__s));
}
size_type
find_last_of(_CharT __c, size_type __pos = npos) const _GLIBCXX_NOEXCEPT
{ return this->rfind(__c, __pos); }
#if __cplusplus >= 201703L
template<typename _Tp>
_If_sv<_Tp, size_type>
find_last_of(const _Tp& __svt, size_type __pos = npos) const
noexcept(is_same<_Tp, __sv_type>::value)
{
__sv_type __sv = __svt;
return this->find_last_of(__sv.data(), __pos, __sv.size());
}
#endif
size_type
find_first_not_of(const basic_string& __str, size_type __pos = 0) const
_GLIBCXX_NOEXCEPT
{ return this->find_first_not_of(__str.data(), __pos, __str.size()); }
size_type
find_first_not_of(const _CharT* __s, size_type __pos,
size_type __n) const _GLIBCXX_NOEXCEPT;
size_type
find_first_not_of(const _CharT* __s, size_type __pos = 0) const
_GLIBCXX_NOEXCEPT
{
__glibcxx_requires_string(__s);
return this->find_first_not_of(__s, __pos, traits_type::length(__s));
}
size_type
find_first_not_of(_CharT __c, size_type __pos = 0) const
_GLIBCXX_NOEXCEPT;
#if __cplusplus >= 201703L
template<typename _Tp>
_If_sv<_Tp, size_type>
find_first_not_of(const _Tp& __svt, size_type __pos = 0) const
noexcept(is_same<_Tp, __sv_type>::value)
{
__sv_type __sv = __svt;
return this->find_first_not_of(__sv.data(), __pos, __sv.size());
}
#endif
size_type
find_last_not_of(const basic_string& __str, size_type __pos = npos) const
_GLIBCXX_NOEXCEPT
{ return this->find_last_not_of(__str.data(), __pos, __str.size()); }
size_type
find_last_not_of(const _CharT* __s, size_type __pos,
size_type __n) const _GLIBCXX_NOEXCEPT;
size_type
find_last_not_of(const _CharT* __s, size_type __pos = npos) const
_GLIBCXX_NOEXCEPT
{
__glibcxx_requires_string(__s);
return this->find_last_not_of(__s, __pos, traits_type::length(__s));
}
size_type
find_last_not_of(_CharT __c, size_type __pos = npos) const
_GLIBCXX_NOEXCEPT;
#if __cplusplus >= 201703L
template<typename _Tp>
_If_sv<_Tp, size_type>
find_last_not_of(const _Tp& __svt, size_type __pos = npos) const
noexcept(is_same<_Tp, __sv_type>::value)
{
__sv_type __sv = __svt;
return this->find_last_not_of(__sv.data(), __pos, __sv.size());
}
#endif
basic_string
substr(size_type __pos = 0, size_type __n = npos) const
{ return basic_string(*this,
_M_check(__pos, "basic_string::substr"), __n); }
int
compare(const basic_string& __str) const
{
const size_type __size = this->size();
const size_type __osize = __str.size();
const size_type __len = std::min(__size, __osize);
int __r = traits_type::compare(_M_data(), __str.data(), __len);
if (!__r)
__r = _S_compare(__size, __osize);
return __r;
}
#if __cplusplus >= 201703L
template<typename _Tp>
_If_sv<_Tp, int>
compare(const _Tp& __svt) const
noexcept(is_same<_Tp, __sv_type>::value)
{
__sv_type __sv = __svt;
const size_type __size = this->size();
const size_type __osize = __sv.size();
const size_type __len = std::min(__size, __osize);
int __r = traits_type::compare(_M_data(), __sv.data(), __len);
if (!__r)
__r = _S_compare(__size, __osize);
return __r;
}
template<typename _Tp>
_If_sv<_Tp, int>
compare(size_type __pos, size_type __n, const _Tp& __svt) const
noexcept(is_same<_Tp, __sv_type>::value)
{
__sv_type __sv = __svt;
return __sv_type(*this).substr(__pos, __n).compare(__sv);
}
template<typename _Tp>
_If_sv<_Tp, int>
compare(size_type __pos1, size_type __n1, const _Tp& __svt,
size_type __pos2, size_type __n2 = npos) const
noexcept(is_same<_Tp, __sv_type>::value)
{
__sv_type __sv = __svt;
return __sv_type(*this)
.substr(__pos1, __n1).compare(__sv.substr(__pos2, __n2));
}
#endif
int
compare(size_type __pos, size_type __n, const basic_string& __str) const;
int
compare(size_type __pos1, size_type __n1, const basic_string& __str,
size_type __pos2, size_type __n2 = npos) const;
int
compare(const _CharT* __s) const _GLIBCXX_NOEXCEPT;
int
compare(size_type __pos, size_type __n1, const _CharT* __s) const;
int
compare(size_type __pos, size_type __n1, const _CharT* __s,
size_type __n2) const;
#if __cplusplus > 201703L
bool
starts_with(basic_string_view<_CharT, _Traits> __x) const noexcept
{ return __sv_type(this->data(), this->size()).starts_with(__x); }
bool
starts_with(_CharT __x) const noexcept
{ return __sv_type(this->data(), this->size()).starts_with(__x); }
bool
starts_with(const _CharT* __x) const noexcept
{ return __sv_type(this->data(), this->size()).starts_with(__x); }
bool
ends_with(basic_string_view<_CharT, _Traits> __x) const noexcept
{ return __sv_type(this->data(), this->size()).ends_with(__x); }
bool
ends_with(_CharT __x) const noexcept
{ return __sv_type(this->data(), this->size()).ends_with(__x); }
bool
ends_with(const _CharT* __x) const noexcept
{ return __sv_type(this->data(), this->size()).ends_with(__x); }
#endif
#if __cplusplus > 202011L
bool
contains(basic_string_view<_CharT, _Traits> __x) const noexcept
{ return __sv_type(this->data(), this->size()).contains(__x); }
bool
contains(_CharT __x) const noexcept
{ return __sv_type(this->data(), this->size()).contains(__x); }
bool
contains(const _CharT* __x) const noexcept
{ return __sv_type(this->data(), this->size()).contains(__x); }
#endif
# ifdef _GLIBCXX_TM_TS_INTERNAL
friend void
::_txnal_cow_string_C1_for_exceptions(void* that, const char* s,
void* exc);
friend const char*
::_txnal_cow_string_c_str(const void *that);
friend void
::_txnal_cow_string_D1(void *that);
friend void
::_txnal_cow_string_D1_commit(void *that);
# endif
};
#endif
#if __cpp_deduction_guides >= 201606
_GLIBCXX_BEGIN_NAMESPACE_CXX11
template<typename _InputIterator, typename _CharT
= typename iterator_traits<_InputIterator>::value_type,
typename _Allocator = allocator<_CharT>,
typename = _RequireInputIter<_InputIterator>,
typename = _RequireAllocator<_Allocator>>
basic_string(_InputIterator, _InputIterator, _Allocator = _Allocator())
-> basic_string<_CharT, char_traits<_CharT>, _Allocator>;
template<typename _CharT, typename _Traits,
typename _Allocator = allocator<_CharT>,
typename = _RequireAllocator<_Allocator>>
basic_string(basic_string_view<_CharT, _Traits>, const _Allocator& = _Allocator())
-> basic_string<_CharT, _Traits, _Allocator>;
template<typename _CharT, typename _Traits,
typename _Allocator = allocator<_CharT>,
typename = _RequireAllocator<_Allocator>>
basic_string(basic_string_view<_CharT, _Traits>,
typename basic_string<_CharT, _Traits, _Allocator>::size_type,
typename basic_string<_CharT, _Traits, _Allocator>::size_type,
const _Allocator& = _Allocator())
-> basic_string<_CharT, _Traits, _Allocator>;
_GLIBCXX_END_NAMESPACE_CXX11
#endif
template<typename _CharT, typename _Traits, typename _Alloc>
basic_string<_CharT, _Traits, _Alloc>
operator+(const basic_string<_CharT, _Traits, _Alloc>& __lhs,
const basic_string<_CharT, _Traits, _Alloc>& __rhs)
{
basic_string<_CharT, _Traits, _Alloc> __str(__lhs);
__str.append(__rhs);
return __str;
}
template<typename _CharT, typename _Traits, typename _Alloc>
basic_string<_CharT,_Traits,_Alloc>
operator+(const _CharT* __lhs,
const basic_string<_CharT,_Traits,_Alloc>& __rhs);
template<typename _CharT, typename _Traits, typename _Alloc>
basic_string<_CharT,_Traits,_Alloc>
operator+(_CharT __lhs, const basic_string<_CharT,_Traits,_Alloc>& __rhs);
template<typename _CharT, typename _Traits, typename _Alloc>
inline basic_string<_CharT, _Traits, _Alloc>
operator+(const basic_string<_CharT, _Traits, _Alloc>& __lhs,
const _CharT* __rhs)
{
basic_string<_CharT, _Traits, _Alloc> __str(__lhs);
__str.append(__rhs);
return __str;
}
template<typename _CharT, typename _Traits, typename _Alloc>
inline basic_string<_CharT, _Traits, _Alloc>
operator+(const basic_string<_CharT, _Traits, _Alloc>& __lhs, _CharT __rhs)
{
typedef basic_string<_CharT, _Traits, _Alloc>	__string_type;
typedef typename __string_type::size_type		__size_type;
__string_type __str(__lhs);
__str.append(__size_type(1), __rhs);
return __str;
}
#if __cplusplus >= 201103L
template<typename _CharT, typename _Traits, typename _Alloc>
inline basic_string<_CharT, _Traits, _Alloc>
operator+(basic_string<_CharT, _Traits, _Alloc>&& __lhs,
const basic_string<_CharT, _Traits, _Alloc>& __rhs)
{ return std::move(__lhs.append(__rhs)); }
template<typename _CharT, typename _Traits, typename _Alloc>
inline basic_string<_CharT, _Traits, _Alloc>
operator+(const basic_string<_CharT, _Traits, _Alloc>& __lhs,
basic_string<_CharT, _Traits, _Alloc>&& __rhs)
{ return std::move(__rhs.insert(0, __lhs)); }
template<typename _CharT, typename _Traits, typename _Alloc>
inline basic_string<_CharT, _Traits, _Alloc>
operator+(basic_string<_CharT, _Traits, _Alloc>&& __lhs,
basic_string<_CharT, _Traits, _Alloc>&& __rhs)
{
#if _GLIBCXX_USE_CXX11_ABI
using _Alloc_traits = allocator_traits<_Alloc>;
bool __use_rhs = false;
if _GLIBCXX17_CONSTEXPR (typename _Alloc_traits::is_always_equal{})
__use_rhs = true;
else if (__lhs.get_allocator() == __rhs.get_allocator())
__use_rhs = true;
if (__use_rhs)
#endif
{
const auto __size = __lhs.size() + __rhs.size();
if (__size > __lhs.capacity() && __size <= __rhs.capacity())
return std::move(__rhs.insert(0, __lhs));
}
return std::move(__lhs.append(__rhs));
}
template<typename _CharT, typename _Traits, typename _Alloc>
inline basic_string<_CharT, _Traits, _Alloc>
operator+(const _CharT* __lhs,
basic_string<_CharT, _Traits, _Alloc>&& __rhs)
{ return std::move(__rhs.insert(0, __lhs)); }
template<typename _CharT, typename _Traits, typename _Alloc>
inline basic_string<_CharT, _Traits, _Alloc>
operator+(_CharT __lhs,
basic_string<_CharT, _Traits, _Alloc>&& __rhs)
{ return std::move(__rhs.insert(0, 1, __lhs)); }
template<typename _CharT, typename _Traits, typename _Alloc>
inline basic_string<_CharT, _Traits, _Alloc>
operator+(basic_string<_CharT, _Traits, _Alloc>&& __lhs,
const _CharT* __rhs)
{ return std::move(__lhs.append(__rhs)); }
template<typename _CharT, typename _Traits, typename _Alloc>
inline basic_string<_CharT, _Traits, _Alloc>
operator+(basic_string<_CharT, _Traits, _Alloc>&& __lhs,
_CharT __rhs)
{ return std::move(__lhs.append(1, __rhs)); }
#endif
template<typename _CharT, typename _Traits, typename _Alloc>
inline bool
operator==(const basic_string<_CharT, _Traits, _Alloc>& __lhs,
const basic_string<_CharT, _Traits, _Alloc>& __rhs)
_GLIBCXX_NOEXCEPT
{ return __lhs.compare(__rhs) == 0; }
template<typename _CharT>
inline
typename __gnu_cxx::__enable_if<__is_char<_CharT>::__value, bool>::__type
operator==(const basic_string<_CharT>& __lhs,
const basic_string<_CharT>& __rhs) _GLIBCXX_NOEXCEPT
{ return (__lhs.size() == __rhs.size()
&& !std::char_traits<_CharT>::compare(__lhs.data(), __rhs.data(),
__lhs.size())); }
template<typename _CharT, typename _Traits, typename _Alloc>
inline bool
operator==(const basic_string<_CharT, _Traits, _Alloc>& __lhs,
const _CharT* __rhs)
{ return __lhs.compare(__rhs) == 0; }
#if __cpp_lib_three_way_comparison
template<typename _CharT, typename _Traits, typename _Alloc>
inline auto
operator<=>(const basic_string<_CharT, _Traits, _Alloc>& __lhs,
const basic_string<_CharT, _Traits, _Alloc>& __rhs) noexcept
-> decltype(__detail::__char_traits_cmp_cat<_Traits>(0))
{ return __detail::__char_traits_cmp_cat<_Traits>(__lhs.compare(__rhs)); }
template<typename _CharT, typename _Traits, typename _Alloc>
inline auto
operator<=>(const basic_string<_CharT, _Traits, _Alloc>& __lhs,
const _CharT* __rhs) noexcept
-> decltype(__detail::__char_traits_cmp_cat<_Traits>(0))
{ return __detail::__char_traits_cmp_cat<_Traits>(__lhs.compare(__rhs)); }
#else
template<typename _CharT, typename _Traits, typename _Alloc>
inline bool
operator==(const _CharT* __lhs,
const basic_string<_CharT, _Traits, _Alloc>& __rhs)
{ return __rhs.compare(__lhs) == 0; }
template<typename _CharT, typename _Traits, typename _Alloc>
inline bool
operator!=(const basic_string<_CharT, _Traits, _Alloc>& __lhs,
const basic_string<_CharT, _Traits, _Alloc>& __rhs)
_GLIBCXX_NOEXCEPT
{ return !(__lhs == __rhs); }
template<typename _CharT, typename _Traits, typename _Alloc>
inline bool
operator!=(const _CharT* __lhs,
const basic_string<_CharT, _Traits, _Alloc>& __rhs)
{ return !(__lhs == __rhs); }
template<typename _CharT, typename _Traits, typename _Alloc>
inline bool
operator!=(const basic_string<_CharT, _Traits, _Alloc>& __lhs,
const _CharT* __rhs)
{ return !(__lhs == __rhs); }
template<typename _CharT, typename _Traits, typename _Alloc>
inline bool
operator<(const basic_string<_CharT, _Traits, _Alloc>& __lhs,
const basic_string<_CharT, _Traits, _Alloc>& __rhs)
_GLIBCXX_NOEXCEPT
{ return __lhs.compare(__rhs) < 0; }
template<typename _CharT, typename _Traits, typename _Alloc>
inline bool
operator<(const basic_string<_CharT, _Traits, _Alloc>& __lhs,
const _CharT* __rhs)
{ return __lhs.compare(__rhs) < 0; }
template<typename _CharT, typename _Traits, typename _Alloc>
inline bool
operator<(const _CharT* __lhs,
const basic_string<_CharT, _Traits, _Alloc>& __rhs)
{ return __rhs.compare(__lhs) > 0; }
template<typename _CharT, typename _Traits, typename _Alloc>
inline bool
operator>(const basic_string<_CharT, _Traits, _Alloc>& __lhs,
const basic_string<_CharT, _Traits, _Alloc>& __rhs)
_GLIBCXX_NOEXCEPT
{ return __lhs.compare(__rhs) > 0; }
template<typename _CharT, typename _Traits, typename _Alloc>
inline bool
operator>(const basic_string<_CharT, _Traits, _Alloc>& __lhs,
const _CharT* __rhs)
{ return __lhs.compare(__rhs) > 0; }
template<typename _CharT, typename _Traits, typename _Alloc>
inline bool
operator>(const _CharT* __lhs,
const basic_string<_CharT, _Traits, _Alloc>& __rhs)
{ return __rhs.compare(__lhs) < 0; }
template<typename _CharT, typename _Traits, typename _Alloc>
inline bool
operator<=(const basic_string<_CharT, _Traits, _Alloc>& __lhs,
const basic_string<_CharT, _Traits, _Alloc>& __rhs)
_GLIBCXX_NOEXCEPT
{ return __lhs.compare(__rhs) <= 0; }
template<typename _CharT, typename _Traits, typename _Alloc>
inline bool
operator<=(const basic_string<_CharT, _Traits, _Alloc>& __lhs,
const _CharT* __rhs)
{ return __lhs.compare(__rhs) <= 0; }
template<typename _CharT, typename _Traits, typename _Alloc>
inline bool
operator<=(const _CharT* __lhs,
const basic_string<_CharT, _Traits, _Alloc>& __rhs)
{ return __rhs.compare(__lhs) >= 0; }
template<typename _CharT, typename _Traits, typename _Alloc>
inline bool
operator>=(const basic_string<_CharT, _Traits, _Alloc>& __lhs,
const basic_string<_CharT, _Traits, _Alloc>& __rhs)
_GLIBCXX_NOEXCEPT
{ return __lhs.compare(__rhs) >= 0; }
template<typename _CharT, typename _Traits, typename _Alloc>
inline bool
operator>=(const basic_string<_CharT, _Traits, _Alloc>& __lhs,
const _CharT* __rhs)
{ return __lhs.compare(__rhs) >= 0; }
template<typename _CharT, typename _Traits, typename _Alloc>
inline bool
operator>=(const _CharT* __lhs,
const basic_string<_CharT, _Traits, _Alloc>& __rhs)
{ return __rhs.compare(__lhs) <= 0; }
#endif
template<typename _CharT, typename _Traits, typename _Alloc>
inline void
swap(basic_string<_CharT, _Traits, _Alloc>& __lhs,
basic_string<_CharT, _Traits, _Alloc>& __rhs)
_GLIBCXX_NOEXCEPT_IF(noexcept(__lhs.swap(__rhs)))
{ __lhs.swap(__rhs); }
template<typename _CharT, typename _Traits, typename _Alloc>
basic_istream<_CharT, _Traits>&
operator>>(basic_istream<_CharT, _Traits>& __is,
basic_string<_CharT, _Traits, _Alloc>& __str);
template<>
basic_istream<char>&
operator>>(basic_istream<char>& __is, basic_string<char>& __str);
template<typename _CharT, typename _Traits, typename _Alloc>
inline basic_ostream<_CharT, _Traits>&
operator<<(basic_ostream<_CharT, _Traits>& __os,
const basic_string<_CharT, _Traits, _Alloc>& __str)
{
return __ostream_insert(__os, __str.data(), __str.size());
}
template<typename _CharT, typename _Traits, typename _Alloc>
basic_istream<_CharT, _Traits>&
getline(basic_istream<_CharT, _Traits>& __is,
basic_string<_CharT, _Traits, _Alloc>& __str, _CharT __delim);
template<typename _CharT, typename _Traits, typename _Alloc>
inline basic_istream<_CharT, _Traits>&
getline(basic_istream<_CharT, _Traits>& __is,
basic_string<_CharT, _Traits, _Alloc>& __str)
{ return std::getline(__is, __str, __is.widen('\n')); }
#if __cplusplus >= 201103L
template<typename _CharT, typename _Traits, typename _Alloc>
inline basic_istream<_CharT, _Traits>&
getline(basic_istream<_CharT, _Traits>&& __is,
basic_string<_CharT, _Traits, _Alloc>& __str, _CharT __delim)
{ return std::getline(__is, __str, __delim); }
template<typename _CharT, typename _Traits, typename _Alloc>
inline basic_istream<_CharT, _Traits>&
getline(basic_istream<_CharT, _Traits>&& __is,
basic_string<_CharT, _Traits, _Alloc>& __str)
{ return std::getline(__is, __str); }
#endif
template<>
basic_istream<char>&
getline(basic_istream<char>& __in, basic_string<char>& __str,
char __delim);
#ifdef _GLIBCXX_USE_WCHAR_T
template<>
basic_istream<wchar_t>&
getline(basic_istream<wchar_t>& __in, basic_string<wchar_t>& __str,
wchar_t __delim);
#endif
_GLIBCXX_END_NAMESPACE_VERSION
}
#if __cplusplus >= 201103L
#include <ext/string_conversions.h>
#include <bits/charconv.h>
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
_GLIBCXX_BEGIN_NAMESPACE_CXX11
#if _GLIBCXX_USE_C99_STDLIB
inline int
stoi(const string& __str, size_t* __idx = 0, int __base = 10)
{ return __gnu_cxx::__stoa<long, int>(&std::strtol, "stoi", __str.c_str(),
__idx, __base); }
inline long
stol(const string& __str, size_t* __idx = 0, int __base = 10)
{ return __gnu_cxx::__stoa(&std::strtol, "stol", __str.c_str(),
__idx, __base); }
inline unsigned long
stoul(const string& __str, size_t* __idx = 0, int __base = 10)
{ return __gnu_cxx::__stoa(&std::strtoul, "stoul", __str.c_str(),
__idx, __base); }
inline long long
stoll(const string& __str, size_t* __idx = 0, int __base = 10)
{ return __gnu_cxx::__stoa(&std::strtoll, "stoll", __str.c_str(),
__idx, __base); }
inline unsigned long long
stoull(const string& __str, size_t* __idx = 0, int __base = 10)
{ return __gnu_cxx::__stoa(&std::strtoull, "stoull", __str.c_str(),
__idx, __base); }
inline float
stof(const string& __str, size_t* __idx = 0)
{ return __gnu_cxx::__stoa(&std::strtof, "stof", __str.c_str(), __idx); }
inline double
stod(const string& __str, size_t* __idx = 0)
{ return __gnu_cxx::__stoa(&std::strtod, "stod", __str.c_str(), __idx); }
inline long double
stold(const string& __str, size_t* __idx = 0)
{ return __gnu_cxx::__stoa(&std::strtold, "stold", __str.c_str(), __idx); }
#endif
inline string
to_string(int __val)
{
const bool __neg = __val < 0;
const unsigned __uval = __neg ? (unsigned)~__val + 1u : __val;
const auto __len = __detail::__to_chars_len(__uval);
string __str(__neg + __len, '-');
__detail::__to_chars_10_impl(&__str[__neg], __len, __uval);
return __str;
}
inline string
to_string(unsigned __val)
{
string __str(__detail::__to_chars_len(__val), '\0');
__detail::__to_chars_10_impl(&__str[0], __str.size(), __val);
return __str;
}
inline string
to_string(long __val)
{
const bool __neg = __val < 0;
const unsigned long __uval = __neg ? (unsigned long)~__val + 1ul : __val;
const auto __len = __detail::__to_chars_len(__uval);
string __str(__neg + __len, '-');
__detail::__to_chars_10_impl(&__str[__neg], __len, __uval);
return __str;
}
inline string
to_string(unsigned long __val)
{
string __str(__detail::__to_chars_len(__val), '\0');
__detail::__to_chars_10_impl(&__str[0], __str.size(), __val);
return __str;
}
inline string
to_string(long long __val)
{
const bool __neg = __val < 0;
const unsigned long long __uval
= __neg ? (unsigned long long)~__val + 1ull : __val;
const auto __len = __detail::__to_chars_len(__uval);
string __str(__neg + __len, '-');
__detail::__to_chars_10_impl(&__str[__neg], __len, __uval);
return __str;
}
inline string
to_string(unsigned long long __val)
{
string __str(__detail::__to_chars_len(__val), '\0');
__detail::__to_chars_10_impl(&__str[0], __str.size(), __val);
return __str;
}
#if _GLIBCXX_USE_C99_STDIO
inline string
to_string(float __val)
{
const int __n =
__gnu_cxx::__numeric_traits<float>::__max_exponent10 + 20;
return __gnu_cxx::__to_xstring<string>(&std::vsnprintf, __n,
"%f", __val);
}
inline string
to_string(double __val)
{
const int __n =
__gnu_cxx::__numeric_traits<double>::__max_exponent10 + 20;
return __gnu_cxx::__to_xstring<string>(&std::vsnprintf, __n,
"%f", __val);
}
inline string
to_string(long double __val)
{
const int __n =
__gnu_cxx::__numeric_traits<long double>::__max_exponent10 + 20;
return __gnu_cxx::__to_xstring<string>(&std::vsnprintf, __n,
"%Lf", __val);
}
#endif
#if defined(_GLIBCXX_USE_WCHAR_T) && _GLIBCXX_USE_C99_WCHAR
inline int
stoi(const wstring& __str, size_t* __idx = 0, int __base = 10)
{ return __gnu_cxx::__stoa<long, int>(&std::wcstol, "stoi", __str.c_str(),
__idx, __base); }
inline long
stol(const wstring& __str, size_t* __idx = 0, int __base = 10)
{ return __gnu_cxx::__stoa(&std::wcstol, "stol", __str.c_str(),
__idx, __base); }
inline unsigned long
stoul(const wstring& __str, size_t* __idx = 0, int __base = 10)
{ return __gnu_cxx::__stoa(&std::wcstoul, "stoul", __str.c_str(),
__idx, __base); }
inline long long
stoll(const wstring& __str, size_t* __idx = 0, int __base = 10)
{ return __gnu_cxx::__stoa(&std::wcstoll, "stoll", __str.c_str(),
__idx, __base); }
inline unsigned long long
stoull(const wstring& __str, size_t* __idx = 0, int __base = 10)
{ return __gnu_cxx::__stoa(&std::wcstoull, "stoull", __str.c_str(),
__idx, __base); }
inline float
stof(const wstring& __str, size_t* __idx = 0)
{ return __gnu_cxx::__stoa(&std::wcstof, "stof", __str.c_str(), __idx); }
inline double
stod(const wstring& __str, size_t* __idx = 0)
{ return __gnu_cxx::__stoa(&std::wcstod, "stod", __str.c_str(), __idx); }
inline long double
stold(const wstring& __str, size_t* __idx = 0)
{ return __gnu_cxx::__stoa(&std::wcstold, "stold", __str.c_str(), __idx); }
#ifndef _GLIBCXX_HAVE_BROKEN_VSWPRINTF
inline wstring
to_wstring(int __val)
{ return __gnu_cxx::__to_xstring<wstring>(&std::vswprintf, 4 * sizeof(int),
L"%d", __val); }
inline wstring
to_wstring(unsigned __val)
{ return __gnu_cxx::__to_xstring<wstring>(&std::vswprintf,
4 * sizeof(unsigned),
L"%u", __val); }
inline wstring
to_wstring(long __val)
{ return __gnu_cxx::__to_xstring<wstring>(&std::vswprintf, 4 * sizeof(long),
L"%ld", __val); }
inline wstring
to_wstring(unsigned long __val)
{ return __gnu_cxx::__to_xstring<wstring>(&std::vswprintf,
4 * sizeof(unsigned long),
L"%lu", __val); }
inline wstring
to_wstring(long long __val)
{ return __gnu_cxx::__to_xstring<wstring>(&std::vswprintf,
4 * sizeof(long long),
L"%lld", __val); }
inline wstring
to_wstring(unsigned long long __val)
{ return __gnu_cxx::__to_xstring<wstring>(&std::vswprintf,
4 * sizeof(unsigned long long),
L"%llu", __val); }
inline wstring
to_wstring(float __val)
{
const int __n =
__gnu_cxx::__numeric_traits<float>::__max_exponent10 + 20;
return __gnu_cxx::__to_xstring<wstring>(&std::vswprintf, __n,
L"%f", __val);
}
inline wstring
to_wstring(double __val)
{
const int __n =
__gnu_cxx::__numeric_traits<double>::__max_exponent10 + 20;
return __gnu_cxx::__to_xstring<wstring>(&std::vswprintf, __n,
L"%f", __val);
}
inline wstring
to_wstring(long double __val)
{
const int __n =
__gnu_cxx::__numeric_traits<long double>::__max_exponent10 + 20;
return __gnu_cxx::__to_xstring<wstring>(&std::vswprintf, __n,
L"%Lf", __val);
}
#endif
#endif
_GLIBCXX_END_NAMESPACE_CXX11
_GLIBCXX_END_NAMESPACE_VERSION
}
#endif
#if __cplusplus >= 201103L
#include <bits/functional_hash.h>
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
#ifndef _GLIBCXX_COMPATIBILITY_CXX0X
template<>
struct hash<string>
: public __hash_base<size_t, string>
{
size_t
operator()(const string& __s) const noexcept
{ return std::_Hash_impl::hash(__s.data(), __s.length()); }
};
template<>
struct __is_fast_hash<hash<string>> : std::false_type
{ };
#ifdef _GLIBCXX_USE_WCHAR_T
template<>
struct hash<wstring>
: public __hash_base<size_t, wstring>
{
size_t
operator()(const wstring& __s) const noexcept
{ return std::_Hash_impl::hash(__s.data(),
__s.length() * sizeof(wchar_t)); }
};
template<>
struct __is_fast_hash<hash<wstring>> : std::false_type
{ };
#endif
#endif
#ifdef _GLIBCXX_USE_CHAR8_T
template<>
struct hash<u8string>
: public __hash_base<size_t, u8string>
{
size_t
operator()(const u8string& __s) const noexcept
{ return std::_Hash_impl::hash(__s.data(),
__s.length() * sizeof(char8_t)); }
};
template<>
struct __is_fast_hash<hash<u8string>> : std::false_type
{ };
#endif
template<>
struct hash<u16string>
: public __hash_base<size_t, u16string>
{
size_t
operator()(const u16string& __s) const noexcept
{ return std::_Hash_impl::hash(__s.data(),
__s.length() * sizeof(char16_t)); }
};
template<>
struct __is_fast_hash<hash<u16string>> : std::false_type
{ };
template<>
struct hash<u32string>
: public __hash_base<size_t, u32string>
{
size_t
operator()(const u32string& __s) const noexcept
{ return std::_Hash_impl::hash(__s.data(),
__s.length() * sizeof(char32_t)); }
};
template<>
struct __is_fast_hash<hash<u32string>> : std::false_type
{ };
#if __cplusplus >= 201402L
#define __cpp_lib_string_udls 201304
inline namespace literals
{
inline namespace string_literals
{
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wliteral-suffix"
_GLIBCXX_DEFAULT_ABI_TAG
inline basic_string<char>
operator""s(const char* __str, size_t __len)
{ return basic_string<char>{__str, __len}; }
#ifdef _GLIBCXX_USE_WCHAR_T
_GLIBCXX_DEFAULT_ABI_TAG
inline basic_string<wchar_t>
operator""s(const wchar_t* __str, size_t __len)
{ return basic_string<wchar_t>{__str, __len}; }
#endif
#ifdef _GLIBCXX_USE_CHAR8_T
_GLIBCXX_DEFAULT_ABI_TAG
inline basic_string<char8_t>
operator""s(const char8_t* __str, size_t __len)
{ return basic_string<char8_t>{__str, __len}; }
#endif
_GLIBCXX_DEFAULT_ABI_TAG
inline basic_string<char16_t>
operator""s(const char16_t* __str, size_t __len)
{ return basic_string<char16_t>{__str, __len}; }
_GLIBCXX_DEFAULT_ABI_TAG
inline basic_string<char32_t>
operator""s(const char32_t* __str, size_t __len)
{ return basic_string<char32_t>{__str, __len}; }
#pragma GCC diagnostic pop
}
}
#if __cplusplus >= 201703L
namespace __detail::__variant
{
template<typename> struct _Never_valueless_alt;
template<typename _Tp, typename _Traits, typename _Alloc>
struct _Never_valueless_alt<std::basic_string<_Tp, _Traits, _Alloc>>
: __and_<
is_nothrow_move_constructible<std::basic_string<_Tp, _Traits, _Alloc>>,
is_nothrow_move_assignable<std::basic_string<_Tp, _Traits, _Alloc>>
>::type
{ };
}
#endif
#endif
_GLIBCXX_END_NAMESPACE_VERSION
}
#endif
#endif