#ifndef _STL_ITERATOR_H
#define _STL_ITERATOR_H 1
#include <bits/cpp_type_traits.h>
#include <bits/stl_iterator_base_types.h>
#include <ext/type_traits.h>
#include <bits/move.h>
#include <bits/ptr_traits.h>
#if __cplusplus >= 201103L
# include <type_traits>
#endif
#if __cplusplus > 201703L
# define __cpp_lib_array_constexpr 201811L
# define __cpp_lib_constexpr_iterator 201811L
#elif __cplusplus == 201703L
# define __cpp_lib_array_constexpr 201803L
#endif
#if __cplusplus > 201703L
# include <compare>
# include <new>
# include <bits/exception_defines.h>
# include <bits/iterator_concepts.h>
#endif
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
#if __cpp_lib_concepts
namespace __detail
{
template<typename _Cat, typename _Limit, typename _Otherwise = _Cat>
using __clamp_iter_cat
= conditional_t<derived_from<_Cat, _Limit>, _Limit, _Otherwise>;
}
#endif
template<typename _Iterator>
class reverse_iterator
: public iterator<typename iterator_traits<_Iterator>::iterator_category,
typename iterator_traits<_Iterator>::value_type,
typename iterator_traits<_Iterator>::difference_type,
typename iterator_traits<_Iterator>::pointer,
typename iterator_traits<_Iterator>::reference>
{
template<typename _Iter>
friend class reverse_iterator;
#if __cpp_lib_concepts
template<typename _Iter>
static constexpr bool __convertible = !is_same_v<_Iter, _Iterator>
&& convertible_to<const _Iter&, _Iterator>;
#endif
protected:
_Iterator current;
typedef iterator_traits<_Iterator>		__traits_type;
public:
typedef _Iterator					iterator_type;
typedef typename __traits_type::pointer		pointer;
#if ! __cpp_lib_concepts
typedef typename __traits_type::difference_type	difference_type;
typedef typename __traits_type::reference		reference;
#else
using iterator_concept
= conditional_t<random_access_iterator<_Iterator>,
random_access_iterator_tag,
bidirectional_iterator_tag>;
using iterator_category
= __detail::__clamp_iter_cat<typename __traits_type::iterator_category,
random_access_iterator_tag>;
using value_type = iter_value_t<_Iterator>;
using difference_type = iter_difference_t<_Iterator>;
using reference = iter_reference_t<_Iterator>;
#endif
_GLIBCXX17_CONSTEXPR
reverse_iterator() : current() { }
explicit _GLIBCXX17_CONSTEXPR
reverse_iterator(iterator_type __x) : current(__x) { }
_GLIBCXX17_CONSTEXPR
reverse_iterator(const reverse_iterator& __x)
: current(__x.current) { }
#if __cplusplus >= 201103L
reverse_iterator& operator=(const reverse_iterator&) = default;
#endif
template<typename _Iter>
#if __cpp_lib_concepts
requires __convertible<_Iter>
#endif
_GLIBCXX17_CONSTEXPR
reverse_iterator(const reverse_iterator<_Iter>& __x)
: current(__x.current) { }
#if __cplusplus >= 201103L
template<typename _Iter>
#if __cpp_lib_concepts
requires __convertible<_Iter>
&& assignable_from<_Iterator&, const _Iter&>
#endif
_GLIBCXX17_CONSTEXPR
reverse_iterator&
operator=(const reverse_iterator<_Iter>& __x)
{
current = __x.current;
return *this;
}
#endif
_GLIBCXX17_CONSTEXPR iterator_type
base() const
{ return current; }
_GLIBCXX17_CONSTEXPR reference
operator*() const
{
_Iterator __tmp = current;
return *--__tmp;
}
_GLIBCXX17_CONSTEXPR pointer
operator->() const
#if __cplusplus > 201703L && __cpp_concepts >= 201907L
requires is_pointer_v<_Iterator>
|| requires(const _Iterator __i) { __i.operator->(); }
#endif
{
_Iterator __tmp = current;
--__tmp;
return _S_to_pointer(__tmp);
}
_GLIBCXX17_CONSTEXPR reverse_iterator&
operator++()
{
--current;
return *this;
}
_GLIBCXX17_CONSTEXPR reverse_iterator
operator++(int)
{
reverse_iterator __tmp = *this;
--current;
return __tmp;
}
_GLIBCXX17_CONSTEXPR reverse_iterator&
operator--()
{
++current;
return *this;
}
_GLIBCXX17_CONSTEXPR reverse_iterator
operator--(int)
{
reverse_iterator __tmp = *this;
++current;
return __tmp;
}
_GLIBCXX17_CONSTEXPR reverse_iterator
operator+(difference_type __n) const
{ return reverse_iterator(current - __n); }
_GLIBCXX17_CONSTEXPR reverse_iterator&
operator+=(difference_type __n)
{
current -= __n;
return *this;
}
_GLIBCXX17_CONSTEXPR reverse_iterator
operator-(difference_type __n) const
{ return reverse_iterator(current + __n); }
_GLIBCXX17_CONSTEXPR reverse_iterator&
operator-=(difference_type __n)
{
current += __n;
return *this;
}
_GLIBCXX17_CONSTEXPR reference
operator[](difference_type __n) const
{ return *(*this + __n); }
#if __cplusplus > 201703L && __cpp_lib_concepts
friend constexpr iter_rvalue_reference_t<_Iterator>
iter_move(const reverse_iterator& __i)
noexcept(is_nothrow_copy_constructible_v<_Iterator>
&& noexcept(ranges::iter_move(--std::declval<_Iterator&>())))
{
auto __tmp = __i.base();
return ranges::iter_move(--__tmp);
}
template<indirectly_swappable<_Iterator> _Iter2>
friend constexpr void
iter_swap(const reverse_iterator& __x,
const reverse_iterator<_Iter2>& __y)
noexcept(is_nothrow_copy_constructible_v<_Iterator>
&& is_nothrow_copy_constructible_v<_Iter2>
&& noexcept(ranges::iter_swap(--std::declval<_Iterator&>(),
--std::declval<_Iter2&>())))
{
auto __xtmp = __x.base();
auto __ytmp = __y.base();
ranges::iter_swap(--__xtmp, --__ytmp);
}
#endif
private:
template<typename _Tp>
static _GLIBCXX17_CONSTEXPR _Tp*
_S_to_pointer(_Tp* __p)
{ return __p; }
template<typename _Tp>
static _GLIBCXX17_CONSTEXPR pointer
_S_to_pointer(_Tp __t)
{ return __t.operator->(); }
};
#if __cplusplus <= 201703L || ! defined __cpp_lib_concepts
template<typename _Iterator>
inline _GLIBCXX17_CONSTEXPR bool
operator==(const reverse_iterator<_Iterator>& __x,
const reverse_iterator<_Iterator>& __y)
{ return __x.base() == __y.base(); }
template<typename _Iterator>
inline _GLIBCXX17_CONSTEXPR bool
operator<(const reverse_iterator<_Iterator>& __x,
const reverse_iterator<_Iterator>& __y)
{ return __y.base() < __x.base(); }
template<typename _Iterator>
inline _GLIBCXX17_CONSTEXPR bool
operator!=(const reverse_iterator<_Iterator>& __x,
const reverse_iterator<_Iterator>& __y)
{ return !(__x == __y); }
template<typename _Iterator>
inline _GLIBCXX17_CONSTEXPR bool
operator>(const reverse_iterator<_Iterator>& __x,
const reverse_iterator<_Iterator>& __y)
{ return __y < __x; }
template<typename _Iterator>
inline _GLIBCXX17_CONSTEXPR bool
operator<=(const reverse_iterator<_Iterator>& __x,
const reverse_iterator<_Iterator>& __y)
{ return !(__y < __x); }
template<typename _Iterator>
inline _GLIBCXX17_CONSTEXPR bool
operator>=(const reverse_iterator<_Iterator>& __x,
const reverse_iterator<_Iterator>& __y)
{ return !(__x < __y); }
template<typename _IteratorL, typename _IteratorR>
inline _GLIBCXX17_CONSTEXPR bool
operator==(const reverse_iterator<_IteratorL>& __x,
const reverse_iterator<_IteratorR>& __y)
{ return __x.base() == __y.base(); }
template<typename _IteratorL, typename _IteratorR>
inline _GLIBCXX17_CONSTEXPR bool
operator<(const reverse_iterator<_IteratorL>& __x,
const reverse_iterator<_IteratorR>& __y)
{ return __x.base() > __y.base(); }
template<typename _IteratorL, typename _IteratorR>
inline _GLIBCXX17_CONSTEXPR bool
operator!=(const reverse_iterator<_IteratorL>& __x,
const reverse_iterator<_IteratorR>& __y)
{ return __x.base() != __y.base(); }
template<typename _IteratorL, typename _IteratorR>
inline _GLIBCXX17_CONSTEXPR bool
operator>(const reverse_iterator<_IteratorL>& __x,
const reverse_iterator<_IteratorR>& __y)
{ return __x.base() < __y.base(); }
template<typename _IteratorL, typename _IteratorR>
inline _GLIBCXX17_CONSTEXPR bool
operator<=(const reverse_iterator<_IteratorL>& __x,
const reverse_iterator<_IteratorR>& __y)
{ return __x.base() >= __y.base(); }
template<typename _IteratorL, typename _IteratorR>
inline _GLIBCXX17_CONSTEXPR bool
operator>=(const reverse_iterator<_IteratorL>& __x,
const reverse_iterator<_IteratorR>& __y)
{ return __x.base() <= __y.base(); }
#else
template<typename _IteratorL, typename _IteratorR>
constexpr bool
operator==(const reverse_iterator<_IteratorL>& __x,
const reverse_iterator<_IteratorR>& __y)
requires requires { { __x.base() == __y.base() } -> convertible_to<bool>; }
{ return __x.base() == __y.base(); }
template<typename _IteratorL, typename _IteratorR>
constexpr bool
operator!=(const reverse_iterator<_IteratorL>& __x,
const reverse_iterator<_IteratorR>& __y)
requires requires { { __x.base() != __y.base() } -> convertible_to<bool>; }
{ return __x.base() != __y.base(); }
template<typename _IteratorL, typename _IteratorR>
constexpr bool
operator<(const reverse_iterator<_IteratorL>& __x,
const reverse_iterator<_IteratorR>& __y)
requires requires { { __x.base() > __y.base() } -> convertible_to<bool>; }
{ return __x.base() > __y.base(); }
template<typename _IteratorL, typename _IteratorR>
constexpr bool
operator>(const reverse_iterator<_IteratorL>& __x,
const reverse_iterator<_IteratorR>& __y)
requires requires { { __x.base() < __y.base() } -> convertible_to<bool>; }
{ return __x.base() < __y.base(); }
template<typename _IteratorL, typename _IteratorR>
constexpr bool
operator<=(const reverse_iterator<_IteratorL>& __x,
const reverse_iterator<_IteratorR>& __y)
requires requires { { __x.base() >= __y.base() } -> convertible_to<bool>; }
{ return __x.base() >= __y.base(); }
template<typename _IteratorL, typename _IteratorR>
constexpr bool
operator>=(const reverse_iterator<_IteratorL>& __x,
const reverse_iterator<_IteratorR>& __y)
requires requires { { __x.base() <= __y.base() } -> convertible_to<bool>; }
{ return __x.base() <= __y.base(); }
template<typename _IteratorL,
three_way_comparable_with<_IteratorL> _IteratorR>
constexpr compare_three_way_result_t<_IteratorL, _IteratorR>
operator<=>(const reverse_iterator<_IteratorL>& __x,
const reverse_iterator<_IteratorR>& __y)
{ return __y.base() <=> __x.base(); }
#endif
#if __cplusplus < 201103L
template<typename _Iterator>
inline typename reverse_iterator<_Iterator>::difference_type
operator-(const reverse_iterator<_Iterator>& __x,
const reverse_iterator<_Iterator>& __y)
{ return __y.base() - __x.base(); }
template<typename _IteratorL, typename _IteratorR>
inline typename reverse_iterator<_IteratorL>::difference_type
operator-(const reverse_iterator<_IteratorL>& __x,
const reverse_iterator<_IteratorR>& __y)
{ return __y.base() - __x.base(); }
#else
template<typename _IteratorL, typename _IteratorR>
inline _GLIBCXX17_CONSTEXPR auto
operator-(const reverse_iterator<_IteratorL>& __x,
const reverse_iterator<_IteratorR>& __y)
-> decltype(__y.base() - __x.base())
{ return __y.base() - __x.base(); }
#endif
template<typename _Iterator>
inline _GLIBCXX17_CONSTEXPR reverse_iterator<_Iterator>
operator+(typename reverse_iterator<_Iterator>::difference_type __n,
const reverse_iterator<_Iterator>& __x)
{ return reverse_iterator<_Iterator>(__x.base() - __n); }
#if __cplusplus >= 201103L
template<typename _Iterator>
inline _GLIBCXX17_CONSTEXPR reverse_iterator<_Iterator>
__make_reverse_iterator(_Iterator __i)
{ return reverse_iterator<_Iterator>(__i); }
# if __cplusplus >= 201402L
#  define __cpp_lib_make_reverse_iterator 201402
template<typename _Iterator>
inline _GLIBCXX17_CONSTEXPR reverse_iterator<_Iterator>
make_reverse_iterator(_Iterator __i)
{ return reverse_iterator<_Iterator>(__i); }
#  if __cplusplus > 201703L && defined __cpp_lib_concepts
template<typename _Iterator1, typename _Iterator2>
requires (!sized_sentinel_for<_Iterator1, _Iterator2>)
inline constexpr bool
disable_sized_sentinel_for<reverse_iterator<_Iterator1>,
reverse_iterator<_Iterator2>> = true;
#  endif
# endif
template<typename _Iterator>
_GLIBCXX20_CONSTEXPR
auto
__niter_base(reverse_iterator<_Iterator> __it)
-> decltype(__make_reverse_iterator(__niter_base(__it.base())))
{ return __make_reverse_iterator(__niter_base(__it.base())); }
template<typename _Iterator>
struct __is_move_iterator<reverse_iterator<_Iterator> >
: __is_move_iterator<_Iterator>
{ };
template<typename _Iterator>
_GLIBCXX20_CONSTEXPR
auto
__miter_base(reverse_iterator<_Iterator> __it)
-> decltype(__make_reverse_iterator(__miter_base(__it.base())))
{ return __make_reverse_iterator(__miter_base(__it.base())); }
#endif
template<typename _Container>
class back_insert_iterator
: public iterator<output_iterator_tag, void, void, void, void>
{
protected:
_Container* container;
public:
typedef _Container          container_type;
#if __cplusplus > 201703L
using difference_type = ptrdiff_t;
constexpr back_insert_iterator() noexcept : container(nullptr) { }
#endif
explicit _GLIBCXX20_CONSTEXPR
back_insert_iterator(_Container& __x)
: container(std::__addressof(__x)) { }
#if __cplusplus < 201103L
back_insert_iterator&
operator=(typename _Container::const_reference __value)
{
container->push_back(__value);
return *this;
}
#else
_GLIBCXX20_CONSTEXPR
back_insert_iterator&
operator=(const typename _Container::value_type& __value)
{
container->push_back(__value);
return *this;
}
_GLIBCXX20_CONSTEXPR
back_insert_iterator&
operator=(typename _Container::value_type&& __value)
{
container->push_back(std::move(__value));
return *this;
}
#endif
_GLIBCXX20_CONSTEXPR
back_insert_iterator&
operator*()
{ return *this; }
_GLIBCXX20_CONSTEXPR
back_insert_iterator&
operator++()
{ return *this; }
_GLIBCXX20_CONSTEXPR
back_insert_iterator
operator++(int)
{ return *this; }
};
template<typename _Container>
_GLIBCXX20_CONSTEXPR
inline back_insert_iterator<_Container>
back_inserter(_Container& __x)
{ return back_insert_iterator<_Container>(__x); }
template<typename _Container>
class front_insert_iterator
: public iterator<output_iterator_tag, void, void, void, void>
{
protected:
_Container* container;
public:
typedef _Container          container_type;
#if __cplusplus > 201703L
using difference_type = ptrdiff_t;
constexpr front_insert_iterator() noexcept : container(nullptr) { }
#endif
explicit _GLIBCXX20_CONSTEXPR
front_insert_iterator(_Container& __x)
: container(std::__addressof(__x)) { }
#if __cplusplus < 201103L
front_insert_iterator&
operator=(typename _Container::const_reference __value)
{
container->push_front(__value);
return *this;
}
#else
_GLIBCXX20_CONSTEXPR
front_insert_iterator&
operator=(const typename _Container::value_type& __value)
{
container->push_front(__value);
return *this;
}
_GLIBCXX20_CONSTEXPR
front_insert_iterator&
operator=(typename _Container::value_type&& __value)
{
container->push_front(std::move(__value));
return *this;
}
#endif
_GLIBCXX20_CONSTEXPR
front_insert_iterator&
operator*()
{ return *this; }
_GLIBCXX20_CONSTEXPR
front_insert_iterator&
operator++()
{ return *this; }
_GLIBCXX20_CONSTEXPR
front_insert_iterator
operator++(int)
{ return *this; }
};
template<typename _Container>
_GLIBCXX20_CONSTEXPR
inline front_insert_iterator<_Container>
front_inserter(_Container& __x)
{ return front_insert_iterator<_Container>(__x); }
template<typename _Container>
class insert_iterator
: public iterator<output_iterator_tag, void, void, void, void>
{
#if __cplusplus > 201703L && defined __cpp_lib_concepts
using _Iter = std::__detail::__range_iter_t<_Container>;
protected:
_Container* container = nullptr;
_Iter iter = _Iter();
#else
typedef typename _Container::iterator		_Iter;
protected:
_Container* container;
_Iter iter;
#endif
public:
typedef _Container          container_type;
#if __cplusplus > 201703L && defined __cpp_lib_concepts
using difference_type = ptrdiff_t;
insert_iterator() = default;
#endif
_GLIBCXX20_CONSTEXPR
insert_iterator(_Container& __x, _Iter __i)
: container(std::__addressof(__x)), iter(__i) {}
#if __cplusplus < 201103L
insert_iterator&
operator=(typename _Container::const_reference __value)
{
iter = container->insert(iter, __value);
++iter;
return *this;
}
#else
_GLIBCXX20_CONSTEXPR
insert_iterator&
operator=(const typename _Container::value_type& __value)
{
iter = container->insert(iter, __value);
++iter;
return *this;
}
_GLIBCXX20_CONSTEXPR
insert_iterator&
operator=(typename _Container::value_type&& __value)
{
iter = container->insert(iter, std::move(__value));
++iter;
return *this;
}
#endif
_GLIBCXX20_CONSTEXPR
insert_iterator&
operator*()
{ return *this; }
_GLIBCXX20_CONSTEXPR
insert_iterator&
operator++()
{ return *this; }
_GLIBCXX20_CONSTEXPR
insert_iterator&
operator++(int)
{ return *this; }
};
#if __cplusplus > 201703L && defined __cpp_lib_concepts
template<typename _Container>
constexpr insert_iterator<_Container>
inserter(_Container& __x, std::__detail::__range_iter_t<_Container> __i)
{ return insert_iterator<_Container>(__x, __i); }
#else
template<typename _Container>
inline insert_iterator<_Container>
inserter(_Container& __x, typename _Container::iterator __i)
{ return insert_iterator<_Container>(__x, __i); }
#endif
_GLIBCXX_END_NAMESPACE_VERSION
}
namespace __gnu_cxx _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
template<typename _Iterator, typename _Container>
class __normal_iterator
{
protected:
_Iterator _M_current;
typedef std::iterator_traits<_Iterator>		__traits_type;
public:
typedef _Iterator					iterator_type;
typedef typename __traits_type::iterator_category iterator_category;
typedef typename __traits_type::value_type  	value_type;
typedef typename __traits_type::difference_type 	difference_type;
typedef typename __traits_type::reference 	reference;
typedef typename __traits_type::pointer   	pointer;
#if __cplusplus > 201703L && __cpp_lib_concepts
using iterator_concept = std::__detail::__iter_concept<_Iterator>;
#endif
_GLIBCXX_CONSTEXPR __normal_iterator() _GLIBCXX_NOEXCEPT
: _M_current(_Iterator()) { }
explicit _GLIBCXX20_CONSTEXPR
__normal_iterator(const _Iterator& __i) _GLIBCXX_NOEXCEPT
: _M_current(__i) { }
template<typename _Iter>
_GLIBCXX20_CONSTEXPR
__normal_iterator(const __normal_iterator<_Iter,
typename __enable_if<
(std::__are_same<_Iter, typename _Container::pointer>::__value),
_Container>::__type>& __i) _GLIBCXX_NOEXCEPT
: _M_current(__i.base()) { }
_GLIBCXX20_CONSTEXPR
reference
operator*() const _GLIBCXX_NOEXCEPT
{ return *_M_current; }
_GLIBCXX20_CONSTEXPR
pointer
operator->() const _GLIBCXX_NOEXCEPT
{ return _M_current; }
_GLIBCXX20_CONSTEXPR
__normal_iterator&
operator++() _GLIBCXX_NOEXCEPT
{
++_M_current;
return *this;
}
_GLIBCXX20_CONSTEXPR
__normal_iterator
operator++(int) _GLIBCXX_NOEXCEPT
{ return __normal_iterator(_M_current++); }
_GLIBCXX20_CONSTEXPR
__normal_iterator&
operator--() _GLIBCXX_NOEXCEPT
{
--_M_current;
return *this;
}
_GLIBCXX20_CONSTEXPR
__normal_iterator
operator--(int) _GLIBCXX_NOEXCEPT
{ return __normal_iterator(_M_current--); }
_GLIBCXX20_CONSTEXPR
reference
operator[](difference_type __n) const _GLIBCXX_NOEXCEPT
{ return _M_current[__n]; }
_GLIBCXX20_CONSTEXPR
__normal_iterator&
operator+=(difference_type __n) _GLIBCXX_NOEXCEPT
{ _M_current += __n; return *this; }
_GLIBCXX20_CONSTEXPR
__normal_iterator
operator+(difference_type __n) const _GLIBCXX_NOEXCEPT
{ return __normal_iterator(_M_current + __n); }
_GLIBCXX20_CONSTEXPR
__normal_iterator&
operator-=(difference_type __n) _GLIBCXX_NOEXCEPT
{ _M_current -= __n; return *this; }
_GLIBCXX20_CONSTEXPR
__normal_iterator
operator-(difference_type __n) const _GLIBCXX_NOEXCEPT
{ return __normal_iterator(_M_current - __n); }
_GLIBCXX20_CONSTEXPR
const _Iterator&
base() const _GLIBCXX_NOEXCEPT
{ return _M_current; }
};
#if __cpp_lib_three_way_comparison
template<typename _IteratorL, typename _IteratorR, typename _Container>
requires requires (_IteratorL __lhs, _IteratorR __rhs)
{ { __lhs == __rhs } -> std::convertible_to<bool>; }
constexpr bool
operator==(const __normal_iterator<_IteratorL, _Container>& __lhs,
const __normal_iterator<_IteratorR, _Container>& __rhs)
noexcept(noexcept(__lhs.base() == __rhs.base()))
{ return __lhs.base() == __rhs.base(); }
template<typename _IteratorL, typename _IteratorR, typename _Container>
constexpr std::__detail::__synth3way_t<_IteratorR, _IteratorL>
operator<=>(const __normal_iterator<_IteratorL, _Container>& __lhs,
const __normal_iterator<_IteratorR, _Container>& __rhs)
noexcept(noexcept(std::__detail::__synth3way(__lhs.base(), __rhs.base())))
{ return std::__detail::__synth3way(__lhs.base(), __rhs.base()); }
#else
template<typename _IteratorL, typename _IteratorR, typename _Container>
_GLIBCXX20_CONSTEXPR
inline bool
operator==(const __normal_iterator<_IteratorL, _Container>& __lhs,
const __normal_iterator<_IteratorR, _Container>& __rhs)
_GLIBCXX_NOEXCEPT
{ return __lhs.base() == __rhs.base(); }
template<typename _Iterator, typename _Container>
_GLIBCXX20_CONSTEXPR
inline bool
operator==(const __normal_iterator<_Iterator, _Container>& __lhs,
const __normal_iterator<_Iterator, _Container>& __rhs)
_GLIBCXX_NOEXCEPT
{ return __lhs.base() == __rhs.base(); }
template<typename _IteratorL, typename _IteratorR, typename _Container>
_GLIBCXX20_CONSTEXPR
inline bool
operator!=(const __normal_iterator<_IteratorL, _Container>& __lhs,
const __normal_iterator<_IteratorR, _Container>& __rhs)
_GLIBCXX_NOEXCEPT
{ return __lhs.base() != __rhs.base(); }
template<typename _Iterator, typename _Container>
_GLIBCXX20_CONSTEXPR
inline bool
operator!=(const __normal_iterator<_Iterator, _Container>& __lhs,
const __normal_iterator<_Iterator, _Container>& __rhs)
_GLIBCXX_NOEXCEPT
{ return __lhs.base() != __rhs.base(); }
template<typename _IteratorL, typename _IteratorR, typename _Container>
inline bool
operator<(const __normal_iterator<_IteratorL, _Container>& __lhs,
const __normal_iterator<_IteratorR, _Container>& __rhs)
_GLIBCXX_NOEXCEPT
{ return __lhs.base() < __rhs.base(); }
template<typename _Iterator, typename _Container>
_GLIBCXX20_CONSTEXPR
inline bool
operator<(const __normal_iterator<_Iterator, _Container>& __lhs,
const __normal_iterator<_Iterator, _Container>& __rhs)
_GLIBCXX_NOEXCEPT
{ return __lhs.base() < __rhs.base(); }
template<typename _IteratorL, typename _IteratorR, typename _Container>
inline bool
operator>(const __normal_iterator<_IteratorL, _Container>& __lhs,
const __normal_iterator<_IteratorR, _Container>& __rhs)
_GLIBCXX_NOEXCEPT
{ return __lhs.base() > __rhs.base(); }
template<typename _Iterator, typename _Container>
_GLIBCXX20_CONSTEXPR
inline bool
operator>(const __normal_iterator<_Iterator, _Container>& __lhs,
const __normal_iterator<_Iterator, _Container>& __rhs)
_GLIBCXX_NOEXCEPT
{ return __lhs.base() > __rhs.base(); }
template<typename _IteratorL, typename _IteratorR, typename _Container>
inline bool
operator<=(const __normal_iterator<_IteratorL, _Container>& __lhs,
const __normal_iterator<_IteratorR, _Container>& __rhs)
_GLIBCXX_NOEXCEPT
{ return __lhs.base() <= __rhs.base(); }
template<typename _Iterator, typename _Container>
_GLIBCXX20_CONSTEXPR
inline bool
operator<=(const __normal_iterator<_Iterator, _Container>& __lhs,
const __normal_iterator<_Iterator, _Container>& __rhs)
_GLIBCXX_NOEXCEPT
{ return __lhs.base() <= __rhs.base(); }
template<typename _IteratorL, typename _IteratorR, typename _Container>
inline bool
operator>=(const __normal_iterator<_IteratorL, _Container>& __lhs,
const __normal_iterator<_IteratorR, _Container>& __rhs)
_GLIBCXX_NOEXCEPT
{ return __lhs.base() >= __rhs.base(); }
template<typename _Iterator, typename _Container>
_GLIBCXX20_CONSTEXPR
inline bool
operator>=(const __normal_iterator<_Iterator, _Container>& __lhs,
const __normal_iterator<_Iterator, _Container>& __rhs)
_GLIBCXX_NOEXCEPT
{ return __lhs.base() >= __rhs.base(); }
#endif
template<typename _IteratorL, typename _IteratorR, typename _Container>
#if __cplusplus >= 201103L
_GLIBCXX20_CONSTEXPR
inline auto
operator-(const __normal_iterator<_IteratorL, _Container>& __lhs,
const __normal_iterator<_IteratorR, _Container>& __rhs) noexcept
-> decltype(__lhs.base() - __rhs.base())
#else
inline typename __normal_iterator<_IteratorL, _Container>::difference_type
operator-(const __normal_iterator<_IteratorL, _Container>& __lhs,
const __normal_iterator<_IteratorR, _Container>& __rhs)
#endif
{ return __lhs.base() - __rhs.base(); }
template<typename _Iterator, typename _Container>
_GLIBCXX20_CONSTEXPR
inline typename __normal_iterator<_Iterator, _Container>::difference_type
operator-(const __normal_iterator<_Iterator, _Container>& __lhs,
const __normal_iterator<_Iterator, _Container>& __rhs)
_GLIBCXX_NOEXCEPT
{ return __lhs.base() - __rhs.base(); }
template<typename _Iterator, typename _Container>
_GLIBCXX20_CONSTEXPR
inline __normal_iterator<_Iterator, _Container>
operator+(typename __normal_iterator<_Iterator, _Container>::difference_type
__n, const __normal_iterator<_Iterator, _Container>& __i)
_GLIBCXX_NOEXCEPT
{ return __normal_iterator<_Iterator, _Container>(__i.base() + __n); }
_GLIBCXX_END_NAMESPACE_VERSION
}
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
template<typename _Iterator, typename _Container>
_GLIBCXX20_CONSTEXPR
_Iterator
__niter_base(__gnu_cxx::__normal_iterator<_Iterator, _Container> __it)
_GLIBCXX_NOEXCEPT_IF(std::is_nothrow_copy_constructible<_Iterator>::value)
{ return __it.base(); }
#if __cplusplus >= 201103L
#if __cplusplus > 201703L && __cpp_lib_concepts
template<semiregular _Sent>
class move_sentinel
{
public:
constexpr
move_sentinel()
noexcept(is_nothrow_default_constructible_v<_Sent>)
: _M_last() { }
constexpr explicit
move_sentinel(_Sent __s)
noexcept(is_nothrow_move_constructible_v<_Sent>)
: _M_last(std::move(__s)) { }
template<typename _S2> requires convertible_to<const _S2&, _Sent>
constexpr
move_sentinel(const move_sentinel<_S2>& __s)
noexcept(is_nothrow_constructible_v<_Sent, const _S2&>)
: _M_last(__s.base())
{ }
template<typename _S2> requires assignable_from<_Sent&, const _S2&>
constexpr move_sentinel&
operator=(const move_sentinel<_S2>& __s)
noexcept(is_nothrow_assignable_v<_Sent, const _S2&>)
{
_M_last = __s.base();
return *this;
}
constexpr _Sent
base() const
noexcept(is_nothrow_copy_constructible_v<_Sent>)
{ return _M_last; }
private:
_Sent _M_last;
};
#endif
namespace __detail
{
#if __cplusplus > 201703L && __cpp_lib_concepts
template<typename _Iterator>
struct __move_iter_cat
{ };
template<typename _Iterator>
requires requires { typename iterator_traits<_Iterator>::iterator_category; }
struct __move_iter_cat<_Iterator>
{
using iterator_category
= __clamp_iter_cat<typename iterator_traits<_Iterator>::iterator_category,
random_access_iterator_tag>;
};
#endif
}
template<typename _Iterator>
class move_iterator
#if __cplusplus > 201703L && __cpp_lib_concepts
: public __detail::__move_iter_cat<_Iterator>
#endif
{
_Iterator _M_current;
using __traits_type = iterator_traits<_Iterator>;
#if ! (__cplusplus > 201703L && __cpp_lib_concepts)
using __base_ref = typename __traits_type::reference;
#endif
template<typename _Iter2>
friend class move_iterator;
#if __cpp_lib_concepts
template<typename _Iter2>
static constexpr bool __convertible = !is_same_v<_Iter2, _Iterator>
&& convertible_to<const _Iter2&, _Iterator>;
#endif
public:
using iterator_type = _Iterator;
#if __cplusplus > 201703L && __cpp_lib_concepts
using iterator_concept = input_iterator_tag;
using value_type = iter_value_t<_Iterator>;
using difference_type = iter_difference_t<_Iterator>;
using pointer = _Iterator;
using reference = iter_rvalue_reference_t<_Iterator>;
#else
typedef typename __traits_type::iterator_category iterator_category;
typedef typename __traits_type::value_type  	value_type;
typedef typename __traits_type::difference_type	difference_type;
typedef _Iterator					pointer;
typedef typename conditional<is_reference<__base_ref>::value,
typename remove_reference<__base_ref>::type&&,
__base_ref>::type		reference;
#endif
_GLIBCXX17_CONSTEXPR
move_iterator()
: _M_current() { }
explicit _GLIBCXX17_CONSTEXPR
move_iterator(iterator_type __i)
: _M_current(std::move(__i)) { }
template<typename _Iter>
#if __cpp_lib_concepts
requires __convertible<_Iter>
#endif
_GLIBCXX17_CONSTEXPR
move_iterator(const move_iterator<_Iter>& __i)
: _M_current(__i._M_current) { }
template<typename _Iter>
#if __cpp_lib_concepts
requires __convertible<_Iter>
&& assignable_from<_Iterator&, const _Iter&>
#endif
_GLIBCXX17_CONSTEXPR
move_iterator& operator=(const move_iterator<_Iter>& __i)
{
_M_current = __i._M_current;
return *this;
}
#if __cplusplus <= 201703L
_GLIBCXX17_CONSTEXPR iterator_type
base() const
{ return _M_current; }
#else
constexpr const iterator_type&
base() const & noexcept
{ return _M_current; }
constexpr iterator_type
base() &&
{ return std::move(_M_current); }
#endif
_GLIBCXX17_CONSTEXPR reference
operator*() const
#if __cplusplus > 201703L && __cpp_lib_concepts
{ return ranges::iter_move(_M_current); }
#else
{ return static_cast<reference>(*_M_current); }
#endif
_GLIBCXX17_CONSTEXPR pointer
operator->() const
{ return _M_current; }
_GLIBCXX17_CONSTEXPR move_iterator&
operator++()
{
++_M_current;
return *this;
}
_GLIBCXX17_CONSTEXPR move_iterator
operator++(int)
{
move_iterator __tmp = *this;
++_M_current;
return __tmp;
}
#if __cpp_lib_concepts
constexpr void
operator++(int) requires (!forward_iterator<_Iterator>)
{ ++_M_current; }
#endif
_GLIBCXX17_CONSTEXPR move_iterator&
operator--()
{
--_M_current;
return *this;
}
_GLIBCXX17_CONSTEXPR move_iterator
operator--(int)
{
move_iterator __tmp = *this;
--_M_current;
return __tmp;
}
_GLIBCXX17_CONSTEXPR move_iterator
operator+(difference_type __n) const
{ return move_iterator(_M_current + __n); }
_GLIBCXX17_CONSTEXPR move_iterator&
operator+=(difference_type __n)
{
_M_current += __n;
return *this;
}
_GLIBCXX17_CONSTEXPR move_iterator
operator-(difference_type __n) const
{ return move_iterator(_M_current - __n); }
_GLIBCXX17_CONSTEXPR move_iterator&
operator-=(difference_type __n)
{
_M_current -= __n;
return *this;
}
_GLIBCXX17_CONSTEXPR reference
operator[](difference_type __n) const
#if __cplusplus > 201703L && __cpp_lib_concepts
{ return ranges::iter_move(_M_current + __n); }
#else
{ return std::move(_M_current[__n]); }
#endif
#if __cplusplus > 201703L && __cpp_lib_concepts
template<sentinel_for<_Iterator> _Sent>
friend constexpr bool
operator==(const move_iterator& __x, const move_sentinel<_Sent>& __y)
{ return __x.base() == __y.base(); }
template<sized_sentinel_for<_Iterator> _Sent>
friend constexpr iter_difference_t<_Iterator>
operator-(const move_sentinel<_Sent>& __x, const move_iterator& __y)
{ return __x.base() - __y.base(); }
template<sized_sentinel_for<_Iterator> _Sent>
friend constexpr iter_difference_t<_Iterator>
operator-(const move_iterator& __x, const move_sentinel<_Sent>& __y)
{ return __x.base() - __y.base(); }
friend constexpr iter_rvalue_reference_t<_Iterator>
iter_move(const move_iterator& __i)
noexcept(noexcept(ranges::iter_move(__i._M_current)))
{ return ranges::iter_move(__i._M_current); }
template<indirectly_swappable<_Iterator> _Iter2>
friend constexpr void
iter_swap(const move_iterator& __x, const move_iterator<_Iter2>& __y)
noexcept(noexcept(ranges::iter_swap(__x._M_current, __y._M_current)))
{ return ranges::iter_swap(__x._M_current, __y._M_current); }
#endif
};
template<typename _IteratorL, typename _IteratorR>
inline _GLIBCXX17_CONSTEXPR bool
operator==(const move_iterator<_IteratorL>& __x,
const move_iterator<_IteratorR>& __y)
#if __cplusplus > 201703L && __cpp_lib_concepts
requires requires { { __x.base() == __y.base() } -> convertible_to<bool>; }
#endif
{ return __x.base() == __y.base(); }
#if __cpp_lib_three_way_comparison
template<typename _IteratorL,
three_way_comparable_with<_IteratorL> _IteratorR>
constexpr compare_three_way_result_t<_IteratorL, _IteratorR>
operator<=>(const move_iterator<_IteratorL>& __x,
const move_iterator<_IteratorR>& __y)
{ return __x.base() <=> __y.base(); }
#else
template<typename _IteratorL, typename _IteratorR>
inline _GLIBCXX17_CONSTEXPR bool
operator!=(const move_iterator<_IteratorL>& __x,
const move_iterator<_IteratorR>& __y)
{ return !(__x == __y); }
#endif
template<typename _IteratorL, typename _IteratorR>
inline _GLIBCXX17_CONSTEXPR bool
operator<(const move_iterator<_IteratorL>& __x,
const move_iterator<_IteratorR>& __y)
#if __cplusplus > 201703L && __cpp_lib_concepts
requires requires { { __x.base() < __y.base() } -> convertible_to<bool>; }
#endif
{ return __x.base() < __y.base(); }
template<typename _IteratorL, typename _IteratorR>
inline _GLIBCXX17_CONSTEXPR bool
operator<=(const move_iterator<_IteratorL>& __x,
const move_iterator<_IteratorR>& __y)
#if __cplusplus > 201703L && __cpp_lib_concepts
requires requires { { __y.base() < __x.base() } -> convertible_to<bool>; }
#endif
{ return !(__y < __x); }
template<typename _IteratorL, typename _IteratorR>
inline _GLIBCXX17_CONSTEXPR bool
operator>(const move_iterator<_IteratorL>& __x,
const move_iterator<_IteratorR>& __y)
#if __cplusplus > 201703L && __cpp_lib_concepts
requires requires { { __y.base() < __x.base() } -> convertible_to<bool>; }
#endif
{ return __y < __x; }
template<typename _IteratorL, typename _IteratorR>
inline _GLIBCXX17_CONSTEXPR bool
operator>=(const move_iterator<_IteratorL>& __x,
const move_iterator<_IteratorR>& __y)
#if __cplusplus > 201703L && __cpp_lib_concepts
requires requires { { __x.base() < __y.base() } -> convertible_to<bool>; }
#endif
{ return !(__x < __y); }
#if ! (__cplusplus > 201703L && __cpp_lib_concepts)
template<typename _Iterator>
inline _GLIBCXX17_CONSTEXPR bool
operator==(const move_iterator<_Iterator>& __x,
const move_iterator<_Iterator>& __y)
{ return __x.base() == __y.base(); }
template<typename _Iterator>
inline _GLIBCXX17_CONSTEXPR bool
operator!=(const move_iterator<_Iterator>& __x,
const move_iterator<_Iterator>& __y)
{ return !(__x == __y); }
template<typename _Iterator>
inline _GLIBCXX17_CONSTEXPR bool
operator<(const move_iterator<_Iterator>& __x,
const move_iterator<_Iterator>& __y)
{ return __x.base() < __y.base(); }
template<typename _Iterator>
inline _GLIBCXX17_CONSTEXPR bool
operator<=(const move_iterator<_Iterator>& __x,
const move_iterator<_Iterator>& __y)
{ return !(__y < __x); }
template<typename _Iterator>
inline _GLIBCXX17_CONSTEXPR bool
operator>(const move_iterator<_Iterator>& __x,
const move_iterator<_Iterator>& __y)
{ return __y < __x; }
template<typename _Iterator>
inline _GLIBCXX17_CONSTEXPR bool
operator>=(const move_iterator<_Iterator>& __x,
const move_iterator<_Iterator>& __y)
{ return !(__x < __y); }
#endif
template<typename _IteratorL, typename _IteratorR>
inline _GLIBCXX17_CONSTEXPR auto
operator-(const move_iterator<_IteratorL>& __x,
const move_iterator<_IteratorR>& __y)
-> decltype(__x.base() - __y.base())
{ return __x.base() - __y.base(); }
template<typename _Iterator>
inline _GLIBCXX17_CONSTEXPR move_iterator<_Iterator>
operator+(typename move_iterator<_Iterator>::difference_type __n,
const move_iterator<_Iterator>& __x)
{ return __x + __n; }
template<typename _Iterator>
inline _GLIBCXX17_CONSTEXPR move_iterator<_Iterator>
make_move_iterator(_Iterator __i)
{ return move_iterator<_Iterator>(std::move(__i)); }
template<typename _Iterator, typename _ReturnType
= typename conditional<__move_if_noexcept_cond
<typename iterator_traits<_Iterator>::value_type>::value,
_Iterator, move_iterator<_Iterator>>::type>
inline _GLIBCXX17_CONSTEXPR _ReturnType
__make_move_if_noexcept_iterator(_Iterator __i)
{ return _ReturnType(__i); }
template<typename _Tp, typename _ReturnType
= typename conditional<__move_if_noexcept_cond<_Tp>::value,
const _Tp*, move_iterator<_Tp*>>::type>
inline _GLIBCXX17_CONSTEXPR _ReturnType
__make_move_if_noexcept_iterator(_Tp* __i)
{ return _ReturnType(__i); }
#if __cplusplus > 201703L && __cpp_lib_concepts
namespace __detail
{
template<typename _It>
concept __common_iter_has_arrow = indirectly_readable<const _It>
&& (requires(const _It& __it) { __it.operator->(); }
|| is_reference_v<iter_reference_t<_It>>
|| constructible_from<iter_value_t<_It>, iter_reference_t<_It>>);
template<typename _It>
concept __common_iter_use_postfix_proxy
= (!requires (_It& __i) { { *__i++ } -> __can_reference; })
&& constructible_from<iter_value_t<_It>, iter_reference_t<_It>>
&& move_constructible<iter_value_t<_It>>;
}
template<input_or_output_iterator _It, sentinel_for<_It> _Sent>
requires (!same_as<_It, _Sent>) && copyable<_It>
class common_iterator
{
template<typename _Tp, typename _Up>
static constexpr bool
_S_noexcept1()
{
if constexpr (is_trivially_default_constructible_v<_Tp>)
return is_nothrow_assignable_v<_Tp, _Up>;
else
return is_nothrow_constructible_v<_Tp, _Up>;
}
template<typename _It2, typename _Sent2>
static constexpr bool
_S_noexcept()
{ return _S_noexcept1<_It, _It2>() && _S_noexcept1<_Sent, _Sent2>(); }
class __arrow_proxy
{
iter_value_t<_It> _M_keep;
constexpr
__arrow_proxy(iter_reference_t<_It>&& __x)
: _M_keep(std::move(__x)) { }
friend class common_iterator;
public:
constexpr const iter_value_t<_It>*
operator->() const noexcept
{ return std::__addressof(_M_keep); }
};
class __postfix_proxy
{
iter_value_t<_It> _M_keep;
constexpr
__postfix_proxy(iter_reference_t<_It>&& __x)
: _M_keep(std::forward<iter_reference_t<_It>>(__x)) { }
friend class common_iterator;
public:
constexpr const iter_value_t<_It>&
operator*() const noexcept
{ return _M_keep; }
};
public:
constexpr
common_iterator()
noexcept(is_nothrow_default_constructible_v<_It>)
requires default_initializable<_It>
: _M_it(), _M_index(0)
{ }
constexpr
common_iterator(_It __i)
noexcept(is_nothrow_move_constructible_v<_It>)
: _M_it(std::move(__i)), _M_index(0)
{ }
constexpr
common_iterator(_Sent __s)
noexcept(is_nothrow_move_constructible_v<_Sent>)
: _M_sent(std::move(__s)), _M_index(1)
{ }
template<typename _It2, typename _Sent2>
requires convertible_to<const _It2&, _It>
&& convertible_to<const _Sent2&, _Sent>
constexpr
common_iterator(const common_iterator<_It2, _Sent2>& __x)
noexcept(_S_noexcept<const _It2&, const _Sent2&>())
: _M_valueless(), _M_index(__x._M_index)
{
if (_M_index == 0)
{
if constexpr (is_trivially_default_constructible_v<_It>)
_M_it = std::move(__x._M_it);
else
::new((void*)std::__addressof(_M_it)) _It(__x._M_it);
}
else if (_M_index == 1)
{
if constexpr (is_trivially_default_constructible_v<_Sent>)
_M_sent = std::move(__x._M_sent);
else
::new((void*)std::__addressof(_M_sent)) _Sent(__x._M_sent);
}
}
constexpr
common_iterator(const common_iterator& __x)
noexcept(_S_noexcept<const _It&, const _Sent&>())
: _M_valueless(), _M_index(__x._M_index)
{
if (_M_index == 0)
{
if constexpr (is_trivially_default_constructible_v<_It>)
_M_it = std::move(__x._M_it);
else
::new((void*)std::__addressof(_M_it)) _It(__x._M_it);
}
else if (_M_index == 1)
{
if constexpr (is_trivially_default_constructible_v<_Sent>)
_M_sent = std::move(__x._M_sent);
else
::new((void*)std::__addressof(_M_sent)) _Sent(__x._M_sent);
}
}
common_iterator&
operator=(const common_iterator& __x)
noexcept(is_nothrow_copy_assignable_v<_It>
&& is_nothrow_copy_assignable_v<_Sent>
&& is_nothrow_copy_constructible_v<_It>
&& is_nothrow_copy_constructible_v<_Sent>)
{
return this->operator=<_It, _Sent>(__x);
}
template<typename _It2, typename _Sent2>
requires convertible_to<const _It2&, _It>
&& convertible_to<const _Sent2&, _Sent>
&& assignable_from<_It&, const _It2&>
&& assignable_from<_Sent&, const _Sent2&>
common_iterator&
operator=(const common_iterator<_It2, _Sent2>& __x)
noexcept(is_nothrow_constructible_v<_It, const _It2&>
&& is_nothrow_constructible_v<_Sent, const _Sent2&>
&& is_nothrow_assignable_v<_It, const _It2&>
&& is_nothrow_assignable_v<_Sent, const _Sent2&>)
{
switch(_M_index << 2 | __x._M_index)
{
case 0b0000:
_M_it = __x._M_it;
break;
case 0b0101:
_M_sent = __x._M_sent;
break;
case 0b0001:
_M_it.~_It();
_M_index = -1;
[[fallthrough]];
case 0b1001:
::new((void*)std::__addressof(_M_sent)) _Sent(__x._M_sent);
_M_index = 1;
break;
case 0b0100:
_M_sent.~_Sent();
_M_index = -1;
[[fallthrough]];
case 0b1000:
::new((void*)std::__addressof(_M_it)) _It(__x._M_it);
_M_index = 0;
break;
default:
__glibcxx_assert(__x._M_has_value());
__builtin_unreachable();
}
return *this;
}
~common_iterator()
{
switch (_M_index)
{
case 0:
_M_it.~_It();
break;
case 1:
_M_sent.~_Sent();
break;
}
}
decltype(auto)
operator*()
{
__glibcxx_assert(_M_index == 0);
return *_M_it;
}
decltype(auto)
operator*() const requires __detail::__dereferenceable<const _It>
{
__glibcxx_assert(_M_index == 0);
return *_M_it;
}
decltype(auto)
operator->() const requires __detail::__common_iter_has_arrow<_It>
{
__glibcxx_assert(_M_index == 0);
if constexpr (is_pointer_v<_It> || requires { _M_it.operator->(); })
return _M_it;
else if constexpr (is_reference_v<iter_reference_t<_It>>)
{
auto&& __tmp = *_M_it;
return std::__addressof(__tmp);
}
else
return __arrow_proxy{*_M_it};
}
common_iterator&
operator++()
{
__glibcxx_assert(_M_index == 0);
++_M_it;
return *this;
}
decltype(auto)
operator++(int)
{
__glibcxx_assert(_M_index == 0);
if constexpr (forward_iterator<_It>)
{
common_iterator __tmp = *this;
++*this;
return __tmp;
}
else if constexpr (!__detail::__common_iter_use_postfix_proxy<_It>)
return _M_it++;
else
{
__postfix_proxy __p(**this);
++*this;
return __p;
}
}
template<typename _It2, sentinel_for<_It> _Sent2>
requires sentinel_for<_Sent, _It2>
friend bool
operator==(const common_iterator& __x,
const common_iterator<_It2, _Sent2>& __y)
{
switch(__x._M_index << 2 | __y._M_index)
{
case 0b0000:
case 0b0101:
return true;
case 0b0001:
return __x._M_it == __y._M_sent;
case 0b0100:
return __x._M_sent == __y._M_it;
default:
__glibcxx_assert(__x._M_has_value());
__glibcxx_assert(__y._M_has_value());
__builtin_unreachable();
}
}
template<typename _It2, sentinel_for<_It> _Sent2>
requires sentinel_for<_Sent, _It2> && equality_comparable_with<_It, _It2>
friend bool
operator==(const common_iterator& __x,
const common_iterator<_It2, _Sent2>& __y)
{
switch(__x._M_index << 2 | __y._M_index)
{
case 0b0101:
return true;
case 0b0000:
return __x._M_it == __y._M_it;
case 0b0001:
return __x._M_it == __y._M_sent;
case 0b0100:
return __x._M_sent == __y._M_it;
default:
__glibcxx_assert(__x._M_has_value());
__glibcxx_assert(__y._M_has_value());
__builtin_unreachable();
}
}
template<sized_sentinel_for<_It> _It2, sized_sentinel_for<_It> _Sent2>
requires sized_sentinel_for<_Sent, _It2>
friend iter_difference_t<_It2>
operator-(const common_iterator& __x,
const common_iterator<_It2, _Sent2>& __y)
{
switch(__x._M_index << 2 | __y._M_index)
{
case 0b0101:
return 0;
case 0b0000:
return __x._M_it - __y._M_it;
case 0b0001:
return __x._M_it - __y._M_sent;
case 0b0100:
return __x._M_sent - __y._M_it;
default:
__glibcxx_assert(__x._M_has_value());
__glibcxx_assert(__y._M_has_value());
__builtin_unreachable();
}
}
friend iter_rvalue_reference_t<_It>
iter_move(const common_iterator& __i)
noexcept(noexcept(ranges::iter_move(std::declval<const _It&>())))
requires input_iterator<_It>
{
__glibcxx_assert(__i._M_index == 0);
return ranges::iter_move(__i._M_it);
}
template<indirectly_swappable<_It> _It2, typename _Sent2>
friend void
iter_swap(const common_iterator& __x,
const common_iterator<_It2, _Sent2>& __y)
noexcept(noexcept(ranges::iter_swap(std::declval<const _It&>(),
std::declval<const _It2&>())))
{
__glibcxx_assert(__x._M_index == 0);
__glibcxx_assert(__y._M_index == 0);
return ranges::iter_swap(__x._M_it, __y._M_it);
}
private:
template<input_or_output_iterator _It2, sentinel_for<_It2> _Sent2>
friend class common_iterator;
bool _M_has_value() const noexcept { return _M_index < 2; }
union
{
_It _M_it;
_Sent _M_sent;
unsigned char _M_valueless;
};
unsigned char _M_index;
};
template<typename _It, typename _Sent>
struct incrementable_traits<common_iterator<_It, _Sent>>
{
using difference_type = iter_difference_t<_It>;
};
template<input_iterator _It, typename _Sent>
struct iterator_traits<common_iterator<_It, _Sent>>
{
private:
template<typename _Iter>
struct __ptr
{
using type = void;
};
template<typename _Iter>
requires __detail::__common_iter_has_arrow<_Iter>
struct __ptr<_Iter>
{
using _CIter = common_iterator<_Iter, _Sent>;
using type = decltype(std::declval<const _CIter&>().operator->());
};
static auto
_S_iter_cat()
{
using _Traits = iterator_traits<_It>;
if constexpr (requires { requires derived_from<typename _Traits::iterator_category,
forward_iterator_tag>; })
return forward_iterator_tag{};
else
return input_iterator_tag{};
}
public:
using iterator_concept = conditional_t<forward_iterator<_It>,
forward_iterator_tag, input_iterator_tag>;
using iterator_category = decltype(_S_iter_cat());
using value_type = iter_value_t<_It>;
using difference_type = iter_difference_t<_It>;
using pointer = typename __ptr<_It>::type;
using reference = iter_reference_t<_It>;
};
namespace __detail
{
template<typename _It>
struct __counted_iter_value_type
{ };
template<indirectly_readable _It>
struct __counted_iter_value_type<_It>
{ using value_type = iter_value_t<_It>; };
template<typename _It>
struct __counted_iter_concept
{ };
template<typename _It>
requires requires { typename _It::iterator_concept; }
struct __counted_iter_concept<_It>
{ using iterator_concept = typename _It::iterator_concept; };
template<typename _It>
struct __counted_iter_cat
{ };
template<typename _It>
requires requires { typename _It::iterator_category; }
struct __counted_iter_cat<_It>
{ using iterator_category = typename _It::iterator_category; };
}
template<input_or_output_iterator _It>
class counted_iterator
: public __detail::__counted_iter_value_type<_It>,
public __detail::__counted_iter_concept<_It>,
public __detail::__counted_iter_cat<_It>
{
public:
using iterator_type = _It;
using difference_type = iter_difference_t<_It>;
constexpr counted_iterator() requires default_initializable<_It> = default;
constexpr
counted_iterator(_It __i, iter_difference_t<_It> __n)
: _M_current(std::move(__i)), _M_length(__n)
{ __glibcxx_assert(__n >= 0); }
template<typename _It2>
requires convertible_to<const _It2&, _It>
constexpr
counted_iterator(const counted_iterator<_It2>& __x)
: _M_current(__x._M_current), _M_length(__x._M_length)
{ }
template<typename _It2>
requires assignable_from<_It&, const _It2&>
constexpr counted_iterator&
operator=(const counted_iterator<_It2>& __x)
{
_M_current = __x._M_current;
_M_length = __x._M_length;
return *this;
}
constexpr const _It&
base() const & noexcept
{ return _M_current; }
constexpr _It
base() &&
noexcept(is_nothrow_move_constructible_v<_It>)
{ return std::move(_M_current); }
constexpr iter_difference_t<_It>
count() const noexcept { return _M_length; }
constexpr decltype(auto)
operator*()
noexcept(noexcept(*_M_current))
{
__glibcxx_assert( _M_length > 0 );
return *_M_current;
}
constexpr decltype(auto)
operator*() const
noexcept(noexcept(*_M_current))
requires __detail::__dereferenceable<const _It>
{
__glibcxx_assert( _M_length > 0 );
return *_M_current;
}
constexpr auto
operator->() const noexcept
requires contiguous_iterator<_It>
{ return std::to_address(_M_current); }
constexpr counted_iterator&
operator++()
{
__glibcxx_assert(_M_length > 0);
++_M_current;
--_M_length;
return *this;
}
decltype(auto)
operator++(int)
{
__glibcxx_assert(_M_length > 0);
--_M_length;
__try
{
return _M_current++;
} __catch(...) {
++_M_length;
__throw_exception_again;
}
}
constexpr counted_iterator
operator++(int) requires forward_iterator<_It>
{
auto __tmp = *this;
++*this;
return __tmp;
}
constexpr counted_iterator&
operator--() requires bidirectional_iterator<_It>
{
--_M_current;
++_M_length;
return *this;
}
constexpr counted_iterator
operator--(int) requires bidirectional_iterator<_It>
{
auto __tmp = *this;
--*this;
return __tmp;
}
constexpr counted_iterator
operator+(iter_difference_t<_It> __n) const
requires random_access_iterator<_It>
{ return counted_iterator(_M_current + __n, _M_length - __n); }
friend constexpr counted_iterator
operator+(iter_difference_t<_It> __n, const counted_iterator& __x)
requires random_access_iterator<_It>
{ return __x + __n; }
constexpr counted_iterator&
operator+=(iter_difference_t<_It> __n)
requires random_access_iterator<_It>
{
__glibcxx_assert(__n <= _M_length);
_M_current += __n;
_M_length -= __n;
return *this;
}
constexpr counted_iterator
operator-(iter_difference_t<_It> __n) const
requires random_access_iterator<_It>
{ return counted_iterator(_M_current - __n, _M_length + __n); }
template<common_with<_It> _It2>
friend constexpr iter_difference_t<_It2>
operator-(const counted_iterator& __x,
const counted_iterator<_It2>& __y)
{ return __y._M_length - __x._M_length; }
friend constexpr iter_difference_t<_It>
operator-(const counted_iterator& __x, default_sentinel_t)
{ return -__x._M_length; }
friend constexpr iter_difference_t<_It>
operator-(default_sentinel_t, const counted_iterator& __y)
{ return __y._M_length; }
constexpr counted_iterator&
operator-=(iter_difference_t<_It> __n)
requires random_access_iterator<_It>
{
__glibcxx_assert(-__n <= _M_length);
_M_current -= __n;
_M_length += __n;
return *this;
}
constexpr decltype(auto)
operator[](iter_difference_t<_It> __n) const
noexcept(noexcept(_M_current[__n]))
requires random_access_iterator<_It>
{
__glibcxx_assert(__n < _M_length);
return _M_current[__n];
}
template<common_with<_It> _It2>
friend constexpr bool
operator==(const counted_iterator& __x,
const counted_iterator<_It2>& __y)
{ return __x._M_length == __y._M_length; }
friend constexpr bool
operator==(const counted_iterator& __x, default_sentinel_t)
{ return __x._M_length == 0; }
template<common_with<_It> _It2>
friend constexpr strong_ordering
operator<=>(const counted_iterator& __x,
const counted_iterator<_It2>& __y)
{ return __y._M_length <=> __x._M_length; }
friend constexpr iter_rvalue_reference_t<_It>
iter_move(const counted_iterator& __i)
noexcept(noexcept(ranges::iter_move(__i._M_current)))
requires input_iterator<_It>
{
__glibcxx_assert( __i._M_length > 0 );
return ranges::iter_move(__i._M_current);
}
template<indirectly_swappable<_It> _It2>
friend constexpr void
iter_swap(const counted_iterator& __x,
const counted_iterator<_It2>& __y)
noexcept(noexcept(ranges::iter_swap(__x._M_current, __y._M_current)))
{
__glibcxx_assert( __x._M_length > 0 && __y._M_length > 0 );
ranges::iter_swap(__x._M_current, __y._M_current);
}
private:
template<input_or_output_iterator _It2> friend class counted_iterator;
_It _M_current = _It();
iter_difference_t<_It> _M_length = 0;
};
template<input_iterator _It>
requires same_as<__detail::__iter_traits<_It>, iterator_traits<_It>>
struct iterator_traits<counted_iterator<_It>> : iterator_traits<_It>
{
using pointer = conditional_t<contiguous_iterator<_It>,
add_pointer_t<iter_reference_t<_It>>,
void>;
};
#endif
template<typename _Iterator>
_GLIBCXX20_CONSTEXPR
auto
__niter_base(move_iterator<_Iterator> __it)
-> decltype(make_move_iterator(__niter_base(__it.base())))
{ return make_move_iterator(__niter_base(__it.base())); }
template<typename _Iterator>
struct __is_move_iterator<move_iterator<_Iterator> >
{
enum { __value = 1 };
typedef __true_type __type;
};
template<typename _Iterator>
_GLIBCXX20_CONSTEXPR
auto
__miter_base(move_iterator<_Iterator> __it)
-> decltype(__miter_base(__it.base()))
{ return __miter_base(__it.base()); }
#define _GLIBCXX_MAKE_MOVE_ITERATOR(_Iter) std::make_move_iterator(_Iter)
#define _GLIBCXX_MAKE_MOVE_IF_NOEXCEPT_ITERATOR(_Iter) \
std::__make_move_if_noexcept_iterator(_Iter)
#else
#define _GLIBCXX_MAKE_MOVE_ITERATOR(_Iter) (_Iter)
#define _GLIBCXX_MAKE_MOVE_IF_NOEXCEPT_ITERATOR(_Iter) (_Iter)
#endif
#if __cpp_deduction_guides >= 201606
template<typename _InputIterator>
using __iter_key_t = remove_const_t<
typename iterator_traits<_InputIterator>::value_type::first_type>;
template<typename _InputIterator>
using __iter_val_t =
typename iterator_traits<_InputIterator>::value_type::second_type;
template<typename _T1, typename _T2>
struct pair;
template<typename _InputIterator>
using __iter_to_alloc_t =
pair<add_const_t<__iter_key_t<_InputIterator>>,
__iter_val_t<_InputIterator>>;
#endif
_GLIBCXX_END_NAMESPACE_VERSION
}
#ifdef _GLIBCXX_DEBUG
# include <debug/stl_iterator.h>
#endif
#endif