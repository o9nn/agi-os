#ifndef _STL_ITERATOR_BASE_FUNCS_H
#define _STL_ITERATOR_BASE_FUNCS_H 1
#pragma GCC system_header
#include <bits/concept_check.h>
#include <debug/assertions.h>
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
_GLIBCXX_BEGIN_NAMESPACE_CONTAINER
template <typename> struct _List_iterator;
template <typename> struct _List_const_iterator;
_GLIBCXX_END_NAMESPACE_CONTAINER
template<typename _InputIterator>
inline _GLIBCXX14_CONSTEXPR
typename iterator_traits<_InputIterator>::difference_type
__distance(_InputIterator __first, _InputIterator __last,
input_iterator_tag)
{
__glibcxx_function_requires(_InputIteratorConcept<_InputIterator>)
typename iterator_traits<_InputIterator>::difference_type __n = 0;
while (__first != __last)
{
++__first;
++__n;
}
return __n;
}
template<typename _RandomAccessIterator>
inline _GLIBCXX14_CONSTEXPR
typename iterator_traits<_RandomAccessIterator>::difference_type
__distance(_RandomAccessIterator __first, _RandomAccessIterator __last,
random_access_iterator_tag)
{
__glibcxx_function_requires(_RandomAccessIteratorConcept<
_RandomAccessIterator>)
return __last - __first;
}
#if _GLIBCXX_USE_CXX11_ABI
template<typename _Tp>
ptrdiff_t
__distance(_GLIBCXX_STD_C::_List_iterator<_Tp>,
_GLIBCXX_STD_C::_List_iterator<_Tp>,
input_iterator_tag);
template<typename _Tp>
ptrdiff_t
__distance(_GLIBCXX_STD_C::_List_const_iterator<_Tp>,
_GLIBCXX_STD_C::_List_const_iterator<_Tp>,
input_iterator_tag);
#endif
template<typename _InputIterator>
inline _GLIBCXX17_CONSTEXPR
typename iterator_traits<_InputIterator>::difference_type
distance(_InputIterator __first, _InputIterator __last)
{
return std::__distance(__first, __last,
std::__iterator_category(__first));
}
template<typename _InputIterator, typename _Distance>
inline _GLIBCXX14_CONSTEXPR void
__advance(_InputIterator& __i, _Distance __n, input_iterator_tag)
{
__glibcxx_function_requires(_InputIteratorConcept<_InputIterator>)
__glibcxx_assert(__n >= 0);
while (__n--)
++__i;
}
template<typename _BidirectionalIterator, typename _Distance>
inline _GLIBCXX14_CONSTEXPR void
__advance(_BidirectionalIterator& __i, _Distance __n,
bidirectional_iterator_tag)
{
__glibcxx_function_requires(_BidirectionalIteratorConcept<
_BidirectionalIterator>)
if (__n > 0)
while (__n--)
++__i;
else
while (__n++)
--__i;
}
template<typename _RandomAccessIterator, typename _Distance>
inline _GLIBCXX14_CONSTEXPR void
__advance(_RandomAccessIterator& __i, _Distance __n,
random_access_iterator_tag)
{
__glibcxx_function_requires(_RandomAccessIteratorConcept<
_RandomAccessIterator>)
if (__builtin_constant_p(__n) && __n == 1)
++__i;
else if (__builtin_constant_p(__n) && __n == -1)
--__i;
else
__i += __n;
}
template<typename _InputIterator, typename _Distance>
inline _GLIBCXX17_CONSTEXPR void
advance(_InputIterator& __i, _Distance __n)
{
typename iterator_traits<_InputIterator>::difference_type __d = __n;
std::__advance(__i, __d, std::__iterator_category(__i));
}
#if __cplusplus >= 201103L
template<typename _InputIterator>
inline _GLIBCXX17_CONSTEXPR _InputIterator
next(_InputIterator __x, typename
iterator_traits<_InputIterator>::difference_type __n = 1)
{
__glibcxx_function_requires(_InputIteratorConcept<_InputIterator>)
std::advance(__x, __n);
return __x;
}
template<typename _BidirectionalIterator>
inline _GLIBCXX17_CONSTEXPR _BidirectionalIterator
prev(_BidirectionalIterator __x, typename
iterator_traits<_BidirectionalIterator>::difference_type __n = 1)
{
__glibcxx_function_requires(_BidirectionalIteratorConcept<
_BidirectionalIterator>)
std::advance(__x, -__n);
return __x;
}
#endif
_GLIBCXX_END_NAMESPACE_VERSION
}
#endif