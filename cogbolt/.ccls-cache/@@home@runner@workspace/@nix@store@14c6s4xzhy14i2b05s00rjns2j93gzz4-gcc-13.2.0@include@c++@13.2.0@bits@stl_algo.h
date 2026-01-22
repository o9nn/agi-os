#ifndef _STL_ALGO_H
#define _STL_ALGO_H 1
#include <bits/algorithmfwd.h>
#include <bits/stl_algobase.h>
#include <bits/stl_heap.h>
#include <bits/predefined_ops.h>
#if __cplusplus >= 201103L
#include <bits/uniform_int_dist.h>
#endif
#if _GLIBCXX_HOSTED
# include <bits/stl_tempbuf.h>
# if (__cplusplus <= 201103L || _GLIBCXX_USE_DEPRECATED)
#  include <cstdlib>
# endif
#endif
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
template<typename _Iterator, typename _Compare>
_GLIBCXX20_CONSTEXPR
void
__move_median_to_first(_Iterator __result,_Iterator __a, _Iterator __b,
_Iterator __c, _Compare __comp)
{
if (__comp(__a, __b))
{
if (__comp(__b, __c))
std::iter_swap(__result, __b);
else if (__comp(__a, __c))
std::iter_swap(__result, __c);
else
std::iter_swap(__result, __a);
}
else if (__comp(__a, __c))
std::iter_swap(__result, __a);
else if (__comp(__b, __c))
std::iter_swap(__result, __c);
else
std::iter_swap(__result, __b);
}
template<typename _InputIterator, typename _Predicate>
_GLIBCXX20_CONSTEXPR
inline _InputIterator
__find_if_not(_InputIterator __first, _InputIterator __last,
_Predicate __pred)
{
return std::__find_if(__first, __last,
__gnu_cxx::__ops::__negate(__pred),
std::__iterator_category(__first));
}
template<typename _InputIterator, typename _Predicate, typename _Distance>
_GLIBCXX20_CONSTEXPR
_InputIterator
__find_if_not_n(_InputIterator __first, _Distance& __len, _Predicate __pred)
{
for (; __len; --__len,  (void) ++__first)
if (!__pred(__first))
break;
return __first;
}
template<typename _ForwardIterator1, typename _ForwardIterator2,
typename _BinaryPredicate>
_GLIBCXX20_CONSTEXPR
_ForwardIterator1
__search(_ForwardIterator1 __first1, _ForwardIterator1 __last1,
_ForwardIterator2 __first2, _ForwardIterator2 __last2,
_BinaryPredicate  __predicate)
{
if (__first1 == __last1 || __first2 == __last2)
return __first1;
_ForwardIterator2 __p1(__first2);
if (++__p1 == __last2)
return std::__find_if(__first1, __last1,
__gnu_cxx::__ops::__iter_comp_iter(__predicate, __first2));
_ForwardIterator1 __current = __first1;
for (;;)
{
__first1 =
std::__find_if(__first1, __last1,
__gnu_cxx::__ops::__iter_comp_iter(__predicate, __first2));
if (__first1 == __last1)
return __last1;
_ForwardIterator2 __p = __p1;
__current = __first1;
if (++__current == __last1)
return __last1;
while (__predicate(__current, __p))
{
if (++__p == __last2)
return __first1;
if (++__current == __last1)
return __last1;
}
++__first1;
}
return __first1;
}
template<typename _ForwardIterator, typename _Integer,
typename _UnaryPredicate>
_GLIBCXX20_CONSTEXPR
_ForwardIterator
__search_n_aux(_ForwardIterator __first, _ForwardIterator __last,
_Integer __count, _UnaryPredicate __unary_pred,
std::forward_iterator_tag)
{
__first = std::__find_if(__first, __last, __unary_pred);
while (__first != __last)
{
typename iterator_traits<_ForwardIterator>::difference_type
__n = __count;
_ForwardIterator __i = __first;
++__i;
while (__i != __last && __n != 1 && __unary_pred(__i))
{
++__i;
--__n;
}
if (__n == 1)
return __first;
if (__i == __last)
return __last;
__first = std::__find_if(++__i, __last, __unary_pred);
}
return __last;
}
template<typename _RandomAccessIter, typename _Integer,
typename _UnaryPredicate>
_GLIBCXX20_CONSTEXPR
_RandomAccessIter
__search_n_aux(_RandomAccessIter __first, _RandomAccessIter __last,
_Integer __count, _UnaryPredicate __unary_pred,
std::random_access_iterator_tag)
{
typedef typename std::iterator_traits<_RandomAccessIter>::difference_type
_DistanceType;
_DistanceType __tailSize = __last - __first;
_DistanceType __remainder = __count;
while (__remainder <= __tailSize)
{
__first += __remainder;
__tailSize -= __remainder;
_RandomAccessIter __backTrack = __first;
while (__unary_pred(--__backTrack))
{
if (--__remainder == 0)
return (__first - __count);
}
__remainder = __count + 1 - (__first - __backTrack);
}
return __last;
}
template<typename _ForwardIterator, typename _Integer,
typename _UnaryPredicate>
_GLIBCXX20_CONSTEXPR
_ForwardIterator
__search_n(_ForwardIterator __first, _ForwardIterator __last,
_Integer __count,
_UnaryPredicate __unary_pred)
{
if (__count <= 0)
return __first;
if (__count == 1)
return std::__find_if(__first, __last, __unary_pred);
return std::__search_n_aux(__first, __last, __count, __unary_pred,
std::__iterator_category(__first));
}
template<typename _ForwardIterator1, typename _ForwardIterator2,
typename _BinaryPredicate>
_GLIBCXX20_CONSTEXPR
_ForwardIterator1
__find_end(_ForwardIterator1 __first1, _ForwardIterator1 __last1,
_ForwardIterator2 __first2, _ForwardIterator2 __last2,
forward_iterator_tag, forward_iterator_tag,
_BinaryPredicate __comp)
{
if (__first2 == __last2)
return __last1;
_ForwardIterator1 __result = __last1;
while (1)
{
_ForwardIterator1 __new_result
= std::__search(__first1, __last1, __first2, __last2, __comp);
if (__new_result == __last1)
return __result;
else
{
__result = __new_result;
__first1 = __new_result;
++__first1;
}
}
}
template<typename _BidirectionalIterator1, typename _BidirectionalIterator2,
typename _BinaryPredicate>
_GLIBCXX20_CONSTEXPR
_BidirectionalIterator1
__find_end(_BidirectionalIterator1 __first1,
_BidirectionalIterator1 __last1,
_BidirectionalIterator2 __first2,
_BidirectionalIterator2 __last2,
bidirectional_iterator_tag, bidirectional_iterator_tag,
_BinaryPredicate __comp)
{
__glibcxx_function_requires(_BidirectionalIteratorConcept<
_BidirectionalIterator1>)
__glibcxx_function_requires(_BidirectionalIteratorConcept<
_BidirectionalIterator2>)
typedef reverse_iterator<_BidirectionalIterator1> _RevIterator1;
typedef reverse_iterator<_BidirectionalIterator2> _RevIterator2;
_RevIterator1 __rlast1(__first1);
_RevIterator2 __rlast2(__first2);
_RevIterator1 __rresult = std::__search(_RevIterator1(__last1), __rlast1,
_RevIterator2(__last2), __rlast2,
__comp);
if (__rresult == __rlast1)
return __last1;
else
{
_BidirectionalIterator1 __result = __rresult.base();
std::advance(__result, -std::distance(__first2, __last2));
return __result;
}
}
template<typename _ForwardIterator1, typename _ForwardIterator2>
_GLIBCXX20_CONSTEXPR
inline _ForwardIterator1
find_end(_ForwardIterator1 __first1, _ForwardIterator1 __last1,
_ForwardIterator2 __first2, _ForwardIterator2 __last2)
{
__glibcxx_function_requires(_ForwardIteratorConcept<_ForwardIterator1>)
__glibcxx_function_requires(_ForwardIteratorConcept<_ForwardIterator2>)
__glibcxx_function_requires(_EqualOpConcept<
typename iterator_traits<_ForwardIterator1>::value_type,
typename iterator_traits<_ForwardIterator2>::value_type>)
__glibcxx_requires_valid_range(__first1, __last1);
__glibcxx_requires_valid_range(__first2, __last2);
return std::__find_end(__first1, __last1, __first2, __last2,
std::__iterator_category(__first1),
std::__iterator_category(__first2),
__gnu_cxx::__ops::__iter_equal_to_iter());
}
template<typename _ForwardIterator1, typename _ForwardIterator2,
typename _BinaryPredicate>
_GLIBCXX20_CONSTEXPR
inline _ForwardIterator1
find_end(_ForwardIterator1 __first1, _ForwardIterator1 __last1,
_ForwardIterator2 __first2, _ForwardIterator2 __last2,
_BinaryPredicate __comp)
{
__glibcxx_function_requires(_ForwardIteratorConcept<_ForwardIterator1>)
__glibcxx_function_requires(_ForwardIteratorConcept<_ForwardIterator2>)
__glibcxx_function_requires(_BinaryPredicateConcept<_BinaryPredicate,
typename iterator_traits<_ForwardIterator1>::value_type,
typename iterator_traits<_ForwardIterator2>::value_type>)
__glibcxx_requires_valid_range(__first1, __last1);
__glibcxx_requires_valid_range(__first2, __last2);
return std::__find_end(__first1, __last1, __first2, __last2,
std::__iterator_category(__first1),
std::__iterator_category(__first2),
__gnu_cxx::__ops::__iter_comp_iter(__comp));
}
#if __cplusplus >= 201103L
template<typename _InputIterator, typename _Predicate>
_GLIBCXX20_CONSTEXPR
inline bool
all_of(_InputIterator __first, _InputIterator __last, _Predicate __pred)
{ return __last == std::find_if_not(__first, __last, __pred); }
template<typename _InputIterator, typename _Predicate>
_GLIBCXX20_CONSTEXPR
inline bool
none_of(_InputIterator __first, _InputIterator __last, _Predicate __pred)
{ return __last == _GLIBCXX_STD_A::find_if(__first, __last, __pred); }
template<typename _InputIterator, typename _Predicate>
_GLIBCXX20_CONSTEXPR
inline bool
any_of(_InputIterator __first, _InputIterator __last, _Predicate __pred)
{ return !std::none_of(__first, __last, __pred); }
template<typename _InputIterator, typename _Predicate>
_GLIBCXX20_CONSTEXPR
inline _InputIterator
find_if_not(_InputIterator __first, _InputIterator __last,
_Predicate __pred)
{
__glibcxx_function_requires(_InputIteratorConcept<_InputIterator>)
__glibcxx_function_requires(_UnaryPredicateConcept<_Predicate,
typename iterator_traits<_InputIterator>::value_type>)
__glibcxx_requires_valid_range(__first, __last);
return std::__find_if_not(__first, __last,
__gnu_cxx::__ops::__pred_iter(__pred));
}
template<typename _InputIterator, typename _Predicate>
_GLIBCXX20_CONSTEXPR
inline bool
is_partitioned(_InputIterator __first, _InputIterator __last,
_Predicate __pred)
{
__first = std::find_if_not(__first, __last, __pred);
if (__first == __last)
return true;
++__first;
return std::none_of(__first, __last, __pred);
}
template<typename _ForwardIterator, typename _Predicate>
_GLIBCXX20_CONSTEXPR
_ForwardIterator
partition_point(_ForwardIterator __first, _ForwardIterator __last,
_Predicate __pred)
{
__glibcxx_function_requires(_ForwardIteratorConcept<_ForwardIterator>)
__glibcxx_function_requires(_UnaryPredicateConcept<_Predicate,
typename iterator_traits<_ForwardIterator>::value_type>)
__glibcxx_requires_valid_range(__first, __last);
typedef typename iterator_traits<_ForwardIterator>::difference_type
_DistanceType;
_DistanceType __len = std::distance(__first, __last);
while (__len > 0)
{
_DistanceType __half = __len >> 1;
_ForwardIterator __middle = __first;
std::advance(__middle, __half);
if (__pred(*__middle))
{
__first = __middle;
++__first;
__len = __len - __half - 1;
}
else
__len = __half;
}
return __first;
}
#endif
template<typename _InputIterator, typename _OutputIterator,
typename _Predicate>
_GLIBCXX20_CONSTEXPR
_OutputIterator
__remove_copy_if(_InputIterator __first, _InputIterator __last,
_OutputIterator __result, _Predicate __pred)
{
for (; __first != __last; ++__first)
if (!__pred(__first))
{
*__result = *__first;
++__result;
}
return __result;
}
template<typename _InputIterator, typename _OutputIterator, typename _Tp>
_GLIBCXX20_CONSTEXPR
inline _OutputIterator
remove_copy(_InputIterator __first, _InputIterator __last,
_OutputIterator __result, const _Tp& __value)
{
__glibcxx_function_requires(_InputIteratorConcept<_InputIterator>)
__glibcxx_function_requires(_OutputIteratorConcept<_OutputIterator,
typename iterator_traits<_InputIterator>::value_type>)
__glibcxx_function_requires(_EqualOpConcept<
typename iterator_traits<_InputIterator>::value_type, _Tp>)
__glibcxx_requires_valid_range(__first, __last);
return std::__remove_copy_if(__first, __last, __result,
__gnu_cxx::__ops::__iter_equals_val(__value));
}
template<typename _InputIterator, typename _OutputIterator,
typename _Predicate>
_GLIBCXX20_CONSTEXPR
inline _OutputIterator
remove_copy_if(_InputIterator __first, _InputIterator __last,
_OutputIterator __result, _Predicate __pred)
{
__glibcxx_function_requires(_InputIteratorConcept<_InputIterator>)
__glibcxx_function_requires(_OutputIteratorConcept<_OutputIterator,
typename iterator_traits<_InputIterator>::value_type>)
__glibcxx_function_requires(_UnaryPredicateConcept<_Predicate,
typename iterator_traits<_InputIterator>::value_type>)
__glibcxx_requires_valid_range(__first, __last);
return std::__remove_copy_if(__first, __last, __result,
__gnu_cxx::__ops::__pred_iter(__pred));
}
#if __cplusplus >= 201103L
template<typename _InputIterator, typename _OutputIterator,
typename _Predicate>
_GLIBCXX20_CONSTEXPR
_OutputIterator
copy_if(_InputIterator __first, _InputIterator __last,
_OutputIterator __result, _Predicate __pred)
{
__glibcxx_function_requires(_InputIteratorConcept<_InputIterator>)
__glibcxx_function_requires(_OutputIteratorConcept<_OutputIterator,
typename iterator_traits<_InputIterator>::value_type>)
__glibcxx_function_requires(_UnaryPredicateConcept<_Predicate,
typename iterator_traits<_InputIterator>::value_type>)
__glibcxx_requires_valid_range(__first, __last);
for (; __first != __last; ++__first)
if (__pred(*__first))
{
*__result = *__first;
++__result;
}
return __result;
}
template<typename _InputIterator, typename _Size, typename _OutputIterator>
_GLIBCXX20_CONSTEXPR
_OutputIterator
__copy_n(_InputIterator __first, _Size __n,
_OutputIterator __result, input_iterator_tag)
{
return std::__niter_wrap(__result,
__copy_n_a(__first, __n,
std::__niter_base(__result), true));
}
template<typename _RandomAccessIterator, typename _Size,
typename _OutputIterator>
_GLIBCXX20_CONSTEXPR
inline _OutputIterator
__copy_n(_RandomAccessIterator __first, _Size __n,
_OutputIterator __result, random_access_iterator_tag)
{ return std::copy(__first, __first + __n, __result); }
template<typename _InputIterator, typename _Size, typename _OutputIterator>
_GLIBCXX20_CONSTEXPR
inline _OutputIterator
copy_n(_InputIterator __first, _Size __n, _OutputIterator __result)
{
__glibcxx_function_requires(_InputIteratorConcept<_InputIterator>)
__glibcxx_function_requires(_OutputIteratorConcept<_OutputIterator,
typename iterator_traits<_InputIterator>::value_type>)
const auto __n2 = std::__size_to_integer(__n);
if (__n2 <= 0)
return __result;
__glibcxx_requires_can_increment(__first, __n2);
__glibcxx_requires_can_increment(__result, __n2);
return std::__copy_n(__first, __n2, __result,
std::__iterator_category(__first));
}
template<typename _InputIterator, typename _OutputIterator1,
typename _OutputIterator2, typename _Predicate>
_GLIBCXX20_CONSTEXPR
pair<_OutputIterator1, _OutputIterator2>
partition_copy(_InputIterator __first, _InputIterator __last,
_OutputIterator1 __out_true, _OutputIterator2 __out_false,
_Predicate __pred)
{
__glibcxx_function_requires(_InputIteratorConcept<_InputIterator>)
__glibcxx_function_requires(_OutputIteratorConcept<_OutputIterator1,
typename iterator_traits<_InputIterator>::value_type>)
__glibcxx_function_requires(_OutputIteratorConcept<_OutputIterator2,
typename iterator_traits<_InputIterator>::value_type>)
__glibcxx_function_requires(_UnaryPredicateConcept<_Predicate,
typename iterator_traits<_InputIterator>::value_type>)
__glibcxx_requires_valid_range(__first, __last);
for (; __first != __last; ++__first)
if (__pred(*__first))
{
*__out_true = *__first;
++__out_true;
}
else
{
*__out_false = *__first;
++__out_false;
}
return pair<_OutputIterator1, _OutputIterator2>(__out_true, __out_false);
}
#endif
template<typename _ForwardIterator, typename _Tp>
_GLIBCXX20_CONSTEXPR
inline _ForwardIterator
remove(_ForwardIterator __first, _ForwardIterator __last,
const _Tp& __value)
{
__glibcxx_function_requires(_Mutable_ForwardIteratorConcept<
_ForwardIterator>)
__glibcxx_function_requires(_EqualOpConcept<
typename iterator_traits<_ForwardIterator>::value_type, _Tp>)
__glibcxx_requires_valid_range(__first, __last);
return std::__remove_if(__first, __last,
__gnu_cxx::__ops::__iter_equals_val(__value));
}
template<typename _ForwardIterator, typename _Predicate>
_GLIBCXX20_CONSTEXPR
inline _ForwardIterator
remove_if(_ForwardIterator __first, _ForwardIterator __last,
_Predicate __pred)
{
__glibcxx_function_requires(_Mutable_ForwardIteratorConcept<
_ForwardIterator>)
__glibcxx_function_requires(_UnaryPredicateConcept<_Predicate,
typename iterator_traits<_ForwardIterator>::value_type>)
__glibcxx_requires_valid_range(__first, __last);
return std::__remove_if(__first, __last,
__gnu_cxx::__ops::__pred_iter(__pred));
}
template<typename _ForwardIterator, typename _BinaryPredicate>
_GLIBCXX20_CONSTEXPR
_ForwardIterator
__adjacent_find(_ForwardIterator __first, _ForwardIterator __last,
_BinaryPredicate __binary_pred)
{
if (__first == __last)
return __last;
_ForwardIterator __next = __first;
while (++__next != __last)
{
if (__binary_pred(__first, __next))
return __first;
__first = __next;
}
return __last;
}
template<typename _ForwardIterator, typename _BinaryPredicate>
_GLIBCXX20_CONSTEXPR
_ForwardIterator
__unique(_ForwardIterator __first, _ForwardIterator __last,
_BinaryPredicate __binary_pred)
{
__first = std::__adjacent_find(__first, __last, __binary_pred);
if (__first == __last)
return __last;
_ForwardIterator __dest = __first;
++__first;
while (++__first != __last)
if (!__binary_pred(__dest, __first))
*++__dest = _GLIBCXX_MOVE(*__first);
return ++__dest;
}
template<typename _ForwardIterator>
_GLIBCXX20_CONSTEXPR
inline _ForwardIterator
unique(_ForwardIterator __first, _ForwardIterator __last)
{
__glibcxx_function_requires(_Mutable_ForwardIteratorConcept<
_ForwardIterator>)
__glibcxx_function_requires(_EqualityComparableConcept<
typename iterator_traits<_ForwardIterator>::value_type>)
__glibcxx_requires_valid_range(__first, __last);
return std::__unique(__first, __last,
__gnu_cxx::__ops::__iter_equal_to_iter());
}
template<typename _ForwardIterator, typename _BinaryPredicate>
_GLIBCXX20_CONSTEXPR
inline _ForwardIterator
unique(_ForwardIterator __first, _ForwardIterator __last,
_BinaryPredicate __binary_pred)
{
__glibcxx_function_requires(_Mutable_ForwardIteratorConcept<
_ForwardIterator>)
__glibcxx_function_requires(_BinaryPredicateConcept<_BinaryPredicate,
typename iterator_traits<_ForwardIterator>::value_type,
typename iterator_traits<_ForwardIterator>::value_type>)
__glibcxx_requires_valid_range(__first, __last);
return std::__unique(__first, __last,
__gnu_cxx::__ops::__iter_comp_iter(__binary_pred));
}
template<typename _ForwardIterator, typename _OutputIterator,
typename _BinaryPredicate>
_GLIBCXX20_CONSTEXPR
_OutputIterator
__unique_copy(_ForwardIterator __first, _ForwardIterator __last,
_OutputIterator __result, _BinaryPredicate __binary_pred,
forward_iterator_tag, output_iterator_tag)
{
__glibcxx_function_requires(_BinaryPredicateConcept<_BinaryPredicate,
typename iterator_traits<_ForwardIterator>::value_type,
typename iterator_traits<_ForwardIterator>::value_type>)
_ForwardIterator __next = __first;
*__result = *__first;
while (++__next != __last)
if (!__binary_pred(__first, __next))
{
__first = __next;
*++__result = *__first;
}
return ++__result;
}
template<typename _InputIterator, typename _OutputIterator,
typename _BinaryPredicate>
_GLIBCXX20_CONSTEXPR
_OutputIterator
__unique_copy(_InputIterator __first, _InputIterator __last,
_OutputIterator __result, _BinaryPredicate __binary_pred,
input_iterator_tag, output_iterator_tag)
{
__glibcxx_function_requires(_BinaryPredicateConcept<_BinaryPredicate,
typename iterator_traits<_InputIterator>::value_type,
typename iterator_traits<_InputIterator>::value_type>)
typename iterator_traits<_InputIterator>::value_type __value = *__first;
__decltype(__gnu_cxx::__ops::__iter_comp_val(__binary_pred))
__rebound_pred
= __gnu_cxx::__ops::__iter_comp_val(__binary_pred);
*__result = __value;
while (++__first != __last)
if (!__rebound_pred(__first, __value))
{
__value = *__first;
*++__result = __value;
}
return ++__result;
}
template<typename _InputIterator, typename _ForwardIterator,
typename _BinaryPredicate>
_GLIBCXX20_CONSTEXPR
_ForwardIterator
__unique_copy(_InputIterator __first, _InputIterator __last,
_ForwardIterator __result, _BinaryPredicate __binary_pred,
input_iterator_tag, forward_iterator_tag)
{
__glibcxx_function_requires(_BinaryPredicateConcept<_BinaryPredicate,
typename iterator_traits<_ForwardIterator>::value_type,
typename iterator_traits<_InputIterator>::value_type>)
*__result = *__first;
while (++__first != __last)
if (!__binary_pred(__result, __first))
*++__result = *__first;
return ++__result;
}
template<typename _BidirectionalIterator>
_GLIBCXX20_CONSTEXPR
void
__reverse(_BidirectionalIterator __first, _BidirectionalIterator __last,
bidirectional_iterator_tag)
{
while (true)
if (__first == __last || __first == --__last)
return;
else
{
std::iter_swap(__first, __last);
++__first;
}
}
template<typename _RandomAccessIterator>
_GLIBCXX20_CONSTEXPR
void
__reverse(_RandomAccessIterator __first, _RandomAccessIterator __last,
random_access_iterator_tag)
{
if (__first == __last)
return;
--__last;
while (__first < __last)
{
std::iter_swap(__first, __last);
++__first;
--__last;
}
}
template<typename _BidirectionalIterator>
_GLIBCXX20_CONSTEXPR
inline void
reverse(_BidirectionalIterator __first, _BidirectionalIterator __last)
{
__glibcxx_function_requires(_Mutable_BidirectionalIteratorConcept<
_BidirectionalIterator>)
__glibcxx_requires_valid_range(__first, __last);
std::__reverse(__first, __last, std::__iterator_category(__first));
}
template<typename _BidirectionalIterator, typename _OutputIterator>
_GLIBCXX20_CONSTEXPR
_OutputIterator
reverse_copy(_BidirectionalIterator __first, _BidirectionalIterator __last,
_OutputIterator __result)
{
__glibcxx_function_requires(_BidirectionalIteratorConcept<
_BidirectionalIterator>)
__glibcxx_function_requires(_OutputIteratorConcept<_OutputIterator,
typename iterator_traits<_BidirectionalIterator>::value_type>)
__glibcxx_requires_valid_range(__first, __last);
while (__first != __last)
{
--__last;
*__result = *__last;
++__result;
}
return __result;
}
template<typename _EuclideanRingElement>
_GLIBCXX20_CONSTEXPR
_EuclideanRingElement
__gcd(_EuclideanRingElement __m, _EuclideanRingElement __n)
{
while (__n != 0)
{
_EuclideanRingElement __t = __m % __n;
__m = __n;
__n = __t;
}
return __m;
}
_GLIBCXX_BEGIN_INLINE_ABI_NAMESPACE(_V2)
template<typename _ForwardIterator>
_GLIBCXX20_CONSTEXPR
_ForwardIterator
__rotate(_ForwardIterator __first,
_ForwardIterator __middle,
_ForwardIterator __last,
forward_iterator_tag)
{
if (__first == __middle)
return __last;
else if (__last == __middle)
return __first;
_ForwardIterator __first2 = __middle;
do
{
std::iter_swap(__first, __first2);
++__first;
++__first2;
if (__first == __middle)
__middle = __first2;
}
while (__first2 != __last);
_ForwardIterator __ret = __first;
__first2 = __middle;
while (__first2 != __last)
{
std::iter_swap(__first, __first2);
++__first;
++__first2;
if (__first == __middle)
__middle = __first2;
else if (__first2 == __last)
__first2 = __middle;
}
return __ret;
}
template<typename _BidirectionalIterator>
_GLIBCXX20_CONSTEXPR
_BidirectionalIterator
__rotate(_BidirectionalIterator __first,
_BidirectionalIterator __middle,
_BidirectionalIterator __last,
bidirectional_iterator_tag)
{
__glibcxx_function_requires(_Mutable_BidirectionalIteratorConcept<
_BidirectionalIterator>)
if (__first == __middle)
return __last;
else if (__last == __middle)
return __first;
std::__reverse(__first,  __middle, bidirectional_iterator_tag());
std::__reverse(__middle, __last,   bidirectional_iterator_tag());
while (__first != __middle && __middle != __last)
{
std::iter_swap(__first, --__last);
++__first;
}
if (__first == __middle)
{
std::__reverse(__middle, __last,   bidirectional_iterator_tag());
return __last;
}
else
{
std::__reverse(__first,  __middle, bidirectional_iterator_tag());
return __first;
}
}
template<typename _RandomAccessIterator>
_GLIBCXX20_CONSTEXPR
_RandomAccessIterator
__rotate(_RandomAccessIterator __first,
_RandomAccessIterator __middle,
_RandomAccessIterator __last,
random_access_iterator_tag)
{
__glibcxx_function_requires(_Mutable_RandomAccessIteratorConcept<
_RandomAccessIterator>)
if (__first == __middle)
return __last;
else if (__last == __middle)
return __first;
typedef typename iterator_traits<_RandomAccessIterator>::difference_type
_Distance;
typedef typename iterator_traits<_RandomAccessIterator>::value_type
_ValueType;
_Distance __n = __last   - __first;
_Distance __k = __middle - __first;
if (__k == __n - __k)
{
std::swap_ranges(__first, __middle, __middle);
return __middle;
}
_RandomAccessIterator __p = __first;
_RandomAccessIterator __ret = __first + (__last - __middle);
for (;;)
{
if (__k < __n - __k)
{
if (__is_pod(_ValueType) && __k == 1)
{
_ValueType __t = _GLIBCXX_MOVE(*__p);
_GLIBCXX_MOVE3(__p + 1, __p + __n, __p);
*(__p + __n - 1) = _GLIBCXX_MOVE(__t);
return __ret;
}
_RandomAccessIterator __q = __p + __k;
for (_Distance __i = 0; __i < __n - __k; ++ __i)
{
std::iter_swap(__p, __q);
++__p;
++__q;
}
__n %= __k;
if (__n == 0)
return __ret;
std::swap(__n, __k);
__k = __n - __k;
}
else
{
__k = __n - __k;
if (__is_pod(_ValueType) && __k == 1)
{
_ValueType __t = _GLIBCXX_MOVE(*(__p + __n - 1));
_GLIBCXX_MOVE_BACKWARD3(__p, __p + __n - 1, __p + __n);
*__p = _GLIBCXX_MOVE(__t);
return __ret;
}
_RandomAccessIterator __q = __p + __n;
__p = __q - __k;
for (_Distance __i = 0; __i < __n - __k; ++ __i)
{
--__p;
--__q;
std::iter_swap(__p, __q);
}
__n %= __k;
if (__n == 0)
return __ret;
std::swap(__n, __k);
}
}
}
template<typename _ForwardIterator>
_GLIBCXX20_CONSTEXPR
inline _ForwardIterator
rotate(_ForwardIterator __first, _ForwardIterator __middle,
_ForwardIterator __last)
{
__glibcxx_function_requires(_Mutable_ForwardIteratorConcept<
_ForwardIterator>)
__glibcxx_requires_valid_range(__first, __middle);
__glibcxx_requires_valid_range(__middle, __last);
return std::__rotate(__first, __middle, __last,
std::__iterator_category(__first));
}
_GLIBCXX_END_INLINE_ABI_NAMESPACE(_V2)
template<typename _ForwardIterator, typename _OutputIterator>
_GLIBCXX20_CONSTEXPR
inline _OutputIterator
rotate_copy(_ForwardIterator __first, _ForwardIterator __middle,
_ForwardIterator __last, _OutputIterator __result)
{
__glibcxx_function_requires(_ForwardIteratorConcept<_ForwardIterator>)
__glibcxx_function_requires(_OutputIteratorConcept<_OutputIterator,
typename iterator_traits<_ForwardIterator>::value_type>)
__glibcxx_requires_valid_range(__first, __middle);
__glibcxx_requires_valid_range(__middle, __last);
return std::copy(__first, __middle,
std::copy(__middle, __last, __result));
}
template<typename _ForwardIterator, typename _Predicate>
_GLIBCXX20_CONSTEXPR
_ForwardIterator
__partition(_ForwardIterator __first, _ForwardIterator __last,
_Predicate __pred, forward_iterator_tag)
{
if (__first == __last)
return __first;
while (__pred(*__first))
if (++__first == __last)
return __first;
_ForwardIterator __next = __first;
while (++__next != __last)
if (__pred(*__next))
{
std::iter_swap(__first, __next);
++__first;
}
return __first;
}
template<typename _BidirectionalIterator, typename _Predicate>
_GLIBCXX20_CONSTEXPR
_BidirectionalIterator
__partition(_BidirectionalIterator __first, _BidirectionalIterator __last,
_Predicate __pred, bidirectional_iterator_tag)
{
while (true)
{
while (true)
if (__first == __last)
return __first;
else if (__pred(*__first))
++__first;
else
break;
--__last;
while (true)
if (__first == __last)
return __first;
else if (!bool(__pred(*__last)))
--__last;
else
break;
std::iter_swap(__first, __last);
++__first;
}
}
#if _GLIBCXX_HOSTED
template<typename _ForwardIterator, typename _Pointer, typename _Predicate,
typename _Distance>
_ForwardIterator
__stable_partition_adaptive(_ForwardIterator __first,
_ForwardIterator __last,
_Predicate __pred, _Distance __len,
_Pointer __buffer,
_Distance __buffer_size)
{
if (__len == 1)
return __first;
if (__len <= __buffer_size)
{
_ForwardIterator __result1 = __first;
_Pointer __result2 = __buffer;
*__result2 = _GLIBCXX_MOVE(*__first);
++__result2;
++__first;
for (; __first != __last; ++__first)
if (__pred(__first))
{
*__result1 = _GLIBCXX_MOVE(*__first);
++__result1;
}
else
{
*__result2 = _GLIBCXX_MOVE(*__first);
++__result2;
}
_GLIBCXX_MOVE3(__buffer, __result2, __result1);
return __result1;
}
_ForwardIterator __middle = __first;
std::advance(__middle, __len / 2);
_ForwardIterator __left_split =
std::__stable_partition_adaptive(__first, __middle, __pred,
__len / 2, __buffer,
__buffer_size);
_Distance __right_len = __len - __len / 2;
_ForwardIterator __right_split =
std::__find_if_not_n(__middle, __right_len, __pred);
if (__right_len)
__right_split =
std::__stable_partition_adaptive(__right_split, __last, __pred,
__right_len,
__buffer, __buffer_size);
return std::rotate(__left_split, __middle, __right_split);
}
template<typename _ForwardIterator, typename _Predicate>
_ForwardIterator
__stable_partition(_ForwardIterator __first, _ForwardIterator __last,
_Predicate __pred)
{
__first = std::__find_if_not(__first, __last, __pred);
if (__first == __last)
return __first;
typedef typename iterator_traits<_ForwardIterator>::value_type
_ValueType;
typedef typename iterator_traits<_ForwardIterator>::difference_type
_DistanceType;
_Temporary_buffer<_ForwardIterator, _ValueType>
__buf(__first, std::distance(__first, __last));
return
std::__stable_partition_adaptive(__first, __last, __pred,
_DistanceType(__buf.requested_size()),
__buf.begin(),
_DistanceType(__buf.size()));
}
template<typename _ForwardIterator, typename _Predicate>
inline _ForwardIterator
stable_partition(_ForwardIterator __first, _ForwardIterator __last,
_Predicate __pred)
{
__glibcxx_function_requires(_Mutable_ForwardIteratorConcept<
_ForwardIterator>)
__glibcxx_function_requires(_UnaryPredicateConcept<_Predicate,
typename iterator_traits<_ForwardIterator>::value_type>)
__glibcxx_requires_valid_range(__first, __last);
return std::__stable_partition(__first, __last,
__gnu_cxx::__ops::__pred_iter(__pred));
}
#endif
template<typename _RandomAccessIterator, typename _Compare>
_GLIBCXX20_CONSTEXPR
void
__heap_select(_RandomAccessIterator __first,
_RandomAccessIterator __middle,
_RandomAccessIterator __last, _Compare __comp)
{
std::__make_heap(__first, __middle, __comp);
for (_RandomAccessIterator __i = __middle; __i < __last; ++__i)
if (__comp(__i, __first))
std::__pop_heap(__first, __middle, __i, __comp);
}
template<typename _InputIterator, typename _RandomAccessIterator,
typename _Compare>
_GLIBCXX20_CONSTEXPR
_RandomAccessIterator
__partial_sort_copy(_InputIterator __first, _InputIterator __last,
_RandomAccessIterator __result_first,
_RandomAccessIterator __result_last,
_Compare __comp)
{
typedef typename iterator_traits<_InputIterator>::value_type
_InputValueType;
typedef iterator_traits<_RandomAccessIterator> _RItTraits;
typedef typename _RItTraits::difference_type _DistanceType;
if (__result_first == __result_last)
return __result_last;
_RandomAccessIterator __result_real_last = __result_first;
while (__first != __last && __result_real_last != __result_last)
{
*__result_real_last = *__first;
++__result_real_last;
++__first;
}
std::__make_heap(__result_first, __result_real_last, __comp);
while (__first != __last)
{
if (__comp(__first, __result_first))
std::__adjust_heap(__result_first, _DistanceType(0),
_DistanceType(__result_real_last
- __result_first),
_InputValueType(*__first), __comp);
++__first;
}
std::__sort_heap(__result_first, __result_real_last, __comp);
return __result_real_last;
}
template<typename _InputIterator, typename _RandomAccessIterator>
_GLIBCXX20_CONSTEXPR
inline _RandomAccessIterator
partial_sort_copy(_InputIterator __first, _InputIterator __last,
_RandomAccessIterator __result_first,
_RandomAccessIterator __result_last)
{
#ifdef _GLIBCXX_CONCEPT_CHECKS
typedef typename iterator_traits<_InputIterator>::value_type
_InputValueType;
typedef typename iterator_traits<_RandomAccessIterator>::value_type
_OutputValueType;
#endif
__glibcxx_function_requires(_InputIteratorConcept<_InputIterator>)
__glibcxx_function_requires(_ConvertibleConcept<_InputValueType,
_OutputValueType>)
__glibcxx_function_requires(_LessThanOpConcept<_InputValueType,
_OutputValueType>)
__glibcxx_function_requires(_LessThanComparableConcept<_OutputValueType>)
__glibcxx_requires_valid_range(__first, __last);
__glibcxx_requires_irreflexive(__first, __last);
__glibcxx_requires_valid_range(__result_first, __result_last);
return std::__partial_sort_copy(__first, __last,
__result_first, __result_last,
__gnu_cxx::__ops::__iter_less_iter());
}
template<typename _InputIterator, typename _RandomAccessIterator,
typename _Compare>
_GLIBCXX20_CONSTEXPR
inline _RandomAccessIterator
partial_sort_copy(_InputIterator __first, _InputIterator __last,
_RandomAccessIterator __result_first,
_RandomAccessIterator __result_last,
_Compare __comp)
{
#ifdef _GLIBCXX_CONCEPT_CHECKS
typedef typename iterator_traits<_InputIterator>::value_type
_InputValueType;
typedef typename iterator_traits<_RandomAccessIterator>::value_type
_OutputValueType;
#endif
__glibcxx_function_requires(_InputIteratorConcept<_InputIterator>)
__glibcxx_function_requires(_Mutable_RandomAccessIteratorConcept<
_RandomAccessIterator>)
__glibcxx_function_requires(_ConvertibleConcept<_InputValueType,
_OutputValueType>)
__glibcxx_function_requires(_BinaryPredicateConcept<_Compare,
_InputValueType, _OutputValueType>)
__glibcxx_function_requires(_BinaryPredicateConcept<_Compare,
_OutputValueType, _OutputValueType>)
__glibcxx_requires_valid_range(__first, __last);
__glibcxx_requires_irreflexive_pred(__first, __last, __comp);
__glibcxx_requires_valid_range(__result_first, __result_last);
return std::__partial_sort_copy(__first, __last,
__result_first, __result_last,
__gnu_cxx::__ops::__iter_comp_iter(__comp));
}
template<typename _RandomAccessIterator, typename _Compare>
_GLIBCXX20_CONSTEXPR
void
__unguarded_linear_insert(_RandomAccessIterator __last,
_Compare __comp)
{
typename iterator_traits<_RandomAccessIterator>::value_type
__val = _GLIBCXX_MOVE(*__last);
_RandomAccessIterator __next = __last;
--__next;
while (__comp(__val, __next))
{
*__last = _GLIBCXX_MOVE(*__next);
__last = __next;
--__next;
}
*__last = _GLIBCXX_MOVE(__val);
}
template<typename _RandomAccessIterator, typename _Compare>
_GLIBCXX20_CONSTEXPR
void
__insertion_sort(_RandomAccessIterator __first,
_RandomAccessIterator __last, _Compare __comp)
{
if (__first == __last) return;
for (_RandomAccessIterator __i = __first + 1; __i != __last; ++__i)
{
if (__comp(__i, __first))
{
typename iterator_traits<_RandomAccessIterator>::value_type
__val = _GLIBCXX_MOVE(*__i);
_GLIBCXX_MOVE_BACKWARD3(__first, __i, __i + 1);
*__first = _GLIBCXX_MOVE(__val);
}
else
std::__unguarded_linear_insert(__i,
__gnu_cxx::__ops::__val_comp_iter(__comp));
}
}
template<typename _RandomAccessIterator, typename _Compare>
_GLIBCXX20_CONSTEXPR
inline void
__unguarded_insertion_sort(_RandomAccessIterator __first,
_RandomAccessIterator __last, _Compare __comp)
{
for (_RandomAccessIterator __i = __first; __i != __last; ++__i)
std::__unguarded_linear_insert(__i,
__gnu_cxx::__ops::__val_comp_iter(__comp));
}
enum { _S_threshold = 16 };
template<typename _RandomAccessIterator, typename _Compare>
_GLIBCXX20_CONSTEXPR
void
__final_insertion_sort(_RandomAccessIterator __first,
_RandomAccessIterator __last, _Compare __comp)
{
if (__last - __first > int(_S_threshold))
{
std::__insertion_sort(__first, __first + int(_S_threshold), __comp);
std::__unguarded_insertion_sort(__first + int(_S_threshold), __last,
__comp);
}
else
std::__insertion_sort(__first, __last, __comp);
}
template<typename _RandomAccessIterator, typename _Compare>
_GLIBCXX20_CONSTEXPR
_RandomAccessIterator
__unguarded_partition(_RandomAccessIterator __first,
_RandomAccessIterator __last,
_RandomAccessIterator __pivot, _Compare __comp)
{
while (true)
{
while (__comp(__first, __pivot))
++__first;
--__last;
while (__comp(__pivot, __last))
--__last;
if (!(__first < __last))
return __first;
std::iter_swap(__first, __last);
++__first;
}
}
template<typename _RandomAccessIterator, typename _Compare>
_GLIBCXX20_CONSTEXPR
inline _RandomAccessIterator
__unguarded_partition_pivot(_RandomAccessIterator __first,
_RandomAccessIterator __last, _Compare __comp)
{
_RandomAccessIterator __mid = __first + (__last - __first) / 2;
std::__move_median_to_first(__first, __first + 1, __mid, __last - 1,
__comp);
return std::__unguarded_partition(__first + 1, __last, __first, __comp);
}
template<typename _RandomAccessIterator, typename _Compare>
_GLIBCXX20_CONSTEXPR
inline void
__partial_sort(_RandomAccessIterator __first,
_RandomAccessIterator __middle,
_RandomAccessIterator __last,
_Compare __comp)
{
std::__heap_select(__first, __middle, __last, __comp);
std::__sort_heap(__first, __middle, __comp);
}
template<typename _RandomAccessIterator, typename _Size, typename _Compare>
_GLIBCXX20_CONSTEXPR
void
__introsort_loop(_RandomAccessIterator __first,
_RandomAccessIterator __last,
_Size __depth_limit, _Compare __comp)
{
while (__last - __first > int(_S_threshold))
{
if (__depth_limit == 0)
{
std::__partial_sort(__first, __last, __last, __comp);
return;
}
--__depth_limit;
_RandomAccessIterator __cut =
std::__unguarded_partition_pivot(__first, __last, __comp);
std::__introsort_loop(__cut, __last, __depth_limit, __comp);
__last = __cut;
}
}
template<typename _RandomAccessIterator, typename _Compare>
_GLIBCXX20_CONSTEXPR
inline void
__sort(_RandomAccessIterator __first, _RandomAccessIterator __last,
_Compare __comp)
{
if (__first != __last)
{
std::__introsort_loop(__first, __last,
std::__lg(__last - __first) * 2,
__comp);
std::__final_insertion_sort(__first, __last, __comp);
}
}
template<typename _RandomAccessIterator, typename _Size, typename _Compare>
_GLIBCXX20_CONSTEXPR
void
__introselect(_RandomAccessIterator __first, _RandomAccessIterator __nth,
_RandomAccessIterator __last, _Size __depth_limit,
_Compare __comp)
{
while (__last - __first > 3)
{
if (__depth_limit == 0)
{
std::__heap_select(__first, __nth + 1, __last, __comp);
std::iter_swap(__first, __nth);
return;
}
--__depth_limit;
_RandomAccessIterator __cut =
std::__unguarded_partition_pivot(__first, __last, __comp);
if (__cut <= __nth)
__first = __cut;
else
__last = __cut;
}
std::__insertion_sort(__first, __last, __comp);
}
template<typename _ForwardIterator, typename _Tp, typename _Compare>
_GLIBCXX20_CONSTEXPR
inline _ForwardIterator
lower_bound(_ForwardIterator __first, _ForwardIterator __last,
const _Tp& __val, _Compare __comp)
{
__glibcxx_function_requires(_ForwardIteratorConcept<_ForwardIterator>)
__glibcxx_function_requires(_BinaryPredicateConcept<_Compare,
typename iterator_traits<_ForwardIterator>::value_type, _Tp>)
__glibcxx_requires_partitioned_lower_pred(__first, __last,
__val, __comp);
return std::__lower_bound(__first, __last, __val,
__gnu_cxx::__ops::__iter_comp_val(__comp));
}
template<typename _ForwardIterator, typename _Tp, typename _Compare>
_GLIBCXX20_CONSTEXPR
_ForwardIterator
__upper_bound(_ForwardIterator __first, _ForwardIterator __last,
const _Tp& __val, _Compare __comp)
{
typedef typename iterator_traits<_ForwardIterator>::difference_type
_DistanceType;
_DistanceType __len = std::distance(__first, __last);
while (__len > 0)
{
_DistanceType __half = __len >> 1;
_ForwardIterator __middle = __first;
std::advance(__middle, __half);
if (__comp(__val, __middle))
__len = __half;
else
{
__first = __middle;
++__first;
__len = __len - __half - 1;
}
}
return __first;
}
template<typename _ForwardIterator, typename _Tp>
_GLIBCXX20_CONSTEXPR
inline _ForwardIterator
upper_bound(_ForwardIterator __first, _ForwardIterator __last,
const _Tp& __val)
{
__glibcxx_function_requires(_ForwardIteratorConcept<_ForwardIterator>)
__glibcxx_function_requires(_LessThanOpConcept<
_Tp, typename iterator_traits<_ForwardIterator>::value_type>)
__glibcxx_requires_partitioned_upper(__first, __last, __val);
return std::__upper_bound(__first, __last, __val,
__gnu_cxx::__ops::__val_less_iter());
}
template<typename _ForwardIterator, typename _Tp, typename _Compare>
_GLIBCXX20_CONSTEXPR
inline _ForwardIterator
upper_bound(_ForwardIterator __first, _ForwardIterator __last,
const _Tp& __val, _Compare __comp)
{
__glibcxx_function_requires(_ForwardIteratorConcept<_ForwardIterator>)
__glibcxx_function_requires(_BinaryPredicateConcept<_Compare,
_Tp, typename iterator_traits<_ForwardIterator>::value_type>)
__glibcxx_requires_partitioned_upper_pred(__first, __last,
__val, __comp);
return std::__upper_bound(__first, __last, __val,
__gnu_cxx::__ops::__val_comp_iter(__comp));
}
template<typename _ForwardIterator, typename _Tp,
typename _CompareItTp, typename _CompareTpIt>
_GLIBCXX20_CONSTEXPR
pair<_ForwardIterator, _ForwardIterator>
__equal_range(_ForwardIterator __first, _ForwardIterator __last,
const _Tp& __val,
_CompareItTp __comp_it_val, _CompareTpIt __comp_val_it)
{
typedef typename iterator_traits<_ForwardIterator>::difference_type
_DistanceType;
_DistanceType __len = std::distance(__first, __last);
while (__len > 0)
{
_DistanceType __half = __len >> 1;
_ForwardIterator __middle = __first;
std::advance(__middle, __half);
if (__comp_it_val(__middle, __val))
{
__first = __middle;
++__first;
__len = __len - __half - 1;
}
else if (__comp_val_it(__val, __middle))
__len = __half;
else
{
_ForwardIterator __left
= std::__lower_bound(__first, __middle, __val, __comp_it_val);
std::advance(__first, __len);
_ForwardIterator __right
= std::__upper_bound(++__middle, __first, __val, __comp_val_it);
return pair<_ForwardIterator, _ForwardIterator>(__left, __right);
}
}
return pair<_ForwardIterator, _ForwardIterator>(__first, __first);
}
template<typename _ForwardIterator, typename _Tp>
_GLIBCXX20_CONSTEXPR
inline pair<_ForwardIterator, _ForwardIterator>
equal_range(_ForwardIterator __first, _ForwardIterator __last,
const _Tp& __val)
{
__glibcxx_function_requires(_ForwardIteratorConcept<_ForwardIterator>)
__glibcxx_function_requires(_LessThanOpConcept<
typename iterator_traits<_ForwardIterator>::value_type, _Tp>)
__glibcxx_function_requires(_LessThanOpConcept<
_Tp, typename iterator_traits<_ForwardIterator>::value_type>)
__glibcxx_requires_partitioned_lower(__first, __last, __val);
__glibcxx_requires_partitioned_upper(__first, __last, __val);
return std::__equal_range(__first, __last, __val,
__gnu_cxx::__ops::__iter_less_val(),
__gnu_cxx::__ops::__val_less_iter());
}
template<typename _ForwardIterator, typename _Tp, typename _Compare>
_GLIBCXX20_CONSTEXPR
inline pair<_ForwardIterator, _ForwardIterator>
equal_range(_ForwardIterator __first, _ForwardIterator __last,
const _Tp& __val, _Compare __comp)
{
__glibcxx_function_requires(_ForwardIteratorConcept<_ForwardIterator>)
__glibcxx_function_requires(_BinaryPredicateConcept<_Compare,
typename iterator_traits<_ForwardIterator>::value_type, _Tp>)
__glibcxx_function_requires(_BinaryPredicateConcept<_Compare,
_Tp, typename iterator_traits<_ForwardIterator>::value_type>)
__glibcxx_requires_partitioned_lower_pred(__first, __last,
__val, __comp);
__glibcxx_requires_partitioned_upper_pred(__first, __last,
__val, __comp);
return std::__equal_range(__first, __last, __val,
__gnu_cxx::__ops::__iter_comp_val(__comp),
__gnu_cxx::__ops::__val_comp_iter(__comp));
}
template<typename _ForwardIterator, typename _Tp>
_GLIBCXX20_CONSTEXPR
bool
binary_search(_ForwardIterator __first, _ForwardIterator __last,
const _Tp& __val)
{
__glibcxx_function_requires(_ForwardIteratorConcept<_ForwardIterator>)
__glibcxx_function_requires(_LessThanOpConcept<
_Tp, typename iterator_traits<_ForwardIterator>::value_type>)
__glibcxx_requires_partitioned_lower(__first, __last, __val);
__glibcxx_requires_partitioned_upper(__first, __last, __val);
_ForwardIterator __i
= std::__lower_bound(__first, __last, __val,
__gnu_cxx::__ops::__iter_less_val());
return __i != __last && !(__val < *__i);
}
template<typename _ForwardIterator, typename _Tp, typename _Compare>
_GLIBCXX20_CONSTEXPR
bool
binary_search(_ForwardIterator __first, _ForwardIterator __last,
const _Tp& __val, _Compare __comp)
{
__glibcxx_function_requires(_ForwardIteratorConcept<_ForwardIterator>)
__glibcxx_function_requires(_BinaryPredicateConcept<_Compare,
_Tp, typename iterator_traits<_ForwardIterator>::value_type>)
__glibcxx_requires_partitioned_lower_pred(__first, __last,
__val, __comp);
__glibcxx_requires_partitioned_upper_pred(__first, __last,
__val, __comp);
_ForwardIterator __i
= std::__lower_bound(__first, __last, __val,
__gnu_cxx::__ops::__iter_comp_val(__comp));
return __i != __last && !bool(__comp(__val, *__i));
}
template<typename _InputIterator1, typename _InputIterator2,
typename _OutputIterator, typename _Compare>
void
__move_merge_adaptive(_InputIterator1 __first1, _InputIterator1 __last1,
_InputIterator2 __first2, _InputIterator2 __last2,
_OutputIterator __result, _Compare __comp)
{
while (__first1 != __last1 && __first2 != __last2)
{
if (__comp(__first2, __first1))
{
*__result = _GLIBCXX_MOVE(*__first2);
++__first2;
}
else
{
*__result = _GLIBCXX_MOVE(*__first1);
++__first1;
}
++__result;
}
if (__first1 != __last1)
_GLIBCXX_MOVE3(__first1, __last1, __result);
}
template<typename _BidirectionalIterator1, typename _BidirectionalIterator2,
typename _BidirectionalIterator3, typename _Compare>
void
__move_merge_adaptive_backward(_BidirectionalIterator1 __first1,
_BidirectionalIterator1 __last1,
_BidirectionalIterator2 __first2,
_BidirectionalIterator2 __last2,
_BidirectionalIterator3 __result,
_Compare __comp)
{
if (__first1 == __last1)
{
_GLIBCXX_MOVE_BACKWARD3(__first2, __last2, __result);
return;
}
else if (__first2 == __last2)
return;
--__last1;
--__last2;
while (true)
{
if (__comp(__last2, __last1))
{
*--__result = _GLIBCXX_MOVE(*__last1);
if (__first1 == __last1)
{
_GLIBCXX_MOVE_BACKWARD3(__first2, ++__last2, __result);
return;
}
--__last1;
}
else
{
*--__result = _GLIBCXX_MOVE(*__last2);
if (__first2 == __last2)
return;
--__last2;
}
}
}
template<typename _BidirectionalIterator1, typename _BidirectionalIterator2,
typename _Distance>
_BidirectionalIterator1
__rotate_adaptive(_BidirectionalIterator1 __first,
_BidirectionalIterator1 __middle,
_BidirectionalIterator1 __last,
_Distance __len1, _Distance __len2,
_BidirectionalIterator2 __buffer,
_Distance __buffer_size)
{
_BidirectionalIterator2 __buffer_end;
if (__len1 > __len2 && __len2 <= __buffer_size)
{
if (__len2)
{
__buffer_end = _GLIBCXX_MOVE3(__middle, __last, __buffer);
_GLIBCXX_MOVE_BACKWARD3(__first, __middle, __last);
return _GLIBCXX_MOVE3(__buffer, __buffer_end, __first);
}
else
return __first;
}
else if (__len1 <= __buffer_size)
{
if (__len1)
{
__buffer_end = _GLIBCXX_MOVE3(__first, __middle, __buffer);
_GLIBCXX_MOVE3(__middle, __last, __first);
return _GLIBCXX_MOVE_BACKWARD3(__buffer, __buffer_end, __last);
}
else
return __last;
}
else
return std::rotate(__first, __middle, __last);
}
template<typename _BidirectionalIterator, typename _Distance,
typename _Pointer, typename _Compare>
void
__merge_adaptive(_BidirectionalIterator __first,
_BidirectionalIterator __middle,
_BidirectionalIterator __last,
_Distance __len1, _Distance __len2,
_Pointer __buffer, _Compare __comp)
{
if (__len1 <= __len2)
{
_Pointer __buffer_end = _GLIBCXX_MOVE3(__first, __middle, __buffer);
std::__move_merge_adaptive(__buffer, __buffer_end, __middle, __last,
__first, __comp);
}
else
{
_Pointer __buffer_end = _GLIBCXX_MOVE3(__middle, __last, __buffer);
std::__move_merge_adaptive_backward(__first, __middle, __buffer,
__buffer_end, __last, __comp);
}
}
template<typename _BidirectionalIterator, typename _Distance,
typename _Pointer, typename _Compare>
void
__merge_adaptive_resize(_BidirectionalIterator __first,
_BidirectionalIterator __middle,
_BidirectionalIterator __last,
_Distance __len1, _Distance __len2,
_Pointer __buffer, _Distance __buffer_size,
_Compare __comp)
{
if (__len1 <= __buffer_size || __len2 <= __buffer_size)
std::__merge_adaptive(__first, __middle, __last,
__len1, __len2, __buffer, __comp);
else
{
_BidirectionalIterator __first_cut = __first;
_BidirectionalIterator __second_cut = __middle;
_Distance __len11 = 0;
_Distance __len22 = 0;
if (__len1 > __len2)
{
__len11 = __len1 / 2;
std::advance(__first_cut, __len11);
__second_cut
= std::__lower_bound(__middle, __last, *__first_cut,
__gnu_cxx::__ops::__iter_comp_val(__comp));
__len22 = std::distance(__middle, __second_cut);
}
else
{
__len22 = __len2 / 2;
std::advance(__second_cut, __len22);
__first_cut
= std::__upper_bound(__first, __middle, *__second_cut,
__gnu_cxx::__ops::__val_comp_iter(__comp));
__len11 = std::distance(__first, __first_cut);
}
_BidirectionalIterator __new_middle
= std::__rotate_adaptive(__first_cut, __middle, __second_cut,
_Distance(__len1 - __len11), __len22,
__buffer, __buffer_size);
std::__merge_adaptive_resize(__first, __first_cut, __new_middle,
__len11, __len22,
__buffer, __buffer_size, __comp);
std::__merge_adaptive_resize(__new_middle, __second_cut, __last,
_Distance(__len1 - __len11),
_Distance(__len2 - __len22),
__buffer, __buffer_size, __comp);
}
}
template<typename _BidirectionalIterator, typename _Distance,
typename _Compare>
void
__merge_without_buffer(_BidirectionalIterator __first,
_BidirectionalIterator __middle,
_BidirectionalIterator __last,
_Distance __len1, _Distance __len2,
_Compare __comp)
{
if (__len1 == 0 || __len2 == 0)
return;
if (__len1 + __len2 == 2)
{
if (__comp(__middle, __first))
std::iter_swap(__first, __middle);
return;
}
_BidirectionalIterator __first_cut = __first;
_BidirectionalIterator __second_cut = __middle;
_Distance __len11 = 0;
_Distance __len22 = 0;
if (__len1 > __len2)
{
__len11 = __len1 / 2;
std::advance(__first_cut, __len11);
__second_cut
= std::__lower_bound(__middle, __last, *__first_cut,
__gnu_cxx::__ops::__iter_comp_val(__comp));
__len22 = std::distance(__middle, __second_cut);
}
else
{
__len22 = __len2 / 2;
std::advance(__second_cut, __len22);
__first_cut
= std::__upper_bound(__first, __middle, *__second_cut,
__gnu_cxx::__ops::__val_comp_iter(__comp));
__len11 = std::distance(__first, __first_cut);
}
_BidirectionalIterator __new_middle
= std::rotate(__first_cut, __middle, __second_cut);
std::__merge_without_buffer(__first, __first_cut, __new_middle,
__len11, __len22, __comp);
std::__merge_without_buffer(__new_middle, __second_cut, __last,
__len1 - __len11, __len2 - __len22, __comp);
}
template<typename _BidirectionalIterator, typename _Compare>
void
__inplace_merge(_BidirectionalIterator __first,
_BidirectionalIterator __middle,
_BidirectionalIterator __last,
_Compare __comp)
{
typedef typename iterator_traits<_BidirectionalIterator>::value_type
_ValueType;
typedef typename iterator_traits<_BidirectionalIterator>::difference_type
_DistanceType;
if (__first == __middle || __middle == __last)
return;
const _DistanceType __len1 = std::distance(__first, __middle);
const _DistanceType __len2 = std::distance(__middle, __last);
#if _GLIBCXX_HOSTED
typedef _Temporary_buffer<_BidirectionalIterator, _ValueType> _TmpBuf;
_TmpBuf __buf(__first, std::min(__len1, __len2));
if (__builtin_expect(__buf.size() == __buf.requested_size(), true))
std::__merge_adaptive
(__first, __middle, __last, __len1, __len2, __buf.begin(), __comp);
else if (__builtin_expect(__buf.begin() == 0, false))
std::__merge_without_buffer
(__first, __middle, __last, __len1, __len2, __comp);
else
std::__merge_adaptive_resize
(__first, __middle, __last, __len1, __len2, __buf.begin(),
_DistanceType(__buf.size()), __comp);
#else
std::__merge_without_buffer
(__first, __middle, __last, __len1, __len2, __comp);
#endif
}
template<typename _BidirectionalIterator>
inline void
inplace_merge(_BidirectionalIterator __first,
_BidirectionalIterator __middle,
_BidirectionalIterator __last)
{
__glibcxx_function_requires(_Mutable_BidirectionalIteratorConcept<
_BidirectionalIterator>)
__glibcxx_function_requires(_LessThanComparableConcept<
typename iterator_traits<_BidirectionalIterator>::value_type>)
__glibcxx_requires_sorted(__first, __middle);
__glibcxx_requires_sorted(__middle, __last);
__glibcxx_requires_irreflexive(__first, __last);
std::__inplace_merge(__first, __middle, __last,
__gnu_cxx::__ops::__iter_less_iter());
}
template<typename _BidirectionalIterator, typename _Compare>
inline void
inplace_merge(_BidirectionalIterator __first,
_BidirectionalIterator __middle,
_BidirectionalIterator __last,
_Compare __comp)
{
__glibcxx_function_requires(_Mutable_BidirectionalIteratorConcept<
_BidirectionalIterator>)
__glibcxx_function_requires(_BinaryPredicateConcept<_Compare,
typename iterator_traits<_BidirectionalIterator>::value_type,
typename iterator_traits<_BidirectionalIterator>::value_type>)
__glibcxx_requires_sorted_pred(__first, __middle, __comp);
__glibcxx_requires_sorted_pred(__middle, __last, __comp);
__glibcxx_requires_irreflexive_pred(__first, __last, __comp);
std::__inplace_merge(__first, __middle, __last,
__gnu_cxx::__ops::__iter_comp_iter(__comp));
}
template<typename _InputIterator, typename _OutputIterator,
typename _Compare>
_OutputIterator
__move_merge(_InputIterator __first1, _InputIterator __last1,
_InputIterator __first2, _InputIterator __last2,
_OutputIterator __result, _Compare __comp)
{
while (__first1 != __last1 && __first2 != __last2)
{
if (__comp(__first2, __first1))
{
*__result = _GLIBCXX_MOVE(*__first2);
++__first2;
}
else
{
*__result = _GLIBCXX_MOVE(*__first1);
++__first1;
}
++__result;
}
return _GLIBCXX_MOVE3(__first2, __last2,
_GLIBCXX_MOVE3(__first1, __last1,
__result));
}
template<typename _RandomAccessIterator1, typename _RandomAccessIterator2,
typename _Distance, typename _Compare>
void
__merge_sort_loop(_RandomAccessIterator1 __first,
_RandomAccessIterator1 __last,
_RandomAccessIterator2 __result, _Distance __step_size,
_Compare __comp)
{
const _Distance __two_step = 2 * __step_size;
while (__last - __first >= __two_step)
{
__result = std::__move_merge(__first, __first + __step_size,
__first + __step_size,
__first + __two_step,
__result, __comp);
__first += __two_step;
}
__step_size = std::min(_Distance(__last - __first), __step_size);
std::__move_merge(__first, __first + __step_size,
__first + __step_size, __last, __result, __comp);
}
template<typename _RandomAccessIterator, typename _Distance,
typename _Compare>
_GLIBCXX20_CONSTEXPR
void
__chunk_insertion_sort(_RandomAccessIterator __first,
_RandomAccessIterator __last,
_Distance __chunk_size, _Compare __comp)
{
while (__last - __first >= __chunk_size)
{
std::__insertion_sort(__first, __first + __chunk_size, __comp);
__first += __chunk_size;
}
std::__insertion_sort(__first, __last, __comp);
}
enum { _S_chunk_size = 7 };
template<typename _RandomAccessIterator, typename _Pointer, typename _Compare>
void
__merge_sort_with_buffer(_RandomAccessIterator __first,
_RandomAccessIterator __last,
_Pointer __buffer, _Compare __comp)
{
typedef typename iterator_traits<_RandomAccessIterator>::difference_type
_Distance;
const _Distance __len = __last - __first;
const _Pointer __buffer_last = __buffer + __len;
_Distance __step_size = _S_chunk_size;
std::__chunk_insertion_sort(__first, __last, __step_size, __comp);
while (__step_size < __len)
{
std::__merge_sort_loop(__first, __last, __buffer,
__step_size, __comp);
__step_size *= 2;
std::__merge_sort_loop(__buffer, __buffer_last, __first,
__step_size, __comp);
__step_size *= 2;
}
}
template<typename _RandomAccessIterator, typename _Pointer, typename _Compare>
void
__stable_sort_adaptive(_RandomAccessIterator __first,
_RandomAccessIterator __middle,
_RandomAccessIterator __last,
_Pointer __buffer, _Compare __comp)
{
std::__merge_sort_with_buffer(__first, __middle, __buffer, __comp);
std::__merge_sort_with_buffer(__middle, __last, __buffer, __comp);
std::__merge_adaptive(__first, __middle, __last,
__middle - __first, __last - __middle,
__buffer, __comp);
}
template<typename _RandomAccessIterator, typename _Pointer,
typename _Distance, typename _Compare>
void
__stable_sort_adaptive_resize(_RandomAccessIterator __first,
_RandomAccessIterator __last,
_Pointer __buffer, _Distance __buffer_size,
_Compare __comp)
{
const _Distance __len = (__last - __first + 1) / 2;
const _RandomAccessIterator __middle = __first + __len;
if (__len > __buffer_size)
{
std::__stable_sort_adaptive_resize(__first, __middle, __buffer,
__buffer_size, __comp);
std::__stable_sort_adaptive_resize(__middle, __last, __buffer,
__buffer_size, __comp);
std::__merge_adaptive_resize(__first, __middle, __last,
_Distance(__middle - __first),
_Distance(__last - __middle),
__buffer, __buffer_size,
__comp);
}
else
std::__stable_sort_adaptive(__first, __middle, __last,
__buffer, __comp);
}
template<typename _RandomAccessIterator, typename _Compare>
void
__inplace_stable_sort(_RandomAccessIterator __first,
_RandomAccessIterator __last, _Compare __comp)
{
if (__last - __first < 15)
{
std::__insertion_sort(__first, __last, __comp);
return;
}
_RandomAccessIterator __middle = __first + (__last - __first) / 2;
std::__inplace_stable_sort(__first, __middle, __comp);
std::__inplace_stable_sort(__middle, __last, __comp);
std::__merge_without_buffer(__first, __middle, __last,
__middle - __first,
__last - __middle,
__comp);
}
template<typename _InputIterator1, typename _InputIterator2,
typename _Compare>
_GLIBCXX20_CONSTEXPR
bool
__includes(_InputIterator1 __first1, _InputIterator1 __last1,
_InputIterator2 __first2, _InputIterator2 __last2,
_Compare __comp)
{
while (__first1 != __last1 && __first2 != __last2)
{
if (__comp(__first2, __first1))
return false;
if (!__comp(__first1, __first2))
++__first2;
++__first1;
}
return __first2 == __last2;
}
template<typename _InputIterator1, typename _InputIterator2>
_GLIBCXX20_CONSTEXPR
inline bool
includes(_InputIterator1 __first1, _InputIterator1 __last1,
_InputIterator2 __first2, _InputIterator2 __last2)
{
__glibcxx_function_requires(_InputIteratorConcept<_InputIterator1>)
__glibcxx_function_requires(_InputIteratorConcept<_InputIterator2>)
__glibcxx_function_requires(_LessThanOpConcept<
typename iterator_traits<_InputIterator1>::value_type,
typename iterator_traits<_InputIterator2>::value_type>)
__glibcxx_function_requires(_LessThanOpConcept<
typename iterator_traits<_InputIterator2>::value_type,
typename iterator_traits<_InputIterator1>::value_type>)
__glibcxx_requires_sorted_set(__first1, __last1, __first2);
__glibcxx_requires_sorted_set(__first2, __last2, __first1);
__glibcxx_requires_irreflexive2(__first1, __last1);
__glibcxx_requires_irreflexive2(__first2, __last2);
return std::__includes(__first1, __last1, __first2, __last2,
__gnu_cxx::__ops::__iter_less_iter());
}
template<typename _InputIterator1, typename _InputIterator2,
typename _Compare>
_GLIBCXX20_CONSTEXPR
inline bool
includes(_InputIterator1 __first1, _InputIterator1 __last1,
_InputIterator2 __first2, _InputIterator2 __last2,
_Compare __comp)
{
__glibcxx_function_requires(_InputIteratorConcept<_InputIterator1>)
__glibcxx_function_requires(_InputIteratorConcept<_InputIterator2>)
__glibcxx_function_requires(_BinaryPredicateConcept<_Compare,
typename iterator_traits<_InputIterator1>::value_type,
typename iterator_traits<_InputIterator2>::value_type>)
__glibcxx_function_requires(_BinaryPredicateConcept<_Compare,
typename iterator_traits<_InputIterator2>::value_type,
typename iterator_traits<_InputIterator1>::value_type>)
__glibcxx_requires_sorted_set_pred(__first1, __last1, __first2, __comp);
__glibcxx_requires_sorted_set_pred(__first2, __last2, __first1, __comp);
__glibcxx_requires_irreflexive_pred2(__first1, __last1, __comp);
__glibcxx_requires_irreflexive_pred2(__first2, __last2, __comp);
return std::__includes(__first1, __last1, __first2, __last2,
__gnu_cxx::__ops::__iter_comp_iter(__comp));
}
template<typename _BidirectionalIterator, typename _Compare>
_GLIBCXX20_CONSTEXPR
bool
__next_permutation(_BidirectionalIterator __first,
_BidirectionalIterator __last, _Compare __comp)
{
if (__first == __last)
return false;
_BidirectionalIterator __i = __first;
++__i;
if (__i == __last)
return false;
__i = __last;
--__i;
for(;;)
{
_BidirectionalIterator __ii = __i;
--__i;
if (__comp(__i, __ii))
{
_BidirectionalIterator __j = __last;
while (!__comp(__i, --__j))
{}
std::iter_swap(__i, __j);
std::__reverse(__ii, __last,
std::__iterator_category(__first));
return true;
}
if (__i == __first)
{
std::__reverse(__first, __last,
std::__iterator_category(__first));
return false;
}
}
}
template<typename _BidirectionalIterator>
_GLIBCXX20_CONSTEXPR
inline bool
next_permutation(_BidirectionalIterator __first,
_BidirectionalIterator __last)
{
__glibcxx_function_requires(_BidirectionalIteratorConcept<
_BidirectionalIterator>)
__glibcxx_function_requires(_LessThanComparableConcept<
typename iterator_traits<_BidirectionalIterator>::value_type>)
__glibcxx_requires_valid_range(__first, __last);
__glibcxx_requires_irreflexive(__first, __last);
return std::__next_permutation
(__first, __last, __gnu_cxx::__ops::__iter_less_iter());
}
template<typename _BidirectionalIterator, typename _Compare>
_GLIBCXX20_CONSTEXPR
inline bool
next_permutation(_BidirectionalIterator __first,
_BidirectionalIterator __last, _Compare __comp)
{
__glibcxx_function_requires(_BidirectionalIteratorConcept<
_BidirectionalIterator>)
__glibcxx_function_requires(_BinaryPredicateConcept<_Compare,
typename iterator_traits<_BidirectionalIterator>::value_type,
typename iterator_traits<_BidirectionalIterator>::value_type>)
__glibcxx_requires_valid_range(__first, __last);
__glibcxx_requires_irreflexive_pred(__first, __last, __comp);
return std::__next_permutation
(__first, __last, __gnu_cxx::__ops::__iter_comp_iter(__comp));
}
template<typename _BidirectionalIterator, typename _Compare>
_GLIBCXX20_CONSTEXPR
bool
__prev_permutation(_BidirectionalIterator __first,
_BidirectionalIterator __last, _Compare __comp)
{
if (__first == __last)
return false;
_BidirectionalIterator __i = __first;
++__i;
if (__i == __last)
return false;
__i = __last;
--__i;
for(;;)
{
_BidirectionalIterator __ii = __i;
--__i;
if (__comp(__ii, __i))
{
_BidirectionalIterator __j = __last;
while (!__comp(--__j, __i))
{}
std::iter_swap(__i, __j);
std::__reverse(__ii, __last,
std::__iterator_category(__first));
return true;
}
if (__i == __first)
{
std::__reverse(__first, __last,
std::__iterator_category(__first));
return false;
}
}
}
template<typename _BidirectionalIterator>
_GLIBCXX20_CONSTEXPR
inline bool
prev_permutation(_BidirectionalIterator __first,
_BidirectionalIterator __last)
{
__glibcxx_function_requires(_BidirectionalIteratorConcept<
_BidirectionalIterator>)
__glibcxx_function_requires(_LessThanComparableConcept<
typename iterator_traits<_BidirectionalIterator>::value_type>)
__glibcxx_requires_valid_range(__first, __last);
__glibcxx_requires_irreflexive(__first, __last);
return std::__prev_permutation(__first, __last,
__gnu_cxx::__ops::__iter_less_iter());
}
template<typename _BidirectionalIterator, typename _Compare>
_GLIBCXX20_CONSTEXPR
inline bool
prev_permutation(_BidirectionalIterator __first,
_BidirectionalIterator __last, _Compare __comp)
{
__glibcxx_function_requires(_BidirectionalIteratorConcept<
_BidirectionalIterator>)
__glibcxx_function_requires(_BinaryPredicateConcept<_Compare,
typename iterator_traits<_BidirectionalIterator>::value_type,
typename iterator_traits<_BidirectionalIterator>::value_type>)
__glibcxx_requires_valid_range(__first, __last);
__glibcxx_requires_irreflexive_pred(__first, __last, __comp);
return std::__prev_permutation(__first, __last,
__gnu_cxx::__ops::__iter_comp_iter(__comp));
}
template<typename _InputIterator, typename _OutputIterator,
typename _Predicate, typename _Tp>
_GLIBCXX20_CONSTEXPR
_OutputIterator
__replace_copy_if(_InputIterator __first, _InputIterator __last,
_OutputIterator __result,
_Predicate __pred, const _Tp& __new_value)
{
for (; __first != __last; ++__first, (void)++__result)
if (__pred(__first))
*__result = __new_value;
else
*__result = *__first;
return __result;
}
template<typename _InputIterator, typename _OutputIterator, typename _Tp>
_GLIBCXX20_CONSTEXPR
inline _OutputIterator
replace_copy(_InputIterator __first, _InputIterator __last,
_OutputIterator __result,
const _Tp& __old_value, const _Tp& __new_value)
{
__glibcxx_function_requires(_InputIteratorConcept<_InputIterator>)
__glibcxx_function_requires(_OutputIteratorConcept<_OutputIterator,
typename iterator_traits<_InputIterator>::value_type>)
__glibcxx_function_requires(_EqualOpConcept<
typename iterator_traits<_InputIterator>::value_type, _Tp>)
__glibcxx_requires_valid_range(__first, __last);
return std::__replace_copy_if(__first, __last, __result,
__gnu_cxx::__ops::__iter_equals_val(__old_value),
__new_value);
}
template<typename _InputIterator, typename _OutputIterator,
typename _Predicate, typename _Tp>
_GLIBCXX20_CONSTEXPR
inline _OutputIterator
replace_copy_if(_InputIterator __first, _InputIterator __last,
_OutputIterator __result,
_Predicate __pred, const _Tp& __new_value)
{
__glibcxx_function_requires(_InputIteratorConcept<_InputIterator>)
__glibcxx_function_requires(_OutputIteratorConcept<_OutputIterator,
typename iterator_traits<_InputIterator>::value_type>)
__glibcxx_function_requires(_UnaryPredicateConcept<_Predicate,
typename iterator_traits<_InputIterator>::value_type>)
__glibcxx_requires_valid_range(__first, __last);
return std::__replace_copy_if(__first, __last, __result,
__gnu_cxx::__ops::__pred_iter(__pred),
__new_value);
}
#if __cplusplus >= 201103L
template<typename _ForwardIterator>
_GLIBCXX20_CONSTEXPR
inline bool
is_sorted(_ForwardIterator __first, _ForwardIterator __last)
{ return std::is_sorted_until(__first, __last) == __last; }
template<typename _ForwardIterator, typename _Compare>
_GLIBCXX20_CONSTEXPR
inline bool
is_sorted(_ForwardIterator __first, _ForwardIterator __last,
_Compare __comp)
{ return std::is_sorted_until(__first, __last, __comp) == __last; }
template<typename _ForwardIterator, typename _Compare>
_GLIBCXX20_CONSTEXPR
_ForwardIterator
__is_sorted_until(_ForwardIterator __first, _ForwardIterator __last,
_Compare __comp)
{
if (__first == __last)
return __last;
_ForwardIterator __next = __first;
for (++__next; __next != __last; __first = __next, (void)++__next)
if (__comp(__next, __first))
return __next;
return __next;
}
template<typename _ForwardIterator>
_GLIBCXX20_CONSTEXPR
inline _ForwardIterator
is_sorted_until(_ForwardIterator __first, _ForwardIterator __last)
{
__glibcxx_function_requires(_ForwardIteratorConcept<_ForwardIterator>)
__glibcxx_function_requires(_LessThanComparableConcept<
typename iterator_traits<_ForwardIterator>::value_type>)
__glibcxx_requires_valid_range(__first, __last);
__glibcxx_requires_irreflexive(__first, __last);
return std::__is_sorted_until(__first, __last,
__gnu_cxx::__ops::__iter_less_iter());
}
template<typename _ForwardIterator, typename _Compare>
_GLIBCXX20_CONSTEXPR
inline _ForwardIterator
is_sorted_until(_ForwardIterator __first, _ForwardIterator __last,
_Compare __comp)
{
__glibcxx_function_requires(_ForwardIteratorConcept<_ForwardIterator>)
__glibcxx_function_requires(_BinaryPredicateConcept<_Compare,
typename iterator_traits<_ForwardIterator>::value_type,
typename iterator_traits<_ForwardIterator>::value_type>)
__glibcxx_requires_valid_range(__first, __last);
__glibcxx_requires_irreflexive_pred(__first, __last, __comp);
return std::__is_sorted_until(__first, __last,
__gnu_cxx::__ops::__iter_comp_iter(__comp));
}
template<typename _Tp>
_GLIBCXX14_CONSTEXPR
inline pair<const _Tp&, const _Tp&>
minmax(const _Tp& __a, const _Tp& __b)
{
__glibcxx_function_requires(_LessThanComparableConcept<_Tp>)
return __b < __a ? pair<const _Tp&, const _Tp&>(__b, __a)
: pair<const _Tp&, const _Tp&>(__a, __b);
}
template<typename _Tp, typename _Compare>
_GLIBCXX14_CONSTEXPR
inline pair<const _Tp&, const _Tp&>
minmax(const _Tp& __a, const _Tp& __b, _Compare __comp)
{
return __comp(__b, __a) ? pair<const _Tp&, const _Tp&>(__b, __a)
: pair<const _Tp&, const _Tp&>(__a, __b);
}
template<typename _ForwardIterator, typename _Compare>
_GLIBCXX14_CONSTEXPR
pair<_ForwardIterator, _ForwardIterator>
__minmax_element(_ForwardIterator __first, _ForwardIterator __last,
_Compare __comp)
{
_ForwardIterator __next = __first;
if (__first == __last
|| ++__next == __last)
return std::make_pair(__first, __first);
_ForwardIterator __min{}, __max{};
if (__comp(__next, __first))
{
__min = __next;
__max = __first;
}
else
{
__min = __first;
__max = __next;
}
__first = __next;
++__first;
while (__first != __last)
{
__next = __first;
if (++__next == __last)
{
if (__comp(__first, __min))
__min = __first;
else if (!__comp(__first, __max))
__max = __first;
break;
}
if (__comp(__next, __first))
{
if (__comp(__next, __min))
__min = __next;
if (!__comp(__first, __max))
__max = __first;
}
else
{
if (__comp(__first, __min))
__min = __first;
if (!__comp(__next, __max))
__max = __next;
}
__first = __next;
++__first;
}
return std::make_pair(__min, __max);
}
template<typename _ForwardIterator>
_GLIBCXX14_CONSTEXPR
inline pair<_ForwardIterator, _ForwardIterator>
minmax_element(_ForwardIterator __first, _ForwardIterator __last)
{
__glibcxx_function_requires(_ForwardIteratorConcept<_ForwardIterator>)
__glibcxx_function_requires(_LessThanComparableConcept<
typename iterator_traits<_ForwardIterator>::value_type>)
__glibcxx_requires_valid_range(__first, __last);
__glibcxx_requires_irreflexive(__first, __last);
return std::__minmax_element(__first, __last,
__gnu_cxx::__ops::__iter_less_iter());
}
template<typename _ForwardIterator, typename _Compare>
_GLIBCXX14_CONSTEXPR
inline pair<_ForwardIterator, _ForwardIterator>
minmax_element(_ForwardIterator __first, _ForwardIterator __last,
_Compare __comp)
{
__glibcxx_function_requires(_ForwardIteratorConcept<_ForwardIterator>)
__glibcxx_function_requires(_BinaryPredicateConcept<_Compare,
typename iterator_traits<_ForwardIterator>::value_type,
typename iterator_traits<_ForwardIterator>::value_type>)
__glibcxx_requires_valid_range(__first, __last);
__glibcxx_requires_irreflexive_pred(__first, __last, __comp);
return std::__minmax_element(__first, __last,
__gnu_cxx::__ops::__iter_comp_iter(__comp));
}
template<typename _Tp>
_GLIBCXX14_CONSTEXPR
inline pair<_Tp, _Tp>
minmax(initializer_list<_Tp> __l)
{
__glibcxx_requires_irreflexive(__l.begin(), __l.end());
pair<const _Tp*, const _Tp*> __p =
std::__minmax_element(__l.begin(), __l.end(),
__gnu_cxx::__ops::__iter_less_iter());
return std::make_pair(*__p.first, *__p.second);
}
template<typename _Tp, typename _Compare>
_GLIBCXX14_CONSTEXPR
inline pair<_Tp, _Tp>
minmax(initializer_list<_Tp> __l, _Compare __comp)
{
__glibcxx_requires_irreflexive_pred(__l.begin(), __l.end(), __comp);
pair<const _Tp*, const _Tp*> __p =
std::__minmax_element(__l.begin(), __l.end(),
__gnu_cxx::__ops::__iter_comp_iter(__comp));
return std::make_pair(*__p.first, *__p.second);
}
template<typename _ForwardIterator1, typename _ForwardIterator2,
typename _BinaryPredicate>
_GLIBCXX20_CONSTEXPR
inline bool
is_permutation(_ForwardIterator1 __first1, _ForwardIterator1 __last1,
_ForwardIterator2 __first2, _BinaryPredicate __pred)
{
__glibcxx_function_requires(_ForwardIteratorConcept<_ForwardIterator1>)
__glibcxx_function_requires(_ForwardIteratorConcept<_ForwardIterator2>)
__glibcxx_function_requires(_BinaryPredicateConcept<_BinaryPredicate,
typename iterator_traits<_ForwardIterator1>::value_type,
typename iterator_traits<_ForwardIterator2>::value_type>)
__glibcxx_requires_valid_range(__first1, __last1);
return std::__is_permutation(__first1, __last1, __first2,
__gnu_cxx::__ops::__iter_comp_iter(__pred));
}
#if __cplusplus > 201103L
template<typename _ForwardIterator1, typename _ForwardIterator2,
typename _BinaryPredicate>
_GLIBCXX20_CONSTEXPR
bool
__is_permutation(_ForwardIterator1 __first1, _ForwardIterator1 __last1,
_ForwardIterator2 __first2, _ForwardIterator2 __last2,
_BinaryPredicate __pred)
{
using _Cat1
= typename iterator_traits<_ForwardIterator1>::iterator_category;
using _Cat2
= typename iterator_traits<_ForwardIterator2>::iterator_category;
using _It1_is_RA = is_same<_Cat1, random_access_iterator_tag>;
using _It2_is_RA = is_same<_Cat2, random_access_iterator_tag>;
constexpr bool __ra_iters = _It1_is_RA() && _It2_is_RA();
if (__ra_iters)
{
auto __d1 = std::distance(__first1, __last1);
auto __d2 = std::distance(__first2, __last2);
if (__d1 != __d2)
return false;
}
for (; __first1 != __last1 && __first2 != __last2;
++__first1, (void)++__first2)
if (!__pred(__first1, __first2))
break;
if (__ra_iters)
{
if (__first1 == __last1)
return true;
}
else
{
auto __d1 = std::distance(__first1, __last1);
auto __d2 = std::distance(__first2, __last2);
if (__d1 == 0 && __d2 == 0)
return true;
if (__d1 != __d2)
return false;
}
for (_ForwardIterator1 __scan = __first1; __scan != __last1; ++__scan)
{
if (__scan != std::__find_if(__first1, __scan,
__gnu_cxx::__ops::__iter_comp_iter(__pred, __scan)))
continue;
auto __matches = std::__count_if(__first2, __last2,
__gnu_cxx::__ops::__iter_comp_iter(__pred, __scan));
if (0 == __matches
|| std::__count_if(__scan, __last1,
__gnu_cxx::__ops::__iter_comp_iter(__pred, __scan))
!= __matches)
return false;
}
return true;
}
template<typename _ForwardIterator1, typename _ForwardIterator2>
_GLIBCXX20_CONSTEXPR
inline bool
is_permutation(_ForwardIterator1 __first1, _ForwardIterator1 __last1,
_ForwardIterator2 __first2, _ForwardIterator2 __last2)
{
__glibcxx_requires_valid_range(__first1, __last1);
__glibcxx_requires_valid_range(__first2, __last2);
return
std::__is_permutation(__first1, __last1, __first2, __last2,
__gnu_cxx::__ops::__iter_equal_to_iter());
}
template<typename _ForwardIterator1, typename _ForwardIterator2,
typename _BinaryPredicate>
_GLIBCXX20_CONSTEXPR
inline bool
is_permutation(_ForwardIterator1 __first1, _ForwardIterator1 __last1,
_ForwardIterator2 __first2, _ForwardIterator2 __last2,
_BinaryPredicate __pred)
{
__glibcxx_requires_valid_range(__first1, __last1);
__glibcxx_requires_valid_range(__first2, __last2);
return std::__is_permutation(__first1, __last1, __first2, __last2,
__gnu_cxx::__ops::__iter_comp_iter(__pred));
}
#if __cplusplus >= 201703L
#define __cpp_lib_clamp 201603L
template<typename _Tp>
constexpr const _Tp&
clamp(const _Tp& __val, const _Tp& __lo, const _Tp& __hi)
{
__glibcxx_assert(!(__hi < __lo));
return std::min(std::max(__val, __lo), __hi);
}
template<typename _Tp, typename _Compare>
constexpr const _Tp&
clamp(const _Tp& __val, const _Tp& __lo, const _Tp& __hi, _Compare __comp)
{
__glibcxx_assert(!__comp(__hi, __lo));
return std::min(std::max(__val, __lo, __comp), __hi, __comp);
}
#endif
#endif
#ifdef _GLIBCXX_USE_C99_STDINT_TR1
template<typename _IntType, typename _UniformRandomBitGenerator>
pair<_IntType, _IntType>
__gen_two_uniform_ints(_IntType __b0, _IntType __b1,
_UniformRandomBitGenerator&& __g)
{
_IntType __x
= uniform_int_distribution<_IntType>{0, (__b0 * __b1) - 1}(__g);
return std::make_pair(__x / __b1, __x % __b1);
}
template<typename _RandomAccessIterator,
typename _UniformRandomNumberGenerator>
void
shuffle(_RandomAccessIterator __first, _RandomAccessIterator __last,
_UniformRandomNumberGenerator&& __g)
{
__glibcxx_function_requires(_Mutable_RandomAccessIteratorConcept<
_RandomAccessIterator>)
__glibcxx_requires_valid_range(__first, __last);
if (__first == __last)
return;
typedef typename iterator_traits<_RandomAccessIterator>::difference_type
_DistanceType;
typedef typename std::make_unsigned<_DistanceType>::type __ud_type;
typedef typename std::uniform_int_distribution<__ud_type> __distr_type;
typedef typename __distr_type::param_type __p_type;
typedef typename remove_reference<_UniformRandomNumberGenerator>::type
_Gen;
typedef typename common_type<typename _Gen::result_type, __ud_type>::type
__uc_type;
const __uc_type __urngrange = __g.max() - __g.min();
const __uc_type __urange = __uc_type(__last - __first);
if (__urngrange / __urange >= __urange)
{
_RandomAccessIterator __i = __first + 1;
if ((__urange % 2) == 0)
{
__distr_type __d{0, 1};
std::iter_swap(__i++, __first + __d(__g));
}
while (__i != __last)
{
const __uc_type __swap_range = __uc_type(__i - __first) + 1;
const pair<__uc_type, __uc_type> __pospos =
__gen_two_uniform_ints(__swap_range, __swap_range + 1, __g);
std::iter_swap(__i++, __first + __pospos.first);
std::iter_swap(__i++, __first + __pospos.second);
}
return;
}
__distr_type __d;
for (_RandomAccessIterator __i = __first + 1; __i != __last; ++__i)
std::iter_swap(__i, __first + __d(__g, __p_type(0, __i - __first)));
}
#endif
#endif
_GLIBCXX_BEGIN_NAMESPACE_ALGO
template<typename _InputIterator, typename _Function>
_GLIBCXX20_CONSTEXPR
_Function
for_each(_InputIterator __first, _InputIterator __last, _Function __f)
{
__glibcxx_function_requires(_InputIteratorConcept<_InputIterator>)
__glibcxx_requires_valid_range(__first, __last);
for (; __first != __last; ++__first)
__f(*__first);
return __f;
}
#if __cplusplus >= 201703L
template<typename _InputIterator, typename _Size, typename _Function>
_GLIBCXX20_CONSTEXPR
_InputIterator
for_each_n(_InputIterator __first, _Size __n, _Function __f)
{
auto __n2 = std::__size_to_integer(__n);
using _Cat = typename iterator_traits<_InputIterator>::iterator_category;
if constexpr (is_base_of_v<random_access_iterator_tag, _Cat>)
{
if (__n2 <= 0)
return __first;
auto __last = __first + __n2;
std::for_each(__first, __last, std::move(__f));
return __last;
}
else
{
while (__n2-->0)
{
__f(*__first);
++__first;
}
return __first;
}
}
#endif
template<typename _InputIterator, typename _Tp>
_GLIBCXX20_CONSTEXPR
inline _InputIterator
find(_InputIterator __first, _InputIterator __last,
const _Tp& __val)
{
__glibcxx_function_requires(_InputIteratorConcept<_InputIterator>)
__glibcxx_function_requires(_EqualOpConcept<
typename iterator_traits<_InputIterator>::value_type, _Tp>)
__glibcxx_requires_valid_range(__first, __last);
return std::__find_if(__first, __last,
__gnu_cxx::__ops::__iter_equals_val(__val));
}
template<typename _InputIterator, typename _Predicate>
_GLIBCXX20_CONSTEXPR
inline _InputIterator
find_if(_InputIterator __first, _InputIterator __last,
_Predicate __pred)
{
__glibcxx_function_requires(_InputIteratorConcept<_InputIterator>)
__glibcxx_function_requires(_UnaryPredicateConcept<_Predicate,
typename iterator_traits<_InputIterator>::value_type>)
__glibcxx_requires_valid_range(__first, __last);
return std::__find_if(__first, __last,
__gnu_cxx::__ops::__pred_iter(__pred));
}
template<typename _InputIterator, typename _ForwardIterator>
_GLIBCXX20_CONSTEXPR
_InputIterator
find_first_of(_InputIterator __first1, _InputIterator __last1,
_ForwardIterator __first2, _ForwardIterator __last2)
{
__glibcxx_function_requires(_InputIteratorConcept<_InputIterator>)
__glibcxx_function_requires(_ForwardIteratorConcept<_ForwardIterator>)
__glibcxx_function_requires(_EqualOpConcept<
typename iterator_traits<_InputIterator>::value_type,
typename iterator_traits<_ForwardIterator>::value_type>)
__glibcxx_requires_valid_range(__first1, __last1);
__glibcxx_requires_valid_range(__first2, __last2);
for (; __first1 != __last1; ++__first1)
for (_ForwardIterator __iter = __first2; __iter != __last2; ++__iter)
if (*__first1 == *__iter)
return __first1;
return __last1;
}
template<typename _InputIterator, typename _ForwardIterator,
typename _BinaryPredicate>
_GLIBCXX20_CONSTEXPR
_InputIterator
find_first_of(_InputIterator __first1, _InputIterator __last1,
_ForwardIterator __first2, _ForwardIterator __last2,
_BinaryPredicate __comp)
{
__glibcxx_function_requires(_InputIteratorConcept<_InputIterator>)
__glibcxx_function_requires(_ForwardIteratorConcept<_ForwardIterator>)
__glibcxx_function_requires(_BinaryPredicateConcept<_BinaryPredicate,
typename iterator_traits<_InputIterator>::value_type,
typename iterator_traits<_ForwardIterator>::value_type>)
__glibcxx_requires_valid_range(__first1, __last1);
__glibcxx_requires_valid_range(__first2, __last2);
for (; __first1 != __last1; ++__first1)
for (_ForwardIterator __iter = __first2; __iter != __last2; ++__iter)
if (__comp(*__first1, *__iter))
return __first1;
return __last1;
}
template<typename _ForwardIterator>
_GLIBCXX20_CONSTEXPR
inline _ForwardIterator
adjacent_find(_ForwardIterator __first, _ForwardIterator __last)
{
__glibcxx_function_requires(_ForwardIteratorConcept<_ForwardIterator>)
__glibcxx_function_requires(_EqualityComparableConcept<
typename iterator_traits<_ForwardIterator>::value_type>)
__glibcxx_requires_valid_range(__first, __last);
return std::__adjacent_find(__first, __last,
__gnu_cxx::__ops::__iter_equal_to_iter());
}
template<typename _ForwardIterator, typename _BinaryPredicate>
_GLIBCXX20_CONSTEXPR
inline _ForwardIterator
adjacent_find(_ForwardIterator __first, _ForwardIterator __last,
_BinaryPredicate __binary_pred)
{
__glibcxx_function_requires(_ForwardIteratorConcept<_ForwardIterator>)
__glibcxx_function_requires(_BinaryPredicateConcept<_BinaryPredicate,
typename iterator_traits<_ForwardIterator>::value_type,
typename iterator_traits<_ForwardIterator>::value_type>)
__glibcxx_requires_valid_range(__first, __last);
return std::__adjacent_find(__first, __last,
__gnu_cxx::__ops::__iter_comp_iter(__binary_pred));
}
template<typename _InputIterator, typename _Tp>
_GLIBCXX20_CONSTEXPR
inline typename iterator_traits<_InputIterator>::difference_type
count(_InputIterator __first, _InputIterator __last, const _Tp& __value)
{
__glibcxx_function_requires(_InputIteratorConcept<_InputIterator>)
__glibcxx_function_requires(_EqualOpConcept<
typename iterator_traits<_InputIterator>::value_type, _Tp>)
__glibcxx_requires_valid_range(__first, __last);
return std::__count_if(__first, __last,
__gnu_cxx::__ops::__iter_equals_val(__value));
}
template<typename _InputIterator, typename _Predicate>
_GLIBCXX20_CONSTEXPR
inline typename iterator_traits<_InputIterator>::difference_type
count_if(_InputIterator __first, _InputIterator __last, _Predicate __pred)
{
__glibcxx_function_requires(_InputIteratorConcept<_InputIterator>)
__glibcxx_function_requires(_UnaryPredicateConcept<_Predicate,
typename iterator_traits<_InputIterator>::value_type>)
__glibcxx_requires_valid_range(__first, __last);
return std::__count_if(__first, __last,
__gnu_cxx::__ops::__pred_iter(__pred));
}
template<typename _ForwardIterator1, typename _ForwardIterator2>
_GLIBCXX20_CONSTEXPR
inline _ForwardIterator1
search(_ForwardIterator1 __first1, _ForwardIterator1 __last1,
_ForwardIterator2 __first2, _ForwardIterator2 __last2)
{
__glibcxx_function_requires(_ForwardIteratorConcept<_ForwardIterator1>)
__glibcxx_function_requires(_ForwardIteratorConcept<_ForwardIterator2>)
__glibcxx_function_requires(_EqualOpConcept<
typename iterator_traits<_ForwardIterator1>::value_type,
typename iterator_traits<_ForwardIterator2>::value_type>)
__glibcxx_requires_valid_range(__first1, __last1);
__glibcxx_requires_valid_range(__first2, __last2);
return std::__search(__first1, __last1, __first2, __last2,
__gnu_cxx::__ops::__iter_equal_to_iter());
}
template<typename _ForwardIterator1, typename _ForwardIterator2,
typename _BinaryPredicate>
_GLIBCXX20_CONSTEXPR
inline _ForwardIterator1
search(_ForwardIterator1 __first1, _ForwardIterator1 __last1,
_ForwardIterator2 __first2, _ForwardIterator2 __last2,
_BinaryPredicate  __predicate)
{
__glibcxx_function_requires(_ForwardIteratorConcept<_ForwardIterator1>)
__glibcxx_function_requires(_ForwardIteratorConcept<_ForwardIterator2>)
__glibcxx_function_requires(_BinaryPredicateConcept<_BinaryPredicate,
typename iterator_traits<_ForwardIterator1>::value_type,
typename iterator_traits<_ForwardIterator2>::value_type>)
__glibcxx_requires_valid_range(__first1, __last1);
__glibcxx_requires_valid_range(__first2, __last2);
return std::__search(__first1, __last1, __first2, __last2,
__gnu_cxx::__ops::__iter_comp_iter(__predicate));
}
template<typename _ForwardIterator, typename _Integer, typename _Tp>
_GLIBCXX20_CONSTEXPR
inline _ForwardIterator
search_n(_ForwardIterator __first, _ForwardIterator __last,
_Integer __count, const _Tp& __val)
{
__glibcxx_function_requires(_ForwardIteratorConcept<_ForwardIterator>)
__glibcxx_function_requires(_EqualOpConcept<
typename iterator_traits<_ForwardIterator>::value_type, _Tp>)
__glibcxx_requires_valid_range(__first, __last);
return std::__search_n(__first, __last, __count,
__gnu_cxx::__ops::__iter_equals_val(__val));
}
template<typename _ForwardIterator, typename _Integer, typename _Tp,
typename _BinaryPredicate>
_GLIBCXX20_CONSTEXPR
inline _ForwardIterator
search_n(_ForwardIterator __first, _ForwardIterator __last,
_Integer __count, const _Tp& __val,
_BinaryPredicate __binary_pred)
{
__glibcxx_function_requires(_ForwardIteratorConcept<_ForwardIterator>)
__glibcxx_function_requires(_BinaryPredicateConcept<_BinaryPredicate,
typename iterator_traits<_ForwardIterator>::value_type, _Tp>)
__glibcxx_requires_valid_range(__first, __last);
return std::__search_n(__first, __last, __count,
__gnu_cxx::__ops::__iter_comp_val(__binary_pred, __val));
}
#if __cplusplus >= 201703L
template<typename _ForwardIterator, typename _Searcher>
_GLIBCXX20_CONSTEXPR
inline _ForwardIterator
search(_ForwardIterator __first, _ForwardIterator __last,
const _Searcher& __searcher)
{ return __searcher(__first, __last).first; }
#endif
template<typename _InputIterator, typename _OutputIterator,
typename _UnaryOperation>
_GLIBCXX20_CONSTEXPR
_OutputIterator
transform(_InputIterator __first, _InputIterator __last,
_OutputIterator __result, _UnaryOperation __unary_op)
{
__glibcxx_function_requires(_InputIteratorConcept<_InputIterator>)
__glibcxx_function_requires(_OutputIteratorConcept<_OutputIterator,
__typeof__(__unary_op(*__first))>)
__glibcxx_requires_valid_range(__first, __last);
for (; __first != __last; ++__first, (void)++__result)
*__result = __unary_op(*__first);
return __result;
}
template<typename _InputIterator1, typename _InputIterator2,
typename _OutputIterator, typename _BinaryOperation>
_GLIBCXX20_CONSTEXPR
_OutputIterator
transform(_InputIterator1 __first1, _InputIterator1 __last1,
_InputIterator2 __first2, _OutputIterator __result,
_BinaryOperation __binary_op)
{
__glibcxx_function_requires(_InputIteratorConcept<_InputIterator1>)
__glibcxx_function_requires(_InputIteratorConcept<_InputIterator2>)
__glibcxx_function_requires(_OutputIteratorConcept<_OutputIterator,
__typeof__(__binary_op(*__first1,*__first2))>)
__glibcxx_requires_valid_range(__first1, __last1);
for (; __first1 != __last1; ++__first1, (void)++__first2, ++__result)
*__result = __binary_op(*__first1, *__first2);
return __result;
}
template<typename _ForwardIterator, typename _Tp>
_GLIBCXX20_CONSTEXPR
void
replace(_ForwardIterator __first, _ForwardIterator __last,
const _Tp& __old_value, const _Tp& __new_value)
{
__glibcxx_function_requires(_Mutable_ForwardIteratorConcept<
_ForwardIterator>)
__glibcxx_function_requires(_EqualOpConcept<
typename iterator_traits<_ForwardIterator>::value_type, _Tp>)
__glibcxx_function_requires(_ConvertibleConcept<_Tp,
typename iterator_traits<_ForwardIterator>::value_type>)
__glibcxx_requires_valid_range(__first, __last);
for (; __first != __last; ++__first)
if (*__first == __old_value)
*__first = __new_value;
}
template<typename _ForwardIterator, typename _Predicate, typename _Tp>
_GLIBCXX20_CONSTEXPR
void
replace_if(_ForwardIterator __first, _ForwardIterator __last,
_Predicate __pred, const _Tp& __new_value)
{
__glibcxx_function_requires(_Mutable_ForwardIteratorConcept<
_ForwardIterator>)
__glibcxx_function_requires(_ConvertibleConcept<_Tp,
typename iterator_traits<_ForwardIterator>::value_type>)
__glibcxx_function_requires(_UnaryPredicateConcept<_Predicate,
typename iterator_traits<_ForwardIterator>::value_type>)
__glibcxx_requires_valid_range(__first, __last);
for (; __first != __last; ++__first)
if (__pred(*__first))
*__first = __new_value;
}
template<typename _ForwardIterator, typename _Generator>
_GLIBCXX20_CONSTEXPR
void
generate(_ForwardIterator __first, _ForwardIterator __last,
_Generator __gen)
{
__glibcxx_function_requires(_ForwardIteratorConcept<_ForwardIterator>)
__glibcxx_function_requires(_GeneratorConcept<_Generator,
typename iterator_traits<_ForwardIterator>::value_type>)
__glibcxx_requires_valid_range(__first, __last);
for (; __first != __last; ++__first)
*__first = __gen();
}
template<typename _OutputIterator, typename _Size, typename _Generator>
_GLIBCXX20_CONSTEXPR
_OutputIterator
generate_n(_OutputIterator __first, _Size __n, _Generator __gen)
{
__glibcxx_function_requires(_OutputIteratorConcept<_OutputIterator,
__typeof__(__gen())>)
typedef __decltype(std::__size_to_integer(__n)) _IntSize;
for (_IntSize __niter = std::__size_to_integer(__n);
__niter > 0; --__niter, (void) ++__first)
*__first = __gen();
return __first;
}
template<typename _InputIterator, typename _OutputIterator>
_GLIBCXX20_CONSTEXPR
inline _OutputIterator
unique_copy(_InputIterator __first, _InputIterator __last,
_OutputIterator __result)
{
__glibcxx_function_requires(_InputIteratorConcept<_InputIterator>)
__glibcxx_function_requires(_OutputIteratorConcept<_OutputIterator,
typename iterator_traits<_InputIterator>::value_type>)
__glibcxx_function_requires(_EqualityComparableConcept<
typename iterator_traits<_InputIterator>::value_type>)
__glibcxx_requires_valid_range(__first, __last);
if (__first == __last)
return __result;
return std::__unique_copy(__first, __last, __result,
__gnu_cxx::__ops::__iter_equal_to_iter(),
std::__iterator_category(__first),
std::__iterator_category(__result));
}
template<typename _InputIterator, typename _OutputIterator,
typename _BinaryPredicate>
_GLIBCXX20_CONSTEXPR
inline _OutputIterator
unique_copy(_InputIterator __first, _InputIterator __last,
_OutputIterator __result,
_BinaryPredicate __binary_pred)
{
__glibcxx_function_requires(_InputIteratorConcept<_InputIterator>)
__glibcxx_function_requires(_OutputIteratorConcept<_OutputIterator,
typename iterator_traits<_InputIterator>::value_type>)
__glibcxx_requires_valid_range(__first, __last);
if (__first == __last)
return __result;
return std::__unique_copy(__first, __last, __result,
__gnu_cxx::__ops::__iter_comp_iter(__binary_pred),
std::__iterator_category(__first),
std::__iterator_category(__result));
}
#if __cplusplus <= 201103L || _GLIBCXX_USE_DEPRECATED
#if _GLIBCXX_HOSTED
template<typename _RandomAccessIterator>
_GLIBCXX14_DEPRECATED_SUGGEST("std::shuffle")
inline void
random_shuffle(_RandomAccessIterator __first, _RandomAccessIterator __last)
{
__glibcxx_function_requires(_Mutable_RandomAccessIteratorConcept<
_RandomAccessIterator>)
__glibcxx_requires_valid_range(__first, __last);
if (__first != __last)
for (_RandomAccessIterator __i = __first + 1; __i != __last; ++__i)
{
_RandomAccessIterator __j = __first
+ std::rand() % ((__i - __first) + 1);
if (__i != __j)
std::iter_swap(__i, __j);
}
}
template<typename _RandomAccessIterator, typename _RandomNumberGenerator>
_GLIBCXX14_DEPRECATED_SUGGEST("std::shuffle")
void
random_shuffle(_RandomAccessIterator __first, _RandomAccessIterator __last,
#if __cplusplus >= 201103L
_RandomNumberGenerator&& __rand)
#else
_RandomNumberGenerator& __rand)
#endif
{
__glibcxx_function_requires(_Mutable_RandomAccessIteratorConcept<
_RandomAccessIterator>)
__glibcxx_requires_valid_range(__first, __last);
if (__first == __last)
return;
for (_RandomAccessIterator __i = __first + 1; __i != __last; ++__i)
{
_RandomAccessIterator __j = __first + __rand((__i - __first) + 1);
if (__i != __j)
std::iter_swap(__i, __j);
}
}
#endif
#endif
template<typename _ForwardIterator, typename _Predicate>
_GLIBCXX20_CONSTEXPR
inline _ForwardIterator
partition(_ForwardIterator __first, _ForwardIterator __last,
_Predicate   __pred)
{
__glibcxx_function_requires(_Mutable_ForwardIteratorConcept<
_ForwardIterator>)
__glibcxx_function_requires(_UnaryPredicateConcept<_Predicate,
typename iterator_traits<_ForwardIterator>::value_type>)
__glibcxx_requires_valid_range(__first, __last);
return std::__partition(__first, __last, __pred,
std::__iterator_category(__first));
}
template<typename _RandomAccessIterator>
_GLIBCXX20_CONSTEXPR
inline void
partial_sort(_RandomAccessIterator __first,
_RandomAccessIterator __middle,
_RandomAccessIterator __last)
{
__glibcxx_function_requires(_Mutable_RandomAccessIteratorConcept<
_RandomAccessIterator>)
__glibcxx_function_requires(_LessThanComparableConcept<
typename iterator_traits<_RandomAccessIterator>::value_type>)
__glibcxx_requires_valid_range(__first, __middle);
__glibcxx_requires_valid_range(__middle, __last);
__glibcxx_requires_irreflexive(__first, __last);
std::__partial_sort(__first, __middle, __last,
__gnu_cxx::__ops::__iter_less_iter());
}
template<typename _RandomAccessIterator, typename _Compare>
_GLIBCXX20_CONSTEXPR
inline void
partial_sort(_RandomAccessIterator __first,
_RandomAccessIterator __middle,
_RandomAccessIterator __last,
_Compare __comp)
{
__glibcxx_function_requires(_Mutable_RandomAccessIteratorConcept<
_RandomAccessIterator>)
__glibcxx_function_requires(_BinaryPredicateConcept<_Compare,
typename iterator_traits<_RandomAccessIterator>::value_type,
typename iterator_traits<_RandomAccessIterator>::value_type>)
__glibcxx_requires_valid_range(__first, __middle);
__glibcxx_requires_valid_range(__middle, __last);
__glibcxx_requires_irreflexive_pred(__first, __last, __comp);
std::__partial_sort(__first, __middle, __last,
__gnu_cxx::__ops::__iter_comp_iter(__comp));
}
template<typename _RandomAccessIterator>
_GLIBCXX20_CONSTEXPR
inline void
nth_element(_RandomAccessIterator __first, _RandomAccessIterator __nth,
_RandomAccessIterator __last)
{
__glibcxx_function_requires(_Mutable_RandomAccessIteratorConcept<
_RandomAccessIterator>)
__glibcxx_function_requires(_LessThanComparableConcept<
typename iterator_traits<_RandomAccessIterator>::value_type>)
__glibcxx_requires_valid_range(__first, __nth);
__glibcxx_requires_valid_range(__nth, __last);
__glibcxx_requires_irreflexive(__first, __last);
if (__first == __last || __nth == __last)
return;
std::__introselect(__first, __nth, __last,
std::__lg(__last - __first) * 2,
__gnu_cxx::__ops::__iter_less_iter());
}
template<typename _RandomAccessIterator, typename _Compare>
_GLIBCXX20_CONSTEXPR
inline void
nth_element(_RandomAccessIterator __first, _RandomAccessIterator __nth,
_RandomAccessIterator __last, _Compare __comp)
{
__glibcxx_function_requires(_Mutable_RandomAccessIteratorConcept<
_RandomAccessIterator>)
__glibcxx_function_requires(_BinaryPredicateConcept<_Compare,
typename iterator_traits<_RandomAccessIterator>::value_type,
typename iterator_traits<_RandomAccessIterator>::value_type>)
__glibcxx_requires_valid_range(__first, __nth);
__glibcxx_requires_valid_range(__nth, __last);
__glibcxx_requires_irreflexive_pred(__first, __last, __comp);
if (__first == __last || __nth == __last)
return;
std::__introselect(__first, __nth, __last,
std::__lg(__last - __first) * 2,
__gnu_cxx::__ops::__iter_comp_iter(__comp));
}
template<typename _RandomAccessIterator>
_GLIBCXX20_CONSTEXPR
inline void
sort(_RandomAccessIterator __first, _RandomAccessIterator __last)
{
__glibcxx_function_requires(_Mutable_RandomAccessIteratorConcept<
_RandomAccessIterator>)
__glibcxx_function_requires(_LessThanComparableConcept<
typename iterator_traits<_RandomAccessIterator>::value_type>)
__glibcxx_requires_valid_range(__first, __last);
__glibcxx_requires_irreflexive(__first, __last);
std::__sort(__first, __last, __gnu_cxx::__ops::__iter_less_iter());
}
template<typename _RandomAccessIterator, typename _Compare>
_GLIBCXX20_CONSTEXPR
inline void
sort(_RandomAccessIterator __first, _RandomAccessIterator __last,
_Compare __comp)
{
__glibcxx_function_requires(_Mutable_RandomAccessIteratorConcept<
_RandomAccessIterator>)
__glibcxx_function_requires(_BinaryPredicateConcept<_Compare,
typename iterator_traits<_RandomAccessIterator>::value_type,
typename iterator_traits<_RandomAccessIterator>::value_type>)
__glibcxx_requires_valid_range(__first, __last);
__glibcxx_requires_irreflexive_pred(__first, __last, __comp);
std::__sort(__first, __last, __gnu_cxx::__ops::__iter_comp_iter(__comp));
}
template<typename _InputIterator1, typename _InputIterator2,
typename _OutputIterator, typename _Compare>
_GLIBCXX20_CONSTEXPR
_OutputIterator
__merge(_InputIterator1 __first1, _InputIterator1 __last1,
_InputIterator2 __first2, _InputIterator2 __last2,
_OutputIterator __result, _Compare __comp)
{
while (__first1 != __last1 && __first2 != __last2)
{
if (__comp(__first2, __first1))
{
*__result = *__first2;
++__first2;
}
else
{
*__result = *__first1;
++__first1;
}
++__result;
}
return std::copy(__first2, __last2,
std::copy(__first1, __last1, __result));
}
template<typename _InputIterator1, typename _InputIterator2,
typename _OutputIterator>
_GLIBCXX20_CONSTEXPR
inline _OutputIterator
merge(_InputIterator1 __first1, _InputIterator1 __last1,
_InputIterator2 __first2, _InputIterator2 __last2,
_OutputIterator __result)
{
__glibcxx_function_requires(_InputIteratorConcept<_InputIterator1>)
__glibcxx_function_requires(_InputIteratorConcept<_InputIterator2>)
__glibcxx_function_requires(_OutputIteratorConcept<_OutputIterator,
typename iterator_traits<_InputIterator1>::value_type>)
__glibcxx_function_requires(_OutputIteratorConcept<_OutputIterator,
typename iterator_traits<_InputIterator2>::value_type>)
__glibcxx_function_requires(_LessThanOpConcept<
typename iterator_traits<_InputIterator2>::value_type,
typename iterator_traits<_InputIterator1>::value_type>)
__glibcxx_requires_sorted_set(__first1, __last1, __first2);
__glibcxx_requires_sorted_set(__first2, __last2, __first1);
__glibcxx_requires_irreflexive2(__first1, __last1);
__glibcxx_requires_irreflexive2(__first2, __last2);
return _GLIBCXX_STD_A::__merge(__first1, __last1,
__first2, __last2, __result,
__gnu_cxx::__ops::__iter_less_iter());
}
template<typename _InputIterator1, typename _InputIterator2,
typename _OutputIterator, typename _Compare>
_GLIBCXX20_CONSTEXPR
inline _OutputIterator
merge(_InputIterator1 __first1, _InputIterator1 __last1,
_InputIterator2 __first2, _InputIterator2 __last2,
_OutputIterator __result, _Compare __comp)
{
__glibcxx_function_requires(_InputIteratorConcept<_InputIterator1>)
__glibcxx_function_requires(_InputIteratorConcept<_InputIterator2>)
__glibcxx_function_requires(_OutputIteratorConcept<_OutputIterator,
typename iterator_traits<_InputIterator1>::value_type>)
__glibcxx_function_requires(_OutputIteratorConcept<_OutputIterator,
typename iterator_traits<_InputIterator2>::value_type>)
__glibcxx_function_requires(_BinaryPredicateConcept<_Compare,
typename iterator_traits<_InputIterator2>::value_type,
typename iterator_traits<_InputIterator1>::value_type>)
__glibcxx_requires_sorted_set_pred(__first1, __last1, __first2, __comp);
__glibcxx_requires_sorted_set_pred(__first2, __last2, __first1, __comp);
__glibcxx_requires_irreflexive_pred2(__first1, __last1, __comp);
__glibcxx_requires_irreflexive_pred2(__first2, __last2, __comp);
return _GLIBCXX_STD_A::__merge(__first1, __last1,
__first2, __last2, __result,
__gnu_cxx::__ops::__iter_comp_iter(__comp));
}
template<typename _RandomAccessIterator, typename _Compare>
inline void
__stable_sort(_RandomAccessIterator __first, _RandomAccessIterator __last,
_Compare __comp)
{
typedef typename iterator_traits<_RandomAccessIterator>::value_type
_ValueType;
typedef typename iterator_traits<_RandomAccessIterator>::difference_type
_DistanceType;
if (__first == __last)
return;
#if _GLIBCXX_HOSTED
typedef _Temporary_buffer<_RandomAccessIterator, _ValueType> _TmpBuf;
_TmpBuf __buf(__first, (__last - __first + 1) / 2);
if (__builtin_expect(__buf.requested_size() == __buf.size(), true))
std::__stable_sort_adaptive(__first,
__first + _DistanceType(__buf.size()),
__last, __buf.begin(), __comp);
else if (__builtin_expect(__buf.begin() == 0, false))
std::__inplace_stable_sort(__first, __last, __comp);
else
std::__stable_sort_adaptive_resize(__first, __last, __buf.begin(),
_DistanceType(__buf.size()), __comp);
#else
std::__inplace_stable_sort(__first, __last, __comp);
#endif
}
template<typename _RandomAccessIterator>
inline void
stable_sort(_RandomAccessIterator __first, _RandomAccessIterator __last)
{
__glibcxx_function_requires(_Mutable_RandomAccessIteratorConcept<
_RandomAccessIterator>)
__glibcxx_function_requires(_LessThanComparableConcept<
typename iterator_traits<_RandomAccessIterator>::value_type>)
__glibcxx_requires_valid_range(__first, __last);
__glibcxx_requires_irreflexive(__first, __last);
_GLIBCXX_STD_A::__stable_sort(__first, __last,
__gnu_cxx::__ops::__iter_less_iter());
}
template<typename _RandomAccessIterator, typename _Compare>
inline void
stable_sort(_RandomAccessIterator __first, _RandomAccessIterator __last,
_Compare __comp)
{
__glibcxx_function_requires(_Mutable_RandomAccessIteratorConcept<
_RandomAccessIterator>)
__glibcxx_function_requires(_BinaryPredicateConcept<_Compare,
typename iterator_traits<_RandomAccessIterator>::value_type,
typename iterator_traits<_RandomAccessIterator>::value_type>)
__glibcxx_requires_valid_range(__first, __last);
__glibcxx_requires_irreflexive_pred(__first, __last, __comp);
_GLIBCXX_STD_A::__stable_sort(__first, __last,
__gnu_cxx::__ops::__iter_comp_iter(__comp));
}
template<typename _InputIterator1, typename _InputIterator2,
typename _OutputIterator,
typename _Compare>
_GLIBCXX20_CONSTEXPR
_OutputIterator
__set_union(_InputIterator1 __first1, _InputIterator1 __last1,
_InputIterator2 __first2, _InputIterator2 __last2,
_OutputIterator __result, _Compare __comp)
{
while (__first1 != __last1 && __first2 != __last2)
{
if (__comp(__first1, __first2))
{
*__result = *__first1;
++__first1;
}
else if (__comp(__first2, __first1))
{
*__result = *__first2;
++__first2;
}
else
{
*__result = *__first1;
++__first1;
++__first2;
}
++__result;
}
return std::copy(__first2, __last2,
std::copy(__first1, __last1, __result));
}
template<typename _InputIterator1, typename _InputIterator2,
typename _OutputIterator>
_GLIBCXX20_CONSTEXPR
inline _OutputIterator
set_union(_InputIterator1 __first1, _InputIterator1 __last1,
_InputIterator2 __first2, _InputIterator2 __last2,
_OutputIterator __result)
{
__glibcxx_function_requires(_InputIteratorConcept<_InputIterator1>)
__glibcxx_function_requires(_InputIteratorConcept<_InputIterator2>)
__glibcxx_function_requires(_OutputIteratorConcept<_OutputIterator,
typename iterator_traits<_InputIterator1>::value_type>)
__glibcxx_function_requires(_OutputIteratorConcept<_OutputIterator,
typename iterator_traits<_InputIterator2>::value_type>)
__glibcxx_function_requires(_LessThanOpConcept<
typename iterator_traits<_InputIterator1>::value_type,
typename iterator_traits<_InputIterator2>::value_type>)
__glibcxx_function_requires(_LessThanOpConcept<
typename iterator_traits<_InputIterator2>::value_type,
typename iterator_traits<_InputIterator1>::value_type>)
__glibcxx_requires_sorted_set(__first1, __last1, __first2);
__glibcxx_requires_sorted_set(__first2, __last2, __first1);
__glibcxx_requires_irreflexive2(__first1, __last1);
__glibcxx_requires_irreflexive2(__first2, __last2);
return _GLIBCXX_STD_A::__set_union(__first1, __last1,
__first2, __last2, __result,
__gnu_cxx::__ops::__iter_less_iter());
}
template<typename _InputIterator1, typename _InputIterator2,
typename _OutputIterator, typename _Compare>
_GLIBCXX20_CONSTEXPR
inline _OutputIterator
set_union(_InputIterator1 __first1, _InputIterator1 __last1,
_InputIterator2 __first2, _InputIterator2 __last2,
_OutputIterator __result, _Compare __comp)
{
__glibcxx_function_requires(_InputIteratorConcept<_InputIterator1>)
__glibcxx_function_requires(_InputIteratorConcept<_InputIterator2>)
__glibcxx_function_requires(_OutputIteratorConcept<_OutputIterator,
typename iterator_traits<_InputIterator1>::value_type>)
__glibcxx_function_requires(_OutputIteratorConcept<_OutputIterator,
typename iterator_traits<_InputIterator2>::value_type>)
__glibcxx_function_requires(_BinaryPredicateConcept<_Compare,
typename iterator_traits<_InputIterator1>::value_type,
typename iterator_traits<_InputIterator2>::value_type>)
__glibcxx_function_requires(_BinaryPredicateConcept<_Compare,
typename iterator_traits<_InputIterator2>::value_type,
typename iterator_traits<_InputIterator1>::value_type>)
__glibcxx_requires_sorted_set_pred(__first1, __last1, __first2, __comp);
__glibcxx_requires_sorted_set_pred(__first2, __last2, __first1, __comp);
__glibcxx_requires_irreflexive_pred2(__first1, __last1, __comp);
__glibcxx_requires_irreflexive_pred2(__first2, __last2, __comp);
return _GLIBCXX_STD_A::__set_union(__first1, __last1,
__first2, __last2, __result,
__gnu_cxx::__ops::__iter_comp_iter(__comp));
}
template<typename _InputIterator1, typename _InputIterator2,
typename _OutputIterator,
typename _Compare>
_GLIBCXX20_CONSTEXPR
_OutputIterator
__set_intersection(_InputIterator1 __first1, _InputIterator1 __last1,
_InputIterator2 __first2, _InputIterator2 __last2,
_OutputIterator __result, _Compare __comp)
{
while (__first1 != __last1 && __first2 != __last2)
if (__comp(__first1, __first2))
++__first1;
else if (__comp(__first2, __first1))
++__first2;
else
{
*__result = *__first1;
++__first1;
++__first2;
++__result;
}
return __result;
}
template<typename _InputIterator1, typename _InputIterator2,
typename _OutputIterator>
_GLIBCXX20_CONSTEXPR
inline _OutputIterator
set_intersection(_InputIterator1 __first1, _InputIterator1 __last1,
_InputIterator2 __first2, _InputIterator2 __last2,
_OutputIterator __result)
{
__glibcxx_function_requires(_InputIteratorConcept<_InputIterator1>)
__glibcxx_function_requires(_InputIteratorConcept<_InputIterator2>)
__glibcxx_function_requires(_OutputIteratorConcept<_OutputIterator,
typename iterator_traits<_InputIterator1>::value_type>)
__glibcxx_function_requires(_LessThanOpConcept<
typename iterator_traits<_InputIterator1>::value_type,
typename iterator_traits<_InputIterator2>::value_type>)
__glibcxx_function_requires(_LessThanOpConcept<
typename iterator_traits<_InputIterator2>::value_type,
typename iterator_traits<_InputIterator1>::value_type>)
__glibcxx_requires_sorted_set(__first1, __last1, __first2);
__glibcxx_requires_sorted_set(__first2, __last2, __first1);
__glibcxx_requires_irreflexive2(__first1, __last1);
__glibcxx_requires_irreflexive2(__first2, __last2);
return _GLIBCXX_STD_A::__set_intersection(__first1, __last1,
__first2, __last2, __result,
__gnu_cxx::__ops::__iter_less_iter());
}
template<typename _InputIterator1, typename _InputIterator2,
typename _OutputIterator, typename _Compare>
_GLIBCXX20_CONSTEXPR
inline _OutputIterator
set_intersection(_InputIterator1 __first1, _InputIterator1 __last1,
_InputIterator2 __first2, _InputIterator2 __last2,
_OutputIterator __result, _Compare __comp)
{
__glibcxx_function_requires(_InputIteratorConcept<_InputIterator1>)
__glibcxx_function_requires(_InputIteratorConcept<_InputIterator2>)
__glibcxx_function_requires(_OutputIteratorConcept<_OutputIterator,
typename iterator_traits<_InputIterator1>::value_type>)
__glibcxx_function_requires(_BinaryPredicateConcept<_Compare,
typename iterator_traits<_InputIterator1>::value_type,
typename iterator_traits<_InputIterator2>::value_type>)
__glibcxx_function_requires(_BinaryPredicateConcept<_Compare,
typename iterator_traits<_InputIterator2>::value_type,
typename iterator_traits<_InputIterator1>::value_type>)
__glibcxx_requires_sorted_set_pred(__first1, __last1, __first2, __comp);
__glibcxx_requires_sorted_set_pred(__first2, __last2, __first1, __comp);
__glibcxx_requires_irreflexive_pred2(__first1, __last1, __comp);
__glibcxx_requires_irreflexive_pred2(__first2, __last2, __comp);
return _GLIBCXX_STD_A::__set_intersection(__first1, __last1,
__first2, __last2, __result,
__gnu_cxx::__ops::__iter_comp_iter(__comp));
}
template<typename _InputIterator1, typename _InputIterator2,
typename _OutputIterator,
typename _Compare>
_GLIBCXX20_CONSTEXPR
_OutputIterator
__set_difference(_InputIterator1 __first1, _InputIterator1 __last1,
_InputIterator2 __first2, _InputIterator2 __last2,
_OutputIterator __result, _Compare __comp)
{
while (__first1 != __last1 && __first2 != __last2)
if (__comp(__first1, __first2))
{
*__result = *__first1;
++__first1;
++__result;
}
else if (__comp(__first2, __first1))
++__first2;
else
{
++__first1;
++__first2;
}
return std::copy(__first1, __last1, __result);
}
template<typename _InputIterator1, typename _InputIterator2,
typename _OutputIterator>
_GLIBCXX20_CONSTEXPR
inline _OutputIterator
set_difference(_InputIterator1 __first1, _InputIterator1 __last1,
_InputIterator2 __first2, _InputIterator2 __last2,
_OutputIterator __result)
{
__glibcxx_function_requires(_InputIteratorConcept<_InputIterator1>)
__glibcxx_function_requires(_InputIteratorConcept<_InputIterator2>)
__glibcxx_function_requires(_OutputIteratorConcept<_OutputIterator,
typename iterator_traits<_InputIterator1>::value_type>)
__glibcxx_function_requires(_LessThanOpConcept<
typename iterator_traits<_InputIterator1>::value_type,
typename iterator_traits<_InputIterator2>::value_type>)
__glibcxx_function_requires(_LessThanOpConcept<
typename iterator_traits<_InputIterator2>::value_type,
typename iterator_traits<_InputIterator1>::value_type>)
__glibcxx_requires_sorted_set(__first1, __last1, __first2);
__glibcxx_requires_sorted_set(__first2, __last2, __first1);
__glibcxx_requires_irreflexive2(__first1, __last1);
__glibcxx_requires_irreflexive2(__first2, __last2);
return _GLIBCXX_STD_A::__set_difference(__first1, __last1,
__first2, __last2, __result,
__gnu_cxx::__ops::__iter_less_iter());
}
template<typename _InputIterator1, typename _InputIterator2,
typename _OutputIterator, typename _Compare>
_GLIBCXX20_CONSTEXPR
inline _OutputIterator
set_difference(_InputIterator1 __first1, _InputIterator1 __last1,
_InputIterator2 __first2, _InputIterator2 __last2,
_OutputIterator __result, _Compare __comp)
{
__glibcxx_function_requires(_InputIteratorConcept<_InputIterator1>)
__glibcxx_function_requires(_InputIteratorConcept<_InputIterator2>)
__glibcxx_function_requires(_OutputIteratorConcept<_OutputIterator,
typename iterator_traits<_InputIterator1>::value_type>)
__glibcxx_function_requires(_BinaryPredicateConcept<_Compare,
typename iterator_traits<_InputIterator1>::value_type,
typename iterator_traits<_InputIterator2>::value_type>)
__glibcxx_function_requires(_BinaryPredicateConcept<_Compare,
typename iterator_traits<_InputIterator2>::value_type,
typename iterator_traits<_InputIterator1>::value_type>)
__glibcxx_requires_sorted_set_pred(__first1, __last1, __first2, __comp);
__glibcxx_requires_sorted_set_pred(__first2, __last2, __first1, __comp);
__glibcxx_requires_irreflexive_pred2(__first1, __last1, __comp);
__glibcxx_requires_irreflexive_pred2(__first2, __last2, __comp);
return _GLIBCXX_STD_A::__set_difference(__first1, __last1,
__first2, __last2, __result,
__gnu_cxx::__ops::__iter_comp_iter(__comp));
}
template<typename _InputIterator1, typename _InputIterator2,
typename _OutputIterator,
typename _Compare>
_GLIBCXX20_CONSTEXPR
_OutputIterator
__set_symmetric_difference(_InputIterator1 __first1,
_InputIterator1 __last1,
_InputIterator2 __first2,
_InputIterator2 __last2,
_OutputIterator __result,
_Compare __comp)
{
while (__first1 != __last1 && __first2 != __last2)
if (__comp(__first1, __first2))
{
*__result = *__first1;
++__first1;
++__result;
}
else if (__comp(__first2, __first1))
{
*__result = *__first2;
++__first2;
++__result;
}
else
{
++__first1;
++__first2;
}
return std::copy(__first2, __last2,
std::copy(__first1, __last1, __result));
}
template<typename _InputIterator1, typename _InputIterator2,
typename _OutputIterator>
_GLIBCXX20_CONSTEXPR
inline _OutputIterator
set_symmetric_difference(_InputIterator1 __first1, _InputIterator1 __last1,
_InputIterator2 __first2, _InputIterator2 __last2,
_OutputIterator __result)
{
__glibcxx_function_requires(_InputIteratorConcept<_InputIterator1>)
__glibcxx_function_requires(_InputIteratorConcept<_InputIterator2>)
__glibcxx_function_requires(_OutputIteratorConcept<_OutputIterator,
typename iterator_traits<_InputIterator1>::value_type>)
__glibcxx_function_requires(_OutputIteratorConcept<_OutputIterator,
typename iterator_traits<_InputIterator2>::value_type>)
__glibcxx_function_requires(_LessThanOpConcept<
typename iterator_traits<_InputIterator1>::value_type,
typename iterator_traits<_InputIterator2>::value_type>)
__glibcxx_function_requires(_LessThanOpConcept<
typename iterator_traits<_InputIterator2>::value_type,
typename iterator_traits<_InputIterator1>::value_type>)
__glibcxx_requires_sorted_set(__first1, __last1, __first2);
__glibcxx_requires_sorted_set(__first2, __last2, __first1);
__glibcxx_requires_irreflexive2(__first1, __last1);
__glibcxx_requires_irreflexive2(__first2, __last2);
return _GLIBCXX_STD_A::__set_symmetric_difference(__first1, __last1,
__first2, __last2, __result,
__gnu_cxx::__ops::__iter_less_iter());
}
template<typename _InputIterator1, typename _InputIterator2,
typename _OutputIterator, typename _Compare>
_GLIBCXX20_CONSTEXPR
inline _OutputIterator
set_symmetric_difference(_InputIterator1 __first1, _InputIterator1 __last1,
_InputIterator2 __first2, _InputIterator2 __last2,
_OutputIterator __result,
_Compare __comp)
{
__glibcxx_function_requires(_InputIteratorConcept<_InputIterator1>)
__glibcxx_function_requires(_InputIteratorConcept<_InputIterator2>)
__glibcxx_function_requires(_OutputIteratorConcept<_OutputIterator,
typename iterator_traits<_InputIterator1>::value_type>)
__glibcxx_function_requires(_OutputIteratorConcept<_OutputIterator,
typename iterator_traits<_InputIterator2>::value_type>)
__glibcxx_function_requires(_BinaryPredicateConcept<_Compare,
typename iterator_traits<_InputIterator1>::value_type,
typename iterator_traits<_InputIterator2>::value_type>)
__glibcxx_function_requires(_BinaryPredicateConcept<_Compare,
typename iterator_traits<_InputIterator2>::value_type,
typename iterator_traits<_InputIterator1>::value_type>)
__glibcxx_requires_sorted_set_pred(__first1, __last1, __first2, __comp);
__glibcxx_requires_sorted_set_pred(__first2, __last2, __first1, __comp);
__glibcxx_requires_irreflexive_pred2(__first1, __last1, __comp);
__glibcxx_requires_irreflexive_pred2(__first2, __last2, __comp);
return _GLIBCXX_STD_A::__set_symmetric_difference(__first1, __last1,
__first2, __last2, __result,
__gnu_cxx::__ops::__iter_comp_iter(__comp));
}
template<typename _ForwardIterator, typename _Compare>
_GLIBCXX14_CONSTEXPR
_ForwardIterator
__min_element(_ForwardIterator __first, _ForwardIterator __last,
_Compare __comp)
{
if (__first == __last)
return __first;
_ForwardIterator __result = __first;
while (++__first != __last)
if (__comp(__first, __result))
__result = __first;
return __result;
}
template<typename _ForwardIterator>
_GLIBCXX14_CONSTEXPR
_ForwardIterator
inline min_element(_ForwardIterator __first, _ForwardIterator __last)
{
__glibcxx_function_requires(_ForwardIteratorConcept<_ForwardIterator>)
__glibcxx_function_requires(_LessThanComparableConcept<
typename iterator_traits<_ForwardIterator>::value_type>)
__glibcxx_requires_valid_range(__first, __last);
__glibcxx_requires_irreflexive(__first, __last);
return _GLIBCXX_STD_A::__min_element(__first, __last,
__gnu_cxx::__ops::__iter_less_iter());
}
template<typename _ForwardIterator, typename _Compare>
_GLIBCXX14_CONSTEXPR
inline _ForwardIterator
min_element(_ForwardIterator __first, _ForwardIterator __last,
_Compare __comp)
{
__glibcxx_function_requires(_ForwardIteratorConcept<_ForwardIterator>)
__glibcxx_function_requires(_BinaryPredicateConcept<_Compare,
typename iterator_traits<_ForwardIterator>::value_type,
typename iterator_traits<_ForwardIterator>::value_type>)
__glibcxx_requires_valid_range(__first, __last);
__glibcxx_requires_irreflexive_pred(__first, __last, __comp);
return _GLIBCXX_STD_A::__min_element(__first, __last,
__gnu_cxx::__ops::__iter_comp_iter(__comp));
}
template<typename _ForwardIterator, typename _Compare>
_GLIBCXX14_CONSTEXPR
_ForwardIterator
__max_element(_ForwardIterator __first, _ForwardIterator __last,
_Compare __comp)
{
if (__first == __last) return __first;
_ForwardIterator __result = __first;
while (++__first != __last)
if (__comp(__result, __first))
__result = __first;
return __result;
}
template<typename _ForwardIterator>
_GLIBCXX14_CONSTEXPR
inline _ForwardIterator
max_element(_ForwardIterator __first, _ForwardIterator __last)
{
__glibcxx_function_requires(_ForwardIteratorConcept<_ForwardIterator>)
__glibcxx_function_requires(_LessThanComparableConcept<
typename iterator_traits<_ForwardIterator>::value_type>)
__glibcxx_requires_valid_range(__first, __last);
__glibcxx_requires_irreflexive(__first, __last);
return _GLIBCXX_STD_A::__max_element(__first, __last,
__gnu_cxx::__ops::__iter_less_iter());
}
template<typename _ForwardIterator, typename _Compare>
_GLIBCXX14_CONSTEXPR
inline _ForwardIterator
max_element(_ForwardIterator __first, _ForwardIterator __last,
_Compare __comp)
{
__glibcxx_function_requires(_ForwardIteratorConcept<_ForwardIterator>)
__glibcxx_function_requires(_BinaryPredicateConcept<_Compare,
typename iterator_traits<_ForwardIterator>::value_type,
typename iterator_traits<_ForwardIterator>::value_type>)
__glibcxx_requires_valid_range(__first, __last);
__glibcxx_requires_irreflexive_pred(__first, __last, __comp);
return _GLIBCXX_STD_A::__max_element(__first, __last,
__gnu_cxx::__ops::__iter_comp_iter(__comp));
}
#if __cplusplus >= 201103L
template<typename _Tp>
_GLIBCXX14_CONSTEXPR
inline _Tp
min(initializer_list<_Tp> __l)
{
__glibcxx_requires_irreflexive(__l.begin(), __l.end());
return *_GLIBCXX_STD_A::__min_element(__l.begin(), __l.end(),
__gnu_cxx::__ops::__iter_less_iter());
}
template<typename _Tp, typename _Compare>
_GLIBCXX14_CONSTEXPR
inline _Tp
min(initializer_list<_Tp> __l, _Compare __comp)
{
__glibcxx_requires_irreflexive_pred(__l.begin(), __l.end(), __comp);
return *_GLIBCXX_STD_A::__min_element(__l.begin(), __l.end(),
__gnu_cxx::__ops::__iter_comp_iter(__comp));
}
template<typename _Tp>
_GLIBCXX14_CONSTEXPR
inline _Tp
max(initializer_list<_Tp> __l)
{
__glibcxx_requires_irreflexive(__l.begin(), __l.end());
return *_GLIBCXX_STD_A::__max_element(__l.begin(), __l.end(),
__gnu_cxx::__ops::__iter_less_iter());
}
template<typename _Tp, typename _Compare>
_GLIBCXX14_CONSTEXPR
inline _Tp
max(initializer_list<_Tp> __l, _Compare __comp)
{
__glibcxx_requires_irreflexive_pred(__l.begin(), __l.end(), __comp);
return *_GLIBCXX_STD_A::__max_element(__l.begin(), __l.end(),
__gnu_cxx::__ops::__iter_comp_iter(__comp));
}
#endif
#if __cplusplus >= 201402L
template<typename _InputIterator, typename _RandomAccessIterator,
typename _Size, typename _UniformRandomBitGenerator>
_RandomAccessIterator
__sample(_InputIterator __first, _InputIterator __last, input_iterator_tag,
_RandomAccessIterator __out, random_access_iterator_tag,
_Size __n, _UniformRandomBitGenerator&& __g)
{
using __distrib_type = uniform_int_distribution<_Size>;
using __param_type = typename __distrib_type::param_type;
__distrib_type __d{};
_Size __sample_sz = 0;
while (__first != __last && __sample_sz != __n)
{
__out[__sample_sz++] = *__first;
++__first;
}
for (auto __pop_sz = __sample_sz; __first != __last;
++__first, (void) ++__pop_sz)
{
const auto __k = __d(__g, __param_type{0, __pop_sz});
if (__k < __n)
__out[__k] = *__first;
}
return __out + __sample_sz;
}
template<typename _ForwardIterator, typename _OutputIterator, typename _Cat,
typename _Size, typename _UniformRandomBitGenerator>
_OutputIterator
__sample(_ForwardIterator __first, _ForwardIterator __last,
forward_iterator_tag,
_OutputIterator __out, _Cat,
_Size __n, _UniformRandomBitGenerator&& __g)
{
using __distrib_type = uniform_int_distribution<_Size>;
using __param_type = typename __distrib_type::param_type;
using _USize = make_unsigned_t<_Size>;
using _Gen = remove_reference_t<_UniformRandomBitGenerator>;
using __uc_type = common_type_t<typename _Gen::result_type, _USize>;
if (__first == __last)
return __out;
__distrib_type __d{};
_Size __unsampled_sz = std::distance(__first, __last);
__n = std::min(__n, __unsampled_sz);
const __uc_type __urngrange = __g.max() - __g.min();
if (__urngrange / __uc_type(__unsampled_sz) >= __uc_type(__unsampled_sz))
{
while (__n != 0 && __unsampled_sz >= 2)
{
const pair<_Size, _Size> __p =
__gen_two_uniform_ints(__unsampled_sz, __unsampled_sz - 1, __g);
--__unsampled_sz;
if (__p.first < __n)
{
*__out++ = *__first;
--__n;
}
++__first;
if (__n == 0) break;
--__unsampled_sz;
if (__p.second < __n)
{
*__out++ = *__first;
--__n;
}
++__first;
}
}
for (; __n != 0; ++__first)
if (__d(__g, __param_type{0, --__unsampled_sz}) < __n)
{
*__out++ = *__first;
--__n;
}
return __out;
}
#if __cplusplus > 201402L
#define __cpp_lib_sample 201603L
template<typename _PopulationIterator, typename _SampleIterator,
typename _Distance, typename _UniformRandomBitGenerator>
_SampleIterator
sample(_PopulationIterator __first, _PopulationIterator __last,
_SampleIterator __out, _Distance __n,
_UniformRandomBitGenerator&& __g)
{
using __pop_cat = typename
std::iterator_traits<_PopulationIterator>::iterator_category;
using __samp_cat = typename
std::iterator_traits<_SampleIterator>::iterator_category;
static_assert(
__or_<is_convertible<__pop_cat, forward_iterator_tag>,
is_convertible<__samp_cat, random_access_iterator_tag>>::value,
"output range must use a RandomAccessIterator when input range"
" does not meet the ForwardIterator requirements");
static_assert(is_integral<_Distance>::value,
"sample size must be an integer type");
typename iterator_traits<_PopulationIterator>::difference_type __d = __n;
return _GLIBCXX_STD_A::
__sample(__first, __last, __pop_cat{}, __out, __samp_cat{}, __d,
std::forward<_UniformRandomBitGenerator>(__g));
}
#endif
#endif
_GLIBCXX_END_NAMESPACE_ALGO
_GLIBCXX_END_NAMESPACE_VERSION
}
#endif