#ifndef _STL_HEAP_H
#define _STL_HEAP_H 1
#include <debug/debug.h>
#include <bits/move.h>
#include <bits/predefined_ops.h>
#include <bits/stl_iterator_base_funcs.h>
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
template<typename _RandomAccessIterator, typename _Distance,
typename _Compare>
_GLIBCXX20_CONSTEXPR
_Distance
__is_heap_until(_RandomAccessIterator __first, _Distance __n,
_Compare& __comp)
{
_Distance __parent = 0;
for (_Distance __child = 1; __child < __n; ++__child)
{
if (__comp(__first + __parent, __first + __child))
return __child;
if ((__child & 1) == 0)
++__parent;
}
return __n;
}
template<typename _RandomAccessIterator, typename _Distance>
_GLIBCXX20_CONSTEXPR
inline bool
__is_heap(_RandomAccessIterator __first, _Distance __n)
{
__gnu_cxx::__ops::_Iter_less_iter __comp;
return std::__is_heap_until(__first, __n, __comp) == __n;
}
template<typename _RandomAccessIterator, typename _Compare,
typename _Distance>
_GLIBCXX20_CONSTEXPR
inline bool
__is_heap(_RandomAccessIterator __first, _Compare __comp, _Distance __n)
{
typedef __decltype(__comp) _Cmp;
__gnu_cxx::__ops::_Iter_comp_iter<_Cmp> __cmp(_GLIBCXX_MOVE(__comp));
return std::__is_heap_until(__first, __n, __cmp) == __n;
}
template<typename _RandomAccessIterator>
_GLIBCXX20_CONSTEXPR
inline bool
__is_heap(_RandomAccessIterator __first, _RandomAccessIterator __last)
{ return std::__is_heap(__first, std::distance(__first, __last)); }
template<typename _RandomAccessIterator, typename _Compare>
_GLIBCXX20_CONSTEXPR
inline bool
__is_heap(_RandomAccessIterator __first, _RandomAccessIterator __last,
_Compare __comp)
{
return std::__is_heap(__first, _GLIBCXX_MOVE(__comp),
std::distance(__first, __last));
}
template<typename _RandomAccessIterator, typename _Distance, typename _Tp,
typename _Compare>
_GLIBCXX20_CONSTEXPR
void
__push_heap(_RandomAccessIterator __first,
_Distance __holeIndex, _Distance __topIndex, _Tp __value,
_Compare& __comp)
{
_Distance __parent = (__holeIndex - 1) / 2;
while (__holeIndex > __topIndex && __comp(__first + __parent, __value))
{
*(__first + __holeIndex) = _GLIBCXX_MOVE(*(__first + __parent));
__holeIndex = __parent;
__parent = (__holeIndex - 1) / 2;
}
*(__first + __holeIndex) = _GLIBCXX_MOVE(__value);
}
template<typename _RandomAccessIterator>
_GLIBCXX20_CONSTEXPR
inline void
push_heap(_RandomAccessIterator __first, _RandomAccessIterator __last)
{
typedef typename iterator_traits<_RandomAccessIterator>::value_type
_ValueType;
typedef typename iterator_traits<_RandomAccessIterator>::difference_type
_DistanceType;
__glibcxx_function_requires(_Mutable_RandomAccessIteratorConcept<
_RandomAccessIterator>)
__glibcxx_function_requires(_LessThanComparableConcept<_ValueType>)
__glibcxx_requires_valid_range(__first, __last);
__glibcxx_requires_irreflexive(__first, __last);
__glibcxx_requires_heap(__first, __last - 1);
__gnu_cxx::__ops::_Iter_less_val __comp;
_ValueType __value = _GLIBCXX_MOVE(*(__last - 1));
std::__push_heap(__first, _DistanceType((__last - __first) - 1),
_DistanceType(0), _GLIBCXX_MOVE(__value), __comp);
}
template<typename _RandomAccessIterator, typename _Compare>
_GLIBCXX20_CONSTEXPR
inline void
push_heap(_RandomAccessIterator __first, _RandomAccessIterator __last,
_Compare __comp)
{
typedef typename iterator_traits<_RandomAccessIterator>::value_type
_ValueType;
typedef typename iterator_traits<_RandomAccessIterator>::difference_type
_DistanceType;
__glibcxx_function_requires(_Mutable_RandomAccessIteratorConcept<
_RandomAccessIterator>)
__glibcxx_requires_valid_range(__first, __last);
__glibcxx_requires_irreflexive_pred(__first, __last, __comp);
__glibcxx_requires_heap_pred(__first, __last - 1, __comp);
__decltype(__gnu_cxx::__ops::__iter_comp_val(_GLIBCXX_MOVE(__comp)))
__cmp(_GLIBCXX_MOVE(__comp));
_ValueType __value = _GLIBCXX_MOVE(*(__last - 1));
std::__push_heap(__first, _DistanceType((__last - __first) - 1),
_DistanceType(0), _GLIBCXX_MOVE(__value), __cmp);
}
template<typename _RandomAccessIterator, typename _Distance,
typename _Tp, typename _Compare>
_GLIBCXX20_CONSTEXPR
void
__adjust_heap(_RandomAccessIterator __first, _Distance __holeIndex,
_Distance __len, _Tp __value, _Compare __comp)
{
const _Distance __topIndex = __holeIndex;
_Distance __secondChild = __holeIndex;
while (__secondChild < (__len - 1) / 2)
{
__secondChild = 2 * (__secondChild + 1);
if (__comp(__first + __secondChild,
__first + (__secondChild - 1)))
__secondChild--;
*(__first + __holeIndex) = _GLIBCXX_MOVE(*(__first + __secondChild));
__holeIndex = __secondChild;
}
if ((__len & 1) == 0 && __secondChild == (__len - 2) / 2)
{
__secondChild = 2 * (__secondChild + 1);
*(__first + __holeIndex) = _GLIBCXX_MOVE(*(__first
+ (__secondChild - 1)));
__holeIndex = __secondChild - 1;
}
__decltype(__gnu_cxx::__ops::__iter_comp_val(_GLIBCXX_MOVE(__comp)))
__cmp(_GLIBCXX_MOVE(__comp));
std::__push_heap(__first, __holeIndex, __topIndex,
_GLIBCXX_MOVE(__value), __cmp);
}
template<typename _RandomAccessIterator, typename _Compare>
_GLIBCXX20_CONSTEXPR
inline void
__pop_heap(_RandomAccessIterator __first, _RandomAccessIterator __last,
_RandomAccessIterator __result, _Compare& __comp)
{
typedef typename iterator_traits<_RandomAccessIterator>::value_type
_ValueType;
typedef typename iterator_traits<_RandomAccessIterator>::difference_type
_DistanceType;
_ValueType __value = _GLIBCXX_MOVE(*__result);
*__result = _GLIBCXX_MOVE(*__first);
std::__adjust_heap(__first, _DistanceType(0),
_DistanceType(__last - __first),
_GLIBCXX_MOVE(__value), __comp);
}
template<typename _RandomAccessIterator>
_GLIBCXX20_CONSTEXPR
inline void
pop_heap(_RandomAccessIterator __first, _RandomAccessIterator __last)
{
__glibcxx_function_requires(_Mutable_RandomAccessIteratorConcept<
_RandomAccessIterator>)
__glibcxx_function_requires(_LessThanComparableConcept<
typename iterator_traits<_RandomAccessIterator>::value_type>)
__glibcxx_requires_non_empty_range(__first, __last);
__glibcxx_requires_valid_range(__first, __last);
__glibcxx_requires_irreflexive(__first, __last);
__glibcxx_requires_heap(__first, __last);
if (__last - __first > 1)
{
--__last;
__gnu_cxx::__ops::_Iter_less_iter __comp;
std::__pop_heap(__first, __last, __last, __comp);
}
}
template<typename _RandomAccessIterator, typename _Compare>
_GLIBCXX20_CONSTEXPR
inline void
pop_heap(_RandomAccessIterator __first,
_RandomAccessIterator __last, _Compare __comp)
{
__glibcxx_function_requires(_Mutable_RandomAccessIteratorConcept<
_RandomAccessIterator>)
__glibcxx_requires_valid_range(__first, __last);
__glibcxx_requires_irreflexive_pred(__first, __last, __comp);
__glibcxx_requires_non_empty_range(__first, __last);
__glibcxx_requires_heap_pred(__first, __last, __comp);
if (__last - __first > 1)
{
typedef __decltype(__comp) _Cmp;
__gnu_cxx::__ops::_Iter_comp_iter<_Cmp> __cmp(_GLIBCXX_MOVE(__comp));
--__last;
std::__pop_heap(__first, __last, __last, __cmp);
}
}
template<typename _RandomAccessIterator, typename _Compare>
_GLIBCXX20_CONSTEXPR
void
__make_heap(_RandomAccessIterator __first, _RandomAccessIterator __last,
_Compare& __comp)
{
typedef typename iterator_traits<_RandomAccessIterator>::value_type
_ValueType;
typedef typename iterator_traits<_RandomAccessIterator>::difference_type
_DistanceType;
if (__last - __first < 2)
return;
const _DistanceType __len = __last - __first;
_DistanceType __parent = (__len - 2) / 2;
while (true)
{
_ValueType __value = _GLIBCXX_MOVE(*(__first + __parent));
std::__adjust_heap(__first, __parent, __len, _GLIBCXX_MOVE(__value),
__comp);
if (__parent == 0)
return;
__parent--;
}
}
template<typename _RandomAccessIterator>
_GLIBCXX20_CONSTEXPR
inline void
make_heap(_RandomAccessIterator __first, _RandomAccessIterator __last)
{
__glibcxx_function_requires(_Mutable_RandomAccessIteratorConcept<
_RandomAccessIterator>)
__glibcxx_function_requires(_LessThanComparableConcept<
typename iterator_traits<_RandomAccessIterator>::value_type>)
__glibcxx_requires_valid_range(__first, __last);
__glibcxx_requires_irreflexive(__first, __last);
__gnu_cxx::__ops::_Iter_less_iter __comp;
std::__make_heap(__first, __last, __comp);
}
template<typename _RandomAccessIterator, typename _Compare>
_GLIBCXX20_CONSTEXPR
inline void
make_heap(_RandomAccessIterator __first, _RandomAccessIterator __last,
_Compare __comp)
{
__glibcxx_function_requires(_Mutable_RandomAccessIteratorConcept<
_RandomAccessIterator>)
__glibcxx_requires_valid_range(__first, __last);
__glibcxx_requires_irreflexive_pred(__first, __last, __comp);
typedef __decltype(__comp) _Cmp;
__gnu_cxx::__ops::_Iter_comp_iter<_Cmp> __cmp(_GLIBCXX_MOVE(__comp));
std::__make_heap(__first, __last, __cmp);
}
template<typename _RandomAccessIterator, typename _Compare>
_GLIBCXX20_CONSTEXPR
void
__sort_heap(_RandomAccessIterator __first, _RandomAccessIterator __last,
_Compare& __comp)
{
while (__last - __first > 1)
{
--__last;
std::__pop_heap(__first, __last, __last, __comp);
}
}
template<typename _RandomAccessIterator>
_GLIBCXX20_CONSTEXPR
inline void
sort_heap(_RandomAccessIterator __first, _RandomAccessIterator __last)
{
__glibcxx_function_requires(_Mutable_RandomAccessIteratorConcept<
_RandomAccessIterator>)
__glibcxx_function_requires(_LessThanComparableConcept<
typename iterator_traits<_RandomAccessIterator>::value_type>)
__glibcxx_requires_valid_range(__first, __last);
__glibcxx_requires_irreflexive(__first, __last);
__glibcxx_requires_heap(__first, __last);
__gnu_cxx::__ops::_Iter_less_iter __comp;
std::__sort_heap(__first, __last, __comp);
}
template<typename _RandomAccessIterator, typename _Compare>
_GLIBCXX20_CONSTEXPR
inline void
sort_heap(_RandomAccessIterator __first, _RandomAccessIterator __last,
_Compare __comp)
{
__glibcxx_function_requires(_Mutable_RandomAccessIteratorConcept<
_RandomAccessIterator>)
__glibcxx_requires_valid_range(__first, __last);
__glibcxx_requires_irreflexive_pred(__first, __last, __comp);
__glibcxx_requires_heap_pred(__first, __last, __comp);
typedef __decltype(__comp) _Cmp;
__gnu_cxx::__ops::_Iter_comp_iter<_Cmp> __cmp(_GLIBCXX_MOVE(__comp));
std::__sort_heap(__first, __last, __cmp);
}
#if __cplusplus >= 201103L
template<typename _RandomAccessIterator>
_GLIBCXX20_CONSTEXPR
inline _RandomAccessIterator
is_heap_until(_RandomAccessIterator __first, _RandomAccessIterator __last)
{
__glibcxx_function_requires(_RandomAccessIteratorConcept<
_RandomAccessIterator>)
__glibcxx_function_requires(_LessThanComparableConcept<
typename iterator_traits<_RandomAccessIterator>::value_type>)
__glibcxx_requires_valid_range(__first, __last);
__glibcxx_requires_irreflexive(__first, __last);
__gnu_cxx::__ops::_Iter_less_iter __comp;
return __first +
std::__is_heap_until(__first, std::distance(__first, __last), __comp);
}
template<typename _RandomAccessIterator, typename _Compare>
_GLIBCXX20_CONSTEXPR
inline _RandomAccessIterator
is_heap_until(_RandomAccessIterator __first, _RandomAccessIterator __last,
_Compare __comp)
{
__glibcxx_function_requires(_RandomAccessIteratorConcept<
_RandomAccessIterator>)
__glibcxx_requires_valid_range(__first, __last);
__glibcxx_requires_irreflexive_pred(__first, __last, __comp);
typedef __decltype(__comp) _Cmp;
__gnu_cxx::__ops::_Iter_comp_iter<_Cmp> __cmp(_GLIBCXX_MOVE(__comp));
return __first
+ std::__is_heap_until(__first, std::distance(__first, __last), __cmp);
}
template<typename _RandomAccessIterator>
_GLIBCXX20_CONSTEXPR
inline bool
is_heap(_RandomAccessIterator __first, _RandomAccessIterator __last)
{ return std::is_heap_until(__first, __last) == __last; }
template<typename _RandomAccessIterator, typename _Compare>
_GLIBCXX20_CONSTEXPR
inline bool
is_heap(_RandomAccessIterator __first, _RandomAccessIterator __last,
_Compare __comp)
{
__glibcxx_function_requires(_RandomAccessIteratorConcept<
_RandomAccessIterator>)
__glibcxx_requires_valid_range(__first, __last);
__glibcxx_requires_irreflexive_pred(__first, __last, __comp);
const auto __dist = std::distance(__first, __last);
typedef __decltype(__comp) _Cmp;
__gnu_cxx::__ops::_Iter_comp_iter<_Cmp> __cmp(_GLIBCXX_MOVE(__comp));
return std::__is_heap_until(__first, __dist, __cmp) == __dist;
}
#endif
_GLIBCXX_END_NAMESPACE_VERSION
}
#endif