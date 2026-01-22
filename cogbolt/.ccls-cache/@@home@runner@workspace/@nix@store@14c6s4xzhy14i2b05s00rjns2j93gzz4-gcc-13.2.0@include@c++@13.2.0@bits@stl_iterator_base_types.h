#ifndef _STL_ITERATOR_BASE_TYPES_H
#define _STL_ITERATOR_BASE_TYPES_H 1
#pragma GCC system_header
#include <bits/c++config.h>
#if __cplusplus >= 201103L
# include <type_traits>
#endif
#if __cplusplus > 201703L && __cpp_concepts >= 201907L
# include <bits/iterator_concepts.h>
#endif
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
struct input_iterator_tag { };
struct output_iterator_tag { };
struct forward_iterator_tag : public input_iterator_tag { };
struct bidirectional_iterator_tag : public forward_iterator_tag { };
struct random_access_iterator_tag : public bidirectional_iterator_tag { };
#if __cplusplus > 201703L
struct contiguous_iterator_tag : public random_access_iterator_tag { };
#endif
template<typename _Category, typename _Tp, typename _Distance = ptrdiff_t,
typename _Pointer = _Tp*, typename _Reference = _Tp&>
struct _GLIBCXX17_DEPRECATED iterator
{
typedef _Category  iterator_category;
typedef _Tp        value_type;
typedef _Distance  difference_type;
typedef _Pointer   pointer;
typedef _Reference reference;
};
template<typename _Iterator>
struct iterator_traits;
#if __cplusplus >= 201103L
template<typename _Iterator, typename = __void_t<>>
struct __iterator_traits { };
#if ! __cpp_lib_concepts
template<typename _Iterator>
struct __iterator_traits<_Iterator,
__void_t<typename _Iterator::iterator_category,
typename _Iterator::value_type,
typename _Iterator::difference_type,
typename _Iterator::pointer,
typename _Iterator::reference>>
{
typedef typename _Iterator::iterator_category iterator_category;
typedef typename _Iterator::value_type        value_type;
typedef typename _Iterator::difference_type   difference_type;
typedef typename _Iterator::pointer           pointer;
typedef typename _Iterator::reference         reference;
};
#endif
template<typename _Iterator>
struct iterator_traits
: public __iterator_traits<_Iterator> { };
#else
template<typename _Iterator>
struct iterator_traits
{
typedef typename _Iterator::iterator_category iterator_category;
typedef typename _Iterator::value_type        value_type;
typedef typename _Iterator::difference_type   difference_type;
typedef typename _Iterator::pointer           pointer;
typedef typename _Iterator::reference         reference;
};
#endif
#if __cplusplus > 201703L
template<typename _Tp>
#if __cpp_concepts >= 201907L
requires is_object_v<_Tp>
#endif
struct iterator_traits<_Tp*>
{
using iterator_concept  = contiguous_iterator_tag;
using iterator_category = random_access_iterator_tag;
using value_type	      = remove_cv_t<_Tp>;
using difference_type   = ptrdiff_t;
using pointer	      = _Tp*;
using reference	      = _Tp&;
};
#else
template<typename _Tp>
struct iterator_traits<_Tp*>
{
typedef random_access_iterator_tag iterator_category;
typedef _Tp                         value_type;
typedef ptrdiff_t                   difference_type;
typedef _Tp*                        pointer;
typedef _Tp&                        reference;
};
template<typename _Tp>
struct iterator_traits<const _Tp*>
{
typedef random_access_iterator_tag iterator_category;
typedef _Tp                         value_type;
typedef ptrdiff_t                   difference_type;
typedef const _Tp*                  pointer;
typedef const _Tp&                  reference;
};
#endif
template<typename _Iter>
__attribute__((__always_inline__))
inline _GLIBCXX_CONSTEXPR
typename iterator_traits<_Iter>::iterator_category
__iterator_category(const _Iter&)
{ return typename iterator_traits<_Iter>::iterator_category(); }
#if __cplusplus >= 201103L
template<typename _Iter>
using __iterator_category_t
= typename iterator_traits<_Iter>::iterator_category;
template<typename _InIter>
using _RequireInputIter =
__enable_if_t<is_convertible<__iterator_category_t<_InIter>,
input_iterator_tag>::value>;
template<typename _It,
typename _Cat = __iterator_category_t<_It>>
struct __is_random_access_iter
: is_base_of<random_access_iterator_tag, _Cat>
{
typedef is_base_of<random_access_iterator_tag, _Cat> _Base;
enum { __value = _Base::value };
};
#else
template<typename _It, typename _Traits = iterator_traits<_It>,
typename _Cat = typename _Traits::iterator_category>
struct __is_random_access_iter
{ enum { __value = __is_base_of(random_access_iterator_tag, _Cat) }; };
#endif
_GLIBCXX_END_NAMESPACE_VERSION
}
#endif