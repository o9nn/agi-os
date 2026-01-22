#ifndef _STL_RAW_STORAGE_ITERATOR_H
#define _STL_RAW_STORAGE_ITERATOR_H 1
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"
template <class _OutputIterator, class _Tp>
class _GLIBCXX17_DEPRECATED raw_storage_iterator
: public iterator<output_iterator_tag, void, void, void, void>
{
protected:
_OutputIterator _M_iter;
public:
explicit
raw_storage_iterator(_OutputIterator __x)
: _M_iter(__x) {}
raw_storage_iterator&
operator*() { return *this; }
raw_storage_iterator&
operator=(const _Tp& __element)
{
std::_Construct(std::__addressof(*_M_iter), __element);
return *this;
}
#if __cplusplus >= 201103L
raw_storage_iterator&
operator=(_Tp&& __element)
{
std::_Construct(std::__addressof(*_M_iter), std::move(__element));
return *this;
}
#endif
raw_storage_iterator&
operator++()
{
++_M_iter;
return *this;
}
raw_storage_iterator
operator++(int)
{
raw_storage_iterator __tmp = *this;
++_M_iter;
return __tmp;
}
_OutputIterator base() const { return _M_iter; }
};
#pragma GCC diagnostic pop
_GLIBCXX_END_NAMESPACE_VERSION
}
#endif