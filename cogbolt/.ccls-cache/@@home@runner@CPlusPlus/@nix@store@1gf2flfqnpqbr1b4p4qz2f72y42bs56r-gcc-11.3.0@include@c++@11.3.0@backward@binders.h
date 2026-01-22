#ifndef _BACKWARD_BINDERS_H
#define _BACKWARD_BINDERS_H 1
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
template<typename _Operation>
class binder1st
: public unary_function<typename _Operation::second_argument_type,
typename _Operation::result_type>
{
protected:
_Operation op;
typename _Operation::first_argument_type value;
public:
binder1st(const _Operation& __x,
const typename _Operation::first_argument_type& __y)
: op(__x), value(__y) { }
typename _Operation::result_type
operator()(const typename _Operation::second_argument_type& __x) const
{ return op(value, __x); }
typename _Operation::result_type
operator()(typename _Operation::second_argument_type& __x) const
{ return op(value, __x); }
} _GLIBCXX11_DEPRECATED_SUGGEST("std::bind");
template<typename _Operation, typename _Tp>
inline binder1st<_Operation>
bind1st(const _Operation& __fn, const _Tp& __x)
{
typedef typename _Operation::first_argument_type _Arg1_type;
return binder1st<_Operation>(__fn, _Arg1_type(__x));
}
template<typename _Operation>
class binder2nd
: public unary_function<typename _Operation::first_argument_type,
typename _Operation::result_type>
{
protected:
_Operation op;
typename _Operation::second_argument_type value;
public:
binder2nd(const _Operation& __x,
const typename _Operation::second_argument_type& __y)
: op(__x), value(__y) { }
typename _Operation::result_type
operator()(const typename _Operation::first_argument_type& __x) const
{ return op(__x, value); }
typename _Operation::result_type
operator()(typename _Operation::first_argument_type& __x) const
{ return op(__x, value); }
} _GLIBCXX11_DEPRECATED_SUGGEST("std::bind");
template<typename _Operation, typename _Tp>
inline binder2nd<_Operation>
bind2nd(const _Operation& __fn, const _Tp& __x)
{
typedef typename _Operation::second_argument_type _Arg2_type;
return binder2nd<_Operation>(__fn, _Arg2_type(__x));
}
_GLIBCXX_END_NAMESPACE_VERSION
}
#pragma GCC diagnostic pop
#endif