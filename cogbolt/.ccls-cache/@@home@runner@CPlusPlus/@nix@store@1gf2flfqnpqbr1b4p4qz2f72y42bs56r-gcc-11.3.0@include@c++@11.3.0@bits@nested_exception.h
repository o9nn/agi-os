#ifndef _GLIBCXX_NESTED_EXCEPTION_H
#define _GLIBCXX_NESTED_EXCEPTION_H 1
#pragma GCC visibility push(default)
#if __cplusplus < 201103L
# include <bits/c++0x_warning.h>
#else
#include <bits/c++config.h>
#include <bits/move.h>
extern "C++" {
namespace std
{
class nested_exception
{
exception_ptr _M_ptr;
public:
nested_exception() noexcept : _M_ptr(current_exception()) { }
nested_exception(const nested_exception&) noexcept = default;
nested_exception& operator=(const nested_exception&) noexcept = default;
virtual ~nested_exception() noexcept;
[[noreturn]]
void
rethrow_nested() const
{
if (_M_ptr)
rethrow_exception(_M_ptr);
std::terminate();
}
exception_ptr
nested_ptr() const noexcept
{ return _M_ptr; }
};
template<typename _Except>
struct _Nested_exception : public _Except, public nested_exception
{
explicit _Nested_exception(const _Except& __ex)
: _Except(__ex)
{ }
explicit _Nested_exception(_Except&& __ex)
: _Except(static_cast<_Except&&>(__ex))
{ }
};
template<typename _Tp>
[[noreturn]]
inline void
__throw_with_nested_impl(_Tp&& __t, true_type)
{
using _Up = typename remove_reference<_Tp>::type;
throw _Nested_exception<_Up>{std::forward<_Tp>(__t)};
}
template<typename _Tp>
[[noreturn]]
inline void
__throw_with_nested_impl(_Tp&& __t, false_type)
{ throw std::forward<_Tp>(__t); }
template<typename _Tp>
[[noreturn]]
inline void
throw_with_nested(_Tp&& __t)
{
using _Up = typename decay<_Tp>::type;
using _CopyConstructible
= __and_<is_copy_constructible<_Up>, is_move_constructible<_Up>>;
static_assert(_CopyConstructible::value,
"throw_with_nested argument must be CopyConstructible");
using __nest = __and_<is_class<_Up>, __bool_constant<!__is_final(_Up)>,
__not_<is_base_of<nested_exception, _Up>>>;
std::__throw_with_nested_impl(std::forward<_Tp>(__t), __nest{});
}
template<typename _Tp>
using __rethrow_if_nested_cond = typename enable_if<
__and_<is_polymorphic<_Tp>,
__or_<__not_<is_base_of<nested_exception, _Tp>>,
is_convertible<_Tp*, nested_exception*>>>::value
>::type;
template<typename _Ex>
inline __rethrow_if_nested_cond<_Ex>
__rethrow_if_nested_impl(const _Ex* __ptr)
{
if (auto __ne_ptr = dynamic_cast<const nested_exception*>(__ptr))
__ne_ptr->rethrow_nested();
}
inline void
__rethrow_if_nested_impl(const void*)
{ }
template<typename _Ex>
inline void
rethrow_if_nested(const _Ex& __ex)
{ std::__rethrow_if_nested_impl(std::__addressof(__ex)); }
}
}
#endif
#pragma GCC visibility pop
#endif