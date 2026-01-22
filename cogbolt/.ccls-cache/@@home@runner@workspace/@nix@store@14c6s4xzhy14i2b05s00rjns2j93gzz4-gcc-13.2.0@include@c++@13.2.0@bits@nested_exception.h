#ifndef _GLIBCXX_NESTED_EXCEPTION_H
#define _GLIBCXX_NESTED_EXCEPTION_H 1
#if __cplusplus < 201103L
# include <bits/c++0x_warning.h>
#else
#include <bits/move.h>
#include <bits/exception_ptr.h>
extern "C++" {
namespace std _GLIBCXX_VISIBILITY(default)
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
#if __cplusplus < 201703L || ! defined __cpp_if_constexpr
template<typename _Tp>
[[noreturn]]
inline void
__throw_with_nested_impl(_Tp&& __t, true_type)
{
throw _Nested_exception<__remove_cvref_t<_Tp>>{std::forward<_Tp>(__t)};
}
template<typename _Tp>
[[noreturn]]
inline void
__throw_with_nested_impl(_Tp&& __t, false_type)
{ throw std::forward<_Tp>(__t); }
#endif
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
#if __cplusplus >= 201703L && __cpp_if_constexpr
if constexpr (is_class_v<_Up>)
if constexpr (!is_final_v<_Up>)
if constexpr (!is_base_of_v<nested_exception, _Up>)
throw _Nested_exception<_Up>{std::forward<_Tp>(__t)};
throw std::forward<_Tp>(__t);
#else
using __nest = __and_<is_class<_Up>, __bool_constant<!__is_final(_Up)>,
__not_<is_base_of<nested_exception, _Up>>>;
std::__throw_with_nested_impl(std::forward<_Tp>(__t), __nest{});
#endif
}
#if __cplusplus < 201703L || ! defined __cpp_if_constexpr
template<typename _Ex>
inline void
__rethrow_if_nested_impl(const _Ex* __ptr, true_type)
{
if (auto __ne_ptr = dynamic_cast<const nested_exception*>(__ptr))
__ne_ptr->rethrow_nested();
}
inline void
__rethrow_if_nested_impl(const void*, false_type)
{ }
#endif
template<typename _Ex>
# if ! __cpp_rtti
[[__gnu__::__always_inline__]]
#endif
inline void
rethrow_if_nested(const _Ex& __ex)
{
const _Ex* __ptr = __builtin_addressof(__ex);
#if __cplusplus < 201703L || ! defined __cpp_if_constexpr
# if __cpp_rtti
using __cast = __and_<is_polymorphic<_Ex>,
__or_<__not_<is_base_of<nested_exception, _Ex>>,
is_convertible<_Ex*, nested_exception*>>>;
# else
using __cast = __and_<is_polymorphic<_Ex>,
is_base_of<nested_exception, _Ex>,
is_convertible<_Ex*, nested_exception*>>;
# endif
std::__rethrow_if_nested_impl(__ptr, __cast{});
#else
if constexpr (!is_polymorphic_v<_Ex>)
return;
else if constexpr (is_base_of_v<nested_exception, _Ex>
&& !is_convertible_v<_Ex*, nested_exception*>)
return;
# if ! __cpp_rtti
else if constexpr (!is_base_of_v<nested_exception, _Ex>)
return;
# endif
else if (auto __ne_ptr = dynamic_cast<const nested_exception*>(__ptr))
__ne_ptr->rethrow_nested();
#endif
}
}
}
#endif
#endif