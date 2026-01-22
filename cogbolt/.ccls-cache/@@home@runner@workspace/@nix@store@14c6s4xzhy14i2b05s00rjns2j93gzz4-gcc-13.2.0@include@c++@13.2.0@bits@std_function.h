#ifndef _GLIBCXX_STD_FUNCTION_H
#define _GLIBCXX_STD_FUNCTION_H 1
#pragma GCC system_header
#if __cplusplus < 201103L
# include <bits/c++0x_warning.h>
#else
#include <new>
#include <typeinfo>
#include <bits/invoke.h>
#include <bits/refwrap.h>
#include <bits/functexcept.h>
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
class bad_function_call : public std::exception
{
public:
virtual ~bad_function_call() noexcept;
const char* what() const noexcept;
};
template<typename _Tp>
struct __is_location_invariant
: is_trivially_copyable<_Tp>::type
{ };
class _Undefined_class;
union _Nocopy_types
{
void* _M_object;
const void* _M_const_object;
void (*_M_function_pointer)();
void (_Undefined_class::*_M_member_pointer)();
};
union [[gnu::may_alias]] _Any_data
{
void* _M_access() noexcept { return &_M_pod_data[0]; }
const void* _M_access() const noexcept { return &_M_pod_data[0]; }
template<typename _Tp>
_Tp&
_M_access() noexcept
{ return *static_cast<_Tp*>(_M_access()); }
template<typename _Tp>
const _Tp&
_M_access() const noexcept
{ return *static_cast<const _Tp*>(_M_access()); }
_Nocopy_types _M_unused;
char _M_pod_data[sizeof(_Nocopy_types)];
};
enum _Manager_operation
{
__get_type_info,
__get_functor_ptr,
__clone_functor,
__destroy_functor
};
template<typename _Signature>
class function;
class _Function_base
{
public:
static const size_t _M_max_size = sizeof(_Nocopy_types);
static const size_t _M_max_align = __alignof__(_Nocopy_types);
template<typename _Functor>
class _Base_manager
{
protected:
static const bool __stored_locally =
(__is_location_invariant<_Functor>::value
&& sizeof(_Functor) <= _M_max_size
&& __alignof__(_Functor) <= _M_max_align
&& (_M_max_align % __alignof__(_Functor) == 0));
using _Local_storage = integral_constant<bool, __stored_locally>;
static _Functor*
_M_get_pointer(const _Any_data& __source) noexcept
{
if _GLIBCXX17_CONSTEXPR (__stored_locally)
{
const _Functor& __f = __source._M_access<_Functor>();
return const_cast<_Functor*>(std::__addressof(__f));
}
else
return __source._M_access<_Functor*>();
}
private:
template<typename _Fn>
static void
_M_create(_Any_data& __dest, _Fn&& __f, true_type)
{
::new (__dest._M_access()) _Functor(std::forward<_Fn>(__f));
}
template<typename _Fn>
static void
_M_create(_Any_data& __dest, _Fn&& __f, false_type)
{
__dest._M_access<_Functor*>()
= new _Functor(std::forward<_Fn>(__f));
}
static void
_M_destroy(_Any_data& __victim, true_type)
{
__victim._M_access<_Functor>().~_Functor();
}
static void
_M_destroy(_Any_data& __victim, false_type)
{
delete __victim._M_access<_Functor*>();
}
public:
static bool
_M_manager(_Any_data& __dest, const _Any_data& __source,
_Manager_operation __op)
{
switch (__op)
{
case __get_type_info:
#if __cpp_rtti
__dest._M_access<const type_info*>() = &typeid(_Functor);
#else
__dest._M_access<const type_info*>() = nullptr;
#endif
break;
case __get_functor_ptr:
__dest._M_access<_Functor*>() = _M_get_pointer(__source);
break;
case __clone_functor:
_M_init_functor(__dest,
*const_cast<const _Functor*>(_M_get_pointer(__source)));
break;
case __destroy_functor:
_M_destroy(__dest, _Local_storage());
break;
}
return false;
}
template<typename _Fn>
static void
_M_init_functor(_Any_data& __functor, _Fn&& __f)
noexcept(__and_<_Local_storage,
is_nothrow_constructible<_Functor, _Fn>>::value)
{
_M_create(__functor, std::forward<_Fn>(__f), _Local_storage());
}
template<typename _Signature>
static bool
_M_not_empty_function(const function<_Signature>& __f) noexcept
{ return static_cast<bool>(__f); }
template<typename _Tp>
static bool
_M_not_empty_function(_Tp* __fp) noexcept
{ return __fp != nullptr; }
template<typename _Class, typename _Tp>
static bool
_M_not_empty_function(_Tp _Class::* __mp) noexcept
{ return __mp != nullptr; }
template<typename _Tp>
static bool
_M_not_empty_function(const _Tp&) noexcept
{ return true; }
};
_Function_base() = default;
~_Function_base()
{
if (_M_manager)
_M_manager(_M_functor, _M_functor, __destroy_functor);
}
bool _M_empty() const { return !_M_manager; }
using _Manager_type
= bool (*)(_Any_data&, const _Any_data&, _Manager_operation);
_Any_data _M_functor{};
_Manager_type _M_manager{};
};
template<typename _Signature, typename _Functor>
class _Function_handler;
template<typename _Res, typename _Functor, typename... _ArgTypes>
class _Function_handler<_Res(_ArgTypes...), _Functor>
: public _Function_base::_Base_manager<_Functor>
{
using _Base = _Function_base::_Base_manager<_Functor>;
public:
static bool
_M_manager(_Any_data& __dest, const _Any_data& __source,
_Manager_operation __op)
{
switch (__op)
{
#if __cpp_rtti
case __get_type_info:
__dest._M_access<const type_info*>() = &typeid(_Functor);
break;
#endif
case __get_functor_ptr:
__dest._M_access<_Functor*>() = _Base::_M_get_pointer(__source);
break;
default:
_Base::_M_manager(__dest, __source, __op);
}
return false;
}
static _Res
_M_invoke(const _Any_data& __functor, _ArgTypes&&... __args)
{
return std::__invoke_r<_Res>(*_Base::_M_get_pointer(__functor),
std::forward<_ArgTypes>(__args)...);
}
template<typename _Fn>
static constexpr bool
_S_nothrow_init() noexcept
{
return __and_<typename _Base::_Local_storage,
is_nothrow_constructible<_Functor, _Fn>>::value;
}
};
template<>
class _Function_handler<void, void>
{
public:
static bool
_M_manager(_Any_data&, const _Any_data&, _Manager_operation)
{ return false; }
};
template<typename _Signature, typename _Functor,
bool __valid = is_object<_Functor>::value>
struct _Target_handler
: _Function_handler<_Signature, typename remove_cv<_Functor>::type>
{ };
template<typename _Signature, typename _Functor>
struct _Target_handler<_Signature, _Functor, false>
: _Function_handler<void, void>
{ };
template<typename _Res, typename... _ArgTypes>
class function<_Res(_ArgTypes...)>
: public _Maybe_unary_or_binary_function<_Res, _ArgTypes...>,
private _Function_base
{
template<typename _Func,
bool _Self = is_same<__remove_cvref_t<_Func>, function>::value>
using _Decay_t
= typename __enable_if_t<!_Self, decay<_Func>>::type;
template<typename _Func,
typename _DFunc = _Decay_t<_Func>,
typename _Res2 = __invoke_result<_DFunc&, _ArgTypes...>>
struct _Callable
: __is_invocable_impl<_Res2, _Res>::type
{ };
template<typename _Cond, typename _Tp = void>
using _Requires = __enable_if_t<_Cond::value, _Tp>;
template<typename _Functor>
using _Handler
= _Function_handler<_Res(_ArgTypes...), __decay_t<_Functor>>;
public:
typedef _Res result_type;
function() noexcept
: _Function_base() { }
function(nullptr_t) noexcept
: _Function_base() { }
function(const function& __x)
: _Function_base()
{
if (static_cast<bool>(__x))
{
__x._M_manager(_M_functor, __x._M_functor, __clone_functor);
_M_invoker = __x._M_invoker;
_M_manager = __x._M_manager;
}
}
function(function&& __x) noexcept
: _Function_base(), _M_invoker(__x._M_invoker)
{
if (static_cast<bool>(__x))
{
_M_functor = __x._M_functor;
_M_manager = __x._M_manager;
__x._M_manager = nullptr;
__x._M_invoker = nullptr;
}
}
template<typename _Functor,
typename _Constraints = _Requires<_Callable<_Functor>>>
function(_Functor&& __f)
noexcept(_Handler<_Functor>::template _S_nothrow_init<_Functor>())
: _Function_base()
{
static_assert(is_copy_constructible<__decay_t<_Functor>>::value,
"std::function target must be copy-constructible");
static_assert(is_constructible<__decay_t<_Functor>, _Functor>::value,
"std::function target must be constructible from the "
"constructor argument");
using _My_handler = _Handler<_Functor>;
if (_My_handler::_M_not_empty_function(__f))
{
_My_handler::_M_init_functor(_M_functor,
std::forward<_Functor>(__f));
_M_invoker = &_My_handler::_M_invoke;
_M_manager = &_My_handler::_M_manager;
}
}
function&
operator=(const function& __x)
{
function(__x).swap(*this);
return *this;
}
function&
operator=(function&& __x) noexcept
{
function(std::move(__x)).swap(*this);
return *this;
}
function&
operator=(nullptr_t) noexcept
{
if (_M_manager)
{
_M_manager(_M_functor, _M_functor, __destroy_functor);
_M_manager = nullptr;
_M_invoker = nullptr;
}
return *this;
}
template<typename _Functor>
_Requires<_Callable<_Functor>, function&>
operator=(_Functor&& __f)
noexcept(_Handler<_Functor>::template _S_nothrow_init<_Functor>())
{
function(std::forward<_Functor>(__f)).swap(*this);
return *this;
}
template<typename _Functor>
function&
operator=(reference_wrapper<_Functor> __f) noexcept
{
function(__f).swap(*this);
return *this;
}
void swap(function& __x) noexcept
{
std::swap(_M_functor, __x._M_functor);
std::swap(_M_manager, __x._M_manager);
std::swap(_M_invoker, __x._M_invoker);
}
explicit operator bool() const noexcept
{ return !_M_empty(); }
_Res
operator()(_ArgTypes... __args) const
{
if (_M_empty())
__throw_bad_function_call();
return _M_invoker(_M_functor, std::forward<_ArgTypes>(__args)...);
}
#if __cpp_rtti
const type_info&
target_type() const noexcept
{
if (_M_manager)
{
_Any_data __typeinfo_result;
_M_manager(__typeinfo_result, _M_functor, __get_type_info);
if (auto __ti = __typeinfo_result._M_access<const type_info*>())
return *__ti;
}
return typeid(void);
}
#endif
template<typename _Functor>
_Functor*
target() noexcept
{
const function* __const_this = this;
const _Functor* __func = __const_this->template target<_Functor>();
return *const_cast<_Functor**>(&__func);
}
template<typename _Functor>
const _Functor*
target() const noexcept
{
if _GLIBCXX17_CONSTEXPR (is_object<_Functor>::value)
{
using _Handler = _Target_handler<_Res(_ArgTypes...), _Functor>;
if (_M_manager == &_Handler::_M_manager
#if __cpp_rtti
|| (_M_manager && typeid(_Functor) == target_type())
#endif
)
{
_Any_data __ptr;
_M_manager(__ptr, _M_functor, __get_functor_ptr);
return __ptr._M_access<const _Functor*>();
}
}
return nullptr;
}
private:
using _Invoker_type = _Res (*)(const _Any_data&, _ArgTypes&&...);
_Invoker_type _M_invoker = nullptr;
};
#if __cpp_deduction_guides >= 201606
template<typename>
struct __function_guide_helper
{ };
template<typename _Res, typename _Tp, bool _Nx, typename... _Args>
struct __function_guide_helper<
_Res (_Tp::*) (_Args...) noexcept(_Nx)
>
{ using type = _Res(_Args...); };
template<typename _Res, typename _Tp, bool _Nx, typename... _Args>
struct __function_guide_helper<
_Res (_Tp::*) (_Args...) & noexcept(_Nx)
>
{ using type = _Res(_Args...); };
template<typename _Res, typename _Tp, bool _Nx, typename... _Args>
struct __function_guide_helper<
_Res (_Tp::*) (_Args...) const noexcept(_Nx)
>
{ using type = _Res(_Args...); };
template<typename _Res, typename _Tp, bool _Nx, typename... _Args>
struct __function_guide_helper<
_Res (_Tp::*) (_Args...) const & noexcept(_Nx)
>
{ using type = _Res(_Args...); };
#if __cpp_static_call_operator >= 202207L && __cpp_concepts >= 202002L
template<typename _StaticCallOp>
struct __function_guide_static_helper
{ };
template<typename _Res, bool _Nx, typename... _Args>
struct __function_guide_static_helper<_Res (*) (_Args...) noexcept(_Nx)>
{ using type = _Res(_Args...); };
template<typename _Fn, typename _Op>
using __function_guide_t = typename __conditional_t<
requires (_Fn& __f) { (void) __f.operator(); },
__function_guide_static_helper<_Op>,
__function_guide_helper<_Op>>::type;
#else
template<typename _Fn, typename _Op>
using __function_guide_t = typename __function_guide_helper<_Op>::type;
#endif
template<typename _Res, typename... _ArgTypes>
function(_Res(*)(_ArgTypes...)) -> function<_Res(_ArgTypes...)>;
template<typename _Fn, typename _Signature
= __function_guide_t<_Fn, decltype(&_Fn::operator())>>
function(_Fn) -> function<_Signature>;
#endif
template<typename _Res, typename... _Args>
inline bool
operator==(const function<_Res(_Args...)>& __f, nullptr_t) noexcept
{ return !static_cast<bool>(__f); }
#if __cpp_impl_three_way_comparison < 201907L
template<typename _Res, typename... _Args>
inline bool
operator==(nullptr_t, const function<_Res(_Args...)>& __f) noexcept
{ return !static_cast<bool>(__f); }
template<typename _Res, typename... _Args>
inline bool
operator!=(const function<_Res(_Args...)>& __f, nullptr_t) noexcept
{ return static_cast<bool>(__f); }
template<typename _Res, typename... _Args>
inline bool
operator!=(nullptr_t, const function<_Res(_Args...)>& __f) noexcept
{ return static_cast<bool>(__f); }
#endif
template<typename _Res, typename... _Args>
inline void
swap(function<_Res(_Args...)>& __x, function<_Res(_Args...)>& __y) noexcept
{ __x.swap(__y); }
#if __cplusplus >= 201703L
namespace __detail::__variant
{
template<typename> struct _Never_valueless_alt;
template<typename _Signature>
struct _Never_valueless_alt<std::function<_Signature>>
: std::true_type
{ };
}
#endif
_GLIBCXX_END_NAMESPACE_VERSION
}
#endif
#endif