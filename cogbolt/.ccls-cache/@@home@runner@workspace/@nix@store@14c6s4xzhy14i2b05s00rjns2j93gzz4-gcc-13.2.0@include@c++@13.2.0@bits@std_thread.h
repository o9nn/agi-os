#ifndef _GLIBCXX_THREAD_H
#define _GLIBCXX_THREAD_H 1
#pragma GCC system_header
#if __cplusplus >= 201103L
#include <bits/c++config.h>
#include <iosfwd>
#include <tuple>
#include <bits/functional_hash.h>
#include <bits/invoke.h>
#include <bits/refwrap.h>
#include <bits/unique_ptr.h>
#ifdef _GLIBCXX_HAS_GTHREADS
# include <bits/gthr.h>
#else
# include <errno.h>
# include <bits/functexcept.h>
#endif
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
class thread
{
public:
#ifdef _GLIBCXX_HAS_GTHREADS
using native_handle_type = __gthread_t;
#else
using native_handle_type = int;
#endif
class id
{
native_handle_type	_M_thread;
public:
id() noexcept : _M_thread() { }
explicit
id(native_handle_type __id) : _M_thread(__id) { }
private:
friend class thread;
friend struct hash<id>;
friend bool
operator==(id __x, id __y) noexcept;
#if __cpp_lib_three_way_comparison
friend strong_ordering
operator<=>(id __x, id __y) noexcept;
#else
friend bool
operator<(id __x, id __y) noexcept;
#endif
template<class _CharT, class _Traits>
friend basic_ostream<_CharT, _Traits>&
operator<<(basic_ostream<_CharT, _Traits>& __out, id __id);
};
private:
id				_M_id;
template<typename _Tp>
using __not_same = __not_<is_same<__remove_cvref_t<_Tp>, thread>>;
public:
thread() noexcept = default;
#ifdef _GLIBCXX_HAS_GTHREADS
private:
static void
_M_thread_deps_never_run() {
#ifdef GTHR_ACTIVE_PROXY
reinterpret_cast<void (*)(void)>(&pthread_create)();
reinterpret_cast<void (*)(void)>(&pthread_join)();
#endif
}
public:
template<typename _Callable, typename... _Args,
typename = _Require<__not_same<_Callable>>>
explicit
thread(_Callable&& __f, _Args&&... __args)
{
static_assert( __is_invocable<typename decay<_Callable>::type,
typename decay<_Args>::type...>::value,
"std::thread arguments must be invocable after conversion to rvalues"
);
using _Wrapper = _Call_wrapper<_Callable, _Args...>;
_M_start_thread(_State_ptr(new _State_impl<_Wrapper>(
std::forward<_Callable>(__f), std::forward<_Args>(__args)...)),
_M_thread_deps_never_run);
}
#endif
~thread()
{
if (joinable())
std::__terminate();
}
thread(const thread&) = delete;
thread(thread&& __t) noexcept
{ swap(__t); }
thread& operator=(const thread&) = delete;
thread& operator=(thread&& __t) noexcept
{
if (joinable())
std::__terminate();
swap(__t);
return *this;
}
void
swap(thread& __t) noexcept
{ std::swap(_M_id, __t._M_id); }
bool
joinable() const noexcept
{ return !(_M_id == id()); }
void
join();
void
detach();
id
get_id() const noexcept
{ return _M_id; }
native_handle_type
native_handle()
{ return _M_id._M_thread; }
static unsigned int
hardware_concurrency() noexcept;
#ifdef _GLIBCXX_HAS_GTHREADS
#ifndef _GLIBCXX_THREAD_IMPL
private:
#endif
struct _State
{
virtual ~_State();
virtual void _M_run() = 0;
};
using _State_ptr = unique_ptr<_State>;
private:
template<typename _Callable>
struct _State_impl : public _State
{
_Callable		_M_func;
template<typename... _Args>
_State_impl(_Args&&... __args)
: _M_func(std::forward<_Args>(__args)...)
{ }
void
_M_run() { _M_func(); }
};
void
_M_start_thread(_State_ptr, void (*)());
#if _GLIBCXX_THREAD_ABI_COMPAT
public:
struct _Impl_base;
typedef shared_ptr<_Impl_base>	__shared_base_type;
struct _Impl_base
{
__shared_base_type	_M_this_ptr;
virtual ~_Impl_base() = default;
virtual void _M_run() = 0;
};
private:
void
_M_start_thread(__shared_base_type, void (*)());
void
_M_start_thread(__shared_base_type);
#endif
private:
template<typename _Tuple>
struct _Invoker
{
template<typename... _Args>
explicit
_Invoker(_Args&&... __args)
: _M_t(std::forward<_Args>(__args)...)
{ }
_Tuple _M_t;
template<typename>
struct __result;
template<typename _Fn, typename... _Args>
struct __result<tuple<_Fn, _Args...>>
: __invoke_result<_Fn, _Args...>
{ };
template<size_t... _Ind>
typename __result<_Tuple>::type
_M_invoke(_Index_tuple<_Ind...>)
{ return std::__invoke(std::get<_Ind>(std::move(_M_t))...); }
typename __result<_Tuple>::type
operator()()
{
using _Indices
= typename _Build_index_tuple<tuple_size<_Tuple>::value>::__type;
return _M_invoke(_Indices());
}
};
public:
template<typename... _Tp>
using _Call_wrapper = _Invoker<tuple<typename decay<_Tp>::type...>>;
#endif
};
#ifndef _GLIBCXX_HAS_GTHREADS
inline void thread::join() { std::__throw_system_error(EINVAL); }
inline void thread::detach() { std::__throw_system_error(EINVAL); }
inline unsigned int thread::hardware_concurrency() noexcept { return 0; }
#endif
inline void
swap(thread& __x, thread& __y) noexcept
{ __x.swap(__y); }
inline bool
operator==(thread::id __x, thread::id __y) noexcept
{
return __x._M_thread == __y._M_thread;
}
template<>
struct hash<thread::id>
: public __hash_base<size_t, thread::id>
{
size_t
operator()(const thread::id& __id) const noexcept
{ return std::_Hash_impl::hash(__id._M_thread); }
};
namespace this_thread
{
inline thread::id
get_id() noexcept
{
#ifndef _GLIBCXX_HAS_GTHREADS
return thread::id(1);
#elif defined _GLIBCXX_NATIVE_THREAD_ID
return thread::id(_GLIBCXX_NATIVE_THREAD_ID);
#else
return thread::id(__gthread_self());
#endif
}
inline void
yield() noexcept
{
#if defined _GLIBCXX_HAS_GTHREADS && defined _GLIBCXX_USE_SCHED_YIELD
__gthread_yield();
#endif
}
}
_GLIBCXX_END_NAMESPACE_VERSION
}
#endif
#endif