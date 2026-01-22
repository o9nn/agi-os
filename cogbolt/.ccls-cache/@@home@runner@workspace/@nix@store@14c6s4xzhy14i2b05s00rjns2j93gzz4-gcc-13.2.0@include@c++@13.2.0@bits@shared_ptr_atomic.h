#ifndef _SHARED_PTR_ATOMIC_H
#define _SHARED_PTR_ATOMIC_H 1
#include <bits/atomic_base.h>
#if defined _GLIBCXX_TSAN && __has_include(<sanitizer/tsan_interface.h>)
#include <sanitizer/tsan_interface.h>
#define _GLIBCXX_TSAN_MUTEX_DESTROY(X) \
__tsan_mutex_destroy(X, __tsan_mutex_not_static)
#define _GLIBCXX_TSAN_MUTEX_TRY_LOCK(X) \
__tsan_mutex_pre_lock(X, __tsan_mutex_not_static|__tsan_mutex_try_lock)
#define _GLIBCXX_TSAN_MUTEX_TRY_LOCK_FAILED(X) __tsan_mutex_post_lock(X, \
__tsan_mutex_not_static|__tsan_mutex_try_lock_failed, 0)
#define _GLIBCXX_TSAN_MUTEX_LOCKED(X) \
__tsan_mutex_post_lock(X, __tsan_mutex_not_static, 0)
#define _GLIBCXX_TSAN_MUTEX_PRE_UNLOCK(X) __tsan_mutex_pre_unlock(X, 0)
#define _GLIBCXX_TSAN_MUTEX_POST_UNLOCK(X) __tsan_mutex_post_unlock(X, 0)
#define _GLIBCXX_TSAN_MUTEX_PRE_SIGNAL(X) __tsan_mutex_pre_signal(X, 0)
#define _GLIBCXX_TSAN_MUTEX_POST_SIGNAL(X) __tsan_mutex_post_signal(X, 0)
#else
#define _GLIBCXX_TSAN_MUTEX_DESTROY(X)
#define _GLIBCXX_TSAN_MUTEX_TRY_LOCK(X)
#define _GLIBCXX_TSAN_MUTEX_TRY_LOCK_FAILED(X)
#define _GLIBCXX_TSAN_MUTEX_LOCKED(X)
#define _GLIBCXX_TSAN_MUTEX_PRE_UNLOCK(X)
#define _GLIBCXX_TSAN_MUTEX_POST_UNLOCK(X)
#define _GLIBCXX_TSAN_MUTEX_PRE_SIGNAL(X)
#define _GLIBCXX_TSAN_MUTEX_POST_SIGNAL(X)
#endif
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
struct _Sp_locker
{
_Sp_locker(const _Sp_locker&) = delete;
_Sp_locker& operator=(const _Sp_locker&) = delete;
#ifdef __GTHREADS
explicit
_Sp_locker(const void*) noexcept;
_Sp_locker(const void*, const void*) noexcept;
~_Sp_locker();
private:
unsigned char _M_key1;
unsigned char _M_key2;
#else
explicit _Sp_locker(const void*, const void* = nullptr) { }
#endif
};
template<typename _Tp, _Lock_policy _Lp>
inline bool
atomic_is_lock_free(const __shared_ptr<_Tp, _Lp>* __p)
{
#ifdef __GTHREADS
return __gthread_active_p() == 0;
#else
return true;
#endif
}
template<typename _Tp>
inline bool
atomic_is_lock_free(const shared_ptr<_Tp>* __p)
{ return std::atomic_is_lock_free<_Tp, __default_lock_policy>(__p); }
template<typename _Tp>
inline shared_ptr<_Tp>
atomic_load_explicit(const shared_ptr<_Tp>* __p, memory_order)
{
_Sp_locker __lock{__p};
return *__p;
}
template<typename _Tp>
inline shared_ptr<_Tp>
atomic_load(const shared_ptr<_Tp>* __p)
{ return std::atomic_load_explicit(__p, memory_order_seq_cst); }
template<typename _Tp, _Lock_policy _Lp>
inline __shared_ptr<_Tp, _Lp>
atomic_load_explicit(const __shared_ptr<_Tp, _Lp>* __p, memory_order)
{
_Sp_locker __lock{__p};
return *__p;
}
template<typename _Tp, _Lock_policy _Lp>
inline __shared_ptr<_Tp, _Lp>
atomic_load(const __shared_ptr<_Tp, _Lp>* __p)
{ return std::atomic_load_explicit(__p, memory_order_seq_cst); }
template<typename _Tp>
inline void
atomic_store_explicit(shared_ptr<_Tp>* __p, shared_ptr<_Tp> __r,
memory_order)
{
_Sp_locker __lock{__p};
__p->swap(__r);
}
template<typename _Tp>
inline void
atomic_store(shared_ptr<_Tp>* __p, shared_ptr<_Tp> __r)
{ std::atomic_store_explicit(__p, std::move(__r), memory_order_seq_cst); }
template<typename _Tp, _Lock_policy _Lp>
inline void
atomic_store_explicit(__shared_ptr<_Tp, _Lp>* __p,
__shared_ptr<_Tp, _Lp> __r,
memory_order)
{
_Sp_locker __lock{__p};
__p->swap(__r);
}
template<typename _Tp, _Lock_policy _Lp>
inline void
atomic_store(__shared_ptr<_Tp, _Lp>* __p, __shared_ptr<_Tp, _Lp> __r)
{ std::atomic_store_explicit(__p, std::move(__r), memory_order_seq_cst); }
template<typename _Tp>
inline shared_ptr<_Tp>
atomic_exchange_explicit(shared_ptr<_Tp>* __p, shared_ptr<_Tp> __r,
memory_order)
{
_Sp_locker __lock{__p};
__p->swap(__r);
return __r;
}
template<typename _Tp>
inline shared_ptr<_Tp>
atomic_exchange(shared_ptr<_Tp>* __p, shared_ptr<_Tp> __r)
{
return std::atomic_exchange_explicit(__p, std::move(__r),
memory_order_seq_cst);
}
template<typename _Tp, _Lock_policy _Lp>
inline __shared_ptr<_Tp, _Lp>
atomic_exchange_explicit(__shared_ptr<_Tp, _Lp>* __p,
__shared_ptr<_Tp, _Lp> __r,
memory_order)
{
_Sp_locker __lock{__p};
__p->swap(__r);
return __r;
}
template<typename _Tp, _Lock_policy _Lp>
inline __shared_ptr<_Tp, _Lp>
atomic_exchange(__shared_ptr<_Tp, _Lp>* __p, __shared_ptr<_Tp, _Lp> __r)
{
return std::atomic_exchange_explicit(__p, std::move(__r),
memory_order_seq_cst);
}
template<typename _Tp>
bool
atomic_compare_exchange_strong_explicit(shared_ptr<_Tp>* __p,
shared_ptr<_Tp>* __v,
shared_ptr<_Tp> __w,
memory_order,
memory_order)
{
shared_ptr<_Tp> __x;
_Sp_locker __lock{__p, __v};
owner_less<shared_ptr<_Tp>> __less;
if (*__p == *__v && !__less(*__p, *__v) && !__less(*__v, *__p))
{
__x = std::move(*__p);
*__p = std::move(__w);
return true;
}
__x = std::move(*__v);
*__v = *__p;
return false;
}
template<typename _Tp>
inline bool
atomic_compare_exchange_strong(shared_ptr<_Tp>* __p, shared_ptr<_Tp>* __v,
shared_ptr<_Tp> __w)
{
return std::atomic_compare_exchange_strong_explicit(__p, __v,
std::move(__w), memory_order_seq_cst, memory_order_seq_cst);
}
template<typename _Tp>
inline bool
atomic_compare_exchange_weak_explicit(shared_ptr<_Tp>* __p,
shared_ptr<_Tp>* __v,
shared_ptr<_Tp> __w,
memory_order __success,
memory_order __failure)
{
return std::atomic_compare_exchange_strong_explicit(__p, __v,
std::move(__w), __success, __failure);
}
template<typename _Tp>
inline bool
atomic_compare_exchange_weak(shared_ptr<_Tp>* __p, shared_ptr<_Tp>* __v,
shared_ptr<_Tp> __w)
{
return std::atomic_compare_exchange_weak_explicit(__p, __v,
std::move(__w), memory_order_seq_cst, memory_order_seq_cst);
}
template<typename _Tp, _Lock_policy _Lp>
bool
atomic_compare_exchange_strong_explicit(__shared_ptr<_Tp, _Lp>* __p,
__shared_ptr<_Tp, _Lp>* __v,
__shared_ptr<_Tp, _Lp> __w,
memory_order,
memory_order)
{
__shared_ptr<_Tp, _Lp> __x;
_Sp_locker __lock{__p, __v};
owner_less<__shared_ptr<_Tp, _Lp>> __less;
if (*__p == *__v && !__less(*__p, *__v) && !__less(*__v, *__p))
{
__x = std::move(*__p);
*__p = std::move(__w);
return true;
}
__x = std::move(*__v);
*__v = *__p;
return false;
}
template<typename _Tp, _Lock_policy _Lp>
inline bool
atomic_compare_exchange_strong(__shared_ptr<_Tp, _Lp>* __p,
__shared_ptr<_Tp, _Lp>* __v,
__shared_ptr<_Tp, _Lp> __w)
{
return std::atomic_compare_exchange_strong_explicit(__p, __v,
std::move(__w), memory_order_seq_cst, memory_order_seq_cst);
}
template<typename _Tp, _Lock_policy _Lp>
inline bool
atomic_compare_exchange_weak_explicit(__shared_ptr<_Tp, _Lp>* __p,
__shared_ptr<_Tp, _Lp>* __v,
__shared_ptr<_Tp, _Lp> __w,
memory_order __success,
memory_order __failure)
{
return std::atomic_compare_exchange_strong_explicit(__p, __v,
std::move(__w), __success, __failure);
}
template<typename _Tp, _Lock_policy _Lp>
inline bool
atomic_compare_exchange_weak(__shared_ptr<_Tp, _Lp>* __p,
__shared_ptr<_Tp, _Lp>* __v,
__shared_ptr<_Tp, _Lp> __w)
{
return std::atomic_compare_exchange_weak_explicit(__p, __v,
std::move(__w), memory_order_seq_cst, memory_order_seq_cst);
}
#if __cplusplus >= 202002L
# define __cpp_lib_atomic_shared_ptr 201711L
template<typename _Tp>
class atomic;
template<typename _Up>
static constexpr bool __is_shared_ptr = false;
template<typename _Up>
static constexpr bool __is_shared_ptr<shared_ptr<_Up>> = true;
template<typename _Tp>
class _Sp_atomic
{
using value_type = _Tp;
friend class atomic<_Tp>;
struct _Atomic_count
{
using __count_type = decltype(_Tp::_M_refcount);
using pointer = decltype(__count_type::_M_pi);
static_assert(alignof(remove_pointer_t<pointer>) > 1);
constexpr _Atomic_count() noexcept = default;
explicit
_Atomic_count(__count_type&& __c) noexcept
: _M_val(reinterpret_cast<uintptr_t>(__c._M_pi))
{
__c._M_pi = nullptr;
}
~_Atomic_count()
{
auto __val = _M_val.load(memory_order_relaxed);
_GLIBCXX_TSAN_MUTEX_DESTROY(&_M_val);
__glibcxx_assert(!(__val & _S_lock_bit));
if (auto __pi = reinterpret_cast<pointer>(__val))
{
if constexpr (__is_shared_ptr<_Tp>)
__pi->_M_release();
else
__pi->_M_weak_release();
}
}
_Atomic_count(const _Atomic_count&) = delete;
_Atomic_count& operator=(const _Atomic_count&) = delete;
pointer
lock(memory_order __o) const noexcept
{
auto __current = _M_val.load(memory_order_relaxed);
while (__current & _S_lock_bit)
{
#if __cpp_lib_atomic_wait
__detail::__thread_relax();
#endif
__current = _M_val.load(memory_order_relaxed);
}
_GLIBCXX_TSAN_MUTEX_TRY_LOCK(&_M_val);
while (!_M_val.compare_exchange_strong(__current,
__current | _S_lock_bit,
__o,
memory_order_relaxed))
{
_GLIBCXX_TSAN_MUTEX_TRY_LOCK_FAILED(&_M_val);
#if __cpp_lib_atomic_wait
__detail::__thread_relax();
#endif
__current = __current & ~_S_lock_bit;
_GLIBCXX_TSAN_MUTEX_TRY_LOCK(&_M_val);
}
_GLIBCXX_TSAN_MUTEX_LOCKED(&_M_val);
return reinterpret_cast<pointer>(__current);
}
void
unlock(memory_order __o) const noexcept
{
_GLIBCXX_TSAN_MUTEX_PRE_UNLOCK(&_M_val);
_M_val.fetch_sub(1, __o);
_GLIBCXX_TSAN_MUTEX_POST_UNLOCK(&_M_val);
}
void
_M_swap_unlock(__count_type& __c, memory_order __o) noexcept
{
if (__o != memory_order_seq_cst)
__o = memory_order_release;
auto __x = reinterpret_cast<uintptr_t>(__c._M_pi);
_GLIBCXX_TSAN_MUTEX_PRE_UNLOCK(&_M_val);
__x = _M_val.exchange(__x, __o);
_GLIBCXX_TSAN_MUTEX_POST_UNLOCK(&_M_val);
__c._M_pi = reinterpret_cast<pointer>(__x & ~_S_lock_bit);
}
#if __cpp_lib_atomic_wait
void
_M_wait_unlock(memory_order __o) const noexcept
{
_GLIBCXX_TSAN_MUTEX_PRE_UNLOCK(&_M_val);
auto __v = _M_val.fetch_sub(1, memory_order_relaxed);
_GLIBCXX_TSAN_MUTEX_POST_UNLOCK(&_M_val);
_M_val.wait(__v & ~_S_lock_bit, __o);
}
void
notify_one() noexcept
{
_GLIBCXX_TSAN_MUTEX_PRE_SIGNAL(&_M_val);
_M_val.notify_one();
_GLIBCXX_TSAN_MUTEX_POST_SIGNAL(&_M_val);
}
void
notify_all() noexcept
{
_GLIBCXX_TSAN_MUTEX_PRE_SIGNAL(&_M_val);
_M_val.notify_all();
_GLIBCXX_TSAN_MUTEX_POST_SIGNAL(&_M_val);
}
#endif
private:
mutable __atomic_base<uintptr_t> _M_val{0};
static constexpr uintptr_t _S_lock_bit{1};
};
typename _Tp::element_type* _M_ptr = nullptr;
_Atomic_count _M_refcount;
static typename _Atomic_count::pointer
_S_add_ref(typename _Atomic_count::pointer __p)
{
if (__p)
{
if constexpr (__is_shared_ptr<_Tp>)
__p->_M_add_ref_copy();
else
__p->_M_weak_add_ref();
}
return __p;
}
constexpr _Sp_atomic() noexcept = default;
explicit
_Sp_atomic(value_type __r) noexcept
: _M_ptr(__r._M_ptr), _M_refcount(std::move(__r._M_refcount))
{ }
~_Sp_atomic() = default;
_Sp_atomic(const _Sp_atomic&) = delete;
void operator=(const _Sp_atomic&) = delete;
value_type
load(memory_order __o) const noexcept
{
__glibcxx_assert(__o != memory_order_release
&& __o != memory_order_acq_rel);
if (__o != memory_order_seq_cst)
__o = memory_order_acquire;
value_type __ret;
auto __pi = _M_refcount.lock(__o);
__ret._M_ptr = _M_ptr;
__ret._M_refcount._M_pi = _S_add_ref(__pi);
_M_refcount.unlock(memory_order_relaxed);
return __ret;
}
void
swap(value_type& __r, memory_order __o) noexcept
{
_M_refcount.lock(memory_order_acquire);
std::swap(_M_ptr, __r._M_ptr);
_M_refcount._M_swap_unlock(__r._M_refcount, __o);
}
bool
compare_exchange_strong(value_type& __expected, value_type __desired,
memory_order __o, memory_order __o2) noexcept
{
bool __result = true;
auto __pi = _M_refcount.lock(memory_order_acquire);
if (_M_ptr == __expected._M_ptr
&& __pi == __expected._M_refcount._M_pi)
{
_M_ptr = __desired._M_ptr;
_M_refcount._M_swap_unlock(__desired._M_refcount, __o);
}
else
{
_Tp __sink = std::move(__expected);
__expected._M_ptr = _M_ptr;
__expected._M_refcount._M_pi = _S_add_ref(__pi);
_M_refcount.unlock(__o2);
__result = false;
}
return __result;
}
#if __cpp_lib_atomic_wait
void
wait(value_type __old, memory_order __o) const noexcept
{
auto __pi = _M_refcount.lock(memory_order_acquire);
if (_M_ptr == __old._M_ptr && __pi == __old._M_refcount._M_pi)
_M_refcount._M_wait_unlock(__o);
else
_M_refcount.unlock(memory_order_relaxed);
}
void
notify_one() noexcept
{
_M_refcount.notify_one();
}
void
notify_all() noexcept
{
_M_refcount.notify_all();
}
#endif
};
template<typename _Tp>
class atomic<shared_ptr<_Tp>>
{
public:
using value_type = shared_ptr<_Tp>;
static constexpr bool is_always_lock_free = false;
bool
is_lock_free() const noexcept
{ return false; }
constexpr atomic() noexcept = default;
constexpr atomic(nullptr_t) noexcept : atomic() { }
atomic(shared_ptr<_Tp> __r) noexcept
: _M_impl(std::move(__r))
{ }
atomic(const atomic&) = delete;
void operator=(const atomic&) = delete;
shared_ptr<_Tp>
load(memory_order __o = memory_order_seq_cst) const noexcept
{ return _M_impl.load(__o); }
operator shared_ptr<_Tp>() const noexcept
{ return _M_impl.load(memory_order_seq_cst); }
void
store(shared_ptr<_Tp> __desired,
memory_order __o = memory_order_seq_cst) noexcept
{ _M_impl.swap(__desired, __o); }
void
operator=(shared_ptr<_Tp> __desired) noexcept
{ _M_impl.swap(__desired, memory_order_seq_cst); }
void
operator=(nullptr_t) noexcept
{ store(nullptr); }
shared_ptr<_Tp>
exchange(shared_ptr<_Tp> __desired,
memory_order __o = memory_order_seq_cst) noexcept
{
_M_impl.swap(__desired, __o);
return __desired;
}
bool
compare_exchange_strong(shared_ptr<_Tp>& __expected,
shared_ptr<_Tp> __desired,
memory_order __o, memory_order __o2) noexcept
{
return _M_impl.compare_exchange_strong(__expected, __desired, __o, __o2);
}
bool
compare_exchange_strong(value_type& __expected, value_type __desired,
memory_order __o = memory_order_seq_cst) noexcept
{
memory_order __o2;
switch (__o)
{
case memory_order_acq_rel:
__o2 = memory_order_acquire;
break;
case memory_order_release:
__o2 = memory_order_relaxed;
break;
default:
__o2 = __o;
}
return compare_exchange_strong(__expected, std::move(__desired),
__o, __o2);
}
bool
compare_exchange_weak(value_type& __expected, value_type __desired,
memory_order __o, memory_order __o2) noexcept
{
return compare_exchange_strong(__expected, std::move(__desired),
__o, __o2);
}
bool
compare_exchange_weak(value_type& __expected, value_type __desired,
memory_order __o = memory_order_seq_cst) noexcept
{
return compare_exchange_strong(__expected, std::move(__desired), __o);
}
#if __cpp_lib_atomic_wait
void
wait(value_type __old,
memory_order __o = memory_order_seq_cst) const noexcept
{
_M_impl.wait(std::move(__old), __o);
}
void
notify_one() noexcept
{
_M_impl.notify_one();
}
void
notify_all() noexcept
{
_M_impl.notify_all();
}
#endif
private:
_Sp_atomic<shared_ptr<_Tp>> _M_impl;
};
template<typename _Tp>
class atomic<weak_ptr<_Tp>>
{
public:
using value_type = weak_ptr<_Tp>;
static constexpr bool is_always_lock_free = false;
bool
is_lock_free() const noexcept
{ return false; }
constexpr atomic() noexcept = default;
atomic(weak_ptr<_Tp> __r) noexcept
: _M_impl(move(__r))
{ }
atomic(const atomic&) = delete;
void operator=(const atomic&) = delete;
weak_ptr<_Tp>
load(memory_order __o = memory_order_seq_cst) const noexcept
{ return _M_impl.load(__o); }
operator weak_ptr<_Tp>() const noexcept
{ return _M_impl.load(memory_order_seq_cst); }
void
store(weak_ptr<_Tp> __desired,
memory_order __o = memory_order_seq_cst) noexcept
{ _M_impl.swap(__desired, __o); }
void
operator=(weak_ptr<_Tp> __desired) noexcept
{ _M_impl.swap(__desired, memory_order_seq_cst); }
weak_ptr<_Tp>
exchange(weak_ptr<_Tp> __desired,
memory_order __o = memory_order_seq_cst) noexcept
{
_M_impl.swap(__desired, __o);
return __desired;
}
bool
compare_exchange_strong(weak_ptr<_Tp>& __expected,
weak_ptr<_Tp> __desired,
memory_order __o, memory_order __o2) noexcept
{
return _M_impl.compare_exchange_strong(__expected, __desired, __o, __o2);
}
bool
compare_exchange_strong(value_type& __expected, value_type __desired,
memory_order __o = memory_order_seq_cst) noexcept
{
memory_order __o2;
switch (__o)
{
case memory_order_acq_rel:
__o2 = memory_order_acquire;
break;
case memory_order_release:
__o2 = memory_order_relaxed;
break;
default:
__o2 = __o;
}
return compare_exchange_strong(__expected, std::move(__desired),
__o, __o2);
}
bool
compare_exchange_weak(value_type& __expected, value_type __desired,
memory_order __o, memory_order __o2) noexcept
{
return compare_exchange_strong(__expected, std::move(__desired),
__o, __o2);
}
bool
compare_exchange_weak(value_type& __expected, value_type __desired,
memory_order __o = memory_order_seq_cst) noexcept
{
return compare_exchange_strong(__expected, std::move(__desired), __o);
}
#if __cpp_lib_atomic_wait
void
wait(value_type __old,
memory_order __o = memory_order_seq_cst) const noexcept
{
_M_impl.wait(std::move(__old), __o);
}
void
notify_one() noexcept
{
_M_impl.notify_one();
}
void
notify_all() noexcept
{
_M_impl.notify_all();
}
#endif
private:
_Sp_atomic<weak_ptr<_Tp>> _M_impl;
};
#endif
_GLIBCXX_END_NAMESPACE_VERSION
}
#endif