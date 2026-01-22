#ifndef _KERN_ATOMIC_H_
#define _KERN_ATOMIC_H_ 1
#define __atomic_cas_helper(ptr, exp, nval, mo) \
({ \
typeof(exp) __e = (exp); \
__atomic_compare_exchange_n ((ptr), &__e, (nval), 0, \
__ATOMIC_##mo, __ATOMIC_RELAXED); \
})
#define atomic_cas_acq(ptr, exp, nval) \
__atomic_cas_helper (ptr, exp, nval, ACQUIRE)
#define atomic_cas_rel(ptr, exp, nval) \
__atomic_cas_helper (ptr, exp, nval, RELEASE)
#define atomic_cas_seq(ptr, exp, nval) \
__atomic_cas_helper (ptr, exp, nval, SEQ_CST)
#define __atomic_swap_helper(ptr, val, mo) \
__atomic_exchange_n ((ptr), (val), __ATOMIC_##mo)
#define atomic_swap_acq(ptr, val) \
__atomic_swap_helper (ptr, val, ACQUIRE)
#define atomic_swap_rel(ptr, val) \
__atomic_swap_helper (ptr, val, RELEASE)
#define atomic_swap_seq(ptr, val) \
__atomic_swap_helper (ptr, val, SEQ_CST)
#endif