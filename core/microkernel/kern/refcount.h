#ifndef _KERN_REFCOUNT_H_
#define _KERN_REFCOUNT_H_
#include <kern/macros.h>
#ifndef MACHINE_REFCOUNT
#include <kern/lock.h>
struct RefCount {
decl_simple_lock_data(,lock)
int ref_count;
};
typedef struct RefCount RefCount;
#define refcount_init(refcount, refs) \
MACRO_BEGIN \
simple_lock_init(&(refcount)->lock); \
((refcount)->ref_count = (refs)); \
MACRO_END
#define refcount_take(refcount) \
MACRO_BEGIN \
simple_lock(&(refcount)->lock); \
(refcount)->ref_count++; \
simple_unlock(&(refcount)->lock); \
MACRO_END
#define refcount_drop(refcount, func) \
MACRO_BEGIN \
int new_value; \
simple_lock(&(refcount)->lock); \
new_value = --(refcount)->ref_count; \
simple_unlock(&(refcount)->lock); \
if (new_value == 0) { func; } \
MACRO_END
#endif
#endif