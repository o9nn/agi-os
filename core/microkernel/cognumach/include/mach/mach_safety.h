#ifndef _MACH_MACH_SAFETY_H_
#define _MACH_MACH_SAFETY_H_
#include <mach/kern_return.h>
#include <stdint.h>
#ifndef UINT32_MAX
#define UINT32_MAX 0xFFFFFFFFU
#endif
#ifndef INT32_MAX
#define INT32_MAX 0x7FFFFFFF
#endif
#ifndef SIZE_MAX
#define SIZE_MAX (~(size_t)0)
#endif
#define MACH_SAFE_ADD_CHECK(a, b, type) \
((a) > type##_MAX - (b))
#define MACH_SAFE_MUL_CHECK(a, b, type) \
((a) != 0 && (b) > type##_MAX / (a))
#define MACH_SAFE_ADD(a, b, result, type) \
(MACH_SAFE_ADD_CHECK(a, b, type) ? KERN_INVALID_ARGUMENT : \
(*(result) = (a) + (b), KERN_SUCCESS))
#define MACH_SAFE_MUL(a, b, result, type) \
(MACH_SAFE_MUL_CHECK(a, b, type) ? KERN_INVALID_ARGUMENT : \
(*(result) = (a) * (b), KERN_SUCCESS))
#define MACH_BOUNDS_CHECK(base, offset, size, limit) \
((offset) < (size) && (base) + (offset) <= (limit))
#define MACH_RANGE_CHECK(start, length, limit) \
((start) <= (limit) && (length) <= (limit) - (start))
#define MACH_VALIDATE_PTR(ptr, min_addr, max_addr) \
((ptr) != NULL && (uintptr_t)(ptr) >= (uintptr_t)(min_addr) && \
(uintptr_t)(ptr) < (uintptr_t)(max_addr))
#define MACH_VALIDATE_REGION(addr, size) \
((size) > 0 && (uintptr_t)(addr) + (size) > (uintptr_t)(addr))
#define MACH_VM_ENTRY_VALID(entry) \
((entry) != NULL && (entry)->start <= (entry)->end)
#define MACH_VM_ALIGNED(addr, align) \
(((uintptr_t)(addr) & ((align) - 1)) == 0)
#endif