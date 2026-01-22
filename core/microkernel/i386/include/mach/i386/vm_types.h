#ifndef _MACHINE_VM_TYPES_H_
#define _MACHINE_VM_TYPES_H_ 1
#ifdef __ASSEMBLER__
#else
#include <stdint.h>
#ifdef MACH_KERNEL
#include <kern/assert.h>
#endif
typedef unsigned int natural_t;
typedef int integer_t;
typedef unsigned long long_natural_t;
typedef long long_integer_t;
typedef uintptr_t vm_offset_t;
typedef vm_offset_t * vm_offset_array_t;
#ifdef MACH_KERNEL
#ifdef PAE
typedef unsigned long long phys_addr_t;
#else
typedef unsigned long phys_addr_t;
#endif
#else
typedef unsigned long long phys_addr_t;
#endif
typedef unsigned long long rpc_phys_addr_t;
typedef rpc_phys_addr_t *rpc_phys_addr_array_t;
typedef uintptr_t vm_size_t;
typedef vm_size_t * vm_size_array_t;
#if defined(MACH_KERNEL) && defined(USER32)
typedef uint32_t rpc_uintptr_t;
typedef uint32_t rpc_vm_address_t;
typedef uint32_t rpc_vm_offset_t;
typedef uint32_t rpc_vm_size_t;
static inline uint64_t convert_vm_from_user(uint32_t uaddr)
{
return (uint64_t)uaddr;
}
static inline uint32_t convert_vm_to_user(uint64_t kaddr)
{
assert(kaddr <= 0xFFFFFFFF);
return (uint32_t)kaddr;
}
typedef uint32_t rpc_long_natural_t;
typedef int32_t rpc_long_integer_t;
static inline int64_t convert_long_integer_from_user(int32_t i)
{
return (int64_t)i;
}
static inline int32_t convert_long_integer_to_user(int64_t i)
{
assert(i <= 0x7FFFFFFF);
return (int32_t)i;
}
typedef uint32_t rpc_long_natural_t;
typedef int32_t rpc_long_integer_t;
#elif defined(MACH_KERNEL) && defined(KERNEL_USER)
typedef uint64_t rpc_uintptr_t;
typedef uint64_t rpc_vm_address_t;
typedef uint64_t rpc_vm_offset_t;
typedef uint64_t rpc_vm_size_t;
#define convert_vm_to_user null_conversion
#define convert_vm_from_user null_conversion
typedef long_natural_t rpc_long_natural_t;
typedef long_integer_t rpc_long_integer_t;
#define convert_long_integer_to_user null_conversion
#define convert_long_integer_from_user null_conversion
#else
typedef uintptr_t rpc_uintptr_t;
typedef vm_offset_t rpc_vm_address_t;
typedef vm_offset_t rpc_vm_offset_t;
typedef vm_size_t rpc_vm_size_t;
#define convert_vm_to_user null_conversion
#define convert_vm_from_user null_conversion
typedef long_natural_t rpc_long_natural_t;
typedef long_integer_t rpc_long_integer_t;
#define convert_long_integer_to_user null_conversion
#define convert_long_integer_from_user null_conversion
#endif
#define convert_long_natural_to_user convert_vm_to_user
#define convert_long_natural_from_user convert_vm_from_user
typedef rpc_vm_size_t * rpc_vm_size_array_t;
typedef rpc_vm_offset_t * rpc_vm_offset_array_t;
#endif
#define MACH_MSG_TYPE_INTEGER_T MACH_MSG_TYPE_INTEGER_32
#endif