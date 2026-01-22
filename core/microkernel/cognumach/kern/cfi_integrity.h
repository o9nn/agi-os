#ifndef _KERN_CFI_INTEGRITY_H_
#define _KERN_CFI_INTEGRITY_H_
#include <mach/mach_security.h>
#include <mach/boolean.h>
#include <mach/kern_return.h>
extern void cfi_init_context(struct cfi_context *ctx, uintptr_t stack_base, uintptr_t stack_limit);
extern cfi_result_t cfi_validate_return(uintptr_t return_addr, uintptr_t expected);
extern cfi_result_t cfi_validate_call_target(uintptr_t target);
extern cfi_result_t cfi_check_stack_integrity(struct cfi_context *ctx);
extern cfi_result_t cfi_protected_call(uintptr_t target, uintptr_t return_site);
extern cfi_result_t cfi_protected_return(uintptr_t return_addr);
extern void cfi_init(void);
extern kern_return_t cfi_add_function(uintptr_t entry_point);
extern kern_return_t cfi_remove_function(uintptr_t entry_point);
extern int cfi_get_call_depth(void);
extern void cfi_dump_call_stack(void);
#endif