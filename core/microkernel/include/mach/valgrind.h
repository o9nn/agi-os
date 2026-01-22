#ifndef _MACH_VALGRIND_H_
#define _MACH_VALGRIND_H_
#include <mach/boolean.h>
#include <mach/kern_return.h>
#include <mach/std_types.h>
#include <mach/machine/vm_types.h>
#include <vm/vm_types.h>
#ifndef CONFIG_VALGRIND_SUPPORT
#define CONFIG_VALGRIND_SUPPORT 1
#endif
typedef enum {
VALGRIND_MEM_UNDEFINED = 0,
VALGRIND_MEM_DEFINED,
VALGRIND_MEM_ADDRESSABLE,
VALGRIND_MEM_NOACCESS
} valgrind_mem_state_t;
struct valgrind_mem_record {
vm_address_t start_addr;
vm_size_t size;
valgrind_mem_state_t state;
uint32_t alloc_id;
vm_address_t caller_pc;
uint32_t timestamp;
} __attribute__((packed));
typedef enum {
VALGRIND_OP_MALLOC = 0,
VALGRIND_OP_FREE,
VALGRIND_OP_REALLOC,
VALGRIND_OP_CALLOC,
VALGRIND_OP_MMAP,
VALGRIND_OP_MUNMAP,
VALGRIND_OP_STACK_ALLOC,
VALGRIND_OP_STACK_FREE,
VALGRIND_OP_MAKE_DEFINED,
VALGRIND_OP_MAKE_UNDEFINED,
VALGRIND_OP_MAKE_NOACCESS
} valgrind_op_type_t;
struct valgrind_notification {
valgrind_op_type_t operation;
vm_address_t address;
vm_size_t size;
vm_address_t old_address;
vm_size_t old_size;
uint32_t flags;
};
#define VALGRIND_MAX_RECORDS 4096
extern kern_return_t valgrind_init(void);
extern kern_return_t valgrind_enable(boolean_t enable);
extern boolean_t valgrind_is_enabled(void);
extern kern_return_t valgrind_track_alloc(vm_address_t addr, vm_size_t size,
vm_address_t caller_pc);
extern kern_return_t valgrind_track_free(vm_address_t addr);
extern kern_return_t valgrind_track_realloc(vm_address_t old_addr, vm_address_t new_addr,
vm_size_t new_size);
extern kern_return_t valgrind_make_mem_defined(vm_address_t addr, vm_size_t size);
extern kern_return_t valgrind_make_mem_undefined(vm_address_t addr, vm_size_t size);
extern kern_return_t valgrind_make_mem_noaccess(vm_address_t addr, vm_size_t size);
extern valgrind_mem_state_t valgrind_check_mem_state(vm_address_t addr);
extern boolean_t valgrind_is_mem_defined(vm_address_t addr, vm_size_t size);
extern boolean_t valgrind_is_mem_addressable(vm_address_t addr, vm_size_t size);
extern void valgrind_report_error(const char *error_type, vm_address_t addr,
vm_size_t size, const char *description);
extern void valgrind_print_backtrace(vm_address_t pc);
#if CONFIG_VALGRIND_SUPPORT
#define VALGRIND_CLIENT_REQUEST_BASE 0x1000
enum {
VG_USERREQ_MAKE_MEM_NOACCESS = VALGRIND_CLIENT_REQUEST_BASE,
VG_USERREQ_MAKE_MEM_UNDEFINED,
VG_USERREQ_MAKE_MEM_DEFINED,
VG_USERREQ_DISCARD_TRANSLATIONS,
VG_USERREQ_CHECK_MEM_IS_ADDRESSABLE,
VG_USERREQ_CHECK_MEM_IS_DEFINED,
VG_USERREQ_DO_LEAK_CHECK,
VG_USERREQ_COUNT_ERRORS
};
#define VALGRIND_DO_CLIENT_REQUEST(result, default_val, request_code, \
arg1, arg2, arg3, arg4, arg5) \
do { \
(result) = valgrind_handle_client_request((request_code), \
(vm_address_t)(arg1), (vm_address_t)(arg2), \
(vm_address_t)(arg3), (vm_address_t)(arg4), \
(vm_address_t)(arg5)); \
if ((result) == KERN_INVALID_ARGUMENT) \
(result) = (default_val); \
} while (0)
extern kern_return_t valgrind_handle_client_request(uint32_t request,
vm_address_t arg1, vm_address_t arg2,
vm_address_t arg3, vm_address_t arg4,
vm_address_t arg5);
#define VALGRIND_MAKE_MEM_NOACCESS(addr, size) \
valgrind_make_mem_noaccess((vm_address_t)(addr), (size))
#define VALGRIND_MAKE_MEM_UNDEFINED(addr, size) \
valgrind_make_mem_undefined((vm_address_t)(addr), (size))
#define VALGRIND_MAKE_MEM_DEFINED(addr, size) \
valgrind_make_mem_defined((vm_address_t)(addr), (size))
#define VALGRIND_CHECK_MEM_IS_DEFINED(addr, size) \
valgrind_is_mem_defined((vm_address_t)(addr), (size))
#define VALGRIND_CHECK_MEM_IS_ADDRESSABLE(addr, size) \
valgrind_is_mem_addressable((vm_address_t)(addr), (size))
#else
#define VALGRIND_MAKE_MEM_NOACCESS(addr, size) do { } while (0)
#define VALGRIND_MAKE_MEM_UNDEFINED(addr, size) do { } while (0)
#define VALGRIND_MAKE_MEM_DEFINED(addr, size) do { } while (0)
#define VALGRIND_CHECK_MEM_IS_DEFINED(addr, size) TRUE
#define VALGRIND_CHECK_MEM_IS_ADDRESSABLE(addr, size) TRUE
#endif
#define MACH_VALGRIND_ENABLE_CALL 3600
#define MACH_VALGRIND_TRACK_ALLOC 3601
#define MACH_VALGRIND_TRACK_FREE 3602
#define MACH_VALGRIND_MAKE_DEFINED 3603
#define MACH_VALGRIND_MAKE_UNDEFINED 3604
#define MACH_VALGRIND_MAKE_NOACCESS 3605
#define MACH_VALGRIND_CHECK_MEM 3606
#define MACH_VALGRIND_CLIENT_REQUEST 3607
#endif