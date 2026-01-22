#ifndef _MACH_DEBUG_VM_INFO_H_
#define _MACH_DEBUG_VM_INFO_H_
#include <mach/boolean.h>
#include <mach/machine/vm_types.h>
#include <mach/vm_inherit.h>
#include <mach/vm_prot.h>
#include <mach/memory_object.h>
#include <stdint.h>
typedef struct vm_region_info {
rpc_vm_offset_t vri_start;
rpc_vm_offset_t vri_end;
vm_prot_t vri_protection;
vm_prot_t vri_max_protection;
vm_inherit_t vri_inheritance;
unsigned int vri_wired_count;
unsigned int vri_user_wired_count;
rpc_vm_offset_t vri_object;
rpc_vm_offset_t vri_offset;
integer_t vri_needs_copy;
unsigned int vri_sharing;
} vm_region_info_t;
typedef vm_region_info_t *vm_region_info_array_t;
typedef uint32_t vm_object_info_state_t;
#define VOI_STATE_PAGER_CREATED 0x00000001
#define VOI_STATE_PAGER_INITIALIZED 0x00000002
#define VOI_STATE_PAGER_READY 0x00000004
#define VOI_STATE_CAN_PERSIST 0x00000008
#define VOI_STATE_INTERNAL 0x00000010
#define VOI_STATE_TEMPORARY 0x00000020
#define VOI_STATE_ALIVE 0x00000040
#define VOI_STATE_LOCK_IN_PROGRESS 0x00000080
#define VOI_STATE_LOCK_RESTART 0x00000100
typedef struct vm_object_info {
rpc_vm_offset_t voi_object;
rpc_vm_size_t voi_pagesize;
rpc_vm_size_t voi_size;
unsigned int voi_ref_count;
unsigned int voi_resident_page_count;
unsigned int voi_absent_count;
rpc_vm_offset_t voi_copy;
rpc_vm_offset_t voi_shadow;
rpc_vm_offset_t voi_shadow_offset;
rpc_vm_offset_t voi_paging_offset;
memory_object_copy_strategy_t voi_copy_strategy;
rpc_vm_offset_t voi_last_alloc;
unsigned int voi_paging_in_progress;
vm_object_info_state_t voi_state;
} vm_object_info_t;
typedef vm_object_info_t *vm_object_info_array_t;
typedef uint32_t vm_page_info_state_t;
#define VPI_STATE_BUSY 0x00000001
#define VPI_STATE_WANTED 0x00000002
#define VPI_STATE_TABLED 0x00000004
#define VPI_STATE_FICTITIOUS 0x00000008
#define VPI_STATE_PRIVATE 0x00000010
#define VPI_STATE_ABSENT 0x00000020
#define VPI_STATE_ERROR 0x00000040
#define VPI_STATE_DIRTY 0x00000080
#define VPI_STATE_PRECIOUS 0x00000100
#define VPI_STATE_OVERWRITING 0x00000200
#define VPI_STATE_INACTIVE 0x00000400
#define VPI_STATE_ACTIVE 0x00000800
#define VPI_STATE_LAUNDRY 0x00001000
#define VPI_STATE_FREE 0x00002000
#define VPI_STATE_REFERENCE 0x00004000
#define VPI_STATE_PAGER 0x80000000
typedef struct vm_page_info {
rpc_vm_offset_t vpi_offset;
rpc_vm_offset_t vpi_phys_addr;
unsigned int vpi_wire_count;
vm_prot_t vpi_page_lock;
vm_prot_t vpi_unlock_request;
vm_page_info_state_t vpi_state;
} vm_page_info_t;
typedef vm_page_info_t *vm_page_info_array_t;
typedef struct vm_page_phys_info {
rpc_vm_offset_t vpi_offset;
rpc_phys_addr_t vpi_phys_addr;
unsigned int vpi_wire_count;
vm_prot_t vpi_page_lock;
vm_prot_t vpi_unlock_request;
vm_page_info_state_t vpi_state;
} vm_page_phys_info_t;
typedef vm_page_phys_info_t *vm_page_phys_info_array_t;
#endif