#ifndef __XEN_PUBLIC_XEN_H__
#define __XEN_PUBLIC_XEN_H__
#include <sys/types.h>
#include "xen-compat.h"
#if defined(__i386__) || defined(__x86_64__)
#include "arch-x86/xen.h"
#elif defined(__ia64__)
#include "arch-ia64.h"
#else
#error "Unsupported architecture"
#endif
#ifndef __ASSEMBLY__
DEFINE_XEN_GUEST_HANDLE(char);
__DEFINE_XEN_GUEST_HANDLE(uchar, unsigned char);
DEFINE_XEN_GUEST_HANDLE(int);
__DEFINE_XEN_GUEST_HANDLE(uint, unsigned int);
DEFINE_XEN_GUEST_HANDLE(long);
__DEFINE_XEN_GUEST_HANDLE(ulong, unsigned long);
DEFINE_XEN_GUEST_HANDLE(void);
DEFINE_XEN_GUEST_HANDLE(xen_pfn_t);
#endif
#define __HYPERVISOR_set_trap_table 0
#define __HYPERVISOR_mmu_update 1
#define __HYPERVISOR_set_gdt 2
#define __HYPERVISOR_stack_switch 3
#define __HYPERVISOR_set_callbacks 4
#define __HYPERVISOR_fpu_taskswitch 5
#define __HYPERVISOR_sched_op_compat 6
#define __HYPERVISOR_platform_op 7
#define __HYPERVISOR_set_debugreg 8
#define __HYPERVISOR_get_debugreg 9
#define __HYPERVISOR_update_descriptor 10
#define __HYPERVISOR_memory_op 12
#define __HYPERVISOR_multicall 13
#define __HYPERVISOR_update_va_mapping 14
#define __HYPERVISOR_set_timer_op 15
#define __HYPERVISOR_event_channel_op_compat 16
#define __HYPERVISOR_xen_version 17
#define __HYPERVISOR_console_io 18
#define __HYPERVISOR_physdev_op_compat 19
#define __HYPERVISOR_grant_table_op 20
#define __HYPERVISOR_vm_assist 21
#define __HYPERVISOR_update_va_mapping_otherdomain 22
#define __HYPERVISOR_iret 23
#define __HYPERVISOR_vcpu_op 24
#define __HYPERVISOR_set_segment_base 25
#define __HYPERVISOR_mmuext_op 26
#define __HYPERVISOR_xsm_op 27
#define __HYPERVISOR_nmi_op 28
#define __HYPERVISOR_sched_op 29
#define __HYPERVISOR_callback_op 30
#define __HYPERVISOR_xenoprof_op 31
#define __HYPERVISOR_event_channel_op 32
#define __HYPERVISOR_physdev_op 33
#define __HYPERVISOR_hvm_op 34
#define __HYPERVISOR_sysctl 35
#define __HYPERVISOR_domctl 36
#define __HYPERVISOR_kexec_op 37
#define __HYPERVISOR_arch_0 48
#define __HYPERVISOR_arch_1 49
#define __HYPERVISOR_arch_2 50
#define __HYPERVISOR_arch_3 51
#define __HYPERVISOR_arch_4 52
#define __HYPERVISOR_arch_5 53
#define __HYPERVISOR_arch_6 54
#define __HYPERVISOR_arch_7 55
#if __XEN_INTERFACE_VERSION__ < 0x00030101
#undef __HYPERVISOR_sched_op
#define __HYPERVISOR_sched_op __HYPERVISOR_sched_op_compat
#endif
#if __XEN_INTERFACE_VERSION__ < 0x00030202
#undef __HYPERVISOR_event_channel_op
#define __HYPERVISOR_event_channel_op __HYPERVISOR_event_channel_op_compat
#undef __HYPERVISOR_physdev_op
#define __HYPERVISOR_physdev_op __HYPERVISOR_physdev_op_compat
#endif
#if __XEN_INTERFACE_VERSION__ < 0x00030204
#define __HYPERVISOR_dom0_op __HYPERVISOR_platform_op
#endif
#define VIRQ_TIMER 0
#define VIRQ_DEBUG 1
#define VIRQ_CONSOLE 2
#define VIRQ_DOM_EXC 3
#define VIRQ_TBUF 4
#define VIRQ_DEBUGGER 6
#define VIRQ_XENOPROF 7
#define VIRQ_CON_RING 8
#define VIRQ_ARCH_0 16
#define VIRQ_ARCH_1 17
#define VIRQ_ARCH_2 18
#define VIRQ_ARCH_3 19
#define VIRQ_ARCH_4 20
#define VIRQ_ARCH_5 21
#define VIRQ_ARCH_6 22
#define VIRQ_ARCH_7 23
#define NR_VIRQS 24
#define MMU_NORMAL_PT_UPDATE 0
#define MMU_MACHPHYS_UPDATE 1
#define MMU_PT_UPDATE_PRESERVE_AD 2
#define MMUEXT_PIN_L1_TABLE 0
#define MMUEXT_PIN_L2_TABLE 1
#define MMUEXT_PIN_L3_TABLE 2
#define MMUEXT_PIN_L4_TABLE 3
#define MMUEXT_UNPIN_TABLE 4
#define MMUEXT_NEW_BASEPTR 5
#define MMUEXT_TLB_FLUSH_LOCAL 6
#define MMUEXT_INVLPG_LOCAL 7
#define MMUEXT_TLB_FLUSH_MULTI 8
#define MMUEXT_INVLPG_MULTI 9
#define MMUEXT_TLB_FLUSH_ALL 10
#define MMUEXT_INVLPG_ALL 11
#define MMUEXT_FLUSH_CACHE 12
#define MMUEXT_SET_LDT 13
#define MMUEXT_NEW_USER_BASEPTR 15
#define MMUEXT_CLEAR_PAGE 16
#define MMUEXT_COPY_PAGE 17
#ifndef __ASSEMBLY__
struct mmuext_op {
unsigned int cmd;
union {
xen_pfn_t mfn;
unsigned long linear_addr;
} arg1;
union {
unsigned int nr_ents;
#if __XEN_INTERFACE_VERSION__ >= 0x00030205
XEN_GUEST_HANDLE(void) vcpumask;
#else
void *vcpumask;
#endif
xen_pfn_t src_mfn;
} arg2;
};
typedef struct mmuext_op mmuext_op_t;
DEFINE_XEN_GUEST_HANDLE(mmuext_op_t);
#endif
#define UVMF_NONE (0UL<<0)
#define UVMF_TLB_FLUSH (1UL<<0)
#define UVMF_INVLPG (2UL<<0)
#define UVMF_FLUSHTYPE_MASK (3UL<<0)
#define UVMF_MULTI (0UL<<2)
#define UVMF_LOCAL (0UL<<2)
#define UVMF_ALL (1UL<<2)
#define CONSOLEIO_write 0
#define CONSOLEIO_read 1
#define VMASST_CMD_enable 0
#define VMASST_CMD_disable 1
#define VMASST_TYPE_4gb_segments 0
#define VMASST_TYPE_4gb_segments_notify 1
#define VMASST_TYPE_writable_pagetables 2
#define VMASST_TYPE_pae_extended_cr3 3
#define MAX_VMASST_TYPE 3
#ifndef __ASSEMBLY__
typedef uint16_t domid_t;
#define DOMID_FIRST_RESERVED (0x7FF0U)
#define DOMID_SELF (0x7FF0U)
#define DOMID_IO (0x7FF1U)
#define DOMID_XEN (0x7FF2U)
struct mmu_update {
uint64_t ptr;
uint64_t val;
};
typedef struct mmu_update mmu_update_t;
DEFINE_XEN_GUEST_HANDLE(mmu_update_t);
struct multicall_entry {
unsigned long op, result;
unsigned long args[6];
};
typedef struct multicall_entry multicall_entry_t;
DEFINE_XEN_GUEST_HANDLE(multicall_entry_t);
#define NR_EVENT_CHANNELS (sizeof(unsigned long) * sizeof(unsigned long) * 64)
struct vcpu_time_info {
uint32_t version;
uint32_t pad0;
uint64_t tsc_timestamp;
uint64_t system_time;
uint32_t tsc_to_system_mul;
int8_t tsc_shift;
int8_t pad1[3];
};
typedef struct vcpu_time_info vcpu_time_info_t;
struct vcpu_info {
uint8_t evtchn_upcall_pending;
uint8_t evtchn_upcall_mask;
unsigned long evtchn_pending_sel;
struct arch_vcpu_info arch;
struct vcpu_time_info time;
};
#ifndef __XEN__
typedef struct vcpu_info vcpu_info_t;
#endif
struct shared_info {
struct vcpu_info vcpu_info[MAX_VIRT_CPUS];
unsigned long evtchn_pending[sizeof(unsigned long) * 8];
unsigned long evtchn_mask[sizeof(unsigned long) * 8];
uint32_t wc_version;
uint32_t wc_sec;
uint32_t wc_nsec;
struct arch_shared_info arch;
};
#ifndef __XEN__
typedef struct shared_info shared_info_t;
#endif
#define MAX_GUEST_CMDLINE 1024
struct start_info {
char magic[32];
unsigned long nr_pages;
unsigned long shared_info;
uint32_t flags;
xen_pfn_t store_mfn;
uint32_t store_evtchn;
union {
struct {
xen_pfn_t mfn;
uint32_t evtchn;
} domU;
struct {
uint32_t info_off;
uint32_t info_size;
} dom0;
} console;
unsigned long pt_base;
unsigned long nr_pt_frames;
unsigned long mfn_list;
unsigned long mod_start;
unsigned long mod_len;
int8_t cmd_line[MAX_GUEST_CMDLINE];
unsigned mods_count;
};
typedef struct start_info start_info_t;
#if __XEN_INTERFACE_VERSION__ < 0x00030203
#define console_mfn console.domU.mfn
#define console_evtchn console.domU.evtchn
#endif
#define SIF_PRIVILEGED (1<<0)
#define SIF_INITDOMAIN (1<<1)
#define SIF_MULTIBOOT_MOD (1<<2)
#define SIF_PM_MASK (0xFF<<8)
typedef struct dom0_vga_console_info {
uint8_t video_type;
#define XEN_VGATYPE_TEXT_MODE_3 0x03
#define XEN_VGATYPE_VESA_LFB 0x23
union {
struct {
uint16_t font_height;
uint16_t cursor_x, cursor_y;
uint16_t rows, columns;
} text_mode_3;
struct {
uint16_t width, height;
uint16_t bytes_per_line;
uint16_t bits_per_pixel;
uint32_t lfb_base;
uint32_t lfb_size;
uint8_t red_pos, red_size;
uint8_t green_pos, green_size;
uint8_t blue_pos, blue_size;
uint8_t rsvd_pos, rsvd_size;
#if __XEN_INTERFACE_VERSION__ >= 0x00030206
uint32_t gbl_caps;
uint16_t mode_attrs;
#endif
} vesa_lfb;
} u;
} dom0_vga_console_info_t;
#define xen_vga_console_info dom0_vga_console_info
#define xen_vga_console_info_t dom0_vga_console_info_t
typedef uint8_t xen_domain_handle_t[16];
#define __mk_unsigned_long(x) x ## UL
#define mk_unsigned_long(x) __mk_unsigned_long(x)
__DEFINE_XEN_GUEST_HANDLE(uint8, uint8_t);
__DEFINE_XEN_GUEST_HANDLE(uint16, uint16_t);
__DEFINE_XEN_GUEST_HANDLE(uint32, uint32_t);
__DEFINE_XEN_GUEST_HANDLE(uint64, uint64_t);
#else
#define mk_unsigned_long(x) x
#endif
#if defined(__XEN__) || defined(__XEN_TOOLS__)
#ifndef uint64_aligned_t
#define uint64_aligned_t uint64_t
#endif
#ifndef XEN_GUEST_HANDLE_64
#define XEN_GUEST_HANDLE_64(name) XEN_GUEST_HANDLE(name)
#endif
#endif
#endif