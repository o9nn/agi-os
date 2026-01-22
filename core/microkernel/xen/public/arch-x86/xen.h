#include "../xen.h"
#ifndef __XEN_PUBLIC_ARCH_X86_XEN_H__
#define __XEN_PUBLIC_ARCH_X86_XEN_H__
#if __XEN_INTERFACE_VERSION__ >= 0x00030201
#define ___DEFINE_XEN_GUEST_HANDLE(name, type) \
typedef struct { type *p; } __guest_handle_ ## name
#else
#define ___DEFINE_XEN_GUEST_HANDLE(name, type) \
typedef type * __guest_handle_ ## name
#endif
#define __DEFINE_XEN_GUEST_HANDLE(name, type) \
___DEFINE_XEN_GUEST_HANDLE(name, type); \
___DEFINE_XEN_GUEST_HANDLE(const_##name, const type)
#define DEFINE_XEN_GUEST_HANDLE(name) __DEFINE_XEN_GUEST_HANDLE(name, name)
#define __XEN_GUEST_HANDLE(name) __guest_handle_ ## name
#define XEN_GUEST_HANDLE(name) __XEN_GUEST_HANDLE(name)
#define set_xen_guest_handle(hnd, val) do { (hnd).p = val; } while (0)
#ifdef __XEN_TOOLS__
#define get_xen_guest_handle(val, hnd) do { val = (hnd).p; } while (0)
#endif
#if defined(__i386__)
#include "xen-x86_32.h"
#elif defined(__x86_64__)
#include "xen-x86_64.h"
#endif
#ifndef __ASSEMBLY__
typedef unsigned long xen_pfn_t;
#define PRI_xen_pfn "lx"
#endif
#define FIRST_RESERVED_GDT_PAGE 14
#define FIRST_RESERVED_GDT_BYTE (FIRST_RESERVED_GDT_PAGE * 4096)
#define FIRST_RESERVED_GDT_ENTRY (FIRST_RESERVED_GDT_BYTE / 8)
#define MAX_VIRT_CPUS 32
#include "xen-mca.h"
#ifndef __ASSEMBLY__
typedef unsigned long xen_ulong_t;
#define TI_GET_DPL(_ti) ((_ti)->flags & 3)
#define TI_GET_IF(_ti) ((_ti)->flags & 4)
#define TI_SET_DPL(_ti,_dpl) ((_ti)->flags |= (_dpl))
#define TI_SET_IF(_ti,_if) ((_ti)->flags |= ((!!(_if))<<2))
struct trap_info {
uint8_t vector;
uint8_t flags;
uint16_t cs;
unsigned long address;
};
typedef struct trap_info trap_info_t;
DEFINE_XEN_GUEST_HANDLE(trap_info_t);
typedef uint64_t tsc_timestamp_t;
struct vcpu_guest_context {
struct { char x[512]; } fpu_ctxt;
#define VGCF_I387_VALID (1<<0)
#define VGCF_IN_KERNEL (1<<2)
#define _VGCF_i387_valid 0
#define VGCF_i387_valid (1<<_VGCF_i387_valid)
#define _VGCF_in_kernel 2
#define VGCF_in_kernel (1<<_VGCF_in_kernel)
#define _VGCF_failsafe_disables_events 3
#define VGCF_failsafe_disables_events (1<<_VGCF_failsafe_disables_events)
#define _VGCF_syscall_disables_events 4
#define VGCF_syscall_disables_events (1<<_VGCF_syscall_disables_events)
#define _VGCF_online 5
#define VGCF_online (1<<_VGCF_online)
unsigned long flags;
struct cpu_user_regs user_regs;
struct trap_info trap_ctxt[256];
unsigned long ldt_base, ldt_ents;
unsigned long gdt_frames[16], gdt_ents;
unsigned long kernel_ss, kernel_sp;
unsigned long ctrlreg[8];
unsigned long debugreg[8];
#ifdef __i386__
unsigned long event_callback_cs;
unsigned long event_callback_eip;
unsigned long failsafe_callback_cs;
unsigned long failsafe_callback_eip;
#else
unsigned long event_callback_eip;
unsigned long failsafe_callback_eip;
#ifdef __XEN__
union {
unsigned long syscall_callback_eip;
struct {
unsigned int event_callback_cs;
unsigned int failsafe_callback_cs;
};
};
#else
unsigned long syscall_callback_eip;
#endif
#endif
unsigned long vm_assist;
#ifdef __x86_64__
uint64_t fs_base;
uint64_t gs_base_kernel;
uint64_t gs_base_user;
#endif
};
typedef struct vcpu_guest_context vcpu_guest_context_t;
DEFINE_XEN_GUEST_HANDLE(vcpu_guest_context_t);
struct arch_shared_info {
unsigned long max_pfn;
xen_pfn_t pfn_to_mfn_frame_list_list;
unsigned long nmi_reason;
uint64_t pad[32];
};
typedef struct arch_shared_info arch_shared_info_t;
#endif
#ifdef __ASSEMBLY__
#define XEN_EMULATE_PREFIX .byte 0x0f,0x0b,0x78,0x65,0x6e ;
#define XEN_CPUID XEN_EMULATE_PREFIX cpuid
#else
#define XEN_EMULATE_PREFIX ".byte 0x0f,0x0b,0x78,0x65,0x6e ; "
#define XEN_CPUID XEN_EMULATE_PREFIX "cpuid"
#endif
#endif