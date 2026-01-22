#ifndef __XEN_PUBLIC_ARCH_X86_XEN_X86_32_H__
#define __XEN_PUBLIC_ARCH_X86_XEN_X86_32_H__
#if __XEN_INTERFACE_VERSION__ < 0x00030203
#define TRAP_INSTR "int $0x82"
#endif
#define FLAT_RING1_CS 0xe019
#define FLAT_RING1_DS 0xe021
#define FLAT_RING1_SS 0xe021
#define FLAT_RING3_CS 0xe02b
#define FLAT_RING3_DS 0xe033
#define FLAT_RING3_SS 0xe033
#define FLAT_KERNEL_CS FLAT_RING1_CS
#define FLAT_KERNEL_DS FLAT_RING1_DS
#define FLAT_KERNEL_SS FLAT_RING1_SS
#define FLAT_USER_CS FLAT_RING3_CS
#define FLAT_USER_DS FLAT_RING3_DS
#define FLAT_USER_SS FLAT_RING3_SS
#define __HYPERVISOR_VIRT_START_PAE 0xF5800000
#define __MACH2PHYS_VIRT_START_PAE 0xF5800000
#define __MACH2PHYS_VIRT_END_PAE 0xF6800000
#define HYPERVISOR_VIRT_START_PAE \
mk_unsigned_long(__HYPERVISOR_VIRT_START_PAE)
#define MACH2PHYS_VIRT_START_PAE \
mk_unsigned_long(__MACH2PHYS_VIRT_START_PAE)
#define MACH2PHYS_VIRT_END_PAE \
mk_unsigned_long(__MACH2PHYS_VIRT_END_PAE)
#define __HYPERVISOR_VIRT_START_NONPAE 0xFC000000
#define __MACH2PHYS_VIRT_START_NONPAE 0xFC000000
#define __MACH2PHYS_VIRT_END_NONPAE 0xFC400000
#define HYPERVISOR_VIRT_START_NONPAE \
mk_unsigned_long(__HYPERVISOR_VIRT_START_NONPAE)
#define MACH2PHYS_VIRT_START_NONPAE \
mk_unsigned_long(__MACH2PHYS_VIRT_START_NONPAE)
#define MACH2PHYS_VIRT_END_NONPAE \
mk_unsigned_long(__MACH2PHYS_VIRT_END_NONPAE)
#define __HYPERVISOR_VIRT_START __HYPERVISOR_VIRT_START_PAE
#define __MACH2PHYS_VIRT_START __MACH2PHYS_VIRT_START_PAE
#define __MACH2PHYS_VIRT_END __MACH2PHYS_VIRT_END_PAE
#ifndef HYPERVISOR_VIRT_START
#define HYPERVISOR_VIRT_START mk_unsigned_long(__HYPERVISOR_VIRT_START)
#endif
#define MACH2PHYS_VIRT_START mk_unsigned_long(__MACH2PHYS_VIRT_START)
#define MACH2PHYS_VIRT_END mk_unsigned_long(__MACH2PHYS_VIRT_END)
#define MACH2PHYS_NR_ENTRIES ((MACH2PHYS_VIRT_END-MACH2PHYS_VIRT_START)>>2)
#ifndef machine_to_phys_mapping
#define machine_to_phys_mapping ((unsigned long *)MACH2PHYS_VIRT_START)
#endif
#if defined(__XEN__) || defined(__XEN_TOOLS__)
#undef ___DEFINE_XEN_GUEST_HANDLE
#define ___DEFINE_XEN_GUEST_HANDLE(name, type) \
typedef struct { type *p; } \
__guest_handle_ ## name; \
typedef struct { union { type *p; uint64_aligned_t q; }; } \
__guest_handle_64_ ## name
#undef set_xen_guest_handle
#define set_xen_guest_handle(hnd, val) \
do { if ( sizeof(hnd) == 8 ) *(uint64_t *)&(hnd) = 0; \
(hnd).p = val; \
} while ( 0 )
#define uint64_aligned_t uint64_t __attribute__((aligned(8)))
#define __XEN_GUEST_HANDLE_64(name) __guest_handle_64_ ## name
#define XEN_GUEST_HANDLE_64(name) __XEN_GUEST_HANDLE_64(name)
#endif
#ifndef __ASSEMBLY__
struct cpu_user_regs {
uint32_t ebx;
uint32_t ecx;
uint32_t edx;
uint32_t esi;
uint32_t edi;
uint32_t ebp;
uint32_t eax;
uint16_t error_code;
uint16_t entry_vector;
uint32_t eip;
uint16_t cs;
uint8_t saved_upcall_mask;
uint8_t _pad0;
uint32_t eflags;
uint32_t esp;
uint16_t ss, _pad1;
uint16_t es, _pad2;
uint16_t ds, _pad3;
uint16_t fs, _pad4;
uint16_t gs, _pad5;
};
typedef struct cpu_user_regs cpu_user_regs_t;
DEFINE_XEN_GUEST_HANDLE(cpu_user_regs_t);
#define xen_pfn_to_cr3(pfn) (((unsigned)(pfn) << 12) | ((unsigned)(pfn) >> 20))
#define xen_cr3_to_pfn(cr3) (((unsigned)(cr3) >> 12) | ((unsigned)(cr3) << 20))
struct arch_vcpu_info {
unsigned long cr2;
unsigned long pad[5];
};
typedef struct arch_vcpu_info arch_vcpu_info_t;
struct xen_callback {
unsigned long cs;
unsigned long eip;
};
typedef struct xen_callback xen_callback_t;
#endif
#endif