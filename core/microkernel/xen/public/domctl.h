#ifndef __XEN_PUBLIC_DOMCTL_H__
#define __XEN_PUBLIC_DOMCTL_H__
#if !defined(__XEN__) && !defined(__XEN_TOOLS__)
#error "domctl operations are intended for use by node control tools only"
#endif
#include "xen.h"
#define XEN_DOMCTL_INTERFACE_VERSION 0x00000005
struct xenctl_cpumap {
XEN_GUEST_HANDLE_64(uint8) bitmap;
uint32_t nr_cpus;
};
#define XEN_DOMCTL_createdomain       1
struct xen_domctl_createdomain {
uint32_t ssidref;
xen_domain_handle_t handle;
#define _XEN_DOMCTL_CDF_hvm_guest 0
#define XEN_DOMCTL_CDF_hvm_guest  (1U<<_XEN_DOMCTL_CDF_hvm_guest)
#define _XEN_DOMCTL_CDF_hap       1
#define XEN_DOMCTL_CDF_hap        (1U<<_XEN_DOMCTL_CDF_hap)
uint32_t flags;
};
typedef struct xen_domctl_createdomain xen_domctl_createdomain_t;
DEFINE_XEN_GUEST_HANDLE(xen_domctl_createdomain_t);
#define XEN_DOMCTL_destroydomain      2
#define XEN_DOMCTL_pausedomain        3
#define XEN_DOMCTL_unpausedomain      4
#define XEN_DOMCTL_resumedomain      27
#define XEN_DOMCTL_getdomaininfo      5
struct xen_domctl_getdomaininfo {
domid_t  domain;
#define _XEN_DOMINF_dying     0
#define XEN_DOMINF_dying      (1U<<_XEN_DOMINF_dying)
#define _XEN_DOMINF_hvm_guest 1
#define XEN_DOMINF_hvm_guest  (1U<<_XEN_DOMINF_hvm_guest)
#define _XEN_DOMINF_shutdown  2
#define XEN_DOMINF_shutdown   (1U<<_XEN_DOMINF_shutdown)
#define _XEN_DOMINF_paused    3
#define XEN_DOMINF_paused     (1U<<_XEN_DOMINF_paused)
#define _XEN_DOMINF_blocked   4
#define XEN_DOMINF_blocked    (1U<<_XEN_DOMINF_blocked)
#define _XEN_DOMINF_running   5
#define XEN_DOMINF_running    (1U<<_XEN_DOMINF_running)
#define _XEN_DOMINF_debugged  6
#define XEN_DOMINF_debugged   (1U<<_XEN_DOMINF_debugged)
#define XEN_DOMINF_cpumask      255
#define XEN_DOMINF_cpushift       8
#define XEN_DOMINF_shutdownmask 255
#define XEN_DOMINF_shutdownshift 16
uint32_t flags;
uint64_aligned_t tot_pages;
uint64_aligned_t max_pages;
uint64_aligned_t shared_info_frame;
uint64_aligned_t cpu_time;
uint32_t nr_online_vcpus;
uint32_t max_vcpu_id;
uint32_t ssidref;
xen_domain_handle_t handle;
};
typedef struct xen_domctl_getdomaininfo xen_domctl_getdomaininfo_t;
DEFINE_XEN_GUEST_HANDLE(xen_domctl_getdomaininfo_t);
#define XEN_DOMCTL_getmemlist         6
struct xen_domctl_getmemlist {
uint64_aligned_t max_pfns;
uint64_aligned_t start_pfn;
XEN_GUEST_HANDLE_64(uint64) buffer;
uint64_aligned_t num_pfns;
};
typedef struct xen_domctl_getmemlist xen_domctl_getmemlist_t;
DEFINE_XEN_GUEST_HANDLE(xen_domctl_getmemlist_t);
#define XEN_DOMCTL_getpageframeinfo   7
#define XEN_DOMCTL_PFINFO_LTAB_SHIFT 28
#define XEN_DOMCTL_PFINFO_NOTAB   (0x0U<<28)
#define XEN_DOMCTL_PFINFO_L1TAB   (0x1U<<28)
#define XEN_DOMCTL_PFINFO_L2TAB   (0x2U<<28)
#define XEN_DOMCTL_PFINFO_L3TAB   (0x3U<<28)
#define XEN_DOMCTL_PFINFO_L4TAB   (0x4U<<28)
#define XEN_DOMCTL_PFINFO_LTABTYPE_MASK (0x7U<<28)
#define XEN_DOMCTL_PFINFO_LPINTAB (0x1U<<31)
#define XEN_DOMCTL_PFINFO_XTAB    (0xfU<<28)
#define XEN_DOMCTL_PFINFO_LTAB_MASK (0xfU<<28)
struct xen_domctl_getpageframeinfo {
uint64_aligned_t gmfn;
uint32_t type;
};
typedef struct xen_domctl_getpageframeinfo xen_domctl_getpageframeinfo_t;
DEFINE_XEN_GUEST_HANDLE(xen_domctl_getpageframeinfo_t);
#define XEN_DOMCTL_getpageframeinfo2  8
struct xen_domctl_getpageframeinfo2 {
uint64_aligned_t num;
XEN_GUEST_HANDLE_64(uint32) array;
};
typedef struct xen_domctl_getpageframeinfo2 xen_domctl_getpageframeinfo2_t;
DEFINE_XEN_GUEST_HANDLE(xen_domctl_getpageframeinfo2_t);
#define XEN_DOMCTL_shadow_op         10
#define XEN_DOMCTL_SHADOW_OP_OFF         0
#define XEN_DOMCTL_SHADOW_OP_ENABLE      32
#define XEN_DOMCTL_SHADOW_OP_CLEAN       11
#define XEN_DOMCTL_SHADOW_OP_PEEK        12
#define XEN_DOMCTL_SHADOW_OP_GET_ALLOCATION   30
#define XEN_DOMCTL_SHADOW_OP_SET_ALLOCATION   31
#define XEN_DOMCTL_SHADOW_OP_ENABLE_TEST       1
#define XEN_DOMCTL_SHADOW_OP_ENABLE_LOGDIRTY   2
#define XEN_DOMCTL_SHADOW_OP_ENABLE_TRANSLATE  3
#define XEN_DOMCTL_SHADOW_ENABLE_REFCOUNT  (1 << 1)
#define XEN_DOMCTL_SHADOW_ENABLE_LOG_DIRTY (1 << 2)
#define XEN_DOMCTL_SHADOW_ENABLE_TRANSLATE (1 << 3)
#define XEN_DOMCTL_SHADOW_ENABLE_EXTERNAL  (1 << 4)
struct xen_domctl_shadow_op_stats {
uint32_t fault_count;
uint32_t dirty_count;
};
typedef struct xen_domctl_shadow_op_stats xen_domctl_shadow_op_stats_t;
DEFINE_XEN_GUEST_HANDLE(xen_domctl_shadow_op_stats_t);
struct xen_domctl_shadow_op {
uint32_t       op;
uint32_t       mode;
uint32_t       mb;
XEN_GUEST_HANDLE_64(uint8) dirty_bitmap;
uint64_aligned_t pages;
struct xen_domctl_shadow_op_stats stats;
};
typedef struct xen_domctl_shadow_op xen_domctl_shadow_op_t;
DEFINE_XEN_GUEST_HANDLE(xen_domctl_shadow_op_t);
#define XEN_DOMCTL_max_mem           11
struct xen_domctl_max_mem {
uint64_aligned_t max_memkb;
};
typedef struct xen_domctl_max_mem xen_domctl_max_mem_t;
DEFINE_XEN_GUEST_HANDLE(xen_domctl_max_mem_t);
#define XEN_DOMCTL_setvcpucontext    12
#define XEN_DOMCTL_getvcpucontext    13
struct xen_domctl_vcpucontext {
uint32_t              vcpu;
XEN_GUEST_HANDLE_64(vcpu_guest_context_t) ctxt;
};
typedef struct xen_domctl_vcpucontext xen_domctl_vcpucontext_t;
DEFINE_XEN_GUEST_HANDLE(xen_domctl_vcpucontext_t);
#define XEN_DOMCTL_getvcpuinfo       14
struct xen_domctl_getvcpuinfo {
uint32_t vcpu;
uint8_t  online;
uint8_t  blocked;
uint8_t  running;
uint64_aligned_t cpu_time;
uint32_t cpu;
};
typedef struct xen_domctl_getvcpuinfo xen_domctl_getvcpuinfo_t;
DEFINE_XEN_GUEST_HANDLE(xen_domctl_getvcpuinfo_t);
#define XEN_DOMCTL_setvcpuaffinity    9
#define XEN_DOMCTL_getvcpuaffinity   25
struct xen_domctl_vcpuaffinity {
uint32_t  vcpu;
struct xenctl_cpumap cpumap;
};
typedef struct xen_domctl_vcpuaffinity xen_domctl_vcpuaffinity_t;
DEFINE_XEN_GUEST_HANDLE(xen_domctl_vcpuaffinity_t);
#define XEN_DOMCTL_max_vcpus         15
struct xen_domctl_max_vcpus {
uint32_t max;
};
typedef struct xen_domctl_max_vcpus xen_domctl_max_vcpus_t;
DEFINE_XEN_GUEST_HANDLE(xen_domctl_max_vcpus_t);
#define XEN_DOMCTL_scheduler_op      16
#define XEN_SCHEDULER_SEDF     4
#define XEN_SCHEDULER_CREDIT   5
#define XEN_DOMCTL_SCHEDOP_putinfo 0
#define XEN_DOMCTL_SCHEDOP_getinfo 1
struct xen_domctl_scheduler_op {
uint32_t sched_id;
uint32_t cmd;
union {
struct xen_domctl_sched_sedf {
uint64_aligned_t period;
uint64_aligned_t slice;
uint64_aligned_t latency;
uint32_t extratime;
uint32_t weight;
} sedf;
struct xen_domctl_sched_credit {
uint16_t weight;
uint16_t cap;
} credit;
} u;
};
typedef struct xen_domctl_scheduler_op xen_domctl_scheduler_op_t;
DEFINE_XEN_GUEST_HANDLE(xen_domctl_scheduler_op_t);
#define XEN_DOMCTL_setdomainhandle   17
struct xen_domctl_setdomainhandle {
xen_domain_handle_t handle;
};
typedef struct xen_domctl_setdomainhandle xen_domctl_setdomainhandle_t;
DEFINE_XEN_GUEST_HANDLE(xen_domctl_setdomainhandle_t);
#define XEN_DOMCTL_setdebugging      18
struct xen_domctl_setdebugging {
uint8_t enable;
};
typedef struct xen_domctl_setdebugging xen_domctl_setdebugging_t;
DEFINE_XEN_GUEST_HANDLE(xen_domctl_setdebugging_t);
#define XEN_DOMCTL_irq_permission    19
struct xen_domctl_irq_permission {
uint8_t pirq;
uint8_t allow_access;
};
typedef struct xen_domctl_irq_permission xen_domctl_irq_permission_t;
DEFINE_XEN_GUEST_HANDLE(xen_domctl_irq_permission_t);
#define XEN_DOMCTL_iomem_permission  20
struct xen_domctl_iomem_permission {
uint64_aligned_t first_mfn;
uint64_aligned_t nr_mfns;
uint8_t  allow_access;
};
typedef struct xen_domctl_iomem_permission xen_domctl_iomem_permission_t;
DEFINE_XEN_GUEST_HANDLE(xen_domctl_iomem_permission_t);
#define XEN_DOMCTL_ioport_permission 21
struct xen_domctl_ioport_permission {
uint32_t first_port;
uint32_t nr_ports;
uint8_t  allow_access;
};
typedef struct xen_domctl_ioport_permission xen_domctl_ioport_permission_t;
DEFINE_XEN_GUEST_HANDLE(xen_domctl_ioport_permission_t);
#define XEN_DOMCTL_hypercall_init    22
struct xen_domctl_hypercall_init {
uint64_aligned_t  gmfn;
};
typedef struct xen_domctl_hypercall_init xen_domctl_hypercall_init_t;
DEFINE_XEN_GUEST_HANDLE(xen_domctl_hypercall_init_t);
#define XEN_DOMCTL_arch_setup        23
#define _XEN_DOMAINSETUP_hvm_guest 0
#define XEN_DOMAINSETUP_hvm_guest  (1UL<<_XEN_DOMAINSETUP_hvm_guest)
#define _XEN_DOMAINSETUP_query 1
#define XEN_DOMAINSETUP_query  (1UL<<_XEN_DOMAINSETUP_query)
#define _XEN_DOMAINSETUP_sioemu_guest 2
#define XEN_DOMAINSETUP_sioemu_guest  (1UL<<_XEN_DOMAINSETUP_sioemu_guest)
typedef struct xen_domctl_arch_setup {
uint64_aligned_t flags;
#ifdef __ia64__
uint64_aligned_t bp;
uint64_aligned_t maxmem;
uint64_aligned_t xsi_va;
uint32_t hypercall_imm;
int8_t vhpt_size_log2;
#endif
} xen_domctl_arch_setup_t;
DEFINE_XEN_GUEST_HANDLE(xen_domctl_arch_setup_t);
#define XEN_DOMCTL_settimeoffset     24
struct xen_domctl_settimeoffset {
int32_t  time_offset_seconds;
};
typedef struct xen_domctl_settimeoffset xen_domctl_settimeoffset_t;
DEFINE_XEN_GUEST_HANDLE(xen_domctl_settimeoffset_t);
#define XEN_DOMCTL_gethvmcontext     33
#define XEN_DOMCTL_sethvmcontext     34
typedef struct xen_domctl_hvmcontext {
uint32_t size;
XEN_GUEST_HANDLE_64(uint8) buffer;
} xen_domctl_hvmcontext_t;
DEFINE_XEN_GUEST_HANDLE(xen_domctl_hvmcontext_t);
#define XEN_DOMCTL_set_address_size  35
#define XEN_DOMCTL_get_address_size  36
typedef struct xen_domctl_address_size {
uint32_t size;
} xen_domctl_address_size_t;
DEFINE_XEN_GUEST_HANDLE(xen_domctl_address_size_t);
#define XEN_DOMCTL_real_mode_area    26
struct xen_domctl_real_mode_area {
uint32_t log;
};
typedef struct xen_domctl_real_mode_area xen_domctl_real_mode_area_t;
DEFINE_XEN_GUEST_HANDLE(xen_domctl_real_mode_area_t);
#define XEN_DOMCTL_sendtrigger       28
#define XEN_DOMCTL_SENDTRIGGER_NMI    0
#define XEN_DOMCTL_SENDTRIGGER_RESET  1
#define XEN_DOMCTL_SENDTRIGGER_INIT   2
struct xen_domctl_sendtrigger {
uint32_t  trigger;
uint32_t  vcpu;
};
typedef struct xen_domctl_sendtrigger xen_domctl_sendtrigger_t;
DEFINE_XEN_GUEST_HANDLE(xen_domctl_sendtrigger_t);
#define XEN_DOMCTL_assign_device      37
#define XEN_DOMCTL_test_assign_device 45
#define XEN_DOMCTL_deassign_device 47
struct xen_domctl_assign_device {
uint32_t  machine_bdf;
};
typedef struct xen_domctl_assign_device xen_domctl_assign_device_t;
DEFINE_XEN_GUEST_HANDLE(xen_domctl_assign_device_t);
#define XEN_DOMCTL_get_device_group 50
struct xen_domctl_get_device_group {
uint32_t  machine_bdf;
uint32_t  max_sdevs;
uint32_t  num_sdevs;
XEN_GUEST_HANDLE_64(uint32)  sdev_array;
};
typedef struct xen_domctl_get_device_group xen_domctl_get_device_group_t;
DEFINE_XEN_GUEST_HANDLE(xen_domctl_get_device_group_t);
#define XEN_DOMCTL_bind_pt_irq       38
#define XEN_DOMCTL_unbind_pt_irq     48
typedef enum pt_irq_type_e {
PT_IRQ_TYPE_PCI,
PT_IRQ_TYPE_ISA,
PT_IRQ_TYPE_MSI,
} pt_irq_type_t;
struct xen_domctl_bind_pt_irq {
uint32_t machine_irq;
pt_irq_type_t irq_type;
uint32_t hvm_domid;
union {
struct {
uint8_t isa_irq;
} isa;
struct {
uint8_t bus;
uint8_t device;
uint8_t intx;
} pci;
struct {
uint8_t gvec;
uint32_t gflags;
} msi;
} u;
};
typedef struct xen_domctl_bind_pt_irq xen_domctl_bind_pt_irq_t;
DEFINE_XEN_GUEST_HANDLE(xen_domctl_bind_pt_irq_t);
#define XEN_DOMCTL_memory_mapping    39
#define DPCI_ADD_MAPPING         1
#define DPCI_REMOVE_MAPPING      0
struct xen_domctl_memory_mapping {
uint64_aligned_t first_gfn;
uint64_aligned_t first_mfn;
uint64_aligned_t nr_mfns;
uint32_t add_mapping;
uint32_t padding;
};
typedef struct xen_domctl_memory_mapping xen_domctl_memory_mapping_t;
DEFINE_XEN_GUEST_HANDLE(xen_domctl_memory_mapping_t);
#define XEN_DOMCTL_ioport_mapping    40
struct xen_domctl_ioport_mapping {
uint32_t first_gport;
uint32_t first_mport;
uint32_t nr_ports;
uint32_t add_mapping;
};
typedef struct xen_domctl_ioport_mapping xen_domctl_ioport_mapping_t;
DEFINE_XEN_GUEST_HANDLE(xen_domctl_ioport_mapping_t);
#define XEN_DOMCTL_pin_mem_cacheattr 41
#define XEN_DOMCTL_MEM_CACHEATTR_UC  0
#define XEN_DOMCTL_MEM_CACHEATTR_WC  1
#define XEN_DOMCTL_MEM_CACHEATTR_WT  4
#define XEN_DOMCTL_MEM_CACHEATTR_WP  5
#define XEN_DOMCTL_MEM_CACHEATTR_WB  6
#define XEN_DOMCTL_MEM_CACHEATTR_UCM 7
struct xen_domctl_pin_mem_cacheattr {
uint64_aligned_t start, end;
unsigned int type;
};
typedef struct xen_domctl_pin_mem_cacheattr xen_domctl_pin_mem_cacheattr_t;
DEFINE_XEN_GUEST_HANDLE(xen_domctl_pin_mem_cacheattr_t);
#define XEN_DOMCTL_set_ext_vcpucontext 42
#define XEN_DOMCTL_get_ext_vcpucontext 43
struct xen_domctl_ext_vcpucontext {
uint32_t         vcpu;
uint32_t         size;
#if defined(__i386__) || defined(__x86_64__)
uint64_aligned_t syscall32_callback_eip;
uint64_aligned_t sysenter_callback_eip;
uint16_t         syscall32_callback_cs;
uint16_t         sysenter_callback_cs;
uint8_t          syscall32_disables_events;
uint8_t          sysenter_disables_events;
#endif
};
typedef struct xen_domctl_ext_vcpucontext xen_domctl_ext_vcpucontext_t;
DEFINE_XEN_GUEST_HANDLE(xen_domctl_ext_vcpucontext_t);
#define XEN_DOMCTL_set_opt_feature    44
struct xen_domctl_set_opt_feature {
#if defined(__ia64__)
struct xen_ia64_opt_feature optf;
#else
uint64_t dummy;
#endif
};
typedef struct xen_domctl_set_opt_feature xen_domctl_set_opt_feature_t;
DEFINE_XEN_GUEST_HANDLE(xen_domctl_set_opt_feature_t);
#define XEN_DOMCTL_set_target    46
struct xen_domctl_set_target {
domid_t target;
};
typedef struct xen_domctl_set_target xen_domctl_set_target_t;
DEFINE_XEN_GUEST_HANDLE(xen_domctl_set_target_t);
#if defined(__i386__) || defined(__x86_64__)
# define XEN_CPUID_INPUT_UNUSED  0xFFFFFFFF
# define XEN_DOMCTL_set_cpuid 49
struct xen_domctl_cpuid {
unsigned int  input[2];
unsigned int  eax;
unsigned int  ebx;
unsigned int  ecx;
unsigned int  edx;
};
typedef struct xen_domctl_cpuid xen_domctl_cpuid_t;
DEFINE_XEN_GUEST_HANDLE(xen_domctl_cpuid_t);
#endif
#define XEN_DOMCTL_subscribe          29
struct xen_domctl_subscribe {
uint32_t port;
};
typedef struct xen_domctl_subscribe xen_domctl_subscribe_t;
DEFINE_XEN_GUEST_HANDLE(xen_domctl_subscribe_t);
#define XEN_DOMCTL_set_machine_address_size  51
#define XEN_DOMCTL_get_machine_address_size  52
#define XEN_DOMCTL_suppress_spurious_page_faults 53
struct xen_domctl {
uint32_t cmd;
uint32_t interface_version;
domid_t  domain;
union {
struct xen_domctl_createdomain      createdomain;
struct xen_domctl_getdomaininfo     getdomaininfo;
struct xen_domctl_getmemlist        getmemlist;
struct xen_domctl_getpageframeinfo  getpageframeinfo;
struct xen_domctl_getpageframeinfo2 getpageframeinfo2;
struct xen_domctl_vcpuaffinity      vcpuaffinity;
struct xen_domctl_shadow_op         shadow_op;
struct xen_domctl_max_mem           max_mem;
struct xen_domctl_vcpucontext       vcpucontext;
struct xen_domctl_getvcpuinfo       getvcpuinfo;
struct xen_domctl_max_vcpus         max_vcpus;
struct xen_domctl_scheduler_op      scheduler_op;
struct xen_domctl_setdomainhandle   setdomainhandle;
struct xen_domctl_setdebugging      setdebugging;
struct xen_domctl_irq_permission    irq_permission;
struct xen_domctl_iomem_permission  iomem_permission;
struct xen_domctl_ioport_permission ioport_permission;
struct xen_domctl_hypercall_init    hypercall_init;
struct xen_domctl_arch_setup        arch_setup;
struct xen_domctl_settimeoffset     settimeoffset;
struct xen_domctl_real_mode_area    real_mode_area;
struct xen_domctl_hvmcontext        hvmcontext;
struct xen_domctl_address_size      address_size;
struct xen_domctl_sendtrigger       sendtrigger;
struct xen_domctl_get_device_group  get_device_group;
struct xen_domctl_assign_device     assign_device;
struct xen_domctl_bind_pt_irq       bind_pt_irq;
struct xen_domctl_memory_mapping    memory_mapping;
struct xen_domctl_ioport_mapping    ioport_mapping;
struct xen_domctl_pin_mem_cacheattr pin_mem_cacheattr;
struct xen_domctl_ext_vcpucontext   ext_vcpucontext;
struct xen_domctl_set_opt_feature   set_opt_feature;
struct xen_domctl_set_target        set_target;
struct xen_domctl_subscribe         subscribe;
#if defined(__i386__) || defined(__x86_64__)
struct xen_domctl_cpuid             cpuid;
#endif
uint8_t                             pad[128];
} u;
};
typedef struct xen_domctl xen_domctl_t;
DEFINE_XEN_GUEST_HANDLE(xen_domctl_t);
#endif