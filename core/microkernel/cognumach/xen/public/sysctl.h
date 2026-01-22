#ifndef __XEN_PUBLIC_SYSCTL_H__
#define __XEN_PUBLIC_SYSCTL_H__
#if !defined(__XEN__) && !defined(__XEN_TOOLS__)
#error "sysctl operations are intended for use by node control tools only"
#endif
#include "xen.h"
#include "domctl.h"
#define XEN_SYSCTL_INTERFACE_VERSION 0x00000006
#define XEN_SYSCTL_readconsole       1
struct xen_sysctl_readconsole {
uint8_t clear;
uint8_t incremental;
uint8_t pad0, pad1;
uint32_t index;
XEN_GUEST_HANDLE_64(char) buffer;
uint32_t count;
};
typedef struct xen_sysctl_readconsole xen_sysctl_readconsole_t;
DEFINE_XEN_GUEST_HANDLE(xen_sysctl_readconsole_t);
#define XEN_SYSCTL_tbuf_op           2
struct xen_sysctl_tbuf_op {
#define XEN_SYSCTL_TBUFOP_get_info     0
#define XEN_SYSCTL_TBUFOP_set_cpu_mask 1
#define XEN_SYSCTL_TBUFOP_set_evt_mask 2
#define XEN_SYSCTL_TBUFOP_set_size     3
#define XEN_SYSCTL_TBUFOP_enable       4
#define XEN_SYSCTL_TBUFOP_disable      5
uint32_t cmd;
struct xenctl_cpumap cpu_mask;
uint32_t             evt_mask;
uint64_aligned_t buffer_mfn;
uint32_t size;
};
typedef struct xen_sysctl_tbuf_op xen_sysctl_tbuf_op_t;
DEFINE_XEN_GUEST_HANDLE(xen_sysctl_tbuf_op_t);
#define XEN_SYSCTL_physinfo          3
#define _XEN_SYSCTL_PHYSCAP_hvm          0
#define XEN_SYSCTL_PHYSCAP_hvm           (1u<<_XEN_SYSCTL_PHYSCAP_hvm)
#define _XEN_SYSCTL_PHYSCAP_hvm_directio 1
#define XEN_SYSCTL_PHYSCAP_hvm_directio  (1u<<_XEN_SYSCTL_PHYSCAP_hvm_directio)
struct xen_sysctl_physinfo {
uint32_t threads_per_core;
uint32_t cores_per_socket;
uint32_t nr_cpus;
uint32_t nr_nodes;
uint32_t cpu_khz;
uint64_aligned_t total_pages;
uint64_aligned_t free_pages;
uint64_aligned_t scrub_pages;
uint32_t hw_cap[8];
uint32_t max_cpu_id;
XEN_GUEST_HANDLE_64(uint32) cpu_to_node;
uint32_t capabilities;
};
typedef struct xen_sysctl_physinfo xen_sysctl_physinfo_t;
DEFINE_XEN_GUEST_HANDLE(xen_sysctl_physinfo_t);
#define XEN_SYSCTL_sched_id          4
struct xen_sysctl_sched_id {
uint32_t sched_id;
};
typedef struct xen_sysctl_sched_id xen_sysctl_sched_id_t;
DEFINE_XEN_GUEST_HANDLE(xen_sysctl_sched_id_t);
#define XEN_SYSCTL_perfc_op          5
#define XEN_SYSCTL_PERFCOP_reset 1
#define XEN_SYSCTL_PERFCOP_query 2
struct xen_sysctl_perfc_desc {
char         name[80];
uint32_t     nr_vals;
};
typedef struct xen_sysctl_perfc_desc xen_sysctl_perfc_desc_t;
DEFINE_XEN_GUEST_HANDLE(xen_sysctl_perfc_desc_t);
typedef uint32_t xen_sysctl_perfc_val_t;
DEFINE_XEN_GUEST_HANDLE(xen_sysctl_perfc_val_t);
struct xen_sysctl_perfc_op {
uint32_t       cmd;
uint32_t       nr_counters;
uint32_t       nr_vals;
XEN_GUEST_HANDLE_64(xen_sysctl_perfc_desc_t) desc;
XEN_GUEST_HANDLE_64(xen_sysctl_perfc_val_t) val;
};
typedef struct xen_sysctl_perfc_op xen_sysctl_perfc_op_t;
DEFINE_XEN_GUEST_HANDLE(xen_sysctl_perfc_op_t);
#define XEN_SYSCTL_getdomaininfolist 6
struct xen_sysctl_getdomaininfolist {
domid_t               first_domain;
uint32_t              max_domains;
XEN_GUEST_HANDLE_64(xen_domctl_getdomaininfo_t) buffer;
uint32_t              num_domains;
};
typedef struct xen_sysctl_getdomaininfolist xen_sysctl_getdomaininfolist_t;
DEFINE_XEN_GUEST_HANDLE(xen_sysctl_getdomaininfolist_t);
#define XEN_SYSCTL_debug_keys        7
struct xen_sysctl_debug_keys {
XEN_GUEST_HANDLE_64(char) keys;
uint32_t nr_keys;
};
typedef struct xen_sysctl_debug_keys xen_sysctl_debug_keys_t;
DEFINE_XEN_GUEST_HANDLE(xen_sysctl_debug_keys_t);
#define XEN_SYSCTL_getcpuinfo        8
struct xen_sysctl_cpuinfo {
uint64_aligned_t idletime;
};
typedef struct xen_sysctl_cpuinfo xen_sysctl_cpuinfo_t;
DEFINE_XEN_GUEST_HANDLE(xen_sysctl_cpuinfo_t);
struct xen_sysctl_getcpuinfo {
uint32_t max_cpus;
XEN_GUEST_HANDLE_64(xen_sysctl_cpuinfo_t) info;
uint32_t nr_cpus;
};
typedef struct xen_sysctl_getcpuinfo xen_sysctl_getcpuinfo_t;
DEFINE_XEN_GUEST_HANDLE(xen_sysctl_getcpuinfo_t);
#define XEN_SYSCTL_availheap         9
struct xen_sysctl_availheap {
uint32_t min_bitwidth;
uint32_t max_bitwidth;
int32_t  node;
uint64_aligned_t avail_bytes;
};
typedef struct xen_sysctl_availheap xen_sysctl_availheap_t;
DEFINE_XEN_GUEST_HANDLE(xen_sysctl_availheap_t);
#define XEN_SYSCTL_get_pmstat        10
struct pm_px_val {
uint64_aligned_t freq;
uint64_aligned_t residency;
uint64_aligned_t count;
};
typedef struct pm_px_val pm_px_val_t;
DEFINE_XEN_GUEST_HANDLE(pm_px_val_t);
struct pm_px_stat {
uint8_t total;
uint8_t usable;
uint8_t last;
uint8_t cur;
XEN_GUEST_HANDLE_64(uint64) trans_pt;
XEN_GUEST_HANDLE_64(pm_px_val_t) pt;
};
typedef struct pm_px_stat pm_px_stat_t;
DEFINE_XEN_GUEST_HANDLE(pm_px_stat_t);
struct pm_cx_stat {
uint32_t nr;
uint32_t last;
uint64_aligned_t idle_time;
XEN_GUEST_HANDLE_64(uint64) triggers;
XEN_GUEST_HANDLE_64(uint64) residencies;
};
struct xen_sysctl_get_pmstat {
#define PMSTAT_CATEGORY_MASK 0xf0
#define PMSTAT_PX            0x10
#define PMSTAT_CX            0x20
#define PMSTAT_get_max_px    (PMSTAT_PX | 0x1)
#define PMSTAT_get_pxstat    (PMSTAT_PX | 0x2)
#define PMSTAT_reset_pxstat  (PMSTAT_PX | 0x3)
#define PMSTAT_get_max_cx    (PMSTAT_CX | 0x1)
#define PMSTAT_get_cxstat    (PMSTAT_CX | 0x2)
#define PMSTAT_reset_cxstat  (PMSTAT_CX | 0x3)
uint32_t type;
uint32_t cpuid;
union {
struct pm_px_stat getpx;
struct pm_cx_stat getcx;
} u;
};
typedef struct xen_sysctl_get_pmstat xen_sysctl_get_pmstat_t;
DEFINE_XEN_GUEST_HANDLE(xen_sysctl_get_pmstat_t);
#define XEN_SYSCTL_cpu_hotplug       11
struct xen_sysctl_cpu_hotplug {
uint32_t cpu;
#define XEN_SYSCTL_CPU_HOTPLUG_ONLINE  0
#define XEN_SYSCTL_CPU_HOTPLUG_OFFLINE 1
uint32_t op;
};
typedef struct xen_sysctl_cpu_hotplug xen_sysctl_cpu_hotplug_t;
DEFINE_XEN_GUEST_HANDLE(xen_sysctl_cpu_hotplug_t);
struct xen_sysctl {
uint32_t cmd;
uint32_t interface_version;
union {
struct xen_sysctl_readconsole       readconsole;
struct xen_sysctl_tbuf_op           tbuf_op;
struct xen_sysctl_physinfo          physinfo;
struct xen_sysctl_sched_id          sched_id;
struct xen_sysctl_perfc_op          perfc_op;
struct xen_sysctl_getdomaininfolist getdomaininfolist;
struct xen_sysctl_debug_keys        debug_keys;
struct xen_sysctl_getcpuinfo        getcpuinfo;
struct xen_sysctl_availheap         availheap;
struct xen_sysctl_get_pmstat        get_pmstat;
struct xen_sysctl_cpu_hotplug       cpu_hotplug;
uint8_t                             pad[128];
} u;
};
typedef struct xen_sysctl xen_sysctl_t;
DEFINE_XEN_GUEST_HANDLE(xen_sysctl_t);
#endif