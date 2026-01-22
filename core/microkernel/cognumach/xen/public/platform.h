#ifndef __XEN_PUBLIC_PLATFORM_H__
#define __XEN_PUBLIC_PLATFORM_H__
#include "xen.h"
#define XENPF_INTERFACE_VERSION 0x03000001
#define XENPF_settime 17
struct xenpf_settime {
uint32_t secs;
uint32_t nsecs;
uint64_t system_time;
};
typedef struct xenpf_settime xenpf_settime_t;
DEFINE_XEN_GUEST_HANDLE(xenpf_settime_t);
#define XENPF_add_memtype 31
struct xenpf_add_memtype {
xen_pfn_t mfn;
uint64_t nr_mfns;
uint32_t type;
uint32_t handle;
uint32_t reg;
};
typedef struct xenpf_add_memtype xenpf_add_memtype_t;
DEFINE_XEN_GUEST_HANDLE(xenpf_add_memtype_t);
#define XENPF_del_memtype 32
struct xenpf_del_memtype {
uint32_t handle;
uint32_t reg;
};
typedef struct xenpf_del_memtype xenpf_del_memtype_t;
DEFINE_XEN_GUEST_HANDLE(xenpf_del_memtype_t);
#define XENPF_read_memtype 33
struct xenpf_read_memtype {
uint32_t reg;
xen_pfn_t mfn;
uint64_t nr_mfns;
uint32_t type;
};
typedef struct xenpf_read_memtype xenpf_read_memtype_t;
DEFINE_XEN_GUEST_HANDLE(xenpf_read_memtype_t);
#define XENPF_microcode_update 35
struct xenpf_microcode_update {
XEN_GUEST_HANDLE(const_void) data;
uint32_t length;
};
typedef struct xenpf_microcode_update xenpf_microcode_update_t;
DEFINE_XEN_GUEST_HANDLE(xenpf_microcode_update_t);
#define XENPF_platform_quirk 39
#define QUIRK_NOIRQBALANCING 1
#define QUIRK_IOAPIC_BAD_REGSEL 2
#define QUIRK_IOAPIC_GOOD_REGSEL 3
struct xenpf_platform_quirk {
uint32_t quirk_id;
};
typedef struct xenpf_platform_quirk xenpf_platform_quirk_t;
DEFINE_XEN_GUEST_HANDLE(xenpf_platform_quirk_t);
#define XENPF_firmware_info 50
#define XEN_FW_DISK_INFO 1
#define XEN_FW_DISK_MBR_SIGNATURE 2
#define XEN_FW_VBEDDC_INFO 3
struct xenpf_firmware_info {
uint32_t type;
uint32_t index;
union {
struct {
uint8_t device;
uint8_t version;
uint16_t interface_support;
uint16_t legacy_max_cylinder;
uint8_t legacy_max_head;
uint8_t legacy_sectors_per_track;
XEN_GUEST_HANDLE(void) edd_params;
} disk_info;
struct {
uint8_t device;
uint32_t mbr_signature;
} disk_mbr_signature;
struct {
uint8_t capabilities;
uint8_t edid_transfer_time;
XEN_GUEST_HANDLE(uint8) edid;
} vbeddc_info;
} u;
};
typedef struct xenpf_firmware_info xenpf_firmware_info_t;
DEFINE_XEN_GUEST_HANDLE(xenpf_firmware_info_t);
#define XENPF_enter_acpi_sleep 51
struct xenpf_enter_acpi_sleep {
uint16_t pm1a_cnt_val;
uint16_t pm1b_cnt_val;
uint32_t sleep_state;
uint32_t flags;
};
typedef struct xenpf_enter_acpi_sleep xenpf_enter_acpi_sleep_t;
DEFINE_XEN_GUEST_HANDLE(xenpf_enter_acpi_sleep_t);
#define XENPF_change_freq 52
struct xenpf_change_freq {
uint32_t flags;
uint32_t cpu;
uint64_t freq;
};
typedef struct xenpf_change_freq xenpf_change_freq_t;
DEFINE_XEN_GUEST_HANDLE(xenpf_change_freq_t);
#define XENPF_getidletime 53
struct xenpf_getidletime {
XEN_GUEST_HANDLE(uint8) cpumap_bitmap;
uint32_t cpumap_nr_cpus;
XEN_GUEST_HANDLE(uint64) idletime;
uint64_t now;
};
typedef struct xenpf_getidletime xenpf_getidletime_t;
DEFINE_XEN_GUEST_HANDLE(xenpf_getidletime_t);
#define XENPF_set_processor_pminfo 54
#define XEN_PROCESSOR_PM_CX 1
#define XEN_PROCESSOR_PM_PX 2
#define XEN_PROCESSOR_PM_TX 4
#define XEN_PM_CX 0
#define XEN_PM_PX 1
#define XEN_PM_TX 2
#define XEN_PX_PCT 1
#define XEN_PX_PSS 2
#define XEN_PX_PPC 4
#define XEN_PX_PSD 8
struct xen_power_register {
uint32_t space_id;
uint32_t bit_width;
uint32_t bit_offset;
uint32_t access_size;
uint64_t address;
};
struct xen_processor_csd {
uint32_t domain;
uint32_t coord_type;
uint32_t num;
};
typedef struct xen_processor_csd xen_processor_csd_t;
DEFINE_XEN_GUEST_HANDLE(xen_processor_csd_t);
struct xen_processor_cx {
struct xen_power_register reg;
uint8_t type;
uint32_t latency;
uint32_t power;
uint32_t dpcnt;
XEN_GUEST_HANDLE(xen_processor_csd_t) dp;
};
typedef struct xen_processor_cx xen_processor_cx_t;
DEFINE_XEN_GUEST_HANDLE(xen_processor_cx_t);
struct xen_processor_flags {
uint32_t bm_control:1;
uint32_t bm_check:1;
uint32_t has_cst:1;
uint32_t power_setup_done:1;
uint32_t bm_rld_set:1;
};
struct xen_processor_power {
uint32_t count;
struct xen_processor_flags flags;
XEN_GUEST_HANDLE(xen_processor_cx_t) states;
};
struct xen_pct_register {
uint8_t descriptor;
uint16_t length;
uint8_t space_id;
uint8_t bit_width;
uint8_t bit_offset;
uint8_t reserved;
uint64_t address;
};
struct xen_processor_px {
uint64_t core_frequency;
uint64_t power;
uint64_t transition_latency;
uint64_t bus_master_latency;
uint64_t control;
uint64_t status;
};
typedef struct xen_processor_px xen_processor_px_t;
DEFINE_XEN_GUEST_HANDLE(xen_processor_px_t);
struct xen_psd_package {
uint64_t num_entries;
uint64_t revision;
uint64_t domain;
uint64_t coord_type;
uint64_t num_processors;
};
struct xen_processor_performance {
uint32_t flags;
uint32_t platform_limit;
struct xen_pct_register control_register;
struct xen_pct_register status_register;
uint32_t state_count;
XEN_GUEST_HANDLE(xen_processor_px_t) states;
struct xen_psd_package domain_info;
uint32_t shared_type;
};
typedef struct xen_processor_performance xen_processor_performance_t;
DEFINE_XEN_GUEST_HANDLE(xen_processor_performance_t);
struct xenpf_set_processor_pminfo {
uint32_t id;
uint32_t type;
union {
struct xen_processor_power power;
struct xen_processor_performance perf;
};
};
typedef struct xenpf_set_processor_pminfo xenpf_set_processor_pminfo_t;
DEFINE_XEN_GUEST_HANDLE(xenpf_set_processor_pminfo_t);
struct xen_platform_op {
uint32_t cmd;
uint32_t interface_version;
union {
struct xenpf_settime settime;
struct xenpf_add_memtype add_memtype;
struct xenpf_del_memtype del_memtype;
struct xenpf_read_memtype read_memtype;
struct xenpf_microcode_update microcode;
struct xenpf_platform_quirk platform_quirk;
struct xenpf_firmware_info firmware_info;
struct xenpf_enter_acpi_sleep enter_acpi_sleep;
struct xenpf_change_freq change_freq;
struct xenpf_getidletime getidletime;
struct xenpf_set_processor_pminfo set_pminfo;
uint8_t pad[128];
} u;
};
typedef struct xen_platform_op xen_platform_op_t;
DEFINE_XEN_GUEST_HANDLE(xen_platform_op_t);
#endif