#ifndef __XEN_PUBLIC_ARCH_X86_MCA_H__
#define __XEN_PUBLIC_ARCH_X86_MCA_H__
#define __HYPERVISOR_mca __HYPERVISOR_arch_0
#define XEN_MCA_INTERFACE_VERSION 0x03000001
#define XEN_MC_CORRECTABLE 0x0
#define XEN_MC_TRAP 0x1
#define XEN_MC_OK 0x0
#define XEN_MC_FETCHFAILED 0x1
#define XEN_MC_NODATA 0x2
#define XEN_MC_NOMATCH 0x4
#define XEN_MC_CANNOTHANDLE 0x8
#define XEN_MC_NOTDELIVERED 0x10
#ifndef __ASSEMBLY__
#define VIRQ_MCA VIRQ_ARCH_0
#define MC_TYPE_GLOBAL 0
#define MC_TYPE_BANK 1
#define MC_TYPE_EXTENDED 2
struct mcinfo_common {
uint16_t type;
uint16_t size;
};
#define MC_FLAG_CORRECTABLE (1 << 0)
#define MC_FLAG_UNCORRECTABLE (1 << 1)
struct mcinfo_global {
struct mcinfo_common common;
uint16_t mc_domid;
uint32_t mc_socketid;
uint16_t mc_coreid;
uint16_t mc_core_threadid;
uint16_t mc_vcpuid;
uint64_t mc_gstatus;
uint32_t mc_flags;
};
struct mcinfo_bank {
struct mcinfo_common common;
uint16_t mc_bank;
uint16_t mc_domid;
uint64_t mc_status;
uint64_t mc_addr;
uint64_t mc_misc;
};
struct mcinfo_msr {
uint64_t reg;
uint64_t value;
};
struct mcinfo_extended {
struct mcinfo_common common;
uint32_t mc_msrs;
struct mcinfo_msr mc_msr[5];
};
#define MCINFO_HYPERCALLSIZE 1024
#define MCINFO_MAXSIZE 768
struct mc_info {
uint32_t mi_nentries;
uint8_t mi_data[MCINFO_MAXSIZE - sizeof(uint32_t)];
};
typedef struct mc_info mc_info_t;
#define x86_mcinfo_nentries(_mi) \
(_mi)->mi_nentries
#define x86_mcinfo_first(_mi) \
(struct mcinfo_common *)((_mi)->mi_data)
#define x86_mcinfo_next(_mic) \
(struct mcinfo_common *)((uint8_t *)(_mic) + (_mic)->size)
#define x86_mcinfo_lookup(_ret, _mi, _type) \
do { \
uint32_t found, i; \
struct mcinfo_common *_mic; \
\
found = 0; \
(_ret) = NULL; \
if (_mi == NULL) break; \
_mic = x86_mcinfo_first(_mi); \
for (i = 0; i < x86_mcinfo_nentries(_mi); i++) { \
if (_mic->type == (_type)) { \
found = 1; \
break; \
} \
_mic = x86_mcinfo_next(_mic); \
} \
(_ret) = found ? _mic : NULL; \
} while (0)
#define XEN_MC_fetch 1
struct xen_mc_fetch {
uint32_t flags;
uint32_t fetch_idx;
struct mc_info mc_info;
};
typedef struct xen_mc_fetch xen_mc_fetch_t;
DEFINE_XEN_GUEST_HANDLE(xen_mc_fetch_t);
#define XEN_MC_notifydomain 2
struct xen_mc_notifydomain {
uint16_t mc_domid;
uint16_t mc_vcpuid;
uint32_t fetch_idx;
uint32_t flags;
};
typedef struct xen_mc_notifydomain xen_mc_notifydomain_t;
DEFINE_XEN_GUEST_HANDLE(xen_mc_notifydomain_t);
struct xen_mc {
uint32_t cmd;
uint32_t interface_version;
union {
struct xen_mc_fetch mc_fetch;
struct xen_mc_notifydomain mc_notifydomain;
uint8_t pad[MCINFO_HYPERCALLSIZE];
} u;
};
typedef struct xen_mc xen_mc_t;
DEFINE_XEN_GUEST_HANDLE(xen_mc_t);
#endif
#endif