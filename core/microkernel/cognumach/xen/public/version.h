#ifndef __XEN_PUBLIC_VERSION_H__
#define __XEN_PUBLIC_VERSION_H__
#define XENVER_version      0
#define XENVER_extraversion 1
typedef char xen_extraversion_t[16];
#define XEN_EXTRAVERSION_LEN (sizeof(xen_extraversion_t))
#define XENVER_compile_info 2
struct xen_compile_info {
char compiler[64];
char compile_by[16];
char compile_domain[32];
char compile_date[32];
};
typedef struct xen_compile_info xen_compile_info_t;
#define XENVER_capabilities 3
typedef char xen_capabilities_info_t[1024];
#define XEN_CAPABILITIES_INFO_LEN (sizeof(xen_capabilities_info_t))
#define XENVER_changeset 4
typedef char xen_changeset_info_t[64];
#define XEN_CHANGESET_INFO_LEN (sizeof(xen_changeset_info_t))
#define XENVER_platform_parameters 5
struct xen_platform_parameters {
unsigned long virt_start;
};
typedef struct xen_platform_parameters xen_platform_parameters_t;
#define XENVER_get_features 6
struct xen_feature_info {
unsigned int submap_idx;
uint32_t     submap;
};
typedef struct xen_feature_info xen_feature_info_t;
#include "features.h"
#define XENVER_pagesize 7
#define XENVER_guest_handle 8
#endif