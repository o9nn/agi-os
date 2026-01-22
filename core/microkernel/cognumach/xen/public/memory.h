#ifndef __XEN_PUBLIC_MEMORY_H__
#define __XEN_PUBLIC_MEMORY_H__
#define XENMEM_increase_reservation 0
#define XENMEM_decrease_reservation 1
#define XENMEM_populate_physmap     6
#if __XEN_INTERFACE_VERSION__ >= 0x00030209
#define XENMEMF_address_bits(x)     (x)
#define XENMEMF_get_address_bits(x) ((x) & 0xffu)
#define XENMEMF_node(x)     (((x) + 1) << 8)
#define XENMEMF_get_node(x) ((((x) >> 8) - 1) & 0xffu)
#endif
struct xen_memory_reservation {
XEN_GUEST_HANDLE(xen_pfn_t) extent_start;
xen_ulong_t    nr_extents;
unsigned int   extent_order;
#if __XEN_INTERFACE_VERSION__ >= 0x00030209
unsigned int   mem_flags;
#else
unsigned int   address_bits;
#endif
domid_t        domid;
};
typedef struct xen_memory_reservation xen_memory_reservation_t;
DEFINE_XEN_GUEST_HANDLE(xen_memory_reservation_t);
#define XENMEM_exchange             11
struct xen_memory_exchange {
struct xen_memory_reservation in;
struct xen_memory_reservation out;
xen_ulong_t nr_exchanged;
};
typedef struct xen_memory_exchange xen_memory_exchange_t;
DEFINE_XEN_GUEST_HANDLE(xen_memory_exchange_t);
#define XENMEM_maximum_ram_page     2
#define XENMEM_current_reservation  3
#define XENMEM_maximum_reservation  4
#define XENMEM_maximum_gpfn         14
#define XENMEM_machphys_mfn_list    5
struct xen_machphys_mfn_list {
unsigned int max_extents;
XEN_GUEST_HANDLE(xen_pfn_t) extent_start;
unsigned int nr_extents;
};
typedef struct xen_machphys_mfn_list xen_machphys_mfn_list_t;
DEFINE_XEN_GUEST_HANDLE(xen_machphys_mfn_list_t);
#define XENMEM_machphys_mapping     12
struct xen_machphys_mapping {
xen_ulong_t v_start, v_end;
xen_ulong_t max_mfn;
};
typedef struct xen_machphys_mapping xen_machphys_mapping_t;
DEFINE_XEN_GUEST_HANDLE(xen_machphys_mapping_t);
#define XENMEM_add_to_physmap      7
struct xen_add_to_physmap {
domid_t domid;
#define XENMAPSPACE_shared_info 0
#define XENMAPSPACE_grant_table 1
#define XENMAPSPACE_mfn         2
unsigned int space;
xen_ulong_t idx;
xen_pfn_t     gpfn;
};
typedef struct xen_add_to_physmap xen_add_to_physmap_t;
DEFINE_XEN_GUEST_HANDLE(xen_add_to_physmap_t);
#define XENMEM_remove_from_physmap      15
struct xen_remove_from_physmap {
domid_t domid;
xen_pfn_t     gpfn;
};
typedef struct xen_remove_from_physmap xen_remove_from_physmap_t;
DEFINE_XEN_GUEST_HANDLE(xen_remove_from_physmap_t);
#define XENMEM_translate_gpfn_list  8
struct xen_translate_gpfn_list {
domid_t domid;
xen_ulong_t nr_gpfns;
XEN_GUEST_HANDLE(xen_pfn_t) gpfn_list;
XEN_GUEST_HANDLE(xen_pfn_t) mfn_list;
};
typedef struct xen_translate_gpfn_list xen_translate_gpfn_list_t;
DEFINE_XEN_GUEST_HANDLE(xen_translate_gpfn_list_t);
#define XENMEM_memory_map           9
struct xen_memory_map {
unsigned int nr_entries;
XEN_GUEST_HANDLE(void) buffer;
};
typedef struct xen_memory_map xen_memory_map_t;
DEFINE_XEN_GUEST_HANDLE(xen_memory_map_t);
#define XENMEM_machine_memory_map   10
#define XENMEM_set_memory_map       13
struct xen_foreign_memory_map {
domid_t domid;
struct xen_memory_map map;
};
typedef struct xen_foreign_memory_map xen_foreign_memory_map_t;
DEFINE_XEN_GUEST_HANDLE(xen_foreign_memory_map_t);
#endif