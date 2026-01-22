#ifndef __XEN_PUBLIC_PHYSDEV_H__
#define __XEN_PUBLIC_PHYSDEV_H__
#define PHYSDEVOP_eoi                   12
struct physdev_eoi {
uint32_t irq;
};
typedef struct physdev_eoi physdev_eoi_t;
DEFINE_XEN_GUEST_HANDLE(physdev_eoi_t);
#define PHYSDEVOP_irq_status_query       5
struct physdev_irq_status_query {
uint32_t irq;
uint32_t flags;
};
typedef struct physdev_irq_status_query physdev_irq_status_query_t;
DEFINE_XEN_GUEST_HANDLE(physdev_irq_status_query_t);
#define _XENIRQSTAT_needs_eoi   (0)
#define  XENIRQSTAT_needs_eoi   (1U<<_XENIRQSTAT_needs_eoi)
#define _XENIRQSTAT_shared      (1)
#define  XENIRQSTAT_shared      (1U<<_XENIRQSTAT_shared)
#define PHYSDEVOP_set_iopl               6
struct physdev_set_iopl {
uint32_t iopl;
};
typedef struct physdev_set_iopl physdev_set_iopl_t;
DEFINE_XEN_GUEST_HANDLE(physdev_set_iopl_t);
#define PHYSDEVOP_set_iobitmap           7
struct physdev_set_iobitmap {
#if __XEN_INTERFACE_VERSION__ >= 0x00030205
XEN_GUEST_HANDLE(uint8) bitmap;
#else
uint8_t *bitmap;
#endif
uint32_t nr_ports;
};
typedef struct physdev_set_iobitmap physdev_set_iobitmap_t;
DEFINE_XEN_GUEST_HANDLE(physdev_set_iobitmap_t);
#define PHYSDEVOP_apic_read              8
#define PHYSDEVOP_apic_write             9
struct physdev_apic {
unsigned long apic_physbase;
uint32_t reg;
uint32_t value;
};
typedef struct physdev_apic physdev_apic_t;
DEFINE_XEN_GUEST_HANDLE(physdev_apic_t);
#define PHYSDEVOP_alloc_irq_vector      10
#define PHYSDEVOP_free_irq_vector       11
struct physdev_irq {
uint32_t irq;
uint32_t vector;
};
typedef struct physdev_irq physdev_irq_t;
DEFINE_XEN_GUEST_HANDLE(physdev_irq_t);
#define MAP_PIRQ_TYPE_MSI               0x0
#define MAP_PIRQ_TYPE_GSI               0x1
#define MAP_PIRQ_TYPE_UNKNOWN           0x2
#define PHYSDEVOP_map_pirq               13
struct physdev_map_pirq {
domid_t domid;
int type;
int index;
int pirq;
int bus;
int devfn;
int entry_nr;
uint64_t table_base;
};
typedef struct physdev_map_pirq physdev_map_pirq_t;
DEFINE_XEN_GUEST_HANDLE(physdev_map_pirq_t);
#define PHYSDEVOP_unmap_pirq             14
struct physdev_unmap_pirq {
domid_t domid;
int pirq;
};
typedef struct physdev_unmap_pirq physdev_unmap_pirq_t;
DEFINE_XEN_GUEST_HANDLE(physdev_unmap_pirq_t);
#define PHYSDEVOP_manage_pci_add         15
#define PHYSDEVOP_manage_pci_remove      16
struct physdev_manage_pci {
uint8_t bus;
uint8_t devfn;
};
typedef struct physdev_manage_pci physdev_manage_pci_t;
DEFINE_XEN_GUEST_HANDLE(physdev_manage_pci_t);
struct physdev_op {
uint32_t cmd;
union {
struct physdev_irq_status_query      irq_status_query;
struct physdev_set_iopl              set_iopl;
struct physdev_set_iobitmap          set_iobitmap;
struct physdev_apic                  apic_op;
struct physdev_irq                   irq_op;
} u;
};
typedef struct physdev_op physdev_op_t;
DEFINE_XEN_GUEST_HANDLE(physdev_op_t);
#define PHYSDEVOP_IRQ_UNMASK_NOTIFY      4
#define PHYSDEVOP_IRQ_STATUS_QUERY       PHYSDEVOP_irq_status_query
#define PHYSDEVOP_SET_IOPL               PHYSDEVOP_set_iopl
#define PHYSDEVOP_SET_IOBITMAP           PHYSDEVOP_set_iobitmap
#define PHYSDEVOP_APIC_READ              PHYSDEVOP_apic_read
#define PHYSDEVOP_APIC_WRITE             PHYSDEVOP_apic_write
#define PHYSDEVOP_ASSIGN_VECTOR          PHYSDEVOP_alloc_irq_vector
#define PHYSDEVOP_FREE_VECTOR            PHYSDEVOP_free_irq_vector
#define PHYSDEVOP_IRQ_NEEDS_UNMASK_NOTIFY XENIRQSTAT_needs_eoi
#define PHYSDEVOP_IRQ_SHARED             XENIRQSTAT_shared
#endif