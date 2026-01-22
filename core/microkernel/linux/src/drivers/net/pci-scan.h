#ifndef _PCI_SCAN_H
#define _PCI_SCAN_H
enum pci_id_flags_bits {
PCI_USES_IO=1, PCI_USES_MEM=2, PCI_USES_MASTER=4,
PCI_ADDR0=0<<4, PCI_ADDR1=1<<4, PCI_ADDR2=2<<4, PCI_ADDR3=3<<4,
PCI_ADDR_64BITS=0x100, PCI_NO_ACPI_WAKE=0x200, PCI_NO_MIN_LATENCY=0x400,
PCI_UNUSED_IRQ=0x800,
};
struct pci_id_info {
const char *name;
struct match_info {
int pci, pci_mask, subsystem, subsystem_mask;
int revision, revision_mask;
} id;
enum pci_id_flags_bits pci_flags;
int io_size;
int drv_flags;
};
enum drv_id_flags {
PCI_HOTSWAP=1,
};
enum drv_pwr_action {
DRV_NOOP,
DRV_ATTACH,
DRV_SUSPEND,
DRV_RESUME,
DRV_DETACH,
DRV_PWR_WakeOn,
DRV_PWR_DOWN,
DRV_PWR_UP,
};
struct drv_id_info {
const char *name;
int flags;
int pci_class;
struct pci_id_info *pci_dev_tbl;
void *(*probe1)(struct pci_dev *pdev, void *dev_ptr,
long ioaddr, int irq, int table_idx, int fnd_cnt);
int (*pwr_event)(void *dev, int event);
struct drv_id_info *next;
void *cb_ops;
};
extern int pci_drv_register(struct drv_id_info *drv_id, void *initial_device);
extern void pci_drv_unregister(struct drv_id_info *drv_id);
int acpi_wake(struct pci_dev *pdev);
enum acpi_pwr_state {ACPI_D0, ACPI_D1, ACPI_D2, ACPI_D3};
int acpi_set_pwr_state(struct pci_dev *pdev, enum acpi_pwr_state state);
#endif