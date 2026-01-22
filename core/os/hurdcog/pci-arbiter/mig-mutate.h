#define PCI_IMPORTS \
import "../libnetfs/priv.h"; \
#define PCI_INTRAN protid_t begin_using_protid_port (pci_t)
#define PCI_INTRAN_PAYLOAD protid_t begin_using_protid_payload
#define PCI_DESTRUCTOR end_using_protid_port (protid_t)