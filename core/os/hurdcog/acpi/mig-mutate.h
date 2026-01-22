#define ACPI_IMPORTS \
import "../libnetfs/priv.h"; \
#define ACPI_INTRAN protid_t begin_using_protid_port (acpi_t)
#define ACPI_INTRAN_PAYLOAD protid_t begin_using_protid_payload
#define ACPI_DESTRUCTOR end_using_protid_port (protid_t)