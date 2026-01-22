#ifndef __XEN_PUBLIC_NMI_H__
#define __XEN_PUBLIC_NMI_H__
#define _XEN_NMIREASON_io_error 0
#define XEN_NMIREASON_io_error (1UL << _XEN_NMIREASON_io_error)
#define _XEN_NMIREASON_parity_error 1
#define XEN_NMIREASON_parity_error (1UL << _XEN_NMIREASON_parity_error)
#define _XEN_NMIREASON_unknown 2
#define XEN_NMIREASON_unknown (1UL << _XEN_NMIREASON_unknown)
#define XENNMI_register_callback 0
struct xennmi_callback {
unsigned long handler_address;
unsigned long pad;
};
typedef struct xennmi_callback xennmi_callback_t;
DEFINE_XEN_GUEST_HANDLE(xennmi_callback_t);
#define XENNMI_unregister_callback 1
#endif