#ifndef _MACH_DEVICE_NOTIFY_H_
#define _MACH_DEVICE_NOTIFY_H_
#include <mach/port.h>
#include <mach/message.h>
typedef struct
{
mach_msg_header_t intr_header;
mach_msg_type_t intr_type;
int id;
} device_intr_notification_t;
#define DEVICE_INTR_NOTIFY 100
#endif