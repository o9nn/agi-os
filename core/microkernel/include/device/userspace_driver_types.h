#ifndef _DEVICE_USERSPACE_DRIVER_TYPES_H_
#define _DEVICE_USERSPACE_DRIVER_TYPES_H_
#include <mach/mach_types.h>
#include <device/device_types.h>
#define USRD_CAP_NONE           0x00000000
#define USRD_CAP_BLOCK_DEVICE   0x00000001
#define USRD_CAP_CHAR_DEVICE    0x00000002
#define USRD_CAP_NETWORK        0x00000004
#define USRD_CAP_GRAPHICS       0x00000008
#define USRD_CAP_AUDIO          0x00000010
#define USRD_CAP_INPUT          0x00000020
#define USRD_CAP_STORAGE        0x00000040
#define USRD_CAP_USB            0x00000080
#define USRD_CAP_PCI            0x00000100
#define USRD_CAP_INTERRUPT      0x00000200
#define USRD_CAP_DMA            0x00000400
#define USRD_CAP_POWER_MGMT     0x00000800
#define USRD_CAP_HOTPLUG        0x00001000
#define USRD_STATUS_RUNNING     0x00000001
#define USRD_STATUS_IDLE        0x00000002
#define USRD_STATUS_BUSY        0x00000004
#define USRD_STATUS_ERROR       0x00000008
#define USRD_STATUS_SUSPENDED   0x00000010
#define USRD_STATUS_RECOVERING  0x00000020
#define USRD_RESOURCE_MEMORY    0
#define USRD_RESOURCE_CPU       1
#define USRD_RESOURCE_IO_OPS    2
#define USRD_RESOURCE_INTERRUPTS 3
#define USRD_RESOURCE_TIMEOUT   4
#define USRD_RESOURCE_BANDWIDTH 5
#define USRD_RESOURCE_RESERVED1 6
#define USRD_RESOURCE_RESERVED2 7
typedef unsigned int usrd_token_t;
#define USRD_TOKEN_INVALID      0
struct usrd_driver_info {
char        name[64];
char        description[128];
unsigned int version_major;
unsigned int version_minor;
unsigned int capabilities;
unsigned int resource_limits[8];
};
typedef struct usrd_driver_info *usrd_driver_info_t;
struct usrd_resource_usage {
unsigned int memory_kb;
unsigned int cpu_percent;
unsigned int io_ops_active;
unsigned int interrupt_rate;
unsigned int avg_response_time;
unsigned int error_count;
unsigned int recovery_count;
unsigned int uptime_seconds;
};
typedef struct usrd_resource_usage *usrd_resource_usage_t;
struct usrd_registry {
mach_port_t registry_port;
unsigned int max_drivers;
unsigned int active_drivers;
};
typedef struct usrd_registry *usrd_registry_t;
struct usrd_proxy {
usrd_token_t token;
mach_port_t driver_port;
struct usrd_driver_info info;
struct usrd_resource_usage usage;
unsigned int status_flags;
unsigned int last_heartbeat;
boolean_t is_trusted;
};
typedef struct usrd_proxy *usrd_proxy_t;
struct usrd_device_proxy {
mach_port_t device_port;
usrd_proxy_t driver_proxy;
char device_name[64];
unsigned int device_type;
boolean_t is_open;
unsigned int open_count;
};
typedef struct usrd_device_proxy *usrd_device_proxy_t;
#define USRD_MAX_DRIVER_NAME    64
#define USRD_MAX_DRIVER_DESC    128
#define USRD_MAX_DEVICE_NAME    64
#define USRD_MAX_DRIVERS        256
#define USRD_MAX_DEVICES_PER_DRIVER 32
#define USRD_SUCCESS            0
#define USRD_ERROR_INVALID_DRIVER    1
#define USRD_ERROR_DRIVER_EXISTS     2
#define USRD_ERROR_NO_RESOURCES      3
#define USRD_ERROR_NOT_REGISTERED    4
#define USRD_ERROR_DEVICE_BUSY       5
#define USRD_ERROR_TIMEOUT           6
#define USRD_ERROR_PERMISSION_DENIED 7
#define USRD_ERROR_DRIVER_CRASHED    8
#endif