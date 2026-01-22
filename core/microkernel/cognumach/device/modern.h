#ifndef _DEVICE_MODERN_H_
#define _DEVICE_MODERN_H_
#include <device/conf.h>
#include <device/dev_hdr.h>
#include <kern/lock.h>
#define DEV_CAP_NONE            0x00000000
#define DEV_CAP_POWER_MGMT      0x00000001
#define DEV_CAP_HOTPLUG         0x00000002
#define DEV_CAP_MULTI_QUEUE     0x00000004
#define DEV_CAP_ERROR_RECOVERY  0x00000008
#define DEV_CAP_RESOURCE_LIMIT  0x00000010
#define DEV_CAP_DIAGNOSTICS     0x00000020
#define DEV_CAP_ASYNC_NOTIFY    0x00000040
struct dev_version {
unsigned int    major;
unsigned int    minor;
unsigned int    patch;
unsigned int    build;
};
struct dev_error_info {
io_return_t     basic_error;
unsigned int    extended_code;
unsigned int    context_flags;
char            description[64];
void           *debug_data;
unsigned int    debug_size;
};
struct dev_resource_limits {
unsigned int    max_memory;
unsigned int    max_interrupts;
unsigned int    max_io_ops;
unsigned int    timeout_ms;
};
struct dev_resource_usage {
unsigned int    current_memory;
unsigned int    current_interrupts;
unsigned int    current_io_ops;
unsigned int    total_errors;
unsigned int    last_error_time;
};
typedef enum {
DEV_STATE_UNKNOWN = 0,
DEV_STATE_INITIALIZING,
DEV_STATE_READY,
DEV_STATE_BUSY,
DEV_STATE_ERROR,
DEV_STATE_RECOVERY,
DEV_STATE_SUSPENDED,
DEV_STATE_REMOVING
} dev_health_state_t;
struct dev_health_info {
dev_health_state_t  state;
unsigned int        uptime;
unsigned int        error_count;
unsigned int        recovery_count;
unsigned int        last_health_check;
};
struct dev_modern_ops {
struct dev_ops      basic;
struct dev_version  version;
unsigned int        capabilities;
const char         *vendor;
const char         *description;
struct dev_resource_limits limits;
struct dev_resource_usage  usage;
decl_simple_lock_data(, resource_lock)
struct dev_health_info health;
int (*d_health_check)(dev_t);
int (*d_diagnostics)(dev_t, void *buffer, size_t *size);
int (*d_get_error_info)(dev_t, struct dev_error_info *);
int (*d_recovery)(dev_t, unsigned int recovery_type);
int (*d_suspend)(dev_t);
int (*d_resume)(dev_t);
int (*d_power_state)(dev_t, unsigned int state);
int (*d_io_queue_setup)(dev_t, unsigned int num_queues);
int (*d_io_queue_submit)(dev_t, unsigned int queue_id, io_req_t);
};
typedef struct dev_modern_ops *dev_modern_ops_t;
struct mach_device_modern {
struct mach_device  basic;
dev_modern_ops_t    modern_ops;
boolean_t           is_modern;
decl_simple_lock_data(, safety_lock)
unsigned int        validation_cookie;
unsigned int        last_validated;
unsigned int        total_operations;
unsigned int        failed_operations;
unsigned int        avg_response_time;
};
typedef struct mach_device_modern *mach_device_modern_t;
#define MACH_DEVICE_MODERN_NULL ((mach_device_modern_t)0)
#define DEV_VALIDATION_COOKIE   0x44455643
#define DEV_VALIDATE_DEVICE(dev) \
do { if ((dev) == MACH_DEVICE_NULL) panic("NULL device pointer"); } while(0)
#define DEV_VALIDATE_MODERN_DEVICE(dev) \
do { \
DEV_VALIDATE_DEVICE((mach_device_t)(dev)); \
if (!(dev)->is_modern) panic("Not a modern device"); \
if ((dev)->validation_cookie != DEV_VALIDATION_COOKIE) panic("Device corruption detected"); \
} while(0)
#define DEV_CHECK_RESOURCE_LIMITS(dev, resource_type, amount) \
dev_check_resource_limits((dev), (resource_type), (amount))
#define DEV_RESOURCE_MEMORY     0
#define DEV_RESOURCE_INTERRUPTS 1
#define DEV_RESOURCE_IO_OPS     2
extern mach_device_modern_t mach_device_modern_create(dev_modern_ops_t ops);
extern void mach_device_modern_destroy(mach_device_modern_t device);
extern kern_return_t mach_device_modern_register(mach_device_modern_t device, const char *name);
extern boolean_t dev_check_resource_limits(mach_device_modern_t device,
unsigned int resource_type,
unsigned int amount);
extern void dev_update_resource_usage(mach_device_modern_t device,
unsigned int resource_type,
int delta);
extern kern_return_t dev_health_check(mach_device_modern_t device);
extern void dev_update_health_state(mach_device_modern_t device, dev_health_state_t new_state);
extern boolean_t dev_validate_modern_device(mach_device_modern_t device);
extern void dev_mark_device_corrupted(mach_device_modern_t device);
extern void dev_log_error(mach_device_modern_t device, struct dev_error_info *error);
extern kern_return_t dev_attempt_recovery(mach_device_modern_t device, unsigned int recovery_type);
extern void dev_dump_diagnostics(mach_device_modern_t device);
extern kern_return_t dev_get_performance_stats(mach_device_modern_t device, void *buffer, size_t *size);
static inline boolean_t dev_is_modern(mach_device_t device) {
mach_device_modern_t modern = (mach_device_modern_t)device;
return (modern && modern->is_modern);
}
static inline dev_modern_ops_t dev_get_modern_ops(mach_device_t device) {
if (dev_is_modern(device)) {
return ((mach_device_modern_t)device)->modern_ops;
}
return NULL;
}
#endif