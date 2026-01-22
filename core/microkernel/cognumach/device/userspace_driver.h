#ifndef _DEVICE_USERSPACE_DRIVER_H_
#define _DEVICE_USERSPACE_DRIVER_H_
#include <device/userspace_driver_types.h>
#include <device/dev_hdr.h>
#include <kern/lock.h>
#include <kern/queue.h>
#include <kern/kalloc.h>
#include <mach/kern_return.h>
#include <stddef.h>
struct usrd_registry_impl {
decl_simple_lock_data(, lock)
queue_head_t driver_list;
usrd_proxy_t driver_table[USRD_MAX_DRIVERS];
unsigned int next_token;
unsigned int active_count;
mach_port_t registry_port;
boolean_t initialized;
};
struct usrd_proxy_impl {
struct usrd_proxy public;
queue_chain_t chain;
decl_simple_lock_data(, lock)
task_t driver_task;
vm_map_t driver_map;
boolean_t is_sandboxed;
unsigned int memory_allocated;
unsigned int memory_peak;
unsigned int io_operations_active;
unsigned int interrupt_count;
queue_head_t device_list;
unsigned int device_count;
unsigned int last_heartbeat;
unsigned int crash_count;
unsigned int restart_count;
boolean_t is_healthy;
mach_port_t driver_port;
mach_port_t reply_port;
};
struct usrd_device_proxy_impl {
struct usrd_device_proxy public;
queue_chain_t chain;
decl_simple_lock_data(, lock)
struct mach_device device_header;
dev_ops_t original_ops;
unsigned int total_reads;
unsigned int total_writes;
unsigned int total_ioctls;
unsigned int error_count;
unsigned int operation_timeout;
unsigned int last_operation;
boolean_t is_responsive;
};
extern struct usrd_registry_impl usrd_global_registry;
extern kern_return_t usrd_registry_init(void);
extern void usrd_registry_shutdown(void);
extern kern_return_t usrd_driver_register(struct usrd_driver_info *info,
mach_port_t driver_port,
usrd_token_t *token_out);
extern kern_return_t usrd_driver_unregister(usrd_token_t token);
extern usrd_proxy_t usrd_driver_lookup(usrd_token_t token);
extern usrd_proxy_t usrd_driver_lookup_by_name(const char *name);
extern usrd_proxy_t usrd_proxy_create(struct usrd_driver_info *info,
mach_port_t driver_port,
task_t driver_task);
extern void usrd_proxy_destroy(usrd_proxy_t proxy);
extern kern_return_t usrd_proxy_update_status(usrd_proxy_t proxy,
unsigned int status_flags,
struct usrd_resource_usage *usage);
extern boolean_t usrd_proxy_check_health(usrd_proxy_t proxy);
extern kern_return_t usrd_proxy_enforce_limits(usrd_proxy_t proxy);
extern usrd_device_proxy_t usrd_device_proxy_create(usrd_proxy_t driver_proxy,
const char *device_name,
mach_port_t device_port);
extern void usrd_device_proxy_destroy(usrd_device_proxy_t device_proxy);
extern kern_return_t usrd_device_proxy_forward_operation(usrd_device_proxy_t device_proxy,
unsigned int operation,
void *data);
extern kern_return_t usrd_create_sandbox(usrd_proxy_t proxy);
extern kern_return_t usrd_enforce_sandbox(usrd_proxy_t proxy);
extern boolean_t usrd_validate_driver_port(mach_port_t port, task_t task);
extern boolean_t usrd_check_resource_limits(usrd_proxy_t proxy,
unsigned int resource_type,
unsigned int amount);
extern io_return_t usrd_device_open(device_t dev, dev_mode_t mode, io_req_t ior);
extern void usrd_device_close(device_t dev);
extern io_return_t usrd_device_read(device_t dev, io_req_t ior);
extern io_return_t usrd_device_write(device_t dev, io_req_t ior);
extern io_return_t usrd_device_get_status(device_t dev, dev_flavor_t flavor,
dev_status_t status, natural_t *count);
extern io_return_t usrd_device_set_status(device_t dev, dev_flavor_t flavor,
dev_status_t status, natural_t count);
extern void usrd_dump_registry_info(void);
extern void usrd_dump_driver_info(usrd_proxy_t proxy);
extern void usrd_dump_device_info(usrd_device_proxy_t device_proxy);
extern kern_return_t usrd_get_statistics(void *buffer, size_t *size);
#define USRD_PROXY_LOCK(proxy) \
simple_lock(&(proxy)->lock)
#define USRD_PROXY_UNLOCK(proxy) \
simple_unlock(&(proxy)->lock)
#define USRD_DEVICE_PROXY_LOCK(device) \
simple_lock(&(device)->lock)
#define USRD_DEVICE_PROXY_UNLOCK(device) \
simple_unlock(&(device)->lock)
#define USRD_REGISTRY_LOCK() \
simple_lock(&usrd_global_registry.lock)
#define USRD_REGISTRY_UNLOCK() \
simple_unlock(&usrd_global_registry.lock)
#define USRD_VALIDATE_TOKEN(token) \
((token) != USRD_TOKEN_INVALID && (token) < USRD_MAX_DRIVERS)
#define USRD_VALIDATE_PROXY(proxy) \
((proxy) != NULL && (proxy)->public.token != USRD_TOKEN_INVALID)
#define USRD_VALIDATE_DEVICE_PROXY(device) \
((device) != NULL && (device)->public.driver_proxy != NULL)
#define USRD_LOG_ERROR(proxy, fmt, ...) \
printf("USRD[%s]: " fmt "\n", (proxy)->public.info.name, ##__VA_ARGS__)
#define USRD_LOG_WARNING(proxy, fmt, ...) \
printf("USRD[%s]: WARNING: " fmt "\n", (proxy)->public.info.name, ##__VA_ARGS__)
#define USRD_LOG_INFO(proxy, fmt, ...) \
printf("USRD[%s]: " fmt "\n", (proxy)->public.info.name, ##__VA_ARGS__)
#endif