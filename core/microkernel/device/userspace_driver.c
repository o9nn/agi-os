#include <device/userspace_driver.h>
#include <device/ds_routines.h>
#include <kern/printf.h>
#include <i386/i386/time_stamp.h>
#include <kern/task.h>
#include <vm/vm_map.h>
#include <ipc/ipc_port.h>
#include <ipc/ipc_space.h>
static io_return_t usrd_device_open(device_t dev, dev_mode_t mode, io_req_t ior);
static io_return_t usrd_device_read(device_t dev, io_req_t ior);
static io_return_t usrd_device_write(device_t dev, io_req_t ior);
struct usrd_registry_impl usrd_global_registry;
kern_return_t usrd_registry_init(void)
{
simple_lock_init(&usrd_global_registry.lock);
queue_init(&usrd_global_registry.driver_list);
for (int i = 0; i < USRD_MAX_DRIVERS; i++) {
usrd_global_registry.driver_table[i] = NULL;
}
usrd_global_registry.next_token = 1;
usrd_global_registry.active_count = 0;
usrd_global_registry.registry_port = MACH_PORT_NULL;
usrd_global_registry.initialized = TRUE;
printf("User-space driver registry initialized\n");
return KERN_SUCCESS;
}
void usrd_registry_shutdown(void)
{
usrd_proxy_t proxy, next_proxy;
USRD_REGISTRY_LOCK();
queue_iterate_safely(&usrd_global_registry.driver_list, proxy, next_proxy,
struct usrd_proxy_impl *, chain) {
usrd_proxy_destroy(proxy);
}
usrd_global_registry.initialized = FALSE;
USRD_REGISTRY_UNLOCK();
printf("User-space driver registry shutdown\n");
}
kern_return_t usrd_driver_register(struct usrd_driver_info *info,
mach_port_t driver_port,
usrd_token_t *token_out)
{
usrd_proxy_t proxy;
usrd_token_t token;
task_t driver_task;
if (!info || !token_out) {
return KERN_INVALID_ARGUMENT;
}
if (!usrd_global_registry.initialized) {
return KERN_FAILURE;
}
if (!MACH_PORT_VALID(driver_port)) {
return KERN_INVALID_ARGUMENT;
}
driver_task = current_task();
USRD_REGISTRY_LOCK();
if (usrd_driver_lookup_by_name(info->name) != NULL) {
USRD_REGISTRY_UNLOCK();
return KERN_ALREADY_IN_SET;
}
if (usrd_global_registry.active_count >= USRD_MAX_DRIVERS) {
USRD_REGISTRY_UNLOCK();
return KERN_RESOURCE_SHORTAGE;
}
token = usrd_global_registry.next_token++;
if (usrd_global_registry.next_token >= USRD_MAX_DRIVERS) {
usrd_global_registry.next_token = 1;
}
USRD_REGISTRY_UNLOCK();
proxy = usrd_proxy_create(info, driver_port, driver_task);
if (proxy == NULL) {
return KERN_RESOURCE_SHORTAGE;
}
proxy->public.token = token;
USRD_REGISTRY_LOCK();
usrd_global_registry.driver_table[token % USRD_MAX_DRIVERS] = proxy;
queue_enter(&usrd_global_registry.driver_list, proxy, usrd_proxy_t, chain);
usrd_global_registry.active_count++;
USRD_REGISTRY_UNLOCK();
*token_out = token;
printf("User-space driver '%s' registered with token %u\n",
info->name, token);
return KERN_SUCCESS;
}
kern_return_t usrd_driver_unregister(usrd_token_t token)
{
usrd_proxy_t proxy;
if (!USRD_VALIDATE_TOKEN(token)) {
return KERN_INVALID_ARGUMENT;
}
USRD_REGISTRY_LOCK();
proxy = usrd_global_registry.driver_table[token % USRD_MAX_DRIVERS];
if (proxy == NULL || proxy->public.token != token) {
USRD_REGISTRY_UNLOCK();
return KERN_INVALID_ARGUMENT;
}
usrd_global_registry.driver_table[token % USRD_MAX_DRIVERS] = NULL;
queue_remove(&usrd_global_registry.driver_list, proxy, usrd_proxy_t, chain);
usrd_global_registry.active_count--;
USRD_REGISTRY_UNLOCK();
printf("User-space driver '%s' unregistered\n", proxy->public.info.name);
usrd_proxy_destroy(proxy);
return KERN_SUCCESS;
}
usrd_proxy_t usrd_driver_lookup(usrd_token_t token)
{
usrd_proxy_t proxy;
if (!USRD_VALIDATE_TOKEN(token)) {
return NULL;
}
USRD_REGISTRY_LOCK();
proxy = usrd_global_registry.driver_table[token % USRD_MAX_DRIVERS];
if (proxy && proxy->public.token == token) {
USRD_REGISTRY_UNLOCK();
return proxy;
}
USRD_REGISTRY_UNLOCK();
return NULL;
}
usrd_proxy_t usrd_driver_lookup_by_name(const char *name)
{
usrd_proxy_t proxy;
if (!name) {
return NULL;
}
USRD_REGISTRY_LOCK();
queue_iterate(&usrd_global_registry.driver_list, proxy, usrd_proxy_t, chain) {
if (strcmp(proxy->public.info.name, name) == 0) {
USRD_REGISTRY_UNLOCK();
return proxy;
}
}
USRD_REGISTRY_UNLOCK();
return NULL;
}
usrd_proxy_t usrd_proxy_create(struct usrd_driver_info *info,
mach_port_t driver_port,
task_t driver_task)
{
struct usrd_proxy_impl *proxy;
proxy = (struct usrd_proxy_impl *)kalloc(sizeof(struct usrd_proxy_impl));
if (proxy == NULL) {
return NULL;
}
memset(proxy, 0, sizeof(struct usrd_proxy_impl));
simple_lock_init(&proxy->lock);
queue_init(&proxy->device_list);
proxy->public.info = *info;
proxy->public.driver_port = driver_port;
proxy->driver_task = driver_task;
proxy->driver_map = driver_task->map;
proxy->public.usage.memory_kb = 0;
proxy->public.usage.cpu_percent = 0;
proxy->public.usage.io_ops_active = 0;
proxy->public.usage.interrupt_rate = 0;
proxy->public.usage.error_count = 0;
proxy->public.usage.recovery_count = 0;
proxy->public.usage.uptime_seconds = 0;
proxy->public.status_flags = USRD_STATUS_RUNNING;
proxy->public.last_heartbeat = (unsigned int)time_stamp;
proxy->is_healthy = TRUE;
proxy->is_sandboxed = FALSE;
printf("Created driver proxy for '%s'\n", info->name);
return (usrd_proxy_t)proxy;
}
void usrd_proxy_destroy(usrd_proxy_t proxy_pub)
{
struct usrd_proxy_impl *proxy = (struct usrd_proxy_impl *)proxy_pub;
usrd_device_proxy_t device, next_device;
if (!USRD_VALIDATE_PROXY(proxy_pub)) {
return;
}
USRD_PROXY_LOCK(proxy);
queue_iterate_safely(&proxy->device_list, device, next_device,
struct usrd_device_proxy_impl *, chain) {
usrd_device_proxy_destroy(device);
}
printf("Destroying driver proxy for '%s'\n", proxy->public.info.name);
USRD_PROXY_UNLOCK(proxy);
kfree((vm_offset_t)proxy, sizeof(struct usrd_proxy_impl));
}
kern_return_t usrd_proxy_update_status(usrd_proxy_t proxy_pub,
unsigned int status_flags,
struct usrd_resource_usage *usage)
{
struct usrd_proxy_impl *proxy = (struct usrd_proxy_impl *)proxy_pub;
if (!USRD_VALIDATE_PROXY(proxy_pub) || !usage) {
return KERN_INVALID_ARGUMENT;
}
USRD_PROXY_LOCK(proxy);
proxy->public.status_flags = status_flags;
proxy->public.usage = *usage;
proxy->public.last_heartbeat = (unsigned int)time_stamp;
proxy->is_healthy = !(status_flags & USRD_STATUS_ERROR);
USRD_PROXY_UNLOCK(proxy);
return KERN_SUCCESS;
}
usrd_device_proxy_t usrd_device_proxy_create(usrd_proxy_t driver_proxy,
const char *device_name,
mach_port_t device_port)
{
struct usrd_device_proxy_impl *device;
struct usrd_proxy_impl *proxy = (struct usrd_proxy_impl *)driver_proxy;
if (!USRD_VALIDATE_PROXY(driver_proxy) || !device_name) {
return NULL;
}
device = (struct usrd_device_proxy_impl *)kalloc(sizeof(struct usrd_device_proxy_impl));
if (device == NULL) {
return NULL;
}
memset(device, 0, sizeof(struct usrd_device_proxy_impl));
simple_lock_init(&device->lock);
device->public.driver_proxy = driver_proxy;
strncpy(device->public.device_name, device_name, USRD_MAX_DEVICE_NAME - 1);
device->public.device_name[USRD_MAX_DEVICE_NAME - 1] = '\0';
device->public.device_port = device_port;
device->public.is_open = FALSE;
device->public.open_count = 0;
device->total_reads = 0;
device->total_writes = 0;
device->total_ioctls = 0;
device->error_count = 0;
device->operation_timeout = 5000;
device->is_responsive = TRUE;
USRD_PROXY_LOCK(proxy);
queue_enter(&proxy->device_list, device, usrd_device_proxy_t, chain);
proxy->device_count++;
USRD_PROXY_UNLOCK(proxy);
printf("Created device proxy '%s' for driver '%s'\n",
device_name, proxy->public.info.name);
return (usrd_device_proxy_t)device;
}
void usrd_device_proxy_destroy(usrd_device_proxy_t device_pub)
{
struct usrd_device_proxy_impl *device = (struct usrd_device_proxy_impl *)device_pub;
struct usrd_proxy_impl *proxy;
if (!USRD_VALIDATE_DEVICE_PROXY(device_pub)) {
return;
}
proxy = (struct usrd_proxy_impl *)device->public.driver_proxy;
USRD_PROXY_LOCK(proxy);
queue_remove(&proxy->device_list, device, usrd_device_proxy_t, chain);
proxy->device_count--;
USRD_PROXY_UNLOCK(proxy);
printf("Destroying device proxy '%s'\n", device->public.device_name);
kfree((vm_offset_t)device, sizeof(struct usrd_device_proxy_impl));
}
boolean_t usrd_check_resource_limits(usrd_proxy_t proxy_pub,
unsigned int resource_type,
unsigned int amount)
{
struct usrd_proxy_impl *proxy = (struct usrd_proxy_impl *)proxy_pub;
unsigned int limit, current;
if (!USRD_VALIDATE_PROXY(proxy_pub) || resource_type >= 8) {
return FALSE;
}
USRD_PROXY_LOCK(proxy);
limit = proxy->public.info.resource_limits[resource_type];
switch (resource_type) {
case USRD_RESOURCE_MEMORY:
current = proxy->public.usage.memory_kb;
break;
case USRD_RESOURCE_CPU:
current = proxy->public.usage.cpu_percent;
break;
case USRD_RESOURCE_IO_OPS:
current = proxy->public.usage.io_ops_active;
break;
case USRD_RESOURCE_INTERRUPTS:
current = proxy->public.usage.interrupt_rate;
break;
default:
current = 0;
break;
}
USRD_PROXY_UNLOCK(proxy);
return (limit == 0 || (current + amount) <= limit);
}
void usrd_dump_registry_info(void)
{
usrd_proxy_t proxy;
printf("=== User-space Driver Registry ===\n");
printf("Initialized: %s\n", usrd_global_registry.initialized ? "Yes" : "No");
printf("Active drivers: %u / %u\n",
usrd_global_registry.active_count, USRD_MAX_DRIVERS);
printf("Next token: %u\n", usrd_global_registry.next_token);
USRD_REGISTRY_LOCK();
printf("\nRegistered drivers:\n");
queue_iterate(&usrd_global_registry.driver_list, proxy, usrd_proxy_t, chain) {
printf("  Token %u: %s (v%u.%u) - %s\n",
proxy->token,
proxy->info.name,
proxy->info.version_major,
proxy->info.version_minor,
proxy->status_flags & USRD_STATUS_RUNNING ? "Running" : "Stopped");
}
USRD_REGISTRY_UNLOCK();
printf("=== End Registry Info ===\n");
}
static io_return_t usrd_device_open(device_t dev, dev_mode_t mode, io_req_t ior)
{
printf("USRD: device_open called\n");
return D_SUCCESS;
}
void usrd_device_close(device_t dev)
{
printf("USRD: device_close called\n");
}
static io_return_t usrd_device_read(device_t dev, io_req_t ior)
{
printf("USRD: device_read called\n");
return D_SUCCESS;
}
static io_return_t usrd_device_write(device_t dev, io_req_t ior)
{
printf("USRD: device_write called\n");
return D_SUCCESS;
}
io_return_t usrd_device_get_status(device_t dev, dev_flavor_t flavor,
dev_status_t status, natural_t *count)
{
printf("USRD: device_get_status called\n");
return D_SUCCESS;
}
io_return_t usrd_device_set_status(device_t dev, dev_flavor_t flavor,
dev_status_t status, natural_t count)
{
printf("USRD: device_set_status called\n");
return D_SUCCESS;
}