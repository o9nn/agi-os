#include <device/userspace_driver_sdk.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <pthread.h>
#include <sys/time.h>
struct usrd_driver_instance_impl {
struct usrd_driver_instance public;
size_t allocated_memory;
size_t peak_memory;
struct {
char name[USRD_MAX_DEVICE_NAME];
unsigned int type;
void *context;
boolean_t in_use;
} devices[USRD_MAX_DEVICES_PER_DRIVER];
unsigned int total_operations;
unsigned int failed_operations;
unsigned int start_time;
pthread_mutex_t mutex;
pthread_cond_t message_cond;
};
static unsigned int get_timestamp(void)
{
struct timeval tv;
gettimeofday(&tv, NULL);
return (unsigned int)tv.tv_sec;
}
usrd_driver_t usrd_driver_create(const char *name,
const char *description,
unsigned int version_major,
unsigned int version_minor,
unsigned int capabilities,
struct usrd_driver_callbacks *callbacks)
{
struct usrd_driver_instance_impl *driver;
if (!name || !description || !callbacks) {
return NULL;
}
driver = malloc(sizeof(struct usrd_driver_instance_impl));
if (!driver) {
return NULL;
}
memset(driver, 0, sizeof(struct usrd_driver_instance_impl));
strncpy(driver->public.info.name, name, sizeof(driver->public.info.name) - 1);
strncpy(driver->public.info.description, description,
sizeof(driver->public.info.description) - 1);
driver->public.info.version_major = version_major;
driver->public.info.version_minor = version_minor;
driver->public.info.capabilities = capabilities;
driver->public.callbacks = callbacks;
driver->public.max_devices = USRD_MAX_DEVICES_PER_DRIVER;
driver->public.active_devices = 0;
driver->public.is_running = FALSE;
driver->public.should_exit = FALSE;
driver->public.registration_token = USRD_TOKEN_INVALID;
driver->public.info.resource_limits[USRD_RESOURCE_MEMORY] = 1024;
driver->public.info.resource_limits[USRD_RESOURCE_CPU] = 50;
driver->public.info.resource_limits[USRD_RESOURCE_IO_OPS] = 16;
driver->public.info.resource_limits[USRD_RESOURCE_INTERRUPTS] = 1000;
driver->public.info.resource_limits[USRD_RESOURCE_TIMEOUT] = 5000;
driver->allocated_memory = 0;
driver->peak_memory = 0;
driver->total_operations = 0;
driver->failed_operations = 0;
driver->start_time = get_timestamp();
for (int i = 0; i < USRD_MAX_DEVICES_PER_DRIVER; i++) {
driver->devices[i].in_use = FALSE;
}
pthread_mutex_init(&driver->mutex, NULL);
pthread_cond_init(&driver->message_cond, NULL);
driver->public.status_flags = USRD_STATUS_IDLE;
printf("Created user-space driver '%s' v%u.%u\n",
name, version_major, version_minor);
return (usrd_driver_t)driver;
}
void usrd_driver_destroy(usrd_driver_t driver_pub)
{
struct usrd_driver_instance_impl *driver =
(struct usrd_driver_instance_impl *)driver_pub;
if (!driver) {
return;
}
if (driver->public.is_running) {
usrd_driver_stop(driver_pub);
}
pthread_mutex_destroy(&driver->mutex);
pthread_cond_destroy(&driver->message_cond);
printf("Destroyed user-space driver '%s'\n", driver->public.info.name);
free(driver);
}
kern_return_t usrd_driver_register(usrd_driver_t driver_pub,
mach_port_t registry_port)
{
struct usrd_driver_instance_impl *driver =
(struct usrd_driver_instance_impl *)driver_pub;
kern_return_t ret;
if (!driver) {
return KERN_INVALID_ARGUMENT;
}
pthread_mutex_lock(&driver->mutex);
if (driver->public.registration_token != USRD_TOKEN_INVALID) {
pthread_mutex_unlock(&driver->mutex);
return KERN_ALREADY_IN_SET;
}
ret = mach_port_allocate(mach_task_self(), MACH_PORT_RIGHT_RECEIVE,
&driver->public.driver_port);
if (ret != KERN_SUCCESS) {
pthread_mutex_unlock(&driver->mutex);
return ret;
}
driver->public.registry_port = registry_port;
driver->public.registration_token = (usrd_token_t)(driver->start_time & 0xFFFF);
driver->public.status_flags = USRD_STATUS_RUNNING;
pthread_mutex_unlock(&driver->mutex);
printf("Registered driver '%s' with token %u\n",
driver->public.info.name, driver->public.registration_token);
return KERN_SUCCESS;
}
kern_return_t usrd_driver_unregister(usrd_driver_t driver_pub)
{
struct usrd_driver_instance_impl *driver =
(struct usrd_driver_instance_impl *)driver_pub;
if (!driver) {
return KERN_INVALID_ARGUMENT;
}
pthread_mutex_lock(&driver->mutex);
if (driver->public.registration_token == USRD_TOKEN_INVALID) {
pthread_mutex_unlock(&driver->mutex);
return KERN_INVALID_ARGUMENT;
}
if (MACH_PORT_VALID(driver->public.driver_port)) {
mach_port_deallocate(mach_task_self(), driver->public.driver_port);
driver->public.driver_port = MACH_PORT_NULL;
}
driver->public.registration_token = USRD_TOKEN_INVALID;
driver->public.status_flags = USRD_STATUS_IDLE;
pthread_mutex_unlock(&driver->mutex);
printf("Unregistered driver '%s'\n", driver->public.info.name);
return KERN_SUCCESS;
}
static void *driver_message_thread(void *arg)
{
struct usrd_driver_instance_impl *driver =
(struct usrd_driver_instance_impl *)arg;
printf("Started message thread for driver '%s'\n",
driver->public.info.name);
while (!driver->public.should_exit) {
pthread_mutex_lock(&driver->mutex);
pthread_cond_wait(&driver->message_cond, &driver->mutex);
if (driver->public.should_exit) {
pthread_mutex_unlock(&driver->mutex);
break;
}
pthread_mutex_unlock(&driver->mutex);
}
printf("Message thread for driver '%s' exiting\n",
driver->public.info.name);
return NULL;
}
kern_return_t usrd_driver_run(usrd_driver_t driver_pub)
{
struct usrd_driver_instance_impl *driver =
(struct usrd_driver_instance_impl *)driver_pub;
kern_return_t ret;
if (!driver) {
return KERN_INVALID_ARGUMENT;
}
pthread_mutex_lock(&driver->mutex);
if (driver->public.is_running) {
pthread_mutex_unlock(&driver->mutex);
return KERN_ALREADY_IN_SET;
}
if (driver->public.registration_token == USRD_TOKEN_INVALID) {
pthread_mutex_unlock(&driver->mutex);
return KERN_INVALID_ARGUMENT;
}
if (driver->public.callbacks->driver_init) {
ret = driver->public.callbacks->driver_init(driver_pub);
if (ret != KERN_SUCCESS) {
pthread_mutex_unlock(&driver->mutex);
return ret;
}
}
driver->public.is_running = TRUE;
driver->public.should_exit = FALSE;
if (pthread_create(&driver->public.message_thread, NULL,
driver_message_thread, driver) != 0) {
driver->public.is_running = FALSE;
pthread_mutex_unlock(&driver->mutex);
return KERN_RESOURCE_SHORTAGE;
}
pthread_mutex_unlock(&driver->mutex);
printf("Driver '%s' is now running\n", driver->public.info.name);
pthread_join(driver->public.message_thread, NULL);
return KERN_SUCCESS;
}
kern_return_t usrd_driver_stop(usrd_driver_t driver_pub)
{
struct usrd_driver_instance_impl *driver =
(struct usrd_driver_instance_impl *)driver_pub;
if (!driver) {
return KERN_INVALID_ARGUMENT;
}
pthread_mutex_lock(&driver->mutex);
if (!driver->public.is_running) {
pthread_mutex_unlock(&driver->mutex);
return KERN_INVALID_ARGUMENT;
}
driver->public.should_exit = TRUE;
pthread_cond_signal(&driver->message_cond);
pthread_mutex_unlock(&driver->mutex);
pthread_join(driver->public.message_thread, NULL);
pthread_mutex_lock(&driver->mutex);
if (driver->public.callbacks->driver_shutdown) {
driver->public.callbacks->driver_shutdown(driver_pub);
}
driver->public.is_running = FALSE;
driver->public.status_flags = USRD_STATUS_IDLE;
pthread_mutex_unlock(&driver->mutex);
printf("Driver '%s' stopped\n", driver->public.info.name);
return KERN_SUCCESS;
}
kern_return_t usrd_driver_set_resource_limits(usrd_driver_t driver_pub,
unsigned int resource_type,
unsigned int limit)
{
struct usrd_driver_instance_impl *driver =
(struct usrd_driver_instance_impl *)driver_pub;
if (!driver || resource_type >= 8) {
return KERN_INVALID_ARGUMENT;
}
pthread_mutex_lock(&driver->mutex);
driver->public.info.resource_limits[resource_type] = limit;
pthread_mutex_unlock(&driver->mutex);
return KERN_SUCCESS;
}
kern_return_t usrd_driver_add_device(usrd_driver_t driver_pub,
const char *device_name,
unsigned int device_type,
void *device_context)
{
struct usrd_driver_instance_impl *driver =
(struct usrd_driver_instance_impl *)driver_pub;
int free_slot = -1;
if (!driver || !device_name) {
return KERN_INVALID_ARGUMENT;
}
pthread_mutex_lock(&driver->mutex);
for (int i = 0; i < USRD_MAX_DEVICES_PER_DRIVER; i++) {
if (!driver->devices[i].in_use) {
free_slot = i;
break;
}
}
if (free_slot == -1) {
pthread_mutex_unlock(&driver->mutex);
return KERN_RESOURCE_SHORTAGE;
}
strncpy(driver->devices[free_slot].name, device_name,
USRD_MAX_DEVICE_NAME - 1);
driver->devices[free_slot].name[USRD_MAX_DEVICE_NAME - 1] = '\0';
driver->devices[free_slot].type = device_type;
driver->devices[free_slot].context = device_context;
driver->devices[free_slot].in_use = TRUE;
driver->public.active_devices++;
pthread_mutex_unlock(&driver->mutex);
printf("Added device '%s' to driver '%s'\n",
device_name, driver->public.info.name);
return KERN_SUCCESS;
}
void usrd_log(usrd_driver_t driver_pub, int level, const char *fmt, ...)
{
struct usrd_driver_instance_impl *driver =
(struct usrd_driver_instance_impl *)driver_pub;
va_list args;
const char *level_str;
if (!driver || !fmt) {
return;
}
switch (level) {
case USRD_LOG_ERROR:   level_str = "ERROR"; break;
case USRD_LOG_WARNING: level_str = "WARN"; break;
case USRD_LOG_INFO:    level_str = "INFO"; break;
case USRD_LOG_DEBUG:   level_str = "DEBUG"; break;
default:               level_str = "UNKNOWN"; break;
}
printf("[%s:%s] ", driver->public.info.name, level_str);
va_start(args, fmt);
vprintf(fmt, args);
va_end(args);
printf("\n");
}
void *usrd_malloc(usrd_driver_t driver_pub, size_t size)
{
struct usrd_driver_instance_impl *driver =
(struct usrd_driver_instance_impl *)driver_pub;
void *ptr;
if (!driver || size == 0) {
return NULL;
}
ptr = malloc(size);
if (ptr) {
pthread_mutex_lock(&driver->mutex);
driver->allocated_memory += size;
if (driver->allocated_memory > driver->peak_memory) {
driver->peak_memory = driver->allocated_memory;
}
driver->public.usage.memory_kb = driver->allocated_memory / 1024;
pthread_mutex_unlock(&driver->mutex);
}
return ptr;
}
void usrd_free(usrd_driver_t driver_pub, void *ptr, size_t size)
{
struct usrd_driver_instance_impl *driver =
(struct usrd_driver_instance_impl *)driver_pub;
if (!driver || !ptr) {
return;
}
free(ptr);
pthread_mutex_lock(&driver->mutex);
if (driver->allocated_memory >= size) {
driver->allocated_memory -= size;
}
driver->public.usage.memory_kb = driver->allocated_memory / 1024;
pthread_mutex_unlock(&driver->mutex);
}
void usrd_dump_stats(usrd_driver_t driver_pub)
{
struct usrd_driver_instance_impl *driver =
(struct usrd_driver_instance_impl *)driver_pub;
unsigned int uptime;
if (!driver) {
return;
}
pthread_mutex_lock(&driver->mutex);
uptime = get_timestamp() - driver->start_time;
printf("=== Driver Statistics: %s ===\n", driver->public.info.name);
printf("Version: %u.%u\n",
driver->public.info.version_major, driver->public.info.version_minor);
printf("Capabilities: 0x%x\n", driver->public.info.capabilities);
printf("Status: %s\n",
driver->public.is_running ? "Running" : "Stopped");
printf("Uptime: %u seconds\n", uptime);
printf("Devices: %u / %u\n",
driver->public.active_devices, driver->public.max_devices);
printf("Memory: %zu KB (peak: %zu KB)\n",
driver->allocated_memory / 1024, driver->peak_memory / 1024);
printf("Operations: %u total, %u failed\n",
driver->total_operations, driver->failed_operations);
printf("Registration token: %u\n", driver->public.registration_token);
pthread_mutex_unlock(&driver->mutex);
printf("=== End Statistics ===\n");
}