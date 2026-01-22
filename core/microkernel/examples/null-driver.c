#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <device/userspace_driver_sdk.h>
struct null_device_context {
char device_name[64];
boolean_t is_open;
unsigned int open_count;
unsigned int total_reads;
unsigned int total_writes;
unsigned int bytes_read;
unsigned int bytes_written;
};
static kern_return_t null_driver_init(usrd_driver_t driver);
static void null_driver_shutdown(usrd_driver_t driver);
static kern_return_t null_device_open(usrd_driver_t driver, const char *device_name,
dev_mode_t mode, void **device_context);
static kern_return_t null_device_close(usrd_driver_t driver, void *device_context);
static kern_return_t null_device_read(usrd_driver_t driver, void *device_context,
dev_mode_t mode, recnum_t recnum,
vm_size_t bytes_wanted, void **data,
vm_size_t *bytes_read);
static kern_return_t null_device_write(usrd_driver_t driver, void *device_context,
dev_mode_t mode, recnum_t recnum,
void *data, vm_size_t bytes_to_write,
vm_size_t *bytes_written);
static kern_return_t null_device_get_status(usrd_driver_t driver, void *device_context,
dev_flavor_t flavor, dev_status_t status,
natural_t *status_count);
static struct usrd_driver_callbacks null_callbacks = {
.driver_init = null_driver_init,
.driver_shutdown = null_driver_shutdown,
.device_open = null_device_open,
.device_close = null_device_close,
.device_read = null_device_read,
.device_write = null_device_write,
.device_get_status = null_device_get_status,
};
static kern_return_t null_driver_init(usrd_driver_t driver)
{
usrd_log(driver, USRD_LOG_INFO, "Null driver initializing");
USRD_CHECK_RETURN(usrd_driver_add_device(driver, "null", USRD_CAP_CHAR_DEVICE, NULL));
usrd_log(driver, USRD_LOG_INFO, "Null driver initialized successfully");
return KERN_SUCCESS;
}
static void null_driver_shutdown(usrd_driver_t driver)
{
usrd_log(driver, USRD_LOG_INFO, "Null driver shutting down");
usrd_driver_remove_device(driver, "null");
usrd_log(driver, USRD_LOG_INFO, "Null driver shutdown complete");
}
static kern_return_t null_device_open(usrd_driver_t driver, const char *device_name,
dev_mode_t mode, void **device_context)
{
struct null_device_context *ctx;
usrd_log(driver, USRD_LOG_INFO, "Opening device '%s' with mode 0x%x",
device_name, mode);
ctx = (struct null_device_context *)usrd_malloc(driver, sizeof(*ctx));
if (!ctx) {
usrd_log(driver, USRD_LOG_ERROR, "Failed to allocate device context");
return KERN_RESOURCE_SHORTAGE;
}
strncpy(ctx->device_name, device_name, sizeof(ctx->device_name) - 1);
ctx->device_name[sizeof(ctx->device_name) - 1] = '\0';
ctx->is_open = TRUE;
ctx->open_count = 1;
ctx->total_reads = 0;
ctx->total_writes = 0;
ctx->bytes_read = 0;
ctx->bytes_written = 0;
*device_context = ctx;
usrd_log(driver, USRD_LOG_INFO, "Device '%s' opened successfully", device_name);
return KERN_SUCCESS;
}
static kern_return_t null_device_close(usrd_driver_t driver, void *device_context)
{
struct null_device_context *ctx = (struct null_device_context *)device_context;
if (!ctx) {
return KERN_INVALID_ARGUMENT;
}
usrd_log(driver, USRD_LOG_INFO,
"Closing device '%s' (reads: %u, writes: %u, bytes read: %u, bytes written: %u)",
ctx->device_name, ctx->total_reads, ctx->total_writes,
ctx->bytes_read, ctx->bytes_written);
ctx->is_open = FALSE;
ctx->open_count = 0;
usrd_free(driver, ctx, sizeof(*ctx));
return KERN_SUCCESS;
}
static kern_return_t null_device_read(usrd_driver_t driver, void *device_context,
dev_mode_t mode, recnum_t recnum,
vm_size_t bytes_wanted, void **data,
vm_size_t *bytes_read)
{
struct null_device_context *ctx = (struct null_device_context *)device_context;
if (!ctx) {
return KERN_INVALID_ARGUMENT;
}
usrd_log(driver, USRD_LOG_DEBUG,
"Read request for %zu bytes from device '%s' at record %u",
bytes_wanted, ctx->device_name, recnum);
*data = NULL;
*bytes_read = 0;
ctx->total_reads++;
usrd_log(driver, USRD_LOG_DEBUG, "Read completed: 0 bytes returned");
return KERN_SUCCESS;
}
static kern_return_t null_device_write(usrd_driver_t driver, void *device_context,
dev_mode_t mode, recnum_t recnum,
void *data, vm_size_t bytes_to_write,
vm_size_t *bytes_written)
{
struct null_device_context *ctx = (struct null_device_context *)device_context;
if (!ctx) {
return KERN_INVALID_ARGUMENT;
}
usrd_log(driver, USRD_LOG_DEBUG,
"Write request for %zu bytes to device '%s' at record %u",
bytes_to_write, ctx->device_name, recnum);
*bytes_written = bytes_to_write;
ctx->total_writes++;
ctx->bytes_written += bytes_to_write;
usrd_log(driver, USRD_LOG_DEBUG, "Write completed: %zu bytes discarded",
bytes_to_write);
return KERN_SUCCESS;
}
static kern_return_t null_device_get_status(usrd_driver_t driver, void *device_context,
dev_flavor_t flavor, dev_status_t status,
natural_t *status_count)
{
struct null_device_context *ctx = (struct null_device_context *)device_context;
if (!ctx || !status || !status_count) {
return KERN_INVALID_ARGUMENT;
}
usrd_log(driver, USRD_LOG_DEBUG, "Status request for device '%s', flavor %u",
ctx->device_name, flavor);
switch (flavor) {
case 0:
if (*status_count < 4) {
return KERN_INVALID_ARGUMENT;
}
status[0] = ctx->is_open ? 1 : 0;
status[1] = ctx->total_reads;
status[2] = ctx->total_writes;
status[3] = ctx->bytes_written;
*status_count = 4;
break;
default:
usrd_log(driver, USRD_LOG_WARNING, "Unknown status flavor %u", flavor);
return KERN_INVALID_ARGUMENT;
}
return KERN_SUCCESS;
}
int main(int argc, char **argv)
{
usrd_driver_t driver;
kern_return_t ret;
printf("=== GNU Mach Null Device Driver ===\n");
printf("Example user-space driver implementation\n\n");
driver = usrd_driver_create("null-driver",
"Null device driver - discards writes, returns EOF on reads",
1, 0, USRD_CAP_CHAR_DEVICE, &null_callbacks);
if (!driver) {
fprintf(stderr, "Failed to create driver instance\n");
return 1;
}
printf("Driver instance created successfully\n");
USRD_SET_MEMORY_LIMIT(driver, 512);
USRD_SET_IO_LIMIT(driver, 16);
usrd_driver_set_resource_limits(driver, USRD_RESOURCE_TIMEOUT, 10000);
printf("Resource limits configured\n");
ret = usrd_driver_register(driver, mach_task_self());
if (ret != KERN_SUCCESS) {
fprintf(stderr, "Failed to register driver: %d\n", ret);
usrd_driver_destroy(driver);
return 1;
}
printf("Driver registered with kernel successfully\n");
printf("Driver is now ready to handle device operations\n");
printf("Press Ctrl+C to stop the driver\n\n");
ret = usrd_driver_run(driver);
if (ret != KERN_SUCCESS) {
fprintf(stderr, "Driver run failed: %d\n", ret);
}
printf("\nDriver stopping...\n");
usrd_driver_unregister(driver);
usrd_driver_destroy(driver);
printf("Null device driver shut down successfully\n");
return 0;
}