#include <device/io_req.h>
#include <device/modern.h>
#include <device/ds_routines.h>
#include <kern/printf.h>
#include <kern/debug.h>
#include <string.h>
#ifndef ULONG_MAX
#define ULONG_MAX (~0UL)
#endif
#define IO_VALIDATE_BASIC 0x01
#define IO_VALIDATE_BOUNDS 0x02
#define IO_VALIDATE_DEVICE 0x04
#define IO_VALIDATE_TIMEOUT 0x08
#define IO_VALIDATE_ALL 0xFF
struct io_req_validated {
struct io_req basic;
unsigned int validation_flags;
unsigned int submission_time;
unsigned int timeout_ms;
unsigned int retry_count;
unsigned int max_retries;
unsigned int safety_cookie;
void *original_data;
vm_size_t original_size;
unsigned int start_time_us;
unsigned int completion_time_us;
};
typedef struct io_req_validated *io_req_validated_t;
#define IO_REQ_VALIDATION_COOKIE 0x494F5245
#define IO_MAX_DATA_SIZE (32 * 1024 * 1024)
#define IO_MAX_TIMEOUT_MS (60 * 1000)
#define IO_MAX_RETRIES 5
static boolean_t
io_req_validate_basic(io_req_t ior)
{
if (ior == IO_REQ_NULL) {
printf("io_req_validate_basic: NULL I/O request\n");
return FALSE;
}
if ((ior->io_op & ~(IO_READ | IO_WRITE | IO_OPEN | IO_DONE | IO_ERROR |
IO_BUSY | IO_WANTED | IO_BAD | IO_CALL | IO_INBAND | IO_INTERNAL)) != 0) {
printf("io_req_validate_basic: invalid operation flags 0x%x\n", ior->io_op);
return FALSE;
}
if (ior->io_count > IO_MAX_DATA_SIZE) {
printf("io_req_validate_basic: excessive data size %lu\n", ior->io_count);
return FALSE;
}
if (ior->io_count < 0) {
printf("io_req_validate_basic: negative I/O count %ld\n", ior->io_count);
return FALSE;
}
return TRUE;
}
static boolean_t
io_req_validate_bounds(io_req_t ior)
{
if (ior->io_count == 0) {
return TRUE;
}
if (ior->io_data && ((vm_offset_t)ior->io_data & 0x3)) {
}
if (ior->io_recnum > 0 && ior->io_count > 0 &&
ior->io_recnum > (ULONG_MAX - ior->io_count)) {
printf("io_req_validate_bounds: overflow in offset calculation\n");
return FALSE;
}
if (ior->io_op & IO_INBAND) {
if (ior->io_count > sizeof(io_buf_ptr_inband_t)) {
printf("io_req_validate_bounds: inband data too large %ld\n", ior->io_count);
return FALSE;
}
}
return TRUE;
}
static boolean_t
io_req_validate_device(io_req_t ior, mach_device_t device)
{
if (device == MACH_DEVICE_NULL) {
printf("io_req_validate_device: NULL device\n");
return FALSE;
}
simple_lock(&device->lock);
boolean_t valid = (device->state == DEV_STATE_OPEN);
simple_unlock(&device->lock);
if (!valid) {
printf("io_req_validate_device: device not in open state (state=%d)\n", device->state);
return FALSE;
}
if (dev_is_modern(device)) {
mach_device_modern_t modern_device = (mach_device_modern_t)device;
if (!dev_validate_modern_device(modern_device)) {
printf("io_req_validate_device: modern device validation failed\n");
return FALSE;
}
if (!dev_check_resource_limits(modern_device, DEV_RESOURCE_IO_OPS, 1)) {
printf("io_req_validate_device: I/O operation would exceed limits\n");
return FALSE;
}
if (modern_device->modern_ops->health.state == DEV_STATE_ERROR) {
printf("io_req_validate_device: device in error state\n");
return FALSE;
}
}
return TRUE;
}
io_req_validated_t
io_req_create_validated(io_req_t original_ior, unsigned int validation_flags,
unsigned int timeout_ms)
{
io_req_validated_t validated_ior;
validated_ior = (io_req_validated_t)kalloc(sizeof(struct io_req_validated));
if (validated_ior == NULL) {
printf("io_req_create_validated: allocation failed\n");
return NULL;
}
validated_ior->basic = *original_ior;
validated_ior->validation_flags = validation_flags;
validated_ior->submission_time = (unsigned int)time.seconds;
validated_ior->timeout_ms = (timeout_ms > 0) ? timeout_ms : IO_MAX_TIMEOUT_MS;
validated_ior->retry_count = 0;
validated_ior->max_retries = IO_MAX_RETRIES;
validated_ior->safety_cookie = IO_REQ_VALIDATION_COOKIE;
validated_ior->original_data = original_ior->io_data;
validated_ior->original_size = original_ior->io_count;
validated_ior->start_time_us = 0;
validated_ior->completion_time_us = 0;
return validated_ior;
}
kern_return_t
io_req_validate(io_req_t ior, mach_device_t device, unsigned int validation_flags)
{
if (validation_flags & IO_VALIDATE_BASIC) {
if (!io_req_validate_basic(ior)) {
return KERN_INVALID_ARGUMENT;
}
}
if (validation_flags & IO_VALIDATE_BOUNDS) {
if (!io_req_validate_bounds(ior)) {
return KERN_INVALID_ADDRESS;
}
}
if (validation_flags & IO_VALIDATE_DEVICE) {
if (!io_req_validate_device(ior, device)) {
return KERN_INVALID_CAPABILITY;
}
}
return KERN_SUCCESS;
}
kern_return_t
device_io_validated(mach_device_t device, io_req_t ior, unsigned int validation_flags)
{
kern_return_t result;
mach_device_modern_t modern_device = NULL;
unsigned int start_time;
result = io_req_validate(ior, device, validation_flags);
if (result != KERN_SUCCESS) {
printf("device_io_validated: validation failed with result %d\n", result);
return result;
}
if (dev_is_modern(device)) {
modern_device = (mach_device_modern_t)device;
dev_update_resource_usage(modern_device, DEV_RESOURCE_IO_OPS, 1);
simple_lock(&modern_device->safety_lock);
modern_device->total_operations++;
simple_unlock(&modern_device->safety_lock);
start_time = (unsigned int)(time.seconds * 1000000 + time.microseconds);
}
if (ior->io_op & IO_READ) {
result = (device->dev_ops->d_read)(device->dev_number, ior);
} else if (ior->io_op & IO_WRITE) {
result = (device->dev_ops->d_write)(device->dev_number, ior);
} else {
printf("device_io_validated: unsupported operation 0x%x\n", ior->io_op);
result = KERN_INVALID_ARGUMENT;
}
if (modern_device) {
simple_lock(&modern_device->safety_lock);
if (result != KERN_SUCCESS) {
modern_device->failed_operations++;
}
dev_update_resource_usage(modern_device, DEV_RESOURCE_IO_OPS, -1);
if (result == KERN_SUCCESS) {
unsigned int end_time = (unsigned int)(time.seconds * 1000000 + time.microseconds);
unsigned int response_time = end_time - start_time;
if (modern_device->avg_response_time == 0) {
modern_device->avg_response_time = response_time;
} else {
modern_device->avg_response_time =
(modern_device->avg_response_time * 7 + response_time) / 8;
}
}
simple_unlock(&modern_device->safety_lock);
}
return result;
}
void
io_req_timeout_check(void)
{
}
void
io_req_validated_cleanup(io_req_validated_t validated_ior)
{
if (validated_ior == NULL) {
return;
}
if (validated_ior->safety_cookie != IO_REQ_VALIDATION_COOKIE) {
printf("io_req_validated_cleanup: invalid safety cookie 0x%x\n",
validated_ior->safety_cookie);
return;
}
validated_ior->safety_cookie = 0;
memset(validated_ior, 0, sizeof(struct io_req_validated));
kfree((vm_offset_t)validated_ior, sizeof(struct io_req_validated));
}