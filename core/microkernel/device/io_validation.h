#ifndef _DEVICE_IO_VALIDATION_H_
#define _DEVICE_IO_VALIDATION_H_
#include <device/io_req.h>
#include <device/dev_hdr.h>
#include <mach/kern_return.h>
#define IO_VALIDATE_BASIC 0x01
#define IO_VALIDATE_BOUNDS 0x02
#define IO_VALIDATE_DEVICE 0x04
#define IO_VALIDATE_TIMEOUT 0x08
#define IO_VALIDATE_ALL 0xFF
struct io_req_validated;
typedef struct io_req_validated *io_req_validated_t;
extern io_req_validated_t io_req_create_validated(io_req_t original_ior,
unsigned int validation_flags,
unsigned int timeout_ms);
extern kern_return_t io_req_validate(io_req_t ior, mach_device_t device,
unsigned int validation_flags);
extern kern_return_t device_io_validated(mach_device_t device, io_req_t ior,
unsigned int validation_flags);
extern void io_req_timeout_check(void);
extern void io_req_validated_cleanup(io_req_validated_t validated_ior);
#endif