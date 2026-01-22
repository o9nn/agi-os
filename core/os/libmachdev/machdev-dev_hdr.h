#ifndef	_MACHDEV_DEVICE_DEV_HDR_H_
#define	_MACHDEV_DEVICE_DEV_HDR_H_
#include <mach.h>
#include <hurd.h>
#include <hurd/ports.h>
#include "machdev-device_emul.h"
struct machdev_emul_device
{
struct machdev_device_emulation_ops *emul_ops;
void *emul_data;
};
typedef struct machdev_emul_device *machdev_emul_device_t;
#endif