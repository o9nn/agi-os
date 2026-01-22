#ifndef	_MACHDEV_MACH_DEVICE_H
#define	_MACHDEV_MACH_DEVICE_H
struct mach_device {
struct port_info port;
struct machdev_emul_device	dev;
};
typedef	struct mach_device *mach_device_t;
#define	MACH_DEVICE_NULL ((mach_device_t)0)
#endif