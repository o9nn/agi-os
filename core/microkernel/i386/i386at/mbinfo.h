#ifndef _I386AT_MBINFO_H_
#define _I386AT_MBINFO_H_	1
#include <sys/types.h>
#include <vm/vm_types.h>
#include <mach/vm_prot.h>
#include <device/device_types.h>
#include <device/io_req.h>
#include <mach/machine/multiboot.h>
void mbinfo_register_boot_data(const struct multiboot_raw_info *mbi);
io_return_t mbinforead(dev_t dev, io_req_t ior);
#endif