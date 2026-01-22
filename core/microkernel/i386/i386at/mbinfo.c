#include <string.h>
#include <i386at/mbinfo.h>
#include <mach/vm_param.h>
#include <vm/pmap.h>
#include <device/ds_routines.h>
static struct multiboot_raw_info mb_info;
void
mbinfo_register_boot_data(const struct multiboot_raw_info *mbi)
{
mb_info = *mbi;
}
io_return_t
mbinforead(dev_t dev, io_req_t ior)
{
int err;
if (ior->io_count > sizeof(struct multiboot_raw_info))
return D_INVALID_SIZE;
err = device_read_alloc(ior, (vm_size_t)ior->io_count);
if (err != KERN_SUCCESS)
return (err);
memcpy ((uint8_t *)ior->io_data, &mb_info, ior->io_count);
ior->io_residual = 0;
return (D_SUCCESS);
}