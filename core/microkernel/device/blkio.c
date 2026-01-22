#include <mach/kern_return.h>
#include <device/blkio.h>
#include <device/buf.h>
#include <device/param.h>
#include <device/device_types.h>
#include <device/io_req.h>
#include <device/ds_routines.h>
#define MAX_PHYS (256 * 1024)
void minphys(io_req_t ior)
{
if ((ior->io_op & (IO_WRITE | IO_READ | IO_OPEN)) == IO_WRITE)
return;
if (ior->io_count > MAX_PHYS)
ior->io_count = MAX_PHYS;
}
vm_offset_t block_io_mmap(dev_t dev, vm_offset_t off, int prot)
{
return (0);
}