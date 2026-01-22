#ifndef _file_io_h
#define _file_io_h 1
#include <sys/types.h>
#include <device/device_types.h>
#include <device/device.h>
struct storage_run
{
recnum_t start, length;
};
struct file_direct
{
mach_port_t device;
int bshift;
size_t fd_bsize;
recnum_t fd_size;
size_t nruns;
struct storage_run runs[0];
};
int page_read_file_direct (struct file_direct *fdp,
vm_offset_t offset,
vm_size_t size,
vm_offset_t *addr,
mach_msg_type_number_t *size_read);
int page_write_file_direct(struct file_direct *fdp,
vm_offset_t offset,
vm_offset_t addr,
vm_size_t size,
mach_msg_type_number_t *size_written);
#endif