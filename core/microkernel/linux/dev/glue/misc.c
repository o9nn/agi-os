#include <sys/types.h>
#include <mach/vm_param.h>
#include <kern/thread.h>
#include <kern/printf.h>
#include <kern/mach_host.server.h>
#include <vm/vm_map.h>
#include <vm/vm_page.h>
#include <device/device_types.h>
#define MACH_INCLUDE
#include <linux/types.h>
#include <linux/config.h>
#include <linux/errno.h>
#include <linux/mm.h>
#include <linux/fs.h>
#include <linux/blk.h>
#include <linux/proc_fs.h>
#include <linux/kernel_stat.h>
#include <linux/dev/glue/glue.h>
int (*dispatch_scsi_info_ptr) (int ino, char *buffer, char **start,
off_t offset, int length, int inout) = 0;
struct kernel_stat kstat;
int
linux_to_mach_error (int err)
{
switch (err)
{
case 0:
return D_SUCCESS;
case -EPERM:
return D_INVALID_OPERATION;
case -EIO:
return D_IO_ERROR;
case -ENXIO:
return D_NO_SUCH_DEVICE;
case -EACCES:
return D_INVALID_OPERATION;
case -EFAULT:
return D_INVALID_SIZE;
case -EBUSY:
return D_ALREADY_OPEN;
case -EINVAL:
return D_INVALID_SIZE;
case -EROFS:
return D_READ_ONLY;
case -EWOULDBLOCK:
return D_WOULD_BLOCK;
case -ENOMEM:
return D_NO_MEMORY;
default:
printf ("linux_to_mach_error: unknown code %d\n", err);
return D_IO_ERROR;
}
}
int
issig ()
{
if (!current_thread())
return 0;
return current_thread ()->wait_result != THREAD_AWAKENED;
}
int
block_fsync (struct inode *inode, struct file *filp)
{
return 0;
}
int
verify_area (int rw, const void *p, unsigned long size)
{
vm_prot_t prot = (rw == VERIFY_WRITE) ? VM_PROT_WRITE : VM_PROT_READ;
vm_offset_t addr = trunc_page ((vm_offset_t) p);
vm_size_t len = round_page ((vm_size_t) size);
vm_map_entry_t entry;
vm_map_lock_read (current_map ());
while (1)
{
if (!vm_map_lookup_entry (current_map (), addr, &entry)
|| (entry->protection & prot) != prot)
{
vm_map_unlock_read (current_map ());
return -EFAULT;
}
if (entry->vme_end - entry->vme_start >= len)
break;
len -= entry->vme_end - entry->vme_start;
addr += entry->vme_end - entry->vme_start;
}
vm_map_unlock_read (current_map ());
return 0;
}
char *
kdevname (kdev_t dev)
{
static char buffer[32];
linux_sprintf (buffer, "%02x:%02x", MAJOR (dev), MINOR (dev));
return buffer;
}
static long ro_bits[MAX_BLKDEV][8];
int
is_read_only (kdev_t dev)
{
int minor, major;
major = MAJOR (dev);
minor = MINOR (dev);
if (major < 0 || major >= MAX_BLKDEV)
return 0;
return ro_bits[major][minor >> 5] & (1 << (minor & 31));
}
void
set_device_ro (kdev_t dev, int flag)
{
int minor, major;
major = MAJOR (dev);
minor = MINOR (dev);
if (major < 0 || major >= MAX_BLKDEV)
return;
if (flag)
ro_bits[major][minor >> 5] |= 1 << (minor & 31);
else
ro_bits[major][minor >> 5] &= ~(1 << (minor & 31));
}
struct proc_dir_entry proc_scsi;
struct inode_operations proc_scsi_inode_operations;
struct proc_dir_entry proc_net;
struct inode_operations proc_net_inode_operations;
int
proc_register (struct proc_dir_entry *xxx1, struct proc_dir_entry *xxx2)
{
return 0;
}
int
proc_unregister (struct proc_dir_entry *xxx1, int xxx2)
{
return 0;
}
void
add_blkdev_randomness (int major)
{
}
void
do_gettimeofday (struct timeval *tv)
{
time_value64_t tv64;
host_get_time64 ((host_t) 1, &tv64);
tv->tv_sec = tv64.seconds;
tv->tv_usec = tv64.nanoseconds / 1000;
}
int
dev_get_info (char *buffer, char **start, off_t offset, int length, int dummy)
{
return 0;
}