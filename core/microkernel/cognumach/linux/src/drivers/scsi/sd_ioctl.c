#include <linux/kernel.h>
#include <linux/sched.h>
#include <linux/mm.h>
#include <linux/fs.h>
#include <linux/hdreg.h>
#include <linux/errno.h>
#include <asm/segment.h>
#include <linux/blk.h>
#include "scsi.h"
#include <scsi/scsi_ioctl.h>
#include "hosts.h"
#include "sd.h"
#include <scsi/scsicam.h>
int sd_ioctl(struct inode * inode, struct file * file, unsigned int cmd, unsigned long arg)
{
kdev_t dev = inode->i_rdev;
int error;
struct Scsi_Host * host;
int diskinfo[4];
struct hd_geometry *loc = (struct hd_geometry *) arg;
switch (cmd) {
case HDIO_GETGEO:
if (!loc)  return -EINVAL;
#ifndef MACH
error = verify_area(VERIFY_WRITE, loc, sizeof(*loc));
if (error)
return error;
#endif
host = rscsi_disks[MINOR(dev) >> 4].device->host;
diskinfo[0] = 0x40;
diskinfo[1] = 0x20;
diskinfo[2] = rscsi_disks[MINOR(dev) >> 4].capacity >> 11;
if(host->hostt->bios_param != NULL)
host->hostt->bios_param(&rscsi_disks[MINOR(dev) >> 4],
dev,
&diskinfo[0]);
else scsicam_bios_param(&rscsi_disks[MINOR(dev) >> 4],
dev, &diskinfo[0]);
#ifdef MACH
loc->heads = diskinfo[0];
loc->sectors = diskinfo[1];
loc->cylinders = diskinfo[2];
loc->start = sd[MINOR(inode->i_rdev)].start_sect;
#else
put_user(diskinfo[0], &loc->heads);
put_user(diskinfo[1], &loc->sectors);
put_user(diskinfo[2], &loc->cylinders);
put_user(sd[MINOR(inode->i_rdev)].start_sect, &loc->start);
#endif
return 0;
case BLKGETSIZE:
if (!arg)  return -EINVAL;
error = verify_area(VERIFY_WRITE, (long *) arg, sizeof(long));
if (error)
return error;
put_user(sd[MINOR(inode->i_rdev)].nr_sects,
(long *) arg);
return 0;
case BLKRASET:
if (!suser())
return -EACCES;
if(!(inode->i_rdev)) return -EINVAL;
if(arg > 0xff) return -EINVAL;
read_ahead[MAJOR(inode->i_rdev)] = arg;
return 0;
case BLKRAGET:
if (!arg)
return -EINVAL;
error = verify_area(VERIFY_WRITE, (int *) arg, sizeof(int));
if (error)
return error;
put_user(read_ahead[MAJOR(inode->i_rdev)], (int *) arg);
return 0;
case BLKFLSBUF:
if(!suser())  return -EACCES;
if(!(inode->i_rdev)) return -EINVAL;
fsync_dev(inode->i_rdev);
invalidate_buffers(inode->i_rdev);
return 0;
case BLKRRPART:
return revalidate_scsidisk(dev, 1);
RO_IOCTLS(dev, arg);
default:
return scsi_ioctl(rscsi_disks[MINOR(dev) >> 4].device , cmd, (void *) arg);
}
}