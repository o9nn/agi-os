#define _SCSI_SYMS_VER_
#define __NO_VERSION__
#include <linux/module.h>
#include <linux/fs.h>
#include <linux/genhd.h>
#include <linux/kernel.h>
#include <linux/blk.h>
#include <asm/unaligned.h>
#include "scsi.h"
#include "hosts.h"
#include "sd.h"
#include <scsi/scsicam.h>
static int setsize(unsigned long capacity,unsigned int *cyls,unsigned int *hds,
unsigned int *secs);
int scsicam_bios_param (Disk *disk,
kdev_t dev,
int *ip ) {
struct buffer_head *bh;
int ret_code;
int size = disk->capacity;
unsigned long temp_cyl;
if (!(bh = bread(MKDEV(MAJOR(dev), MINOR(dev)&~0xf), 0, 1024)))
return -1;
ret_code = scsi_partsize (bh, (unsigned long) size, (unsigned int *) ip + 2,
(unsigned int *) ip + 0, (unsigned int *) ip + 1);
brelse (bh);
if (ret_code == -1) {
ret_code = setsize ((unsigned long) size, (unsigned int *) ip + 2,
(unsigned int *) ip + 0, (unsigned int *) ip + 1);
}
if (ret_code || ip[0] > 255 || ip[1] > 63) {
ip[0] = 64;
ip[1] = 32;
temp_cyl = size / (ip[0] * ip[1]);
if (temp_cyl > 65534) {
ip[0] = 255;
ip[1] = 63;
}
ip[2] = size / (ip[0] * ip[1]);
}
return 0;
}
int scsi_partsize(struct buffer_head *bh, unsigned long capacity,
unsigned int *cyls, unsigned int *hds, unsigned int *secs) {
struct partition *p, *largest = NULL;
int i, largest_cyl;
int cyl, ext_cyl, end_head, end_cyl, end_sector;
unsigned int logical_end, physical_end, ext_physical_end;
if (*(unsigned short *) (bh->b_data+510) == 0xAA55) {
for (largest_cyl = -1, p = (struct partition *)
(0x1BE + bh->b_data), i = 0; i < 4; ++i, ++p) {
if (!p->sys_ind)
continue;
#ifdef DEBUG
printk ("scsicam_bios_param : partition %d has system \n",
i);
#endif
cyl = p->cyl + ((p->sector & 0xc0) << 2);
if (cyl > largest_cyl) {
largest_cyl = cyl;
largest = p;
}
}
}
if (largest) {
end_cyl = largest->end_cyl + ((largest->end_sector & 0xc0) << 2);
end_head = largest->end_head;
end_sector = largest->end_sector & 0x3f;
if( end_head + 1 == 0 || end_sector == 0 ) return -1;
#ifdef DEBUG
printk ("scsicam_bios_param : end at h = %d, c = %d, s = %d\n",
end_head, end_cyl, end_sector);
#endif
physical_end = end_cyl * (end_head + 1) * end_sector +
end_head * end_sector + end_sector;
logical_end = get_unaligned(&largest->start_sect)
+ get_unaligned(&largest->nr_sects);
ext_cyl= (logical_end-(end_head * end_sector + end_sector))
/(end_head + 1) / end_sector;
ext_physical_end = ext_cyl * (end_head + 1) * end_sector +
end_head * end_sector + end_sector;
#ifdef DEBUG
printk("scsicam_bios_param : logical_end=%d physical_end=%d ext_physical_end=%d ext_cyl=%d\n"
,logical_end,physical_end,ext_physical_end,ext_cyl);
#endif
if ((logical_end == physical_end) ||
(end_cyl==1023 && ext_physical_end==logical_end)) {
*secs = end_sector;
*hds = end_head + 1;
*cyls = capacity / ((end_head + 1) * end_sector);
return 0;
}
#ifdef DEBUG
printk ("scsicam_bios_param : logical (%u) != physical (%u)\n",
logical_end, physical_end);
#endif
}
return -1;
}
static int setsize(unsigned long capacity,unsigned int *cyls,unsigned int *hds,
unsigned int *secs) {
unsigned int rv = 0;
unsigned long heads, sectors, cylinders, temp;
cylinders = 1024L;
sectors = 62L;
temp = cylinders * sectors;
heads = capacity / temp;
if (capacity % temp) {
heads++;
temp = cylinders * heads;
sectors = capacity / temp;
if (capacity % temp) {
sectors++;
temp = heads * sectors;
cylinders = capacity / temp;
}
}
if (cylinders == 0) rv=(unsigned)-1;
*cyls = (unsigned int) cylinders;
*secs = (unsigned int) sectors;
*hds = (unsigned int) heads;
return(rv);
}