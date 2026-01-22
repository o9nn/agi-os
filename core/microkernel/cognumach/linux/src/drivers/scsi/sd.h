#ifndef _SD_H
#define _SD_H
#ifndef _SCSI_H
#include "scsi.h"
#endif
#ifndef _GENDISK_H
#include <linux/genhd.h>
#endif
extern struct hd_struct * sd;
typedef struct scsi_disk {
unsigned capacity;
unsigned sector_size;
Scsi_Device	 *device;
unsigned char ready;
unsigned char write_prot;
unsigned char sector_bit_size;
unsigned char sector_bit_shift;
unsigned ten:1;
unsigned remap:1;
unsigned has_part_table:1;
} Scsi_Disk;
extern Scsi_Disk * rscsi_disks;
extern int revalidate_scsidisk(kdev_t dev, int maxusage);
#endif