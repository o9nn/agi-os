#ifndef SCSICAM_H
#define SCSICAM_H
#include <linux/kdev_t.h>
extern int scsicam_bios_param (Disk *disk, kdev_t dev, int *ip);
#endif