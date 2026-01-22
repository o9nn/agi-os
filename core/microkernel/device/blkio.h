#ifndef _DEVICE_BLKIO_H_
#define _DEVICE_BLKIO_H_
#include <sys/types.h>
extern vm_offset_t block_io_mmap(dev_t dev, vm_offset_t off, int prot);
#endif