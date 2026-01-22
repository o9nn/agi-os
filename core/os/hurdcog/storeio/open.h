#ifndef __OPEN_H__
#define __OPEN_H__
#include "dev.h"
struct open
{
struct dev *dev;
off_t offs;
pthread_mutex_t lock;
};
error_t open_create (struct dev *dev, struct open **open);
void open_free (struct open *open);
error_t open_write (struct open *open, off_t offs, const void *buf, size_t len,
vm_size_t *amount);
error_t open_read (struct open *open, off_t offs, vm_size_t amount,
void **buf, vm_size_t *buf_len);
error_t open_seek (struct open *open, off_t offs, int whence, off_t *new_offs);
#endif