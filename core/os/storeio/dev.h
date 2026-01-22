#ifndef __DEV_H__
#define __DEV_H__
#include <mach.h>
#include <device/device.h>
#include <pthread.h>
#include <hurd/store.h>
#include <hurd/trivfs.h>
extern struct trivfs_control *storeio_fsys;
struct dev
{
struct store_parsed *store_name;
struct store *store;
int readonly;
int enforced;
int no_fileio;
dev_t rdev;
pid_t owner;
int nperopens;
pthread_mutex_t lock;
int inhibit_cache;
unsigned block_mask;
pthread_rwlock_t io_lock;
void *buf;
off_t buf_offs;
int buf_dirty;
struct pager *pager;
pthread_mutex_t pager_lock;
};
static inline int
dev_is_readonly (const struct dev *dev)
{
return dev->readonly || (dev->store && (dev->store->flags & STORE_READONLY));
}
error_t dev_open (struct dev *dev);
void dev_close (struct dev *dev);
error_t dev_get_memory_object(struct dev *dev, vm_prot_t prot,
memory_object_t *memobj);
int dev_stop_paging (struct dev *dev, int nosync);
error_t dev_sync (struct dev *dev, int wait);
error_t dev_write (struct dev *dev, off_t offs, const void *buf, size_t len,
size_t *amount);
error_t dev_read (struct dev *dev, off_t offs, size_t amount,
void **buf, size_t *len);
#endif