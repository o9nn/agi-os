#include <hurd.h>
#include <assert-backtrace.h>
#include <string.h>
#include <hurd/pager.h>
#include <hurd/store.h>
#include <sys/mman.h>
#include "dev.h"
static inline int
dev_buf_is_active (struct dev *dev)
{
return dev->buf_offs >= 0;
}
static error_t
dev_buf_discard (struct dev *dev)
{
if (dev_buf_is_active (dev))
{
if (dev->buf_dirty)
{
size_t amount;
struct store *store = dev->store;
error_t err =
store_write (store, dev->buf_offs >> store->log2_block_size,
dev->buf, store->block_size, &amount);
if (!err && amount < store->block_size)
err = EIO;
if (err)
return err;
dev->buf_dirty = 0;
}
dev->buf_offs = -1;
}
return 0;
}
static error_t
dev_buf_fill (struct dev *dev, off_t offs)
{
error_t err;
unsigned block_mask = dev->block_mask;
void *buf = dev->buf;
struct store *store = dev->store;
size_t buf_len = store->block_size;
if (dev_buf_is_active (dev))
{
if ((dev->buf_offs & ~block_mask) == (offs & ~block_mask))
return 0;
else
{
err = dev_buf_discard (dev);
if (err)
return err;
}
}
err = store_read (store, offs >> store->log2_block_size, store->block_size,
&buf, &buf_len);
if (err)
return err;
if (buf_len < store->block_size)
{
if (buf != dev->buf)
munmap (buf, buf_len);
return EIO;
}
if (buf != dev->buf)
{
munmap (dev->buf, store->block_size);
dev->buf = buf;
}
dev->buf_offs = offs & ~block_mask;
return 0;
}
static error_t
dev_buf_rw (struct dev *dev, size_t buf_offs, size_t *io_offs, size_t *len,
error_t (*const buf_rw) (size_t buf_offs,
size_t io_offs, size_t len))
{
size_t block_size = dev->store->block_size;
assert_backtrace (dev_buf_is_active (dev));
if (buf_offs + *len >= block_size)
{
size_t buf_len = block_size - buf_offs;
error_t err = (*buf_rw) (buf_offs, *io_offs, buf_len);
if (err)
return err;
*io_offs += buf_len;
*len -= buf_len;
return dev_buf_discard (dev);
}
else
{
error_t err = (*buf_rw) (buf_offs, *io_offs, *len);
if (err)
return err;
*io_offs += *len;
*len = 0;
return 0;
}
}
error_t
dev_open (struct dev *dev)
{
error_t err;
const int flags = ((dev->readonly ? STORE_READONLY : 0)
| (dev->no_fileio ? STORE_NO_FILEIO : 0));
assert_backtrace (dev->store == 0);
if (dev->store_name == 0)
{
err = store_create (storeio_fsys->underlying, flags, 0, &dev->store);
}
else
err = store_parsed_open (dev->store_name, flags, &dev->store);
if (err)
return err;
store_set_flags (dev->store, STORE_INACTIVE);
if (! dev->store->block_size)
dev->buf = NULL;
else
dev->buf = mmap (0, dev->store->block_size, PROT_READ|PROT_WRITE,
MAP_ANON, 0, 0);
if (dev->buf == MAP_FAILED)
{
store_free (dev->store);
dev->store = 0;
return ENOMEM;
}
if (!dev->inhibit_cache)
{
dev->buf_offs = -1;
pthread_rwlock_init (&dev->io_lock, NULL);
dev->block_mask = (1 << dev->store->log2_block_size) - 1;
dev->pager = 0;
pthread_mutex_init (&dev->pager_lock, NULL);
}
return 0;
}
void
dev_close (struct dev *dev)
{
assert_backtrace (dev->store);
if (!dev->inhibit_cache)
{
if (dev->pager != NULL)
pager_shutdown (dev->pager);
dev_buf_discard (dev);
munmap (dev->buf, dev->store->block_size);
}
store_free (dev->store);
dev->store = 0;
}
error_t
dev_sync(struct dev *dev, int wait)
{
error_t err;
if (dev->inhibit_cache)
return 0;
if (dev->pager != NULL)
pager_sync (dev->pager, wait);
pthread_rwlock_wrlock (&dev->io_lock);
err = dev_buf_discard (dev);
pthread_rwlock_unlock (&dev->io_lock);
return err;
}
static inline error_t
buffered_rw (struct dev *dev, off_t offs, size_t len, size_t *amount,
error_t (* const buf_rw) (size_t buf_offs,
size_t io_offs, size_t len),
error_t (* const raw_rw) (off_t offs,
size_t io_offs, size_t len,
size_t *amount))
{
error_t err = 0;
unsigned block_mask = dev->block_mask;
unsigned block_size = dev->store->block_size;
size_t io_offs = 0;
unsigned block_offs = offs & block_mask;
pthread_rwlock_wrlock (&dev->io_lock);
if (block_offs != 0)
{
err = dev_buf_fill (dev, offs);
if (! err)
err = dev_buf_rw (dev, block_offs, &io_offs, &len, buf_rw);
}
if (!err && len > 0)
{
if (len >= block_size)
{
size_t amount = 0;
err = dev_buf_discard (dev);
if (! err)
err =
(*raw_rw) (offs + io_offs, io_offs, len & ~block_mask, &amount);
if (! err)
{
io_offs += amount;
len -= amount;
}
}
if (len > 0 && len < block_size)
{
err = dev_buf_fill (dev, offs + io_offs);
if (! err)
err = dev_buf_rw (dev, 0, &io_offs, &len, buf_rw);
}
}
if (! err)
*amount = io_offs;
pthread_rwlock_unlock (&dev->io_lock);
return err;
}
static inline error_t
dev_rw (struct dev *dev, off_t offs, size_t len, size_t *amount,
error_t (* const buf_rw) (size_t buf_offs,
size_t io_offs, size_t len),
error_t (* const raw_rw) (off_t offs,
size_t io_offs, size_t len,
size_t *amount))
{
error_t err;
unsigned block_mask = dev->block_mask;
if (offs < 0 || offs > dev->store->size)
return EINVAL;
else if (offs + len > dev->store->size)
len = dev->store->size - offs;
pthread_rwlock_rdlock (&dev->io_lock);
if (dev_buf_is_active (dev)
|| (offs & block_mask) != 0 || (len & block_mask) != 0)
{
pthread_rwlock_unlock (&dev->io_lock);
err = buffered_rw (dev, offs, len, amount, buf_rw, raw_rw);
}
else
{
err = (*raw_rw) (offs, 0, len, amount);
pthread_rwlock_unlock (&dev->io_lock);
}
return err;
}
error_t
dev_write (struct dev *dev, off_t offs, const void *buf, size_t len,
size_t *amount)
{
error_t buf_write (size_t buf_offs, size_t io_offs, size_t len)
{
memcpy (dev->buf + buf_offs, buf + io_offs, len);
dev->buf_dirty = 1;
return 0;
}
error_t raw_write (off_t offs, size_t io_offs, size_t len, size_t *amount)
{
struct store *store = dev->store;
return
store_write (store, offs >> store->log2_block_size,
buf + io_offs, len, amount);
}
if (dev->inhibit_cache)
{
struct store *store = dev->store;
if (store->block_size == 0)
return store_write (dev->store, offs, buf, len, amount);
if ((offs & (store->block_size - 1)) != 0
|| (len & (store->block_size - 1)) != 0)
return EINVAL;
return store_write (dev->store, offs << store->log2_block_size,
buf, len, amount);
}
return dev_rw (dev, offs, len, amount, buf_write, raw_write);
}
error_t
dev_read (struct dev *dev, off_t offs, size_t whole_amount,
void **buf, size_t *len)
{
error_t err;
int allocated_buf = 0;
error_t ensure_buf (void)
{
if (*len < whole_amount)
{
void *new = mmap (0, whole_amount, PROT_READ|PROT_WRITE,
MAP_ANON, 0, 0);
if (new == (void *) -1)
return errno;
*buf = new;
allocated_buf = 1;
}
return 0;
}
error_t buf_read (size_t buf_offs, size_t io_offs, size_t len)
{
error_t err = ensure_buf ();
if (! err)
memcpy (*buf + io_offs, dev->buf + buf_offs, len);
return err;
}
error_t raw_read (off_t offs, size_t io_offs, size_t len, size_t *amount)
{
struct store *store = dev->store;
off_t addr = offs >> store->log2_block_size;
if (len == whole_amount)
return store_read (store, addr, len, buf, amount);
else
{
error_t err = ensure_buf ();
if (! err)
{
void *_req_buf = *buf + io_offs, *req_buf = _req_buf;
size_t req_len = len;
err = store_read (store, addr, len, &req_buf, &req_len);
if (! err)
{
if (req_buf != _req_buf)
{
memcpy (_req_buf, req_buf, req_len);
munmap (req_buf, req_len);
}
*amount = req_len;
}
}
return err;
}
}
if (dev->store->size > 0 && offs == dev->store->size)
{
*len = 0;
return 0;
}
if (dev->inhibit_cache)
{
struct store *store = dev->store;
if (store->block_size == 0)
return store_read (dev->store, offs, whole_amount, buf, len);
if ((offs & (store->block_size - 1)) != 0
|| (whole_amount & (store->block_size - 1)) != 0)
return EINVAL;
return store_read (dev->store, offs << store->log2_block_size,
whole_amount, buf, len);
}
err = dev_rw (dev, offs, whole_amount, len, buf_read, raw_read);
if (err && allocated_buf)
munmap (*buf, whole_amount);
return err;
}