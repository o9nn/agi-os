#include <string.h>
#include <sys/mman.h>
#include "store.h"
static inline store_offset_t
store_find_first_run (struct store *store, store_offset_t addr,
struct store_run **run, struct store_run **runs_end,
store_offset_t *base, size_t *index)
{
struct store_run *tail = store->runs, *tail_end = tail + store->num_runs;
store_offset_t wrap_src = store->wrap_src;
if (addr >= wrap_src && addr < store->end)
{
*base = addr / store->wrap_dst;
addr %= wrap_src;
}
else
*base = 0;
while (tail < tail_end)
{
store_offset_t run_blocks = tail->length;
if (run_blocks > addr)
{
*run = tail;
*runs_end = tail_end;
*index = tail - store->runs;
return addr;
}
addr -= run_blocks;
tail++;
}
return -1;
}
static inline int
store_next_run (struct store *store, struct store_run *runs_end,
struct store_run **run, store_offset_t *base, size_t *index)
{
(*run)++;
(*index)++;
if (*run == runs_end)
{
*run = store->runs;
*base += store->wrap_dst;
*index = 0;
return (*base < store->end);
}
else
return 1;
}
error_t
store_write (struct store *store,
store_offset_t addr, const void *buf, size_t len, size_t *amount)
{
error_t err;
size_t index;
store_offset_t base;
struct store_run *run, *runs_end;
int block_shift = store->log2_block_size;
store_write_meth_t write = store->class->write;
if (store->flags & STORE_READONLY)
return EROFS;
if ((addr << block_shift) + len > store->size)
return EIO;
if (store->block_size != 0 && (len & (store->block_size - 1)) != 0)
return EINVAL;
addr = store_find_first_run (store, addr, &run, &runs_end, &base, &index);
if (addr < 0)
err = EIO;
else if ((len >> block_shift) <= run->length - addr)
err = (*write)(store, base + run->start + addr, index, buf, len, amount);
else
{
vm_size_t try, written;
try = (run->length - addr) << block_shift;
err = (*write) (store, base + run->start + addr, index, buf, try,
&written);
if (!err && written == try)
{
buf += written;
len -= written;
while (store_next_run (store, runs_end, &run, &base, &index)
&& run->start >= 0)
{
vm_size_t seg_written;
if ((len >> block_shift) <= run->length)
try = len;
else
try = run->length << block_shift;
err = (*write)(store, base + run->start, index, buf, try,
&seg_written);
if (err)
break;
written += seg_written;
if (seg_written < try)
break;
len -= seg_written;
if (len == 0)
break;
buf += seg_written;
}
}
*amount = written;
}
return err;
}
error_t
store_read (struct store *store,
store_offset_t addr, size_t amount, void **buf, size_t *len)
{
size_t index;
store_offset_t base;
struct store_run *run, *runs_end;
int block_shift = store->log2_block_size;
store_read_meth_t read = store->class->read;
addr = store_find_first_run (store, addr, &run, &runs_end, &base, &index);
if (addr < 0 || run->start < 0)
return EIO;
if ((addr << block_shift) + amount > store->size)
amount = store->size - (addr << block_shift);
if (store->block_size != 0 && (amount & (store->block_size - 1)) != 0)
return EINVAL;
if ((amount >> block_shift) <= run->length - addr)
return (*read) (store, base + run->start + addr, index, amount, buf, len);
else
{
error_t err;
int all;
void *whole_buf = *buf, *buf_end;
size_t whole_buf_len = *len;
inline error_t seg_read (store_offset_t addr, size_t len, int *all)
{
void *seg_buf = buf_end;
size_t seg_buf_len = len;
error_t err =
(*read)(store, addr, index, len, &seg_buf, &seg_buf_len);
if (!err)
{
if (seg_buf != buf_end)
{
memcpy (buf_end, seg_buf, seg_buf_len);
munmap (seg_buf, seg_buf_len);
}
buf_end += seg_buf_len;
amount -= seg_buf_len;
*all = (seg_buf_len == len);
}
return err;
}
if (whole_buf_len < amount)
{
whole_buf_len = amount;
whole_buf = mmap (0, amount, PROT_READ|PROT_WRITE, MAP_ANON, 0, 0);
if (whole_buf == (void *) -1)
return errno;
}
buf_end = whole_buf;
err = seg_read (base + run->start + addr,
(run->length - addr) << block_shift, &all);
while (!err && all && amount > 0
&& store_next_run (store, runs_end, &run, &base, &index))
{
if (run->start < 0)
break;
else
err = seg_read (base + run->start,
(amount >> block_shift) <= run->length
? amount
: (run->length << block_shift),
&all);
}
*len = buf_end - whole_buf;
if (*len > 0)
err = 0;
if (whole_buf != *buf)
{
if (err)
munmap (whole_buf, whole_buf_len);
else
{
vm_size_t unused = whole_buf_len - round_page (*len);
if (unused)
munmap (whole_buf + whole_buf_len - unused, unused);
*buf = whole_buf;
}
}
return err;
}
}
error_t
store_set_size (struct store *store, size_t newsize)
{
error_t err;
store_set_size_meth_t set_size = store->class->set_size;
err = (* set_size) (store, newsize);
return err;
}