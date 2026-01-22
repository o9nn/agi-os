#include <stdio.h>
#include <string.h>
#include <malloc.h>
#include <sys/mman.h>
#include <mach.h>
#define page_aligned(addr) (((size_t) addr & (vm_page_size - 1)) == 0)
#include "store.h"
static error_t
copy_read (struct store *store, store_offset_t addr, size_t index,
size_t amount, void **buf, size_t *len)
{
char *data = store->hook + (addr * store->block_size);
if (page_aligned (data) && page_aligned (amount))
{
error_t err;
mach_msg_type_number_t nread;
err = vm_read (mach_task_self (),
(vm_address_t) data, amount,
(pointer_t *) buf, &nread);
if (err)
return err;
*len = nread;
return 0;
}
if (*len < amount)
{
*buf = mmap (0, amount, PROT_READ|PROT_WRITE, MAP_ANON, 0, 0);
if (*buf == MAP_FAILED)
return errno;
}
memcpy (*buf, data, amount);
*len = amount;
return 0;
}
static error_t
copy_write (struct store *store,
store_offset_t addr, size_t index,
const void *buf, size_t len, size_t *amount)
{
char *data = store->hook + (addr * store->block_size);
if (page_aligned (data) && page_aligned (len) && page_aligned (buf))
{
error_t err = vm_write (mach_task_self (),
(vm_address_t) data, (vm_address_t) buf, len);
*amount = len;
return err;
}
memcpy (data, buf, len);
*amount = len;
return 0;
}
static error_t
copy_set_size (struct store *store, size_t newsize)
{
return EOPNOTSUPP;
}
error_t
copy_allocate_encoding (const struct store *store, struct store_enc *enc)
{
return EOPNOTSUPP;
}
error_t
copy_encode (const struct store *store, struct store_enc *enc)
{
return EOPNOTSUPP;
}
static error_t
copy_decode (struct store_enc *enc, const struct store_class *const *classes,
struct store **store)
{
return EOPNOTSUPP;
}
static error_t
copy_open (const char *name, int flags,
const struct store_class *const *classes,
struct store **store)
{
return store_copy_open (name, flags, classes, store);
}
static error_t
copy_set_flags (struct store *store, int flags)
{
if ((flags & ~(STORE_INACTIVE | STORE_ENFORCED)) != 0)
return EINVAL;
store->flags |= flags;
return 0;
}
static error_t
copy_clear_flags (struct store *store, int flags)
{
error_t err = 0;
if ((flags & ~(STORE_INACTIVE | STORE_ENFORCED)) != 0)
err = EINVAL;
if (! err)
store->flags &= ~flags;
return err;
}
void
copy_cleanup (struct store *store)
{
if (store->size > 0)
munmap (store->hook, store->size);
}
error_t
copy_clone (const struct store *from, struct store *to)
{
void *buf;
buf = mmap (0, to->size, PROT_READ|PROT_WRITE, MAP_ANON, 0, 0);
if (buf != (void *) -1)
{
to->hook = buf;
memcpy (to->hook, from->hook, from->size);
return 0;
}
return errno;
}
const struct store_class
store_copy_class =
{
STORAGE_COPY, "copy", copy_read, copy_write, copy_set_size,
copy_allocate_encoding, copy_encode, copy_decode,
copy_set_flags, copy_clear_flags, copy_cleanup, copy_clone, 0, copy_open
};
STORE_STD_CLASS (copy);
error_t
store_copy_create (struct store *from, int flags, struct store **store)
{
error_t err;
struct store_run run;
run.start = 0;
run.length = from->size;
flags |= STORE_ENFORCED;
err =
_store_create (&store_copy_class,
MACH_PORT_NULL, flags, from->block_size, &run, 1, 0,
store);
if (! err)
{
size_t buf_len = 0;
err = store_read (from, 0, from->size, &(*store)->hook, &buf_len);
if (! err)
{
if (from->name)
{
size_t len =
strlen (from->class->name) + 1 + strlen (from->name) + 1;
(*store)->name = malloc (len);
if ((*store)->name)
snprintf ((*store)->name, len,
"%s:%s", from->class->name, from->name);
}
else
(*store)->name = strdup (from->class->name);
if (! (*store)->name)
err = ENOMEM;
}
if (err)
store_free (*store);
}
return err;
}
error_t
store_buffer_create (void *buf, size_t buf_len, int flags,
struct store **store)
{
error_t err;
struct store_run run;
run.start = 0;
run.length = buf_len;
flags |= STORE_ENFORCED;
err =
_store_create (&store_copy_class,
MACH_PORT_NULL, flags, 1, &run, 1, 0, store);
if (! err)
(*store)->hook = buf;
return err;
}
error_t
store_copy_open (const char *name, int flags,
const struct store_class *const *classes,
struct store **store)
{
struct store *from;
error_t err =
store_typed_open (name, flags | STORE_HARD_READONLY, classes, &from);
if (! err)
{
err = store_copy_create (from, flags, store);
if (err)
store_free (from);
}
return err;
}