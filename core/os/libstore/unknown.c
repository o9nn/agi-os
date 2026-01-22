#include "store.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
static error_t
noread (struct store *store, store_offset_t addr, size_t index,
size_t amount, void **buf, size_t *len)
{
return EFTYPE;
}
static error_t
nowrite (struct store *store,
store_offset_t addr, size_t index,
const void *buf, size_t len, size_t *amount)
{
return EFTYPE;
}
static error_t
noset_size (struct store *store, size_t newsize)
{
return EFTYPE;
}
static error_t
noflags (struct store *store, int flags)
{
return EINVAL;
}
static struct store_enc *
duplicate_encoding (struct store_enc *enc)
{
struct store_enc *us;
size_t i;
us = calloc (1, sizeof *us);
if (us == NULL)
return NULL;
us->ports = mmap (0, enc->num_ports * sizeof *enc->ports,
PROT_READ|PROT_WRITE, MAP_ANON, 0, 0);
if (us->ports == MAP_FAILED)
{
no_memory:
store_enc_dealloc (us);
free (us);
return NULL;
}
us->ints = mmap (0, enc->num_ints * sizeof *enc->ints,
PROT_READ|PROT_WRITE, MAP_ANON, 0, 0);
if (us->ints == MAP_FAILED)
goto no_memory;
us->offsets = mmap (0, enc->num_offsets * sizeof *enc->offsets,
PROT_READ|PROT_WRITE, MAP_ANON, 0, 0);
if (us->offsets == MAP_FAILED)
goto no_memory;
us->data = mmap (0, enc->data_len, PROT_READ|PROT_WRITE, MAP_ANON, 0, 0);
if (us->data == MAP_FAILED)
goto no_memory;
memcpy (us->ports, enc->ports, enc->num_ports * sizeof *enc->ports);
memcpy (us->ints, enc->ints, enc->num_ints * sizeof *enc->ints);
memcpy (us->offsets, enc->offsets, enc->num_offsets * sizeof *enc->offsets);
memcpy (us->data, enc->data, enc->data_len);
us->num_ports = enc->num_ports;
us->num_ints = enc->num_ints;
us->num_offsets = enc->num_offsets;
us->data_len = enc->data_len;
for (i = 0; i < us->num_ports; ++i)
mach_port_mod_refs (mach_task_self (), us->ports[i],
MACH_PORT_RIGHT_SEND, +1);
return us;
}
error_t
store_unknown_decode (struct store_enc *enc,
const struct store_class *const *classes,
struct store **store)
{
struct store_enc *us;
error_t err = _store_create (&store_unknown_class,
MACH_PORT_NULL, STORE_ENFORCED, 0, NULL, 0, 0,
store);
if (err)
return err;
us = duplicate_encoding (enc);
if (us == NULL)
{
store_free (*store);
return ENOMEM;
}
(*store)->hook = us;
if (enc->cur_int == enc->num_ints)
asprintf (&(*store)->name, "notype:%.*s",
(int) (us->data_len - us->cur_data), us->data + us->cur_data);
else
asprintf (&(*store)->name, "type-%d:%.*s", enc->ints[enc->cur_int],
(int) ( us->data_len - us->cur_data), us->data + us->cur_data);
return 0;
}
error_t
unknown_allocate_encoding (const struct store *store, struct store_enc *enc)
{
const struct store_enc *us = store->hook;
if (us == NULL)
return EOPNOTSUPP;
enc->num_ports += us->num_ports;
enc->num_ints += us->num_ints;
enc->num_offsets += us->num_offsets;
enc->data_len += us->data_len;
return 0;
}
error_t
unknown_encode (const struct store *store, struct store_enc *enc)
{
const struct store_enc *us = store->hook;
if (us == NULL)
return EOPNOTSUPP;
memcpy (enc->ports, us->ports, us->num_ports * sizeof enc->ports[0]);
enc->ports += us->num_ports;
memcpy (enc->ints, us->ints, us->num_ints * sizeof enc->ints[0]);
enc->ints += us->num_ints;
memcpy (enc->offsets, us->offsets, us->num_offsets * sizeof enc->offsets[0]);
enc->offsets += us->num_offsets;
memcpy (enc->data + enc->cur_data, us->data, us->data_len);
enc->cur_data += us->data_len;
return 0;
}
static void
unknown_cleanup (struct store *store)
{
if (store->hook != NULL)
{
store_enc_dealloc (store->hook);
free (store->hook);
}
}
static error_t
unknown_clone (const struct store *from, struct store *to)
{
if (from->hook == NULL)
return 0;
to->hook = duplicate_encoding (from->hook);
return to->hook ? 0 : ENOMEM;
}
static error_t
unknown_validate_name (const char *name,
const struct store_class *const *classes)
{
return name == NULL ? 0 : EINVAL;
}
static error_t
unknown_open (const char *name, int flags,
const struct store_class *const *classes,
struct store **store)
{
return (name == NULL
? _store_create (&store_unknown_class, MACH_PORT_NULL,
STORE_ENFORCED, 0, NULL, 0, 0, store)
: EINVAL);
}
const struct store_class store_unknown_class =
{
-1, "unknown",
read: noread,
write: nowrite,
set_size: noset_size,
allocate_encoding: unknown_allocate_encoding,
encode: unknown_encode,
decode: store_unknown_decode,
set_flags: noflags,
clear_flags: noflags,
cleanup: unknown_cleanup,
clone: unknown_clone,
open: unknown_open,
validate_name: unknown_validate_name,
};
STORE_STD_CLASS (unknown);