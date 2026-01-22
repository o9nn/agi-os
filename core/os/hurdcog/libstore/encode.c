#include <string.h>
#include <sys/mman.h>
#include "store.h"
error_t
store_std_leaf_allocate_encoding (const struct store *store,
struct store_enc *enc)
{
enc->num_ports++;
enc->num_ints += 6;
enc->num_offsets += store->num_runs * 2;
if (store->name)
enc->data_len += strlen (store->name) + 1;
enc->data_len += store->misc_len;
return 0;
}
static inline int too_big (store_offset_t ofs)
{
off_t o = (off_t) ofs;
return o < 0 || ((store_offset_t) o != ofs);
}
error_t
store_std_leaf_encode (const struct store *store, struct store_enc *enc)
{
int i;
size_t name_len = (store->name ? strlen (store->name) + 1 : 0);
enc->ports[enc->cur_port++] = store->port;
enc->ints[enc->cur_int++] = store->class->id;
enc->ints[enc->cur_int++] = store->flags;
enc->ints[enc->cur_int++] = store->block_size;
enc->ints[enc->cur_int++] = store->num_runs;
enc->ints[enc->cur_int++] = name_len;
enc->ints[enc->cur_int++] = store->misc_len;
for (i = 0; i < store->num_runs; i++)
{
if (sizeof (*enc->offsets) != sizeof (store->runs[i].start)
&& (too_big (store->runs[i].start)
|| too_big (store->runs[i].start + store->runs[i].length)))
return EOVERFLOW;
enc->offsets[enc->cur_offset++] = store->runs[i].start;
enc->offsets[enc->cur_offset++] = store->runs[i].length;
}
if (store->name)
{
bcopy (store->name, enc->data + enc->cur_data, name_len);
enc->cur_data += name_len;
}
if (store->misc_len)
{
bcopy (store->misc, enc->data + enc->cur_data, store->misc_len);
enc->cur_data += store->misc_len;
}
return 0;
}
error_t
store_encode (const struct store *store, struct store_enc *enc)
{
void *buf;
error_t err;
const struct store_class *class = store->class;
mach_msg_type_number_t init_num_ports = enc->num_ports;
mach_msg_type_number_t init_num_ints = enc->num_ints;
mach_msg_type_number_t init_num_offsets = enc->num_offsets;
mach_msg_type_number_t init_data_len = enc->data_len;
if (!class->allocate_encoding || !class->encode)
return EOPNOTSUPP;
enc->num_ports = 0;
enc->num_ints = 0;
enc->num_offsets = 0;
enc->data_len = 0;
err = (*class->allocate_encoding) (store, enc);
if (err)
return err;
errno = 0;
if (enc->num_ports > init_num_ports)
{
buf = mmap (0, enc->num_ports * sizeof *enc->ports,
PROT_READ|PROT_WRITE, MAP_ANON, 0, 0);
if (buf != (void *) -1)
enc->ports = buf;
}
if (!errno && enc->num_ints > init_num_ints)
{
buf = mmap (0, enc->num_ints * sizeof *enc->ints,
PROT_READ|PROT_WRITE, MAP_ANON, 0, 0);
if (buf != (void *) -1)
enc->ints = buf;
}
if (!errno && enc->num_offsets > init_num_offsets)
{
buf = mmap (0, enc->num_offsets * sizeof *enc->offsets,
PROT_READ|PROT_WRITE, MAP_ANON, 0, 0);
if (buf != (void *) -1)
enc->offsets = buf;
}
if (!errno && enc->data_len > init_data_len)
{
buf = mmap (0, enc->data_len, PROT_READ|PROT_WRITE, MAP_ANON, 0, 0);
if (buf != (void *) -1)
enc->data = buf;
}
err = errno;
if (! err)
err = (*class->encode) (store, enc);
enc->cur_port = enc->cur_int = enc->cur_offset = enc->cur_data = 0;
if (err)
store_enc_dealloc (enc);
return err;
}
error_t
store_return (const struct store *store,
mach_port_t **ports, mach_msg_type_number_t *num_ports,
int **ints, mach_msg_type_number_t *num_ints,
off_t **offsets, mach_msg_type_number_t *num_offsets,
char **data, mach_msg_type_number_t *data_len)
{
error_t err;
struct store_enc enc;
store_enc_init (&enc, *ports, *num_ports, *ints, *num_ints,
*offsets, *num_offsets, *data, *data_len);
err = store_encode (store, &enc);
if (err)
store_enc_dealloc (&enc);
else
store_enc_return (&enc, ports, num_ports, ints, num_ints,
offsets, num_offsets, data, data_len);
return err;
}