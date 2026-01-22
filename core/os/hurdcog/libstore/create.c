#include <hurd/fs.h>
#include "store.h"
error_t
store_create (file_t source, int flags,
const struct store_class *const *classes,
struct store **store)
{
error_t err;
struct store_enc enc;
mach_port_t inline_ports[10];
int inline_ints[60];
off_t inline_offsets[60];
char inline_data[100];
store_enc_init (&enc, inline_ports, 10, inline_ints, 60,
inline_offsets, 60, inline_data, 100);
err = file_get_storage_info (source,
&enc.ports, &enc.num_ports,
&enc.ints, &enc.num_ints,
&enc.offsets, &enc.num_offsets,
&enc.data, &enc.data_len);
if (err)
return err;
err = store_decode (&enc, classes, store);
if (! err)
{
if (flags & STORE_INACTIVE)
flags &= ~STORE_INACTIVE;
else if ((*store)->flags & STORE_INACTIVE)
err = store_clear_flags (*store, STORE_INACTIVE);
if (!err && flags)
err = store_set_flags (*store, flags);
if (err)
store_free (*store);
}
else if (err == EINVAL && (flags &~ STORE_INACTIVE) == STORE_NO_FILEIO)
err = store_unknown_decode (&enc, classes, store);
store_enc_dealloc (&enc);
if (! err)
(*store)->source = source;
return err;
}