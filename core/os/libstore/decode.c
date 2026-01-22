#include <string.h>
#include <malloc.h>
#include "store.h"
#define MAX_STACK_RUNS (16*1024 / sizeof (struct store_run))
error_t
store_std_leaf_decode (struct store_enc *enc,
store_std_leaf_create_t create,
struct store **store)
{
char *misc, *name;
error_t err;
int flags;
mach_port_t port;
size_t block_size, num_runs, name_len, misc_len;
error_t call_create (const struct store_run *runs, size_t num_runs)
{
return (*create)(port, flags, block_size, runs, num_runs, store);
}
if (enc->cur_int + 6 > enc->num_ints || enc->cur_port + 1 > enc->num_ports)
return EINVAL;
enc->cur_int++;
flags = enc->ints[enc->cur_int++];
block_size = enc->ints[enc->cur_int++];
num_runs = enc->ints[enc->cur_int++];
name_len = enc->ints[enc->cur_int++];
misc_len = enc->ints[enc->cur_int++];
if (enc->cur_offset + num_runs * 2 > enc->num_offsets
|| enc->cur_data + name_len + misc_len > enc->data_len)
return EINVAL;
if (name_len > 0 && enc->data[enc->cur_data + name_len - 1] != '\0')
return EINVAL;
if (name_len > 0)
{
name = strdup (enc->data + enc->cur_data);
if (! name)
return ENOMEM;
enc->cur_data += name_len;
}
else
name = 0;
if (misc_len > 0)
{
misc = malloc (misc_len);
if (! misc)
{
if (name)
free (name);
return ENOMEM;
}
memcpy (misc, enc->data + enc->cur_data + name_len, misc_len);
enc->cur_data += misc_len;
}
else
misc = 0;
port = enc->ports[enc->cur_port++];
err = store_with_decoded_runs (enc, num_runs, call_create);
if (err)
{
mach_port_deallocate (mach_task_self (), port);
if (misc)
free (misc);
if (name)
free (name);
}
else
{
(*store)->flags = flags;
(*store)->name = name;
(*store)->misc = misc;
(*store)->misc_len = misc_len;
}
return err;
}
error_t
store_with_decoded_runs (struct store_enc *enc, size_t num_runs,
error_t (*fun) (const struct store_run *runs,
size_t num_runs))
{
int i;
error_t err;
if (num_runs <= MAX_STACK_RUNS)
{
struct store_run runs[num_runs];
off_t *e = enc->offsets + enc->cur_offset;
for (i = 0; i < num_runs; i++)
{
runs[i].start = *e++;
runs[i].length = *e++;
}
enc->cur_offset = e - enc->offsets;
err = (*fun)(runs, num_runs);
}
else
{
struct store_run *runs = malloc (num_runs * sizeof (struct store_run));
if (runs)
{
off_t *e = enc->offsets + enc->cur_offset;
for (i = 0; i < num_runs; i++)
{
runs[i].start = *e++;
runs[i].length = *e++;
}
enc->cur_offset = e - enc->offsets;
err = (*fun) (runs, num_runs);
free (runs);
}
else
err = ENOMEM;
}
return err;
}
error_t
store_decode (struct store_enc *enc, const struct store_class *const *classes,
struct store **store)
{
const struct store_class *const *cl;
if (enc->cur_int >= enc->num_ints)
return EINVAL;
if (enc->ints[enc->cur_int] == STORAGE_NETWORK)
return store_url_decode (enc, classes, store);
for (cl = classes ?: __start_store_std_classes;
classes ? *cl != 0 : cl < __stop_store_std_classes;
++cl)
if ((*cl)->id == enc->ints[enc->cur_int])
{
if ((*cl)->decode)
return (*(*cl)->decode) (enc, classes, store);
else
return EOPNOTSUPP;
}
# pragma weak store_module_decode
if (! classes && store_module_decode)
{
error_t err = store_module_decode (enc, classes, store);
if (err != ENOENT)
return err;
}
return EINVAL;
}