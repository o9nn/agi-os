#include "store.h"
#include <string.h>
#include <stdlib.h>
error_t
store_url_open (const char *name, int flags,
const struct store_class *const *classes,
struct store **store)
{
if (name == 0 || name[0] == ':' || strchr (name, ':') == 0)
return EINVAL;
return store_typed_open (name, flags, classes, store);
}
error_t
store_url_decode (struct store_enc *enc,
const struct store_class *const *classes,
struct store **store)
{
const struct store_class *cl;
struct store dummy, *dummyptr;
error_t dummy_create (mach_port_t port, int flags, size_t block_size,
const struct store_run *runs, size_t num_runs,
struct store **store)
{
*store = &dummy;
return 0;
}
struct store_enc dummy_enc = *enc;
error_t err = store_std_leaf_decode (&dummy_enc, &dummy_create, &dummyptr);
if (err)
return err;
cl = store_find_class (dummy.name, strchr (dummy.name, ':'), classes);
# pragma weak store_module_find_class
if (cl == 0 && store_module_find_class)
err = store_module_find_class (dummy.name, strchr (dummy.name, ':'),
&cl);
free (dummy.name);
free (dummy.misc);
if (cl == 0)
return EINVAL;
return (!cl->decode ? EOPNOTSUPP : (*cl->decode) (enc, classes, store));
}
const struct store_class store_url_open_class =
{
STORAGE_NETWORK, "url",
open: store_url_open,
decode: store_url_decode
};
STORE_STD_CLASS (url_open);