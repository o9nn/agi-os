#include <malloc.h>
#include <string.h>
#include "store.h"
error_t
store_set_flags (struct store *store, int flags)
{
error_t err = 0;
int orig = store->flags, new = flags & ~orig;
if (new & STORE_BACKEND_FLAGS)
{
if (store->class->set_flags)
err = (*store->class->set_flags) (store, new);
else
err = EINVAL;
}
if (! err)
store->flags |= (new & ~STORE_BACKEND_FLAGS);
return err;
}
error_t
store_clear_flags (struct store *store, int flags)
{
error_t err = 0;
int orig = store->flags, kill = flags & orig;
if (kill & STORE_BACKEND_FLAGS)
{
if (store->class->clear_flags)
err = (*store->class->clear_flags) (store, kill);
else
err = EINVAL;
}
if (! err)
store->flags &= ~(kill & ~STORE_BACKEND_FLAGS);
return err;
}