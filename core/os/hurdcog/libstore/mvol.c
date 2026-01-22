#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "store.h"
struct mvol_state
{
ssize_t cur_vol;
error_t (*swap_vols) (struct store *store, size_t new_vol, ssize_t old_vol);
};
static error_t
ensure_vol (struct store *store, size_t vol)
{
error_t err = 0;
struct mvol_state *mv = store->hook;
if (vol != mv->cur_vol)
{
err = (*mv->swap_vols) (store, vol, mv->cur_vol);
if (! err)
mv->cur_vol = vol;
}
return err;
}
static error_t
mvol_read (struct store *store,
store_offset_t addr, size_t index, size_t amount,
void **buf, size_t *len)
{
error_t err = ensure_vol (store, index);
if (! err)
err = store_read (store->children[0], addr, amount, buf, len);
return err;
}
static error_t
mvol_write (struct store *store,
store_offset_t addr, size_t index,
const void *buf, size_t len, size_t *amount)
{
error_t err = ensure_vol (store, index);
if (! err)
err = store_write (store->children[0], addr, buf, len, amount);
return err;
}
static error_t
mvol_set_size (struct store *store, size_t newsize)
{
return EOPNOTSUPP;
}
error_t
mvol_remap (struct store *source,
const struct store_run *runs, size_t num_runs,
struct store **store)
{
return store_remap_create (source, runs, num_runs, 0, store);
}
const struct store_class
store_mvol_class =
{
-1, "mvol", mvol_read, mvol_write, mvol_set_size,
0, 0, 0,
store_set_child_flags, store_clear_child_flags, 0, 0, mvol_remap
};
STORE_STD_CLASS (mvol);
error_t
store_mvol_create (struct store *phys,
error_t (*swap_vols) (struct store *store, size_t new_vol,
ssize_t old_vol),
int flags,
struct store **store)
{
error_t err;
struct store_run run;
run.start = 0;
run.length = phys->end;
err = _store_create (&store_mvol_class, MACH_PORT_NULL,
flags | phys->flags, phys->block_size,
&run, 1, 0, store);
if (! err)
{
struct mvol_state *mv = malloc (sizeof (struct mvol_state));
if (mv)
{
mv->swap_vols = swap_vols;
mv->cur_vol = -1;
(*store)->hook = mv;
}
else
err = ENOMEM;
if (! err)
err = store_set_children (*store, &phys, 1);
if (! err)
{
if (phys->name)
{
size_t nlen =
strlen (phys->class->name) + 1 + strlen (phys->name) + 1;
char *name = malloc (nlen);
if (name)
{
snprintf (name, nlen, "%s:%s", phys->class->name, phys->name);
(*store)->name = name;
}
else
err = ENOMEM;
}
}
if (err)
{
free (mv);
store_free (*store);
}
}
return err;
}