#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <mach.h>
#include "store.h"
error_t
store_set_runs (struct store *store,
const struct store_run *runs, size_t num_runs)
{
unsigned size = num_runs * sizeof (struct store_run);
struct store_run *copy = malloc (size);
if (!copy)
return ENOMEM;
if (store->runs)
free (store->runs);
memcpy (copy, runs, size);
store->runs = copy;
store->num_runs = num_runs;
if (store->block_size > 0)
_store_derive (store);
return 0;
}
error_t
store_set_name (struct store *store, const char *name)
{
char *copy = strdup (name);
if (!copy)
return ENOMEM;
if (store->name)
free (store->name);
store->name = copy;
return 0;
}
void store_close_source (struct store *store)
{
if (store->source != MACH_PORT_NULL)
{
mach_port_deallocate (mach_task_self (), store->source);
store->source = MACH_PORT_NULL;
}
}