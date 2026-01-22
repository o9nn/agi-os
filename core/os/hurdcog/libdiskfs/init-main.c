#include "diskfs.h"
#include <argp.h>
#include <assert-backtrace.h>
#include <error.h>
#include <hurd/store.h>
struct store *
diskfs_init_main (struct argp *startup_argp,
int argc, char **argv,
struct store_parsed **store_parsed,
mach_port_t *bootstrap)
{
error_t err;
struct store_argp_params store_params = { 0 };
struct store *store;
err = argp_parse (startup_argp ?: &diskfs_store_startup_argp,
argc, argv, ARGP_IN_ORDER, NULL,
&store_params);
assert_perror_backtrace (err);
*store_parsed = store_params.result;
err = store_parsed_name (*store_parsed, &diskfs_disk_name);
if (err)
error (2, err, "store_parsed_name");
diskfs_console_stdio ();
if (diskfs_boot_filesystem ())
*bootstrap = MACH_PORT_NULL;
else
{
task_get_bootstrap_port (mach_task_self (), bootstrap);
if (*bootstrap == MACH_PORT_NULL)
error (2, 0, "Must be started as a translator");
}
err = diskfs_init_diskfs ();
if (err)
error (4, err, "diskfs_init_diskfs");
err = store_parsed_open (*store_parsed, diskfs_readonly ? STORE_READONLY : 0,
&store);
if (err)
error (3, err, "%s", diskfs_disk_name);
if (store->flags & STORE_HARD_READONLY)
diskfs_readonly = diskfs_hard_readonly = 1;
diskfs_spawn_first_thread (diskfs_demuxer);
return store;
}