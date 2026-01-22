#include <string.h>
#include <error.h>
#include <argp.h>
#include <version.h>
#include <limits.h>
#include "isofs.h"
struct node *diskfs_root_node;
struct store *store = 0;
struct store_parsed *store_parsed = 0;
char *diskfs_disk_name = 0;
char *diskfs_server_name = "iso9660fs";
char *diskfs_server_version = HURD_VERSION;
char *diskfs_extra_version = "GNU Hurd";
int diskfs_synchronous = 0;
int diskfs_link_max = INT_MAX;
int diskfs_name_max = 255;
int diskfs_maxsymlinks = 8;
char *host_name;
char *mounted_on;
size_t logical_block_size;
struct sblock *sblock;
static void
fetch_root (void)
{
struct lookup_context ctx;
ino_t id;
error_t err;
ctx.dr = (struct dirrect *) sblock->root;
rrip_initialize (ctx.dr);
rrip_lookup (ctx.dr, &ctx.rr, 1);
err = cache_id (ctx.dr, &ctx.rr, &id);
assert_perror_backtrace (err);
err = diskfs_cached_lookup_context (id, &diskfs_root_node, &ctx);
assert_perror_backtrace (err);
pthread_mutex_unlock (&diskfs_root_node->lock);
}
static void
read_sblock (void)
{
struct voldesc *vd;
struct sblock * volatile sb = 0;
for (vd = disk_image + (logical_sector_size * 16);
(void *) vd < disk_image + (logical_sector_size * 500)
&& (void *) vd + logical_sector_size < disk_image + disk_image_len;
vd = (void *) vd + logical_sector_size)
{
if (vd->type == VOLDESC_END)
break;
if (vd->type == VOLDESC_PRIMARY
&& !memcmp (ISO_STANDARD_ID, vd->id, 5)
&& vd->version == 1)
{
sb = (struct sblock *) vd;
break;
}
}
if (!sb)
error (1, 0, "Could not find valid superblock");
sblock = malloc (sizeof (struct sblock));
if (!sblock)
error (1, errno, "Could not allocate memory for superblock");
memcpy (sblock, sb, sizeof (struct sblock));
logical_block_size = isonum_723 (sblock->blksize);
}
error_t
diskfs_append_args (char **argz, size_t *argz_len)
{
error_t err;
err = diskfs_append_std_options (argz, argz_len);
if (! err)
err = store_parsed_append_args (store_parsed, argz, argz_len);
return err;
}
int
main (int argc, char **argv)
{
mach_port_t bootstrap;
diskfs_readonly = 1;
diskfs_hard_readonly = 1;
store = diskfs_init_main (NULL, argc, argv, &store_parsed, &bootstrap);
create_disk_pager ();
read_sblock ();
fetch_root ();
diskfs_startup_diskfs (bootstrap, 0);
pthread_exit (NULL);
return 0;
}
error_t
diskfs_reload_global_state (void)
{
return 0;
}
error_t
diskfs_set_hypermetadata (int wait, int clean)
{
return 0;
}
void
diskfs_readonly_changed (int readonly)
{
abort ();
}