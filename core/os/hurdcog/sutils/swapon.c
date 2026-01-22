#include <hurd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <argp.h>
#include <argz.h>
#include <error.h>
#include <assert-backtrace.h>
#include <sys/mman.h>
#include <hurd/store.h>
#include <hurd/paths.h>
#include <version.h>
#include <mntent.h>
#include "default_pager_U.h"
#ifdef SWAPOFF
const char *argp_program_version = STANDARD_HURD_VERSION (swapoff);
#else
const char *argp_program_version = STANDARD_HURD_VERSION (swapon);
#endif
static int ignore_signature, require_signature, show, quiet, ifexists;
static struct argp_option options[] =
{
{"standard",	  'a', 0, 0,
"Use all devices marked as `swap' in " _PATH_MNTTAB},
{"ifexists",    'e', 0, 0,
"Silently skip devices that do not exist"},
{"no-signature",'n', 0, 0,
"Do not check for a Linux swap signature page"},
{"require-signature", 's', 0, 0,
"Require a Linux swap signature page"},
{"show", 'S', 0, 0,
"Show devices currently in use"},
{"silent",     'q', 0,      0, "Print only diagnostic messages"},
{"quiet",      'q', 0,      OPTION_ALIAS | OPTION_HIDDEN },
{"verbose",    'v', 0,      0, "Be verbose"},
{0, 0}
};
static char *args_doc = "DEVICE...";
static char *doc =
#ifdef SWAPOFF
"Stop paging on DEVICE..."
"\vUnless overridden, a swap space signature is not considered when deciding"
" whether to remove a paging device or not."
#else
"Start paging onto DEVICE..."
"\vUnless overridden, only devices with a valid (Linux) swap space signature"
" are considered when deciding whether to add a paging device or not."
#endif
;
#define verbose(fmt, arg...) \
if (quiet_now) ((void)0); else error (0, 0, fmt ,##arg)
#define inform_2_0(fmt, arg...) \
verbose ("%s: Linux 2.0 swap signature, " fmt, name ,##arg)
#define inform_2_2(fmt, arg...) \
verbose ("%s: Linux 2.2 swap signature v1, %zuk swap-space" fmt, \
name, freepages * (LINUX_PAGE_SIZE / 1024) ,##arg)
static mach_port_t def_pager = MACH_PORT_NULL;
static mach_port_t dev_master = MACH_PORT_NULL;
static void get_def_pager(int autostart)
{
int err;
mach_port_t host;
if (def_pager != MACH_PORT_NULL)
return;
err = get_privileged_ports (&host, &dev_master);
if (err == EPERM)
{
def_pager = file_name_lookup (_SERVERS_DEFPAGER, O_WRITE, 0);
if (def_pager == MACH_PORT_NULL)
error (11, errno, _SERVERS_DEFPAGER);
}
else
{
if (err)
error (12, err, "Cannot get privileged ports");
err = vm_set_default_memory_manager (host, &def_pager);
if (err)
error (13, err, "Cannot get default pager port");
if (def_pager == MACH_PORT_NULL)
{
error (autostart ? 0 : 14, 0,
"No default pager (memory manager) is running");
if (autostart)
{
err = system ("/hurd/mach-defpager");
if (err)
error (15, err, "Could not start it");
else
{
fprintf (stderr, "Started it\n");
err = vm_set_default_memory_manager (host, &def_pager);
if (err)
error (16, err, "Cannot get default pager port");
}
}
}
mach_port_deallocate (mach_task_self (), host);
}
}
static error_t
check_signature (const char *name, struct store **storep, int no_remap,
int quiet_now)
{
struct store *const store = *storep;
#define LINUX_PAGE_SIZE 4096
#define LINUX_PAGE_SHIFT 12
struct run
{
struct run *next;
size_t start, limit;
};
size_t freepages = store->size / LINUX_PAGE_SIZE;
struct run first_run = { NULL, 0, freepages }, *runs = &first_run;
size_t nruns = 1;
#define BAD_PAGE(pageno)						      \
({									      \
size_t page = (pageno);						      \
if (page == runs->start)						      \
runs->start = page + 1;						      \
else								      \
{									      \
runs->next = alloca (sizeof *runs);				      \
runs->next->start = page + 1;					      \
runs->next->limit = runs->limit;				      \
runs->limit = page;						      \
++nruns;							      \
}									      \
})
void *buf = 0;
size_t len = 0;
error_t err = store_read (store, 0, LINUX_PAGE_SIZE, &buf, &len);
if (err)
{
error (0, err, "%s: cannot read Linux swap signature page", name);
return err;
}
if (len < LINUX_PAGE_SIZE)
{
error (0, 0, "%s: short read %zu reading Linux swap signature page",
name, len);
return EINVAL;
}
quiet_now |= quiet;
if (!memcmp ("SWAP-SPACE", buf + LINUX_PAGE_SIZE-10, 10))
{
size_t i, bad, max;
int waste;
*(uint32_t *) buf &= ~(u_int32_t) 1;
memset (buf + LINUX_PAGE_SIZE-10, 0, 10);
max = LINUX_PAGE_SIZE / sizeof (uint32_t);
if (max > (store->size + 31) / 32)
max = (store->size + 31) / 32;
bad = 0;
for (i = 0; i < max; ++i)
{
size_t p = i*32;
uint32_t bm = ~((uint32_t *) buf)[i];
while (bm != 0)
{
int bit = ffs (bm);
bm >>= bit;
p += bit - 1;
if (p >= runs->limit)
break;
++bad;
BAD_PAGE (p);
}
}
freepages -= bad;
--bad;
waste = (store->size >> LINUX_PAGE_SHIFT) - (8 * (LINUX_PAGE_SIZE-10));
if (waste > 0)
{
bad -= waste;
if (bad > 0)
inform_2_0 ("%zdk swap-space (%zdk bad, %dk wasted at end)",
freepages * (LINUX_PAGE_SIZE / 1024),
bad * (LINUX_PAGE_SIZE / 1024),
waste * (LINUX_PAGE_SIZE / 1024));
else
inform_2_0 ("%zdk swap-space (%dk wasted at end)",
freepages * (LINUX_PAGE_SIZE / 1024),
waste * (LINUX_PAGE_SIZE / 1024));
}
else if (bad > 0)
inform_2_0 ("%zdk swap-space (excludes %zdk marked bad)",
freepages * (LINUX_PAGE_SIZE / 1024),
bad * (LINUX_PAGE_SIZE / 1024));
else
inform_2_0 ("%zdk swap-space", freepages * (LINUX_PAGE_SIZE / 1024));
}
else if (!memcmp ("SWAPSPACE2", buf + LINUX_PAGE_SIZE-10, 10))
{
struct
{
u_int8_t bootbits[1024];
u_int32_t version;
u_int32_t last_page;
u_int32_t nr_badpages;
u_int32_t padding[125];
u_int32_t badpages[1];
} *hdr = buf;
++first_run.start;
--freepages;
switch (hdr->version)
{
default:
error (0, 0,
"%s: Linux 2.2 swap signature with unknown version %u",
name, hdr->version);
munmap (buf, len);
if (require_signature)
{
error (0, 0, "%s: will not use without valid signature page",
name);
return EINVAL;
}
error (0, 0, "WARNING: ignoring unrecognized signature page");
return EFTYPE;
case 1:
{
unsigned int waste, i;
if (hdr->last_page >= first_run.limit)
{
error (0, 0,
"%s: signature says %uk, partition has only %uk!",
name,
hdr->last_page * (LINUX_PAGE_SIZE / 1024),
(unsigned int) (store->size / 1024));
waste = 0;
}
else
{
waste = first_run.limit + 1 - hdr->last_page;
freepages = first_run.limit - first_run.start;
first_run.limit = hdr->last_page + 1;
}
for (i = 0; i < hdr->nr_badpages; ++i)
{
BAD_PAGE (hdr->badpages[i]);
--freepages;
}
{
size_t badk = hdr->nr_badpages * (LINUX_PAGE_SIZE / 1024);
size_t wastek = waste * (LINUX_PAGE_SIZE / 1024);
if (badk && wastek)
inform_2_2 ("\
(excludes %zuk marked bad and %zuk at end of partition)",
badk, wastek);
else if (badk)
inform_2_2 (" (excludes %zuk marked bad)", badk);
else if (wastek)
inform_2_2 (" (excludes %zuk at end of partition)", wastek);
else
inform_2_2 ("");
}
}
}
}
else if (require_signature)
{
error (0, 0, "%s: will not use without Linux swap signature", name);
return EINVAL;
}
else
return EFTYPE;
{
const int scale = LINUX_PAGE_SHIFT - store->log2_block_size;
struct store_run store_runs[nruns];
size_t i = 0;
struct run *r = &first_run;
do
{
struct store_run *sr = &store_runs[i++];
sr->start = (store_offset_t) r->start << scale;
sr->length = (r->limit - r->start) << scale;
do
r = r->next;
while (r != 0 && r->start == r->limit);
} while (r != 0);
return store_remap (store, store_runs, i, storep);
}
}
static int
swaponoff (const char *file, int add, int skipnotexisting)
{
error_t err;
struct store *store;
int quiet_now = 0;
err = store_open (file, 0, 0, &store);
if (err)
{
if (err == ENOENT && skipnotexisting)
return 0;
error (0, err, "%s", file);
return err;
}
if (ignore_signature)
verbose ("%s: %uk swap space",
file, (unsigned int) (store->size / 1024));
else
{
err = check_signature (file, &store, 0, 0);
if (err == EFTYPE)
verbose ("%s: %uk swap space (no Linux signature page)",
file, (unsigned int) (store->size / 1024));
else if (err)
{
store_free (store);
return err;
}
}
if (store->class != &store_device_class)
{
error (0, 0, "%s: Can't get underlying device", file);
store_free (store);
return EINVAL;
}
get_def_pager (add);
recnum_t runs[store->num_runs * 2];
size_t i, j;
for (i = j = 0; i < store->num_runs; ++i)
{
runs[j++] = store->runs[i].start;
runs[j++] = store->runs[i].length;
}
err = default_pager_paging_storage_new (def_pager, store->port,
runs, j, file, add);
#ifdef __i386__
if (err == MIG_BAD_ID || err == EOPNOTSUPP)
{
err = default_pager_paging_storage (def_pager, store->port,
runs, j, file, add);
}
#endif
store_free (store);
if (err)
error (0, err, "%s", file);
return err;
}
#undef inform_2_0
#undef inform_2_2
#undef verbose
static int do_all;
int
main (int argc, char *argv[])
{
error_t parse_opt (int key, char *arg, struct argp_state *state)
{
switch (key)
{
case 'a':
do_all = 1;
break;
case 'e':
ifexists = 1;
break;
case 'n':
ignore_signature = 1;
break;
case 's':
require_signature = 1;
ignore_signature = 0;
break;
case 'S':
show = 1;
break;
case 'q':
quiet = 1;
break;
case 'v':
quiet = 0;
break;
case ARGP_KEY_ARG:
#ifdef SWAPOFF
#define ONOFF 0
#else
#define ONOFF 1
#endif
swaponoff (arg, ONOFF, 0);
break;
default:
return ARGP_ERR_UNKNOWN;
}
return 0;
}
struct argp argp = {options, parse_opt, args_doc, doc};
#ifdef SWAPOFF
ignore_signature = 1;
require_signature = 0;
#else
ignore_signature = 0;
require_signature = 1;
#endif
argp_parse (&argp, argc, argv, ARGP_IN_ORDER, 0, 0);
if (do_all)
{
struct mntent *me;
FILE *f;
f = setmntent (_PATH_MNTTAB, "r");
if (f == NULL)
error (1, errno, "Cannot read %s", _PATH_MNTTAB);
else
{
int done = 0, err = 0;
while ((me = getmntent (f)) != NULL)
if (!strcmp (me->mnt_type, MNTTYPE_SWAP))
{
done = 1;
err |= swaponoff (me->mnt_fsname, ONOFF, ifexists);
}
if (done == 0)
error (2, 0, "No swap partitions found in %s", _PATH_MNTTAB);
else if (err)
return 1;
}
}
if (show)
{
vm_size_t *free = NULL;
mach_msg_type_number_t nfree = 0;
vm_size_t *size = NULL;
mach_msg_type_number_t nsize = 0;
char *names = NULL, *name;
mach_msg_type_number_t names_len = 0;
size_t i;
int err;
get_def_pager (0);
err = default_pager_storage_info (def_pager, &size, &nsize, &free, &nfree,
&names, &names_len);
if (err)
error (3, 0, "Can not get default pager storage information");
printf("Filename\tType\t\tSize\tUsed\tPriority\n");
name = names;
for (i = 0; i < nfree; i++)
{
printf ("%s\tpartition\t%zuM\t%zuM\t-1\n",
name, size[i] >> 20, (size[i] - free[i]) >> 20);
name = argz_next (names, names_len, name);
}
vm_deallocate (mach_task_self(), (vm_offset_t) free,
nfree * sizeof(*free));
vm_deallocate (mach_task_self(), (vm_offset_t) size,
nsize * sizeof(*size));
vm_deallocate (mach_task_self(), (vm_offset_t) names, names_len);
}
return 0;
}