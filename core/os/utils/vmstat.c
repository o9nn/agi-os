#include <stdio.h>
#include <stddef.h>
#include <argp.h>
#include <error.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <fcntl.h>
#include <version.h>
#include <mach.h>
#include <mach/gnumach.h>
#include <mach/vm_statistics.h>
#include <mach/vm_cache_statistics.h>
#include <hurd.h>
#include <hurd/paths.h>
#include "default_pager_U.h"
const char *argp_program_version = STANDARD_HURD_VERSION (vmstat);
static const struct argp_option options[] = {
{"terse", 't', 0, 0, "Use short one-line output format"},
{"no-header", 'H', 0, 0, "Don't print a descriptive header line"},
{"prefix", 'p', 0, 0, "Always display a description before stats"},
{"no-prefix", 'P', 0, 0, "Never display a description before stats"},
{"pages", 'v', 0, 0, "Display sizes in pages"},
{"kilobytes", 'k', 0, 0, "Display sizes in 1024 byte blocks"},
{"bytes", 'b', 0, 0, "Display sizes in bytes"},
{0}
};
static const char *args_doc = "[PERIOD [COUNT [HEADER_INTERVAL]]]";
static const char *doc = "Show system virtual memory statistics"
"\vIf PERIOD is supplied, then terse mode is"
" selected, and the output repeated every PERIOD seconds, with cumulative"
" fields given the difference from the last output.  If COUNT is given"
" and non-zero, only that many lines are output.  HEADER_INTERVAL"
" defaults to 23, and if not zero, is the number of repeats after which a"
" blank line and the header will be reprinted (as well as the totals for"
" cumulative fields).";
typedef long long val_t;
#define BADVAL ((val_t) -1LL)
enum val_type
{
COUNT,
SIZE,
PAGESZ,
PCENT,
};
static size_t
val_width (val_t val, enum val_type type, size_t size_units)
{
size_t vwidth (val_t val)
{
size_t w = 1;
if (val < 0)
w++, val = -val;
while (val > 9)
w++, val /= 10;
return w;
}
if (type == PCENT)
return vwidth (val) + 1;
else if ((type == SIZE || type == PAGESZ) && size_units == 0)
return val > 1000 ? 5 : vwidth (val) + 1;
else
{
if ((type == SIZE || type == PAGESZ) && size_units > 0)
val /= size_units;
return vwidth (val);
}
}
static void
print_val (val_t val, enum val_type type,
size_t size_units, int fwidth, int sign)
{
if (type == PCENT)
printf (sign ? "%+*lld%%" : "%*lld%%", fwidth - 1, val);
else if ((type == SIZE || type == PAGESZ) && size_units == 0)
{
float fval = val;
char *units = " KMGT", *u = units;
while (fval >= 10000)
{
fval /= 1024;
u++;
}
printf ((fval >= 1000
? (sign ? "%+*.0f%c" : "%*.0f%c")
: (sign ? "%+*.3g%c" : "%*.3g%c")),
fwidth - 1, fval, *u);
}
else
{
if ((type == SIZE || type == PAGESZ) && size_units > 0)
val /= size_units;
printf (sign ? "%+*lld" : "%*lld", fwidth, val);
}
}
#define VAL_MAX_MEM -1
#define VAL_MAX_SWAP -2
enum field_change_type
{
VARY,
CONST,
CUMUL,
};
struct vm_state;
struct field
{
char *name;
char *hdr;
char *doc;
enum field_change_type change_type;
enum val_type type;
val_t max;
int standard :1;
int offs;
val_t (*compute)(struct vm_state *state, const struct field *field);
};
struct vm_state
{
struct vm_statistics vmstats;
struct vm_cache_statistics cache_stats;
mach_port_t def_pager;
struct default_pager_info def_pager_info;
};
static error_t
vm_state_refresh (struct vm_state *state)
{
error_t err = vm_statistics (mach_task_self (), &state->vmstats);
if (err)
return err;
err = vm_cache_statistics (mach_task_self (), &state->cache_stats);
if (err)
return err;
memset (&state->def_pager_info, 0, sizeof state->def_pager_info);
return 0;
}
static val_t
get_vmstats_field (struct vm_state *state, const struct field *field)
{
val_t val =
(val_t)(*(integer_t *)((char *)&state->vmstats + field->offs));
if (field->type == SIZE)
val *= state->vmstats.pagesize;
return val;
}
static val_t
get_size (struct vm_state *state, const struct field *field)
{
return
((val_t) (state->vmstats.free_count + state->vmstats.active_count
+ state->vmstats.inactive_count + state->vmstats.wire_count))
* state->vmstats.pagesize;
}
static val_t
vm_state_get_field (struct vm_state *state, const struct field *field)
{
return (field->compute ?: get_vmstats_field) (state, field);
}
static val_t
get_memobj_hit_ratio (struct vm_state *state, const struct field *field)
{
return (val_t)
((float) state->vmstats.hits * 100. / (float) state->vmstats.lookups);
}
static int
ensure_def_pager_info (struct vm_state *state)
{
error_t err;
if (state->def_pager == MACH_PORT_NULL)
{
mach_port_t host;
err = get_privileged_ports (&host, 0);
if (err == EPERM)
{
state->def_pager = file_name_lookup (_SERVERS_DEFPAGER, O_READ, 0);
if (state->def_pager == MACH_PORT_NULL)
{
error (0, errno, _SERVERS_DEFPAGER);
return 0;
}
}
if (state->def_pager == MACH_PORT_NULL)
{
if (err)
{
error (0, err, "get_privileged_ports");
return 0;
}
err = vm_set_default_memory_manager (host, &state->def_pager);
mach_port_deallocate (mach_task_self (), host);
if (err)
{
error (0, err, "vm_set_default_memory_manager");
return 0;
}
}
}
if (!MACH_PORT_VALID (state->def_pager))
{
if (state->def_pager == MACH_PORT_NULL)
{
error (0, 0,
"No default pager running, so no swap information available");
state->def_pager = MACH_PORT_DEAD;
}
return 0;
}
err = default_pager_info (state->def_pager, &state->def_pager_info);
if (err)
error (0, err, "default_pager_info");
return (err == 0);
}
#define SWAP_FIELD(getter, expr) \
static val_t getter (struct vm_state *state, const struct field *field) \
{ return ensure_def_pager_info (state) ? (val_t) (expr) : BADVAL; }
SWAP_FIELD (get_swap_size, state->def_pager_info.dpi_total_space)
SWAP_FIELD (get_swap_free, state->def_pager_info.dpi_free_space)
SWAP_FIELD (get_swap_page_size, state->def_pager_info.dpi_page_size)
SWAP_FIELD (get_swap_active, (state->def_pager_info.dpi_total_space
- state->def_pager_info.dpi_free_space))
#define _F(field_name) offsetof (struct vm_state, field_name)
#define K 1024
#define M (1024*K)
#define G (1024LL*M)
static const struct field fields[] =
{
{"pagesize", "pgsz", "System pagesize",
CONST, PAGESZ, 16*K, 1, _F (vmstats.pagesize) },
{"size", "size", "Usable physical memory",
CONST, SIZE, VAL_MAX_MEM, 1, 0, get_size },
{"free", "free", "Unused physical memory",
VARY, SIZE, VAL_MAX_MEM, 1, _F (vmstats.free_count) },
{"active", "actv", "Physical memory in active use",
VARY, SIZE, VAL_MAX_MEM, 1, _F (vmstats.active_count) },
{"inactive", "inact", "Physical memory in the inactive queue",
VARY, SIZE, VAL_MAX_MEM, 1, _F (vmstats.inactive_count) },
{"wired", "wired", "Unpageable physical memory",
VARY, SIZE, VAL_MAX_MEM, 1, _F (vmstats.wire_count) },
{"zero filled", "zeroed","Cumulative zero-filled pages",
CUMUL, SIZE, 90*G, 1, _F (vmstats.zero_fill_count) },
{"reactivated", "react", "Cumulative reactivated inactive pages",
CUMUL, SIZE, 900*M, 1, _F (vmstats.reactivations) },
{"pageins", "pgins", "Cumulative pages paged in",
CUMUL, SIZE, 90*G, 1, _F (vmstats.pageins) },
{"pageouts", "pgouts","Cumulative pages paged out",
CUMUL, SIZE, 90*G, 1, _F (vmstats.pageouts) },
{"page faults", "pfaults","Cumulative page faults",
CUMUL, COUNT, 99999999, 1, _F (vmstats.faults) },
{"cow faults", "cowpfs", "Cumulative copy-on-write page faults",
CUMUL, COUNT, 9999999, 1, _F (vmstats.cow_faults) },
{"memobj lookups","lkups","Memory-object lookups",
CUMUL, COUNT, 999999, 0, _F (vmstats.lookups) },
{"memobj hits", "hits", "Memory-object lookups with active pagers",
CUMUL, COUNT, 999999, 0, _F (vmstats.hits) },
{"memobj hit ratio","hrat","Percentage of memory-object lookups with active pagers",
VARY, PCENT, 99, 1, -1, get_memobj_hit_ratio },
{"cached memobjs", "caobj", "Number of memory-objects retained in the page cache",
VARY, COUNT, 99999999, 1, _F (cache_stats.cache_object_count) },
{"cache", "cache", "Physical memory used by the page cache",
VARY, SIZE, VAL_MAX_MEM, 1, _F (cache_stats.cache_count) },
{"swap size", "swsize", "Size of the default-pager swap area",
CONST, SIZE, VAL_MAX_SWAP, 1, 0 ,get_swap_size },
{"swap active", "swactv", "Default-pager swap area in use",
VARY, SIZE, VAL_MAX_SWAP, 0, 0 ,get_swap_active },
{"swap free", "swfree", "Default-pager swap area available for swapping",
VARY, SIZE, VAL_MAX_SWAP, 1, 0 ,get_swap_free },
{"swap pagesize","swpgsz", "Units used for swapping to the default pager",
CONST, PAGESZ, 16*K, 0, 0 ,get_swap_page_size },
{0}
};
#undef _F
static char *name_to_option (const char *name)
{
char *opt = strdup (name), *p;
if (opt)
for (p = opt; *p; p++)
if (*p == ' ')
*p = '-';
return opt;
}
int
main (int argc, char **argv)
{
error_t err;
const struct field *field;
struct vm_state state;
int num_fields = 0;
unsigned long output_fields = 0;
int count = 1;
unsigned period = 0;
unsigned hdr_interval = 22;
ssize_t size_units = 0;
int terse = 0, print_heading = 1, print_prefix = -1;
error_t parse_opt (int key, char *arg, struct argp_state *state)
{
if (key < 0)
output_fields |= (1 << (-1 - key));
else
switch (key)
{
case 't': terse = 1; break;
case 'p': print_prefix = 1; break;
case 'P': print_prefix = 0; break;
case 'H': print_heading = 0; break;
case 'b': size_units = 1; break;
case 'v': size_units = -1; break;
case 'k': size_units = K; break;
case ARGP_KEY_ARG:
terse = 1;
switch (state->arg_num)
{
case 0:
period = atoi (arg); count = 0; break;
case 1:
count = atoi (arg); break;
case 2:
hdr_interval = atoi (arg); break;
default:
return ARGP_ERR_UNKNOWN;
}
break;
default:
return ARGP_ERR_UNKNOWN;
}
return 0;
}
struct argp_option *field_opts;
int field_opts_size;
struct argp field_argp = { 0, parse_opt };
const struct argp_child children[] =
{{&field_argp, 0, "Selecting which statistics to show:"}, {0}};
const struct argp argp = { options, parse_opt, args_doc, doc, children };
for (field = fields; field->name; field++)
num_fields++;
field_opts_size = ((num_fields + 1) * sizeof (struct argp_option));
field_opts = alloca (field_opts_size);
memset (field_opts, 0, field_opts_size);
for (field = fields; field->name; field++)
{
int which = field - fields;
struct argp_option *opt = &field_opts[which];
opt->name = name_to_option (field->name);
opt->key = -1 - which;
opt->doc = field->doc;
opt->group = 2;
}
field_argp.options = field_opts;
argp_parse (&argp, argc, argv, 0, 0, 0);
if (output_fields == 0)
for (field = fields; field->name; field++)
if (field->standard)
output_fields |= (1 << (field - fields));
#define SIZE_UNITS(field) \
(size_units >= 0 \
? size_units \
: ((field)->type == PAGESZ ? 0 : state.vmstats.pagesize))
#define PVAL(val, field, width, sign) \
print_val (val, (field)->type, SIZE_UNITS (field), width, sign)
#define FWIDTH(field) \
val_width ((field)->max == VAL_MAX_MEM ? get_size (&state, field) \
: (field)->max == VAL_MAX_SWAP ? get_swap_size (&state, field) \
: (field)->max, \
(field)->type, SIZE_UNITS (field))
memset (&state, 0, sizeof (state));
err = vm_state_refresh (&state);
if (err)
error (2, err, "vm_state_refresh");
if (terse)
{
int first_hdr = 1, first, repeats;
struct vm_state prev_state;
int const_fields = 0;
if (count == 0)
count = -1;
for (field = fields; field->name; field++)
if ((output_fields & (1 << (field - fields)))
&& field->change_type == CONST)
const_fields |= (1 << (field - fields));
output_fields &= ~const_fields;
if (const_fields)
hdr_interval--;
do
{
int num;
int fwidths[num_fields];
if (first_hdr)
first_hdr = 0;
else
putchar ('\n');
if (const_fields)
{
for (field = fields, first = 1; field->name; field++)
if (const_fields & (1 << (field - fields)))
{
val_t val = vm_state_get_field (&state, field);
if (val < 0)
const_fields &= ~(1 << (field - fields));
else
{
if (first)
{
first = 0;
fputs("(", stdout);
}
else
fputs(",", stdout);
printf ("%s: ", field->name);
PVAL (val, field, 0, 0);
}
}
if (! first)
puts (")");
}
for (field = fields, num = 0; field->name; field++, num++)
if (output_fields & (1 << (field - fields)))
{
fwidths[num] = FWIDTH (field);
if (count != 1 && size_units == 0
&& field->change_type == CUMUL && field->type == SIZE)
fwidths[num]++;
if (fwidths[num] < strlen (field->hdr))
fwidths[num] = strlen (field->hdr);
}
if (print_heading)
{
for (field = fields, num = 0, first = 1; field->name; field++, num++)
if (output_fields & (1 << (field - fields)))
{
if (first)
first = 0;
else
fputs (" ", stdout);
fprintf (stdout, "%*s", fwidths[num], field->hdr);
}
putchar ('\n');
}
prev_state = state;
for (repeats = 0
; count && repeats < hdr_interval
; repeats++, count--)
{
for (field = fields, num = 0, first = 1; field->name; field++, num++)
if (output_fields & (1 << (field - fields)))
{
val_t val = vm_state_get_field (&state, field);
if (val < 0)
const_fields &= ~(1 << (field - fields));
else
{
int sign = 0;
if (repeats && field->change_type == CUMUL)
{
sign = 1;
val -= vm_state_get_field (&prev_state, field);
}
if (first)
first = 0;
else
fputs (" ", stdout);
PVAL (val, field, fwidths[num], sign);
}
}
putchar ('\n');
prev_state = state;
if (period)
{
sleep (period);
err = vm_state_refresh (&state);
if (err)
error (2, err, "vm_state_refresh");
}
}
}
while (count);
}
else
{
int max_width = 0;
if (print_prefix < 0)
print_prefix = (output_fields & (output_fields - 1));
if (print_prefix)
for (field = fields; field->name; field++)
if (output_fields & (1 << (field - fields)))
{
int width = strlen (field->name) + FWIDTH (field);
if (width > max_width)
max_width = width;
}
for (field = fields; field->name; field++)
if (output_fields & (1 << (field - fields)))
{
val_t val = vm_state_get_field (&state, field);
if (val >= 0)
{
int fwidth = 0;
if (print_prefix)
{
printf ("%s: ", field->name);
fwidth = max_width - strlen (field->name);
}
PVAL (val, field, fwidth, 0);
putchar ('\n');
}
}
}
exit (0);
}