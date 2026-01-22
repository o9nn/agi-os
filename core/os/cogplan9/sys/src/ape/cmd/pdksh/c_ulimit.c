#include "sh.h"
#include "ksh_time.h"
#ifdef HAVE_SYS_RESOURCE_H
# include <sys/resource.h>
#endif
#ifdef HAVE_ULIMIT_H
# include <ulimit.h>
#else
# ifdef HAVE_ULIMIT
extern	long ulimit();
# endif
#endif
#define SOFT	0x1
#define HARD	0x2
#ifdef RLIM_INFINITY
# define KSH_RLIM_INFINITY RLIM_INFINITY
#else
# define KSH_RLIM_INFINITY ((rlim_t) 1 << (sizeof(rlim_t) * 8 - 1) - 1)
#endif
int
c_ulimit(wp)
char **wp;
{
static const struct limits {
const char	*name;
enum { RLIMIT, ULIMIT } which;
int	gcmd;
int	scmd;
int	factor;
char	option;
} limits[] = {
#ifdef RLIMIT_CPU
{ "time(cpu-seconds)", RLIMIT, RLIMIT_CPU, RLIMIT_CPU, 1, 't' },
#endif
#ifdef RLIMIT_FSIZE
{ "file(blocks)", RLIMIT, RLIMIT_FSIZE, RLIMIT_FSIZE, 512, 'f' },
#else
# ifdef UL_GETFSIZE
{ "file(blocks)", ULIMIT, UL_GETFSIZE, UL_SETFSIZE, 1, 'f' },
# else
#  ifdef UL_GFILLIM
{ "file(blocks)", ULIMIT, UL_GFILLIM, UL_SFILLIM, 1, 'f' },
#  else
{ "file(blocks)", ULIMIT, 1, 2, 1, 'f' },
#  endif
# endif
#endif
#ifdef RLIMIT_CORE
{ "coredump(blocks)", RLIMIT, RLIMIT_CORE, RLIMIT_CORE, 512, 'c' },
#endif
#ifdef RLIMIT_DATA
{ "data(kbytes)", RLIMIT, RLIMIT_DATA, RLIMIT_DATA, 1024, 'd' },
#endif
#ifdef RLIMIT_STACK
{ "stack(kbytes)", RLIMIT, RLIMIT_STACK, RLIMIT_STACK, 1024, 's' },
#endif
#ifdef RLIMIT_MEMLOCK
{ "lockedmem(kbytes)", RLIMIT, RLIMIT_MEMLOCK, RLIMIT_MEMLOCK, 1024, 'l' },
#endif
#ifdef RLIMIT_RSS
{ "memory(kbytes)", RLIMIT, RLIMIT_RSS, RLIMIT_RSS, 1024, 'm' },
#endif
#ifdef RLIMIT_NOFILE
{ "nofiles(descriptors)", RLIMIT, RLIMIT_NOFILE, RLIMIT_NOFILE, 1, 'n' },
#else
# ifdef UL_GDESLIM
{ "nofiles(descriptors)", ULIMIT, UL_GDESLIM, -1, 1, 'n' },
# endif
#endif
#ifdef RLIMIT_NPROC
{ "processes", RLIMIT, RLIMIT_NPROC, RLIMIT_NPROC, 1, 'p' },
#endif
#ifdef RLIMIT_VMEM
{ "vmemory(kbytes)", RLIMIT, RLIMIT_VMEM, RLIMIT_VMEM, 1024, 'v' },
#else
# ifdef UL_GMEMLIM
{ "vmemory(maxaddr)", ULIMIT, UL_GMEMLIM, -1, 1, 'v' },
# else
#  ifdef UL_GETBREAK
{ "vmemory(maxaddr)", ULIMIT, UL_GETBREAK, -1, 1, 'v' },
#  else
#   ifdef UL_GETMAXBRK
{ "vmemory(maxaddr)", ULIMIT, UL_GETMAXBRK, -1, 1, 'v' },
#   endif
#  endif
# endif
#endif
#ifdef RLIMIT_SWAP
{ "swap(kbytes)", RLIMIT_SWAP, RLIMIT_SWAP, 1024, 'w' },
#endif
{ (char *) 0 }
};
static char	options[3 + NELEM(limits)];
rlim_t		UNINITIALIZED(val);
int		how = SOFT | HARD;
const struct limits	*l;
int		set, all = 0;
int		optc, what;
#ifdef HAVE_SETRLIMIT
struct rlimit	limit;
#endif
if (!options[0]) {
char *p = options;
*p++ = 'H'; *p++ = 'S'; *p++ = 'a';
for (l = limits; l->name; l++)
*p++ = l->option;
*p = '\0';
}
what = 'f';
while ((optc = ksh_getopt(wp, &builtin_opt, options)) != EOF)
switch (optc) {
case 'H':
how = HARD;
break;
case 'S':
how = SOFT;
break;
case 'a':
all = 1;
break;
case '?':
return 1;
default:
what = optc;
}
for (l = limits; l->name && l->option != what; l++)
;
if (!l->name) {
internal_errorf(0, "ulimit: %c", what);
return 1;
}
wp += builtin_opt.optind;
set = *wp ? 1 : 0;
if (set) {
if (all || wp[1]) {
bi_errorf("too many arguments");
return 1;
}
if (strcmp(wp[0], "unlimited") == 0)
val = KSH_RLIM_INFINITY;
else {
long rval;
if (!evaluate(wp[0], &rval, KSH_RETURN_ERROR))
return 1;
if (!rval && !digit(wp[0][0])) {
bi_errorf("invalid limit: %s", wp[0]);
return 1;
}
val = rval * l->factor;
}
}
if (all) {
for (l = limits; l->name; l++) {
#ifdef HAVE_SETRLIMIT
if (l->which == RLIMIT) {
getrlimit(l->gcmd, &limit);
if (how & SOFT)
val = limit.rlim_cur;
else if (how & HARD)
val = limit.rlim_max;
} else
#endif
#ifdef HAVE_ULIMIT
{
val = ulimit(l->gcmd, (rlim_t) 0);
}
#else
;
#endif
shprintf("%-20s ", l->name);
#ifdef RLIM_INFINITY
if (val == RLIM_INFINITY)
shprintf("unlimited\n");
else
#endif
{
val /= l->factor;
shprintf("%ld\n", (long) val);
}
}
return 0;
}
#ifdef HAVE_SETRLIMIT
if (l->which == RLIMIT) {
getrlimit(l->gcmd, &limit);
if (set) {
if (how & SOFT)
limit.rlim_cur = val;
if (how & HARD)
limit.rlim_max = val;
if (setrlimit(l->scmd, &limit) < 0) {
if (errno == EPERM)
bi_errorf("exceeds allowable limit");
else
bi_errorf("bad limit: %s",
strerror(errno));
return 1;
}
} else {
if (how & SOFT)
val = limit.rlim_cur;
else if (how & HARD)
val = limit.rlim_max;
}
} else
#endif
#ifdef HAVE_ULIMIT
{
if (set) {
if (l->scmd == -1) {
bi_errorf("can't change limit");
return 1;
} else if (ulimit(l->scmd, val) < 0) {
bi_errorf("bad limit: %s", strerror(errno));
return 1;
}
} else
val = ulimit(l->gcmd, (rlim_t) 0);
}
#else
;
#endif
if (!set) {
#ifdef RLIM_INFINITY
if (val == RLIM_INFINITY)
shprintf("unlimited\n");
else
#endif
{
val /= l->factor;
shprintf("%ld\n", (long) val);
}
}
return 0;
}