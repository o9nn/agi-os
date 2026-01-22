#include <ctype.h>
#include "sh.h"
#include "ksh_stat.h"
static int initio_done;
void
#ifdef HAVE_PROTOTYPES
errorf(const char *fmt, ...)
#else
errorf(fmt, va_alist)
const char *fmt;
va_dcl
#endif
{
va_list va;
shl_stdout_ok = 0;
exstat = 1;
if (*fmt) {
error_prefix(TRUE);
SH_VA_START(va, fmt);
shf_vfprintf(shl_out, fmt, va);
va_end(va);
shf_putchar('\n', shl_out);
}
shf_flush(shl_out);
unwind(LERROR);
}
void
#ifdef HAVE_PROTOTYPES
warningf(int fileline, const char *fmt, ...)
#else
warningf(fileline, fmt, va_alist)
int fileline;
const char *fmt;
va_dcl
#endif
{
va_list va;
error_prefix(fileline);
SH_VA_START(va, fmt);
shf_vfprintf(shl_out, fmt, va);
va_end(va);
shf_putchar('\n', shl_out);
shf_flush(shl_out);
}
void
#ifdef HAVE_PROTOTYPES
bi_errorf(const char *fmt, ...)
#else
bi_errorf(fmt, va_alist)
const char *fmt;
va_dcl
#endif
{
va_list va;
shl_stdout_ok = 0;
exstat = 1;
if (*fmt) {
error_prefix(TRUE);
if (builtin_argv0)
shf_fprintf(shl_out, "%s: ", builtin_argv0);
SH_VA_START(va, fmt);
shf_vfprintf(shl_out, fmt, va);
va_end(va);
shf_putchar('\n', shl_out);
}
shf_flush(shl_out);
if ((builtin_flag & SPEC_BI)
|| (Flag(FPOSIX) && (builtin_flag & KEEPASN)))
{
builtin_argv0 = (char *) 0;
unwind(LERROR);
}
}
void
#ifdef HAVE_PROTOTYPES
internal_errorf(int jump, const char *fmt, ...)
#else
internal_errorf(jump, fmt, va_alist)
int jump;
const char *fmt;
va_dcl
#endif
{
va_list va;
error_prefix(TRUE);
shf_fprintf(shl_out, "internal error: ");
SH_VA_START(va, fmt);
shf_vfprintf(shl_out, fmt, va);
va_end(va);
shf_putchar('\n', shl_out);
shf_flush(shl_out);
if (jump)
unwind(LERROR);
}
void
error_prefix(fileline)
int fileline;
{
if (!fileline || !source || !source->file
|| strcmp(source->file, kshname) != 0)
shf_fprintf(shl_out, "%s: ", kshname + (*kshname == '-'));
if (fileline && source && source->file != NULL) {
shf_fprintf(shl_out, "%s[%d]: ", source->file,
source->errline > 0 ? source->errline : source->line);
source->errline = 0;
}
}
void
#ifdef HAVE_PROTOTYPES
shellf(const char *fmt, ...)
#else
shellf(fmt, va_alist)
const char *fmt;
va_dcl
#endif
{
va_list va;
if (!initio_done)
return;
SH_VA_START(va, fmt);
shf_vfprintf(shl_out, fmt, va);
va_end(va);
shf_flush(shl_out);
}
void
#ifdef HAVE_PROTOTYPES
shprintf(const char *fmt, ...)
#else
shprintf(fmt, va_alist)
const char *fmt;
va_dcl
#endif
{
va_list va;
if (!shl_stdout_ok)
internal_errorf(1, "shl_stdout not valid");
SH_VA_START(va, fmt);
shf_vfprintf(shl_stdout, fmt, va);
va_end(va);
}
#ifdef KSH_DEBUG
static struct shf *kshdebug_shf;
void
kshdebug_init_()
{
if (kshdebug_shf)
shf_close(kshdebug_shf);
kshdebug_shf = shf_open("/tmp/ksh-debug.log",
O_WRONLY|O_APPEND|O_CREAT, 0600,
SHF_WR|SHF_MAPHI);
if (kshdebug_shf) {
shf_fprintf(kshdebug_shf, "\nNew shell[pid %d]\n", getpid());
shf_flush(kshdebug_shf);
}
}
void
# ifdef HAVE_PROTOTYPES
kshdebug_printf_(const char *fmt, ...)
# else
kshdebug_printf_(fmt, va_alist)
const char *fmt;
va_dcl
# endif
{
va_list va;
if (!kshdebug_shf)
return;
SH_VA_START(va, fmt);
shf_fprintf(kshdebug_shf, "[%d] ", getpid());
shf_vfprintf(kshdebug_shf, fmt, va);
va_end(va);
shf_flush(kshdebug_shf);
}
void
kshdebug_dump_(str, mem, nbytes)
const char *str;
const void *mem;
int nbytes;
{
int i, j;
int nprow = 16;
if (!kshdebug_shf)
return;
shf_fprintf(kshdebug_shf, "[%d] %s:\n", getpid(), str);
for (i = 0; i < nbytes; i += nprow) {
char c = '\t';
for (j = 0; j < nprow && i + j < nbytes; j++) {
shf_fprintf(kshdebug_shf, "%c%02x",
c, ((const unsigned char *) mem)[i + j]);
c = ' ';
}
shf_fprintf(kshdebug_shf, "\n");
}
shf_flush(kshdebug_shf);
}
#endif
int
can_seek(fd)
int fd;
{
struct stat statb;
return fstat(fd, &statb) == 0 && !S_ISREG(statb.st_mode) ?
SHF_UNBUF : 0;
}
struct shf	shf_iob[3];
void
initio()
{
shf_fdopen(1, SHF_WR, shl_stdout);
shf_fdopen(2, SHF_WR, shl_out);
shf_fdopen(2, SHF_WR, shl_spare);
initio_done = 1;
kshdebug_init();
}
int
ksh_dup2(ofd, nfd, errok)
int ofd;
int nfd;
int errok;
{
int ret = dup2(ofd, nfd);
if (ret < 0 && errno != EBADF && !errok)
errorf("too many files open in shell");
#ifdef DUP2_BROKEN
if (ret >= 0)
(void) fcntl(nfd, F_SETFD, 0);
#endif
return ret;
}
int
savefd(fd, noclose)
int fd;
int noclose;
{
int nfd;
if (fd < FDBASE) {
nfd = ksh_dupbase(fd, FDBASE);
if (nfd < 0)
if (errno == EBADF)
return -1;
else
errorf("too many files open in shell");
if (!noclose)
close(fd);
} else
nfd = fd;
fd_clexec(nfd);
return nfd;
}
void
restfd(fd, ofd)
int fd, ofd;
{
if (fd == 2)
shf_flush(&shf_iob[fd]);
if (ofd < 0)
close(fd);
else {
ksh_dup2(ofd, fd, TRUE);
close(ofd);
}
}
void
openpipe(pv)
register int *pv;
{
if (pipe(pv) < 0)
errorf("can't create pipe - try again");
pv[0] = savefd(pv[0], 0);
pv[1] = savefd(pv[1], 0);
}
void
closepipe(pv)
register int *pv;
{
close(pv[0]);
close(pv[1]);
}
int
check_fd(name, mode, emsgp)
char *name;
int mode;
const char **emsgp;
{
int fd, fl;
if (isdigit(name[0]) && !name[1]) {
fd = name[0] - '0';
if ((fl = fcntl(fd = name[0] - '0', F_GETFL, 0)) < 0) {
if (emsgp)
*emsgp = "bad file descriptor";
return -1;
}
fl &= O_ACCMODE;
#ifdef OS2
if (mode == W_OK ) {
if (setmode(fd, O_TEXT) == -1) {
if (emsgp)
*emsgp = "couldn't set write mode";
return -1;
}
} else if (mode == R_OK)
if (setmode(fd, O_BINARY) == -1) {
if (emsgp)
*emsgp = "couldn't set read mode";
return -1;
}
#else
if (!(mode & X_OK) && fl != O_RDWR
&& (((mode & R_OK) && fl != O_RDONLY)
|| ((mode & W_OK) && fl != O_WRONLY)))
{
if (emsgp)
*emsgp = (fl == O_WRONLY) ?
"fd not open for reading"
: "fd not open for writing";
return -1;
}
#endif
return fd;
}
#ifdef KSH
else if (name[0] == 'p' && !name[1])
return coproc_getfd(mode, emsgp);
#endif
if (emsgp)
*emsgp = "illegal file descriptor name";
return -1;
}
#ifdef KSH
void
coproc_init()
{
coproc.read = coproc.readw = coproc.write = -1;
coproc.njobs = 0;
coproc.id = 0;
}
void
coproc_read_close(fd)
int fd;
{
if (coproc.read >= 0 && fd == coproc.read) {
coproc_readw_close(fd);
close(coproc.read);
coproc.read = -1;
}
}
void
coproc_readw_close(fd)
int fd;
{
if (coproc.readw >= 0 && coproc.read >= 0 && fd == coproc.read) {
close(coproc.readw);
coproc.readw = -1;
}
}
void
coproc_write_close(fd)
int fd;
{
if (coproc.write >= 0 && fd == coproc.write) {
close(coproc.write);
coproc.write = -1;
}
}
int
coproc_getfd(mode, emsgp)
int mode;
const char **emsgp;
{
int fd = (mode & R_OK) ? coproc.read : coproc.write;
if (fd >= 0)
return fd;
if (emsgp)
*emsgp = "no coprocess";
return -1;
}
void
coproc_cleanup(reuse)
int reuse;
{
if (!reuse || coproc.readw < 0 || coproc.read < 0) {
if (coproc.read >= 0) {
close(coproc.read);
coproc.read = -1;
}
if (coproc.readw >= 0) {
close(coproc.readw);
coproc.readw = -1;
}
}
if (coproc.write >= 0) {
close(coproc.write);
coproc.write = -1;
}
}
#endif
struct temp *
maketemp(ap, type, tlist)
Area *ap;
Temp_type type;
struct temp **tlist;
{
static unsigned int inc;
struct temp *tp;
int len;
int fd;
char *path;
const char *dir;
dir = tmpdir ? tmpdir : "/tmp";
len = strlen(dir) + 3 + 20 + 20 + 1;
tp = (struct temp *) alloc(sizeof(struct temp) + len, ap);
tp->name = path = (char *) &tp[1];
tp->shf = (struct shf *) 0;
tp->type = type;
while (1) {
shf_snprintf(path, len, "%s/sh%05u.%03x",
dir, (unsigned) procpid, inc++);
fd = open(path, O_RDWR|O_CREAT|O_EXCL|O_TRUNC, 0600);
if (fd >= 0) {
tp->shf = shf_fdopen(fd, SHF_WR, (struct shf *) 0);
break;
}
if (errno != EINTR
#ifdef EEXIST
&& errno != EEXIST
#endif
#ifdef EISDIR
&& errno != EISDIR
#endif
)
break;
}
tp->next = NULL;
tp->pid = procpid;
tp->next = *tlist;
*tlist = tp;
return tp;
}