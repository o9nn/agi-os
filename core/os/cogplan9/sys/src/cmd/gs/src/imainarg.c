#include "ctype_.h"
#include "memory_.h"
#include "string_.h"
#include <stdlib.h>
#include "ghost.h"
#include "gp.h"
#include "gsargs.h"
#include "gscdefs.h"
#include "gsmalloc.h"
#include "gsmdebug.h"
#include "gxdevice.h"
#include "gxdevmem.h"
#include "gsdevice.h"
#include "stream.h"
#include "ierrors.h"
#include "estack.h"
#include "ialloc.h"
#include "strimpl.h"
#include "sfilter.h"
#include "ostack.h"
#include "iscan.h"
#include "iconf.h"
#include "imain.h"
#include "imainarg.h"
#include "iapi.h"
#include "iminst.h"
#include "iname.h"
#include "store.h"
#include "files.h"
#include "interp.h"
#include "iutil.h"
#include "ivmspace.h"
#include "vdtrace.h"
extern int zflush(i_ctx_t *);
extern int zflushpage(i_ctx_t *);
#ifndef GS_LIB
# define GS_LIB "GS_LIB"
#endif
#ifndef GS_OPTIONS
# define GS_OPTIONS "GS_OPTIONS"
#endif
#ifndef GS_MAX_LIB_DIRS
# define GS_MAX_LIB_DIRS 25
#endif
#ifndef GS_BUG_MAILBOX
# define GS_BUG_MAILBOX "bug-gs@ghostscript.com"
#endif
#define MAX_BUFFERED_SIZE 1024
#undef puts
#define puts(mem, str) outprintf(mem, "%s\n", str)
#define runInit 1
#define runFlush 2
#define runBuffer 4
private int swproc(gs_main_instance *, const char *, arg_list *);
private int argproc(gs_main_instance *, const char *);
private int run_buffered(gs_main_instance *, const char *);
private int esc_strlen(const char *);
private void esc_strcat(char *, const char *);
private int runarg(gs_main_instance *, const char *, const char *, const char *, int);
private int run_string(gs_main_instance *, const char *, int);
private int run_finish(gs_main_instance *, int, int, ref *);
private int try_stdout_redirect(gs_main_instance * minst,
const char *command, const char *filename);
private void print_help(gs_main_instance *);
private void print_revision(const gs_main_instance *);
private void print_version(const gs_main_instance *);
private void print_usage(const gs_main_instance *);
private void print_devices(const gs_main_instance *);
private void print_emulators(const gs_main_instance *);
private void print_paths(gs_main_instance *);
private void print_help_trailer(const gs_main_instance *);
private FILE *
gs_main_arg_fopen(const char *fname, void *vminst)
{
gs_main_set_lib_paths((gs_main_instance *) vminst);
return lib_fopen(&((gs_main_instance *)vminst)->lib_path,
((gs_main_instance *)vminst)->heap, fname);
}
private void
set_debug_flags(const char *arg, char *flags)
{
byte value = (*arg == '-' ? (++arg, 0) : 0xff);
while (*arg)
flags[*arg++ & 127] = value;
}
int
gs_main_init_with_args(gs_main_instance * minst, int argc, char *argv[])
{
const char *arg;
arg_list args;
int code;
arg_init(&args, (const char **)argv, argc,
gs_main_arg_fopen, (void *)minst);
code = gs_main_init0(minst, 0, 0, 0, GS_MAX_LIB_DIRS);
if (code < 0)
return code;
#ifndef __VMS
{
int len = 0;
int code = gp_getenv(GS_LIB, (char *)0, &len);
if (code < 0) {
char *path = (char *)gs_alloc_bytes(minst->heap, len, "GS_LIB");
gp_getenv(GS_LIB, path, &len);
minst->lib_path.env = path;
}
}
#endif
minst->lib_path.final = gs_lib_default_path;
code = gs_main_set_lib_paths(minst);
if (code < 0)
return code;
{
int i;
bool helping = false;
for (i = 1; i < argc; ++i)
if (!strcmp(argv[i], "--")) {
helping = false;
break;
} else if (!strcmp(argv[i], "--help")) {
print_help(minst);
helping = true;
} else if (!strcmp(argv[i], "--version")) {
print_version(minst);
puts(minst->heap, "");
helping = true;
}
if (helping)
return e_Info;
}
minst->run_start = true;
{
int len = 0;
int code = gp_getenv(GS_OPTIONS, (char *)0, &len);
if (code < 0) {
char *opts =
(char *)gs_alloc_bytes(minst->heap, len, "GS_OPTIONS");
gp_getenv(GS_OPTIONS, opts, &len);
if (arg_push_memory_string(&args, opts, minst->heap))
return e_Fatal;
}
}
while ((arg = arg_next(&args, &code)) != 0) {
switch (*arg) {
case '-':
code = swproc(minst, arg, &args);
if (code < 0)
return code;
if (code > 0)
outprintf(minst->heap, "Unknown switch %s - ignoring\n", arg);
break;
default:
code = argproc(minst, arg);
if (code < 0)
return code;
}
}
if (code < 0)
return code;
code = gs_main_init2(minst);
if (code < 0)
return code;
if (!minst->run_start)
return e_Quit;
return code ;
}
int
gs_main_run_start(gs_main_instance * minst)
{
return run_string(minst, "systemdict /start get exec", runFlush);
}
private int
swproc(gs_main_instance * minst, const char *arg, arg_list * pal)
{
char sw = arg[1];
ref vtrue;
int code = 0;
#undef initial_enter_name
#define initial_enter_name(nstr, pvalue)\
i_initial_enter_name(minst->i_ctx_p, nstr, pvalue)
make_true(&vtrue);
arg += 2;
switch (sw) {
default:
return 1;
case 0:
minst->heap->gs_lib_ctx->stdin_is_interactive = true;
goto run_stdin;
case '_':
minst->heap->gs_lib_ctx->stdin_is_interactive = false;
run_stdin:
minst->run_start = false;
code = swproc(minst, "-dNOPAUSE", pal);
if (code)
return code;
code = gs_main_init2(minst);
if (code < 0)
return code;
code = run_string(minst, ".runstdin", runFlush);
if (code < 0)
return code;
break;
case '-':
case '+':
pal->expand_ats = false;
case '@':
{
const char *psarg = arg_next(pal, &code);
if (code < 0)
return e_Fatal;
if (psarg == 0) {
outprintf(minst->heap, "Usage: gs ... -%c file.ps arg1 ... argn\n", sw);
arg_finit(pal);
return e_Fatal;
}
psarg = arg_copy(psarg, minst->heap);
if (psarg == NULL)
return e_Fatal;
code = gs_main_init2(minst);
if (code < 0)
return code;
code = run_string(minst, "userdict/ARGUMENTS[", 0);
if (code < 0)
return code;
while ((arg = arg_next(pal, &code)) != 0) {
char *fname = arg_copy(arg, minst->heap);
if (fname == NULL)
return e_Fatal;
code = runarg(minst, "", fname, "", runInit);
if (code < 0)
return code;
}
if (code < 0)
return e_Fatal;
runarg(minst, "]put", psarg, ".runfile", runInit | runFlush);
return e_Quit;
}
case 'A':
switch (*arg) {
case 0:
gs_alloc_debug = 1;
break;
case '-':
gs_alloc_debug = 0;
break;
default:
puts(minst->heap, "-A may only be followed by -");
return e_Fatal;
}
break;
case 'B':
if (*arg == '-')
minst->run_buffer_size = 0;
else {
uint bsize;
if (sscanf((const char *)arg, "%u", &bsize) != 1 ||
bsize <= 0 || bsize > MAX_BUFFERED_SIZE
) {
outprintf(minst->heap,
"-B must be followed by - or size between 1 and %u\n",
MAX_BUFFERED_SIZE);
return e_Fatal;
}
minst->run_buffer_size = bsize;
}
break;
case 'c':
{
bool ats = pal->expand_ats;
code = gs_main_init2(minst);
if (code < 0)
return code;
pal->expand_ats = false;
while ((arg = arg_next(pal, &code)) != 0) {
char *sarg;
if (arg[0] == '@' ||
(arg[0] == '-' && !isdigit(arg[1]))
)
break;
sarg = arg_copy(arg, minst->heap);
if (sarg == NULL)
return e_Fatal;
code = runarg(minst, "", sarg, ".runstring", 0);
if (code < 0)
return code;
}
if (code < 0)
return e_Fatal;
if (arg != 0) {
char *p = arg_copy(arg, minst->heap);
if (p == NULL)
return e_Fatal;
arg_push_string(pal, p);
}
pal->expand_ats = ats;
break;
}
case 'E':
switch (*arg) {
case 0:
gs_log_errors = 1;
break;
case '-':
gs_log_errors = 0;
break;
default:
puts(minst->heap, "-E may only be followed by -");
return e_Fatal;
}
break;
case 'f':
if (*arg != 0) {
code = argproc(minst, arg);
if (code < 0)
return code;
}
break;
case 'F':
if (!*arg) {
puts(minst->heap, "-F requires a file name");
return e_Fatal;
} {
uint bsize = minst->run_buffer_size;
minst->run_buffer_size = 1;
code = argproc(minst, arg);
minst->run_buffer_size = bsize;
if (code < 0)
return code;
}
break;
case 'g':
{
long width, height;
ref value;
if ((code = gs_main_init1(minst)) < 0)
return code;
if (sscanf((const char *)arg, "%ldx%ld", &width, &height) != 2) {
puts(minst->heap, "-g must be followed by <width>x<height>");
return e_Fatal;
}
make_int(&value, width);
initial_enter_name("DEVICEWIDTH", &value);
make_int(&value, height);
initial_enter_name("DEVICEHEIGHT", &value);
initial_enter_name("FIXEDMEDIA", &vtrue);
break;
}
case 'h':
case '?':
print_help(minst);
return e_Info;
case 'I':
{
char *path = arg_copy(arg, minst->heap);
if (path == NULL)
return e_Fatal;
gs_main_add_lib_path(minst, path);
}
break;
case 'K':
{
long msize = 0;
gs_malloc_memory_t *rawheap = gs_malloc_wrapped_contents(minst->heap);
sscanf((const char *)arg, "%ld", &msize);
if (msize <= 0 || msize > max_long >> 10) {
outprintf(minst->heap, "-K<numK> must have 1 <= numK <= %ld\n",
max_long >> 10);
return e_Fatal;
}
rawheap->limit = msize << 10;
}
break;
case 'M':
{
unsigned msize = 0;
sscanf((const char *)arg, "%u", &msize);
#if arch_ints_are_short
if (msize <= 0 || msize >= 64) {
puts(minst->heap, "-M must be between 1 and 63");
return e_Fatal;
}
#endif
minst->memory_chunk_size = msize << 10;
}
break;
case 'N':
{
unsigned nsize = 0;
sscanf((const char *)arg, "%d", &nsize);
#if arch_ints_are_short
if (nsize < 2 || nsize > 64) {
puts(minst->heap, "-N must be between 2 and 64");
return e_Fatal;
}
#endif
minst->name_table_size = (ulong) nsize << 10;
}
break;
case 'P':
if (!strcmp(arg, ""))
minst->search_here_first = true;
else if (!strcmp(arg, "-"))
minst->search_here_first = false;
else {
puts(minst->heap, "Only -P or -P- is allowed.");
return e_Fatal;
}
break;
case 'q':
if ((code = gs_main_init1(minst)) < 0)
return code;
initial_enter_name("QUIET", &vtrue);
break;
case 'r':
{
float xres, yres;
ref value;
if ((code = gs_main_init1(minst)) < 0)
return code;
switch (sscanf((const char *)arg, "%fx%f", &xres, &yres)) {
default:
puts(minst->heap, "-r must be followed by <res> or <xres>x<yres>");
return e_Fatal;
case 1:
yres = xres;
case 2:
make_real(&value, xres);
initial_enter_name("DEVICEXRESOLUTION", &value);
make_real(&value, yres);
initial_enter_name("DEVICEYRESOLUTION", &value);
initial_enter_name("FIXEDRESOLUTION", &vtrue);
}
break;
}
case 'D':
case 'd':
case 'S':
case 's':
{
char *adef = arg_copy(arg, minst->heap);
char *eqp;
bool isd = (sw == 'D' || sw == 'd');
ref value;
if (adef == NULL)
return e_Fatal;
eqp = strchr(adef, '=');
if (eqp == NULL)
eqp = strchr(adef, '#');
if ((code = gs_main_init1(minst)) < 0)
return code;
if (eqp == adef) {
puts(minst->heap, "Usage: -dname, -dname=token, -sname=string");
return e_Fatal;
}
if (eqp == NULL) {
if (isd)
make_true(&value);
else
make_empty_string(&value, a_readonly);
} else {
int code;
i_ctx_t *i_ctx_p = minst->i_ctx_p;
uint space = icurrent_space;
*eqp++ = 0;
ialloc_set_space(idmemory, avm_system);
if (isd) {
stream astream;
scanner_state state;
s_init(&astream, NULL);
sread_string(&astream,
(const byte *)eqp, strlen(eqp));
scanner_state_init(&state, false);
code = scan_token(minst->i_ctx_p, &astream, &value,
&state);
if (code) {
puts(minst->heap, "-dname= must be followed by a valid token");
return e_Fatal;
}
if (r_has_type_attrs(&value, t_name,
a_executable)) {
ref nsref;
name_string_ref(minst->heap, &value, &nsref);
#define string_is(nsref, str, len)\
(r_size(&(nsref)) == (len) &&\
!strncmp((const char *)(nsref).value.const_bytes, str, (len)))
if (string_is(nsref, "null", 4))
make_null(&value);
else if (string_is(nsref, "true", 4))
make_true(&value);
else if (string_is(nsref, "false", 5))
make_false(&value);
else {
puts(minst->heap,
"-dvar=name requires name=null, true, or false");
return e_Fatal;
}
#undef name_is_string
}
} else {
int len = strlen(eqp);
char *str =
(char *)gs_alloc_bytes(minst->heap,
(uint) len, "-s");
if (str == 0) {
lprintf("Out of memory!\n");
return e_Fatal;
}
memcpy(str, eqp, len);
make_const_string(&value,
a_readonly | avm_foreign,
len, (const byte *)str);
if ((code = try_stdout_redirect(minst, adef, eqp)) < 0)
return code;
}
ialloc_set_space(idmemory, space);
}
initial_enter_name(adef, &value);
break;
}
case 'T':
set_debug_flags(arg, vd_flags);
break;
case 'u':
if (!*arg) {
puts(minst->heap, "-u requires a name to undefine.");
return e_Fatal;
}
if ((code = gs_main_init1(minst)) < 0)
return code;
i_initial_remove_name(minst->i_ctx_p, arg);
break;
case 'v':
print_revision(minst);
return e_Info;
case 'X':
code = gs_main_init2(minst);
if (code < 0)
return code;
{
int xec;
ref xeo;
#define start_x()\
gs_main_run_string_begin(minst, 1, &xec, &xeo)
#define run_x(str)\
gs_main_run_string_continue(minst, str, strlen(str), 1, &xec, &xeo)
#define stop_x()\
gs_main_run_string_end(minst, 1, &xec, &xeo)
start_x();
run_x("\216\003abc");
run_x("== flush\n");
stop_x();
}
return e_Quit;
case 'Z':
set_debug_flags(arg, gs_debug);
break;
}
return 0;
}
private int
esc_strlen(const char *str)
{
return strlen(str) * 2 + 2;
}
private void
esc_strcat(char *dest, const char *src)
{
char *d = dest + strlen(dest);
const char *p;
static const char *const hex = "0123456789abcdef";
*d++ = '<';
for (p = src; *p; p++) {
byte c = (byte) * p;
*d++ = hex[c >> 4];
*d++ = hex[c & 0xf];
}
*d++ = '>';
*d = 0;
}
private int
argproc(gs_main_instance * minst, const char *arg)
{
int code = gs_main_init1(minst);
char *filearg;
if (code < 0)
return code;
filearg = arg_copy(arg, minst->heap);
if (filearg == NULL)
return e_Fatal;
if (minst->run_buffer_size) {
return run_buffered(minst, filearg);
} else {
return runarg(minst, "", filearg, ".runfile", runInit | runFlush);
}
}
private int
run_buffered(gs_main_instance * minst, const char *arg)
{
FILE *in = gp_fopen(arg, gp_fmode_rb);
int exit_code;
ref error_object;
int code;
if (in == 0) {
outprintf(minst->heap, "Unable to open %s for reading", arg);
return_error(e_invalidfileaccess);
}
code = gs_main_init2(minst);
if (code < 0)
return code;
code = gs_main_run_string_begin(minst, minst->user_errors,
&exit_code, &error_object);
if (!code) {
char buf[MAX_BUFFERED_SIZE];
int count;
code = e_NeedInput;
while ((count = fread(buf, 1, minst->run_buffer_size, in)) > 0) {
code = gs_main_run_string_continue(minst, buf, count,
minst->user_errors,
&exit_code, &error_object);
if (code != e_NeedInput)
break;
}
if (code == e_NeedInput) {
code = gs_main_run_string_end(minst, minst->user_errors,
&exit_code, &error_object);
}
}
fclose(in);
zflush(minst->i_ctx_p);
zflushpage(minst->i_ctx_p);
return run_finish(minst, code, exit_code, &error_object);
}
private int
runarg(gs_main_instance * minst, const char *pre, const char *arg,
const char *post, int options)
{
int len = strlen(pre) + esc_strlen(arg) + strlen(post) + 1;
int code;
char *line;
if (options & runInit) {
code = gs_main_init2(minst);
if (code < 0)
return code;
}
line = (char *)gs_alloc_bytes(minst->heap, len, "argproc");
if (line == 0) {
lprintf("Out of memory!\n");
return_error(e_VMerror);
}
strcpy(line, pre);
esc_strcat(line, arg);
strcat(line, post);
minst->i_ctx_p->starting_arg_file = true;
code = run_string(minst, line, options);
minst->i_ctx_p->starting_arg_file = false;
return code;
}
private int
run_string(gs_main_instance * minst, const char *str, int options)
{
int exit_code;
ref error_object;
int code = gs_main_run_string(minst, str, minst->user_errors,
&exit_code, &error_object);
if ((options & runFlush) || code != 0) {
zflush(minst->i_ctx_p);
zflushpage(minst->i_ctx_p);
}
return run_finish(minst, code, exit_code, &error_object);
}
private int
run_finish(gs_main_instance *minst, int code, int exit_code,
ref * perror_object)
{
switch (code) {
case e_Quit:
case 0:
break;
case e_Fatal:
eprintf1("Unrecoverable error, exit code %d\n", exit_code);
break;
default:
gs_main_dump_stack(minst, code, perror_object);
}
return code;
}
private int
try_stdout_redirect(gs_main_instance * minst,
const char *command, const char *filename)
{
if (strcmp(command, "stdout") == 0) {
minst->heap->gs_lib_ctx->stdout_to_stderr = 0;
minst->heap->gs_lib_ctx->stdout_is_redirected = 0;
if (minst->heap->gs_lib_ctx->fstdout2
&& (minst->heap->gs_lib_ctx->fstdout2 != minst->heap->gs_lib_ctx->fstdout)
&& (minst->heap->gs_lib_ctx->fstdout2 != minst->heap->gs_lib_ctx->fstderr)) {
fclose(minst->heap->gs_lib_ctx->fstdout2);
minst->heap->gs_lib_ctx->fstdout2 = (FILE *)NULL;
}
if ( (filename != 0) && strlen(filename) &&
strcmp(filename, "-") && strcmp(filename, "%stdout") ) {
if (strcmp(filename, "%stderr") == 0) {
minst->heap->gs_lib_ctx->stdout_to_stderr = 1;
}
else if ((minst->heap->gs_lib_ctx->fstdout2 =
fopen(filename, "w")) == (FILE *)NULL)
return_error(e_invalidfileaccess);
minst->heap->gs_lib_ctx->stdout_is_redirected = 1;
}
return 0;
}
return 1;
}
private const char help_usage1[] = "\
Usage: gs [switches] [file1.ps file2.ps ...]\n\
Most frequently used switches: (you can use # in place of =)\n\
-dNOPAUSE no pause after page | -q `quiet', fewer messages\n\
-g<width>x<height> page size in pixels | -r<res> pixels/inch resolution\n";
private const char help_usage2[] = "\
-sDEVICE=<devname> select device | -dBATCH exit after last file\n\
-sOutputFile=<file> select output file: - for stdout, |command for pipe,\n\
embed %d or %ld for page #\n";
private const char help_trailer[] = "\
For more information, see %s.\n\
Report bugs to %s, using the form in Bug-form.htm.\n";
private const char help_devices[] = "Available devices:";
private const char help_default_device[] = "Default output device:";
private const char help_emulators[] = "Input formats:";
private const char help_paths[] = "Search path:";
private void
print_help(gs_main_instance * minst)
{
print_revision(minst);
print_usage(minst);
print_emulators(minst);
print_devices(minst);
print_paths(minst);
if (gs_init_string_sizeof > 0) {
outprintf(minst->heap, "Initialization files are compiled into the executable.\n");
}
print_help_trailer(minst);
}
private void
print_revision(const gs_main_instance *minst)
{
printf_program_ident(minst->heap, gs_product, gs_revision);
outprintf(minst->heap, " (%d-%02d-%02d)\n%s\n",
(int)(gs_revisiondate / 10000),
(int)(gs_revisiondate / 100 % 100),
(int)(gs_revisiondate % 100),
gs_copyright);
}
private void
print_version(const gs_main_instance *minst)
{
printf_program_ident(minst->heap, NULL, gs_revision);
}
private void
print_usage(const gs_main_instance *minst)
{
outprintf(minst->heap, "%s", help_usage1);
outprintf(minst->heap, "%s", help_usage2);
}
private int
cmpstr(const void *v1, const void *v2)
{
return strcmp( *(char * const *)v1, *(char * const *)v2 );
}
private void
print_devices(const gs_main_instance *minst)
{
outprintf(minst->heap, "%s", help_default_device);
outprintf(minst->heap, " %s\n", gs_devicename(gs_getdevice(0)));
outprintf(minst->heap, "%s", help_devices);
{
int i;
int pos = 100;
const gx_device *pdev;
const char **names;
size_t ndev = 0;
for (i = 0; (pdev = gs_getdevice(i)) != 0; i++)
;
ndev = (size_t)i;
names = (const char **)gs_alloc_bytes(minst->heap, ndev * sizeof(const char*), "print_devices");
if (names == (const char **)NULL) {
for (i = 0; (pdev = gs_getdevice(i)) != 0; i++) {
const char *dname = gs_devicename(pdev);
int len = strlen(dname);
if (pos + 1 + len > 76)
outprintf(minst->heap, "\n  "), pos = 2;
outprintf(minst->heap, " %s", dname);
pos += 1 + len;
}
}
else {
for (i = 0; (pdev = gs_getdevice(i)) != 0; i++)
names[i] = gs_devicename(pdev);
qsort((void*)names, ndev, sizeof(const char*), cmpstr);
for (i = 0; i < ndev; i++) {
int len = strlen(names[i]);
if (pos + 1 + len > 76)
outprintf(minst->heap, "\n  "), pos = 2;
outprintf(minst->heap, " %s", names[i]);
pos += 1 + len;
}
gs_free(minst->heap, (char *)names, ndev * sizeof(const char*), 1, "print_devices");
}
}
outprintf(minst->heap, "\n");
}
private void
print_emulators(const gs_main_instance *minst)
{
outprintf(minst->heap, "%s", help_emulators);
{
const ref *pes;
for (pes = gs_emulator_name_array;
pes->value.const_bytes != 0; pes++
)
outprintf(minst->heap, " %s", (const char *)pes->value.const_bytes);
}
outprintf(minst->heap, "\n");
}
private void
print_paths(gs_main_instance * minst)
{
outprintf(minst->heap, "%s", help_paths);
gs_main_set_lib_paths(minst);
{
uint count = r_size(&minst->lib_path.list);
uint i;
int pos = 100;
char fsepr[3];
fsepr[0] = ' ', fsepr[1] = gp_file_name_list_separator,
fsepr[2] = 0;
for (i = 0; i < count; ++i) {
const ref *prdir =
minst->lib_path.list.value.refs + i;
uint len = r_size(prdir);
const char *sepr = (i == count - 1 ? "" : fsepr);
if (1 + pos + strlen(sepr) + len > 76)
outprintf(minst->heap, "\n  "), pos = 2;
outprintf(minst->heap, " ");
{
const char *p = (const char *)prdir->value.bytes;
uint j;
for (j = len; j; j--)
outprintf(minst->heap, "%c", *p++);
}
outprintf(minst->heap, "%s", sepr);
pos += 1 + len + strlen(sepr);
}
}
outprintf(minst->heap, "\n");
}
private void
print_help_trailer(const gs_main_instance *minst)
{
char buffer[gp_file_name_sizeof];
const char *use_htm = "Use.htm", *p = buffer;
uint blen = sizeof(buffer);
if (gp_file_name_combine(gs_doc_directory, strlen(gs_doc_directory),
use_htm, strlen(use_htm), false, buffer, &blen) != gp_combine_success)
p = use_htm;
outprintf(minst->heap, help_trailer, p, GS_BUG_MAILBOX);
}