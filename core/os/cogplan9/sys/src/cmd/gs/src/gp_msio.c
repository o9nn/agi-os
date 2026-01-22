#define fprintf UNDEFINE_fprintf
#include "stdio_.h"
#undef fprintf
#include <stdlib.h>
#include "gx.h"
#include "gp.h"
#include "windows_.h"
#include <shellapi.h>
#ifdef __WIN32__
#include <winspool.h>
#endif
#include "gp_mswin.h"
#include "gsdll.h"
#include "stream.h"
#include "gxiodev.h"
int gp_file_is_console(FILE *);
private void win_std_init(void);
private stream_proc_process(win_std_read_process);
private stream_proc_process(win_std_write_process);
private stream_proc_available(win_std_available);
private iodev_proc_init(win_stdio_init);
const gx_io_device gs_iodev_wstdio = {
0, "Special",
{win_stdio_init, iodev_no_open_device,
iodev_no_open_file, iodev_no_fopen, iodev_no_fclose,
iodev_no_delete_file, iodev_no_rename_file,
iodev_no_file_status, iodev_no_enumerate_files
}
};
private int
win_stdio_init(gx_io_device * iodev, gs_memory_t * mem)
{
win_std_init();
return 0;
}
extern const gx_io_device gs_iodev_stdin;
private int
win_stdin_open(gx_io_device * iodev, const char *access, stream ** ps,
gs_memory_t * mem)
{
int code = gs_iodev_stdin.procs.open_device(iodev, access, ps, mem);
stream *s = *ps;
if (code != 1)
return code;
s->procs.process = win_std_read_process;
s->procs.available = win_std_available;
s->file = NULL;
return 0;
}
extern const gx_io_device gs_iodev_stdout;
private int
win_stdout_open(gx_io_device * iodev, const char *access, stream ** ps,
gs_memory_t * mem)
{
int code = gs_iodev_stdout.procs.open_device(iodev, access, ps, mem);
stream *s = *ps;
if (code != 1)
return code;
s->procs.process = win_std_write_process;
s->procs.available = win_std_available;
s->procs.flush = s_std_write_flush;
s->file = NULL;
return 0;
}
extern const gx_io_device gs_iodev_stderr;
private int
win_stderr_open(gx_io_device * iodev, const char *access, stream ** ps,
gs_memory_t * mem)
{
int code = gs_iodev_stderr.procs.open_device(iodev, access, ps, mem);
stream *s = *ps;
if (code != 1)
return code;
s->procs.process = win_std_write_process;
s->procs.available = win_std_available;
s->procs.flush = s_std_write_flush;
s->file = NULL;
return 0;
}
private void
win_std_init(void)
{
if (gp_file_is_console(gs_stdin))
gs_findiodevice((const byte *)"%stdin", 6)->procs.open_device =
win_stdin_open;
if (gp_file_is_console(gs_stdout))
gs_findiodevice((const byte *)"%stdout", 7)->procs.open_device =
win_stdout_open;
if (gp_file_is_console(gs_stderr))
gs_findiodevice((const byte *)"%stderr", 7)->procs.open_device =
win_stderr_open;
}
private int
win_std_read_process(stream_state * st, stream_cursor_read * ignore_pr,
stream_cursor_write * pw, bool last)
{
int count = pw->limit - pw->ptr;
if (count == 0)
return 1;
count = (*pgsdll_callback) (GSDLL_STDIN, pw->ptr + 1, count);
if (count == 0) {
return EOFC;
}
pw->ptr += count;
return 1;
}
private int
win_std_available(register stream * s, long *pl)
{
*pl = -1;
return 0;
}
private int
win_std_write_process(stream_state * st, stream_cursor_read * pr,
stream_cursor_write * ignore_pw, bool last)
{
uint count = pr->limit - pr->ptr;
(*pgsdll_callback) (GSDLL_STDOUT, (char *)(pr->ptr + 1), count);
pr->ptr = pr->limit;
return 0;
}
#if defined(_WIN32) && (defined(_MSC_VER) || defined(_WATCOM_))
#if defined(_CRTAPI2)
int _CRTAPI2
fprintf(FILE * file, const char *fmt,...)
#else
_CRTIMP int __cdecl
fprintf(FILE * file, const char *fmt,...)
#endif
#else
int _Cdecl _FARFUNC
fprintf(FILE _FAR * file, const char *fmt,...)
#endif
{
int count;
va_list args;
va_start(args, fmt);
if (gp_file_is_console(file)) {
char buf[1024];
count = vsprintf(buf, fmt, args);
(*pgsdll_callback) (GSDLL_STDOUT, buf, count);
} else
count = vfprintf(file, fmt, args);
va_end(args);
return count;
}