#include "memory_.h"
#include "string_.h"
#include "ghost.h"
#include "imemory.h"
#include "interp.h"
#include "iutil.h"
#include "main.h"
#include "ostack.h"
#include "store.h"
#include "gspaint.h"
int gs_server_initialize(int fno_stdin, int fno_stdout, int fno_stderr,
const char *init_str);
int gs_server_run_string(const char *str, int *exit_code_ptr,
char *errstr, int errstr_max_len,
int *errstr_len_ptr);
int gs_server_run_files(const char **file_names, int permanent,
int *exit_code_ptr, char *errstr,
int errstr_max_len, int *errstr_len_ptr);
int gs_server_terminate();
#if 0
#include <fcntl.h>
#include <sys/stat.h>
int
main(int argc, char *argv[])
{
int code, exit_code;
#define emax 50
char errstr[emax + 1];
int errlen;
static const char *fnames[] =
{"golfer.eps", 0};
FILE *cin = fopen("stdin.tmp", "w+");
int sout = open("stdout.tmp", O_WRONLY | O_CREAT | O_TRUNC,
S_IREAD | S_IWRITE);
int serr = open("stderr.tmp", O_WRONLY | O_CREAT | O_TRUNC,
S_IREAD | S_IWRITE);
code = gs_server_initialize(fileno(cin), sout, serr,
"/fubar 42 def");
fprintf(stdout, "init: code %d\n", code);
if (code < 0)
goto x;
code = gs_server_run_string("fubar == flush", &exit_code,
errstr, emax, &errlen);
fprintf(stdout, "print: code %d\n", code);
if (code < 0)
goto x;
code = gs_server_run_files(fnames, 0, &exit_code,
errstr, emax, &errlen);
fprintf(stdout, "golfer: code %d\n", code);
if (code < 0)
goto x;
errlen = 0;
code = gs_server_run_string("fubar 0 div", &exit_code,
errstr, emax, &errlen);
errstr[errlen] = 0;
fprintf(stdout, "0 div: code %d object %s\n", code, errstr);
errlen = 0;
code = gs_server_run_string("xxx", &exit_code,
errstr, emax, &errlen);
errstr[errlen] = 0;
fprintf(stdout, "undef: code %d object %s\n", code, errstr);
x:code = gs_server_terminate();
fprintf(stdout, "end: code %d\n", code);
fflush(stdout);
close(serr);
close(sout);
fclose(cin);
return code;
}
#endif
private int job_begin(void);
private int job_end(void);
private void errstr_report(ref *, char *, int, int *);
int
gs_server_initialize(int fno_stdin, int fno_stdout, int fno_stderr,
const char *init_str)
{
int code, exit_code;
int errstr_len;
FILE *c_stdin, *c_stdout, *c_stderr;
c_stdin = fdopen(fno_stdin, "r");
if (c_stdin == NULL)
return -1;
c_stdout = fdopen(fno_stdout, "w");
if (c_stdout == NULL)
return -1;
c_stderr = fdopen(fno_stderr, "w");
if (c_stderr == NULL)
return -1;
if ((code = gs_init0(c_stdin, c_stdout, c_stderr, 0)) < 0 ||
(code = gs_init1()) < 0 ||
(code = gs_init2()) < 0
)
return code;
code = gs_server_run_string("/QUIET true def /NOPAUSE true def",
&exit_code,
(char *)0, 0, &errstr_len);
if (code < 0)
return code;
return (init_str == NULL ? 0 :
gs_server_run_string(init_str, &exit_code,
(char *)0, 0, &errstr_len));
}
int
gs_server_run_string(const char *str, int *exit_code_ptr,
char *errstr, int errstr_max_len, int *errstr_len_ptr)
{
ref error_object;
int code;
make_tasv(&error_object, t_string, 0, 0, bytes, 0);
code = gs_run_string(str, 0, exit_code_ptr, &error_object);
if (code < 0)
errstr_report(&error_object, errstr, errstr_max_len,
errstr_len_ptr);
return code;
}
int
gs_server_run_files(const char **file_names, int permanent,
int *exit_code_ptr, char *errstr, int errstr_max_len, int *errstr_len_ptr)
{
int code = 0;
ref error_object;
const char **pfn;
if (!permanent)
job_begin();
make_tasv(&error_object, t_string, 0, 0, bytes, 0);
for (pfn = file_names; *pfn != NULL && code == 0; pfn++)
code = gs_run_file(*pfn, 0, exit_code_ptr, &error_object);
if (!permanent)
job_end();
if (code < 0)
errstr_report(&error_object, errstr, errstr_max_len,
errstr_len_ptr);
return code;
}
int
gs_server_terminate()
{
gs_finit(0, 0);
return 0;
}
private ref job_save;
extern int zsave(os_ptr), zrestore(os_ptr);
private int
job_begin()
{
int code;
extern gs_state *igs;
if ((code = gs_erasepage(igs)) < 0)
return code;
code = zsave(osp);
if (code == 0)
job_save = *osp--;
return code;
}
private int
job_end()
{
gs_interp_reset();
*++osp = job_save;
return zrestore(osp);
}
private void
errstr_report(ref * perror_object, char *errstr, int errstr_max_len,
int *errstr_len_ptr)
{
int code = obj_cvs(perror_object, (byte *) errstr,
(uint) errstr_max_len, (uint *) errstr_len_ptr,
false);
if (code < 0) {
const char *ustr = "[unprintable]";
int len = min(strlen(ustr), errstr_max_len);
memcpy(errstr, ustr, len);
*errstr_len_ptr = len;
}
}