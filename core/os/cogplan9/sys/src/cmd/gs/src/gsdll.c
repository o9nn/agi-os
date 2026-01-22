#ifdef _Windows
#include <windows.h>
#endif
#ifdef __OS2__
#define INCL_DOS
#define INCL_WIN
#include <os2.h>
#endif
#include "stdpre.h"
#include "iapi.h"
#include "string_.h"
#include "ierrors.h"
#include "gscdefs.h"
#include "gstypes.h"
#include "iref.h"
#include "iminst.h"
#include "imain.h"
#include "gsdll.h"
#ifdef __MACOS__
extern HWND hwndtext;
#endif
GSDLL_CALLBACK pgsdll_callback = NULL;
static gs_main_instance *pgs_minst = NULL;
private int GSDLLCALL gsdll_old_stdin(void *caller_handle, char *buf, int len);
private int GSDLLCALL gsdll_old_stdout(void *caller_handle, const char *str, int len);
private int GSDLLCALL gsdll_old_stderr(void *caller_handle, const char *str, int len);
private int GSDLLCALL gsdll_old_poll(void *caller_handle);
int GSDLLEXPORT GSDLLAPI
gsdll_init(GSDLL_CALLBACK callback, HWND hwnd, int argc, char * argv[])
{
int code;
if ((code = gsapi_new_instance(&pgs_minst, (void *)1)) < 0)
return -1;
gsapi_set_stdio(pgs_minst,
gsdll_old_stdin, gsdll_old_stdout, gsdll_old_stderr);
gsapi_set_poll(pgs_minst, gsdll_old_poll);
#ifdef __MACOS__
hwndtext=hwnd;
#endif
pgsdll_callback = callback;
code = gsapi_init_with_args(pgs_minst, argc, argv);
if (code == e_Quit) {
gsapi_exit(pgs_minst);
return GSDLL_INIT_QUIT;
}
return code;
}
int GSDLLEXPORT GSDLLAPI
gsdll_execute_begin(void)
{
int exit_code;
return gsapi_run_string_begin(pgs_minst, 0, &exit_code);
}
int GSDLLEXPORT GSDLLAPI
gsdll_execute_cont(const char * str, int len)
{
int exit_code;
int code = gsapi_run_string_continue(pgs_minst, str, len,
0, &exit_code);
if (code == e_NeedInput)
code = 0;
return code;
}
int GSDLLEXPORT GSDLLAPI
gsdll_execute_end(void)
{
int exit_code;
return gsapi_run_string_end(pgs_minst, 0, &exit_code);
}
int GSDLLEXPORT GSDLLAPI
gsdll_exit(void)
{
int code = gsapi_exit(pgs_minst);
gsapi_delete_instance(pgs_minst);
return code;
}
int GSDLLEXPORT GSDLLAPI
gsdll_revision(const char ** product, const char ** copyright,
long * revision, long * revisiondate)
{
if (product)
*product = gs_product;
if (copyright)
*copyright = gs_copyright;
if (revision)
*revision = gs_revision;
if (revisiondate)
*revisiondate = gs_revisiondate;
return 0;
}
private int GSDLLCALL
gsdll_old_stdin(void *caller_handle, char *buf, int len)
{
return (*pgsdll_callback)(GSDLL_STDIN, buf, len);
}
private int GSDLLCALL
gsdll_old_stdout(void *caller_handle, const char *str, int len)
{
return (*pgsdll_callback)(GSDLL_STDOUT, (char *)str, len);
}
private int GSDLLCALL
gsdll_old_stderr(void *caller_handle, const char *str, int len)
{
return (*pgsdll_callback)(GSDLL_STDOUT, (char *)str, len);
}
private int GSDLLCALL
gsdll_old_poll(void *caller_handle)
{
return (*pgsdll_callback)(GSDLL_POLL, NULL, 0);
}