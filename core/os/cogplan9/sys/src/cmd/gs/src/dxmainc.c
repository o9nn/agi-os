#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#define __PROTOTYPES__
#include "ierrors.h"
#include "iapi.h"
const char start_string[] = "systemdict /start get exec\n";
static int gsdll_stdin(void *instance, char *buf, int len);
static int gsdll_stdout(void *instance, const char *str, int len);
static int gsdll_stdout(void *instance, const char *str, int len);
static int
gsdll_stdin(void *instance, char *buf, int len)
{
return read(fileno(stdin), buf, len);
}
static int
gsdll_stdout(void *instance, const char *str, int len)
{
fwrite(str, 1, len, stdout);
fflush(stdout);
return len;
}
static int
gsdll_stderr(void *instance, const char *str, int len)
{
fwrite(str, 1, len, stderr);
fflush(stderr);
return len;
}
int main(int argc, char *argv[])
{
int exit_status;
int code = 1, code1;
void *instance;
int exit_code;
if ((code = gsapi_new_instance(&instance, NULL)) == 0) {
gsapi_set_stdio(instance, gsdll_stdin, gsdll_stdout, gsdll_stderr);
code = gsapi_init_with_args(instance, argc, argv);
if (code == 0)
code = gsapi_run_string(instance, start_string, 0, &exit_code);
code1 = gsapi_exit(instance);
if (code == 0 || code == e_Quit)
code = code1;
if (code == e_Quit)
code = 0;
gsapi_delete_instance(instance);
}
exit_status = 0;
switch (code) {
case 0:
case e_Info:
case e_Quit:
break;
case e_Fatal:
exit_status = 1;
break;
default:
exit_status = 255;
}
return exit_status;
}