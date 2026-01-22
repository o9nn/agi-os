#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include "error.h"
#include "api-structures.h"
#include "print/print.h"
#include "print/print-util.h"
static void default_error_handler(lg_errinfo *, void *);
static TLS struct
{
lg_error_handler handler;
void *handler_data;
lg_errinfo *errmsg;
} lg_error = { default_error_handler };
#define MAX_SEVERITY_LABEL_SIZE 64
const char *severity_label_by_level[] =
{
"Fatal error", "Error", "Warning", "Info", "Debug", "Trace", "",
NULL
};
static const char libname[] = "link-grammar";
static lg_errinfo *error_queue_resize(lg_errinfo *lge, int len)
{
lge = realloc(lge, (len+2) * sizeof(lg_errinfo));
lge[len+1].text = NULL;
return lge;
}
static int error_queue_len(lg_errinfo *lge)
{
size_t len = 0;
if (lge)
while (NULL != lge[len].text) len++;
return len;
}
static void error_queue_append(lg_errinfo **lge, lg_errinfo *current_error)
{
int n = error_queue_len(*lge);
*lge = error_queue_resize(*lge, n);
current_error->text = strdup(current_error->text);
(*lge)[n] = *current_error;
}
static lg_error_severity message_error_severity(const char *msgtext)
{
for (const char **llp = severity_label_by_level; NULL != *llp; llp++)
{
for (const char *s = *llp, *t = msgtext; ; s++, t++)
{
if ((':' == *t) && (t > msgtext))
{
return (int)(llp - severity_label_by_level + 1);
}
if ((*s != *t) || ('\0' == *s)) break;
}
}
return lg_None;
}
static void lg_error_msg_free(lg_errinfo *lge)
{
free((void *)lge->text);
free((void *)lge->severity_label);
}
lg_error_handler lg_error_set_handler(lg_error_handler f, void *data)
{
const lg_error_handler oldf = lg_error.handler;
lg_error.handler = f;
lg_error.handler_data = data;
return oldf;
}
const void *lg_error_set_handler_data(void * data)
{
const char *old_data = lg_error.handler_data;
lg_error.handler_data = data;
return old_data;
}
int lg_error_printall(lg_error_handler f, void *data)
{
int n = error_queue_len(lg_error.errmsg);
if (0 == n) return 0;
for (lg_errinfo *lge = &lg_error.errmsg[n-1]; lge >= lg_error.errmsg; lge--)
{
if (NULL == f)
default_error_handler(lge, data);
else
f(lg_error.errmsg, data);
lg_error_msg_free(lge);
}
free(lg_error.errmsg);
lg_error.errmsg = NULL;
return n;
}
int lg_error_clearall(void)
{
if (NULL == lg_error.errmsg) return 0;
int nerrors = 0;
for (lg_errinfo *lge = lg_error.errmsg; NULL != lge->text; lge++)
{
nerrors++;
lg_error_msg_free(lge);
}
free(lg_error.errmsg);
lg_error.errmsg = NULL;
return nerrors;
}
char *lg_error_formatmsg(lg_errinfo *lge)
{
dyn_str *s = dyn_str_new();
if (lge->severity < lg_Debug)
append_string(s, "%s: ", libname);
if ((NULL != lge->severity_label) && ('\0' != lge->severity_label[0]))
append_string(s, "%s: ", lge->severity_label);
append_string(s, "%s", lge->text);
return dyn_str_take(s);
}
static TLS dyn_str *outbuf = NULL;
bool lg_error_flush(void)
{
if (outbuf == NULL) return false;
prt_error("\n");
return true;
}
static void default_error_handler(lg_errinfo *lge, void *data)
{
FILE *outfile = stdout;
if (((NULL == data) && (lge->severity < lg_Debug)) ||
((NULL != data) && (lge->severity < *(lg_error_severity *)(int *)data) &&
(lg_None !=  lge->severity)))
{
fflush(stdout);
outfile = stderr;
}
char *msgtext = lg_error_formatmsg(lge);
#if 0
fprintf(outfile, "%s", msgtext);
#else
fputs(msgtext, outfile);
#endif
dyn_str_release(msgtext);
fflush(outfile);
}
static const char *error_severity_label(lg_error_severity sev)
{
char *sevlabel = alloca(MAX_SEVERITY_LABEL_SIZE);
if (lg_None == sev)
{
sevlabel[0] = '\0';
}
else if ((sev < 1) || (sev > lg_None))
{
snprintf(sevlabel, MAX_SEVERITY_LABEL_SIZE, "Message severity %d", (int)sev);
}
else
{
sevlabel = (char *)severity_label_by_level[sev-1];
}
return strdup(sevlabel);
}
static void verr_msg(err_ctxt *ec, lg_error_severity sev, const char *fmt, va_list args)
GNUC_PRINTF(3,0);
static void verr_msg(err_ctxt *ec, lg_error_severity sev, const char *fmt, va_list args)
{
if (NULL == outbuf) outbuf = dyn_str_new();
char *nfmt;
bool partline = false;
const int fmtlen = strlen(fmt);
if ('\n' != fmt[fmtlen-1])
{
partline = true;
if ('\\' == fmt[fmtlen-1])
{
nfmt = strdupa(fmt);
nfmt[fmtlen-1] = '\0';
fmt = nfmt;
}
}
vappend_string(outbuf, fmt, args);
if (partline) return;
if ((NULL != ec) && (NULL != ec->sent))
print_sentence_context(ec->sent, outbuf);
lg_errinfo current_error;
const char *error_text = outbuf->str;
lg_error_severity msg_sev = message_error_severity(error_text);
if (lg_None != msg_sev)
{
error_text = strchr(error_text, ':') + 1;
error_text += strspn(error_text, " \t");
}
current_error.text = error_text;
current_error.severity = ((lg_None == msg_sev) && (0 != sev)) ? sev : msg_sev;
current_error.severity_label = error_severity_label(current_error.severity);
if (NULL == lg_error.handler)
{
error_queue_append(&lg_error.errmsg, &current_error);
}
else
{
lg_error.handler(&current_error, lg_error.handler_data);
free((void *)current_error.severity_label);
}
dyn_str_delete(outbuf);
outbuf = NULL;
}
void err_msgc(err_ctxt *ec, lg_error_severity sev, const char *fmt, ...)
{
va_list args;
va_start(args, fmt);
verr_msg(ec, sev, fmt, args);
va_end(args);
}
int prt_error(const char *fmt, ...)
{
va_list args;
va_start(args, fmt);
verr_msg(NULL, 0, fmt, args);
va_end(args);
return 0;
}
const char *feature_enabled(const char * list, ...)
{
const char *feature;
va_list given_features;
va_start(given_features, list);
while (NULL != (feature = va_arg(given_features, char *)))
{
if ('\0' == feature[0]) continue;
size_t len = strlen(feature);
char *buff = alloca(len + 2 + 1);
const char *dir_sep = NULL;
#ifdef _WIN32
dir_sep = strrchr(feature, '\\');
#endif
if (NULL == dir_sep) dir_sep = strrchr(feature, '/');
if (NULL != dir_sep) feature = dir_sep + 1;
buff[0] = ',';
strcpy(buff+1, feature);
strcat(buff, ",");
if (NULL != strstr(list, buff))
{
va_end(given_features);
return ",";
}
buff[len+1] = ':';
if (NULL != strstr(list, buff))
{
va_end(given_features);
return strstr(list, buff) + len + 1;
}
if (list[0] == ':')
{
buff[0] = ':';
bool found = (NULL != strstr(list, buff));
if (!found)
{
buff[len+1] = ',';
found = (NULL != strstr(list, buff));
}
if (found)
{
va_end(given_features);
return strstr(list, buff) + strlen(buff) + 2;
}
}
}
va_end(given_features);
return NULL;
}
#ifdef _WIN32
#define DEBUG_TRAP (*((volatile int*) 0x0) = 42)
#elif defined __GNUC__ || defined __clang_analyzer__
#define DEBUG_TRAP __builtin_trap()
#else
#define DEBUG_TRAP abort()
#endif
void (*lg_library_failure_hook)(void);
void assert_failure(const char cond_str[], const char func[],
const char *src_location, const char *fmt, ...)
{
va_list args;
const char sevfmt[] = "Fatal error: \nAssertion (%s) failed in %s() (%s): ";
fflush(stdout);
lg_error_flush();
va_start(args, fmt);
if ((lg_error.handler == default_error_handler) ||
(lg_error.handler == NULL))
{
fprintf(stderr, sevfmt, cond_str, func, src_location);
vfprintf(stderr, fmt, args);
fprintf(stderr, "\n");                                                \
fflush(stderr);                                                       \
}
else
{
prt_error(sevfmt, cond_str, func, src_location);
verr_msg(NULL, lg_Fatal, fmt, args);
prt_error("\n");
}
va_end(args);
if (lg_library_failure_hook == NULL)
DEBUG_TRAP;                        \
else
lg_library_failure_hook();
exit(1);
}
bool verbosity_check(int level, int v, char print_func , const char func[],
const char file[], const char *filter)
{
if ((((D_SPEC >= v) && (v >= level)) || (v == level)) &&
((level <= 1) || !((level <= D_USER_MAX) && (v > D_USER_MAX))) &&
((debug[0] == '\0') || feature_enabled(debug, func, file, filter, NULL)))
{
if (print_func == '+') err_msg(0, "%s: ", func);
return true;
}
return false;
}
void debug_msg(int level, int v, char print_func, const char func[],
const char file[], const char *fmt, ...)
{
va_list args;
if (verbosity_check(level, v, print_func, func, file, ""))
{
va_start(args, fmt);
verr_msg(NULL, lg_Trace, fmt, args);
va_end(args);
}
}
const char *syserror_msg(int errnum)
{
TLS static char errbuf[64];
lg_strerror(errnum, errbuf, sizeof(errbuf));
return errbuf;
}
void lg_lib_failure(void)
{
if (lg_library_failure_hook != NULL)
lg_library_failure_hook();
exit(1);
}