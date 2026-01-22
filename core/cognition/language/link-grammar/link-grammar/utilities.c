#include <ctype.h>
#include <errno.h>
#include <limits.h>
#ifdef _WIN32
#define _CRT_RAND_S
#endif
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <stdarg.h>
#include <locale.h>
#ifdef HAVE_XLOCALE_H
#include <xlocale.h>
#endif
#ifndef _WIN32
#include <langinfo.h>
#else
#include <windows.h>
#endif
#include "link-includes.h"
#include "error.h"
#include "utilities.h"
#ifdef _WIN32
void lg_strerror(int err_no, char *buf, size_t len)
{
if (strerror_s(buf, len, err_no) != 0)
strerror_s(buf, len, errno);
}
#else
#if HAVE_STRERROR_R
#if STRERROR_R_CHAR_P
void lg_strerror(int err_no, char *buf, size_t len)
{
char *errstr = strerror_r(err_no, buf, len);
strncpy(buf, errstr, len);
buf[len-1] = '\0';
}
#else
void lg_strerror(int err_no, char *buf, size_t len)
{
errno = 0;
if ((strerror_r(err_no, buf, len) == EINVAL) || (errno == EINVAL))
snprintf(buf, len, "Unknown error %d", err_no);
}
#endif
#else
void lg_strerror(int err_no, char *buf, size_t len)
{
snprintf(buf, len, "Error %d", err_no);
}
#endif
#endif
char *safe_strdup(const char *u)
{
if (u)
return strdup(u);
return NULL;
}
size_t
lg_strlcpy(char * restrict dst, const char * restrict src, size_t dsize)
{
const char *osrc = src;
size_t nleft = dsize;
if (nleft != 0) {
while (--nleft != 0) {
if ((*dst++ = *src++) == '\0')
break;
}
}
if (nleft == 0) {
if (dsize != 0)
*dst = '\0';
while (*src++)
;
}
return(src - osrc - 1);
}
void safe_strcat(char *u, const char *v, size_t usize)
{
strncat(u, v, usize-strlen(u)-1);
u[usize-1] = '\0';
}
#ifndef HAVE_STRNDUP
char *
strndup (const char *str, size_t size)
{
size_t len;
char *result;
len = strlen (str);
if (!len) return strdup ("");
if (size > len) size = len;
result = (char *) malloc ((size + 1) * sizeof (char));
memcpy (result, str, size);
result[size] = 0x0;
return result;
}
#endif
#ifndef HAVE_STRTOK_R
char* strtok_r(char *str, const char *delim, char **nextp)
{
char *ret;
if (str == NULL) str = *nextp;
str += strspn(str, delim);
if (*str == '\0') return NULL;
ret = str;
str += strcspn(str, delim);
if (*str) *str++ = '\0';
*nextp = str;
return ret;
}
#endif
#ifdef _WIN32
size_t lg_mbrtowc(wchar_t *pwc, const char *s, size_t n, mbstate_t *ps)
{
int nb, nb2;
if (NULL == s) return 0;
if (0 == n) return -2;
if (0 == *s) { *pwc = 0; return 0; }
nb = utf8_charlen(s);
if (0 == nb) return 0;
if (0 > nb) return nb;
nb2 = MultiByteToWideChar(CP_UTF8, 0, s, nb, NULL, 0);
nb2 = MultiByteToWideChar(CP_UTF8, 0, s, nb, pwc, nb2);
if (0 == nb2) return (size_t)-1;
return nb;
}
int rand_r(unsigned int *s)
{
rand_s(s);
if (*s > INT_MAX) *s -= INT_MAX;
return *s;
}
#endif
static int wctomb_check(char *s, wchar_t wc)
{
int nr;
#ifdef _WIN32
nr = WideCharToMultiByte(CP_UTF8, 0, &wc, 1, NULL, 0, NULL, NULL);
nr = WideCharToMultiByte(CP_UTF8, 0, &wc, 1, s, nr, NULL, NULL);
if (0 == nr) return -1;
#else
mbstate_t mbss;
memset(&mbss, 0, sizeof(mbss));
nr = wcrtomb(s, wc, &mbss);
if (nr < 0) {
prt_error("Fatal Error: unknown character set %s\n", nl_langinfo(CODESET));
lg_lib_failure();
}
#endif
return nr;
}
void downcase_utf8_str(char *to, const char * from, size_t usize, locale_t locale)
{
wchar_t c;
int i, nbl, nbh;
char low[MB_LEN_MAX];
mbstate_t mbs;
if (to != from) strcpy(to, from);
memset(&mbs, 0, sizeof(mbs));
nbh = mbrtowc (&c, from, MB_CUR_MAX, &mbs);
if (nbh < 0)
{
prt_error("Error: Invalid UTF-8 string!\n");
return;
}
c = towlower_l(c, locale);
nbl = wctomb_check(low, c);
if ((nbh < nbl) && (to == from))
{
prt_error("Error: can't downcase UTF-8 string!\n");
return;
}
for (i=0; i<nbl; i++) { to[i] = low[i]; }
if ((nbh == nbl) && (to == from)) return;
from += nbh;
to += nbl;
lg_strlcpy(to, from, usize-nbl);
}
#if 0
void upcase_utf8_str(char *to, const char * from, size_t usize, locale_t locale)
{
wchar_t c;
int i, nbl, nbh;
char low[MB_LEN_MAX];
mbstate_t mbs;
memset(&mbs, 0, sizeof(mbs));
nbh = mbrtowc (&c, from, MB_CUR_MAX, &mbs);
if (nbh < 0)
{
prt_error("Error: Invalid UTF-8 string!\n");
return;
}
c = towupper_l(c, locale);
nbl = wctomb_check(low, c);
if ((nbh < nbl) && (to == from))
{
prt_error("Error: can't upcase UTF-8 string!\n");
return;
}
for (i=0; i<nbl; i++) { to[i] = low[i]; }
if ((nbh == nbl) && (to == from)) return;
from += nbh;
to += nbl;
lg_strlcpy(to, from, usize-nbl);
}
#endif
#ifdef NO_ALIGNED_MALLOC
#if __GNUC__
#warning No aligned alloc found (using malloc() instead).
#endif
#endif
#ifdef HAVE_POSIX_MEMALIGN
void *aligned_alloc(size_t alignment, size_t size)
{
void *ptr;
errno = posix_memalign(&ptr, alignment, size);
return ptr;
}
#endif
#ifdef TRACK_SPACE_USAGE
typedef struct
{
size_t max_space_used;
size_t space_in_use;
size_t num_xallocs;
size_t num_xfrees;
size_t max_outstanding_xallocs;
size_t max_external_space_used;
size_t external_space_in_use;
size_t num_exallocs;
size_t num_exfrees;
size_t max_outstanding_exallocs;
} space_t;
static TLS space_t space;
static space_t * do_init_memusage(void)
{
space_t *s = &space;
s->max_space_used = 0;
s->space_in_use = 0;
s->num_xallocs = 0;
s->num_xfrees = 0;
s->max_outstanding_xallocs = 0;
s->max_external_space_used = 0;
s->external_space_in_use = 0;
s->num_exallocs = 0;
s->num_exfrees = 0;
s->max_outstanding_exallocs = 0;
return s;
}
void init_memusage(void)
{
static bool mem_inited = false;
if (mem_inited) return;
mem_inited = true;
do_init_memusage();
}
static inline space_t *getspace(void)
{
return &space;
}
size_t get_space_in_use(void)
{
return getspace()->space_in_use;
}
size_t get_max_space_used(void)
{
return getspace()->max_space_used;
}
#else
void init_memusage(void) {}
size_t get_space_in_use(void) { return 0; }
size_t get_max_space_used(void) { return 0; }
#endif
void * xalloc(size_t size)
{
void * p = malloc(size);
#ifdef TRACK_SPACE_USAGE
space_t *s = getspace();
s->space_in_use += size;
if (s->max_space_used < s->space_in_use) s->max_space_used = s->space_in_use;
s->num_xallocs ++;
if (s->max_outstanding_xallocs < (s->num_xallocs - s->num_xfrees))
s->max_outstanding_xallocs = (s->num_xallocs - s->num_xfrees);
#endif
if ((p == NULL) && (size != 0))
{
prt_error("Fatal Error: Ran out of space. (int)\n");
abort();
exit(1);
}
return p;
}
#ifdef TRACK_SPACE_USAGE
void xfree(void * p, size_t size)
{
space_t *s = getspace();
s->space_in_use -= size;
s->num_xfrees ++;
free(p);
}
#endif
void * exalloc(size_t size)
{
void * p = malloc(size);
#ifdef TRACK_SPACE_USAGE
space_t *s = getspace();
s->external_space_in_use += size;
if (s->max_external_space_used < s->external_space_in_use)
s->max_external_space_used = s->external_space_in_use;
s->num_exallocs ++;
if (s->max_outstanding_exallocs < (s->num_exallocs - s->num_exfrees))
s->max_outstanding_exallocs = (s->num_exallocs - s->num_exfrees);
#endif
if ((p == NULL) && (size != 0))
{
prt_error("Fatal Error: Ran out of space. (ext)\n");
abort();
exit(1);
}
return p;
}
#ifdef TRACK_SPACE_USAGE
void exfree(void * p, size_t size)
{
space_t *s = getspace();
s->external_space_in_use -= size;
s->num_exfrees ++;
free(p);
}
#endif
dyn_str* dyn_str_new(void)
{
dyn_str *ds = malloc(sizeof(dyn_str));
ds->len = 250;
ds->end = 0;
ds->str = malloc(ds->len);
ds->str[0] = 0x0;
return ds;
}
void dyn_str_delete(dyn_str* ds)
{
free(ds->str);
free(ds);
}
char * dyn_str_take(dyn_str* ds)
{
char * rv = ds->str;
free(ds);
return rv;
}
void dyn_strcat(dyn_str* ds, const char *str)
{
size_t l = strlen(str);
if (ds->end+l+1 >= ds->len)
{
ds->len = 2 * ds->len + l;
ds->str = realloc(ds->str, ds->len);
}
strcpy (ds->str+ds->end, str);
ds->end += l;
}
void dyn_trimback(dyn_str* ds)
{
size_t tail = ds->end;
while (0 < tail && ' ' == ds->str[--tail]) {}
ds->end = ++tail;
ds->str[tail] = 0x0;
}
const char * dyn_str_value(dyn_str* s)
{
return s->str;
}
size_t dyn_strlen(dyn_str* s)
{
return s->end;
}
#ifdef HAVE_LOCALE_T
locale_t newlocale_LC_CTYPE(const char *locale)
{
locale_t locobj;
#ifdef _WIN32
locobj = _create_locale(LC_CTYPE, locale);
#else
locobj = newlocale(LC_CTYPE_MASK, locale, (locale_t)0);
#endif
return locobj;
}
#endif
bool try_locale(const char *locale)
{
#ifdef HAVE_LOCALE_T
locale_t ltmp = newlocale_LC_CTYPE(locale);
if ((locale_t)0 == ltmp) return false;
freelocale(ltmp);
#else
lgdebug(D_USER_FILES, "Debug: Setting program's locale \"%s\"", locale);
if (NULL == setlocale(LC_CTYPE, locale))
{
lgdebug(D_USER_FILES, " failed!\n");
return false;
}
lgdebug(D_USER_FILES, ".\n");
#endif
return true;
}
void set_utf8_program_locale(void)
{
#ifndef _WIN32
const char *codeset = nl_langinfo(CODESET);
if (!strstr(codeset, "UTF") && !strstr(codeset, "utf"))
{
const char *locale = setlocale(LC_CTYPE, NULL);
if ((0 != strcmp(locale, "C")) && (0 != strcmp(locale, "POSIX")))
{
prt_error("Warning: Program locale \"%s\" (codeset %s) was not UTF-8; "
"force-setting to en_US.UTF-8\n", locale, codeset);
}
locale = setlocale(LC_CTYPE, "en_US.UTF-8");
if (NULL == locale)
{
prt_error("Warning: Program locale en_US.UTF-8 could not be set; "
"force-setting to C.UTF-8\n");
locale = setlocale(LC_CTYPE, "C.UTF-8");
if (NULL == locale)
{
prt_error("Warning: Could not set a UTF-8 program locale; "
"program may malfunction\n");
}
}
}
#endif
}
#ifdef _WIN32
static char *
win32_getlocale (void)
{
char lbuf[10];
char locale[32];
LCID lcid = GetThreadLocale();
if (0 >= GetLocaleInfoA(lcid, LOCALE_SISO639LANGNAME, lbuf, sizeof(lbuf)))
{
prt_error("Error: GetLocaleInfoA LOCALE_SENGLISHLANGUAGENAME LCID=%d: "
"Error %d\n", (int)lcid, (int)GetLastError());
return NULL;
}
strcpy(locale, lbuf);
strcat(locale, "-");
if (0 >= GetLocaleInfoA(lcid, LOCALE_SISO3166CTRYNAME, lbuf, sizeof(lbuf)))
{
prt_error("Error: GetLocaleInfoA LOCALE_SISO3166CTRYNAME LCID=%d: "
"Error %d\n", (int)lcid, (int)GetLastError());
return NULL;
}
strcat(locale, lbuf);
return strdup(locale);
}
#endif
char * get_default_locale(void)
{
const char *lc_vars[] = {"LC_ALL", "LC_CTYPE", "LANG", NULL};
char *ev = NULL;
const char **evname;
char *locale = NULL;
for(evname = lc_vars; NULL != *evname; evname++)
{
ev = getenv(*evname);
if ((NULL != ev) && ('\0' != ev[0])) break;
}
if (NULL != *evname)
{
locale = ev;
lgdebug(D_USER_FILES, "Debug: Environment locale \"%s=%s\"\n", *evname, ev);
#ifdef _WIN32
const char *ostype = getenv("OSTYPE");
if ((NULL != ostype) && (0 == strcmp(ostype, "cygwin")))
{
locale = strdupa(locale);
locale[strcspn(locale, "_")] = '-';
locale[strcspn(locale, ".@")] = '\0';
}
#endif
}
else
{
lgdebug(D_USER_FILES, "Debug: Environment locale not set\n");
#ifdef _WIN32
locale = win32_getlocale();
if (NULL == locale)
lgdebug(D_USER_FILES, "Debug: Cannot find user default locale\n");
else
lgdebug(D_USER_FILES, "Debug: User default locale \"%s\"\n", locale);
return locale;
#endif
}
return safe_strdup(locale);
}
#define D_SITOF 5
bool strtofC(const char *s, float *r)
{
#define DFP(n) (1.0f * n)
#define FP_BY_POS(p) \
{ \
p*DFP(0), p*DFP(1), p*DFP(2), p*DFP(3), p*DFP(4), \
p*DFP(5), p*DFP(6), p*DFP(7), p*DFP(8), p*DFP(9)  \
}
static float fpconv[][10] =
{
FP_BY_POS(10), FP_BY_POS(1), FP_BY_POS(0.1f), FP_BY_POS(0.01f),
FP_BY_POS(0.001f), FP_BY_POS(0.0001f)
};
static const int max_int_digits = 2;
static const int max_frac_digits = 4;
static const char max_str[] = "99.9999";
const char *si = s;
bool minus = false;
if ((*si == '-') || (*si == '+'))
{
if (*si == '-') minus = true;
si++;
}
while (*si == '0') si++;
const char *decpoint = strchr(si, '.');
const size_t len = strlen(si);
if (decpoint == NULL)
{
decpoint = &si[len];
}
else
{
if (strchr(decpoint+1, '.'))
{
lgdebug(+D_SITOF, "\"%s\": Extra decimal point\n", s);
return false;
}
}
int pos = max_int_digits - (int)(decpoint - si);
if (pos < 0)
{
lgdebug(+D_SITOF, "\"%s\" is too big (max %s)\n", s, max_str);
return false;
}
if ((si[0] == '\0') || ((si[0] == '.') && (si[1] == '\0')))
{
if ((si == s) || (si[-1] != '0'))
{
lgdebug(+D_SITOF, "\"%s\": No decimal digits found\n", s);
return false;
}
*r = 0.0f;
return true;
}
float total = 0.0f;
do
{
if (*si == '.')
{
si++;
if (*si == '\0') break;
}
unsigned int d = (unsigned int)(*si - '0');
if (d > 9)
{
lgdebug(+D_SITOF, "\"%s\": Invalid digit \"%c\"\n", s, *si);
return false;
}
if ((int)(decpoint - si) >= -max_frac_digits)
total += fpconv[pos][d];
pos++;
}
while (*++si != '\0');
if (minus)
*r = -total;
else
*r = total;
return true;
}
#undef D_SITOF
size_t altlen(const char **arr)
{
size_t len = 0;
if (arr)
while (arr[len] != NULL) len++;
return len;
}
#ifdef __MINGW32__
int __mingw_vfprintf (FILE * __restrict__ stream, const char * __restrict__ fmt,
va_list vl)
{
int n = vsnprintf(NULL, 0, fmt, vl);
if (0 > n) return n;
char *buf = malloc(n+1);
n = vsnprintf(buf, n+1, fmt, vl);
if (0 > n)
{
free(buf);
return n;
}
n = fputs(buf, stdout);
free(buf);
return n;
}
int __mingw_vprintf (const char * __restrict__ fmt, va_list vl)
{
return __mingw_vfprintf(stdout, fmt, vl);
}
#endif