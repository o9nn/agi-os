#include <limits.h>
#include <stdarg.h>
#include <stdbool.h>
#include <errno.h>
#include "print-util.h"
#include "utilities.h"
#include "wcwidth.h"
size_t utf8_strwidth(const char *s)
{
size_t mblen;
#ifdef _WIN32
mblen = MultiByteToWideChar(CP_UTF8, 0, s, -1, NULL, 0) - 1;
#else
mblen = mbsrtowcs(NULL, &s, 0, NULL);
#endif
if ((int)mblen < 0)
{
prt_error("Warning: Error in utf8_strwidth(%s)\n", s);
return 1 ;
}
wchar_t *ws = alloca((mblen + 1) * sizeof(wchar_t));
#ifdef _WIN32
MultiByteToWideChar(CP_UTF8, 0, s, -1, ws, mblen);
#else
mbstate_t mbss;
memset(&mbss, 0, sizeof(mbss));
mbsrtowcs(ws, &s, mblen, &mbss);
#endif
int glyph_width = 0;
for (size_t i = 0; i < mblen; i++)
{
int w = mk_wcwidth(ws[i]);
if (w < 0) w = 2;
glyph_width += w;
}
return glyph_width;
}
int utf8_charwidth(const char *s)
{
wchar_t wc;
int n = mbrtowc(&wc, s, MB_LEN_MAX, NULL);
if (n == 0) return 0;
if (n < 0)
{
return -2 ;
}
return mk_wcwidth(wc);
}
size_t utf8_chars_in_width(const char *s, size_t max_width)
{
size_t total_bytes = 0;
size_t glyph_width = 0;
int n = 0;
wchar_t wc;
do
{
total_bytes += n;
n = mbrtowc(&wc, s+total_bytes, MB_LEN_MAX, NULL);
if (n == 0) break;
if (n < 0)
{
glyph_width += 2;
n = 1;
}
else
{
int gw = mk_wcwidth(wc);
if (0 <= gw)
glyph_width += gw;
else
glyph_width += 2;
}
}
while (glyph_width <= max_width);
return total_bytes;
}
int vappend_string(dyn_str * string, const char *fmt, va_list args)
{
#define TMPLEN 1024
char temp_buffer[TMPLEN];
char *temp_string = temp_buffer;
int templen;
va_list copy_args;
va_copy(copy_args, args);
templen = vsnprintf(temp_string, TMPLEN, fmt, copy_args);
va_end(copy_args);
if (templen < 0) goto error;
if (0)
{
if (fmt[0] == '(') { errno=2; goto error;}
}
if (templen >= TMPLEN)
{
temp_string = malloc(templen+1);
templen = vsnprintf(temp_string, templen+1, fmt, args);
if (templen < 0)
{
free(temp_string);
goto error;
}
}
va_end(args);
patch_subscript_marks(temp_string);
dyn_strcat(string, temp_string);
if (templen >= TMPLEN) free(temp_string);
return templen;
error:
{
const char msg[] = "[vappend_string(): ";
strcpy(temp_buffer, msg);
lg_strerror(errno, temp_buffer+sizeof(msg)-1, TMPLEN-sizeof(msg));
strcat(temp_buffer, "]");
dyn_strcat(string, temp_buffer);
va_end(args);
return templen;
}
}
int append_string(dyn_str * string, const char *fmt, ...)
{
va_list args;
va_start(args, fmt);
return vappend_string(string, fmt, args);
}
size_t append_utf8_char(dyn_str * string, const char * mbs)
{
char buf[12];
assert('\0' != *mbs, "Null string");
int nb = utf8_charlen(mbs);
int n = nb;
if (n < 0) n = 1;
assert((size_t)n<sizeof(buf), "Multi-byte character is too long!");
memcpy(buf, mbs, n);
if (nb < 0) { buf[n] = ' '; n++; }
if (0 < nb && utf8_charwidth(mbs) < 0) { buf[n] = ' '; n++; }
buf[n] = 0;
dyn_strcat(string, buf);
n = nb;
if (n < 0) n = 1;
return n;
}