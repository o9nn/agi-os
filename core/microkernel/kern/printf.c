#include <stdarg.h>
#include <string.h>
#include <device/cons.h>
#include <kern/printf.h>
#include <mach/boolean.h>
#include <mach/time_value.h>
#include <kern/mach_clock.h>
#include <kern/host.h>
#include <kern/constants.h>
#include <kern/lock.h>
boolean_t console_timestamps_enabled = TRUE;
static time_value64_t console_start_time;
static boolean_t console_timestamp_initialized = FALSE;
static console_timestamp_format_t console_timestamp_format = TIMESTAMP_FORMAT_RELATIVE;
static simple_lock_data_t console_timestamp_lock;
static simple_lock_data_t printf_line_tracking_lock;
static boolean_t at_line_start = TRUE;
extern char *kernel_cmdline;
static void console_timestamp_parse_boot_params(void)
{
if (!kernel_cmdline)
return;
if (strstr(kernel_cmdline, "notimestamps")) {
console_timestamps_enabled = FALSE;
return;
}
if (strstr(kernel_cmdline, "console_timestamps=off")) {
console_timestamps_enabled = FALSE;
return;
}
if (strstr(kernel_cmdline, "console_timestamps=on")) {
console_timestamps_enabled = TRUE;
}
if (strstr(kernel_cmdline, "timestamp_format=simple")) {
console_timestamp_format = TIMESTAMP_FORMAT_SIMPLE;
} else if (strstr(kernel_cmdline, "timestamp_format=precise")) {
console_timestamp_format = TIMESTAMP_FORMAT_PRECISE;
} else if (strstr(kernel_cmdline, "timestamp_format=uptime")) {
console_timestamp_format = TIMESTAMP_FORMAT_UPTIME;
}
}
void console_timestamp_init(void)
{
simple_lock_init(&console_timestamp_lock);
simple_lock_init(&printf_line_tracking_lock);
if (console_timestamp_initialized)
return;
console_timestamp_parse_boot_params();
console_start_time = uptime;
console_timestamp_initialized = TRUE;
}
static void
cnputc_wrapper(char c, vm_offset_t arg)
{
cnputc(c);
}
void console_print_timestamp(void)
{
time_value64_t current_uptime, relative_time;
int seconds, milliseconds, microseconds;
console_timestamp_format_t format;
if (!console_timestamps_enabled || !console_timestamp_initialized)
return;
simple_lock(&console_timestamp_lock);
format = console_timestamp_format;
simple_unlock(&console_timestamp_lock);
current_uptime = uptime;
relative_time = current_uptime;
time_value64_sub(&relative_time, &console_start_time);
seconds = (int)relative_time.seconds;
milliseconds = (int)(relative_time.nanoseconds / 1000000);
microseconds = (int)((relative_time.nanoseconds % 1000000) / 1000);
cnputc('[');
switch (format) {
case TIMESTAMP_FORMAT_RELATIVE:
printnum(seconds, 10, cnputc_wrapper, 0);
cnputc('.');
if (milliseconds < 100) cnputc('0');
if (milliseconds < 10) cnputc('0');
printnum(milliseconds, 10, cnputc_wrapper, 0);
break;
case TIMESTAMP_FORMAT_UPTIME:
printnum((int)current_uptime.seconds, 10, cnputc_wrapper, 0);
cnputc('.');
{
int abs_ms = (int)(current_uptime.nanoseconds / 1000000);
if (abs_ms < 100) cnputc('0');
if (abs_ms < 10) cnputc('0');
printnum(abs_ms, 10, cnputc_wrapper, 0);
}
break;
case TIMESTAMP_FORMAT_SIMPLE:
if (seconds < 100) cnputc('0');
if (seconds < 10) cnputc('0');
printnum(seconds, 10, cnputc_wrapper, 0);
cnputc('.');
if (milliseconds < 100) cnputc('0');
if (milliseconds < 10) cnputc('0');
printnum(milliseconds, 10, cnputc_wrapper, 0);
break;
case TIMESTAMP_FORMAT_PRECISE:
if (seconds < 100) cnputc('0');
if (seconds < 10) cnputc('0');
printnum(seconds, 10, cnputc_wrapper, 0);
cnputc('.');
if (milliseconds < 100) cnputc('0');
if (milliseconds < 10) cnputc('0');
printnum(milliseconds, 10, cnputc_wrapper, 0);
cnputc('.');
if (microseconds < 100) cnputc('0');
if (microseconds < 10) cnputc('0');
printnum(microseconds, 10, cnputc_wrapper, 0);
break;
}
cnputc(']');
cnputc(' ');
}
void console_timestamp_enable(boolean_t enable)
{
simple_lock(&console_timestamp_lock);
console_timestamps_enabled = enable;
simple_unlock(&console_timestamp_lock);
}
boolean_t console_timestamp_is_enabled(void)
{
boolean_t enabled;
simple_lock(&console_timestamp_lock);
enabled = console_timestamps_enabled;
simple_unlock(&console_timestamp_lock);
return enabled;
}
void console_timestamp_set_format(console_timestamp_format_t format)
{
if (format < TIMESTAMP_FORMAT_RELATIVE || format > TIMESTAMP_FORMAT_PRECISE) {
return;
}
simple_lock(&console_timestamp_lock);
console_timestamp_format = format;
simple_unlock(&console_timestamp_lock);
}
console_timestamp_format_t console_timestamp_get_format(void)
{
console_timestamp_format_t format;
simple_lock(&console_timestamp_lock);
format = console_timestamp_format;
simple_unlock(&console_timestamp_lock);
return format;
}
void console_timestamp_get_boot_time(time_value64_t *boot_time)
{
if (boot_time) {
simple_lock(&console_timestamp_lock);
*boot_time = console_start_time;
simple_unlock(&console_timestamp_lock);
}
}
#define isdigit(d) ((d) >= '0' && (d) <= '9')
#define Ctod(c) ((c) - '0')
#define MAXBUF (sizeof(long long int) * 8)
void printnum(
unsigned long long	u,
int			base,
void			(*putc)( char, vm_offset_t ),
vm_offset_t		putc_arg)
{
char	buf[MAXBUF];
char *	p = &buf[MAXBUF-1];
static char digs[] = "0123456789abcdef";
do {
*p-- = digs[u % base];
u /= base;
} while (u != 0);
while (++p != &buf[MAXBUF])
(*putc)(*p, putc_arg);
}
boolean_t	_doprnt_truncates = FALSE;
void _doprnt(
const char 	*fmt,
va_list		argp,
void		(*putc)( char, vm_offset_t),
int		radix,
vm_offset_t	putc_arg)
{
int		length;
int		prec;
boolean_t	ladjust;
char		padc;
long long	n;
unsigned long long	u;
int		have_long_long;
int		plus_sign;
int		sign_char;
boolean_t	altfmt, truncate;
int		base;
char		c;
while ((c = *fmt) != '\0') {
if (c != '%') {
(*putc)(c, putc_arg);
fmt++;
continue;
}
fmt++;
length = 0;
prec = -1;
ladjust = FALSE;
padc = ' ';
plus_sign = 0;
sign_char = 0;
altfmt = FALSE;
have_long_long = FALSE;
while (TRUE) {
c = *fmt;
if (c == '#') {
altfmt = TRUE;
}
else if (c == '-') {
ladjust = TRUE;
}
else if (c == '+') {
plus_sign = '+';
}
else if (c == ' ') {
if (plus_sign == 0)
plus_sign = ' ';
}
else
break;
fmt++;
}
if (c == '0') {
padc = '0';
c = *++fmt;
}
if (isdigit(c)) {
while(isdigit(c)) {
length = 10 * length + Ctod(c);
c = *++fmt;
}
}
else if (c == '*') {
length = va_arg(argp, int);
c = *++fmt;
if (length < 0) {
ladjust = !ladjust;
length = -length;
}
}
if (c == '.') {
c = *++fmt;
if (isdigit(c)) {
prec = 0;
while(isdigit(c)) {
prec = 10 * prec + Ctod(c);
c = *++fmt;
}
}
else if (c == '*') {
prec = va_arg(argp, int);
c = *++fmt;
}
}
if (c == 'l')
c = *++fmt;
if (c == 'l') {
c = *++fmt;
have_long_long = TRUE;
}
truncate = FALSE;
switch(c) {
case 'b':
case 'B':
{
char 	*p;
boolean_t	any;
int  	i;
if (! have_long_long)
u = va_arg(argp, unsigned long);
else
u = va_arg(argp, unsigned long long);
p = va_arg(argp, char *);
base = *p++;
printnum(u, base, putc, putc_arg);
if (u == 0)
break;
any = FALSE;
while ((i = *p++)) {
if (*p <= 32) {
int j;
if (any)
(*putc)(',', putc_arg);
else {
(*putc)('<', putc_arg);
any = TRUE;
}
j = *p++;
for (; (c = *p) > 32; p++)
(*putc)(c, putc_arg);
printnum((unsigned)( (u>>(j-1)) & ((2<<(i-j))-1)),
base, putc, putc_arg);
}
else if (u & (1<<(i-1))) {
if (any)
(*putc)(',', putc_arg);
else {
(*putc)('<', putc_arg);
any = TRUE;
}
for (; (c = *p) > 32; p++)
(*putc)(c, putc_arg);
}
else {
for (; *p > 32; p++)
continue;
}
}
if (any)
(*putc)('>', putc_arg);
break;
}
case 'c':
c = va_arg(argp, int);
(*putc)(c, putc_arg);
break;
case 's':
{
char *p;
char *p2;
if (prec == -1)
prec = PRINTF_MAX_PRECISION;
p = va_arg(argp, char *);
if (p == (char *)0)
p = "";
if (length > 0 && !ladjust) {
n = 0;
p2 = p;
for (; *p != '\0' && n < prec; p++)
n++;
p = p2;
while (n < length) {
(*putc)(' ', putc_arg);
n++;
}
}
n = 0;
while (*p != '\0') {
if (++n > prec)
break;
(*putc)(*p++, putc_arg);
}
if (n < length && ladjust) {
while (n < length) {
(*putc)(' ', putc_arg);
n++;
}
}
break;
}
case 'o':
truncate = _doprnt_truncates;
case 'O':
base = 8;
goto print_unsigned;
case 'd':
truncate = _doprnt_truncates;
case 'D':
base = 10;
goto print_signed;
case 'u':
truncate = _doprnt_truncates;
case 'U':
base = 10;
goto print_unsigned;
case 'p':
case 'x':
truncate = _doprnt_truncates;
case 'X':
base = 16;
goto print_unsigned;
case 'z':
truncate = _doprnt_truncates;
case 'Z':
base = 16;
goto print_signed;
case 'r':
truncate = _doprnt_truncates;
case 'R':
base = radix;
goto print_signed;
case 'n':
truncate = _doprnt_truncates;
case 'N':
base = radix;
goto print_unsigned;
print_signed:
if (! have_long_long)
n = va_arg(argp, long);
else
n = va_arg(argp, long long);
if (n >= 0) {
u = n;
sign_char = plus_sign;
}
else {
u = -n;
sign_char = '-';
}
goto print_num;
print_unsigned:
if (! have_long_long)
u = va_arg(argp, unsigned long);
else
u = va_arg(argp, unsigned long long);
goto print_num;
print_num:
{
char	buf[MAXBUF];
char *	p = &buf[MAXBUF-1];
static char digits[] = "0123456789abcdef";
char *prefix = 0;
if (truncate) u = (long)((int)(u));
if (u != 0 && altfmt) {
if (base == 8)
prefix = "0";
else if (base == 16)
prefix = "0x";
}
do {
*p-- = digits[u % base];
u /= base;
} while (u != 0);
length -= (&buf[MAXBUF-1] - p);
if (sign_char)
length--;
if (prefix)
length -= strlen(prefix);
if (padc == ' ' && !ladjust) {
while (--length >= 0)
(*putc)(' ', putc_arg);
}
if (sign_char)
(*putc)(sign_char, putc_arg);
if (prefix)
while (*prefix)
(*putc)(*prefix++, putc_arg);
if (padc == '0') {
while (--length >= 0)
(*putc)('0', putc_arg);
}
while (++p != &buf[MAXBUF])
(*putc)(*p, putc_arg);
if (ladjust) {
while (--length >= 0)
(*putc)(' ', putc_arg);
}
break;
}
case '\0':
fmt--;
break;
default:
(*putc)(c, putc_arg);
}
fmt++;
}
}
int vprintf(const char *fmt, va_list listp)
{
_doprnt(fmt, listp, (void (*)( char, vm_offset_t)) cnputc, 16, 0);
return 0;
}
int printf(const char *fmt, ...)
{
va_list	listp;
boolean_t need_timestamp = FALSE;
size_t len;
if (console_timestamps_enabled && console_timestamp_initialized && fmt && *fmt != '\0') {
simple_lock(&printf_line_tracking_lock);
if (at_line_start) {
need_timestamp = TRUE;
at_line_start = FALSE;
}
len = strlen(fmt);
if (len > 0 && fmt[len-1] == '\n') {
at_line_start = TRUE;
}
simple_unlock(&printf_line_tracking_lock);
if (need_timestamp) {
console_print_timestamp();
}
}
va_start(listp, fmt);
vprintf(fmt, listp);
va_end(listp);
return 0;
}
int	indent = 0;
void iprintf(const char *fmt, ...)
{
va_list	listp;
int i;
for (i = indent; i > 0; ){
if (i >= 8) {
printf("\t");
i -= 8;
}
else {
printf(" ");
i--;
}
}
va_start(listp, fmt);
_doprnt(fmt, listp, (void (*)( char, vm_offset_t)) cnputc, 16, 0);
va_end(listp);
}
static void
sputc(
char		c,
vm_offset_t	arg)
{
char	**bufp = (char **) arg;
char	*p = *bufp;
*p++ = c;
*bufp = p;
}
int
sprintf(char *buf, const char *fmt, ...)
{
va_list	listp;
char	*start = buf;
va_start(listp, fmt);
_doprnt(fmt, listp, sputc, 16, (vm_offset_t)&buf);
va_end(listp);
*buf = 0;
return (buf - start);
}
struct vsnprintf_cookie
{
char *buf;
int index;
int max_len;
};
static void
snputc(char c, vm_offset_t arg)
{
struct vsnprintf_cookie *cookie = (void *) arg;
if (cookie->index < cookie->max_len)
cookie->buf[cookie->index ++] = c;
}
int
vsnprintf(char *buf, size_t size, const char *fmt, va_list args)
{
struct vsnprintf_cookie cookie
= { .buf = buf, .index = 0, .max_len = size };
_doprnt (fmt, args, snputc, 16, (vm_offset_t)&cookie);
cookie.buf[cookie.index] = '\0';
return cookie.index;
}
int
snprintf(char *buf, size_t size, const char *fmt, ...)
{
int written;
va_list	listp;
va_start(listp, fmt);
written = vsnprintf(buf, size, fmt, listp);
va_end(listp);
return written;
}
void safe_gets(
char *str,
int  maxlen)
{
char *lp;
int c;
char *strmax = str + maxlen - 1;
lp = str;
for (;;) {
c = cngetc();
switch (c) {
case '\n':
case '\r':
printf("\n");
*lp++ = 0;
return;
case '\b':
case '#':
case '\177':
if (lp > str) {
printf("\b \b");
lp--;
}
continue;
case '@':
case 'u'&037:
lp = str;
printf("\n\r");
continue;
default:
if (c >= ' ' && c < '\177') {
if (lp < strmax) {
*lp++ = c;
printf("%c", c);
}
else {
printf("%c", '\007');
}
}
}
}
}