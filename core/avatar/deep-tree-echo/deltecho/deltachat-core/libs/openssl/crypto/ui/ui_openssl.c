#include <openssl/e_os2.h>
#if defined(OPENSSL_SYSNAME_VXWORKS)
# include <sys/types.h>
#endif
#if !defined(_POSIX_C_SOURCE) && defined(OPENSSL_SYS_VMS)
# ifndef _POSIX_C_SOURCE
#  define _POSIX_C_SOURCE 2
# endif
#endif
#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#if !defined(OPENSSL_SYS_MSDOS) && !defined(OPENSSL_SYS_VMS)
# ifdef OPENSSL_UNISTD
#  include OPENSSL_UNISTD
# else
#  include <unistd.h>
# endif
# if defined(_POSIX_VERSION)
#  define SIGACTION
#  if !defined(TERMIOS) && !defined(TERMIO) && !defined(SGTTY)
#   define TERMIOS
#  endif
# endif
#endif
#ifdef WIN16TTY
# undef OPENSSL_SYS_WIN16
# undef WIN16
# undef _WINDOWS
# include <graph.h>
#endif
#include "ui_locl.h"
#include "cryptlib.h"
#ifdef OPENSSL_SYS_VMS
# include <starlet.h>
# ifdef __DECC
#  pragma message disable DOLLARID
# endif
#endif
#ifdef WIN_CONSOLE_BUG
# include <windows.h>
# ifndef OPENSSL_SYS_WINCE
#  include <wincon.h>
# endif
#endif
#if !defined(TERMIOS) && !defined(TERMIO) && !defined(SGTTY)
# if defined(_LIBC)
#  undef  TERMIOS
#  define TERMIO
#  undef  SGTTY
# elif !defined(OPENSSL_SYS_VMS) \
&& !defined(OPENSSL_SYS_MSDOS) \
&& !defined(OPENSSL_SYS_MACINTOSH_CLASSIC) \
&& !defined(MAC_OS_GUSI_SOURCE) \
&& !defined(OPENSSL_SYS_VXWORKS) \
&& !defined(OPENSSL_SYS_NETWARE)
#  define TERMIOS
#  undef  TERMIO
#  undef  SGTTY
# endif
#endif
#ifdef TERMIOS
# include <termios.h>
# define TTY_STRUCT             struct termios
# define TTY_FLAGS              c_lflag
# define TTY_get(tty,data)      tcgetattr(tty,data)
# define TTY_set(tty,data)      tcsetattr(tty,TCSANOW,data)
#endif
#ifdef TERMIO
# include <termio.h>
# define TTY_STRUCT             struct termio
# define TTY_FLAGS              c_lflag
# define TTY_get(tty,data)      ioctl(tty,TCGETA,data)
# define TTY_set(tty,data)      ioctl(tty,TCSETA,data)
#endif
#ifdef SGTTY
# include <sgtty.h>
# define TTY_STRUCT             struct sgttyb
# define TTY_FLAGS              sg_flags
# define TTY_get(tty,data)      ioctl(tty,TIOCGETP,data)
# define TTY_set(tty,data)      ioctl(tty,TIOCSETP,data)
#endif
#if !defined(_LIBC) && !defined(OPENSSL_SYS_MSDOS) && !defined(OPENSSL_SYS_VMS) && !defined(OPENSSL_SYS_MACINTOSH_CLASSIC) && !defined(OPENSSL_SYS_SUNOS)
# include <sys/ioctl.h>
#endif
#ifdef OPENSSL_SYS_MSDOS
# include <conio.h>
#endif
#ifdef OPENSSL_SYS_VMS
# include <ssdef.h>
# include <iodef.h>
# include <ttdef.h>
# include <descrip.h>
struct IOSB {
short iosb$w_value;
short iosb$w_count;
long iosb$l_info;
};
#endif
#ifdef OPENSSL_SYS_SUNOS
typedef int sig_atomic_t;
#endif
#if defined(OPENSSL_SYS_MACINTOSH_CLASSIC) || defined(MAC_OS_GUSI_SOURCE) || defined(OPENSSL_SYS_NETWARE)
# define TTY_STRUCT int
#endif
#ifndef NX509_SIG
# define NX509_SIG 32
#endif
#ifdef SIGACTION
static struct sigaction savsig[NX509_SIG];
#else
static void (*savsig[NX509_SIG]) (int);
#endif
#ifdef OPENSSL_SYS_VMS
static struct IOSB iosb;
static $DESCRIPTOR(terminal, "TT");
static long tty_orig[3], tty_new[3];
static long status;
static unsigned short channel = 0;
#else
# if !defined(OPENSSL_SYS_MSDOS) || defined(__DJGPP__)
static TTY_STRUCT tty_orig, tty_new;
# endif
#endif
static FILE *tty_in, *tty_out;
static int is_a_tty;
#if !defined(OPENSSL_SYS_WIN16) && !defined(OPENSSL_SYS_WINCE)
static int read_till_nl(FILE *);
static void recsig(int);
static void pushsig(void);
static void popsig(void);
#endif
#if defined(OPENSSL_SYS_MSDOS) && !defined(OPENSSL_SYS_WIN16)
static int noecho_fgets(char *buf, int size, FILE *tty);
#endif
static int read_string_inner(UI *ui, UI_STRING *uis, int echo, int strip_nl);
static int read_string(UI *ui, UI_STRING *uis);
static int write_string(UI *ui, UI_STRING *uis);
static int open_console(UI *ui);
static int echo_console(UI *ui);
static int noecho_console(UI *ui);
static int close_console(UI *ui);
static UI_METHOD ui_openssl = {
"OpenSSL default user interface",
open_console,
write_string,
NULL,
read_string,
close_console,
NULL
};
UI_METHOD *UI_OpenSSL(void)
{
return &ui_openssl;
}
static int write_string(UI *ui, UI_STRING *uis)
{
switch (UI_get_string_type(uis)) {
case UIT_ERROR:
case UIT_INFO:
fputs(UI_get0_output_string(uis), tty_out);
fflush(tty_out);
break;
default:
break;
}
return 1;
}
static int read_string(UI *ui, UI_STRING *uis)
{
int ok = 0;
switch (UI_get_string_type(uis)) {
case UIT_BOOLEAN:
fputs(UI_get0_output_string(uis), tty_out);
fputs(UI_get0_action_string(uis), tty_out);
fflush(tty_out);
return read_string_inner(ui, uis,
UI_get_input_flags(uis) & UI_INPUT_FLAG_ECHO,
0);
case UIT_PROMPT:
fputs(UI_get0_output_string(uis), tty_out);
fflush(tty_out);
return read_string_inner(ui, uis,
UI_get_input_flags(uis) & UI_INPUT_FLAG_ECHO,
1);
case UIT_VERIFY:
fprintf(tty_out, "Verifying - %s", UI_get0_output_string(uis));
fflush(tty_out);
if ((ok = read_string_inner(ui, uis,
UI_get_input_flags(uis) &
UI_INPUT_FLAG_ECHO, 1)) <= 0)
return ok;
if (strcmp(UI_get0_result_string(uis), UI_get0_test_string(uis)) != 0) {
fprintf(tty_out, "Verify failure\n");
fflush(tty_out);
return 0;
}
break;
default:
break;
}
return 1;
}
#if !defined(OPENSSL_SYS_WIN16) && !defined(OPENSSL_SYS_WINCE)
static int read_till_nl(FILE *in)
{
# define SIZE 4
char buf[SIZE + 1];
do {
if (!fgets(buf, SIZE, in))
return 0;
} while (strchr(buf, '\n') == NULL);
return 1;
}
static volatile sig_atomic_t intr_signal;
#endif
static int read_string_inner(UI *ui, UI_STRING *uis, int echo, int strip_nl)
{
static int ps;
int ok;
char result[BUFSIZ];
int maxsize = BUFSIZ - 1;
#if !defined(OPENSSL_SYS_WIN16) && !defined(OPENSSL_SYS_WINCE)
char *p;
intr_signal = 0;
ok = 0;
ps = 0;
pushsig();
ps = 1;
if (!echo && !noecho_console(ui))
goto error;
ps = 2;
result[0] = '\0';
# ifdef OPENSSL_SYS_MSDOS
if (!echo) {
noecho_fgets(result, maxsize, tty_in);
p = result;
} else
p = fgets(result, maxsize, tty_in);
# else
p = fgets(result, maxsize, tty_in);
# endif
if (!p)
goto error;
if (feof(tty_in))
goto error;
if (ferror(tty_in))
goto error;
if ((p = (char *)strchr(result, '\n')) != NULL) {
if (strip_nl)
*p = '\0';
} else if (!read_till_nl(tty_in))
goto error;
if (UI_set_result(ui, uis, result) >= 0)
ok = 1;
error:
if (intr_signal == SIGINT)
ok = -1;
if (!echo)
fprintf(tty_out, "\n");
if (ps >= 2 && !echo && !echo_console(ui))
ok = 0;
if (ps >= 1)
popsig();
#else
ok = 1;
#endif
OPENSSL_cleanse(result, BUFSIZ);
return ok;
}
static int open_console(UI *ui)
{
CRYPTO_w_lock(CRYPTO_LOCK_UI);
is_a_tty = 1;
#if defined(OPENSSL_SYS_MACINTOSH_CLASSIC) || defined(OPENSSL_SYS_VXWORKS) || defined(OPENSSL_SYS_NETWARE) || defined(OPENSSL_SYS_BEOS)
tty_in = stdin;
tty_out = stderr;
#else
# ifdef OPENSSL_SYS_MSDOS
#  define DEV_TTY "con"
# else
#  define DEV_TTY "/dev/tty"
# endif
if ((tty_in = fopen(DEV_TTY, "r")) == NULL)
tty_in = stdin;
if ((tty_out = fopen(DEV_TTY, "w")) == NULL)
tty_out = stderr;
#endif
#if defined(TTY_get) && !defined(OPENSSL_SYS_VMS)
if (TTY_get(fileno(tty_in), &tty_orig) == -1) {
# ifdef ENOTTY
if (errno == ENOTTY)
is_a_tty = 0;
else
# endif
# ifdef EINVAL
if (errno == EINVAL)
is_a_tty = 0;
else
# endif
return 0;
}
#endif
#ifdef OPENSSL_SYS_VMS
status = sys$assign(&terminal, &channel, 0, 0);
if (status != SS$_NORMAL)
return 0;
status =
sys$qiow(0, channel, IO$_SENSEMODE, &iosb, 0, 0, tty_orig, 12, 0, 0,
0, 0);
if ((status != SS$_NORMAL) || (iosb.iosb$w_value != SS$_NORMAL))
return 0;
#endif
return 1;
}
static int noecho_console(UI *ui)
{
#ifdef TTY_FLAGS
memcpy(&(tty_new), &(tty_orig), sizeof(tty_orig));
tty_new.TTY_FLAGS &= ~ECHO;
#endif
#if defined(TTY_set) && !defined(OPENSSL_SYS_VMS)
if (is_a_tty && (TTY_set(fileno(tty_in), &tty_new) == -1))
return 0;
#endif
#ifdef OPENSSL_SYS_VMS
tty_new[0] = tty_orig[0];
tty_new[1] = tty_orig[1] | TT$M_NOECHO;
tty_new[2] = tty_orig[2];
status =
sys$qiow(0, channel, IO$_SETMODE, &iosb, 0, 0, tty_new, 12, 0, 0, 0,
0);
if ((status != SS$_NORMAL) || (iosb.iosb$w_value != SS$_NORMAL))
return 0;
#endif
return 1;
}
static int echo_console(UI *ui)
{
#if defined(TTY_set) && !defined(OPENSSL_SYS_VMS)
memcpy(&(tty_new), &(tty_orig), sizeof(tty_orig));
tty_new.TTY_FLAGS |= ECHO;
#endif
#if defined(TTY_set) && !defined(OPENSSL_SYS_VMS)
if (is_a_tty && (TTY_set(fileno(tty_in), &tty_new) == -1))
return 0;
#endif
#ifdef OPENSSL_SYS_VMS
tty_new[0] = tty_orig[0];
tty_new[1] = tty_orig[1] & ~TT$M_NOECHO;
tty_new[2] = tty_orig[2];
status =
sys$qiow(0, channel, IO$_SETMODE, &iosb, 0, 0, tty_new, 12, 0, 0, 0,
0);
if ((status != SS$_NORMAL) || (iosb.iosb$w_value != SS$_NORMAL))
return 0;
#endif
return 1;
}
static int close_console(UI *ui)
{
if (tty_in != stdin)
fclose(tty_in);
if (tty_out != stderr)
fclose(tty_out);
#ifdef OPENSSL_SYS_VMS
status = sys$dassgn(channel);
#endif
CRYPTO_w_unlock(CRYPTO_LOCK_UI);
return 1;
}
#if !defined(OPENSSL_SYS_WIN16) && !defined(OPENSSL_SYS_WINCE)
static void pushsig(void)
{
# ifndef OPENSSL_SYS_WIN32
int i;
# endif
# ifdef SIGACTION
struct sigaction sa;
memset(&sa, 0, sizeof sa);
sa.sa_handler = recsig;
# endif
# ifdef OPENSSL_SYS_WIN32
savsig[SIGABRT] = signal(SIGABRT, recsig);
savsig[SIGFPE] = signal(SIGFPE, recsig);
savsig[SIGILL] = signal(SIGILL, recsig);
savsig[SIGINT] = signal(SIGINT, recsig);
savsig[SIGSEGV] = signal(SIGSEGV, recsig);
savsig[SIGTERM] = signal(SIGTERM, recsig);
# else
for (i = 1; i < NX509_SIG; i++) {
#  ifdef SIGUSR1
if (i == SIGUSR1)
continue;
#  endif
#  ifdef SIGUSR2
if (i == SIGUSR2)
continue;
#  endif
#  ifdef SIGKILL
if (i == SIGKILL)
continue;
#  endif
#  ifdef SIGACTION
sigaction(i, &sa, &savsig[i]);
#  else
savsig[i] = signal(i, recsig);
#  endif
}
# endif
# ifdef SIGWINCH
signal(SIGWINCH, SIG_DFL);
# endif
}
static void popsig(void)
{
# ifdef OPENSSL_SYS_WIN32
signal(SIGABRT, savsig[SIGABRT]);
signal(SIGFPE, savsig[SIGFPE]);
signal(SIGILL, savsig[SIGILL]);
signal(SIGINT, savsig[SIGINT]);
signal(SIGSEGV, savsig[SIGSEGV]);
signal(SIGTERM, savsig[SIGTERM]);
# else
int i;
for (i = 1; i < NX509_SIG; i++) {
#  ifdef SIGUSR1
if (i == SIGUSR1)
continue;
#  endif
#  ifdef SIGUSR2
if (i == SIGUSR2)
continue;
#  endif
#  ifdef SIGACTION
sigaction(i, &savsig[i], NULL);
#  else
signal(i, savsig[i]);
#  endif
}
# endif
}
static void recsig(int i)
{
intr_signal = i;
}
#endif
#if defined(OPENSSL_SYS_MSDOS) && !defined(OPENSSL_SYS_WIN16) && !defined(OPENSSL_SYS_WINCE)
static int noecho_fgets(char *buf, int size, FILE *tty)
{
int i;
char *p;
p = buf;
for (;;) {
if (size == 0) {
*p = '\0';
break;
}
size--;
# ifdef WIN16TTY
i = _inchar();
# elif defined(_WIN32)
i = _getch();
# else
i = getch();
# endif
if (i == '\r')
i = '\n';
*(p++) = i;
if (i == '\n') {
*p = '\0';
break;
}
}
# ifdef WIN_CONSOLE_BUG
{
HANDLE inh;
inh = GetStdHandle(STD_INPUT_HANDLE);
FlushConsoleInputBuffer(inh);
}
# endif
return (strlen(buf));
}
#endif