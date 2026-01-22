#ifdef _WIN32
#include <windows.h>
#include <wchar.h>
#include <io.h>
#include <shellapi.h>
#include <errhandlingapi.h>
#else
#include <pwd.h>
#include <signal.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <termios.h>
#include <unistd.h>
#endif
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#if !HAVE_ASPRINTF
#include <stdarg.h>
#endif
#include "parser-utilities.h"
#include "lg_readline.h"
char *expand_homedir(const char *filename)
{
if (filename[0] != '~') return strdup(filename);
#ifndef _WIN32
char *user = NULL;
const char *user_end = &filename[strcspn(filename, "/")];
if (user_end != &filename[1])
{
user = strdup(filename + 1);
user[user_end - filename - 1] = '\0';
}
#endif
#ifdef _WIN32
char *home;
const char *homepath = getenv("HOMEPATH");
if ((homepath == NULL) || (homepath[0] == '\0')) return strdup(filename);
const char *homedrive = getenv("HOMEDRIVE");
if (homedrive == NULL) homedrive = "";
home = malloc(strlen(homepath) + strlen(homedrive) + 1);
strcpy(home, homedrive);
strcat(home, homepath);
filename++;
#else
const char *home;
if (user == NULL)
{
home = getenv("HOME");
if ((home == NULL) || (home[0] == '\0')) return strdup(filename);
filename++;
}
else
{
struct passwd *pwd;
pwd = getpwnam(user);
free(user);
if (pwd == NULL) return strdup(filename);
home = pwd->pw_dir;
filename = user_end;
}
#endif
size_t filename_len = strlen(filename);
size_t home_len = strlen(home);
char *eh_filename = malloc(home_len + filename_len + 1);
memcpy(eh_filename, home, home_len);
memcpy(eh_filename + home_len, filename, filename_len + 1);
#ifdef _WIN32
free(home);
#endif
return eh_filename;
}
#ifdef _WIN32
#define INPUT_UTF16_SIZE 4096
int get_console_line(char *inbuf, int inbuf_size)
{
static HANDLE console_handle = NULL;
wchar_t winbuf[INPUT_UTF16_SIZE];
static bool eof;
if (eof) return 0;
if (NULL == console_handle)
{
console_handle = CreateFileA("CONIN$", GENERIC_READ, FILE_SHARE_READ,
NULL, OPEN_EXISTING, 0, NULL);
if (!console_handle || (INVALID_HANDLE_VALUE == console_handle))
{
snprintf(inbuf, inbuf_size, "CreateFileA CONIN$: Error %lu",
GetLastError());
return -1;
}
}
DWORD nchar;
if (!ReadConsoleW(console_handle, &winbuf, INPUT_UTF16_SIZE-1, &nchar, NULL))
{
snprintf(inbuf, inbuf_size, "ReadConsoleW: Error %lu\n", GetLastError());
return -1;
}
winbuf[nchar] = L'\0';
nchar = WideCharToMultiByte(CP_UTF8, 0, winbuf, -1, inbuf,
inbuf_size, NULL, NULL);
if (0 == nchar)
{
DWORD err = GetLastError();
if (err == ERROR_INSUFFICIENT_BUFFER)
snprintf(inbuf, inbuf_size, "Input line too long (>%d)", inbuf_size-3);
else
snprintf(inbuf, inbuf_size, "WideCharToMultiByte CP_UTF8: Error %lu",
err);
return -1;
}
const char *invalid_char  = strstr(inbuf, "\xEF\xBF\xBD");
if (NULL != invalid_char)
{
prt_error("Error: Unable to process UTF8 in input string.\n");
inbuf[0] = '\0';
}
char *ctrl_z_position = strchr(inbuf, '\x1A');
if (ctrl_z_position != NULL)
{
if (ctrl_z_position == inbuf) return 0;
*ctrl_z_position = '\n';
eof = true;
return 1;
}
return 1;
}
static int console_input_cp;
static int console_output_cp;
static void restore_console_cp(void)
{
SetConsoleCP(console_input_cp);
SetConsoleOutputCP(console_output_cp);
}
static BOOL CtrlHandler(DWORD fdwCtrlType)
{
if ((CTRL_C_EVENT == fdwCtrlType) || (CTRL_BREAK_EVENT  == fdwCtrlType))
{
fprintf(stderr, "Interrupt\n");
restore_console_cp();
exit(2);
}
return FALSE;
}
static void win32_set_utf8_output(void)
{
if (-1 == _setmode(fileno(stdout), _O_BINARY))
{
prt_error("Warning: _setmode(fileno(stdout), _O_BINARY): %s.\n",
strerror(errno));
}
console_input_cp = GetConsoleCP();
console_output_cp = GetConsoleOutputCP();
atexit(restore_console_cp);
if (!SetConsoleCtrlHandler((PHANDLER_ROUTINE)CtrlHandler, TRUE))
{
prt_error("Warning: Cannot not set code page restore handler.\n");
}
if (!SetConsoleCP(CP_UTF8))
{
prt_error("Warning: Cannot set input codepage %d (error %lu).\n",
CP_UTF8, GetLastError());
}
if (!SetConsoleOutputCP(CP_UTF8))
{
prt_error("Warning: Cannot set output codepage %d (error %lu).\n",
CP_UTF8, GetLastError());
}
}
#include <winternl.h>
int lg_isatty(int fd)
{
HANDLE fh;
long buf[66];
PFILE_NAME_INFO pfni = (PFILE_NAME_INFO)buf;
PWCHAR cp;
fh = (HANDLE)_get_osfhandle(fd);
if (!fh || (INVALID_HANDLE_VALUE == fh))
{
errno = EBADF;
return 0;
}
#if 0
if (_isatty(fd))
return 1;
#else
CONSOLE_SCREEN_BUFFER_INFO sbi;
DWORD mode;
if (GetConsoleMode(fh, &mode) || GetConsoleScreenBufferInfo(fh, &sbi))
return 1;
#endif
if (GetFileType(fh) != FILE_TYPE_PIPE)
goto no_tty;
if (!GetFileInformationByHandleEx(fh, FileNameInfo, pfni, sizeof(buf)))
{
printf("GetFileInformationByHandleEx: Error %lu\n", GetLastError());
goto no_tty;
}
pfni->FileName[pfni->FileNameLength / sizeof (WCHAR)] = L'\0';
cp = pfni->FileName;
if ((!wcsncmp(cp, L"\\cygwin-", 8) && !wcsncmp(cp + 24, L"-pty", 4)) ||
(!wcsncmp(cp, L"\\msys-", 6)   && !wcsncmp(cp + 22, L"-pty", 4)))
{
cp = wcschr(cp + 26, '-');
if (!cp)
goto no_tty;
if (!wcsncmp(cp, L"-from-master", 12) || !wcsncmp(cp, L"-to-master", 10))
return 1;
}
no_tty:
errno = ENOTTY;
return 0;
}
static char **utf8_argv;
static int utf8_argc;
static void argv2utf8_free(void)
{
for (int i = 0; i < utf8_argc; i++)
free(utf8_argv[i]);
free(utf8_argv);
}
static char **argv2utf8(int argc)
{
char **nargv = malloc(argc * sizeof(char *));
LPWSTR *warglist = CommandLineToArgvW(GetCommandLineW(), &argc);
if (NULL == warglist) return NULL;
for (int i = 0; i < argc; i++)
{
int n = WideCharToMultiByte(CP_UTF8, 0, warglist[i], -1, NULL, 0, NULL, NULL);
nargv[i] = malloc(n);
n = WideCharToMultiByte(CP_UTF8, 0, warglist[i], -1, nargv[i], n, NULL, NULL);
if (0 == n)
{
prt_error("Error: WideCharToMultiByte CP_UTF8 failed: Error %lu.\n",
GetLastError());
return NULL;
}
}
LocalFree(warglist);
utf8_argv = nargv;
utf8_argc = argc;
atexit(argv2utf8_free);
return nargv;
}
static bool running_under_cygwin;
char **ms_windows_setup(int argc)
{
const char *ostype = getenv("OSTYPE");
if ((NULL != ostype) && (0 == strcmp(ostype, "cygwin")))
running_under_cygwin = true;
char **argv = argv2utf8(argc);
if (NULL == argv)
{
prt_error("Fatal error: Unable to parse command line\n");
exit(-1);
}
win32_set_utf8_output();
return argv;
}
#endif
static int fgets_with_check(char *inbuf, unsigned int inbuf_size, FILE *fh)
{
const char *rc = fgets(inbuf, inbuf_size, fh);
if (rc == NULL)
{
if (!ferror(fh)) return 0;
snprintf(inbuf, inbuf_size, "fgets(): %s", strerror(errno));
return -1;
}
size_t len = strlen(inbuf);
if ((len == inbuf_size -1) && inbuf[len -2] != '\n')
{
snprintf(inbuf, inbuf_size, "Input line too long (>%u).", inbuf_size-2);
return -1;
}
return 1;
}
static int get_terminal_line(const char *uprompt, char **buf,
unsigned int bufsize, FILE *in, FILE *out, bool tty)
{
int rc;
#ifdef HAVE_EDITLINE
*buf = lg_readline(uprompt);
rc = (*buf != NULL);
#else
fprintf(out, "%s", uprompt);
fflush(out);
#ifdef _WIN32
if (!running_under_cygwin && tty)
{
rc = get_console_line(*buf, bufsize);
}
else
#endif
{
rc = fgets_with_check(*buf, bufsize, in);
}
#endif
return rc;
}
bool get_line(const char *uprompt, char **buf, unsigned int bufsize,
FILE *in, FILE *out, bool tty)
{
int rc;
if ((in != stdin) || !tty)
{
rc = fgets_with_check(*buf, bufsize, in);
}
else
{
rc = get_terminal_line(uprompt, buf, bufsize, in, out, tty);
}
if (rc == 0) return false;
if (rc == -1)
{
prt_error("Fatal error: %s\n", *buf);
return false;
}
return true;
}
static unsigned int screen_width = INITIAL_SCREEN_WIDTH;
static void get_screen_width(int sig)
{
static int isatty_stdout = -1;
if (isatty_stdout == -1) isatty_stdout = isatty(fileno(stdout));
if (!isatty_stdout) return;
int fd = fileno(stdout);
#ifdef _WIN32
HANDLE console;
CONSOLE_SCREEN_BUFFER_INFO info;
console = (HANDLE)_get_osfhandle(fd);
if (!console || (console == INVALID_HANDLE_VALUE)) return;
if (GetConsoleScreenBufferInfo(console, &info) == 0) return;
screen_width = info.dwSize.X;
return;
#else
struct winsize ws;
if (fd < 0) return;
if (0 != ioctl(fd, TIOCGWINSZ, &ws))
{
perror("stdout: ioctl TIOCGWINSZ");
return;
}
if ((10 < ws.ws_col) && (16123 > ws.ws_col))
screen_width = ws.ws_col;
#endif
}
void set_screen_width(Command_Options* copts)
{
#if !defined SIGWINCH || defined _WIN32
get_screen_width(0);
#endif
copts->screen_width = screen_width;
}
#ifdef INTERRUPT_EXIT
static void interrupt_exit(int n)
{
exit(128+n);
}
#endif
void initialize_screen_width(Command_Options *copts)
{
#ifdef SIGWINCH
#if HAVE_SIGACTION
struct sigaction winch_act = { { 0 } };
winch_act.sa_handler = get_screen_width;
sigemptyset(&winch_act.sa_mask);
winch_act.sa_flags = 0;
if (sigaction(SIGWINCH, &winch_act, NULL) == -1)
perror("sigaction SIGWINCH");
#else
if (signal(SIGWINCH, get_screen_width) == SIG_ERR)
perror("signal SIGWINCH");
#endif
#endif
#ifdef INTERRUPT_EXIT
(void)signal(SIGINT, interrupt_exit);
(void)signal(SIGTERM, interrupt_exit);
#endif
get_screen_width(0);
set_screen_width(copts);
}
#ifdef __MINGW32__
int __mingw_vfprintf (FILE * __restrict__ stream, const char * __restrict__ fmt, va_list vl)
{
int n = vsnprintf(NULL, 0, fmt, vl);
if (0 > n) return n;
char *buf = malloc(n+1);
if (NULL == buf) return -1;
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
#if !HAVE_ASPRINTF
int vasprintf(char ** restrict buf, const char * restrict fmt, va_list vl)
{
va_list vl_copy;
va_copy(vl_copy, vl);
int n = vsnprintf(NULL, 0, fmt, vl);
if (n < 0) {
va_end(vl_copy);
return n;
}
*buf = malloc(n + 1);
if (*buf == NULL) {
va_end(vl_copy);
return -1;
}
n = vsnprintf(*buf, n + 1, fmt, vl_copy);
if (n < 0) free(*buf);
va_end(vl_copy);
return n;
}
int asprintf(char ** restrict buf, const char * restrict fmt, ...)
{
va_list args;
va_start(args, fmt);
int result = vasprintf(buf, fmt, args);
va_end(args);
return result;
}
#endif