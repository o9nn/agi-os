#ifndef _PARSER_UTILITIES_
#define _PARSER_UTILITIES_
#include "../link-grammar/link-includes.h"
#include "command-line.h"
char *expand_homedir(const char *fn);
void set_screen_width(Command_Options*);
void initialize_screen_width(Command_Options *);
int get_verbosity(void);
#define MAX_INPUT_LINE 2048
#ifdef _WIN32
#ifndef mkdir
#define mkdir(d, a) mkdir(d)
#endif
#ifndef __MINGW32__
#include <BaseTsd.h>
typedef SSIZE_T ssize_t;
#endif
#define strcasecmp _stricmp
#ifndef strncasecmp
#define strncasecmp _strnicmp
#endif
char **ms_windows_setup(int);
int get_console_line(char *inbuf, int inbuf_size);
int lg_isatty(int);
#define isatty lg_isatty
#else
#define ms_windows_setup(argc) (argv)
#endif
bool get_line(const char *, char **, unsigned int, FILE *, FILE *, bool);
#if !HAVE_ASPRINTF
int vasprintf(char ** restrict, const char * restrict, va_list)
GNUC_PRINTF(2,0);
int asprintf(char ** restrict, const char * restrict, ...)
GNUC_PRINTF(2,3);
#endif
#endif