#include "stdpre.h"
#include <stdio.h>
#include <stdlib.h>
#if defined(__sun__) && !defined(const)
extern int fputc(int, FILE *), fputs(const char *, FILE *);
#endif
#include <sys/types.h>
#include <ctype.h>
#include <string.h>
#include <time.h>
static int hputc(int, FILE *), hputs(const char *, FILE *);
int
main(int argc, char *argv[])
{
FILE *out = stdout;
FILE *in = 0;
const char *extn = "";
char fmode[4];
#define FNSIZE 100
char *fnparam;
char fname[FNSIZE];
int newline = 1;
int interact = 0;
int (*eputc)(int, FILE *) = fputc;
int (*eputs)(const char *, FILE *) = fputs;
#define LINESIZE 1000
char line[LINESIZE];
char sw = 0, sp = 0, hexx = 0;
char **argp = argv + 1;
int nargs = argc - 1;
if (nargs > 0 && !strcmp(*argp, "-e")) {
if (nargs < 2)
return 1;
extn = argp[1];
argp += 2, nargs -= 2;
}
if (nargs > 0 && (*argp)[0] == '-' &&
((*argp)[1] == 'w' || (*argp)[1] == 'a')
) {
size_t len = strlen(*argp);
int i;
if (len > 4)
return 1;
for (i = 1; i < nargs; i++)
if (argp[i][0] != '-')
break;
if (i == nargs)
return 1;
fnparam = argp[i];
strcpy(fmode, *argp + 1);
strcpy(fname, fnparam);
strcat(fname, extn);
if (fmode[len - 2] == '-') {
static char dash[2] = { '-', 0 };
fmode[len - 2] = 0;
argp[i] = dash;
argp++, nargs--;
} else {
for (; i > 1; i--)
argp[i] = argp[i - 1];
argp += 2, nargs -= 2;
}
} else
strcpy(fname, "");
if (nargs > 0 && !strcmp(*argp, "-h")) {
eputc = hputc, eputs = hputs;
argp++, nargs--;
}
if (nargs > 0 && !strcmp(*argp, "-n")) {
newline = 0;
argp++, nargs--;
}
if (strlen(fname) != 0) {
out = fopen(fname, fmode);
if (out == 0)
return 1;
}
while (1) {
char *arg;
if (interact) {
if (fgets(line, LINESIZE, in) == NULL) {
interact = 0;
if (in != stdin)
fclose(in);
continue;
}
line[strlen(line) - 1] = 0;
arg = line;
} else {
if (nargs == 0)
break;
arg = *argp;
argp++, nargs--;
}
if (sw == 0 && arg[0] == '-') {
char chr = arg[1];
sp = 0;
swc:switch (chr) {
case 'l':
chr = 'Q';
case 'q':
case 'Q':
if (arg[2] != 0) {
(*eputs) (arg + 2, out);
if (chr == 'Q')
(*eputc) (' ', out);
break;
}
case 'r':
case 'R':
case 'u':
case 'x':
sw = chr;
break;
case 's':
(*eputc) (' ', out);
break;
case 'i':
interact = 1;
in = stdin;
break;
case 'b':
case 'B':
arg = fnparam + strlen(fnparam);
while (arg > fnparam &&
(isalnum(arg[-1]) || arg[-1] == '_'))
--arg;
(*eputs) (arg, out);
break;
case 'd':
case 'D':
{
time_t t;
char str[26];
time(&t);
strcpy(str, ctime(&t));
str[24] = 0;
(*eputs) (str, out);
} break;
case 'f':
case 'F':
(*eputs) (fnparam, out);
break;
case 'X':
hexx = 1;
break;
case '+':
if (arg[1]) {
++arg;
chr = toupper(arg[1]);
goto swc;
}
case 0:
sw = '-';
break;
}
} else
switch (sw) {
case 0:
case '-':
if (hexx)
goto xx;
if (sp)
(*eputc) (' ', out);
(*eputs) (arg, out);
sp = 1;
break;
case 'q':
sw = 0;
(*eputs) (arg, out);
break;
case 'Q':
sw = 0;
(*eputs) (arg, out);
(*eputc) (' ', out);
break;
case 'r':
sw = 0;
in = fopen(arg, "r");
if (in == NULL)
exit(exit_FAILED);
interact = 1;
break;
case 'R':
sw = 0;
in = fopen(arg, "r");
if (in == NULL)
exit(exit_FAILED);
while (fread(line, 1, 1, in) > 0)
(*eputc) (line[0], out);
fclose(in);
break;
case 'u':
{
char *up;
for (up = arg; *up; up++)
(*eputc) (toupper(*up), out);
}
sw = 0;
break;
case 'x':
xx:{
char *xp;
unsigned int xchr = 1;
for (xp = arg; *xp; xp++) {
char ch = *xp;
if (!isxdigit(ch))
return 1;
xchr <<= 4;
xchr += (isdigit(ch) ? ch - '0' :
(isupper(ch) ? tolower(ch) : ch)
- 'a' + 10);
if (xchr >= 0x100) {
(*eputc) (xchr & 0xff, out);
xchr = 1;
}
}
}
sw = 0;
break;
}
}
if (newline)
(*eputc) ('\n', out);
if (out != stdout)
fclose(out);
return exit_OK;
}
static int
hputc(int ch, FILE * out)
{
static const char *hex = "0123456789abcdef";
putc(hex[(ch >> 4) & 0xf], out);
putc(hex[ch & 0xf], out);
return 0;
}
static int
hputs(const char *str, FILE * out)
{
while (*str)
hputc(*str++ & 0xff, out);
return 0;
}