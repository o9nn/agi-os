#if HAVE_CONFIG_H
# include <config.h>
#endif
#include <stdio.h>
#include <ctype.h>
#if HAVE_CONFIG_H
# if STDC_HEADERS || HAVE_STRING_H
#  include <string.h>
# else
#  include <strings.h>
# endif
#else
# ifdef BSD
#  include <strings.h>
# else
#  ifdef VMS
extern int strlen(), strncmp();
#  else
#   include <string.h>
#  endif
# endif
#endif
#if STDC_HEADERS
# include <stdlib.h>
#else
# ifdef MSDOS
#  include <malloc.h>
# else
#  ifdef VMS
extern char *malloc();
extern void free();
#  else
extern char *malloc();
extern int free();
#  endif
# endif
#endif
#ifdef isascii
#  undef HAVE_ISASCII
#  define HAVE_ISASCII 1
#else
#endif
#if STDC_HEADERS || !HAVE_ISASCII
#  define is_ascii(c) 1
#else
#  define is_ascii(c) isascii(c)
#endif
#define is_space(c) (is_ascii(c) && isspace(c))
#define is_alpha(c) (is_ascii(c) && isalpha(c))
#define is_alnum(c) (is_ascii(c) && isalnum(c))
#define isidchar(ch) (is_alnum(ch) || (ch) == '_')
#define isidfirstchar(ch) (is_alpha(ch) || (ch) == '_')
char *skipspace();
int writeblanks();
int test1();
int convert1();
int
main(argc, argv)
int argc;
char *argv[];
{	FILE *in, *out;
#define bufsize 5000
char *buf;
char *line;
char *more;
int convert_varargs = 1;
if ( argc > 1 && argv[1][0] == '-' )
{	if ( !strcmp(argv[1], "--varargs") )
{	convert_varargs = 1;
argc--;
argv++;
}
else
{	fprintf(stderr, "Unrecognized switch: %s\n", argv[1]);
exit(1);
}
}
switch ( argc )
{
default:
printf("Usage: ansi2knr input_file [output_file]\n");
exit(0);
case 2:
out = stdout;
break;
case 3:
out = fopen(argv[2], "w");
if ( out == NULL )
{	fprintf(stderr, "Cannot open output file %s\n", argv[2]);
exit(1);
}
}
in = fopen(argv[1], "r");
if ( in == NULL )
{	fprintf(stderr, "Cannot open input file %s\n", argv[1]);
exit(1);
}
fprintf(out, "#line 1 \"%s\"\n", argv[1]);
buf = malloc(bufsize);
line = buf;
while ( fgets(line, (unsigned)(buf + bufsize - line), in) != NULL )
{
test:		line += strlen(line);
switch ( test1(buf) )
{
case 2:
convert1(buf, out, 1, convert_varargs);
break;
case 1:
more = ++line;
f:			if ( line >= buf + (bufsize - 1) )
goto wl;
if ( fgets(line, (unsigned)(buf + bufsize - line), in) == NULL )
goto wl;
switch ( *skipspace(more, 1) )
{
case '{':
convert1(buf, out, 0, convert_varargs);
fputs(more, out);
break;
case 0:
line += strlen(line);
goto f;
default:
fputs(buf, out);
strcpy(buf, more);
line = buf;
goto test;
}
break;
case -1:
if ( line != buf + (bufsize - 1) )
continue;
default:
wl:			fputs(buf, out);
break;
}
line = buf;
}
if ( line != buf )
fputs(buf, out);
free(buf);
fclose(out);
fclose(in);
return 0;
}
char *
skipspace(p, dir)
register char *p;
register int dir;
{	for ( ; ; )
{	while ( is_space(*p) )
p += dir;
if ( !(*p == '/' && p[dir] == '*') )
break;
p += dir;  p += dir;
while ( !(*p == '*' && p[dir] == '/') )
{	if ( *p == 0 )
return p;
p += dir;
}
p += dir;  p += dir;
}
return p;
}
int
writeblanks(start, end)
char *start;
char *end;
{	char *p;
for ( p = start; p < end; p++ )
if ( *p != '\r' && *p != '\n' )
*p = ' ';
return 0;
}
int
test1(buf)
char *buf;
{	register char *p = buf;
char *bend;
char *endfn;
int contin;
if ( !isidfirstchar(*p) )
return 0;
bend = skipspace(buf + strlen(buf) - 1, -1);
switch ( *bend )
{
case ';': contin = 0 ; break;
case ')': contin = 1; break;
case '{': return 0;
case '}': return 0;
default: contin = -1;
}
while ( isidchar(*p) )
p++;
endfn = p;
p = skipspace(p, 1);
if ( *p++ != '(' )
return 0;
p = skipspace(p, 1);
if ( *p == ')' )
return 0;
{	static char *words[] =
{	"asm", "auto", "case", "char", "const", "double",
"extern", "float", "for", "if", "int", "long",
"register", "return", "short", "signed", "sizeof",
"static", "switch", "typedef", "unsigned",
"void", "volatile", "while", 0
};
char **key = words;
char *kp;
int len = endfn - buf;
while ( (kp = *key) != 0 )
{	if ( strlen(kp) == len && !strncmp(kp, buf, len) )
return 0;
key++;
}
}
return contin;
}
int
convert1(buf, out, header, convert_varargs)
char *buf;
FILE *out;
int header;
int convert_varargs;
{	char *endfn;
register char *p;
char **breaks;
unsigned num_breaks = 2;
char **btop;
char **bp;
char **ap;
char *vararg = 0;
for ( endfn = buf; *(endfn++) != '('; )
;
top:	p = endfn;
breaks = (char **)malloc(sizeof(char *) * num_breaks * 2);
if ( breaks == 0 )
{
fprintf(stderr, "Unable to allocate break table!\n");
fputs(buf, out);
return -1;
}
btop = breaks + num_breaks * 2 - 2;
bp = breaks;
do
{	int level = 0;
char *lp = NULL;
char *rp;
char *end = NULL;
if ( bp >= btop )
{
free((char *)breaks);
num_breaks <<= 1;
goto top;
}
*bp++ = p;
for ( ; end == NULL; p++ )
{	switch(*p)
{
case ',':
if ( !level ) end = p;
break;
case '(':
if ( !level ) lp = p;
level++;
break;
case ')':
if ( --level < 0 ) end = p;
else rp = p;
break;
case '/':
p = skipspace(p, 1) - 1;
break;
default:
;
}
}
if ( lp )
writeblanks(lp + 1, rp);
p--;
for ( ; ; )
{	p = skipspace(p - 1, -1);
switch ( *p )
{
case ']':
case ')':
{	int level = 1;
while ( level )
switch ( *--p )
{
case ']': case ')': level++; break;
case '[': case '(': level--; break;
case '/': p = skipspace(p, -1) + 1; break;
default: ;
}
}
if ( *p == '(' && *skipspace(p + 1, 1) == '*' )
{
while ( !isidfirstchar(*p) )
p = skipspace(p, 1) + 1;
goto found;
}
break;
default:
goto found;
}
}
found:		if ( *p == '.' && p[-1] == '.' && p[-2] == '.' )
{	if ( convert_varargs )
{	*bp++ = "va_alist";
vararg = p-2;
}
else
{	p++;
if ( bp == breaks + 1 )
writeblanks(breaks[0], p);
else
writeblanks(bp[-1] - 1, p);
bp--;
}
}
else
{	while ( isidchar(*p) ) p--;
*bp++ = p+1;
}
p = end;
}
while ( *p++ == ',' );
*bp = p;
if ( bp == breaks+2 )
{	p = skipspace(breaks[0], 1);
if ( !strncmp(p, "void", 4) )
{	p = skipspace(p+4, 1);
if ( p == breaks[2] - 1 )
{	bp = breaks;
writeblanks(breaks[0], p + 1);
}
}
}
p = buf;
while ( p != endfn ) putc(*p, out), p++;
if ( header )
{	fputs(");", out);
for ( p = breaks[0]; *p; p++ )
if ( *p == '\r' || *p == '\n' )
putc(*p, out);
}
else
{	for ( ap = breaks+1; ap < bp; ap += 2 )
{	p = *ap;
while ( isidchar(*p) )
putc(*p, out), p++;
if ( ap < bp - 1 )
fputs(", ", out);
}
fputs(")  ", out);
for ( ap = breaks+2; ap <= bp; ap += 2 )
(*ap)[-1] = ';';
if ( vararg != 0 )
{	*vararg = 0;
fputs(breaks[0], out);
fputs("va_dcl", out);
fputs(bp[0], out);
}
else
fputs(breaks[0], out);
}
free((char *)breaks);
return 0;
}