#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
static void
my_strupr(char *s)
{
char *p = s;
while(*p){
if(islower((int) *p))
*p = toupper((int) *p);
p++;
}
}
#define BITSIZE(TYPE)						\
{								\
int b = 0; TYPE x = 1, zero = 0; char *pre = "U";		\
char tmp[128], tmp2[128];					\
while(x){ x <<= 1; b++; if(x < zero) pre=""; }		\
if(b >= len){						\
int tabs;						\
sprintf(tmp, "%sINT%d" , pre, len/8);			\
sprintf(tmp2, "typedef %s %s;", #TYPE, tmp);		\
my_strupr(tmp);						\
tabs = 5 - strlen(tmp2) / 8;				\
fprintf(f, "%s", tmp2);					\
while(tabs-- > 0) fprintf(f, "\t");			\
fprintf(f, "\n", b);			\
return;                                                 \
}								\
}
static void
try_signed(FILE *f, int len)
{
BITSIZE(signed char);
BITSIZE(short);
BITSIZE(int);
BITSIZE(long);
#ifdef HAVE_LONG_LONG
BITSIZE(long long);
#endif
fprintf(f, "\n", len);
}
static void
try_unsigned(FILE *f, int len)
{
BITSIZE(unsigned char);
BITSIZE(unsigned short);
BITSIZE(unsigned int);
BITSIZE(unsigned long);
#ifdef HAVE_LONG_LONG
BITSIZE(unsigned long long);
#endif
fprintf(f, "\n", len);
}
static int print_pre(FILE *f)
{
fprintf(f,
"\n"
"#ifndef MD5GLOBAL_H\n"
"#define MD5GLOBAL_H\n"
"\n"
"\n"
"#ifndef PROTOTYPES\n"
"#define PROTOTYPES 0\n"
"#endif\n"
"\n"
"\n"
"typedef unsigned char *POINTER;\n"
"\n"
);
return 1;
}
static int print_post(FILE *f)
{
fprintf(f, "\n"
"\n"
"#if PROTOTYPES\n"
"#define PROTO_LIST(list) list\n"
"#else\n"
"#define PROTO_LIST(list) ()\n"
"#endif\n"
"\n"
"#endif \n\n"
);
return 1;
}
int main(int argc, char **argv)
{
FILE *f;
char *fn, *hb;
if(argc < 2){
fn = "bits.h";
hb = "__BITS_H__";
f = stdout;
} else {
char *p;
fn = argv[1];
hb = malloc(strlen(fn) + 5);
sprintf(hb, "__%s__", fn);
for(p = hb; *p; p++){
if(!isalnum((int) *p))
*p = '_';
}
f = fopen(argv[1], "w");
}
print_pre(f);
#ifndef HAVE_INT8_T
try_signed (f, 8);
#endif
#ifndef HAVE_INT16_T
try_signed (f, 16);
#endif
#ifndef HAVE_INT32_T
try_signed (f, 32);
#endif
#ifndef HAVE_INT64_T
try_signed (f, 64);
#endif
#ifndef HAVE_U_INT8_T
try_unsigned (f, 8);
#endif
#ifndef HAVE_U_INT16_T
try_unsigned (f, 16);
#endif
#ifndef HAVE_U_INT32_T
try_unsigned (f, 32);
#endif
#ifndef HAVE_U_INT64_T
try_unsigned (f, 64);
#endif
print_post(f);
fclose(f);
return 0;
}