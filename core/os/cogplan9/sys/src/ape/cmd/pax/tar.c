#ifndef lint
static char *ident = "$Id: tar.c,v 1.2 89/02/12 10:06:05 mark Exp $";
static char *copyright ="Copyright (c) 1989 Mark H. Colburn.\nAll rights reserved.";
#endif
#include "pax.h"
#define DEF_BLOCKING 20
#ifdef __STDC__
static int taropt(int , char **, char *);
static void usage(void);
#else
static int taropt();
static void usage();
#endif
#ifdef __STDC__
int do_tar(int argc, char **argv)
#else
int do_tar(argc, argv)
int argc;
char **argv;
#endif
{
int c;
names_from_stdin = 0;
ar_file = getenv("TAPE");
if (ar_file == 0) {
ar_file = DEF_AR_FILE;
}
f_unconditional = 1;
f_mtime = 1;
f_dir_create = 1;
blocking = 0;
ar_interface = TAR;
ar_format = TAR;
msgfile=stderr;
while ((c = taropt(argc, argv, "b:cf:hlmortuvwx")) != EOF) {
switch (c) {
case 'b':
blocking = atoi(optarg);
break;
case 'c':
f_create = 1;
break;
case 'f':
ar_file = optarg;
break;
case 'h':
f_follow_links = 1;
break;
case 'l':
f_linksleft = 1;
break;
case 'm':
f_modified = 1;
break;
case 'o':
break;
case 'r':
f_append = 1;
break;
case 't':
f_list = 1;
break;
case 'u':
f_newer = 1;
break;
case 'v':
f_verbose = 1;
break;
case 'w':
f_disposition = 1;
break;
case 'x':
f_extract = 1;
break;
case '?':
usage();
exit(EX_ARGSBAD);
}
}
if (f_create + f_extract + f_list + f_append + f_newer != 1) {
(void) fprintf(stderr,
"%s: you must specify exactly one of the c, t, r, u or x options\n",
myname);
usage();
exit(EX_ARGSBAD);
}
if (blocking == 0) {
#ifdef USG
if (f_extract || f_list) {
blocking = DEF_BLOCKING;
fprintf(stderr, "Tar: blocksize = %d\n", blocking);
} else {
blocking = 1;
}
#else
blocking = 20;
#endif
}
blocksize = blocking * BLOCKSIZE;
buf_allocate((OFFSET) blocksize);
if (f_create) {
open_archive(AR_WRITE);
create_archive();
} else if (f_extract) {
open_archive(AR_READ);
read_archive();
} else if (f_list) {
open_archive(AR_READ);
read_archive();
} else if (f_append) {
open_archive(AR_APPEND);
append_archive();
}
if (f_linksleft) {
linkleft();
}
return (0);
}
#ifdef __STDC__
static int taropt(int argc, char **argv, char *optstring)
#else
static int taropt(argc, argv, optstring)
int argc;
char **argv;
char *optstring;
#endif
{
extern char *optarg;
extern int optind;
static char *key;
static char use_getopt;
char c;
char *place;
optarg = (char *)NULL;
if (key == (char *)NULL) {
if (argc < 2)
return EOF;
key = argv[1];
if (*key == '-')
use_getopt++;
else
optind = 2;
}
if (use_getopt) {
return getopt(argc, argv, optstring);
}
c = *key++;
if (c == '\0') {
key--;
return EOF;
}
place = strchr(optstring, c);
if (place == (char *)NULL || c == ':') {
fprintf(stderr, "%s: unknown option %c\n", argv[0], c);
return ('?');
}
place++;
if (*place == ':') {
if (optind < argc) {
optarg = argv[optind];
optind++;
} else {
fprintf(stderr, "%s: %c argument missing\n",
argv[0], c);
return ('?');
}
}
return (c);
}
#ifdef __STDC__
static void usage(void)
#else
static void usage()
#endif
{
fprintf(stderr, "Usage: %s -c[bfvw] device block filename..\n", myname);
fprintf(stderr, "       %s -r[bvw] device block [filename...]\n", myname);
fprintf(stderr, "       %s -t[vf] device\n", myname);
fprintf(stderr, "       %s -u[bvw] device block [filename...]\n", myname);
fprintf(stderr, "       %s -x[flmovw] device [filename...]\n", myname);
exit(1);
}