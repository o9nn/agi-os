#ifndef lint
static char *ident = "$Id: cpio.c,v 1.2 89/02/12 10:04:13 mark Exp $";
static char *copyright = "Copyright (c) 1989 Mark H. Colburn.\nAll rights reserved.\n";
#endif
#include "pax.h"
#ifdef __STDC__
static void 	usage(void);
#else
static void 	usage();
#endif
#ifdef __STDC__
int do_cpio(int argc, char **argv)
#else
int do_cpio(argc, argv)
int             argc;
char          **argv;
#endif
{
int             c;
char           *dirname;
Stat            st;
ar_file = "-";
names_from_stdin = 1;
blocksize = BLOCKSIZE;
ar_interface = CPIO;
ar_format = CPIO;
msgfile=stderr;
while ((c = getopt(argc, argv, "D:Bacdfilmoprtuv")) != EOF) {
switch (c) {
case 'i':
f_extract = 1;
break;
case 'o':
f_create = 1;
break;
case 'p':
f_pass = 1;
dirname = argv[--argc];
if (LSTAT(dirname, &st) < 0) {
fatal(strerror());
}
if ((st.sb_mode & S_IFMT) != S_IFDIR) {
fatal("Not a directory");
}
break;
case 'B':
blocksize = BLOCK;
break;
case 'a':
f_access_time = 1;
break;
case 'c':
break;
case 'D':
ar_file = optarg;
break;
case 'd':
f_dir_create = 1;
break;
case 'f':
f_reverse_match = 1;
break;
case 'l':
f_link = 1;
break;
case 'm':
f_mtime = 1;
break;
case 'r':
f_interactive = 1;
break;
case 't':
f_list = 1;
break;
case 'u':
f_unconditional = 1;
break;
case 'v':
f_verbose = 1;
break;
default:
usage();
}
}
if (f_create + f_pass + f_extract != 1) {
usage();
}
if (!f_pass) {
buf_allocate((OFFSET) blocksize);
}
if (f_extract) {
open_archive(AR_READ);
read_archive();
} else if (f_create) {
open_archive(AR_WRITE);
create_archive();
} else if (f_pass) {
pass(dirname);
}
fprintf(stderr, "%ld Blocks\n", ROUNDUP(total, BLOCKSIZE) / BLOCKSIZE);
exit(0);
}
#ifdef __STDC__
static void usage(void)
#else
static void usage()
#endif
{
fprintf(stderr, "Usage: %s -o[Bacv]\n", myname);
fprintf(stderr, "       %s -i[Bcdmrtuvf] [pattern...]\n", myname);
fprintf(stderr, "       %s -p[adlmruv] directory\n", myname);
exit(1);
}