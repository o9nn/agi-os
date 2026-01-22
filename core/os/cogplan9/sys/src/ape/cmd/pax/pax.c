#ifndef lint
static char *ident = "$Id: pax.c,v 1.2 89/02/12 10:05:17 mark Exp $";
static char *copyright = "Copyright (c) 1989 Mark H. Colburn.\nAll rights reserved.\n";
#endif
#define NO_EXTERN
#include "pax.h"
char           *ar_file;
char           *bufend;
char           *bufstart;
char           *bufidx;
char           *myname;
char          **n_argv;
int             n_argc;
int             archivefd;
int             blocking;
int             gid;
int             head_standard;
int             ar_interface;
int             ar_format;
int             mask;
int             ttyf;
int             uid;
int		names_from_stdin;
OFFSET          total;
short           f_access_time;
short           areof;
short           f_dir_create;
short           f_append;
short           f_create;
short           f_extract;
short           f_follow_links;
short           f_interactive;
short           f_linksleft;
short           f_list;
short           f_modified;
short           f_verbose;
short		f_link;
short		f_owner;
short		f_pass;
short           f_newer;
short		f_disposition;
short           f_reverse_match;
short           f_mtime;
short           f_unconditional;
time_t          now = 0;
uint            arvolume;
uint            blocksize = BLOCKSIZE;
FILE	       *msgfile;
Replstr        *rplhead = (Replstr *)NULL;
Replstr        *rpltail;
#ifdef __STDC__
static void 	usage(void);
static OFFSET   pax_optsize(char *);
#else
static void 	usage();
static OFFSET   pax_optsize();
#endif
#ifdef __STDC__
int main(int argc, char **argv)
#else
int main(argc, argv)
int             argc;
char          **argv;
#endif
{
if ((myname = strrchr(argv[0], '/')) != (char *)NULL) {
myname++;
} else {
myname = argv[0];
}
name_init(argc, argv);
mask = umask(0);
uid = getuid();
gid = getgid();
now = time((time_t *) 0);
ttyf = open_tty();
if (strcmp(myname, "tar")==0) {
do_tar(argc, argv);
} else if (strcmp(myname, "cpio")==0) {
do_cpio(argc, argv);
} else {
do_pax(argc, argv);
}
exit(0);
}
#ifdef __STDC__
int do_pax(int ac, char **av)
#else
int do_pax(ac, av)
int             ac;
char          **av;
#endif
{
int             c;
char	   *dirname;
Stat	    st;
ar_file = "-";
f_unconditional = 1;
f_mtime = 1;
f_dir_create = 1;
f_list = 1;
blocksize = 0;
blocking = 0;
ar_interface = PAX;
ar_format = TAR;
msgfile=stdout;
while ((c = getopt(ac, av, "ab:cdf:ilmoprs:t:uvwx:y")) != EOF) {
switch (c) {
case 'a':
f_append = 1;
f_list = 0;
break;
case 'b':
if ((blocksize = pax_optsize(optarg)) == 0) {
fatal("Bad block size");
}
break;
case 'c':
f_reverse_match = 1;
break;
case 'd':
f_dir_create = 0;
break;
case 'f':
if (blocksize == 0) {
blocking = 1;
blocksize = 1 * BLOCKSIZE;
}
ar_file = optarg;
break;
case 'i':
f_interactive = 1;
break;
case 'l':
f_link = 1;
break;
case 'm':
f_mtime = 0;
break;
case 'o':
f_owner = 1;
break;
case 'p':
f_access_time = 1;
break;
case 'r':
if (f_create) {
f_create = 0;
f_pass = 1;
} else {
f_list = 0;
f_extract = 1;
}
msgfile=stderr;
break;
case 's':
add_replstr(optarg);
break;
case 't':
if (blocksize == 0) {
blocking = 1;
blocksize = 10 * BLOCKSIZE;
}
ar_file = optarg;
break;
case 'u':
f_unconditional = 1;
break;
case 'v':
f_verbose = 1;
break;
case 'w':
if (f_extract) {
f_extract = 0;
f_pass = 1;
} else {
f_list = 0;
f_create = 1;
}
msgfile=stderr;
break;
case 'x':
if (strcmp(optarg, "ustar") == 0) {
ar_format = TAR;
} else if (strcmp(optarg, "cpio") == 0) {
ar_format = CPIO;
} else {
usage();
}
break;
case 'y':
f_disposition = 1;
break;
default:
usage();
}
}
if (blocksize == 0) {
blocking = 1;
blocksize = blocking * BLOCKSIZE;
}
buf_allocate((OFFSET) blocksize);
if (f_extract || f_list) {
open_archive(AR_READ);
get_archive_type();
read_archive();
} else if (f_create) {
if (optind >= n_argc) {
names_from_stdin++;
}
open_archive(AR_WRITE);
create_archive();
} else if (f_append) {
open_archive(AR_APPEND);
get_archive_type();
append_archive();
} else if (f_pass && optind < n_argc) {
dirname = n_argv[--n_argc];
if (LSTAT(dirname, &st) < 0) {
fatal(strerror());
}
if ((st.sb_mode & S_IFMT) != S_IFDIR) {
fatal("Not a directory");
}
if (optind >= n_argc) {
names_from_stdin++;
}
pass(dirname);
} else {
usage();
}
return (0);
}
#ifdef __STDC__
void get_archive_type(void)
#else
void get_archive_type()
#endif
{
if (ar_read() != 0) {
fatal("Unable to determine archive type.");
}
if (strncmp(bufstart, "070707", 6) == 0) {
ar_format = CPIO;
if (f_verbose) {
fputs("CPIO format archive\n", stderr);
}
} else if (strncmp(&bufstart[257], "ustar", 5) == 0) {
ar_format = TAR;
if (f_verbose) {
fputs("USTAR format archive\n", stderr);
}
} else {
ar_format = TAR;
}
}
#ifdef __STDC__
static OFFSET pax_optsize(char *str)
#else
static OFFSET pax_optsize(str)
char           *str;
#endif
{
char           *idx;
OFFSET          number;
OFFSET          result;
result = 0;
idx = str;
for (;;) {
number = 0;
while (*idx >= '0' && *idx <= '9')
number = number * 10 + *idx++ - '0';
switch (*idx++) {
case 'b':
result += number * 512L;
continue;
case 'k':
result += number * 1024L;
continue;
case 'm':
result += number * 1024L * 1024L;
continue;
case '+':
result += number;
continue;
case '\0':
result += number;
break;
default:
break;
}
break;
}
if (*--idx) {
fatal("Unrecognizable value");
}
return (result);
}
#ifdef __STDC__
static void usage(void)
#else
static void usage()
#endif
{
fprintf(stderr, "Usage: %s -[cimopuvy] [-f archive] [-s replstr] [-t device] [pattern...]\n",
myname);
fprintf(stderr, "       %s -r [-cimopuvy] [-f archive] [-s replstr] [-t device] [pattern...]\n",
myname);
fprintf(stderr, "       %s -w [-adimuvy] [-b blocking] [-f archive] [-s replstr]\n              [-t device] [-x format] [pathname...]\n",
myname);
fprintf(stderr, "       %s -r -w [-ilmopuvy] [-s replstr] [pathname...] directory\n",
myname);
exit(1);
}