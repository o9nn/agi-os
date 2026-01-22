#ifndef _PAX_H
#define _PAX_H
#include "config.h"
#include "limits.h"
#include <stdio.h>
#include <errno.h>
#include <signal.h>
#include <ctype.h>
#include <sys/types.h>
#ifndef _POSIX_SOURCE
#include <sys/ioctl.h>
#endif
#include <sys/stat.h>
#include "regexp.h"
#if defined(DIRENT) || defined(_POSIX_SOURCE)
# ifdef PAXDIR
# include "paxdir.h"
# else
# include <dirent.h>
# endif
#else
# ifdef hpux
# include <ndir.h>
# else
# ifdef XENIX_286
# include <sys/ndir.h>
# else XENIX_286
# include <sys/dir.h>
# endif XENIX_286
# endif
# define dirent direct
#endif
#ifdef _POSIX_SOURCE
#define major(x) 0
#define minor(x) 0
#else
#ifndef major
# include <sys/sysmacros.h>
#endif
#endif
#ifdef SYSTIME
# include <sys/time.h>
#else
# include <time.h>
#endif
#ifndef V7
# include <fcntl.h>
#endif
#ifdef XENIX
# include <sys/inode.h>
#endif
#ifdef XENIX_286
#include <sys/param.h>
#endif XENIX_286
#include <pwd.h>
#include <grp.h>
#ifndef XENIX_286
#ifndef _POSIX_SOURCE
#include <sys/file.h>
#endif
#endif
#ifdef _POSIX_SOURCE
#include <unistd.h>
#include <stdlib.h>
#endif
#define STDIN 0
#define STDOUT 1
#ifndef O_RDONLY
# define O_RDONLY 0
#endif
#ifndef O_WRONLY
# define O_WRONLY 1
#endif
#ifndef O_RDWR
# define O_WRONLY 2
#endif
#ifndef O_BINARY
# define O_BINARY 0
#endif
#ifndef NULL
# define NULL 0
#endif
#define TMAGIC "ustar"
#define TMAGLEN 6
#define TVERSION "00"
#define TVERSLEN 2
#define REGTYPE '0'
#define AREGTYPE '\0'
#define LNKTYPE '1'
#define SYMTYPE '2'
#define CHRTYPE '3'
#define BLKTYPE '4'
#define DIRTYPE '5'
#define FIFOTYPE '6'
#define CONTTYPE '7'
#define BLOCKSIZE 512
#define uint unsigned int
#define ushort unsigned short
#define BLOCK 5120
#define H_COUNT 10
#define H_PRINT "%06o%06o%06o%06o%06o%06o%06o%011lo%06o%011lo"
#define H_SCAN "%6ho%6ho%6ho%6ho%6ho%6ho%6ho%11lo%6o%11lo"
#define H_STRLEN 70
#define M_ASCII "070707"
#define M_BINARY 070707
#define M_STRLEN 6
#define PATHELEM 256
#define S_IFSHF 12
#define S_IPERM 07777
#define S_IPEXE 07000
#define S_IPOPN 0777
#ifdef _POSIX_SOURCE
#define S_IFMT 0170000
#define S_IFREG 0100000
#define S_IFDIR 0040000
#define S_IFCHR 0020000
#define S_IFBLK 0060000
#define S_IFIFO 0010000
#endif
#define TRAILER "TRAILER!!!"
#define TRAILZ 11
#include "port.h"
#define TAR 1
#define CPIO 2
#define PAX 3
#define AR_READ 0
#define AR_WRITE 1
#define AR_EXTRACT 2
#define AR_APPEND 4
#define NAMSIZ 100
#define PFIXSIZ 155
#define TUNMLEN 32
#define TGNMLEN 32
#define CHKBLANKS "        "
#define EX_SUCCESS 0
#define EX_ARGSBAD 1
#define EX_BADFILE 2
#define EX_BADARCH 3
#define EX_SYSTEM 4
#define ROUNDUP(a,b) (((a) % (b)) == 0 ? (a) : ((a) + ((b) - ((a) % (b)))))
#define MIN(a, b) (((a) < (b)) ? (a) : (b))
#ifdef _POSIX_SOURCE
#define REMOVE(name, asb) \
(S_ISDIR((asb)->sb_mode)? rmdir(name) : unlink(name))
#else
#define REMOVE(name, asb) \
(((asb)->sb_mode & S_IFMT) == S_IFDIR ? rmdir(name) : unlink(name))
#endif
#define USH(n) (((ushort) (n)) & 0177777)
typedef struct {
short b_dev;
ushort b_ino;
ushort b_mode;
ushort b_uid;
ushort b_gid;
short b_nlink;
short b_rdev;
ushort b_mtime[2];
ushort b_name;
ushort b_size[2];
} Binary;
typedef struct {
struct stat sb_stat;
char sb_link[PATH_MAX + 1];
} Stat;
#define STAT(name, asb) stat(name, &(asb)->sb_stat)
#define FSTAT(fd, asb) fstat(fd, &(asb)->sb_stat)
#define sb_dev sb_stat.st_dev
#define sb_ino sb_stat.st_ino
#define sb_mode sb_stat.st_mode
#define sb_nlink sb_stat.st_nlink
#define sb_uid sb_stat.st_uid
#define sb_gid sb_stat.st_gid
#define sb_rdev sb_stat.st_rdev
#define sb_size sb_stat.st_size
#define sb_atime sb_stat.st_atime
#define sb_mtime sb_stat.st_mtime
#define sb_ctime sb_stat.st_ctime
#ifdef S_IFLNK
# define LSTAT(name, asb) lstat(name, &(asb)->sb_stat)
# define sb_blksize sb_stat.st_blksize
# define sb_blocks sb_stat.st_blocks
#else
# define LSTAT(name, asb) stat(name, &(asb)->sb_stat)
#endif
typedef struct name {
struct name *p_forw;
struct name *p_back;
char *p_name;
} Path;
typedef struct link {
struct link *l_forw;
struct link *l_back;
dev_t l_dev;
ino_t l_ino;
ushort l_nlink;
OFFSET l_size;
char *l_name;
Path *l_path;
} Link;
typedef struct replstr {
regexp *comp;
char *replace;
char print;
char global;
struct replstr *next;
} Replstr;
#ifndef STRERROR
#define strerror xstrerror
#endif
#include "func.h"
#ifndef NO_EXTERN
extern char *ar_file;
extern char *bufend;
extern char *bufstart;
extern char *bufidx;
extern char *myname;
extern int archivefd;
extern int blocking;
extern uint blocksize;
extern int gid;
extern int head_standard;
extern int ar_interface;
extern int ar_format;
extern int mask;
extern int ttyf;
extern int uid;
extern OFFSET total;
extern short areof;
extern short f_append;
extern short f_create;
extern short f_extract;
extern short f_follow_links;
extern short f_interactive;
extern short f_linksleft;
extern short f_list;
extern short f_modified;
extern short f_verbose;
extern short f_link;
extern short f_owner;
extern short f_access_time;
extern short f_pass;
extern short f_pass;
extern short f_disposition;
extern short f_reverse_match;
extern short f_mtime;
extern short f_dir_create;
extern short f_unconditional;
extern short f_newer;
extern time_t now;
extern uint arvolume;
extern int names_from_stdin;
extern Replstr *rplhead;
extern Replstr *rpltail;
extern char **n_argv;
extern int n_argc;
extern FILE *msgfile;
#endif
extern char *optarg;
extern int optind;
#ifndef _POSIX_SOURCE
extern int sys_nerr;
extern char *sys_errlist[];
#endif
extern int errno;
#endif