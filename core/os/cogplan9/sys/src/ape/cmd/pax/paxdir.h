#ifndef _PAX_DIRENT_H
#define _PAX_DIRENT_H
#include "config.h"
#ifdef USG
#define UFS
#else
#ifdef BSD
#define BFS
#endif
#endif
struct dirent {
long d_ino;
off_t d_off;
unsigned short d_reclen;
char d_name[1];
};
typedef struct {
int dd_fd;
int dd_loc;
int dd_size;
char *dd_buf;
} DIR;
#define DIRENTBASESIZ (((struct dirent *)0)->d_name \
- (char *)&((struct dirent *)0)->d_ino)
#define DIRENTSIZ( namlen ) ((DIRENTBASESIZ + sizeof(long) + (namlen)) \
/ sizeof(long) * sizeof(long))
#define MAXNAMLEN 512
#ifndef NAME_MAX
#define NAME_MAX (MAXNAMLEN - 1)
#endif
#define DIRBUF 8192
extern DIR *opendir();
extern struct dirent *readdir();
extern OFFSET telldir();
extern void seekdir();
extern int closedir();
#endif