#ifndef	__DIRENT_H
#define	__DIRENT_H
#pragma lib "/$M/lib/ape/libap.a"
#define MAXNAMLEN 255
struct	dirent {
char	d_name[MAXNAMLEN + 1];
};
typedef struct _dirdesc {
int	dd_fd;
long	dd_loc;
long	dd_size;
char	*dd_buf;
void *dirs;
int	dirsize;
int	dirloc;
} DIR;
#ifdef __cplusplus
extern "C" {
#endif
DIR		*opendir(const char *);
struct dirent	*readdir(DIR *);
void		rewinddir(DIR *);
int		closedir(DIR *);
#ifdef __cplusplus
}
#endif
#endif