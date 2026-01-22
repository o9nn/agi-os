#if defined(HAVE_DIRENT_H)
# include <dirent.h>
# define NLENGTH(dirent)	(strlen(dirent->d_name))
#else
# define dirent direct
# define NLENGTH(dirent)	(dirent->d_namlen)
# ifdef HAVE_SYS_NDIR_H
#  include <sys/ndir.h>
# endif
# ifdef HAVE_SYS_DIR_H
#  include <sys/dir.h>
# endif
# ifdef HAVE_NDIR_H
#  include <ndir.h>
# endif
#endif
#ifdef OPENDIR_DOES_NONDIR
extern DIR *ksh_opendir ARGS((const char *d));
#else
# define ksh_opendir(d)	opendir(d)
#endif