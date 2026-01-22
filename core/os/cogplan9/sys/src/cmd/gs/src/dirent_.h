#ifndef dirent__INCLUDED
# define dirent__INCLUDED
#include "std.h"
#include "gconfig_.h"
#ifdef HAVE_DIRENT_H
# include <dirent.h>
typedef struct dirent dir_entry;
#else
# ifdef HAVE_SYS_DIR_H
# include <sys/dir.h>
# endif
# ifdef HAVE_SYS_NDIR_H
# include <sys/ndir.h>
# endif
# ifdef HAVE_NDIR_H
# include <ndir.h>
# endif
typedef struct direct dir_entry;
#endif
#endif