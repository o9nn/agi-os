#ifndef unistd__INCLUDED
# define unistd__INCLUDED
#include "std.h"
#ifdef __OS2__
# include <io.h>
#endif
#ifdef __WIN32__
# include <io.h>
#endif
#if defined(_MSC_VER)
# define fsync(handle) _commit(handle)
# define read(fd, buf, len) _read(fd, buf, len)
# define isatty(fd) _isatty(fd)
# define setmode(fd, mode) _setmode(fd, mode)
# define fstat(fd, buf) _fstat(fd, buf)
# define dup(fd) _dup(fd)
# define open(fname, flags, mode) _open(fname, flags, mode)
# define close(fd) _close(fd)
#elif defined(__BORLANDC__) && defined(__WIN32__)
# define fsync(handle) _commit(handle)
# define read(fd, buf, len) _read(fd, buf, len)
# define isatty(fd) _isatty(fd)
# define setmode(fd, mode) _setmode(fd, mode)
#else
# include <unistd.h>
#endif
#endif