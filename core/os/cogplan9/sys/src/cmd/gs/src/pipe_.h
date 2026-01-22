#ifndef pipe__INCLUDED
# define pipe__INCLUDED
#include "stdio_.h"
#ifdef __WIN32__
extern FILE *mswin_popen(const char *cmd, const char *mode);
# define popen(cmd, mode) mswin_popen(cmd, mode)
# define pclose(file) _pclose(file)
#else
extern FILE *popen( );
extern int pclose(FILE *);
#endif
#endif