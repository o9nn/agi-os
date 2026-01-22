#ifndef gxstdio_INCLUDED
# define gxstdio_INCLUDED
#include "gsio.h"
#undef stdin
#define stdin gs_stdin
#undef stdout
#define stdout gs_stdout
#undef stderr
#define stderr gs_stderr
#undef fgetchar
#endif