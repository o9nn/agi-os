#ifndef windows__INCLUDED
# define windows__INCLUDED
#define STRICT
#include <windows.h>
#ifdef __WATCOMC__
typedef RGBQUAD FAR * LPRGBQUAD;
# define BEGIN_THREAD(proc, stksize, data)\
_beginthread(proc, NULL, stksize, data)
#else
# define BEGIN_THREAD(proc, stksize, data)\
_beginthread(proc, stksize, data)
# define AllocAlias16(ptr) ((DWORD)(ptr))
# define FreeAlias16(dword)
# define MK_FP16(fp32) ((DWORD)(fp32))
# define MK_FP32(fp16) (fp16)
# define GetProc16(proc, ptype) (proc)
# define ReleaseProc16(cbp)
#endif
#ifdef __WIN32__
# undef _fstrtok
# define _fstrtok(str, set) strtok(str, set)
#endif
#if defined(__BORLANDC__)
# define exception_code() __exception_code
#endif
#endif