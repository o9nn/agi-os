#ifndef GS_DLL_CALL_H
#define GS_DLL_CALL_H
#ifdef __WINDOWS__
# define _Windows
#endif
#ifdef _Windows
# ifndef GSDLLEXPORT
# define GSDLLEXPORT __declspec(dllexport)
# endif
# ifndef GSDLLAPI
# define GSDLLAPI __stdcall
# endif
# ifndef GSDLLCALL
# define GSDLLCALL __stdcall
# endif
#endif
#if defined(OS2) && defined(__IBMC__)
# ifndef GSDLLAPI
# define GSDLLAPI _System
# endif
# ifndef GSDLLCALL
# define GSDLLCALL _System
# endif
#endif
#ifdef __MACOS__
# pragma export on
#endif
#ifndef GSDLLEXPORT
# define GSDLLEXPORT
#endif
#ifndef GSDLLAPI
# define GSDLLAPI
#endif
#ifndef GSDLLCALL
# define GSDLLCALL
#endif
#if defined(__IBMC__)
# define GSDLLAPIPTR * GSDLLAPI
# define GSDLLCALLPTR * GSDLLCALL
#else
# define GSDLLAPIPTR GSDLLAPI *
# define GSDLLCALLPTR GSDLLCALL *
#endif
#endif