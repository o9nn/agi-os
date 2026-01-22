#ifndef gsdll_INCLUDED
# define gsdll_INCLUDED
#include "iapi.h"
#ifdef __MACOS__
#define HWND char *
#include <QDOffscreen.h>
#pragma export on
#endif
#ifdef __WINDOWS__
#define _Windows
#endif
#ifdef __IBMC__
#define GSDLLCALLLINK _System
#else
#define GSDLLCALLLINK
#endif
typedef int (* GSDLLCALLLINK GSDLL_CALLBACK) (int, char *, unsigned long);
extern GSDLL_CALLBACK pgsdll_callback;
#define GSDLL_STDIN 1
#define GSDLL_STDOUT 2
#define GSDLL_DEVICE 3
#define GSDLL_SYNC 4
#define GSDLL_PAGE 5
#define GSDLL_SIZE 6
#define GSDLL_POLL 7
#define GSDLL_INIT_IN_USE 100
#define GSDLL_INIT_QUIT 101
GSDLLEXPORT int GSDLLAPI gsdll_revision(const char * * product, const char * * copyright, long * gs_revision, long * gs_revisiondate);
GSDLLEXPORT int GSDLLAPI gsdll_init(GSDLL_CALLBACK callback, HWND hwnd, int argc, char * * argv);
GSDLLEXPORT int GSDLLAPI gsdll_execute_begin(void);
GSDLLEXPORT int GSDLLAPI gsdll_execute_cont(const char * str, int len);
GSDLLEXPORT int GSDLLAPI gsdll_execute_end(void);
GSDLLEXPORT int GSDLLAPI gsdll_exit(void);
GSDLLEXPORT int GSDLLAPI gsdll_lock_device(unsigned char *device, int flag);
typedef int (GSDLLAPIPTR PFN_gsdll_revision)(const char ** product,
const char ** copyright, long * revision, long * revisiondate);
typedef int (GSDLLAPIPTR PFN_gsdll_init) (GSDLL_CALLBACK, HWND, int argc, char * * argv);
typedef int (GSDLLAPIPTR PFN_gsdll_execute_begin) (void);
typedef int (GSDLLAPIPTR PFN_gsdll_execute_cont) (const char * str, int len);
typedef int (GSDLLAPIPTR PFN_gsdll_execute_end) (void);
typedef int (GSDLLAPIPTR PFN_gsdll_exit) (void);
typedef int (GSDLLAPIPTR PFN_gsdll_lock_device) (unsigned char *, int);
#ifdef __MACOS__
#pragma export off
#endif
#endif