#ifndef gp_mswin_INCLUDED
# define gp_mswin_INCLUDED
#define GSTEXT_ICON 50
#define GSIMAGE_ICON 51
#define SPOOL_PORT 100
#define CANCEL_PCDONE 101
#define CANCEL_PRINTING 102
#ifndef RC_INVOKED
#define M_COPY_CLIP 1
#if defined(_WIN32) && defined(_MSC_VER)
#define _export
#endif
extern HINSTANCE phInstance;
extern const LPSTR szAppName;
extern BOOL is_win32s;
extern int is_spool(const char *queue);
#ifdef _WIN64
#define DLGRETURN INT_PTR
#else
#define DLGRETURN BOOL
#endif
#endif
#endif