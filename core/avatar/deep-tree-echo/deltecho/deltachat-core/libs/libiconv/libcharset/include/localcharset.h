#ifndef _LOCALCHARSET_H
#define _LOCALCHARSET_H
#if 1 && BUILDING_LIBCHARSET
#define LIBCHARSET_DLL_EXPORTED __attribute__((__visibility__("default")))
#else
#define LIBCHARSET_DLL_EXPORTED
#endif
#ifdef __cplusplus
extern "C" {
#endif
extern LIBCHARSET_DLL_EXPORTED const char * locale_charset (void);
#ifdef __cplusplus
}
#endif
#endif