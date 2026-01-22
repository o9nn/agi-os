#ifndef _LIBICONV_H
#define _LIBICONV_H
#define _LIBICONV_VERSION 0x010E
#if 1 && BUILDING_LIBICONV
#define LIBICONV_DLL_EXPORTED __attribute__((__visibility__("default")))
#else
#define LIBICONV_DLL_EXPORTED
#endif
extern LIBICONV_DLL_EXPORTED  int _libiconv_version;
#undef iconv_t
#define iconv_t libiconv_t
typedef void* iconv_t;
#include <stddef.h>
#include <errno.h>
#ifndef EILSEQ
#define EILSEQ
#endif
#ifdef __cplusplus
extern "C" {
#endif
#ifndef LIBICONV_PLUG
#define iconv_open libiconv_open
#endif
extern LIBICONV_DLL_EXPORTED iconv_t iconv_open (const char* tocode, const char* fromcode);
#ifndef LIBICONV_PLUG
#define iconv libiconv
#endif
extern LIBICONV_DLL_EXPORTED size_t iconv (iconv_t cd,  char* * inbuf, size_t *inbytesleft, char* * outbuf, size_t *outbytesleft);
#ifndef LIBICONV_PLUG
#define iconv_close libiconv_close
#endif
extern LIBICONV_DLL_EXPORTED int iconv_close (iconv_t cd);
#ifdef __cplusplus
}
#endif
#ifndef LIBICONV_PLUG
#if 1
#if 0
#include <stddef.h>
#include <stdio.h>
#include <time.h>
#endif
#include <wchar.h>
#endif
#ifdef __cplusplus
extern "C" {
#endif
typedef struct {
void* dummy1[28];
#if 1
mbstate_t dummy2;
#endif
} iconv_allocation_t;
#define iconv_open_into libiconv_open_into
extern LIBICONV_DLL_EXPORTED int iconv_open_into (const char* tocode, const char* fromcode,
iconv_allocation_t* resultp);
#define iconvctl libiconvctl
extern LIBICONV_DLL_EXPORTED int iconvctl (iconv_t cd, int request, void* argument);
typedef void (*iconv_unicode_char_hook) (unsigned int uc, void* data);
typedef void (*iconv_wide_char_hook) (wchar_t wc, void* data);
struct iconv_hooks {
iconv_unicode_char_hook uc_hook;
iconv_wide_char_hook wc_hook;
void* data;
};
typedef void (*iconv_unicode_mb_to_uc_fallback)
(const char* inbuf, size_t inbufsize,
void (*write_replacement) (const unsigned int *buf, size_t buflen,
void* callback_arg),
void* callback_arg,
void* data);
typedef void (*iconv_unicode_uc_to_mb_fallback)
(unsigned int code,
void (*write_replacement) (const char *buf, size_t buflen,
void* callback_arg),
void* callback_arg,
void* data);
#if 1
typedef void (*iconv_wchar_mb_to_wc_fallback)
(const char* inbuf, size_t inbufsize,
void (*write_replacement) (const wchar_t *buf, size_t buflen,
void* callback_arg),
void* callback_arg,
void* data);
typedef void (*iconv_wchar_wc_to_mb_fallback)
(wchar_t code,
void (*write_replacement) (const char *buf, size_t buflen,
void* callback_arg),
void* callback_arg,
void* data);
#else
typedef void (*iconv_wchar_mb_to_wc_fallback) ();
typedef void (*iconv_wchar_wc_to_mb_fallback) ();
#endif
struct iconv_fallbacks {
iconv_unicode_mb_to_uc_fallback mb_to_uc_fallback;
iconv_unicode_uc_to_mb_fallback uc_to_mb_fallback;
iconv_wchar_mb_to_wc_fallback mb_to_wc_fallback;
iconv_wchar_wc_to_mb_fallback wc_to_mb_fallback;
void* data;
};
#define ICONV_TRIVIALP            0
#define ICONV_GET_TRANSLITERATE   1
#define ICONV_SET_TRANSLITERATE   2
#define ICONV_GET_DISCARD_ILSEQ   3
#define ICONV_SET_DISCARD_ILSEQ   4
#define ICONV_SET_HOOKS           5
#define ICONV_SET_FALLBACKS       6
#define iconvlist libiconvlist
extern LIBICONV_DLL_EXPORTED void iconvlist (int (*do_one) (unsigned int namescount,
const char * const * names,
void* data),
void* data);
extern LIBICONV_DLL_EXPORTED const char * iconv_canonicalize (const char * name);
extern LIBICONV_DLL_EXPORTED void libiconv_set_relocation_prefix (const char *orig_prefix,
const char *curr_prefix);
#ifdef __cplusplus
}
#endif
#endif
#endif