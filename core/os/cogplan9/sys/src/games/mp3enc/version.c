#ifdef HAVE_CONFIG_H
# include <config.h>
#endif
#include <stdio.h>
#include <lame.h>
#include "version.h"
#ifdef WITH_DMALLOC
#include <dmalloc.h>
#endif
#define STR(x)   #x
#define XSTR(x)  STR(x)
#if defined(MMX_choose_table)
# define V1  "MMX "
#else
# define V1  ""
#endif
#if defined(KLEMM)
# define V2  "KLM "
#else
# define V2  ""
#endif
#if defined(RH)
# define V3  "RH "
#else
# define V3  ""
#endif
#define V   V1 V2 V3
const char*  get_lame_version ( void )
{
#if   LAME_ALPHA_VERSION > 0
static  const char *const str =
XSTR(LAME_MAJOR_VERSION) "." XSTR(LAME_MINOR_VERSION) " " V
"(alpha " XSTR(LAME_ALPHA_VERSION) ", " __DATE__ " " __TIME__ ")";
#elif LAME_BETA_VERSION > 0
static  const char *const str =
XSTR(LAME_MAJOR_VERSION) "." XSTR(LAME_MINOR_VERSION) " " V
"(beta " XSTR(LAME_BETA_VERSION) ", " __DATE__ ")";
#else
static  const char *const str =
XSTR(LAME_MAJOR_VERSION) "." XSTR(LAME_MINOR_VERSION) " " V;
#endif
return str;
}
const char*  get_lame_short_version ( void )
{
#if   LAME_ALPHA_VERSION > 0
static  const char *const str =
XSTR(LAME_MAJOR_VERSION) "." XSTR(LAME_MINOR_VERSION) " (alpha)";
#elif LAME_BETA_VERSION > 0
static  const char *const str =
XSTR(LAME_MAJOR_VERSION) "." XSTR(LAME_MINOR_VERSION) " (beta)";
#else
static  const char *const str =
XSTR(LAME_MAJOR_VERSION) "." XSTR(LAME_MINOR_VERSION)
#endif
return str;
}
const char*  get_psy_version ( void )
{
#if   PSY_ALPHA_VERSION > 0
static  const char *const str =
XSTR(PSY_MAJOR_VERSION) "." XSTR(PSY_MINOR_VERSION)
" (alpha " XSTR(PSY_ALPHA_VERSION) ", " __DATE__ " " __TIME__ ")";
#elif PSY_BETA_VERSION > 0
static  const char *const str =
XSTR(PSY_MAJOR_VERSION) "." XSTR(PSY_MINOR_VERSION)
" (beta " XSTR(PSY_BETA_VERSION) ", " __DATE__ ")";
#else
static  const char *const str =
XSTR(PSY_MAJOR_VERSION) "." XSTR(PSY_MINOR_VERSION);
#endif
return str;
}
const char*  get_mp3x_version ( void )
{
#if   MP3X_ALPHA_VERSION > 0
static  const char *const str =
XSTR(MP3X_MAJOR_VERSION) "." XSTR(MP3X_MINOR_VERSION)
" (alpha " XSTR(MP3X_ALPHA_VERSION) ", " __DATE__ " " __TIME__ ")";
#elif MP3X_BETA_VERSION > 0
static  const char *const str =
XSTR(MP3X_MAJOR_VERSION) "." XSTR(MP3X_MINOR_VERSION)
" (beta " XSTR(MP3X_BETA_VERSION) ", " __DATE__ ")";
#else
static  const char *const str =
XSTR(MP3X_MAJOR_VERSION) "." XSTR(MP3X_MINOR_VERSION);
#endif
return str;
}
const char*  get_lame_url ( void )
{
static  const char *const str = LAME_URL;
return str;
}
void get_lame_version_numerical ( lame_version_t *const lvp )
{
static  const char *const features = V;
lvp->major = LAME_MAJOR_VERSION;
lvp->minor = LAME_MINOR_VERSION;
lvp->alpha = LAME_ALPHA_VERSION;
lvp->beta  = LAME_BETA_VERSION;
lvp->psy_major = PSY_MAJOR_VERSION;
lvp->psy_minor = PSY_MINOR_VERSION;
lvp->psy_alpha = PSY_ALPHA_VERSION;
lvp->psy_beta  = PSY_BETA_VERSION;
lvp->features = features;
}