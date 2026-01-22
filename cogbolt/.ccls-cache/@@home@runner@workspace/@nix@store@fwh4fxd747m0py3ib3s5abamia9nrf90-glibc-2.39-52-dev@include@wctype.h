#ifndef _WCTYPE_H
#define _WCTYPE_H 1
#include <features.h>
#include <bits/types.h>
#include <bits/types/wint_t.h>
#ifndef WEOF
# define WEOF (0xffffffffu)
#endif
#include <bits/wctype-wchar.h>
__BEGIN_DECLS
typedef const __int32_t *wctrans_t;
extern wctrans_t wctrans (const char *__property) __THROW;
extern wint_t towctrans (wint_t __wc, wctrans_t __desc) __THROW;
# ifdef __USE_XOPEN2K8
#  include <bits/types/locale_t.h>
extern int iswalnum_l (wint_t __wc, locale_t __locale) __THROW;
extern int iswalpha_l (wint_t __wc, locale_t __locale) __THROW;
extern int iswcntrl_l (wint_t __wc, locale_t __locale) __THROW;
extern int iswdigit_l (wint_t __wc, locale_t __locale) __THROW;
extern int iswgraph_l (wint_t __wc, locale_t __locale) __THROW;
extern int iswlower_l (wint_t __wc, locale_t __locale) __THROW;
extern int iswprint_l (wint_t __wc, locale_t __locale) __THROW;
extern int iswpunct_l (wint_t __wc, locale_t __locale) __THROW;
extern int iswspace_l (wint_t __wc, locale_t __locale) __THROW;
extern int iswupper_l (wint_t __wc, locale_t __locale) __THROW;
extern int iswxdigit_l (wint_t __wc, locale_t __locale) __THROW;
extern int iswblank_l (wint_t __wc, locale_t __locale) __THROW;
extern wctype_t wctype_l (const char *__property, locale_t __locale)
__THROW;
extern int iswctype_l (wint_t __wc, wctype_t __desc, locale_t __locale)
__THROW;
extern wint_t towlower_l (wint_t __wc, locale_t __locale) __THROW;
extern wint_t towupper_l (wint_t __wc, locale_t __locale) __THROW;
extern wctrans_t wctrans_l (const char *__property, locale_t __locale)
__THROW;
extern wint_t towctrans_l (wint_t __wc, wctrans_t __desc,
locale_t __locale) __THROW;
# endif
__END_DECLS
#endif