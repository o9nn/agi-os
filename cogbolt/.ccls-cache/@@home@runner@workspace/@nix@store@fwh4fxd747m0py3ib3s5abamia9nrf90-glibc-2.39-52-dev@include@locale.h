#ifndef	_LOCALE_H
#define	_LOCALE_H	1
#include <features.h>
#define __need_NULL
#include <stddef.h>
#include <bits/locale.h>
__BEGIN_DECLS
#define LC_CTYPE          __LC_CTYPE
#define LC_NUMERIC        __LC_NUMERIC
#define LC_TIME           __LC_TIME
#define LC_COLLATE        __LC_COLLATE
#define LC_MONETARY       __LC_MONETARY
#define LC_MESSAGES       __LC_MESSAGES
#define	LC_ALL		  __LC_ALL
#define LC_PAPER	  __LC_PAPER
#define LC_NAME		  __LC_NAME
#define LC_ADDRESS	  __LC_ADDRESS
#define LC_TELEPHONE	  __LC_TELEPHONE
#define LC_MEASUREMENT	  __LC_MEASUREMENT
#define LC_IDENTIFICATION __LC_IDENTIFICATION
struct lconv
{
char *decimal_point;
char *thousands_sep;
char *grouping;
char *int_curr_symbol;
char *currency_symbol;
char *mon_decimal_point;
char *mon_thousands_sep;
char *mon_grouping;
char *positive_sign;
char *negative_sign;
char int_frac_digits;
char frac_digits;
char p_cs_precedes;
char p_sep_by_space;
char n_cs_precedes;
char n_sep_by_space;
char p_sign_posn;
char n_sign_posn;
#ifdef __USE_ISOC99
char int_p_cs_precedes;
char int_p_sep_by_space;
char int_n_cs_precedes;
char int_n_sep_by_space;
char int_p_sign_posn;
char int_n_sign_posn;
#else
char __int_p_cs_precedes;
char __int_p_sep_by_space;
char __int_n_cs_precedes;
char __int_n_sep_by_space;
char __int_p_sign_posn;
char __int_n_sign_posn;
#endif
};
extern char *setlocale (int __category, const char *__locale) __THROW;
extern struct lconv *localeconv (void) __THROW;
#ifdef	__USE_XOPEN2K8
# include <bits/types/locale_t.h>
extern locale_t newlocale (int __category_mask, const char *__locale,
locale_t __base) __THROW;
# define LC_CTYPE_MASK		(1 << __LC_CTYPE)
# define LC_NUMERIC_MASK	(1 << __LC_NUMERIC)
# define LC_TIME_MASK		(1 << __LC_TIME)
# define LC_COLLATE_MASK	(1 << __LC_COLLATE)
# define LC_MONETARY_MASK	(1 << __LC_MONETARY)
# define LC_MESSAGES_MASK	(1 << __LC_MESSAGES)
# define LC_PAPER_MASK		(1 << __LC_PAPER)
# define LC_NAME_MASK		(1 << __LC_NAME)
# define LC_ADDRESS_MASK	(1 << __LC_ADDRESS)
# define LC_TELEPHONE_MASK	(1 << __LC_TELEPHONE)
# define LC_MEASUREMENT_MASK	(1 << __LC_MEASUREMENT)
# define LC_IDENTIFICATION_MASK	(1 << __LC_IDENTIFICATION)
# define LC_ALL_MASK		(LC_CTYPE_MASK \
| LC_NUMERIC_MASK \
| LC_TIME_MASK \
| LC_COLLATE_MASK \
| LC_MONETARY_MASK \
| LC_MESSAGES_MASK \
| LC_PAPER_MASK \
| LC_NAME_MASK \
| LC_ADDRESS_MASK \
| LC_TELEPHONE_MASK \
| LC_MEASUREMENT_MASK \
| LC_IDENTIFICATION_MASK \
)
extern locale_t duplocale (locale_t __dataset) __THROW;
extern void freelocale (locale_t __dataset) __THROW;
extern locale_t uselocale (locale_t __dataset) __THROW;
# define LC_GLOBAL_LOCALE	((locale_t) -1L)
#endif
__END_DECLS
#endif