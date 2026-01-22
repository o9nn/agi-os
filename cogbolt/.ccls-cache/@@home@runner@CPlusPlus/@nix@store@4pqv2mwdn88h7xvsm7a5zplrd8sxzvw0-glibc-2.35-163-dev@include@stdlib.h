#ifndef _STDLIB_H
#define __GLIBC_INTERNAL_STARTING_HEADER_IMPLEMENTATION
#include <bits/libc-header-start.h>
#define __need_size_t
#define __need_wchar_t
#define __need_NULL
#include <stddef.h>
__BEGIN_DECLS
#define _STDLIB_H 1
#if (defined __USE_XOPEN || defined __USE_XOPEN2K8) && !defined _SYS_WAIT_H
# include <bits/waitflags.h>
# include <bits/waitstatus.h>
# define WEXITSTATUS(status) __WEXITSTATUS (status)
# define WTERMSIG(status) __WTERMSIG (status)
# define WSTOPSIG(status) __WSTOPSIG (status)
# define WIFEXITED(status) __WIFEXITED (status)
# define WIFSIGNALED(status) __WIFSIGNALED (status)
# define WIFSTOPPED(status) __WIFSTOPPED (status)
# ifdef __WIFCONTINUED
# define WIFCONTINUED(status) __WIFCONTINUED (status)
# endif
#endif
#include <bits/floatn.h>
typedef struct
{
int quot;
int rem;
} div_t;
#ifndef __ldiv_t_defined
typedef struct
{
long int quot;
long int rem;
} ldiv_t;
# define __ldiv_t_defined 1
#endif
#if defined __USE_ISOC99 && !defined __lldiv_t_defined
__extension__ typedef struct
{
long long int quot;
long long int rem;
} lldiv_t;
# define __lldiv_t_defined 1
#endif
#define RAND_MAX 2147483647
#define EXIT_FAILURE 1
#define EXIT_SUCCESS 0
#define MB_CUR_MAX (__ctype_get_mb_cur_max ())
extern size_t __ctype_get_mb_cur_max (void) __THROW __wur;
extern double atof (const char *__nptr)
__THROW __attribute_pure__ __nonnull ((1)) __wur;
extern int atoi (const char *__nptr)
__THROW __attribute_pure__ __nonnull ((1)) __wur;
extern long int atol (const char *__nptr)
__THROW __attribute_pure__ __nonnull ((1)) __wur;
#ifdef __USE_ISOC99
__extension__ extern long long int atoll (const char *__nptr)
__THROW __attribute_pure__ __nonnull ((1)) __wur;
#endif
extern double strtod (const char *__restrict __nptr,
char **__restrict __endptr)
__THROW __nonnull ((1));
#ifdef __USE_ISOC99
extern float strtof (const char *__restrict __nptr,
char **__restrict __endptr) __THROW __nonnull ((1));
extern long double strtold (const char *__restrict __nptr,
char **__restrict __endptr)
__THROW __nonnull ((1));
#endif
#if __HAVE_FLOAT16 && __GLIBC_USE (IEC_60559_TYPES_EXT)
extern _Float16 strtof16 (const char *__restrict __nptr,
char **__restrict __endptr)
__THROW __nonnull ((1));
#endif
#if __HAVE_FLOAT32 && __GLIBC_USE (IEC_60559_TYPES_EXT)
extern _Float32 strtof32 (const char *__restrict __nptr,
char **__restrict __endptr)
__THROW __nonnull ((1));
#endif
#if __HAVE_FLOAT64 && __GLIBC_USE (IEC_60559_TYPES_EXT)
extern _Float64 strtof64 (const char *__restrict __nptr,
char **__restrict __endptr)
__THROW __nonnull ((1));
#endif
#if __HAVE_FLOAT128 && __GLIBC_USE (IEC_60559_TYPES_EXT)
extern _Float128 strtof128 (const char *__restrict __nptr,
char **__restrict __endptr)
__THROW __nonnull ((1));
#endif
#if __HAVE_FLOAT32X && __GLIBC_USE (IEC_60559_TYPES_EXT)
extern _Float32x strtof32x (const char *__restrict __nptr,
char **__restrict __endptr)
__THROW __nonnull ((1));
#endif
#if __HAVE_FLOAT64X && __GLIBC_USE (IEC_60559_TYPES_EXT)
extern _Float64x strtof64x (const char *__restrict __nptr,
char **__restrict __endptr)
__THROW __nonnull ((1));
#endif
#if __HAVE_FLOAT128X && __GLIBC_USE (IEC_60559_TYPES_EXT)
extern _Float128x strtof128x (const char *__restrict __nptr,
char **__restrict __endptr)
__THROW __nonnull ((1));
#endif
extern long int strtol (const char *__restrict __nptr,
char **__restrict __endptr, int __base)
__THROW __nonnull ((1));
extern unsigned long int strtoul (const char *__restrict __nptr,
char **__restrict __endptr, int __base)
__THROW __nonnull ((1));
#ifdef __USE_MISC
__extension__
extern long long int strtoq (const char *__restrict __nptr,
char **__restrict __endptr, int __base)
__THROW __nonnull ((1));
__extension__
extern unsigned long long int strtouq (const char *__restrict __nptr,
char **__restrict __endptr, int __base)
__THROW __nonnull ((1));
#endif
#ifdef __USE_ISOC99
__extension__
extern long long int strtoll (const char *__restrict __nptr,
char **__restrict __endptr, int __base)
__THROW __nonnull ((1));
__extension__
extern unsigned long long int strtoull (const char *__restrict __nptr,
char **__restrict __endptr, int __base)
__THROW __nonnull ((1));
#endif
#if __GLIBC_USE (IEC_60559_BFP_EXT_C2X)
extern int strfromd (char *__dest, size_t __size, const char *__format,
double __f)
__THROW __nonnull ((3));
extern int strfromf (char *__dest, size_t __size, const char *__format,
float __f)
__THROW __nonnull ((3));
extern int strfroml (char *__dest, size_t __size, const char *__format,
long double __f)
__THROW __nonnull ((3));
#endif
#if __HAVE_FLOAT16 && __GLIBC_USE (IEC_60559_TYPES_EXT)
extern int strfromf16 (char *__dest, size_t __size, const char * __format,
_Float16 __f)
__THROW __nonnull ((3));
#endif
#if __HAVE_FLOAT32 && __GLIBC_USE (IEC_60559_TYPES_EXT)
extern int strfromf32 (char *__dest, size_t __size, const char * __format,
_Float32 __f)
__THROW __nonnull ((3));
#endif
#if __HAVE_FLOAT64 && __GLIBC_USE (IEC_60559_TYPES_EXT)
extern int strfromf64 (char *__dest, size_t __size, const char * __format,
_Float64 __f)
__THROW __nonnull ((3));
#endif
#if __HAVE_FLOAT128 && __GLIBC_USE (IEC_60559_TYPES_EXT)
extern int strfromf128 (char *__dest, size_t __size, const char * __format,
_Float128 __f)
__THROW __nonnull ((3));
#endif
#if __HAVE_FLOAT32X && __GLIBC_USE (IEC_60559_TYPES_EXT)
extern int strfromf32x (char *__dest, size_t __size, const char * __format,
_Float32x __f)
__THROW __nonnull ((3));
#endif
#if __HAVE_FLOAT64X && __GLIBC_USE (IEC_60559_TYPES_EXT)
extern int strfromf64x (char *__dest, size_t __size, const char * __format,
_Float64x __f)
__THROW __nonnull ((3));
#endif
#if __HAVE_FLOAT128X && __GLIBC_USE (IEC_60559_TYPES_EXT)
extern int strfromf128x (char *__dest, size_t __size, const char * __format,
_Float128x __f)
__THROW __nonnull ((3));
#endif
#ifdef __USE_GNU
# include <bits/types/locale_t.h>
extern long int strtol_l (const char *__restrict __nptr,
char **__restrict __endptr, int __base,
locale_t __loc) __THROW __nonnull ((1, 4));
extern unsigned long int strtoul_l (const char *__restrict __nptr,
char **__restrict __endptr,
int __base, locale_t __loc)
__THROW __nonnull ((1, 4));
__extension__
extern long long int strtoll_l (const char *__restrict __nptr,
char **__restrict __endptr, int __base,
locale_t __loc)
__THROW __nonnull ((1, 4));
__extension__
extern unsigned long long int strtoull_l (const char *__restrict __nptr,
char **__restrict __endptr,
int __base, locale_t __loc)
__THROW __nonnull ((1, 4));
extern double strtod_l (const char *__restrict __nptr,
char **__restrict __endptr, locale_t __loc)
__THROW __nonnull ((1, 3));
extern float strtof_l (const char *__restrict __nptr,
char **__restrict __endptr, locale_t __loc)
__THROW __nonnull ((1, 3));
extern long double strtold_l (const char *__restrict __nptr,
char **__restrict __endptr,
locale_t __loc)
__THROW __nonnull ((1, 3));
# if __HAVE_FLOAT16
extern _Float16 strtof16_l (const char *__restrict __nptr,
char **__restrict __endptr,
locale_t __loc)
__THROW __nonnull ((1, 3));
# endif
# if __HAVE_FLOAT32
extern _Float32 strtof32_l (const char *__restrict __nptr,
char **__restrict __endptr,
locale_t __loc)
__THROW __nonnull ((1, 3));
# endif
# if __HAVE_FLOAT64
extern _Float64 strtof64_l (const char *__restrict __nptr,
char **__restrict __endptr,
locale_t __loc)
__THROW __nonnull ((1, 3));
# endif
# if __HAVE_FLOAT128
extern _Float128 strtof128_l (const char *__restrict __nptr,
char **__restrict __endptr,
locale_t __loc)
__THROW __nonnull ((1, 3));
# endif
# if __HAVE_FLOAT32X
extern _Float32x strtof32x_l (const char *__restrict __nptr,
char **__restrict __endptr,
locale_t __loc)
__THROW __nonnull ((1, 3));
# endif
# if __HAVE_FLOAT64X
extern _Float64x strtof64x_l (const char *__restrict __nptr,
char **__restrict __endptr,
locale_t __loc)
__THROW __nonnull ((1, 3));
# endif
# if __HAVE_FLOAT128X
extern _Float128x strtof128x_l (const char *__restrict __nptr,
char **__restrict __endptr,
locale_t __loc)
__THROW __nonnull ((1, 3));
# endif
#endif
#ifdef __USE_EXTERN_INLINES
__extern_inline int
__NTH (atoi (const char *__nptr))
{
return (int) strtol (__nptr, (char **) NULL, 10);
}
__extern_inline long int
__NTH (atol (const char *__nptr))
{
return strtol (__nptr, (char **) NULL, 10);
}
# ifdef __USE_ISOC99
__extension__ __extern_inline long long int
__NTH (atoll (const char *__nptr))
{
return strtoll (__nptr, (char **) NULL, 10);
}
# endif
#endif
#if defined __USE_MISC || defined __USE_XOPEN_EXTENDED
extern char *l64a (long int __n) __THROW __wur;
extern long int a64l (const char *__s)
__THROW __attribute_pure__ __nonnull ((1)) __wur;
#endif
#if defined __USE_MISC || defined __USE_XOPEN_EXTENDED
# include <sys/types.h>
extern long int random (void) __THROW;
extern void srandom (unsigned int __seed) __THROW;
extern char *initstate (unsigned int __seed, char *__statebuf,
size_t __statelen) __THROW __nonnull ((2));
extern char *setstate (char *__statebuf) __THROW __nonnull ((1));
# ifdef __USE_MISC
struct random_data
{
int32_t *fptr;
int32_t *rptr;
int32_t *state;
int rand_type;
int rand_deg;
int rand_sep;
int32_t *end_ptr;
};
extern int random_r (struct random_data *__restrict __buf,
int32_t *__restrict __result) __THROW __nonnull ((1, 2));
extern int srandom_r (unsigned int __seed, struct random_data *__buf)
__THROW __nonnull ((2));
extern int initstate_r (unsigned int __seed, char *__restrict __statebuf,
size_t __statelen,
struct random_data *__restrict __buf)
__THROW __nonnull ((2, 4));
extern int setstate_r (char *__restrict __statebuf,
struct random_data *__restrict __buf)
__THROW __nonnull ((1, 2));
# endif
#endif
extern int rand (void) __THROW;
extern void srand (unsigned int __seed) __THROW;
#ifdef __USE_POSIX199506
extern int rand_r (unsigned int *__seed) __THROW;
#endif
#if defined __USE_MISC || defined __USE_XOPEN
extern double drand48 (void) __THROW;
extern double erand48 (unsigned short int __xsubi[3]) __THROW __nonnull ((1));
extern long int lrand48 (void) __THROW;
extern long int nrand48 (unsigned short int __xsubi[3])
__THROW __nonnull ((1));
extern long int mrand48 (void) __THROW;
extern long int jrand48 (unsigned short int __xsubi[3])
__THROW __nonnull ((1));
extern void srand48 (long int __seedval) __THROW;
extern unsigned short int *seed48 (unsigned short int __seed16v[3])
__THROW __nonnull ((1));
extern void lcong48 (unsigned short int __param[7]) __THROW __nonnull ((1));
# ifdef __USE_MISC
struct drand48_data
{
unsigned short int __x[3];
unsigned short int __old_x[3];
unsigned short int __c;
unsigned short int __init;
__extension__ unsigned long long int __a;
};
extern int drand48_r (struct drand48_data *__restrict __buffer,
double *__restrict __result) __THROW __nonnull ((1, 2));
extern int erand48_r (unsigned short int __xsubi[3],
struct drand48_data *__restrict __buffer,
double *__restrict __result) __THROW __nonnull ((1, 2));
extern int lrand48_r (struct drand48_data *__restrict __buffer,
long int *__restrict __result)
__THROW __nonnull ((1, 2));
extern int nrand48_r (unsigned short int __xsubi[3],
struct drand48_data *__restrict __buffer,
long int *__restrict __result)
__THROW __nonnull ((1, 2));
extern int mrand48_r (struct drand48_data *__restrict __buffer,
long int *__restrict __result)
__THROW __nonnull ((1, 2));
extern int jrand48_r (unsigned short int __xsubi[3],
struct drand48_data *__restrict __buffer,
long int *__restrict __result)
__THROW __nonnull ((1, 2));
extern int srand48_r (long int __seedval, struct drand48_data *__buffer)
__THROW __nonnull ((2));
extern int seed48_r (unsigned short int __seed16v[3],
struct drand48_data *__buffer) __THROW __nonnull ((1, 2));
extern int lcong48_r (unsigned short int __param[7],
struct drand48_data *__buffer)
__THROW __nonnull ((1, 2));
# endif
#endif
extern void *malloc (size_t __size) __THROW __attribute_malloc__
__attribute_alloc_size__ ((1)) __wur;
extern void *calloc (size_t __nmemb, size_t __size)
__THROW __attribute_malloc__ __attribute_alloc_size__ ((1, 2)) __wur;
extern void *realloc (void *__ptr, size_t __size)
__THROW __attribute_warn_unused_result__ __attribute_alloc_size__ ((2));
extern void free (void *__ptr) __THROW;
#ifdef __USE_MISC
extern void *reallocarray (void *__ptr, size_t __nmemb, size_t __size)
__THROW __attribute_warn_unused_result__
__attribute_alloc_size__ ((2, 3))
__attr_dealloc_free;
extern void *reallocarray (void *__ptr, size_t __nmemb, size_t __size)
__THROW __attr_dealloc (reallocarray, 1);
#endif
#ifdef __USE_MISC
# include <alloca.h>
#endif
#if (defined __USE_XOPEN_EXTENDED && !defined __USE_XOPEN2K) \
|| defined __USE_MISC
extern void *valloc (size_t __size) __THROW __attribute_malloc__
__attribute_alloc_size__ ((1)) __wur;
#endif
#ifdef __USE_XOPEN2K
extern int posix_memalign (void **__memptr, size_t __alignment, size_t __size)
__THROW __nonnull ((1)) __wur;
#endif
#ifdef __USE_ISOC11
extern void *aligned_alloc (size_t __alignment, size_t __size)
__THROW __attribute_malloc__ __attribute_alloc_align__ ((1))
__attribute_alloc_size__ ((2)) __wur;
#endif
extern void abort (void) __THROW __attribute__ ((__noreturn__));
extern int atexit (void (*__func) (void)) __THROW __nonnull ((1));
#if defined __USE_ISOC11 || defined __USE_ISOCXX11
# ifdef __cplusplus
extern "C++" int at_quick_exit (void (*__func) (void))
__THROW __asm ("at_quick_exit") __nonnull ((1));
# else
extern int at_quick_exit (void (*__func) (void)) __THROW __nonnull ((1));
# endif
#endif
#ifdef __USE_MISC
extern int on_exit (void (*__func) (int __status, void *__arg), void *__arg)
__THROW __nonnull ((1));
#endif
extern void exit (int __status) __THROW __attribute__ ((__noreturn__));
#if defined __USE_ISOC11 || defined __USE_ISOCXX11
extern void quick_exit (int __status) __THROW __attribute__ ((__noreturn__));
#endif
#ifdef __USE_ISOC99
extern void _Exit (int __status) __THROW __attribute__ ((__noreturn__));
#endif
extern char *getenv (const char *__name) __THROW __nonnull ((1)) __wur;
#ifdef __USE_GNU
extern char *secure_getenv (const char *__name)
__THROW __nonnull ((1)) __wur;
#endif
#if defined __USE_MISC || defined __USE_XOPEN
extern int putenv (char *__string) __THROW __nonnull ((1));
#endif
#ifdef __USE_XOPEN2K
extern int setenv (const char *__name, const char *__value, int __replace)
__THROW __nonnull ((2));
extern int unsetenv (const char *__name) __THROW __nonnull ((1));
#endif
#ifdef __USE_MISC
extern int clearenv (void) __THROW;
#endif
#if defined __USE_MISC \
|| (defined __USE_XOPEN_EXTENDED && !defined __USE_XOPEN2K8)
extern char *mktemp (char *__template) __THROW __nonnull ((1));
#endif
#if defined __USE_XOPEN_EXTENDED || defined __USE_XOPEN2K8
# ifndef __USE_FILE_OFFSET64
extern int mkstemp (char *__template) __nonnull ((1)) __wur;
# else
# ifdef __REDIRECT
extern int __REDIRECT (mkstemp, (char *__template), mkstemp64)
__nonnull ((1)) __wur;
# else
# define mkstemp mkstemp64
# endif
# endif
# ifdef __USE_LARGEFILE64
extern int mkstemp64 (char *__template) __nonnull ((1)) __wur;
# endif
#endif
#ifdef __USE_MISC
# ifndef __USE_FILE_OFFSET64
extern int mkstemps (char *__template, int __suffixlen) __nonnull ((1)) __wur;
# else
# ifdef __REDIRECT
extern int __REDIRECT (mkstemps, (char *__template, int __suffixlen),
mkstemps64) __nonnull ((1)) __wur;
# else
# define mkstemps mkstemps64
# endif
# endif
# ifdef __USE_LARGEFILE64
extern int mkstemps64 (char *__template, int __suffixlen)
__nonnull ((1)) __wur;
# endif
#endif
#ifdef __USE_XOPEN2K8
extern char *mkdtemp (char *__template) __THROW __nonnull ((1)) __wur;
#endif
#ifdef __USE_GNU
# ifndef __USE_FILE_OFFSET64
extern int mkostemp (char *__template, int __flags) __nonnull ((1)) __wur;
# else
# ifdef __REDIRECT
extern int __REDIRECT (mkostemp, (char *__template, int __flags), mkostemp64)
__nonnull ((1)) __wur;
# else
# define mkostemp mkostemp64
# endif
# endif
# ifdef __USE_LARGEFILE64
extern int mkostemp64 (char *__template, int __flags) __nonnull ((1)) __wur;
# endif
# ifndef __USE_FILE_OFFSET64
extern int mkostemps (char *__template, int __suffixlen, int __flags)
__nonnull ((1)) __wur;
# else
# ifdef __REDIRECT
extern int __REDIRECT (mkostemps, (char *__template, int __suffixlen,
int __flags), mkostemps64)
__nonnull ((1)) __wur;
# else
# define mkostemps mkostemps64
# endif
# endif
# ifdef __USE_LARGEFILE64
extern int mkostemps64 (char *__template, int __suffixlen, int __flags)
__nonnull ((1)) __wur;
# endif
#endif
extern int system (const char *__command) __wur;
#ifdef __USE_GNU
extern char *canonicalize_file_name (const char *__name)
__THROW __nonnull ((1)) __attribute_malloc__
__attr_dealloc_free __wur;
#endif
#if defined __USE_MISC || defined __USE_XOPEN_EXTENDED
extern char *realpath (const char *__restrict __name,
char *__restrict __resolved) __THROW __wur;
#endif
#ifndef __COMPAR_FN_T
# define __COMPAR_FN_T
typedef int (*__compar_fn_t) (const void *, const void *);
# ifdef __USE_GNU
typedef __compar_fn_t comparison_fn_t;
# endif
#endif
#ifdef __USE_GNU
typedef int (*__compar_d_fn_t) (const void *, const void *, void *);
#endif
extern void *bsearch (const void *__key, const void *__base,
size_t __nmemb, size_t __size, __compar_fn_t __compar)
__nonnull ((1, 2, 5)) __wur;
#ifdef __USE_EXTERN_INLINES
# include <bits/stdlib-bsearch.h>
#endif
extern void qsort (void *__base, size_t __nmemb, size_t __size,
__compar_fn_t __compar) __nonnull ((1, 4));
#ifdef __USE_GNU
extern void qsort_r (void *__base, size_t __nmemb, size_t __size,
__compar_d_fn_t __compar, void *__arg)
__nonnull ((1, 4));
#endif
extern int abs (int __x) __THROW __attribute__ ((__const__)) __wur;
extern long int labs (long int __x) __THROW __attribute__ ((__const__)) __wur;
#ifdef __USE_ISOC99
__extension__ extern long long int llabs (long long int __x)
__THROW __attribute__ ((__const__)) __wur;
#endif
extern div_t div (int __numer, int __denom)
__THROW __attribute__ ((__const__)) __wur;
extern ldiv_t ldiv (long int __numer, long int __denom)
__THROW __attribute__ ((__const__)) __wur;
#ifdef __USE_ISOC99
__extension__ extern lldiv_t lldiv (long long int __numer,
long long int __denom)
__THROW __attribute__ ((__const__)) __wur;
#endif
#if (defined __USE_XOPEN_EXTENDED && !defined __USE_XOPEN2K8) \
|| defined __USE_MISC
extern char *ecvt (double __value, int __ndigit, int *__restrict __decpt,
int *__restrict __sign) __THROW __nonnull ((3, 4)) __wur;
extern char *fcvt (double __value, int __ndigit, int *__restrict __decpt,
int *__restrict __sign) __THROW __nonnull ((3, 4)) __wur;
extern char *gcvt (double __value, int __ndigit, char *__buf)
__THROW __nonnull ((3)) __wur;
#endif
#ifdef __USE_MISC
extern char *qecvt (long double __value, int __ndigit,
int *__restrict __decpt, int *__restrict __sign)
__THROW __nonnull ((3, 4)) __wur;
extern char *qfcvt (long double __value, int __ndigit,
int *__restrict __decpt, int *__restrict __sign)
__THROW __nonnull ((3, 4)) __wur;
extern char *qgcvt (long double __value, int __ndigit, char *__buf)
__THROW __nonnull ((3)) __wur;
extern int ecvt_r (double __value, int __ndigit, int *__restrict __decpt,
int *__restrict __sign, char *__restrict __buf,
size_t __len) __THROW __nonnull ((3, 4, 5));
extern int fcvt_r (double __value, int __ndigit, int *__restrict __decpt,
int *__restrict __sign, char *__restrict __buf,
size_t __len) __THROW __nonnull ((3, 4, 5));
extern int qecvt_r (long double __value, int __ndigit,
int *__restrict __decpt, int *__restrict __sign,
char *__restrict __buf, size_t __len)
__THROW __nonnull ((3, 4, 5));
extern int qfcvt_r (long double __value, int __ndigit,
int *__restrict __decpt, int *__restrict __sign,
char *__restrict __buf, size_t __len)
__THROW __nonnull ((3, 4, 5));
#endif
extern int mblen (const char *__s, size_t __n) __THROW;
extern int mbtowc (wchar_t *__restrict __pwc,
const char *__restrict __s, size_t __n) __THROW;
extern int wctomb (char *__s, wchar_t __wchar) __THROW;
extern size_t mbstowcs (wchar_t *__restrict __pwcs,
const char *__restrict __s, size_t __n) __THROW
__attr_access ((__read_only__, 2));
extern size_t wcstombs (char *__restrict __s,
const wchar_t *__restrict __pwcs, size_t __n)
__THROW
__fortified_attr_access (__write_only__, 1, 3)
__attr_access ((__read_only__, 2));
#ifdef __USE_MISC
extern int rpmatch (const char *__response) __THROW __nonnull ((1)) __wur;
#endif
#if defined __USE_XOPEN_EXTENDED || defined __USE_XOPEN2K8
extern int getsubopt (char **__restrict __optionp,
char *const *__restrict __tokens,
char **__restrict __valuep)
__THROW __nonnull ((1, 2, 3)) __wur;
#endif
#ifdef __USE_XOPEN2KXSI
extern int posix_openpt (int __oflag) __wur;
#endif
#ifdef __USE_XOPEN_EXTENDED
extern int grantpt (int __fd) __THROW;
extern int unlockpt (int __fd) __THROW;
extern char *ptsname (int __fd) __THROW __wur;
#endif
#ifdef __USE_GNU
extern int ptsname_r (int __fd, char *__buf, size_t __buflen)
__THROW __nonnull ((2)) __fortified_attr_access (__write_only__, 2, 3);
extern int getpt (void);
#endif
#ifdef __USE_MISC
extern int getloadavg (double __loadavg[], int __nelem)
__THROW __nonnull ((1));
#endif
#if defined __USE_XOPEN_EXTENDED && !defined __USE_XOPEN2K
extern int ttyslot (void) __THROW;
#endif
#include <bits/stdlib-float.h>
#if __USE_FORTIFY_LEVEL > 0 && defined __fortify_function
# include <bits/stdlib.h>
#endif
#include <bits/floatn.h>
#if defined __LDBL_COMPAT || __LDOUBLE_REDIRECTS_TO_FLOAT128_ABI == 1
# include <bits/stdlib-ldbl.h>
#endif
__END_DECLS
#endif