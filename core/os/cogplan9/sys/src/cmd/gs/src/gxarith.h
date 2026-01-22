#ifndef gxarith_INCLUDED
#  define gxarith_INCLUDED
#define any_abs(x) ((x) < 0 ? -(x) : (x))
int imod(int m, int n);
int igcd(int x, int y);
int idivmod(int a, int b, int m);
int ilog2(int n);
#define fits_in_bits(i, n)\
(sizeof(i) <= sizeof(int) ? fits_in_ubits((i) + (1 << ((n) - 1)), (n) + 1) :\
fits_in_ubits((i) + (1L << ((n) - 1)), (n) + 1))
#define fits_in_ubits(i, n) (((i) >> (n)) == 0)
#define is_fzero(f) ((f) == 0.0)
#define is_fzero2(f1,f2) ((f1) == 0.0 && (f2) == 0.0)
#define is_fneg(f) ((f) < 0.0)
#define is_fge1(f) ((f) >= 1.0)
#define f_fits_in_bits(f, n)\
((f) >= -2.0 * (1L << ((n) - 2)) && (f) < 2.0 * (1L << ((n) - 2)))
#define f_fits_in_ubits(f, n)\
((f) >= 0 && (f) < 4.0 * (1L << ((n) - 2)))
#define small_exact_log2(n)\
((uint)(05637042010L >> ((((n) % 11) - 1) * 3)) & 7)
#endif