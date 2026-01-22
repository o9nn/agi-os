#ifndef gxfrac_INCLUDED
# define gxfrac_INCLUDED
typedef short frac;
typedef short signed_frac;
#define arch_log2_sizeof_frac arch_log2_sizeof_short
#define arch_sizeof_frac arch_sizeof_short
#define frac_bits 15
#define frac_0 ((frac)0)
#define frac_1_0bits 3
#define frac_1 ((frac)0x7ff8)
#define frac_1_long ((long)frac_1)
#define frac_1_float ((float)frac_1)
#define frac2float(fr) ((fr) / frac_1_float)
#define float2frac(fl) ((frac)(((fl) + 0.5 / frac_1_float) * frac_1_float))
#define _frac2s(fr)\
(((fr) >> (frac_bits - frac_1_0bits)) + (fr))
#define frac2bits(fr, nb)\
((uint)(_frac2s(fr) >> (frac_bits - (nb))))
#define frac2byte(fr) ((byte)frac2bits(fr, 8))
#define bits2frac(v, nb) ((frac)(\
((frac)(v) << (frac_bits - (nb))) +\
((v) >> ((nb) * 2 - frac_bits)) -\
((v) >> ((nb) - frac_1_0bits)) ))
#define byte2frac(b) bits2frac(b, 8)
#define frac2bits_floor(fr, nb)\
((uint)((_frac2s(fr) - (_frac2s(fr) >> (nb))) >> (frac_bits - (nb))))
#define ushort_bits (arch_sizeof_short * 8)
#define frac2ushort(fr) ((ushort)(\
((fr) << (ushort_bits - frac_bits)) +\
((fr) >> (frac_bits * 2 - ushort_bits - frac_1_0bits)) ))
#define ushort2frac(us) ((frac)(\
((us) >> (ushort_bits - frac_bits)) -\
((us) >> (ushort_bits - frac_1_0bits)) ))
#define frac_1_quo(p)\
( (((p) >> frac_1_0bits) + ((p) >> frac_bits) + 1) >> (frac_bits - frac_1_0bits) )
#define frac_1_rem(p, q)\
((frac)( (uint)(p) - ((q) << frac_bits) + ((q) << frac_1_0bits) ))
#endif