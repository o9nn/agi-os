#ifndef gxbitops_INCLUDED
#  define gxbitops_INCLUDED
#include "gsbitops.h"
#define cbytes(ct) size_of(ct)
#  define chunk_bytes cbytes(chunk)
#define clog2_bytes(ct) (size_of(ct) == 8 ? 3 : size_of(ct)>>1)
#  define chunk_log2_bytes clog2_bytes(chunk)
#define cbits(ct) (size_of(ct)*8)
#  define chunk_bits cbits(chunk)
#define clog2_bits(ct) (clog2_bytes(ct)+3)
#  define chunk_log2_bits clog2_bits(chunk)
#define cbit_mask(ct) (cbits(ct)-1)
#  define chunk_bit_mask cbit_mask(chunk)
#define calign_bytes(ct)\
(sizeof(ct) == 1 ? 1:\
sizeof(ct) == sizeof(short) ? arch_align_short_mod :\
sizeof(ct) == sizeof(int) ? arch_align_int_mod : arch_align_long_mod)
#  define chunk_align_bytes calign_bytes(chunk)
#define calign_bit_mask(ct) (calign_bytes(ct)*8-1)
#  define chunk_align_bit_mask calign_bit_mask(chunk)
#define cmask(ct) ((ct) (((((ct)1 << (size_of(ct)*8-2)) - 1) << 2) + 3))
#  define chunk_all_bits cmask(chunk)
#define chi_bits(ct,n) (ct)(~(ct)1 << (cbits(ct)-1 - (n)))
#  define chunk_hi_bits(n) chi_bits(chunk,n)
#define arch_cant_shift_full_chunk\
(arch_is_big_endian && !arch_ints_are_short && !arch_can_shift_full_long)
#define inc_ptr(ptr,delta)\
(ptr = (void *)((byte *)ptr + (delta)))
#if arch_is_big_endian
#  define mono_copy_chunk uint
#  define set_mono_right_mask(var, w)\
(var = ((w) == chunk_bits ? chunk_all_bits : chunk_hi_bits(w)))
#  define set_mono_thin_mask(var, w, bit)\
set_mono_right_mask(var, w), var >>= (bit)
#  define set_mono_left_mask(var, bit)\
(var = chunk_all_bits, var >>= (bit))
#else
#  define mono_copy_chunk bits16
extern const bits16 mono_copy_masks[17];
#  if mono_fill_chunk_bytes == 2
#    define mono_fill_masks mono_copy_masks
#  else
extern const bits32 mono_fill_masks[33];
#  endif
#  define set_mono_left_mask(var, bit)\
(var = mono_masks[bit])
#  define set_mono_thin_mask(var, w, bit)\
(var = ~mono_masks[(w) + (bit)] & mono_masks[bit])
#  define set_mono_right_mask(var, ebit)\
(var = ~mono_masks[ebit])
#endif
#endif