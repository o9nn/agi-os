#ifndef gsbitops_INCLUDED
# define gsbitops_INCLUDED
#define sample_end_\
default: return_error(gs_error_rangecheck);\
} END
#define sample_load_declare(sptr, sbit)\
const byte *sptr;\
int sbit
#define sample_load_declare_setup(sptr, sbit, ptr, bitno, sbpv)\
const byte *sptr = (ptr);\
int sample_load_setup(sbit, bitno, sbpv)
#define sample_load_setup(sbit, bitno, sbpv)\
sbit = (bitno)
#define sample_bound_shift(value, shift)\
((shift) >= 8 * sizeof(value) ? (shift) & (8 * sizeof(value) - 1) : (shift))
#define sample_load8_(value, sptr, sbit, sbpv)\
BEGIN\
switch ( (sbpv) >> 2 ) {\
case 0: value = (*(sptr) >> (8 - (sbit) - (sbpv))) & ((sbpv) | 1); break;\
case 1: value = (*(sptr) >> (4 - (sbit))) & 0xf; break;\
case 2: value = *(sptr); break;
#define sample_load8(value, sptr, sbit, sbpv)\
sample_load8_(value, sptr, sbit, sbpv)\
sample_end_
#define sample_load_next8(value, sptr, sbit, sbpv)\
sample_load8(value, sptr, sbit, sbpv);\
sample_next(sptr, sbit, sbpv)
#define sample_load12_(value, sptr, sbit, sbpv)\
sample_load8_(value, sptr, sbit, sbpv)\
case 3:\
value = ((sbit) ? ((*(sptr) & 0xf) << 8) | (sptr)[1] :\
(*(sptr) << 4) | ((sptr)[1] >> 4));\
break;
#define sample_load12(value, sptr, sbit, sbpv)\
sample_load12_(value, sptr, sbit, sbpv)\
sample_end_
#define sample_load_next12(value, sptr, sbit, sbpv)\
sample_load12(value, sptr, sbit, sbpv);\
sample_next(sptr, sbit, sbpv)
#define sample_load16_(value, sptr, sbit, sbpv)\
sample_load12_(value, sptr, sbit, sbpv)\
case 4: value = (*(sptr) << 8) | (sptr)[1]; break;
#define sample_load16(value, sptr, sbit, sbpv)\
sample_load16_(value, sptr, sbit, sbpv)\
sample_end_
#define sample_load_next16(value, sptr, sbit, sbpv)\
sample_load16(value, sptr, sbit, sbpv);\
sample_next(sptr, sbit, sbpv)
#define sample_load32_(value, sptr, sbit, sbpv)\
sample_load16_(value, sptr, sbit, sbpv)\
case 6: value = (*(sptr) << 16) | ((sptr)[1] << 8) | (sptr)[2]; break;\
case 8:\
value = (*(sptr) << 24) | ((sptr)[1] << 16) | ((sptr)[2] << 8) | sptr[3];\
break;
#define sample_load32(value, sptr, sbit, sbpv)\
sample_load32_(value, sptr, sbit, sbpv);\
sample_end_
#define sample_load_next32(value, sptr, sbit, sbpv)\
sample_load32(value, sptr, sbit, sbpv);\
sample_next(sptr, sbit, sbpv)
#define sample_load64_(value, sptr, sbit, sbpv)\
sample_load32_(value, sptr, sbit, sbpv);\
case 10:\
value = ((gx_color_index)((sptr)[0]) << sample_bound_shift((value), 32)) |\
((gx_color_index)((sptr)[1]) << 24) |\
((gx_color_index)((sptr)[2]) << 16) |\
((gx_color_index)((sptr)[3]) << 8) |\
(gx_color_index)((sptr)[4]);\
break;\
case 12:\
value = ((gx_color_index)((sptr)[0]) << sample_bound_shift((value), 40)) |\
((gx_color_index)((sptr)[1]) << sample_bound_shift((value), 32)) |\
((gx_color_index)((sptr)[2]) << 24) |\
((gx_color_index)((sptr)[3]) << 16) |\
((gx_color_index)((sptr)[4]) << 8) |\
(gx_color_index)((sptr)[5]);\
break;\
case 14:\
value = ((gx_color_index)((sptr)[0]) << sample_bound_shift((value), 48)) |\
((gx_color_index)((sptr)[1]) << sample_bound_shift((value), 40)) |\
((gx_color_index)((sptr)[2]) << sample_bound_shift((value), 32)) |\
((gx_color_index)((sptr)[3]) << 24) |\
((gx_color_index)((sptr)[4]) << 16) |\
((gx_color_index)((sptr)[5]) << 8) |\
(gx_color_index)((sptr)[6]);\
break;\
case 16:\
value = ((gx_color_index)((sptr)[0]) << sample_bound_shift((value), 56)) |\
((gx_color_index)((sptr)[1]) << sample_bound_shift((value), 48)) |\
((gx_color_index)((sptr)[2]) << sample_bound_shift((value), 40)) |\
((gx_color_index)((sptr)[3]) << sample_bound_shift((value), 32)) |\
((gx_color_index)((sptr)[4]) << 24) |\
((gx_color_index)((sptr)[5]) << 16) |\
((gx_color_index)((sptr)[6]) << 8) |\
(gx_color_index)((sptr)[7]);\
break;
#define sample_load64(value, sptr, sbit, sbpv)\
sample_load64_(value, sptr, sbit, sbpv);\
sample_end_
#define sample_load_next64(value, sptr, sbit, sbpv)\
sample_load64(value, sptr, sbit, sbpv);\
sample_next(sptr, sbit, sbpv)
#define sample_load_any(value, sptr, sbit, sbpv)\
if (sizeof(value) > 4)\
sample_load64(value, sptr, sbit, sbpv);\
else\
sample_load32(value, sptr, sbit, sbpv)
#define sample_load_next_any(value, sptr, sbit, sbpv)\
sample_load_any(value, sptr, sbit, sbpv);\
sample_next(sptr, sbit, sbpv)
#define sample_store_declare(dptr, dbit, dbbyte)\
byte *dptr;\
int dbit;\
byte dbbyte
#define sample_store_declare_setup(dptr, dbit, dbbyte, ptr, bitno, dbpv)\
byte *dptr = (ptr);\
int sample_store_setup(dbit, bitno, dbpv);\
byte \
sample_store_preload(dbbyte, dptr, dbit, dbpv)
#define sample_store_setup(dbit, bitno, dbpv)\
dbit = (bitno)
#define sample_store_preload(dbbyte, dptr, dbit, dbpv)\
dbbyte = ((dbit) ? (byte)(*(dptr) & (0xff00 >> (dbit))) : 0)
#define sample_store_next8_(value, dptr, dbit, dbpv, dbbyte)\
BEGIN\
switch ( (dbpv) >> 2 ) {\
case 0:\
if ( (dbit += (dbpv)) == 8 )\
*(dptr)++ = dbbyte | (byte)(value), dbbyte = 0, dbit = 0;\
else dbbyte |= (byte)((value) << (8 - dbit));\
break;\
case 1:\
if ( dbit ^= 4 ) dbbyte = (byte)((value) << 4);\
else *(dptr)++ = dbbyte | ((byte)(value));\
break;\
#define sample_store_next8(value, dptr, dbit, dbpv, dbbyte)\
sample_store_next8_(value, dptr, dbit, dbpv, dbbyte)\
case 2: *(dptr)++ = (byte)(value); break;\
sample_end_
#define sample_store_next_12_(value, dptr, dbit, dbbyte)\
if ( dbit ^= 4 ) *(dptr)++ = (byte)((value) >> 4), dbbyte = (byte)((value) << 4);\
else\
*(dptr) = dbbyte | (byte)((value) >> 8), (dptr)[1] = (byte)(value), dptr += 2;
#define sample_store_next_12(value, dptr, dbit, dbbyte)\
BEGIN sample_store_next_12_(value, dptr, dbit, dbbyte) END
#define sample_store_next12_(value, dptr, dbit, dbpv, dbbyte)\
sample_store_next8_(value, dptr, dbit, dbpv, dbbyte)\
\
case 3: sample_store_next_12_(value, dptr, dbit, dbbyte) break;
#define sample_store_next12(value, dptr, dbit, dbpv, dbbyte)\
sample_store_next12_(value, dptr, dbit, dbpv, dbbyte)\
case 2: *(dptr)++ = (byte)(value); break;\
sample_end_
#define sample_store_next16(value, dptr, dbit, dbpv, dbbyte)\
sample_store_next12_(value, dptr, dbit, dbpv, dbbyte)\
case 4: *(dptr)++ = (byte)((value) >> 8);\
case 2: *(dptr)++ = (byte)(value); break;\
sample_end_
#define sample_store_next32(value, dptr, dbit, dbpv, dbbyte)\
sample_store_next12_(value, dptr, dbit, dbpv, dbbyte)\
case 8: *(dptr)++ = (byte)((value) >> 24);\
case 6: *(dptr)++ = (byte)((value) >> 16);\
case 4: *(dptr)++ = (byte)((value) >> 8);\
case 2: *(dptr)++ = (byte)(value); break;\
sample_end_
#define sample_store_next64(value, dptr, dbit, dbpv, dbbyte)\
sample_store_next12_(value, dptr, dbit, dbpv, dbbyte)\
case 16: *(dptr)++ = (byte)((value) >> sample_bound_shift((value), 56));\
case 14: *(dptr)++ = (byte)((value) >> sample_bound_shift((value), 48));\
case 12: *(dptr)++ = (byte)((value) >> sample_bound_shift((value), 40));\
case 10: *(dptr)++ = (byte)((value) >> sample_bound_shift((value), 32));\
case 8: *(dptr)++ = (byte)((value) >> 24);\
case 6: *(dptr)++ = (byte)((value) >> 16);\
case 4: *(dptr)++ = (byte)((value) >> 8);\
case 2: *(dptr)++ = (byte)(value); break;\
sample_end_
#define sample_store_next_any(value, dptr, dbit, dbpv, dbbyte)\
if (sizeof(value) > 4)\
sample_store_next64(value, dptr, dbit, dbpv, dbbyte);\
else\
sample_store_next32(value, dptr, dbit, dbpv, dbbyte)
#define sample_store_skip_next(dptr, dbit, dbpv, dbbyte)\
if ( (dbpv) < 8 ) {\
sample_store_flush(dptr, dbit, dbpv, dbbyte);\
sample_next(dptr, dbit, dbpv);\
} else dptr += ((dbpv) >> 3)
#define sample_store_flush(dptr, dbit, dbpv, dbbyte)\
if ( (dbit) != 0 )\
*(dptr) = dbbyte | (*(dptr) & (0xff >> (dbit)));
#define sample_next(ptr, bit, bpv)\
BEGIN bit += (bpv); ptr += bit >> 3; bit &= 7; END
#define mono_fill_chunk uint
#define mono_fill_chunk_bytes arch_sizeof_int
#if mono_fill_chunk_bytes == 2
# define mono_fill_make_pattern(byt) (uint)((uint)(byt) * 0x0101)
#else
# define mono_fill_make_pattern(byt) (uint)((uint)(byt) * 0x01010101)
#endif
void bits_fill_rectangle(byte * dest, int dest_bit, uint raster,
mono_fill_chunk pattern, int width_bits, int height);
void bits_fill_rectangle_masked(byte * dest, int dest_bit, uint raster,
mono_fill_chunk pattern, mono_fill_chunk src_mask,
int width_bits, int height);
void bits_replicate_horizontally(byte * data, uint width, uint height,
uint raster, uint replicated_width, uint replicated_raster);
void bits_replicate_vertically(byte * data, uint height, uint raster,
uint replicated_height);
void bits_bounding_box(const byte * data, uint height, uint raster,
gs_int_rect * pbox);
void bits_compress_scaled(const byte * src, int srcx, uint width,
uint height, uint sraster, byte * dest, uint draster,
const gs_log2_scale_point * plog2_scale, int log2_out_bits);
typedef struct bits_plane_s {
union bpd_ {
byte *write;
const byte *read;
} data;
int raster;
int depth;
int x;
} bits_plane_t;
int bits_extract_plane(const bits_plane_t *dest ,
const bits_plane_t *source , int shift, int width, int height);
int bits_expand_plane(const bits_plane_t *dest ,
const bits_plane_t *source , int shift, int width, int height);
void bytes_fill_rectangle(byte * dest, uint raster,
byte value, int width_bytes, int height);
void bytes_copy_rectangle(byte * dest, uint dest_raster,
const byte * src, uint src_raster, int width_bytes, int height);
#endif