#ifndef shc_INCLUDED
#  define shc_INCLUDED
#include "gsbittab.h"
#include "scommon.h"
#define max_hc_length 16
typedef struct hc_definition_s {
ushort *counts;
uint num_counts;
ushort *values;
uint num_values;
} hc_definition;
#define stream_hc_state_common\
stream_state_common;\
\
bool FirstBitLowOrder;\
\
uint bits;		\
\
int bits_left		\
\
typedef struct stream_hc_state_s {
stream_hc_state_common;
} stream_hc_state;
#define hc_bits_size (arch_sizeof_int * 8)
#define s_hce_init_inline(ss)\
((ss)->bits = 0, (ss)->bits_left = hc_bits_size)
#define s_hcd_init_inline(ss)\
((ss)->bits = 0, (ss)->bits_left = 0)
typedef struct hce_code_s {
ushort code;
ushort code_length;
} hce_code;
#define hce_entry(c, len) { c, len }
typedef struct hce_table_s {
uint count;
hce_code *codes;
} hce_table;
#define hce_bits_available(n)\
(ss->bits_left >= (n) || wlimit - q > ((n) - ss->bits_left - 1) >> 3)
#ifdef DEBUG
#  define hc_print_value(code, clen)\
(gs_debug_c('W') ?\
(dlprintf2("[W]0x%x,%d\n", code, clen), 0) : 0)
#  define hc_print_value_then(code, clen) hc_print_value(code, clen),
#else
#  define hc_print_value(code, clen) 0
#  define hc_print_value_then(code, clen)
#endif
#define hc_print_code(rp) hc_print_value((rp)->code, (rp)->code_length)
#define hce_declare_state\
register uint bits;\
register int bits_left
#define hce_load_state()\
bits = ss->bits, bits_left = ss->bits_left
#define hce_store_state()\
ss->bits = bits, ss->bits_left = bits_left
void hc_put_code_proc(bool, byte *, uint);
#define hc_put_value(ss, q, code, clen)\
(hc_print_value_then(code, clen)\
((bits_left -= (clen)) >= 0 ?\
(bits += (code) << bits_left) :\
(hc_put_code_proc((ss)->FirstBitLowOrder,\
q += hc_bits_size >> 3,\
(bits + ((code) >> -bits_left))),\
bits = (code) << (bits_left += hc_bits_size))))
#define hc_put_code(ss, q, cp)\
hc_put_value(ss, q, (cp)->code, (cp)->code_length)
byte *hc_put_last_bits_proc(stream_hc_state *, byte *, uint, int);
#define hc_put_last_bits(ss, q)\
hc_put_last_bits_proc(ss, q, bits, bits_left)
typedef struct hcd_code_s {
short value;
ushort code_length;
} hcd_code;
typedef struct hcd_table_s {
uint count;
uint initial_bits;
hcd_code *codes;
} hcd_table;
#define hcd_declare_state\
register const byte *p;\
const byte *rlimit;\
uint bits;\
int bits_left
#define hcd_load_state()\
p = pr->ptr,\
rlimit = pr->limit,\
bits = ss->bits,\
bits_left = ss->bits_left
#define hcd_store_state()\
pr->ptr = p -= (bits_left >> 3),\
ss->bits = bits >>= (bits_left & ~7),\
ss->bits_left = bits_left &= 7
#define hcd_bits_available(n)\
(bits_left >= (n) || rlimit - p > ((n) - bits_left - 1) >> 3)
#define HCD_ENSURE_BITS_ELSE(n)\
if (bits_left >= n)\
DO_NOTHING;\
else HCD_MORE_BITS_ELSE
#define hcd_ensure_bits(n, outl)\
BEGIN HCD_ENSURE_BITS_ELSE(n) goto outl; END
#define HCD_MORE_BITS_1_ELSE\
if (p < rlimit) {\
int c = *++p;\
\
if (ss->FirstBitLowOrder)\
c = byte_reverse_bits[c];\
bits = (bits << 8) + c, bits_left += 8;\
} else
#if hc_bits_size == 16
#  define HCD_MORE_BITS_ELSE HCD_MORE_BITS_1_ELSE
#else
#  define HCD_MORE_BITS_ELSE\
if (rlimit - p >= 3) {\
if (ss->FirstBitLowOrder)\
bits = (bits << 24) + ((uint)byte_reverse_bits[p[1]] << 16) + ((uint)byte_reverse_bits[p[2]] << 8) + byte_reverse_bits[p[3]];\
else\
bits = (bits << 24) + ((uint)p[1] << 16) + ((uint)p[2] << 8) + p[3];\
bits_left += 24, p += 3;\
} else HCD_MORE_BITS_1_ELSE
#endif
#define hcd_more_bits(outl)\
BEGIN HCD_MORE_BITS_ELSE goto outl; END
#define hcd_peek_bits(n) ((bits >> (bits_left - (n))) & ((1 << (n)) - 1))
#define hcd_peek_var_bits(n)\
((bits >> (bits_left - (n))) & byte_right_mask[n])
#define hcd_peek_bits_left()\
(bits & byte_right_mask[bits_left])
#define hcd_skip_bits(n) (bits_left -= (n))
#endif