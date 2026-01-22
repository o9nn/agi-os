#ifndef scf_INCLUDED
#  define scf_INCLUDED
#include "shc.h"
#if arch_sizeof_int > 2
#  define cfe_max_width (2560 * 32000 * 2 / 3)
#else
#  define cfe_max_width (max_int - 40)
#endif
#define cfe_max_code_bytes(width) ((width) / 2560 * 3 / 2 + 5)
typedef hce_code cfe_run;
#define run_eol_code_length 12
#define run_eol_code_value 1
extern const cfe_run cf_run_eol;
typedef struct cf_runs_s {
cfe_run termination[64];
cfe_run make_up[41];
} cf_runs;
extern const cf_runs
cf_white_runs, cf_black_runs;
extern const cfe_run cf_uncompressed[6];
extern const cfe_run cf_uncompressed_exit[10];
extern const cfe_run cf1_run_uncompressed;
extern const cfe_run cf2_run_pass;
#define cf2_run_pass_length 4
#define cf2_run_pass_value 0x1
#define cf2_run_vertical_offset 3
extern const cfe_run cf2_run_vertical[7];
extern const cfe_run cf2_run_horizontal;
#define cf2_run_horizontal_value 1
#define cf2_run_horizontal_length 3
extern const cfe_run cf2_run_uncompressed;
extern const cfe_run cf2_run_eol_1d;
extern const cfe_run cf2_run_eol_2d;
typedef hcd_code cfd_node;
#define run_length value
#define run_error (-1)
#define run_zeros (-2)
#define run_uncompressed (-3)
#define run2_pass (-4)
#define run2_horizontal (-5)
#define cfd_white_initial_bits 8
#define cfd_white_min_bits 4
extern const cfd_node cf_white_decode[];
#define cfd_black_initial_bits 7
#define cfd_black_min_bits 2
extern const cfd_node cf_black_decode[];
#define cfd_2d_initial_bits 7
#define cfd_2d_min_bits 4
extern const cfd_node cf_2d_decode[];
#define cfd_uncompressed_initial_bits 6
extern const cfd_node cf_uncompressed_decode[];
#define cf_byte_run_length byte_bit_run_length_neg
#define cf_byte_run_length_0 byte_bit_run_length_0
#define skip_white_pixels(data, p, count, white_byte, rlen)\
BEGIN\
rlen = cf_byte_run_length[count & 7][data ^ 0xff];\
if ( rlen >= 8 ) {		\
if ( white_byte == 0 ) {\
if ( p[0] ) { data = p[0]; p += 1; rlen -= 8; }\
else if ( p[1] ) { data = p[1]; p += 2; }\
else {\
while ( !(p[2] | p[3] | p[4] | p[5]) )\
p += 4, rlen += 32;\
if ( p[2] ) {\
data = p[2]; p += 3; rlen += 8;\
} else if ( p[3] ) {\
data = p[3]; p += 4; rlen += 16;\
} else if ( p[4] ) {\
data = p[4]; p += 5; rlen += 24;\
} else  {\
data = p[5]; p += 6; rlen += 32;\
}\
}\
} else {\
if ( p[0] != 0xff ) { data = (byte)~p[0]; p += 1; rlen -= 8; }\
else if ( p[1] != 0xff ) { data = (byte)~p[1]; p += 2; }\
else {\
while ( (p[2] & p[3] & p[4] & p[5]) == 0xff )\
p += 4, rlen += 32;\
if ( p[2] != 0xff ) {\
data = (byte)~p[2]; p += 3; rlen += 8;\
} else if ( p[3] != 0xff ) {\
data = (byte)~p[3]; p += 4; rlen += 16;\
} else if ( p[4] != 0xff ) {\
data = (byte)~p[4]; p += 5; rlen += 24;\
} else  {\
data = (byte)~p[5]; p += 6; rlen += 32;\
}\
}\
}\
rlen += cf_byte_run_length_0[data ^ 0xff];\
}\
count -= rlen;\
END
#define skip_black_pixels(data, p, count, white_byte, rlen)\
BEGIN\
rlen = cf_byte_run_length[count & 7][data];\
if ( rlen >= 8 ) {\
if ( white_byte == 0 )\
for ( ; ; p += 4, rlen += 32 ) {\
if ( p[0] != 0xff ) { data = p[0]; p += 1; rlen -= 8; break; }\
if ( p[1] != 0xff ) { data = p[1]; p += 2; break; }\
if ( p[2] != 0xff ) { data = p[2]; p += 3; rlen += 8; break; }\
if ( p[3] != 0xff ) { data = p[3]; p += 4; rlen += 16; break; }\
}\
else\
for ( ; ; p += 4, rlen += 32 ) {\
if ( p[0] ) { data = (byte)~p[0]; p += 1; rlen -= 8; break; }\
if ( p[1] ) { data = (byte)~p[1]; p += 2; break; }\
if ( p[2] ) { data = (byte)~p[2]; p += 3; rlen += 8; break; }\
if ( p[3] ) { data = (byte)~p[3]; p += 4; rlen += 16; break; }\
}\
rlen += cf_byte_run_length_0[data];\
}\
count -= rlen;\
END
#endif