#ifndef scfx_INCLUDED
#  define scfx_INCLUDED
#include "shc.h"
#define stream_CF_state_common\
stream_hc_state_common;\
\
bool Uncompressed;\
int K;\
bool EndOfLine;\
bool EncodedByteAlign;\
int Columns;\
int Rows;\
bool EndOfBlock;\
bool BlackIs1;\
int DamagedRowsBeforeError;	\
\
int DecodedByteAlign;\
\
uint raster;\
byte *lbuf;		\
\
byte *lprev;		\
\
int k_left		\
typedef struct stream_CF_state_s {
stream_CF_state_common;
} stream_CF_state;
#define s_CF_set_defaults_inline(ss)\
((ss)->Uncompressed = false,\
(ss)->K = 0,\
(ss)->EndOfLine = false,\
(ss)->EncodedByteAlign = false,\
(ss)->Columns = 1728,\
(ss)->Rows = 0,\
(ss)->EndOfBlock = true,\
(ss)->BlackIs1 = false,\
\
(ss)->DamagedRowsBeforeError = 0, \
(ss)->FirstBitLowOrder = false,\
\
(ss)->DecodedByteAlign = 1,\
\
(ss)->lbuf = 0, (ss)->lprev = 0)
typedef struct stream_CFE_state_s {
stream_CF_state_common;
int max_code_bytes;
byte *lcode;
int read_count;
int write_count;
int code_bytes;
} stream_CFE_state;
#define private_st_CFE_state()	\
gs_private_st_ptrs3(st_CFE_state, stream_CFE_state, "CCITTFaxEncode state",\
cfe_enum_ptrs, cfe_reloc_ptrs, lbuf, lprev, lcode)
#define s_CFE_set_defaults_inline(ss)\
(s_CF_set_defaults_inline(ss), (ss)->lcode = 0)
extern const stream_template s_CFE_template;
typedef struct stream_CFD_state_s {
stream_CF_state_common;
int cbit;
int rows_left;
int row;
int rpos;
int wpos;
int eol_count;
byte invert;
int run_color;
int damaged_rows;
bool skipping_damage;
int uncomp_run;
int uncomp_left;
int uncomp_exit;
} stream_CFD_state;
#define private_st_CFD_state()	\
gs_private_st_ptrs2(st_CFD_state, stream_CFD_state, "CCITTFaxDecode state",\
cfd_enum_ptrs, cfd_reloc_ptrs, lbuf, lprev)
#define s_CFD_set_defaults_inline(ss)\
s_CF_set_defaults_inline(ss)
extern const stream_template s_CFD_template;
#endif