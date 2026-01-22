#ifndef spdiffx_INCLUDED
#  define spdiffx_INCLUDED
#define s_PDiff_max_Colors 16
typedef struct stream_PDiff_state_s {
stream_state_common;
int Colors;
int BitsPerComponent;
int Columns;
uint row_count;
byte end_mask;
int case_index;
uint row_left;
uint prev[s_PDiff_max_Colors];
} stream_PDiff_state;
#define private_st_PDiff_state()	\
gs_private_st_simple(st_PDiff_state, stream_PDiff_state,\
"PixelDifferenceEncode/Decode state")
#define s_PDiff_set_defaults_inline(ss)\
((ss)->Colors = 1, (ss)->BitsPerComponent = 8, (ss)->Columns = 1)
extern const stream_template s_PDiffD_template;
extern const stream_template s_PDiffE_template;
#endif