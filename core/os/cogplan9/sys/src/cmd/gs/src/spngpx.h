#ifndef spngpx_INCLUDED
# define spngpx_INCLUDED
typedef struct stream_PNGP_state_s {
stream_state_common;
int Colors;
int BitsPerComponent;
uint Columns;
int Predictor;
uint row_count;
byte end_mask;
int bpp;
byte *prev_row;
int case_index;
long row_left;
byte prev[32];
} stream_PNGP_state;
#define private_st_PNGP_state() \
gs_private_st_ptrs1(st_PNGP_state, stream_PNGP_state,\
"PNGPredictorEncode/Decode state", pngp_enum_ptrs, pngp_reloc_ptrs,\
prev_row)
#define s_PNGP_set_defaults_inline(ss)\
((ss)->Colors = 1, (ss)->BitsPerComponent = 8, (ss)->Columns = 1,\
(ss)->Predictor = 15,\
\
(ss)->prev_row = 0)
extern const stream_template s_PNGPD_template;
extern const stream_template s_PNGPE_template;
#endif