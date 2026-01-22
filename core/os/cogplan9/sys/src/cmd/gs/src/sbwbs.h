#ifndef sbwbs_INCLUDED
#  define sbwbs_INCLUDED
#define stream_buffered_state_common\
stream_state_common;\
\
\
int BlockSize;\
\
\
byte *buffer;		\
\
bool filling;		\
\
int bsize;		\
int bpos
typedef struct stream_buffered_state_s {
stream_buffered_state_common;
} stream_buffered_state;
#define private_st_buffered_state()	\
gs_private_st_ptrs1(st_buffered_state, stream_buffered_state,\
"stream_buffered state", sbuf_enum_ptrs, sbuf_reloc_ptrs, buffer)
typedef struct of_ {
uint v[256];
} offsets_full;
typedef struct stream_BWBS_state_s {
stream_buffered_state_common;
void *offsets;
int N;
int I;
int i;
} stream_BWBS_state;
typedef stream_BWBS_state stream_BWBSE_state;
typedef stream_BWBS_state stream_BWBSD_state;
#define private_st_BWBS_state()	\
gs_private_st_suffix_add1(st_BWBS_state, stream_BWBS_state,\
"BWBlockSortEncode/Decode state", bwbs_enum_ptrs, bwbs_reloc_ptrs,\
st_buffered_state, offsets)
extern const stream_template s_BWBSE_template;
extern const stream_template s_BWBSD_template;
#endif