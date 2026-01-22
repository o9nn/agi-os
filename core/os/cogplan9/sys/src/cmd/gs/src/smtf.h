#ifndef smtf_INCLUDED
#  define smtf_INCLUDED
typedef struct stream_MTF_state_s {
stream_state_common;
union _p {
ulong l[256 / sizeof(long)];
byte b[256];
} prev;
} stream_MTF_state;
typedef stream_MTF_state stream_MTFE_state;
typedef stream_MTF_state stream_MTFD_state;
#define private_st_MTF_state()	\
gs_private_st_simple(st_MTF_state, stream_MTF_state,\
"MoveToFrontEncode/Decode state")
extern const stream_template s_MTFE_template;
extern const stream_template s_MTFD_template;
#endif