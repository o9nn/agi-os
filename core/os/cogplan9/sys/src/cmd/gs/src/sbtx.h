#ifndef sbtx_INCLUDED
#  define sbtx_INCLUDED
typedef struct stream_BT_state_s {
stream_state_common;
byte table[256];
} stream_BT_state;
typedef stream_BT_state stream_BTE_state;
typedef stream_BT_state stream_BTD_state;
#define private_st_BT_state()	\
gs_private_st_simple(st_BT_state, stream_BT_state,\
"ByteTranslateEncode/Decode state")
extern const stream_template s_BTE_template;
extern const stream_template s_BTD_template;
#endif