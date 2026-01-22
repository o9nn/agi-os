#ifndef srlx_INCLUDED
#  define srlx_INCLUDED
#define stream_RL_state_common\
stream_state_common;\
bool EndOfData
typedef struct stream_RLE_state_s {
stream_RL_state_common;
ulong record_size;
ulong record_left;
int copy_left;
} stream_RLE_state;
#define private_st_RLE_state()	\
gs_private_st_simple(st_RLE_state, stream_RLE_state, "RunLengthEncode state")
#define s_RLE_set_defaults_inline(ss)\
((ss)->EndOfData = true, (ss)->record_size = 0)
#define s_RLE_init_inline(ss)\
((ss)->record_left =\
((ss)->record_size == 0 ? ((ss)->record_size = max_uint) :\
(ss)->record_size),\
(ss)->copy_left = 0)
extern const stream_template s_RLE_template;
typedef struct stream_RLD_state_s {
stream_RL_state_common;
int copy_left;
int copy_data;
} stream_RLD_state;
#define private_st_RLD_state()	\
gs_private_st_simple(st_RLD_state, stream_RLD_state, "RunLengthDecode state")
#define s_RLD_set_defaults_inline(ss)\
((ss)->EndOfData = true)
#define s_RLD_init_inline(ss)\
((ss)->min_left = ((ss)->EndOfData ? 1 : 0), (ss)->copy_left = 0)
extern const stream_template s_RLD_template;
#endif