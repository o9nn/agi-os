#ifndef sstring_INCLUDED
# define sstring_INCLUDED
typedef struct stream_AXE_state_s {
stream_state_common;
bool EndOfData;
int count;
} stream_AXE_state;
#define private_st_AXE_state() \
gs_private_st_simple(st_AXE_state, stream_AXE_state,\
"ASCIIHexEncode state")
#define s_AXE_init_inline(ss)\
((ss)->EndOfData = true, (ss)->count = 0)
extern const stream_template s_AXE_template;
typedef struct stream_AXD_state_s {
stream_state_common;
int odd;
} stream_AXD_state;
#define private_st_AXD_state() \
gs_private_st_simple(st_AXD_state, stream_AXD_state,\
"ASCIIHexDecode state")
#define s_AXD_init_inline(ss)\
((ss)->min_left = 1, (ss)->odd = -1, 0)
extern const stream_template s_AXD_template;
typedef struct stream_PSSD_state_s {
stream_state_common;
bool from_string;
int depth;
} stream_PSSD_state;
#define private_st_PSSD_state() \
gs_private_st_simple(st_PSSD_state, stream_PSSD_state,\
"PSStringDecode state")
int s_PSSD_init(stream_state * st);
#define s_PSSD_partially_init_inline(ss)\
((ss)->depth = 0)
extern const stream_template s_PSSD_template;
extern const stream_template s_PSSE_template;
#endif