#ifndef sa85d_INCLUDED
#  define sa85d_INCLUDED
typedef struct stream_A85D_state_s {
stream_state_common;
int odd;
ulong word;
} stream_A85D_state;
#define private_st_A85D_state()	\
gs_private_st_simple(st_A85D_state, stream_A85D_state,\
"ASCII85Decode state")
#define s_A85D_init_inline(ss)\
((ss)->min_left = 1, (ss)->word = 0, (ss)->odd = 0)
extern const stream_template s_A85D_template;
#endif