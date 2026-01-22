#ifndef sa85x_INCLUDED
# define sa85x_INCLUDED
#include "sa85d.h"
typedef struct stream_A85E_state_s {
stream_state_common;
int count;
int last_char;
} stream_A85E_state;
#define private_st_A85E_state() \
gs_private_st_simple(st_A85E_state, stream_A85E_state,\
"ASCII85Encode state")
#define s_A85E_init_inline(ss)\
((ss)->count = 0, (ss)->last_char = '\n', 0)
extern const stream_template s_A85E_template;
#endif