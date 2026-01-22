#ifndef sarc4_INCLUDED
#  define sarc4_INCLUDED
#include "scommon.h"
struct stream_arcfour_state_s
{
stream_state_common;
unsigned int x, y;
unsigned char S[256];
};
#ifndef stream_arcfour_state_DEFINED
#define stream_arcfour_state_DEFINED
typedef struct stream_arcfour_state_s stream_arcfour_state;
#endif
int s_arcfour_set_key(stream_arcfour_state * state, const unsigned char *key,
int keylength);
#define private_st_arcfour_state()	\
gs_private_st_simple(st_arcfour_state, stream_arcfour_state,\
"Arcfour filter state")
extern const stream_template s_arcfour_template;
int s_arcfour_process_buffer(stream_arcfour_state *ss, byte *buf, int buf_size);
#endif