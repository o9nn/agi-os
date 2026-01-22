#ifndef smd5_INCLUDED
#  define smd5_INCLUDED
#include "md5.h"
typedef struct stream_MD5E_state_s {
stream_state_common;
md5_state_t md5;
} stream_MD5E_state;
#define private_st_MD5E_state()	\
gs_private_st_simple(st_MD5E_state, stream_MD5E_state,\
"MD5Encode state")
extern const stream_template s_MD5E_template;
stream *s_MD5E_make_stream(gs_memory_t *mem, byte *digest, int digest_size);
#endif