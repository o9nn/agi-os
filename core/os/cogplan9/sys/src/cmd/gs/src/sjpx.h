#ifndef sjpx_INCLUDED
#  define sjpx_INCLUDED
#include "scommon.h"
#include <jasper/jasper.h>
typedef struct stream_jpxd_state_s
{
stream_state_common;
jas_image_t *image;
jas_stream_t *stream;
long offset;
const gs_memory_t *jpx_memory;
unsigned char *buffer;
long bufsize;
long buffill;
}
stream_jpxd_state;
#define private_st_jpxd_state()	\
gs_private_st_simple(st_jpxd_state, stream_jpxd_state,\
"JPXDecode filter state")
extern const stream_template s_jpxd_template;
#endif