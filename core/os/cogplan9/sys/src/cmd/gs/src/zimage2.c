#include "memory_.h"
#include "ghost.h"
#include "gsimage.h"
#include "gxiparam.h"
#include "icstate.h"
#include "iimage2.h"
#include "igstate.h"
int
process_non_source_image(i_ctx_t *i_ctx_p, const gs_image_common_t * pic,
client_name_t cname)
{
gx_image_enum_common_t *pie;
int code = gs_image_begin_typed(pic, igs, false  ,
&pie);
return code;
}