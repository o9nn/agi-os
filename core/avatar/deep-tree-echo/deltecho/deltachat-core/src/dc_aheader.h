#ifndef __DC_AHEADER_H__
#define __DC_AHEADER_H__
#ifdef __cplusplus
extern "C" {
#endif
#include "dc_key.h"
typedef struct _dc_aheader dc_aheader_t;
struct _dc_aheader
{
char*          addr;
dc_key_t*      public_key;
int            prefer_encrypt;
};
dc_aheader_t* dc_aheader_new               ();
dc_aheader_t* dc_aheader_new_from_imffields(const char* wanted_from, const struct mailimf_fields* mime);
void          dc_aheader_empty             (dc_aheader_t*);
void          dc_aheader_unref             (dc_aheader_t*);
int           dc_aheader_set_from_string   (dc_aheader_t*, const char* header_str);
char*         dc_aheader_render            (const dc_aheader_t*);
#ifdef __cplusplus
}
#endif
#endif