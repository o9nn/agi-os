#ifndef gsutil_INCLUDED
#  define gsutil_INCLUDED
gs_id gs_next_ids(const gs_memory_t *mem, uint count);
void memflip8x8(const byte * inp, int line_size, byte * outp, int dist);
ulong get_u32_msb(const byte *p);
int bytes_compare(const byte * str1, uint len1,
const byte * str2, uint len2);
typedef struct string_match_params_s {
int any_substring;
int any_char;
int quote_next;
bool ignore_case;
bool slash_equiv;
} string_match_params;
extern const string_match_params string_match_params_default;
bool string_match(const byte * str, uint len,
const byte * pstr, uint plen,
const string_match_params * psmp);
typedef enum {
GS_DEVICE_DOESNT_SUPPORT_TAGS = 0,
GS_UNKNOWN_TAG = 0x1,
GS_TEXT_TAG = 0x2,
GS_IMAGE_TAG = 0x4,
GS_PATH_TAG = 0x8,
GS_UNTOUCHED_TAG = 0x10
} gs_object_tag_type_t;
gs_object_tag_type_t gs_current_object_tag(void);
#include "gxstate.h"
void gs_set_object_tag(gs_state * pgs, const gs_object_tag_type_t tag);
void gs_enable_object_tagging(void);
#endif