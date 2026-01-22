#ifndef sbhc_INCLUDED
#  define sbhc_INCLUDED
#include "shc.h"
#define max_zero_run 100
#define stream_BHC_state_common\
stream_hc_state_common;\
hc_definition definition;\
\
bool EndOfData;\
uint EncodeZeroRuns;\
\
int zeros
typedef struct stream_BHC_state_s {
stream_BHC_state_common;
} stream_BHC_state;
typedef struct stream_BHCE_state_s {
stream_BHC_state_common;
hce_table encode;
} stream_BHCE_state;
#define private_st_BHCE_state()	\
gs_private_st_ptrs3(st_BHCE_state, stream_BHCE_state,\
"BoundedHuffmanEncode state", bhce_enum_ptrs, bhce_reloc_ptrs,\
definition.counts, definition.values, encode.codes)
extern const stream_template s_BHCE_template;
#define s_bhce_init_inline(ss)\
(s_hce_init_inline(ss), (ss)->zeros = 0)
typedef struct stream_BHCD_state_s {
stream_BHC_state_common;
hcd_table decode;
} stream_BHCD_state;
#define private_st_BHCD_state()	\
gs_private_st_ptrs3(st_BHCD_state, stream_BHCD_state,\
"BoundedHuffmanDecode state", bhcd_enum_ptrs, bhcd_reloc_ptrs,\
definition.counts, definition.values, decode.codes)
extern const stream_template s_BHCD_template;
#define s_bhcd_init_inline(ss)\
(s_hcd_init_inline(ss), (ss)->zeros = 0)
#define bhcd_declare_state\
hcd_declare_state;\
int zeros
#define bhcd_load_state()\
hcd_load_state(), zeros = ss->zeros
#define bhcd_store_state()\
hcd_store_state(), ss->zeros = zeros
#endif