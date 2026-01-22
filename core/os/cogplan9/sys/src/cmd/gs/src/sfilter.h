#ifndef sfilter_INCLUDED
# define sfilter_INCLUDED
#include "gstypes.h"
typedef struct stream_exE_state_s {
stream_state_common;
ushort cstate;
} stream_exE_state;
#define private_st_exE_state() \
gs_private_st_simple(st_exE_state, stream_exE_state, "eexecEncode state")
extern const stream_template s_exE_template;
typedef struct stream_PFBD_state_s stream_PFBD_state;
typedef struct stream_exD_state_s {
stream_state_common;
ushort cstate;
int binary;
int lenIV;
stream_PFBD_state *pfb_state;
int odd;
long record_left;
long hex_left;
int skip;
} stream_exD_state;
#define private_st_exD_state() \
gs_private_st_ptrs1(st_exD_state, stream_exD_state, "eexecDecode state",\
exd_enum_ptrs, exd_reloc_ptrs, pfb_state)
extern const stream_template s_exD_template;
struct stream_PFBD_state_s {
stream_state_common;
int binary_to_hex;
int record_type;
ulong record_left;
} ;
#define private_st_PFBD_state() \
gs_private_st_simple(st_PFBD_state, stream_PFBD_state, "PFBDecode state")
extern const stream_template s_PFBD_template;
typedef struct stream_SFD_state_s {
stream_state_common;
long count;
gs_const_string eod;
long skip_count;
uint match;
uint copy_count;
uint copy_ptr;
} stream_SFD_state;
#define private_st_SFD_state() \
gs_private_st_const_strings1(st_SFD_state, stream_SFD_state,\
"SubFileDecode state", sfd_enum_ptrs, sfd_reloc_ptrs, eod)
extern const stream_template s_SFD_template;
#endif