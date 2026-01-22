#ifndef icid_INCLUDED
#  define icid_INCLUDED
#ifndef gs_cid_system_info_DEFINED
#  define gs_cid_system_info_DEFINED
typedef struct gs_cid_system_info_s gs_cid_system_info_t;
#endif
int cid_system_info_param(gs_cid_system_info_t *, const ref *);
int cid_to_TT_charcode(const gs_memory_t *mem,
const ref *Decoding, const ref *TT_cmap,
const ref *SubstNWP,
uint nCID, uint *c, ref *src_type, ref *dst_type);
int cid_fill_CIDMap(const gs_memory_t *mem, const ref *Decoding, const ref *TT_cmap, const ref *SubstNWP,
int GDBytes, ref *CIDMap);
int ztype9mapcid(i_ctx_t *i_ctx_p);
#endif