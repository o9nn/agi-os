#ifndef gzspotan_INCLUDED
# define gzspotan_INCLUDED
#include "gxdevcli.h"
#ifndef segment_DEFINED
# define segment_DEFINED
typedef struct segment_s segment;
#endif
#ifndef gx_device_spot_analyzer_DEFINED
# define gx_device_spot_analyzer_DEFINED
typedef struct gx_device_spot_analyzer_s gx_device_spot_analyzer;
#endif
typedef struct gx_san_trap_s gx_san_trap;
typedef struct gx_san_trap_contact_s gx_san_trap_contact;
struct gx_san_trap_s {
gx_san_trap *link;
fixed ybot, ytop;
fixed xlbot, xrbot, xltop, xrtop;
gx_san_trap_contact *upper;
const segment *l;
const segment *r;
int dir_l, dir_r;
bool leftmost, rightmost;
gx_san_trap *next;
gx_san_trap *prev;
bool visited;
int fork;
};
#define private_st_san_trap() \
gs_private_st_ptrs1(st_san_trap, gx_san_trap, "gx_san_trap", \
san_trap_enum_ptrs, san_trap_reloc_ptrs, link)
struct gx_san_trap_contact_s {
gx_san_trap_contact *link;
gx_san_trap_contact *next;
gx_san_trap_contact *prev;
gx_san_trap *upper, *lower;
};
#define private_st_san_trap_contact() \
gs_private_st_ptrs1(st_san_trap_contact, gx_san_trap_contact, "gx_san_trap_contact",\
san_trap_contact_enum_ptrs, san_trap_contact_reloc_ptrs, link)
typedef struct gx_san_sect_s gx_san_sect;
struct gx_san_sect_s {
fixed xl, yl, xr, yr;
const segment *l, *r;
int side_mask;
};
struct gx_device_spot_analyzer_s {
gx_device_common;
int lock;
gx_san_trap *trap_buffer, *trap_buffer_last, *trap_free;
gx_san_trap_contact *cont_buffer, *cont_buffer_last, *cont_free;
int trap_buffer_count;
int cont_buffer_count;
gx_san_trap *bot_band;
gx_san_trap *top_band;
gx_san_trap *bot_current;
fixed xmin, xmax;
};
extern_st(st_device_spot_analyzer);
#define public_st_device_spot_analyzer() \
gs_public_st_suffix_add4_final(st_device_spot_analyzer, gx_device_spot_analyzer,\
"gx_device_spot_analyzer", device_spot_analyzer_enum_ptrs,\
device_spot_analyzer_reloc_ptrs, gx_device_finalize, st_device,\
trap_buffer, trap_buffer_last, cont_buffer, cont_buffer_last)
int gx_san__obtain(gs_memory_t *mem, gx_device_spot_analyzer **ppadev);
void gx_san__release(gx_device_spot_analyzer **ppadev);
void gx_san_begin(gx_device_spot_analyzer *padev);
int gx_san_trap_store(gx_device_spot_analyzer *padev,
fixed ybot, fixed ytop, fixed xlbot, fixed xrbot, fixed xltop, fixed xrtop,
const segment *l, const segment *r, int dir_l, int dir_r);
void gx_san_end(const gx_device_spot_analyzer *padev);
int gx_san_generate_stems(gx_device_spot_analyzer *padev,
bool overall_hints, void *client_data,
int (*handler)(void *client_data, gx_san_sect *ss));
#endif