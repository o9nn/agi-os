#ifndef gxobj_INCLUDED
#  define gxobj_INCLUDED
#include "gxbitmap.h"
#ifdef DEBUG
#define IGC_PTR_STABILITY_CHECK 0
#else
#define IGC_PTR_STABILITY_CHECK 0
#endif
#define obj_flag_bits 1
#define obj_mb_bits (arch_sizeof_int * 8 - obj_flag_bits)
#define o_unmarked (((uint)1 << obj_mb_bits) - 1)
#define o_set_unmarked(pp)\
((pp)->o_smark = o_unmarked)
#define o_is_unmarked(pp)\
((pp)->o_smark == o_unmarked)
#define o_untraced (((uint)1 << obj_mb_bits) - 2)
#define o_set_untraced(pp)\
((pp)->o_smark = o_untraced)
#define o_is_untraced(pp)\
((pp)->o_smark == o_untraced)
#define o_marked 0
#define o_mark(pp)\
((pp)->o_smark = o_marked)
#define obj_back_shift obj_flag_bits
#define obj_back_scale (1 << obj_back_shift)
typedef struct obj_header_data_s {
union _f {
struct _h {
unsigned alone:1;
} h;
struct _m {
unsigned _:1, smark:obj_mb_bits;
} m;
struct _b {
unsigned _:1, back:obj_mb_bits;
} b;
} f;
uint size;
union _t {
gs_memory_type_ptr_t type;
uint reloc;
} t;
#   if IGC_PTR_STABILITY_CHECK
unsigned space_id:3;
#   endif
} obj_header_data_t;
#define obj_align_mod\
(((arch_align_memory_mod - 1) |\
(align_bitmap_mod - 1) |\
(obj_back_scale - 1)) + 1)
#if obj_align_mod == 4
#  define log2_obj_align_mod 2
#else
#if obj_align_mod == 8
#  define log2_obj_align_mod 3
#else
#if obj_align_mod == 16
#  define log2_obj_align_mod 4
#endif
#endif
#endif
#define obj_align_mask (obj_align_mod-1)
#define obj_align_round(siz)\
(uint)(((siz) + obj_align_mask) & -obj_align_mod)
#define obj_size_round(siz)\
obj_align_round((siz) + sizeof(obj_header_t))
struct obj_header_s {
union _d {
obj_header_data_t o;
byte _pad[ROUND_UP(sizeof(obj_header_data_t), obj_align_mod)];
}
d;
};
#define o_alone d.o.f.h.alone
#define o_back d.o.f.b.back
#define o_smark d.o.f.m.smark
#define o_size d.o.size
#define o_type d.o.t.type
#define o_nreloc d.o.t.reloc
#define pre_obj_contents_size(pp)\
((pp)->o_size)
#define pre_obj_rounded_size(pp)\
obj_size_round(pre_obj_contents_size(pp))
#define pre_obj_next(pp)\
((obj_header_t *)((byte *)(pp) + obj_align_round(\
pre_obj_contents_size(pp) + sizeof(obj_header_t) )))
typedef struct chunk_head_s {
byte *dest;
#if obj_align_mod > arch_sizeof_ptr
byte *_pad[obj_align_mod / arch_sizeof_ptr - 1];
#endif
obj_header_t free;
} chunk_head_t;
#endif