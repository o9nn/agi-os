#ifndef gxalloc_INCLUDED
# define gxalloc_INCLUDED
#ifndef gs_ref_memory_DEFINED
# define gs_ref_memory_DEFINED
typedef struct gs_ref_memory_s gs_ref_memory_t;
#endif
#include "gsalloc.h"
#include "gxobj.h"
#define max_size_st_refs (50 * sizeof(ref))
typedef uint string_mark_unit;
#define log2_sizeof_string_mark_unit arch_log2_sizeof_int
typedef uint string_reloc_offset;
#define log2_string_data_quantum (arch_log2_sizeof_int + 4)
#define string_data_quantum (1 << log2_string_data_quantum)
#define string_space_quantum\
(string_data_quantum + (string_data_quantum / 8) +\
sizeof(string_reloc_offset))
#define string_chunk_space(nbytes)\
(((nbytes) + (string_data_quantum - 1)) / string_data_quantum *\
string_space_quantum)
#define string_space_quanta(spacebytes)\
((spacebytes) / string_space_quantum)
#define string_quanta_mark_size(nquanta)\
((nquanta) * (string_data_quantum / 8))
#define STRING_FREELIST_SPACE(cp)\
(((cp->climit - csbase(cp) + 255) >> 8) * sizeof(*cp->sfree1))
typedef struct chunk_s chunk_t;
struct chunk_s {
chunk_head_t *chead;
#define csbase(cp) ((byte *)(cp)->chead)
byte *cbase;
byte *int_freed_top;
byte *cbot;
obj_header_t *rcur;
byte *rtop;
byte *ctop;
byte *climit;
byte *cend;
chunk_t *cprev;
chunk_t *cnext;
chunk_t *outer;
uint inner_count;
bool has_refs;
#define SFREE_NB 4
uint *sfree1;
uint sfree;
byte *odest;
byte *smark;
uint smark_size;
byte *sbase;
string_reloc_offset *sreloc;
byte *sdest;
byte *rescan_bot;
byte *rescan_top;
};
extern_st(st_chunk);
#define public_st_chunk() \
gs_public_st_ptrs2(st_chunk, chunk_t, "chunk_t",\
chunk_enum_ptrs, chunk_reloc_ptrs, cprev, cnext)
#define SCAN_CHUNK_OBJECTS(cp)\
{ obj_header_t *pre = (obj_header_t *)((cp)->cbase);\
obj_header_t *end = (obj_header_t *)((cp)->cbot);\
uint size;\
\
for ( ; pre < end;\
pre = (obj_header_t *)((char *)pre + obj_size_round(size))\
)\
{
#define DO_ALL\
size = pre_obj_contents_size(pre);\
{
#define END_OBJECTS_SCAN_NO_ABORT\
}\
}\
}
#ifdef DEBUG
# define END_OBJECTS_SCAN\
}\
}\
if ( pre != end )\
{ lprintf2("Chunk parsing error, 0x%lx != 0x%lx\n",\
(ulong)pre, (ulong)end);\
\
}\
}
#else
# define END_OBJECTS_SCAN END_OBJECTS_SCAN_NO_ABORT
#endif
void alloc_init_chunk(chunk_t *, byte *, byte *, bool, chunk_t *);
void alloc_init_free_strings(chunk_t *);
#define ptr_is_within_chunk(ptr, cp)\
PTR_BETWEEN((const byte *)(ptr), (cp)->cbase, (cp)->cend)
#define ptr_is_in_inner_chunk(ptr, cp)\
((cp)->inner_count != 0 &&\
PTR_BETWEEN((const byte *)(ptr), (cp)->cbot, (cp)->ctop))
#define ptr_is_in_chunk(ptr, cp)\
(ptr_is_within_chunk(ptr, cp) && !ptr_is_in_inner_chunk(ptr, cp))
typedef struct chunk_locator_s {
const gs_ref_memory_t *memory;
chunk_t *cp;
} chunk_locator_t;
bool chunk_locate_ptr(const void *, chunk_locator_t *);
#define chunk_locate(ptr, clp)\
(((clp)->cp != 0 && ptr_is_in_chunk(ptr, (clp)->cp)) ||\
chunk_locate_ptr(ptr, clp))
void alloc_close_chunk(gs_ref_memory_t * mem);
void alloc_open_chunk(gs_ref_memory_t * mem);
void alloc_link_chunk(chunk_t *, gs_ref_memory_t *);
void alloc_unlink_chunk(chunk_t *, gs_ref_memory_t *);
void alloc_free_chunk(chunk_t *, gs_ref_memory_t *);
#define dprintf_chunk_format\
"%s 0x%lx (0x%lx..0x%lx, 0x%lx..0x%lx..0x%lx)\n"
#define dprintf_chunk(msg, cp)\
dprintf7(dprintf_chunk_format,\
msg, (ulong)(cp), (ulong)(cp)->cbase, (ulong)(cp)->cbot,\
(ulong)(cp)->ctop, (ulong)(cp)->climit, (ulong)(cp)->cend)
#define if_debug_chunk(c, msg, cp)\
if_debug7(c, dprintf_chunk_format,\
msg, (ulong)(cp), (ulong)(cp)->cbase, (ulong)(cp)->cbot,\
(ulong)(cp)->ctop, (ulong)(cp)->climit, (ulong)(cp)->cend)
struct alloc_save_s;
struct alloc_change_s;
#ifndef stream_DEFINED
# define stream_DEFINED
typedef struct stream_s stream;
#endif
#ifndef ref_DEFINED
typedef struct ref_s ref;
# define ref_DEFINED
#endif
#define max_freelist_size 800
#define num_small_freelists\
((max_freelist_size + obj_align_mod - 1) / obj_align_mod + 1)
#define num_freelists (num_small_freelists + 1)
#define LARGE_FREELIST_INDEX num_small_freelists
struct gs_ref_memory_s {
gs_memory_common;
uint chunk_size;
uint large_size;
uint space;
# if IGC_PTR_STABILITY_CHECK
unsigned space_id:3;
# endif
gs_memory_gc_status_t gc_status;
bool is_controlled;
ulong limit;
chunk_t *cfirst;
chunk_t *clast;
chunk_t cc;
chunk_t *pcc;
chunk_locator_t cfreed;
ulong allocated;
long inherited;
ulong gc_allocated;
struct lost_ {
ulong objects;
ulong refs;
ulong strings;
} lost;
int save_level;
uint new_mask;
uint test_mask;
stream *streams;
ref *names_array;
gs_gc_root_t *roots;
int num_contexts;
struct alloc_change_s *changes;
struct alloc_save_s *saved;
long total_scanned;
struct alloc_save_s *reloc_saved;
gs_memory_status_t previous_status;
uint largest_free_size;
obj_header_t *freelists[num_freelists];
};
extern_st(st_ref_memory);
#define public_st_ref_memory() \
gs_public_st_composite(st_ref_memory, gs_ref_memory_t,\
"gs_ref_memory", ref_memory_enum_ptrs, ref_memory_reloc_ptrs)
#define st_ref_memory_max_ptrs 4
extern const gs_memory_procs_t gs_ref_memory_procs;
#define SCAN_MEM_CHUNKS(mem, cp)\
{ chunk_t *cp = (mem)->cfirst;\
for ( ; cp != 0; cp = cp->cnext )\
{
#define END_CHUNKS_SCAN\
}\
}
#ifdef DEBUG
typedef enum {
dump_do_default = 0,
dump_do_strings = 1,
dump_do_type_addresses = 2,
dump_do_no_types = 4,
dump_do_pointers = 8,
dump_do_pointed_strings = 16,
dump_do_contents = 32,
dump_do_marks = 64
} dump_options_t;
typedef struct dump_control_s {
dump_options_t options;
const byte *bottom;
const byte *top;
} dump_control_t;
extern const dump_control_t dump_control_default;
extern const dump_control_t dump_control_all;
void debug_print_object(const gs_memory_t *mem, const void *obj, const dump_control_t * control);
void debug_dump_chunk(const gs_memory_t *mem, const chunk_t * cp, const dump_control_t * control);
void debug_print_chunk(const gs_memory_t *mem, const chunk_t * cp);
void debug_dump_memory(const gs_ref_memory_t *mem,
const dump_control_t *control);
void debug_find_pointers(const gs_ref_memory_t *mem, const void *target);
#endif
#endif