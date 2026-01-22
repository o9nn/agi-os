#ifndef gsfont_INCLUDED
# define gsfont_INCLUDED
#ifndef gs_matrix_DEFINED
# define gs_matrix_DEFINED
typedef struct gs_matrix_s gs_matrix;
#endif
#ifndef gs_font_dir_DEFINED
# define gs_font_dir_DEFINED
typedef struct gs_font_dir_s gs_font_dir;
#endif
#ifndef gs_font_DEFINED
# define gs_font_DEFINED
typedef struct gs_font_s gs_font;
#endif
gs_font_dir *gs_font_dir_alloc2(gs_memory_t * struct_mem,
gs_memory_t * bits_mem);
gs_font_dir *gs_font_dir_alloc2_limits(gs_memory_t * struct_mem,
gs_memory_t * bits_mem,
uint smax, uint bmax, uint mmax,
uint cmax, uint upper);
#define gs_font_dir_alloc(mem) gs_font_dir_alloc2(mem, mem)
#define gs_font_dir_alloc_limits(mem, smax, bmax, mmax, cmax, upper)\
gs_font_dir_alloc2_limits(mem, mem, smax, bmax, mmax, cmax, upper)
int gs_definefont(gs_font_dir *, gs_font *);
int gs_font_find_similar(const gs_font_dir * pdir, const gs_font **ppfont,
int (*similar)(const gs_font *, const gs_font *));
int gs_scalefont(gs_font_dir *, const gs_font *, floatp, gs_font **);
int gs_makefont(gs_font_dir *, const gs_font *, const gs_matrix *, gs_font **);
int gs_setfont(gs_state *, gs_font *);
gs_font *gs_currentfont(const gs_state *);
gs_font *gs_rootfont(const gs_state *);
void gs_set_currentfont(gs_state *, gs_font *);
void gs_purge_font(gs_font *);
gs_font *gs_find_font_by_id(gs_font_dir *pdir, gs_id id, gs_matrix *FontMatrix);
void gs_cachestatus(const gs_font_dir *, uint[7]);
#define gs_setcachelimit(pdir,limit) gs_setcacheupper(pdir,limit)
uint gs_currentcachesize(const gs_font_dir *);
int gs_setcachesize(gs_font_dir *, uint);
uint gs_currentcachelower(const gs_font_dir *);
int gs_setcachelower(gs_font_dir *, uint);
uint gs_currentcacheupper(const gs_font_dir *);
int gs_setcacheupper(gs_font_dir *, uint);
uint gs_currentaligntopixels(const gs_font_dir *);
int gs_setaligntopixels(gs_font_dir *, uint);
uint gs_currentgridfittt(const gs_font_dir *);
int gs_setgridfittt(gs_font_dir *, uint);
#endif