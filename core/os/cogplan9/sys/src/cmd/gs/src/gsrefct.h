#ifndef gsrefct_INCLUDED
# define gsrefct_INCLUDED
typedef struct rc_header_s rc_header;
struct rc_header_s {
long ref_count;
gs_memory_t *memory;
#define rc_free_proc(proc)\
void proc(gs_memory_t *, void *, client_name_t)
rc_free_proc((*free));
};
#ifdef DEBUG
void rc_trace_init_free(const void *vp, const rc_header *prc);
void rc_trace_free_struct(const void *vp, const rc_header *prc,
client_name_t cname);
void rc_trace_increment(const void *vp, const rc_header *prc);
void rc_trace_adjust(const void *vp, const rc_header *prc, int delta);
#define IF_RC_DEBUG(call) if (gs_debug_c('^')) dlputs(""), call
#else
#define IF_RC_DEBUG(call) DO_NOTHING
#endif
rc_free_proc(rc_free_struct_only);
#define rc_init_free(vp, mem, rcinit, proc)\
BEGIN\
(vp)->rc.ref_count = rcinit;\
(vp)->rc.memory = mem;\
(vp)->rc.free = proc;\
IF_RC_DEBUG(rc_trace_init_free(vp, &(vp)->rc));\
END
#define rc_init(vp, mem, rcinit)\
rc_init_free(vp, mem, rcinit, rc_free_struct_only)
#define rc_alloc_struct_n(vp, typ, pstyp, mem, errstat, cname, rcinit)\
BEGIN\
if ( ((vp) = gs_alloc_struct(mem, typ, pstyp, cname)) == 0 ) {\
errstat;\
} else {\
rc_init(vp, mem, rcinit);\
}\
END
#define rc_alloc_struct_0(vp, typ, pstype, mem, errstat, cname)\
rc_alloc_struct_n(vp, typ, pstype, mem, errstat, cname, 0)
#define rc_alloc_struct_1(vp, typ, pstype, mem, errstat, cname)\
rc_alloc_struct_n(vp, typ, pstype, mem, errstat, cname, 1)
#define rc_free_struct(vp, cname)\
BEGIN\
IF_RC_DEBUG(rc_trace_free_struct(vp, &(vp)->rc, cname));\
(vp)->rc.free((vp)->rc.memory, (void *)(vp), cname);\
END
#define RC_DO_INCREMENT(vp)\
BEGIN\
(vp)->rc.ref_count++;\
IF_RC_DEBUG(rc_trace_increment(vp, &(vp)->rc));\
END
#define rc_increment(vp)\
BEGIN\
if (vp) RC_DO_INCREMENT(vp);\
END
#define rc_allocate_struct(vp, typ, pstype, mem, errstat, cname)\
BEGIN\
if (vp)\
RC_DO_INCREMENT(vp);\
else\
rc_alloc_struct_1(vp, typ, pstype, mem, errstat, cname);\
END
#define RC_DO_ADJUST(vp, delta)\
BEGIN\
IF_RC_DEBUG(rc_trace_adjust(vp, &(vp)->rc, delta));\
(vp)->rc.ref_count += (delta);\
END
#define rc_unshare_struct(vp, typ, pstype, mem, errstat, cname)\
BEGIN\
if ( (vp) == 0 || (vp)->rc.ref_count > 1 || (vp)->rc.memory != (mem) ) {\
typ *new;\
rc_alloc_struct_1(new, typ, pstype, mem, errstat, cname);\
if ( vp ) RC_DO_ADJUST(vp, -1);\
(vp) = new;\
}\
END
#ifdef DEBUG
# define rc_check_(vp)\
BEGIN\
if (gs_debug_c('?') && (vp)->rc.ref_count < 0)\
lprintf2("0x%lx has ref_count of %ld!\n", (ulong)(vp),\
(vp)->rc.ref_count);\
END
#else
# define rc_check_(vp) DO_NOTHING
#endif
#define rc_adjust_(vp, delta, cname, body)\
BEGIN\
if (vp) {\
RC_DO_ADJUST(vp, delta);\
if (!(vp)->rc.ref_count) {\
rc_free_struct(vp, cname);\
body;\
} else\
rc_check_(vp);\
}\
END
#define rc_adjust(vp, delta, cname)\
rc_adjust_(vp, delta, cname, (vp) = 0)
#define rc_adjust_only(vp, delta, cname)\
rc_adjust_(vp, delta, cname, DO_NOTHING)
#define rc_adjust_const(vp, delta, cname)\
rc_adjust_only(vp, delta, cname)
#define rc_decrement(vp, cname)\
rc_adjust(vp, -1, cname)
#define rc_decrement_only(vp, cname)\
rc_adjust_only(vp, -1, cname)
#define rc_assign(vpto, vpfrom, cname)\
BEGIN\
if ((vpto) != (vpfrom)) {\
rc_increment(vpfrom);\
rc_decrement_only(vpto, cname);\
(vpto) = (vpfrom);\
}\
END
#define rc_pre_assign(vpto, vpfrom, cname)\
BEGIN\
if ((vpto) != (vpfrom)) {\
rc_increment(vpfrom);\
rc_decrement_only(vpto, cname);\
}\
END
#endif