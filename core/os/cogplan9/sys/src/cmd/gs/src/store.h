#ifndef store_INCLUDED
#  define store_INCLUDED
#include "ialloc.h"
#include "idosave.h"
#define ref_assign_inline(pto,pfrom)\
((pto)->value = (pfrom)->value,\
(pto)->tas = (pfrom)->tas)
#ifdef __TURBOC__
#  define ref_assign(pto,pfrom)\
discard(ref_assign_inline(pto, pfrom))
#else
#  define ref_assign(pto,pfrom)\
(*(pto) = *(pfrom))
#endif
#define ialloc_new_mask (idmemory->new_mask)
#define ref_saving_in(mmem)\
((mmem)->new_mask != 0)
#define ref_must_save_in(mmem,pto)\
((r_type_attrs(pto) & (mmem)->test_mask) == 0)
#define ref_must_save(pto) ref_must_save_in(idmemory, pto)
#define ref_do_save_in(mem, pcont, pto, cname)\
alloc_save_change_in(mem, pcont, (ref_packed *)(pto), cname)
#define ref_do_save(pcont, pto, cname)\
alloc_save_change(idmemory, pcont, (ref_packed *)(pto), cname)
#define ref_save_in(mem, pcont, pto, cname)\
discard((ref_must_save_in(mem, pto) ?\
ref_do_save_in(mem, pcont, pto, cname) : 0))
#define ref_save(pcont, pto, cname)\
discard((ref_must_save(pto) ? ref_do_save(pcont, pto, cname) : 0))
#define ref_mark_new_in(mmem,pto)\
((pto)->tas.type_attrs |= (mmem)->new_mask)
#define ref_mark_new(pto) ref_mark_new_in(idmemory, pto)
#define ref_assign_new_in(mem,pto,pfrom)\
discard((ref_assign(pto,pfrom), ref_mark_new_in(mem,pto)))
#define ref_assign_new(pto,pfrom)\
discard((ref_assign(pto,pfrom), ref_mark_new(pto)))
#define ref_assign_new_inline(pto,pfrom)\
discard((ref_assign_inline(pto,pfrom), ref_mark_new(pto)))
#define ref_assign_old_in(mem,pcont,pto,pfrom,cname)\
(ref_save_in(mem,pcont,pto,cname), ref_assign_new_in(mem,pto,pfrom))
#define ref_assign_old(pcont,pto,pfrom,cname)\
(ref_save(pcont,pto,cname), ref_assign_new(pto,pfrom))
#define ref_assign_old_inline(pcont,pto,pfrom,cname)\
(ref_save(pcont,pto,cname), ref_assign_new_inline(pto,pfrom))
#define ref_mark_old(pto) ((pto)->tas.type_attrs &= ~ialloc_new_mask)
#ifdef DEBUG
#  define and_fill_s(pref)\
, (gs_debug['$'] ? r_set_size(pref, 0xfeed) : 0)
#define DEADBEEF ((int)(((uint)0xdead << 16) | 0xbeef))
#  define and_fill_sv(pref)\
, (gs_debug['$'] ? (r_set_size(pref, 0xfeed),\
(pref)->value.intval = DEADBEEF) : 0)
#else
#  define and_fill_s(pref)
#  define and_fill_sv(pref)
#endif
#define make_ta(pref,newtype,newattrs)\
(r_set_type_attrs(pref, newtype, newattrs) and_fill_sv(pref))
#define make_t(pref,newtype)\
make_ta(pref, newtype, 0)
#define make_t_new_in(mem,pref,newtype)\
make_ta(pref, newtype, imemory_new_mask(mem))
#define make_t_new(pref,newtype)\
make_ta(pref, newtype, ialloc_new_mask)
#define make_t_old_in(mem,pcont,pref,newtype,cname)\
(ref_save_in(mem,pcont,pref,cname), make_t_new_in(mem,pref,newtype))
#define make_t_old(pcont,pref,newtype,cname)\
(ref_save(pcont,pref,cname), make_t_new(pref,newtype))
#define make_tav(pref,newtype,newattrs,valfield,newvalue)\
((pref)->value.valfield = (newvalue),\
r_set_type_attrs(pref, newtype, newattrs)\
and_fill_s(pref))
#define make_tav_new(pref,t,a,vf,v)\
make_tav(pref,t,(a)|ialloc_new_mask,vf,v)
#define make_tav_old(pcont,pref,t,a,vf,v,cname)\
(ref_save(pcont,pref,cname), make_tav_new(pref,t,a,vf,v))
#define make_tv(pref,newtype,valfield,newvalue)\
make_tav(pref,newtype,0,valfield,newvalue)
#define make_tv_new(pref,t,vf,v)\
make_tav_new(pref,t,0,vf,v)
#define make_tv_old(pcont,pref,t,vf,v,cname)\
make_tav_old(pcont,pref,t,0,vf,v,cname)
#define make_tasv(pref,newtype,newattrs,newsize,valfield,newvalue)\
((pref)->value.valfield = (newvalue),\
r_set_type_attrs(pref, newtype, newattrs),\
r_set_size(pref, newsize))
#define make_tasv_new(pref,t,a,s,vf,v)\
make_tasv(pref,t,(a)|ialloc_new_mask,s,vf,v)
#define make_tasv_old(pcont,pref,t,a,s,vf,v,cname)\
(ref_save(pcont,pref,cname), make_tasv_new(pref,t,a,s,vf,v))
#define make_bool(pref,bval)\
make_tv(pref, t_boolean, boolval, bval)
#define make_false(pref)\
make_bool(pref, 0)
#define make_true(pref)\
make_bool(pref, 1)
#define make_int(pref,ival)\
make_tv(pref, t_integer, intval, ival)
#define make_int_new(pref,ival)\
make_tv_new(pref, t_integer, intval, ival)
#define make_mark(pref)\
make_t(pref, t_mark)
#define make_null(pref)\
make_t(pref, t_null)
#define make_null_new(pref)\
make_t_new(pref, t_null)
#define make_null_old_in(mem,pcont,pref,cname)\
make_t_old_in(mem, pcont, pref, t_null, cname)
#define make_null_old(pcont,pref,cname)\
make_t_old(pcont, pref, t_null, cname)
#define make_oper(pref,opidx,proc)\
make_tasv(pref, t_operator, a_executable, opidx, opproc, proc)
#define make_oper_new(pref,opidx,proc)\
make_tasv_new(pref, t_operator, a_executable, opidx, opproc, proc)
#define make_real(pref,rval)\
make_tv(pref, t_real, realval, rval)
#define make_real_new(pref,rval)\
make_tv_new(pref, t_real, realval, rval)
#define make_array(pref,attrs,size,elts)\
make_tasv(pref, t_array, attrs, size, refs, elts)
#define make_array_new(pref,attrs,size,elts)\
make_tasv_new(pref, t_array, attrs, size, refs, elts)
#define make_const_array(pref,attrs,size,elts)\
make_tasv(pref, t_array, attrs, size, const_refs, elts)
#define make_empty_array(pref,attrs)\
make_array(pref, attrs, 0, (ref *)NULL)
#define make_empty_const_array(pref,attrs)\
make_const_array(pref, attrs, 0, (const ref *)NULL)
#define make_string(pref,attrs,size,chars)\
make_tasv(pref, t_string, attrs, size, bytes, chars)
#define make_const_string(pref,attrs,size,chars)\
make_tasv(pref, t_string, attrs, size, const_bytes, chars)
#define make_empty_string(pref,attrs)\
make_string(pref, attrs, 0, (byte *)NULL)
#define make_empty_const_string(pref,attrs)\
make_const_string(pref, attrs, 0, (const byte *)NULL)
#define make_struct(pref,attrs,ptr)\
make_tav(pref, t_struct, attrs, pstruct, (obj_header_t *)(ptr))
#define make_struct_new(pref,attrs,ptr)\
make_tav_new(pref, t_struct, attrs, pstruct, (obj_header_t *)(ptr))
#define make_astruct(pref,attrs,ptr)\
make_tav(pref, t_astruct, attrs, pstruct, (obj_header_t *)(ptr))
#define make_astruct_new(pref,attrs,ptr)\
make_tav_new(pref, t_astruct, attrs, pstruct, (obj_header_t *)(ptr))
#endif