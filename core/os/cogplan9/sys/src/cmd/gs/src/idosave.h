#ifndef idosave_INCLUDED
#  define idosave_INCLUDED
int alloc_save_change(gs_dual_memory_t *dmem, const ref *pcont,
ref_packed *ptr, client_name_t cname);
int alloc_save_change_in(gs_ref_memory_t *mem, const ref *pcont,
ref_packed *ptr, client_name_t cname);
#endif