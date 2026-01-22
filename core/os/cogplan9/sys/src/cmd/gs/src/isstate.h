#ifndef isstate_INCLUDED
# define isstate_INCLUDED
struct alloc_save_s {
gs_ref_memory_t state;
vm_spaces spaces;
bool restore_names;
bool is_current;
ulong id;
void *client_data;
};
#define private_st_alloc_save() \
gs_private_st_suffix_add1(st_alloc_save, alloc_save_t, "alloc_save",\
save_enum_ptrs, save_reloc_ptrs, st_ref_memory, client_data)
#endif