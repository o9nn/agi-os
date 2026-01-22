#ifndef btoken_INCLUDED
#  define btoken_INCLUDED
#define system_names_p (gs_imemory.space_global->names_array)
#define user_names_p (gs_imemory.space_local->names_array)
int create_names_array(ref **ppnames, gs_memory_t *mem,
client_name_t cname);
int encode_binary_token(i_ctx_t *i_ctx_p, const ref *obj, long *ref_offset,
long *char_offset, byte *str);
#define ref_binary_object_format_container i_ctx_p
#define ref_binary_object_format\
(ref_binary_object_format_container->binary_object_format)
#endif