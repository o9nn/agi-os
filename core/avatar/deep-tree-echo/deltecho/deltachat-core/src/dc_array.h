#ifndef __DC_ARRAY_H__
#define __DC_ARRAY_H__
#ifdef __cplusplus
extern "C" {
#endif
struct _dc_array
{
uint32_t magic;
dc_context_t* context;
size_t allocated;
size_t count;
int type;
uintptr_t* array;
};
dc_array_t* dc_array_new (dc_context_t*, size_t initsize);
dc_array_t* dc_array_new_typed (dc_context_t*, int type, size_t initsize);
void dc_array_empty (dc_array_t*);
void dc_array_free_ptr (dc_array_t*);
dc_array_t* dc_array_duplicate (const dc_array_t*);
void dc_array_sort_ids (dc_array_t*);
void dc_array_sort_strings (dc_array_t*);
char* dc_array_get_string (const dc_array_t*, const char* sep);
char* dc_arr_to_string (const uint32_t* arr, int cnt);
#ifdef __cplusplus
}
#endif
#endif