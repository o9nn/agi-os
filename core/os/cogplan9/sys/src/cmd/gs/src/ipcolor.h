#ifndef ipcolor_INCLUDED
#  define ipcolor_INCLUDED
typedef struct int_pattern_s {
ref dict;
} int_pattern;
#define private_st_int_pattern()	\
gs_private_st_ref_struct(st_int_pattern, int_pattern, "int_pattern")
int int_pattern_alloc(int_pattern **ppdata, const ref *op,
gs_memory_t *mem);
#endif