#ifndef szlibx_INCLUDED
# define szlibx_INCLUDED
typedef struct zlib_dynamic_state_s zlib_dynamic_state_t;
typedef struct stream_zlib_state_s {
stream_state_common;
int windowBits;
bool no_wrapper;
int level;
int method;
int memLevel;
int strategy;
zlib_dynamic_state_t *dynamic;
} stream_zlib_state;
extern_st(st_zlib_state);
#define public_st_zlib_state() \
gs_public_st_ptrs1(st_zlib_state, stream_zlib_state,\
"zlibEncode/Decode state", zlib_state_enum_ptrs, zlib_state_reloc_ptrs,\
dynamic)
extern const stream_template s_zlibD_template;
extern const stream_template s_zlibE_template;
stream_proc_set_defaults(s_zlib_set_defaults);
#endif