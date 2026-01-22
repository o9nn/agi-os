#ifndef iname_INCLUDED
#  define iname_INCLUDED
#include "inames.h"
#define name_memory(mem)\
names_memory(mem->gs_lib_ctx->gs_name_table)
#define name_ref(mem, ptr, size, pnref, enterflag)\
names_ref(mem->gs_lib_ctx->gs_name_table, ptr, size, pnref, enterflag)
#define name_string_ref(mem, pnref, psref)\
names_string_ref(mem->gs_lib_ctx->gs_name_table, pnref, psref)
#define name_enter_string(mem, str, pnref)\
names_enter_string(mem->gs_lib_ctx->gs_name_table, str, pnref)
#define name_from_string(mem, psref, pnref)\
names_from_string(mem->gs_lib_ctx->gs_name_table, psref, pnref)
#define name_eq(pnref1, pnref2)\
names_eq(pnref1, pnref2)
#define name_invalidate_value_cache(mem, pnref)\
names_invalidate_value_cache(mem->gs_lib_ctx->gs_name_table, pnref)
#define name_index(mem, pnref)		\
names_index(mem->gs_lib_ctx->gs_name_table, pnref)
#define name_index_ptr(mem, nidx)		\
names_index_ptr(mem->gs_lib_ctx->gs_name_table, nidx)
#define name_index_ref(mem, nidx, pnref)	\
names_index_ref(mem->gs_lib_ctx->gs_name_table, nidx, pnref)
#define name_next_valid_index(mem, nidx)\
names_next_valid_index(mem->gs_lib_ctx->gs_name_table, nidx)
#define name_mark_index(mem, nidx)\
names_mark_index(mem->gs_lib_ctx->gs_name_table, nidx)
#define name_ref_sub_table(mem, pnref)\
names_ref_sub_table(mem->gs_lib_ctx->gs_name_table, pnref)
#endif