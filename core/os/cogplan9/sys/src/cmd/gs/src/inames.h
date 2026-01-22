#ifndef inames_INCLUDED
# define inames_INCLUDED
#ifndef name_table_DEFINED
# define name_table_DEFINED
typedef struct name_table_s name_table;
#endif
typedef uint name_index_t;
extern const uint name_max_string;
#ifndef gs_ref_memory_DEFINED
# define gs_ref_memory_DEFINED
typedef struct gs_ref_memory_s gs_ref_memory_t;
#endif
name_table *names_init(ulong size, gs_ref_memory_t *imem);
gs_memory_t *names_memory(const name_table * nt);
int names_ref(name_table * nt, const byte * ptr, uint size, ref * pnref,
int enterflag);
void names_string_ref(const name_table * nt, const ref * pnref, ref * psref);
int names_enter_string(name_table * nt, const char *str, ref * pnref);
int names_from_string(name_table * nt, const ref * psref, ref * pnref);
#define names_eq(pnref1, pnref2)\
((pnref1)->value.pname == (pnref2)->value.pname)
void names_invalidate_value_cache(name_table * nt, const ref * pnref);
name_index_t names_index(const name_table * nt, const ref * pnref);
name *names_index_ptr(const name_table * nt, name_index_t nidx);
void names_index_ref(const name_table * nt, name_index_t nidx, ref * pnref);
name_index_t names_next_valid_index(name_table * nt, name_index_t nidx);
bool names_mark_index(name_table * nt, name_index_t nidx);
void *
names_ref_sub_table(name_table * nt, const ref * pnref);
void *
names_index_sub_table(name_table * nt, name_index_t nidx);
void *
names_index_string_sub_table(name_table * nt, name_index_t nidx);
#endif