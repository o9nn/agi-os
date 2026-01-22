#ifndef gsparam_INCLUDED
#  define gsparam_INCLUDED
#include "gsstype.h"
#ifndef gs_param_list_DEFINED
#  define gs_param_list_DEFINED
typedef struct gs_param_list_s gs_param_list;
#endif
typedef const char *gs_param_name;
typedef enum {
gs_param_type_null, gs_param_type_bool, gs_param_type_int,
gs_param_type_long, gs_param_type_float,
gs_param_type_string, gs_param_type_name,
gs_param_type_int_array, gs_param_type_float_array,
gs_param_type_string_array, gs_param_type_name_array,
gs_param_type_dict, gs_param_type_dict_int_keys, gs_param_type_array
} gs_param_type;
#define gs_param_type_any ((gs_param_type)-1)
#define _param_array_struct(sname,etype)\
struct sname { const etype *data; uint size; bool persistent; }
typedef _param_array_struct(gs_param_int_array_s, int) gs_param_int_array;
typedef _param_array_struct(gs_param_float_array_s, float) gs_param_float_array;
typedef _param_array_struct(gs_param_string_array_s, gs_param_string) gs_param_string_array;
#define param_string_from_string(ps, str)\
((ps).data = (const byte *)(str),\
(ps).size = strlen((const char *)(ps).data),\
(ps).persistent = true)
#define param_string_from_transient_string(ps, str)\
((ps).data = (const byte *)(str),\
(ps).size = strlen((const char *)(ps).data),\
(ps).persistent = false)
typedef struct gs_param_collection_s {
gs_param_list *list;
uint size;
} gs_param_collection;
typedef gs_param_collection gs_param_dict;
typedef gs_param_collection gs_param_array;
#define GS_PARAM_TYPE_SIZES(dict_size)\
0, sizeof(bool), sizeof(int), sizeof(long), sizeof(float),\
sizeof(gs_param_string), sizeof(gs_param_string),\
sizeof(gs_param_int_array), sizeof(gs_param_float_array),\
sizeof(gs_param_string_array), sizeof(gs_param_string_array),\
(dict_size), (dict_size), (dict_size)
#define GS_PARAM_TYPE_BASE_SIZES(dict_elt_size)\
0, sizeof(bool), sizeof(int), sizeof(long), sizeof(float),\
1, 1, sizeof(int), sizeof(float),\
sizeof(gs_param_string), sizeof(gs_param_string),\
(dict_elt_size), (dict_elt_size), (dict_elt_size)
extern const byte gs_param_type_sizes[];
extern const byte gs_param_type_base_sizes[];
#define GS_PARAM_VALUE_UNION(dict_type)\
bool b;\
int i;\
long l;\
float f;\
gs_param_string s;\
gs_param_string n;\
gs_param_int_array ia;\
gs_param_float_array fa;\
gs_param_string_array sa;\
gs_param_string_array na;\
dict_type d
typedef union gs_param_value_s {
GS_PARAM_VALUE_UNION(gs_param_collection);
} gs_param_value;
typedef struct gs_param_typed_value_s {
gs_param_value value;
gs_param_type type;
} gs_param_typed_value;
struct_proc_enum_ptrs(gs_param_typed_value_enum_ptrs);
struct_proc_reloc_ptrs(gs_param_typed_value_reloc_ptrs);
#define gs_param_typed_value_max_ptrs 1
typedef enum {
gs_param_collection_dict_any = 0,
gs_param_collection_dict_int_keys = 1,
gs_param_collection_array = 2
} gs_param_collection_type_t;
#define gs_param_policy_signal_error 0
#define gs_param_policy_ignore 1
#define gs_param_policy_consult_user 2
typedef union gs_param_enumerator_s {
int intval;
long longval;
void *pvoid;
char *pchar;
} gs_param_enumerator_t;
typedef gs_param_string gs_param_key_t;
typedef struct gs_param_list_procs_s {
#define param_proc_xmit_typed(proc)\
int proc(gs_param_list *, gs_param_name, gs_param_typed_value *)
param_proc_xmit_typed((*xmit_typed));
#define param_write_typed(plist, pkey, pvalue)\
(*(plist)->procs->xmit_typed)(plist, pkey, pvalue)
#define param_proc_begin_xmit_collection(proc)\
int proc(gs_param_list *, gs_param_name, gs_param_dict *,\
gs_param_collection_type_t)
param_proc_begin_xmit_collection((*begin_xmit_collection));
#define param_begin_read_collection(plist, pkey, pvalue, coll_type)\
(*(plist)->procs->begin_xmit_collection)(plist, pkey, pvalue, coll_type)
#define param_begin_read_dict(l, k, v, int_keys)\
param_begin_read_collection(l, k, v,\
(int_keys ? gs_param_collection_dict_int_keys :\
gs_param_collection_dict_any))
#define param_begin_write_collection(plist, pkey, pvalue, coll_type)\
(*(plist)->procs->begin_xmit_collection)(plist, pkey, pvalue, coll_type)
#define param_begin_write_dict(l, k, v, int_keys)\
param_begin_write_collection(l, k, v,\
(int_keys ? gs_param_collection_dict_int_keys :\
gs_param_collection_dict_any))
#define param_proc_end_xmit_collection(proc)\
int proc(gs_param_list *, gs_param_name, gs_param_dict *)
param_proc_end_xmit_collection((*end_xmit_collection));
#define param_end_read_collection(plist, pkey, pvalue)\
(*(plist)->procs->end_xmit_collection)(plist, pkey, pvalue)
#define param_end_read_dict(l, k, v) param_end_read_collection(l, k, v)
#define param_end_write_collection(plist, pkey, pvalue)\
(*(plist)->procs->end_xmit_collection)(plist, pkey, pvalue)
#define param_end_write_dict(l, k, v) param_end_write_collection(l, k, v)
#define param_proc_next_key(proc)\
int proc(gs_param_list *, gs_param_enumerator_t *, gs_param_key_t *)
param_proc_next_key((*next_key));
#define param_get_next_key(plist, penum, pkey)\
(*(plist)->procs->next_key)(plist, penum, pkey)
#define param_proc_request(proc)\
int proc(gs_param_list *, gs_param_name)
param_proc_request((*request));
#define param_request(plist, pkey)\
((plist)->procs->request(plist, pkey))
#define param_proc_requested(proc)\
int proc(const gs_param_list *, gs_param_name)
param_proc_requested((*requested));
#define param_requested(plist, pkey)\
(*(plist)->procs->requested)(plist, pkey)
#define param_proc_get_policy(proc)\
int proc(gs_param_list *, gs_param_name)
param_proc_get_policy((*get_policy));
#define param_get_policy(plist, pkey)\
(*(plist)->procs->get_policy)(plist, pkey)
#define param_proc_signal_error(proc)\
int proc(gs_param_list *, gs_param_name, int)
param_proc_signal_error((*signal_error));
#define param_signal_error(plist, pkey, code)\
(*(plist)->procs->signal_error)(plist, pkey, code)
#define param_return_error(plist, pkey, code)\
return_error(param_signal_error(plist, pkey, code))
#define param_proc_commit(proc)\
int proc(gs_param_list *)
param_proc_commit((*commit));
#define param_commit(plist)\
(*(plist)->procs->commit)(plist)
} gs_param_list_procs;
int param_read_requested_typed(gs_param_list *, gs_param_name,
gs_param_typed_value *);
#define param_read_typed(plist, pkey, pvalue)\
((pvalue)->type = gs_param_type_any,\
param_read_requested_typed(plist, pkey, pvalue))
int param_read_null(gs_param_list *, gs_param_name);
int param_write_null(gs_param_list *, gs_param_name);
int param_read_bool(gs_param_list *, gs_param_name, bool *);
int param_write_bool(gs_param_list *, gs_param_name, const bool *);
int param_read_int(gs_param_list *, gs_param_name, int *);
int param_write_int(gs_param_list *, gs_param_name, const int *);
int param_read_long(gs_param_list *, gs_param_name, long *);
int param_write_long(gs_param_list *, gs_param_name, const long *);
int param_read_float(gs_param_list *, gs_param_name, float *);
int param_write_float(gs_param_list *, gs_param_name, const float *);
int param_read_string(gs_param_list *, gs_param_name, gs_param_string *);
int param_write_string(gs_param_list *, gs_param_name,
const gs_param_string *);
int param_read_name(gs_param_list *, gs_param_name, gs_param_string *);
int param_write_name(gs_param_list *, gs_param_name,
const gs_param_string *);
int param_read_int_array(gs_param_list *, gs_param_name,
gs_param_int_array *);
int param_write_int_array(gs_param_list *, gs_param_name,
const gs_param_int_array *);
int param_write_int_values(gs_param_list *, gs_param_name,
const int *, uint, bool);
int param_read_float_array(gs_param_list *, gs_param_name,
gs_param_float_array *);
int param_write_float_array(gs_param_list *, gs_param_name,
const gs_param_float_array *);
int param_write_float_values(gs_param_list *, gs_param_name,
const float *, uint, bool);
int param_read_string_array(gs_param_list *, gs_param_name,
gs_param_string_array *);
int param_write_string_array(gs_param_list *, gs_param_name,
const gs_param_string_array *);
int param_read_name_array(gs_param_list *, gs_param_name,
gs_param_string_array *);
int param_write_name_array(gs_param_list *, gs_param_name,
const gs_param_string_array *);
#define gs_param_list_common\
const gs_param_list_procs *procs;\
gs_memory_t *memory;	\
bool persistent_keys
struct gs_param_list_s {
gs_param_list_common;
};
#define gs_param_list_set_persistent_keys gs_param_list_set_persist_keys
void gs_param_list_set_persistent_keys(gs_param_list *, bool);
void param_init_enumerator(gs_param_enumerator_t * penum);
typedef struct gs_param_item_s {
const char *key;
byte  type;
short offset;
} gs_param_item_t;
#define gs_param_item_end { 0 }
int gs_param_read_items(gs_param_list * plist, void *obj,
const gs_param_item_t * items);
int gs_param_write_items(gs_param_list * plist, const void *obj,
const void *default_obj,
const gs_param_item_t * items);
void gs_param_list_init(gs_param_list *, const gs_param_list_procs *,
gs_memory_t *);
int param_coerce_typed(gs_param_typed_value * pvalue,
gs_param_type req_type, gs_memory_t * mem);
param_proc_request(gs_param_request_default);
param_proc_requested(gs_param_requested_default);
typedef struct gs_c_param_s gs_c_param;
typedef struct gs_c_param_list_s {
gs_param_list_common;
gs_c_param *head;
gs_param_list *target;
uint count;
bool any_requested;
gs_param_collection_type_t coll_type;
} gs_c_param_list;
#define private_st_c_param_list()	\
gs_private_st_ptrs2(st_c_param_list, gs_c_param_list, "c_param_list",\
c_param_list_enum_ptrs, c_param_list_reloc_ptrs, head, target)
#define private_st_gs_param_string()	\
gs_private_st_composite(st_gs_param_string, gs_param_string, "gs_param_string",\
param_string_enum_ptrs, param_string_reloc_ptrs)
void gs_c_param_list_set_target(gs_c_param_list *, gs_param_list *);
gs_c_param_list *gs_c_param_list_alloc(gs_memory_t *, client_name_t);
void gs_c_param_list_write(gs_c_param_list *, gs_memory_t *);
void gs_c_param_list_write_more(gs_c_param_list *);
void gs_c_param_list_read(gs_c_param_list *);
void gs_c_param_list_release(gs_c_param_list *);
#endif