#ifndef iparam_INCLUDED
#  define iparam_INCLUDED
#include "gsparam.h"
typedef struct iparam_loc_s {
ref *pvalue;
int *presult;
} iparam_loc;
#define iparam_list_common\
gs_param_list_common;\
gs_ref_memory_t *ref_memory; \
union {\
struct {	\
int (*read)(iparam_list *, const ref *, iparam_loc *);\
ref policies;	\
bool require_all;	\
} r;\
struct {		\
int (*write)(iparam_list *, const ref *, const ref *);\
ref wanted;		\
} w;\
} u;\
int (*enumerate)(iparam_list *, gs_param_enumerator_t *, gs_param_key_t *, ref_type *);\
int *results;		\
uint count;		\
bool int_keys
typedef struct iparam_list_s iparam_list;
struct iparam_list_s {
iparam_list_common;
};
typedef struct dict_param_list_s {
iparam_list_common;
ref dict;
} dict_param_list;
typedef struct array_param_list_s {
iparam_list_common;
ref *bot;
ref *top;
} array_param_list;
typedef struct stack_param_list_s {
iparam_list_common;
ref_stack_t *pstack;
uint skip;
} stack_param_list;
int dict_param_list_read(dict_param_list *, const ref *  ,
const ref *, bool, gs_ref_memory_t *);
int dict_param_list_write(dict_param_list *, ref *  ,
const ref *, gs_ref_memory_t *);
int array_indexed_param_list_read(dict_param_list *, const ref *  ,
const ref *, bool, gs_ref_memory_t *);
int array_indexed_param_list_write(dict_param_list *, ref *  ,
const ref *, gs_ref_memory_t *);
int array_param_list_read(array_param_list *, ref *, uint,
const ref *, bool, gs_ref_memory_t *);
int stack_param_list_read(stack_param_list *, ref_stack_t *, uint,
const ref *, bool, gs_ref_memory_t *);
int stack_param_list_write(stack_param_list *, ref_stack_t *,
const ref *, gs_ref_memory_t *);
#define iparam_list_release(plist)\
gs_free_object((plist)->memory, (plist)->results, "iparam_list_release")
#endif