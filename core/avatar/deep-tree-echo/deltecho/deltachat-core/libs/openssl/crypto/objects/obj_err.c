#include <stdio.h>
#include <openssl/err.h>
#include <openssl/objects.h>
#ifndef OPENSSL_NO_ERR
# define ERR_FUNC(func) ERR_PACK(ERR_LIB_OBJ,func,0)
# define ERR_REASON(reason) ERR_PACK(ERR_LIB_OBJ,0,reason)
static ERR_STRING_DATA OBJ_str_functs[] = {
{ERR_FUNC(OBJ_F_OBJ_ADD_OBJECT), "OBJ_add_object"},
{ERR_FUNC(OBJ_F_OBJ_CREATE), "OBJ_create"},
{ERR_FUNC(OBJ_F_OBJ_DUP), "OBJ_dup"},
{ERR_FUNC(OBJ_F_OBJ_NAME_NEW_INDEX), "OBJ_NAME_new_index"},
{ERR_FUNC(OBJ_F_OBJ_NID2LN), "OBJ_nid2ln"},
{ERR_FUNC(OBJ_F_OBJ_NID2OBJ), "OBJ_nid2obj"},
{ERR_FUNC(OBJ_F_OBJ_NID2SN), "OBJ_nid2sn"},
{0, NULL}
};
static ERR_STRING_DATA OBJ_str_reasons[] = {
{ERR_REASON(OBJ_R_MALLOC_FAILURE), "malloc failure"},
{ERR_REASON(OBJ_R_UNKNOWN_NID), "unknown nid"},
{0, NULL}
};
#endif
void ERR_load_OBJ_strings(void)
{
#ifndef OPENSSL_NO_ERR
if (ERR_func_error_string(OBJ_str_functs[0].error) == NULL) {
ERR_load_strings(0, OBJ_str_functs);
ERR_load_strings(0, OBJ_str_reasons);
}
#endif
}