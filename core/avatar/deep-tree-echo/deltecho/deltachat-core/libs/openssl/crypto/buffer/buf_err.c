#include <stdio.h>
#include <openssl/err.h>
#include <openssl/buffer.h>
#ifndef OPENSSL_NO_ERR
# define ERR_FUNC(func) ERR_PACK(ERR_LIB_BUF,func,0)
# define ERR_REASON(reason) ERR_PACK(ERR_LIB_BUF,0,reason)
static ERR_STRING_DATA BUF_str_functs[] = {
{ERR_FUNC(BUF_F_BUF_MEMDUP), "BUF_memdup"},
{ERR_FUNC(BUF_F_BUF_MEM_GROW), "BUF_MEM_grow"},
{ERR_FUNC(BUF_F_BUF_MEM_GROW_CLEAN), "BUF_MEM_grow_clean"},
{ERR_FUNC(BUF_F_BUF_MEM_NEW), "BUF_MEM_new"},
{ERR_FUNC(BUF_F_BUF_STRDUP), "BUF_strdup"},
{ERR_FUNC(BUF_F_BUF_STRNDUP), "BUF_strndup"},
{0, NULL}
};
static ERR_STRING_DATA BUF_str_reasons[] = {
{0, NULL}
};
#endif
void ERR_load_BUF_strings(void)
{
#ifndef OPENSSL_NO_ERR
if (ERR_func_error_string(BUF_str_functs[0].error) == NULL) {
ERR_load_strings(0, BUF_str_functs);
ERR_load_strings(0, BUF_str_reasons);
}
#endif
}