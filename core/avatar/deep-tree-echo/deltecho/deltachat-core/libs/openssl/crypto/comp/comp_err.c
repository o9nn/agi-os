#include <stdio.h>
#include <openssl/err.h>
#include <openssl/comp.h>
#ifndef OPENSSL_NO_ERR
# define ERR_FUNC(func) ERR_PACK(ERR_LIB_COMP,func,0)
# define ERR_REASON(reason) ERR_PACK(ERR_LIB_COMP,0,reason)
static ERR_STRING_DATA COMP_str_functs[] = {
{ERR_FUNC(COMP_F_BIO_ZLIB_FLUSH), "BIO_ZLIB_FLUSH"},
{ERR_FUNC(COMP_F_BIO_ZLIB_NEW), "BIO_ZLIB_NEW"},
{ERR_FUNC(COMP_F_BIO_ZLIB_READ), "BIO_ZLIB_READ"},
{ERR_FUNC(COMP_F_BIO_ZLIB_WRITE), "BIO_ZLIB_WRITE"},
{0, NULL}
};
static ERR_STRING_DATA COMP_str_reasons[] = {
{ERR_REASON(COMP_R_ZLIB_DEFLATE_ERROR), "zlib deflate error"},
{ERR_REASON(COMP_R_ZLIB_INFLATE_ERROR), "zlib inflate error"},
{ERR_REASON(COMP_R_ZLIB_NOT_SUPPORTED), "zlib not supported"},
{0, NULL}
};
#endif
void ERR_load_COMP_strings(void)
{
#ifndef OPENSSL_NO_ERR
if (ERR_func_error_string(COMP_str_functs[0].error) == NULL) {
ERR_load_strings(0, COMP_str_functs);
ERR_load_strings(0, COMP_str_reasons);
}
#endif
}