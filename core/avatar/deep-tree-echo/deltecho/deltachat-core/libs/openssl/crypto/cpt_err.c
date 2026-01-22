#include <stdio.h>
#include <openssl/err.h>
#include <openssl/crypto.h>
#ifndef OPENSSL_NO_ERR
# define ERR_FUNC(func) ERR_PACK(ERR_LIB_CRYPTO,func,0)
# define ERR_REASON(reason) ERR_PACK(ERR_LIB_CRYPTO,0,reason)
static ERR_STRING_DATA CRYPTO_str_functs[] = {
{ERR_FUNC(CRYPTO_F_CRYPTO_GET_EX_NEW_INDEX), "CRYPTO_get_ex_new_index"},
{ERR_FUNC(CRYPTO_F_CRYPTO_GET_NEW_DYNLOCKID), "CRYPTO_get_new_dynlockid"},
{ERR_FUNC(CRYPTO_F_CRYPTO_GET_NEW_LOCKID), "CRYPTO_get_new_lockid"},
{ERR_FUNC(CRYPTO_F_CRYPTO_SET_EX_DATA), "CRYPTO_set_ex_data"},
{ERR_FUNC(CRYPTO_F_DEF_ADD_INDEX), "DEF_ADD_INDEX"},
{ERR_FUNC(CRYPTO_F_DEF_GET_CLASS), "DEF_GET_CLASS"},
{ERR_FUNC(CRYPTO_F_FIPS_MODE_SET), "FIPS_mode_set"},
{ERR_FUNC(CRYPTO_F_INT_DUP_EX_DATA), "INT_DUP_EX_DATA"},
{ERR_FUNC(CRYPTO_F_INT_FREE_EX_DATA), "INT_FREE_EX_DATA"},
{ERR_FUNC(CRYPTO_F_INT_NEW_EX_DATA), "INT_NEW_EX_DATA"},
{0, NULL}
};
static ERR_STRING_DATA CRYPTO_str_reasons[] = {
{ERR_REASON(CRYPTO_R_FIPS_MODE_NOT_SUPPORTED), "fips mode not supported"},
{ERR_REASON(CRYPTO_R_NO_DYNLOCK_CREATE_CALLBACK),
"no dynlock create callback"},
{0, NULL}
};
#endif
void ERR_load_CRYPTO_strings(void)
{
#ifndef OPENSSL_NO_ERR
if (ERR_func_error_string(CRYPTO_str_functs[0].error) == NULL) {
ERR_load_strings(0, CRYPTO_str_functs);
ERR_load_strings(0, CRYPTO_str_reasons);
}
#endif
}