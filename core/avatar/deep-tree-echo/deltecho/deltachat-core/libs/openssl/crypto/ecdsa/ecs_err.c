#include <stdio.h>
#include <openssl/err.h>
#include <openssl/ecdsa.h>
#ifndef OPENSSL_NO_ERR
# define ERR_FUNC(func) ERR_PACK(ERR_LIB_ECDSA,func,0)
# define ERR_REASON(reason) ERR_PACK(ERR_LIB_ECDSA,0,reason)
static ERR_STRING_DATA ECDSA_str_functs[] = {
{ERR_FUNC(ECDSA_F_ECDSA_CHECK), "ECDSA_CHECK"},
{ERR_FUNC(ECDSA_F_ECDSA_DATA_NEW_METHOD), "ECDSA_DATA_NEW_METHOD"},
{ERR_FUNC(ECDSA_F_ECDSA_DO_SIGN), "ECDSA_do_sign"},
{ERR_FUNC(ECDSA_F_ECDSA_DO_VERIFY), "ECDSA_do_verify"},
{ERR_FUNC(ECDSA_F_ECDSA_SIGN_SETUP), "ECDSA_sign_setup"},
{0, NULL}
};
static ERR_STRING_DATA ECDSA_str_reasons[] = {
{ERR_REASON(ECDSA_R_BAD_SIGNATURE), "bad signature"},
{ERR_REASON(ECDSA_R_DATA_TOO_LARGE_FOR_KEY_SIZE),
"data too large for key size"},
{ERR_REASON(ECDSA_R_ERR_EC_LIB), "err ec lib"},
{ERR_REASON(ECDSA_R_MISSING_PARAMETERS), "missing parameters"},
{ERR_REASON(ECDSA_R_NEED_NEW_SETUP_VALUES), "need new setup values"},
{ERR_REASON(ECDSA_R_NON_FIPS_METHOD), "non fips method"},
{ERR_REASON(ECDSA_R_RANDOM_NUMBER_GENERATION_FAILED),
"random number generation failed"},
{ERR_REASON(ECDSA_R_SIGNATURE_MALLOC_FAILED), "signature malloc failed"},
{0, NULL}
};
#endif
void ERR_load_ECDSA_strings(void)
{
#ifndef OPENSSL_NO_ERR
if (ERR_func_error_string(ECDSA_str_functs[0].error) == NULL) {
ERR_load_strings(0, ECDSA_str_functs);
ERR_load_strings(0, ECDSA_str_reasons);
}
#endif
}