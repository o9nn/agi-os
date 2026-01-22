#include <stdio.h>
#include <openssl/err.h>
#include <openssl/dh.h>
#ifndef OPENSSL_NO_ERR
# define ERR_FUNC(func) ERR_PACK(ERR_LIB_DH,func,0)
# define ERR_REASON(reason) ERR_PACK(ERR_LIB_DH,0,reason)
static ERR_STRING_DATA DH_str_functs[] = {
{ERR_FUNC(DH_F_COMPUTE_KEY), "COMPUTE_KEY"},
{ERR_FUNC(DH_F_DHPARAMS_PRINT_FP), "DHparams_print_fp"},
{ERR_FUNC(DH_F_DH_BUILTIN_GENPARAMS), "DH_BUILTIN_GENPARAMS"},
{ERR_FUNC(DH_F_DH_COMPUTE_KEY), "DH_compute_key"},
{ERR_FUNC(DH_F_DH_GENERATE_KEY), "DH_generate_key"},
{ERR_FUNC(DH_F_DH_GENERATE_PARAMETERS_EX), "DH_generate_parameters_ex"},
{ERR_FUNC(DH_F_DH_NEW_METHOD), "DH_new_method"},
{ERR_FUNC(DH_F_DH_PARAM_DECODE), "DH_PARAM_DECODE"},
{ERR_FUNC(DH_F_DH_PRIV_DECODE), "DH_PRIV_DECODE"},
{ERR_FUNC(DH_F_DH_PRIV_ENCODE), "DH_PRIV_ENCODE"},
{ERR_FUNC(DH_F_DH_PUB_DECODE), "DH_PUB_DECODE"},
{ERR_FUNC(DH_F_DH_PUB_ENCODE), "DH_PUB_ENCODE"},
{ERR_FUNC(DH_F_DO_DH_PRINT), "DO_DH_PRINT"},
{ERR_FUNC(DH_F_GENERATE_KEY), "GENERATE_KEY"},
{ERR_FUNC(DH_F_GENERATE_PARAMETERS), "GENERATE_PARAMETERS"},
{ERR_FUNC(DH_F_PKEY_DH_DERIVE), "PKEY_DH_DERIVE"},
{ERR_FUNC(DH_F_PKEY_DH_KEYGEN), "PKEY_DH_KEYGEN"},
{0, NULL}
};
static ERR_STRING_DATA DH_str_reasons[] = {
{ERR_REASON(DH_R_BAD_GENERATOR), "bad generator"},
{ERR_REASON(DH_R_BN_DECODE_ERROR), "bn decode error"},
{ERR_REASON(DH_R_BN_ERROR), "bn error"},
{ERR_REASON(DH_R_DECODE_ERROR), "decode error"},
{ERR_REASON(DH_R_INVALID_PUBKEY), "invalid public key"},
{ERR_REASON(DH_R_KEYS_NOT_SET), "keys not set"},
{ERR_REASON(DH_R_KEY_SIZE_TOO_SMALL), "key size too small"},
{ERR_REASON(DH_R_MODULUS_TOO_LARGE), "modulus too large"},
{ERR_REASON(DH_R_NON_FIPS_METHOD), "non fips method"},
{ERR_REASON(DH_R_NO_PARAMETERS_SET), "no parameters set"},
{ERR_REASON(DH_R_NO_PRIVATE_VALUE), "no private value"},
{ERR_REASON(DH_R_PARAMETER_ENCODING_ERROR), "parameter encoding error"},
{0, NULL}
};
#endif
void ERR_load_DH_strings(void)
{
#ifndef OPENSSL_NO_ERR
if (ERR_func_error_string(DH_str_functs[0].error) == NULL) {
ERR_load_strings(0, DH_str_functs);
ERR_load_strings(0, DH_str_reasons);
}
#endif
}