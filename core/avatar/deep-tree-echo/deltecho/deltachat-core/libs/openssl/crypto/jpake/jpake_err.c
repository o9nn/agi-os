#include <stdio.h>
#include <openssl/err.h>
#include <openssl/jpake.h>
#ifndef OPENSSL_NO_ERR
# define ERR_FUNC(func) ERR_PACK(ERR_LIB_JPAKE,func,0)
# define ERR_REASON(reason) ERR_PACK(ERR_LIB_JPAKE,0,reason)
static ERR_STRING_DATA JPAKE_str_functs[] = {
{ERR_FUNC(JPAKE_F_JPAKE_STEP1_PROCESS), "JPAKE_STEP1_process"},
{ERR_FUNC(JPAKE_F_JPAKE_STEP2_PROCESS), "JPAKE_STEP2_process"},
{ERR_FUNC(JPAKE_F_JPAKE_STEP3A_PROCESS), "JPAKE_STEP3A_process"},
{ERR_FUNC(JPAKE_F_JPAKE_STEP3B_PROCESS), "JPAKE_STEP3B_process"},
{ERR_FUNC(JPAKE_F_VERIFY_ZKP), "VERIFY_ZKP"},
{0, NULL}
};
static ERR_STRING_DATA JPAKE_str_reasons[] = {
{ERR_REASON(JPAKE_R_G_TO_THE_X3_IS_NOT_LEGAL),
"g to the x3 is not legal"},
{ERR_REASON(JPAKE_R_G_TO_THE_X4_IS_NOT_LEGAL),
"g to the x4 is not legal"},
{ERR_REASON(JPAKE_R_G_TO_THE_X4_IS_ONE), "g to the x4 is one"},
{ERR_REASON(JPAKE_R_HASH_OF_HASH_OF_KEY_MISMATCH),
"hash of hash of key mismatch"},
{ERR_REASON(JPAKE_R_HASH_OF_KEY_MISMATCH), "hash of key mismatch"},
{ERR_REASON(JPAKE_R_VERIFY_B_FAILED), "verify b failed"},
{ERR_REASON(JPAKE_R_VERIFY_X3_FAILED), "verify x3 failed"},
{ERR_REASON(JPAKE_R_VERIFY_X4_FAILED), "verify x4 failed"},
{ERR_REASON(JPAKE_R_ZKP_VERIFY_FAILED), "zkp verify failed"},
{0, NULL}
};
#endif
void ERR_load_JPAKE_strings(void)
{
#ifndef OPENSSL_NO_ERR
if (ERR_func_error_string(JPAKE_str_functs[0].error) == NULL) {
ERR_load_strings(0, JPAKE_str_functs);
ERR_load_strings(0, JPAKE_str_reasons);
}
#endif
}