#include <stdio.h>
#include <openssl/err.h>
#include <openssl/ui.h>
#ifndef OPENSSL_NO_ERR
# define ERR_FUNC(func) ERR_PACK(ERR_LIB_UI,func,0)
# define ERR_REASON(reason) ERR_PACK(ERR_LIB_UI,0,reason)
static ERR_STRING_DATA UI_str_functs[] = {
{ERR_FUNC(UI_F_GENERAL_ALLOCATE_BOOLEAN), "GENERAL_ALLOCATE_BOOLEAN"},
{ERR_FUNC(UI_F_GENERAL_ALLOCATE_PROMPT), "GENERAL_ALLOCATE_PROMPT"},
{ERR_FUNC(UI_F_GENERAL_ALLOCATE_STRING), "GENERAL_ALLOCATE_STRING"},
{ERR_FUNC(UI_F_UI_CTRL), "UI_ctrl"},
{ERR_FUNC(UI_F_UI_DUP_ERROR_STRING), "UI_dup_error_string"},
{ERR_FUNC(UI_F_UI_DUP_INFO_STRING), "UI_dup_info_string"},
{ERR_FUNC(UI_F_UI_DUP_INPUT_BOOLEAN), "UI_dup_input_boolean"},
{ERR_FUNC(UI_F_UI_DUP_INPUT_STRING), "UI_dup_input_string"},
{ERR_FUNC(UI_F_UI_DUP_VERIFY_STRING), "UI_dup_verify_string"},
{ERR_FUNC(UI_F_UI_GET0_RESULT), "UI_get0_result"},
{ERR_FUNC(UI_F_UI_NEW_METHOD), "UI_new_method"},
{ERR_FUNC(UI_F_UI_SET_RESULT), "UI_set_result"},
{0, NULL}
};
static ERR_STRING_DATA UI_str_reasons[] = {
{ERR_REASON(UI_R_COMMON_OK_AND_CANCEL_CHARACTERS),
"common ok and cancel characters"},
{ERR_REASON(UI_R_INDEX_TOO_LARGE), "index too large"},
{ERR_REASON(UI_R_INDEX_TOO_SMALL), "index too small"},
{ERR_REASON(UI_R_NO_RESULT_BUFFER), "no result buffer"},
{ERR_REASON(UI_R_RESULT_TOO_LARGE), "result too large"},
{ERR_REASON(UI_R_RESULT_TOO_SMALL), "result too small"},
{ERR_REASON(UI_R_UNKNOWN_CONTROL_COMMAND), "unknown control command"},
{0, NULL}
};
#endif
void ERR_load_UI_strings(void)
{
#ifndef OPENSSL_NO_ERR
if (ERR_func_error_string(UI_str_functs[0].error) == NULL) {
ERR_load_strings(0, UI_str_functs);
ERR_load_strings(0, UI_str_reasons);
}
#endif
}