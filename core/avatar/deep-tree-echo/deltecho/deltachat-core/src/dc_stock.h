#ifndef __DC_STOCK_H__
#define __DC_STOCK_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <stdlib.h>
#include <string.h>
char* dc_stock_str (dc_context_t*, int id);
char* dc_stock_str_repl_string (dc_context_t*, int id, const char* value);
char* dc_stock_str_repl_int    (dc_context_t*, int id, int value);
char* dc_stock_str_repl_string2 (dc_context_t*, int id, const char*, const char*);
char* dc_stock_system_msg(dc_context_t* context, int str_id,
const char* param1, const char* param2,
uint32_t from_id);
#ifdef __cplusplus
}
#endif
#endif