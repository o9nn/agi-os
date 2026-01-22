#ifndef spsdf_INCLUDED
#  define spsdf_INCLUDED
#include "gsparam.h"
#ifndef stream_DEFINED
#  define stream_DEFINED
typedef struct stream_s stream;
#endif
#define PRINT_BINARY_OK 1
#define PRINT_ASCII85_OK 2
#define PRINT_HEX_NOT_OK 4
void s_write_ps_string(stream * s, const byte * str, uint size, int print_ok);
int s_alloc_position_stream(stream ** ps, gs_memory_t * mem);
typedef struct param_printer_params_s {
const char *prefix;
const char *suffix;
const char *item_prefix;
const char *item_suffix;
int print_ok;
} param_printer_params_t;
typedef struct printer_param_list_s {
gs_param_list_common;
stream *strm;
param_printer_params_t params;
bool any;
} printer_param_list_t;
#define private_st_printer_param_list()	\
gs_private_st_ptrs1(st_printer_param_list, printer_param_list_t,\
"printer_param_list_t", printer_plist_enum_ptrs, printer_plist_reloc_ptrs,\
strm)
#define param_printer_params_default_values 0, 0, 0, "\n", 0
extern const param_printer_params_t param_printer_params_default;
int s_alloc_param_printer(gs_param_list ** pplist,
const param_printer_params_t * ppp, stream * s,
gs_memory_t * mem);
void s_free_param_printer(gs_param_list * plist);
int s_init_param_printer(printer_param_list_t *prlist,
const param_printer_params_t * ppp, stream * s);
void s_release_param_printer(printer_param_list_t *prlist);
#endif