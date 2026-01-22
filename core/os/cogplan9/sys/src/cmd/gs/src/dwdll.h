#ifndef dwdll_INCLUDED
#  define dwdll_INCLUDED
#ifndef __PROTOTYPES__
#define __PROTOTYPES__
#endif
#include "iapi.h"
typedef struct GSDLL_S {
HINSTANCE hmodule;
PFN_gsapi_revision revision;
PFN_gsapi_new_instance new_instance;
PFN_gsapi_delete_instance delete_instance;
PFN_gsapi_set_stdio set_stdio;
PFN_gsapi_set_poll set_poll;
PFN_gsapi_set_display_callback set_display_callback;
PFN_gsapi_init_with_args init_with_args;
PFN_gsapi_run_string run_string;
PFN_gsapi_exit exit;
PFN_gsapi_set_visual_tracer set_visual_tracer;
} GSDLL;
int load_dll(GSDLL *gsdll, char *last_error, int len);
void unload_dll(GSDLL *gsdll);
#endif