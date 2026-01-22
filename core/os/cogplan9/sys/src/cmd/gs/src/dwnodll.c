#define STRICT
#include <windows.h>
#include <string.h>
#include <stdio.h>
#include "stdpre.h"
#include "gpgetenv.h"
#include "gscdefs.h"
#define GSREVISION gs_revision
#include "dwdll.h"
int load_dll(GSDLL *gsdll, char *last_error, int len)
{
gsdll->new_instance = &gsapi_new_instance;
gsdll->delete_instance = &gsapi_delete_instance;
gsdll->set_stdio = &gsapi_set_stdio;
gsdll->set_poll = &gsapi_set_poll;
gsdll->set_display_callback = &gsapi_set_display_callback;
gsdll->init_with_args = &gsapi_init_with_args;
gsdll->run_string = &gsapi_run_string;
gsdll->exit = &gsapi_exit;
gsdll->set_visual_tracer = &gsapi_set_visual_tracer;
return 0;
}
void unload_dll(GSDLL *gsdll)
{
}