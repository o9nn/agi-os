#ifndef gsparams_INCLUDED
#  define gsparams_INCLUDED
#include "stream.h"
#include "gsparam.h"
#if 0
int gs_param_list_puts(stream *dest, gs_param_list *list);
int gs_param_list_gets(stream *src, gs_param_list *list, gs_memory_t *mem);
#else
int gs_param_list_serialize(gs_param_list *list, byte *buf, int buf_size);
int gs_param_list_unserialize(gs_param_list *list, const byte *buf);
#endif
#endif