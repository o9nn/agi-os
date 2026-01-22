#ifndef idisp_INCLUDED
#  define idisp_INCLUDED
#ifndef display_callback_DEFINED
# define display_callback_DEFINED
typedef struct display_callback_s display_callback;
#endif
int display_set_callback(gs_main_instance *minst, display_callback *callback);
#endif