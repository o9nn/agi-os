#ifndef imainarg_INCLUDED
#  define imainarg_INCLUDED
#ifndef gs_main_instance_DEFINED
#  define gs_main_instance_DEFINED
typedef struct gs_main_instance_s gs_main_instance;
#endif
int gs_main_init_with_args(gs_main_instance * minst, int argc, char *argv[]);
int gs_main_run_start(gs_main_instance * minst);
#endif