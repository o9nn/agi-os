#ifndef gp_os2_INCLUDED
#  define gp_os2_INCLUDED
int pm_find_queue(char *queue_name, char *driver_name);
int pm_spool(char *filename, const char *queue);
#endif