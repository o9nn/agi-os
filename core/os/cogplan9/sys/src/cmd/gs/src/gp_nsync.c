#include "std.h"
#include "gserror.h"
#include "gserrors.h"
#include "gpsync.h"
uint
gp_semaphore_sizeof(void)
{
return sizeof(gp_semaphore);
}
int
gp_semaphore_open(gp_semaphore * sema)
{
if (sema)
*(int *)sema = 0;
return 0;
}
int
gp_semaphore_close(gp_semaphore * sema)
{
return 0;
}
int
gp_semaphore_wait(gp_semaphore * sema)
{
if (*(int *)sema == 0)
return_error(gs_error_unknownerror);
--(*(int *)sema);
return 0;
}
int
gp_semaphore_signal(gp_semaphore * sema)
{
++(*(int *)sema);
return 0;
}
uint
gp_monitor_sizeof(void)
{
return sizeof(gp_monitor);
}
int
gp_monitor_open(gp_monitor * mon)
{
if (mon)
mon->dummy_ = 0;
return 0;
}
int
gp_monitor_close(gp_monitor * mon)
{
return 0;
}
int
gp_monitor_enter(gp_monitor * mon)
{
if (mon->dummy_ != 0)
return_error(gs_error_unknownerror);
mon->dummy_ = mon;
return 0;
}
int
gp_monitor_leave(gp_monitor * mon)
{
if (mon->dummy_ != mon)
return_error(gs_error_unknownerror);
mon->dummy_ = 0;
return 0;
}
int
gp_create_thread(gp_thread_creation_callback_t proc, void *proc_data)
{
return_error(gs_error_unknownerror);
}