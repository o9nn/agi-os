#include "malloc_.h"
#include "gserror.h"
#include "gserrors.h"
#include "gpsync.h"
#include "windows_.h"
#include <process.h>
typedef struct win32_semaphore_s {
HANDLE handle;
} win32_semaphore;
uint
gp_semaphore_sizeof(void)
{
return sizeof(win32_semaphore);
}
int
gp_semaphore_open(
gp_semaphore * sema
)
{
win32_semaphore *const winSema = (win32_semaphore *)sema;
if (winSema) {
winSema->handle = CreateSemaphore(NULL, 0, max_int, NULL);
return
(winSema->handle != NULL ? 0 :
gs_note_error(gs_error_unknownerror));
} else
return 0;
}
int
gp_semaphore_close(
gp_semaphore * sema
)
{
win32_semaphore *const winSema = (win32_semaphore *)sema;
if (winSema->handle != NULL)
CloseHandle(winSema->handle);
winSema->handle = NULL;
return 0;
}
int
gp_semaphore_wait(
gp_semaphore * sema
)
{
win32_semaphore *const winSema = (win32_semaphore *)sema;
return
(WaitForSingleObject(winSema->handle, INFINITE) == WAIT_OBJECT_0
? 0 : gs_error_unknownerror);
}
int
gp_semaphore_signal(
gp_semaphore * sema
)
{
win32_semaphore *const winSema = (win32_semaphore *)sema;
return
(ReleaseSemaphore(winSema->handle, 1, NULL) ? 0 :
gs_error_unknownerror);
}
typedef struct win32_monitor_s {
CRITICAL_SECTION lock;
} win32_monitor;
uint
gp_monitor_sizeof(void)
{
return sizeof(win32_monitor);
}
int
gp_monitor_open(
gp_monitor * mon
)
{
win32_monitor *const winMon = (win32_monitor *)mon;
if (mon) {
InitializeCriticalSection(&winMon->lock);
return 0;
} else
return 1;
}
int
gp_monitor_close(
gp_monitor * mon
)
{
win32_monitor *const winMon = (win32_monitor *)mon;
DeleteCriticalSection(&winMon->lock);
return 0;
}
int
gp_monitor_enter(
gp_monitor * mon
)
{
win32_monitor *const winMon = (win32_monitor *)mon;
EnterCriticalSection(&winMon->lock);
return 0;
}
int
gp_monitor_leave(
gp_monitor * mon
)
{
win32_monitor *const winMon = (win32_monitor *)mon;
LeaveCriticalSection(&winMon->lock);
return 0;
}
typedef struct gp_thread_creation_closure_s {
gp_thread_creation_callback_t function;
void *data;
} gp_thread_creation_closure;
private void
gp_thread_begin_wrapper(
void *thread_data
)
{
gp_thread_creation_closure closure;
closure = *(gp_thread_creation_closure *)thread_data;
free(thread_data);
(*closure.function)(closure.data);
_endthread();
}
int
gp_create_thread(
gp_thread_creation_callback_t function,
void *data
)
{
gp_thread_creation_closure *closure =
(gp_thread_creation_closure *)malloc(sizeof(*closure));
if (!closure)
return gs_error_VMerror;
closure->function = function;
closure->data = data;
if (~BEGIN_THREAD(gp_thread_begin_wrapper, 0, closure) != 0)
return 0;
return_error(gs_error_unknownerror);
}