#if !defined(gpsync_INCLUDED)
#  define gpsync_INCLUDED
typedef struct {
void *dummy_;
} gp_semaphore;
uint gp_semaphore_sizeof(void);
int gp_semaphore_open(gp_semaphore * sema);
int gp_semaphore_close(gp_semaphore * sema);
int gp_semaphore_wait(gp_semaphore * sema);
int gp_semaphore_signal(gp_semaphore * sema);
typedef struct {
void *dummy_;
} gp_monitor;
uint gp_monitor_sizeof(void);
int gp_monitor_open(gp_monitor * mon);
int gp_monitor_close(gp_monitor * mon);
int gp_monitor_enter(gp_monitor * mon);
int gp_monitor_leave(gp_monitor * mon);
typedef void (*gp_thread_creation_callback_t) (void *);
int gp_create_thread(gp_thread_creation_callback_t, void *);
#endif