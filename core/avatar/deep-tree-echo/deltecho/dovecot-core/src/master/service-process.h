#ifndef SERVICE_PROCESS_H
#define SERVICE_PROCESS_H
struct service_process {
struct service_process *prev, *next;
struct service *service;
int refcount;
pid_t pid;
unsigned int uid;
unsigned int available_count;
unsigned int total_count;
time_t create_time;
time_t idle_start;
struct timeout *to_idle_kill;
time_t last_status_update;
time_t last_kill_sent;
struct timeout *to_status;
bool destroyed:1;
};
#define SERVICE_PROCESS_IS_INITIALIZED(process) \
((process)->to_status == NULL)
struct service_process *service_process_create(struct service *service);
void service_process_destroy(struct service_process *process);
void service_process_ref(struct service_process *process);
void service_process_unref(struct service_process *process);
void service_process_log_status_error(struct service_process *process,
int status);
#endif