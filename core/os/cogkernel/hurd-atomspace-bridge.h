#ifndef _HURD_ATOMSPACE_BRIDGE_H_
#define _HURD_ATOMSPACE_BRIDGE_H_
#include <mach.h>
#include <hurd/hurd_types.h>
#include <sys/time.h>
#include <time.h>
#define MAX_PORTS 256
#define MAX_SERVERS 64
#define MAX_NAME_LENGTH 256
#define MAX_PATH_LENGTH 1024
typedef struct {
char name[MAX_NAME_LENGTH];
mach_port_t port;
mach_port_type_t type;
time_t registered_time;
} atomspace_port_t;
typedef struct {
char name[MAX_NAME_LENGTH];
char path[MAX_PATH_LENGTH];
mach_port_t port;
time_t registered_time;
} atomspace_server_t;
typedef struct {
mach_port_t task_port;
mach_port_t host_priv_port;
atomspace_port_t ports[MAX_PORTS];
atomspace_server_t servers[MAX_SERVERS];
int atom_count;
int port_count;
int server_count;
unsigned long ipc_calls;
unsigned long errors;
struct timeval init_time;
} atomspace_bridge_t;
typedef struct {
int atom_count;
int port_count;
int server_count;
unsigned long ipc_calls;
unsigned long errors;
long uptime_seconds;
} atomspace_stats_t;
error_t hurd_atomspace_bridge_init(void);
void hurd_atomspace_bridge_shutdown(void);
error_t hurd_atomspace_register_port(const char *port_name, mach_port_t port,
mach_port_type_t port_type);
error_t hurd_atomspace_register_server(const char *server_name, const char *server_path,
mach_port_t server_port);
error_t hurd_atomspace_ipc_send(const char *destination, const void *data, size_t size);
int hurd_atomspace_query_objects(const char *object_type, char **results, int max_results);
void hurd_atomspace_get_stats(atomspace_stats_t *stats);
void hurd_atomspace_monitor_performance(void);
error_t hurd_atomspace_bootstrap_core(void);
#endif