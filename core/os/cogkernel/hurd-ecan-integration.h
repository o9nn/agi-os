#ifndef _HURD_ECAN_INTEGRATION_H
#define _HURD_ECAN_INTEGRATION_H
#include <errno.h>
#include <sys/types.h>
#ifdef __GNU__
#include <mach/mach_types.h>
#else
typedef unsigned int mach_port_t;
typedef int error_t;
#endif
#define ECAN_DEFAULT_TOTAL_FUNDS 10000
#define ECAN_DEFAULT_FOCUS_THRESHOLD 100
#define ECAN_DEFAULT_WAGE_RATE 0.1
#define ECAN_DEFAULT_RENT_RATE 0.05
#define ECAN_DEFAULT_SPREAD_RATE 0.2
typedef struct {
double sti;
double lti;
double vlti;
} attention_value_t;
typedef enum {
ACTIVITY_READ = 10,
ACTIVITY_WRITE = 20,
ACTIVITY_CREATE = 15,
ACTIVITY_DELETE = 10,
ACTIVITY_RENAME = 5,
ACTIVITY_STAT = 2,
ACTIVITY_OPEN = 5,
ACTIVITY_CLOSE = 2,
ACTIVITY_NETWORK_SEND = 10,
ACTIVITY_NETWORK_RECV = 8,
} activity_type_t;
typedef enum {
RESOURCE_MEMORY,
RESOURCE_CPU,
RESOURCE_IO,
RESOURCE_NETWORK,
RESOURCE_FILE_DESCRIPTOR,
} resource_type_t;
error_t hurd_ecan_init(void);
void hurd_ecan_shutdown(void);
error_t hurd_ecan_register_client(mach_port_t client_port,
pid_t pid);
error_t hurd_ecan_unregister_client(mach_port_t client_port);
error_t hurd_ecan_record_activity(mach_port_t client_port,
activity_type_t activity,
size_t quantity);
error_t hurd_ecan_record_activities(mach_port_t client_port,
activity_type_t *activities,
size_t *quantities,
size_t count);
error_t hurd_ecan_apply_wages(void);
error_t hurd_ecan_charge_rent(mach_port_t client_port,
resource_type_t resource,
size_t amount);
error_t hurd_ecan_collect_rent(void);
error_t hurd_ecan_check_client_credit(mach_port_t client_port,
size_t required_resources);
size_t hurd_ecan_get_client_limit(mach_port_t client_port,
resource_type_t resource);
error_t hurd_ecan_get_client_attention(mach_port_t client_port,
attention_value_t *av);
error_t hurd_ecan_get_rogue_clients(mach_port_t **rogues,
size_t *count);
int hurd_ecan_is_client_rogue(mach_port_t client_port);
error_t hurd_ecan_cycle(void);
typedef struct {
size_t total_funds;
size_t focus_threshold;
double total_sti;
double total_lti;
size_t client_count;
double wage_rate;
double rent_rate;
double spread_rate;
size_t history_length;
} ecan_economics_t;
error_t hurd_ecan_get_economics(ecan_economics_t *economics);
error_t hurd_ecan_sync_node(const char *node_id);
error_t hurd_ecan_broadcast_event(const char *event_type,
const void *event_data,
size_t data_size);
error_t hurd_ecan_set_wage_rate(double rate);
error_t hurd_ecan_set_rent_rate(double rate);
error_t hurd_ecan_set_spread_rate(double rate);
error_t hurd_ecan_set_focus_threshold(size_t threshold);
error_t hurd_ecan_get_wage_rate(double *rate);
error_t hurd_ecan_get_rent_rate(double *rate);
error_t hurd_ecan_get_spread_rate(double *rate);
error_t hurd_ecan_get_focus_threshold(size_t *threshold);
#define HURD_ECAN_RECORD(client, activity) \
hurd_ecan_record_activity((client), (activity), 1)
#define HURD_ECAN_CHECK_OR_FAIL(client, size) \
do { \
if (hurd_ecan_check_client_credit((client), (size)) != 0) { \
return ENOSPC; \
} \
} while (0)
#define HURD_ECAN_CHARGE_MEMORY(client, size) \
hurd_ecan_charge_rent((client), RESOURCE_MEMORY, (size))
#endif