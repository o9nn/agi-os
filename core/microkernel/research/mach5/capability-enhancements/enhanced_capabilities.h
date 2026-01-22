#ifndef _MACH5_ENHANCED_CAPABILITIES_H_
#define _MACH5_ENHANCED_CAPABILITIES_H_
#include <mach/mach.h>
#include <mach/port.h>
#include <sys/time.h>
#define ECAP_MAX_PERMISSIONS 64
#define ECAP_MAX_DELEGATION_DEPTH 8
#define ECAP_MAX_AUDIT_ENTRIES 1000
typedef enum {
ECAP_PERM_SEND = (1 << 0),
ECAP_PERM_RECEIVE = (1 << 1),
ECAP_PERM_SEND_ONCE = (1 << 2),
ECAP_PERM_SEND_TIMEOUT = (1 << 3),
ECAP_PERM_RECEIVE_TIMEOUT = (1 << 4),
ECAP_PERM_MSG_PRIORITY = (1 << 5),
ECAP_PERM_VM_READ = (1 << 8),
ECAP_PERM_VM_WRITE = (1 << 9),
ECAP_PERM_VM_EXECUTE = (1 << 10),
ECAP_PERM_VM_MAP = (1 << 11),
ECAP_PERM_VM_UNMAP = (1 << 12),
ECAP_PERM_TASK_CREATE = (1 << 16),
ECAP_PERM_TASK_DESTROY = (1 << 17),
ECAP_PERM_TASK_SUSPEND = (1 << 18),
ECAP_PERM_TASK_RESUME = (1 << 19),
ECAP_PERM_TASK_INFO = (1 << 20),
ECAP_PERM_THREAD_CREATE = (1 << 24),
ECAP_PERM_THREAD_DESTROY = (1 << 25),
ECAP_PERM_THREAD_CONTROL = (1 << 26),
ECAP_PERM_CAP_DELEGATE = (1 << 32),
ECAP_PERM_CAP_REVOKE = (1 << 33),
ECAP_PERM_CAP_AUDIT = (1 << 34),
ECAP_PERM_ADMIN_ALL = (1ULL << 63)
} ecap_permission_t;
typedef struct {
unsigned int max_depth;
unsigned int current_depth;
struct timeval expiration_time;
task_t delegator_task;
uint64_t delegation_id;
} ecap_delegation_info_t;
typedef struct {
mach_port_t port;
uint64_t permissions;
task_t owner_task;
ecap_delegation_info_t delegation;
uint32_t security_label;
unsigned int reference_count;
boolean_t is_transferable;
struct timeval creation_time;
struct timeval last_use_time;
unsigned long use_count;
uint64_t capability_id;
} ecap_capability_t;
typedef struct {
uint64_t capability_id;
task_t task;
ecap_permission_t permission_used;
struct timeval timestamp;
kern_return_t result;
char operation[64];
} ecap_audit_entry_t;
typedef struct {
ecap_capability_t *capabilities;
unsigned int capability_count;
unsigned int max_capabilities;
ecap_audit_entry_t *audit_log;
unsigned int audit_count;
unsigned int audit_max;
boolean_t audit_enabled;
boolean_t strict_delegation;
unsigned int default_expiration;
} ecap_system_context_t;
kern_return_t ecap_system_init(ecap_system_context_t *context);
kern_return_t ecap_system_cleanup(ecap_system_context_t *context);
kern_return_t ecap_create_capability(ecap_system_context_t *context,
mach_port_t port,
uint64_t permissions,
task_t owner_task,
ecap_capability_t **capability);
kern_return_t ecap_destroy_capability(ecap_system_context_t *context,
uint64_t capability_id);
kern_return_t ecap_find_capability(ecap_system_context_t *context,
uint64_t capability_id,
ecap_capability_t **capability);
kern_return_t ecap_check_permission(ecap_system_context_t *context,
uint64_t capability_id,
ecap_permission_t permission,
task_t requesting_task);
boolean_t ecap_has_permission(const ecap_capability_t *capability,
ecap_permission_t permission);
kern_return_t ecap_delegate_capability(ecap_system_context_t *context,
uint64_t source_capability_id,
task_t target_task,
uint64_t restricted_permissions,
unsigned int expiration_seconds,
uint64_t *new_capability_id);
kern_return_t ecap_revoke_delegation(ecap_system_context_t *context,
uint64_t capability_id,
boolean_t recursive);
kern_return_t ecap_check_delegation_validity(const ecap_capability_t *capability);
kern_return_t ecap_transfer_capability(ecap_system_context_t *context,
uint64_t capability_id,
task_t source_task,
task_t dest_task);
kern_return_t ecap_audit_operation(ecap_system_context_t *context,
uint64_t capability_id,
task_t task,
ecap_permission_t permission,
const char *operation,
kern_return_t result);
kern_return_t ecap_get_audit_log(ecap_system_context_t *context,
task_t task,
ecap_audit_entry_t **entries,
unsigned int *count);
void ecap_print_audit_entry(const ecap_audit_entry_t *entry);
kern_return_t ecap_set_security_policy(ecap_system_context_t *context,
boolean_t strict_mode,
boolean_t audit_all,
unsigned int default_expiration);
kern_return_t ecap_enforce_security_label(ecap_system_context_t *context,
task_t task,
uint32_t required_label);
kern_return_t ecap_list_capabilities(ecap_system_context_t *context,
task_t task,
uint64_t **capability_ids,
unsigned int *count);
kern_return_t ecap_get_capability_info(ecap_system_context_t *context,
uint64_t capability_id,
ecap_capability_t *info);
void ecap_print_capability(const ecap_capability_t *capability);
typedef struct {
unsigned long total_checks;
unsigned long successful_checks;
unsigned long failed_checks;
unsigned long delegations_created;
unsigned long delegations_revoked;
double avg_check_time_us;
} ecap_statistics_t;
kern_return_t ecap_get_statistics(ecap_system_context_t *context,
ecap_statistics_t *stats);
void ecap_print_statistics(const ecap_statistics_t *stats);
kern_return_t ecap_compose_capabilities(ecap_system_context_t *context,
uint64_t *capability_ids,
unsigned int count,
uint64_t *composed_capability_id);
typedef boolean_t (*ecap_condition_func_t)(task_t task, void *context);
kern_return_t ecap_create_conditional_capability(ecap_system_context_t *context,
mach_port_t port,
uint64_t permissions,
ecap_condition_func_t condition,
void *condition_context,
uint64_t *capability_id);
typedef struct {
char name[64];
uint64_t default_permissions;
unsigned int default_expiration;
boolean_t transferable;
} ecap_template_t;
kern_return_t ecap_create_template(ecap_system_context_t *context,
const char *name,
uint64_t permissions,
ecap_template_t **template);
kern_return_t ecap_create_from_template(ecap_system_context_t *context,
const ecap_template_t *template,
mach_port_t port,
task_t owner_task,
uint64_t *capability_id);
kern_return_t ecap_generate_formal_model(ecap_system_context_t *context,
const char *model_file);
kern_return_t ecap_verify_security_properties(ecap_system_context_t *context,
boolean_t *properties_verified);
kern_return_t ecap_benchmark_permission_checks(ecap_system_context_t *context,
unsigned int iterations,
double *avg_latency_us);
kern_return_t ecap_analyze_compatibility(ecap_system_context_t *context,
mach_port_t traditional_port,
boolean_t *is_compatible);
#endif