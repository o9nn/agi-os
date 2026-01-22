#define _GNU_SOURCE
#define _POSIX_C_SOURCE 199309L
#include "include/dtesn/psystem.h"
#include "include/dtesn/memory.h"
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <errno.h>
#include <unistd.h>
#include <sys/time.h>
#define DTESN_PSYSTEM_DEFAULT_THREAD_COUNT 4
#define DTESN_PSYSTEM_MEMORY_POOL_SIZE     (64 * 1024 * 1024)
#define DTESN_PSYSTEM_OBJECT_POOL_SIZE     1024
#define DTESN_PSYSTEM_RULE_POOL_SIZE       512
static bool g_psystem_initialized = false;
static pthread_mutex_t g_psystem_global_lock = PTHREAD_MUTEX_INITIALIZER;
static const uint32_t g_oeis_a000081[] = DTESN_PSYSTEM_A000081_SEQUENCE;
static struct {
uint64_t total_evolutions;
uint64_t total_evolution_time_ns;
uint64_t total_rule_applications;
uint64_t total_rule_time_ns;
uint64_t total_communications;
uint64_t total_communication_time_ns;
} g_performance_stats = {0};
static uint64_t get_timestamp_ns(void);
static int validate_membrane_hierarchy(dtesn_psystem_t *system);
static void *evolution_thread_worker(void *arg);
static int apply_rule(dtesn_psystem_membrane_t *membrane, dtesn_psystem_rule_t *rule);
static bool check_rule_applicability(dtesn_psystem_rule_t *rule,
dtesn_psystem_multiset_t *objects);
static int execute_communication_rule(dtesn_psystem_t *system,
dtesn_psystem_membrane_t *membrane,
dtesn_psystem_rule_t *rule);
static void update_performance_stats(uint64_t evolution_time_ns,
uint64_t rule_time_ns,
uint64_t comm_time_ns);
static uint64_t get_timestamp_ns(void) {
struct timespec ts;
if (clock_gettime(CLOCK_MONOTONIC, &ts) != 0) {
return 0;
}
return (uint64_t)ts.tv_sec * 1000000000ULL + (uint64_t)ts.tv_nsec;
}
static int validate_membrane_hierarchy(dtesn_psystem_t *system) {
if (!system) {
return DTESN_PSYSTEM_EINVAL;
}
uint32_t depth_counts[DTESN_PSYSTEM_A000081_MAX_DEPTH] = {0};
uint32_t max_depth = 0;
for (uint32_t i = 0; i < system->membrane_count; i++) {
dtesn_psystem_membrane_t *membrane = system->membranes[i];
if (membrane && !membrane->is_dissolved) {
if (membrane->depth_level < DTESN_PSYSTEM_A000081_MAX_DEPTH) {
depth_counts[membrane->depth_level]++;
if (membrane->depth_level > max_depth) {
max_depth = membrane->depth_level;
}
}
}
}
for (uint32_t depth = 0; depth <= max_depth; depth++) {
if (depth < DTESN_PSYSTEM_A000081_MAX_DEPTH) {
uint32_t expected = g_oeis_a000081[depth];
if (depth_counts[depth] > expected) {
return DTESN_PSYSTEM_EVALIDATION;
}
}
}
return 0;
}
static bool check_rule_applicability(dtesn_psystem_rule_t *rule,
dtesn_psystem_multiset_t *objects) {
if (!rule || !objects) {
return false;
}
dtesn_psystem_object_t *lhs_obj = rule->lhs.objects;
while (lhs_obj) {
dtesn_psystem_object_t *membrane_obj = objects->objects;
uint32_t available = 0;
while (membrane_obj) {
if (strcmp(membrane_obj->symbol, lhs_obj->symbol) == 0) {
available = membrane_obj->multiplicity;
break;
}
membrane_obj = membrane_obj->next;
}
if (available < lhs_obj->multiplicity) {
return false;
}
lhs_obj = lhs_obj->next;
}
return true;
}
static int apply_rule(dtesn_psystem_membrane_t *membrane, dtesn_psystem_rule_t *rule) {
if (!membrane || !rule) {
return DTESN_PSYSTEM_EINVAL;
}
uint64_t start_time = get_timestamp_ns();
pthread_mutex_lock(&membrane->lock);
if (!check_rule_applicability(rule, &membrane->objects)) {
pthread_mutex_unlock(&membrane->lock);
return 0;
}
dtesn_psystem_object_t *lhs_obj = rule->lhs.objects;
while (lhs_obj) {
dtesn_membrane_remove_object(NULL, membrane->membrane_id,
lhs_obj->symbol, lhs_obj->multiplicity);
lhs_obj = lhs_obj->next;
}
dtesn_psystem_object_t *rhs_obj = rule->rhs.objects;
while (rhs_obj) {
dtesn_membrane_add_object(NULL, membrane->membrane_id,
rhs_obj->symbol, rhs_obj->multiplicity);
rhs_obj = rhs_obj->next;
}
rule->application_count++;
rule->last_applied_ns = get_timestamp_ns();
membrane->total_rule_applications++;
pthread_mutex_unlock(&membrane->lock);
uint64_t end_time = get_timestamp_ns();
uint64_t rule_time = end_time - start_time;
if (rule_time > DTESN_PSYSTEM_RULE_THRESHOLD_US * 1000) {
}
g_performance_stats.total_rule_applications++;
g_performance_stats.total_rule_time_ns += rule_time;
return 1;
}
static int execute_communication_rule(dtesn_psystem_t *system,
dtesn_psystem_membrane_t *membrane,
dtesn_psystem_rule_t *rule) {
if (!system || !membrane || !rule) {
return DTESN_PSYSTEM_EINVAL;
}
if (rule->rule_type != DTESN_RULE_COMMUNICATION) {
return DTESN_PSYSTEM_EINVAL;
}
uint64_t start_time = get_timestamp_ns();
dtesn_psystem_membrane_t *target_membrane = NULL;
for (uint32_t i = 0; i < system->membrane_count; i++) {
if (system->membranes[i]->membrane_id == rule->target_membrane_id) {
target_membrane = system->membranes[i];
break;
}
}
if (!target_membrane || target_membrane->is_dissolved) {
return DTESN_PSYSTEM_ENOTFOUND;
}
int result = dtesn_membrane_communicate(system, membrane->membrane_id,
target_membrane->membrane_id,
&rule->rhs);
uint64_t end_time = get_timestamp_ns();
uint64_t comm_time = end_time - start_time;
membrane->total_communications++;
g_performance_stats.total_communications++;
g_performance_stats.total_communication_time_ns += comm_time;
if (comm_time > DTESN_PSYSTEM_COMM_THRESHOLD_US * 1000) {
return DTESN_PSYSTEM_ELATENCY;
}
return result;
}
dtesn_psystem_multiset_t *dtesn_multiset_create(void) {
dtesn_psystem_multiset_t *multiset = (dtesn_psystem_multiset_t *)
dtesn_alloc(sizeof(dtesn_psystem_multiset_t), 0);
if (!multiset) {
return NULL;
}
memset(multiset, 0, sizeof(dtesn_psystem_multiset_t));
if (pthread_mutex_init(&multiset->lock, NULL) != 0) {
dtesn_free(multiset);
return NULL;
}
return multiset;
}
void dtesn_multiset_destroy(dtesn_psystem_multiset_t *multiset) {
if (!multiset) {
return;
}
pthread_mutex_lock(&multiset->lock);
dtesn_psystem_object_t *obj = multiset->objects;
while (obj) {
dtesn_psystem_object_t *next = obj->next;
dtesn_free(obj);
obj = next;
}
pthread_mutex_unlock(&multiset->lock);
pthread_mutex_destroy(&multiset->lock);
dtesn_free(multiset);
}
int dtesn_multiset_add(dtesn_psystem_multiset_t *multiset,
const char *symbol,
uint32_t multiplicity) {
if (!multiset || !symbol || multiplicity == 0) {
return DTESN_PSYSTEM_EINVAL;
}
pthread_mutex_lock(&multiset->lock);
dtesn_psystem_object_t *obj = multiset->objects;
while (obj) {
if (strcmp(obj->symbol, symbol) == 0) {
obj->multiplicity += multiplicity;
multiset->total_multiplicity += multiplicity;
pthread_mutex_unlock(&multiset->lock);
return 0;
}
obj = obj->next;
}
dtesn_psystem_object_t *new_obj = (dtesn_psystem_object_t *)
dtesn_alloc(sizeof(dtesn_psystem_object_t), 0);
if (!new_obj) {
pthread_mutex_unlock(&multiset->lock);
return DTESN_PSYSTEM_ENOMEM;
}
strncpy(new_obj->symbol, symbol, DTESN_PSYSTEM_MAX_SYMBOL_LEN - 1);
new_obj->symbol[DTESN_PSYSTEM_MAX_SYMBOL_LEN - 1] = '\0';
new_obj->multiplicity = multiplicity;
new_obj->creation_time_ns = get_timestamp_ns();
new_obj->properties = 0;
new_obj->next = multiset->objects;
multiset->objects = new_obj;
multiset->object_count++;
multiset->total_multiplicity += multiplicity;
pthread_mutex_unlock(&multiset->lock);
return 0;
}
uint32_t dtesn_membrane_add_rule(dtesn_psystem_t *system,
uint32_t membrane_id,
dtesn_rule_type_t rule_type,
uint8_t priority,
const char *rule_string,
dtesn_psystem_multiset_t *lhs_objects,
dtesn_psystem_multiset_t *rhs_objects,
uint32_t target_membrane_id) {
if (!system || !rule_string || !lhs_objects || !rhs_objects) {
return 0;
}
dtesn_psystem_membrane_t *membrane = NULL;
for (uint32_t i = 0; i < system->membrane_count; i++) {
if (system->membranes[i]->membrane_id == membrane_id) {
membrane = system->membranes[i];
break;
}
}
if (!membrane || membrane->is_dissolved) {
return 0;
}
dtesn_psystem_rule_t *rule = (dtesn_psystem_rule_t *)
dtesn_alloc(sizeof(dtesn_psystem_rule_t), 0);
if (!rule) {
return 0;
}
memset(rule, 0, sizeof(dtesn_psystem_rule_t));
rule->rule_id = membrane->rule_count + 1;
rule->rule_type = rule_type;
rule->priority = priority;
rule->target_membrane_id = target_membrane_id;
rule->is_applicable = true;
strncpy(rule->rule_string, rule_string, DTESN_PSYSTEM_MAX_RULE_LEN - 1);
rule->rule_string[DTESN_PSYSTEM_MAX_RULE_LEN - 1] = '\0';
if (pthread_mutex_init(&rule->lhs.lock, NULL) != 0 ||
pthread_mutex_init(&rule->rhs.lock, NULL) != 0) {
dtesn_free(rule);
return 0;
}
rule->lhs = *lhs_objects;
rule->rhs = *rhs_objects;
pthread_mutex_lock(&membrane->lock);
rule->next = membrane->rules;
membrane->rules = rule;
membrane->rule_count++;
pthread_mutex_unlock(&membrane->lock);
return rule->rule_id;
}
int dtesn_psystem_init(void) {
pthread_mutex_lock(&g_psystem_global_lock);
if (g_psystem_initialized) {
pthread_mutex_unlock(&g_psystem_global_lock);
return 0;
}
int mem_result = dtesn_mem_init();
if (mem_result != 0) {
pthread_mutex_unlock(&g_psystem_global_lock);
return mem_result;
}
memset(&g_performance_stats, 0, sizeof(g_performance_stats));
g_psystem_initialized = true;
pthread_mutex_unlock(&g_psystem_global_lock);
return 0;
}
dtesn_psystem_t *dtesn_psystem_create(const char *system_name, uint32_t max_membranes) {
if (!g_psystem_initialized) {
return NULL;
}
if (!system_name) {
return NULL;
}
if (max_membranes == 0) {
max_membranes = DTESN_PSYSTEM_MAX_MEMBRANES;
}
dtesn_psystem_t *system = (dtesn_psystem_t *)dtesn_alloc(sizeof(dtesn_psystem_t), 0);
if (!system) {
return NULL;
}
memset(system, 0, sizeof(dtesn_psystem_t));
strncpy(system->system_name, system_name, DTESN_PSYSTEM_MAX_SYMBOL_LEN - 1);
system->system_id = (uint32_t)time(NULL);
system->membrane_capacity = max_membranes;
system->next_membrane_id = 1;
system->global_phase = DTESN_PHASE_INPUT;
system->membranes = (dtesn_psystem_membrane_t **)dtesn_alloc(
max_membranes * sizeof(dtesn_psystem_membrane_t *), 0);
if (!system->membranes) {
dtesn_free(system);
return NULL;
}
if (pthread_mutex_init(&system->system_lock, NULL) != 0) {
dtesn_free(system->membranes);
dtesn_free(system);
return NULL;
}
system->thread_count = DTESN_PSYSTEM_DEFAULT_THREAD_COUNT;
system->evolution_threads = (pthread_t *)dtesn_alloc(
system->thread_count * sizeof(pthread_t), 0);
if (!system->evolution_threads) {
pthread_mutex_destroy(&system->system_lock);
dtesn_free(system->membranes);
dtesn_free(system);
return NULL;
}
system->memory_pool_size = DTESN_PSYSTEM_MEMORY_POOL_SIZE;
system->memory_pool = dtesn_alloc(system->memory_pool_size, 0);
if (!system->memory_pool) {
dtesn_free(system->evolution_threads);
pthread_mutex_destroy(&system->system_lock);
dtesn_free(system->membranes);
dtesn_free(system);
return NULL;
}
return system;
}
void dtesn_psystem_destroy(dtesn_psystem_t *system) {
if (!system) {
return;
}
pthread_mutex_lock(&system->system_lock);
for (uint32_t i = 0; i < system->membrane_count; i++) {
if (system->membranes[i]) {
dtesn_membrane_destroy(system, system->membranes[i]->membrane_id);
}
}
pthread_mutex_unlock(&system->system_lock);
if (system->memory_pool) {
dtesn_free(system->memory_pool);
}
if (system->evolution_threads) {
dtesn_free(system->evolution_threads);
}
if (system->membranes) {
dtesn_free(system->membranes);
}
pthread_mutex_destroy(&system->system_lock);
dtesn_free(system);
}
uint32_t dtesn_membrane_create(dtesn_psystem_t *system,
dtesn_membrane_type_t membrane_type,
const char *label,
uint32_t parent_id,
uint32_t neuron_count) {
if (!system || !label) {
return 0;
}
pthread_mutex_lock(&system->system_lock);
if (system->membrane_count >= system->membrane_capacity) {
pthread_mutex_unlock(&system->system_lock);
return 0;
}
dtesn_psystem_membrane_t *membrane = (dtesn_psystem_membrane_t *)
dtesn_alloc(sizeof(dtesn_psystem_membrane_t), 0);
if (!membrane) {
pthread_mutex_unlock(&system->system_lock);
return 0;
}
memset(membrane, 0, sizeof(dtesn_psystem_membrane_t));
membrane->membrane_id = system->next_membrane_id++;
membrane->membrane_type = membrane_type;
strncpy(membrane->label, label, DTESN_PSYSTEM_MAX_SYMBOL_LEN - 1);
membrane->parent_id = parent_id;
membrane->neuron_count = neuron_count;
membrane->spectral_radius = 0.9f;
membrane->leak_rate = 0.1f;
membrane->connectivity = 0.1f;
membrane->current_phase = DTESN_PHASE_INPUT;
membrane->is_active = true;
if (parent_id == 0) {
membrane->depth_level = 0;
if (system->skin_membrane_id == 0) {
system->skin_membrane_id = membrane->membrane_id;
}
} else {
for (uint32_t i = 0; i < system->membrane_count; i++) {
if (system->membranes[i]->membrane_id == parent_id) {
membrane->depth_level = system->membranes[i]->depth_level + 1;
break;
}
}
}
if (pthread_mutex_init(&membrane->objects.lock, NULL) != 0) {
dtesn_free(membrane);
pthread_mutex_unlock(&system->system_lock);
return 0;
}
if (pthread_mutex_init(&membrane->lock, NULL) != 0 ||
pthread_cond_init(&membrane->evolution_cond, NULL) != 0) {
pthread_mutex_destroy(&membrane->objects.lock);
dtesn_free(membrane);
pthread_mutex_unlock(&system->system_lock);
return 0;
}
system->membranes[system->membrane_count] = membrane;
system->membrane_count++;
if (validate_membrane_hierarchy(system) != 0) {
system->membrane_count--;
pthread_mutex_destroy(&membrane->lock);
pthread_cond_destroy(&membrane->evolution_cond);
pthread_mutex_destroy(&membrane->objects.lock);
dtesn_free(membrane);
pthread_mutex_unlock(&system->system_lock);
return 0;
}
uint32_t membrane_id = membrane->membrane_id;
pthread_mutex_unlock(&system->system_lock);
return membrane_id;
}
int dtesn_membrane_destroy(dtesn_psystem_t *system, uint32_t membrane_id) {
if (!system || membrane_id == 0) {
return DTESN_PSYSTEM_EINVAL;
}
pthread_mutex_lock(&system->system_lock);
dtesn_psystem_membrane_t *membrane = NULL;
uint32_t membrane_index = 0;
for (uint32_t i = 0; i < system->membrane_count; i++) {
if (system->membranes[i]->membrane_id == membrane_id) {
membrane = system->membranes[i];
membrane_index = i;
break;
}
}
if (!membrane) {
pthread_mutex_unlock(&system->system_lock);
return DTESN_PSYSTEM_ENOTFOUND;
}
pthread_mutex_lock(&membrane->lock);
membrane->is_dissolved = true;
membrane->is_active = false;
if (membrane->parent_id != 0) {
for (uint32_t i = 0; i < system->membrane_count; i++) {
if (system->membranes[i]->membrane_id == membrane->parent_id) {
dtesn_psystem_object_t *obj = membrane->objects.objects;
while (obj) {
dtesn_membrane_add_object(system, membrane->parent_id,
obj->symbol, obj->multiplicity);
obj = obj->next;
}
break;
}
}
}
pthread_mutex_unlock(&membrane->lock);
for (uint32_t i = membrane_index; i < system->membrane_count - 1; i++) {
system->membranes[i] = system->membranes[i + 1];
}
system->membrane_count--;
pthread_mutex_destroy(&membrane->lock);
pthread_cond_destroy(&membrane->evolution_cond);
pthread_mutex_destroy(&membrane->objects.lock);
dtesn_psystem_object_t *obj = membrane->objects.objects;
while (obj) {
dtesn_psystem_object_t *next = obj->next;
dtesn_free(obj);
obj = next;
}
dtesn_psystem_rule_t *rule = membrane->rules;
while (rule) {
dtesn_psystem_rule_t *next = rule->next;
dtesn_free(rule);
rule = next;
}
dtesn_free(membrane);
pthread_mutex_unlock(&system->system_lock);
return 0;
}
int dtesn_membrane_add_object(dtesn_psystem_t *system,
uint32_t membrane_id,
const char *symbol,
uint32_t multiplicity) {
if (!symbol || multiplicity == 0) {
return DTESN_PSYSTEM_EINVAL;
}
dtesn_psystem_membrane_t *membrane = NULL;
if (system) {
for (uint32_t i = 0; i < system->membrane_count; i++) {
if (system->membranes[i]->membrane_id == membrane_id) {
membrane = system->membranes[i];
break;
}
}
}
if (!membrane) {
return DTESN_PSYSTEM_ENOTFOUND;
}
pthread_mutex_lock(&membrane->objects.lock);
dtesn_psystem_object_t *obj = membrane->objects.objects;
while (obj) {
if (strcmp(obj->symbol, symbol) == 0) {
obj->multiplicity += multiplicity;
membrane->objects.total_multiplicity += multiplicity;
pthread_mutex_unlock(&membrane->objects.lock);
return 0;
}
obj = obj->next;
}
dtesn_psystem_object_t *new_obj = (dtesn_psystem_object_t *)
dtesn_alloc(sizeof(dtesn_psystem_object_t), 0);
if (!new_obj) {
pthread_mutex_unlock(&membrane->objects.lock);
return DTESN_PSYSTEM_ENOMEM;
}
strncpy(new_obj->symbol, symbol, DTESN_PSYSTEM_MAX_SYMBOL_LEN - 1);
new_obj->multiplicity = multiplicity;
new_obj->creation_time_ns = get_timestamp_ns();
new_obj->properties = 0;
new_obj->next = membrane->objects.objects;
membrane->objects.objects = new_obj;
membrane->objects.object_count++;
membrane->objects.total_multiplicity += multiplicity;
pthread_mutex_unlock(&membrane->objects.lock);
return 0;
}
uint32_t dtesn_membrane_remove_object(dtesn_psystem_t *system,
uint32_t membrane_id,
const char *symbol,
uint32_t multiplicity) {
if (!symbol || multiplicity == 0) {
return 0;
}
dtesn_psystem_membrane_t *membrane = NULL;
if (system) {
for (uint32_t i = 0; i < system->membrane_count; i++) {
if (system->membranes[i]->membrane_id == membrane_id) {
membrane = system->membranes[i];
break;
}
}
}
if (!membrane) {
return 0;
}
pthread_mutex_lock(&membrane->objects.lock);
dtesn_psystem_object_t **obj_ptr = &membrane->objects.objects;
while (*obj_ptr) {
dtesn_psystem_object_t *obj = *obj_ptr;
if (strcmp(obj->symbol, symbol) == 0) {
uint32_t removed = (multiplicity > obj->multiplicity) ?
obj->multiplicity : multiplicity;
obj->multiplicity -= removed;
membrane->objects.total_multiplicity -= removed;
if (obj->multiplicity == 0) {
*obj_ptr = obj->next;
membrane->objects.object_count--;
dtesn_free(obj);
}
pthread_mutex_unlock(&membrane->objects.lock);
return removed;
}
obj_ptr = &obj->next;
}
pthread_mutex_unlock(&membrane->objects.lock);
return 0;
}
int dtesn_membrane_evolve(dtesn_psystem_t *system, uint32_t membrane_id) {
if (!system) {
return DTESN_PSYSTEM_EINVAL;
}
uint64_t start_time = get_timestamp_ns();
dtesn_psystem_membrane_t *membrane = NULL;
for (uint32_t i = 0; i < system->membrane_count; i++) {
if (system->membranes[i]->membrane_id == membrane_id) {
membrane = system->membranes[i];
break;
}
}
if (!membrane || membrane->is_dissolved || !membrane->is_active) {
return DTESN_PSYSTEM_ENOTFOUND;
}
pthread_mutex_lock(&membrane->lock);
int rules_applied = 0;
dtesn_psystem_rule_t *rule = membrane->rules;
if (membrane->rule_count > 1) {
for (uint32_t i = 0; i < membrane->rule_count - 1; i++) {
dtesn_psystem_rule_t *current = membrane->rules;
for (uint32_t j = 0; j < membrane->rule_count - 1 - i; j++) {
if (current->priority < current->next->priority) {
dtesn_psystem_rule_t temp = *current;
*current = *current->next;
*current->next = temp;
}
current = current->next;
}
}
}
rule = membrane->rules;
while (rule) {
if (check_rule_applicability(rule, &membrane->objects)) {
if (rule->rule_type == DTESN_RULE_EVOLUTION) {
int result = apply_rule(membrane, rule);
if (result > 0) {
rules_applied += result;
}
} else if (rule->rule_type == DTESN_RULE_COMMUNICATION) {
int result = execute_communication_rule(system, membrane, rule);
if (result >= 0) {
rules_applied++;
}
}
}
rule = rule->next;
}
membrane->evolution_step++;
pthread_mutex_unlock(&membrane->lock);
uint64_t end_time = get_timestamp_ns();
uint64_t evolution_time = end_time - start_time;
membrane->total_evolution_time_ns += evolution_time;
g_performance_stats.total_evolutions++;
g_performance_stats.total_evolution_time_ns += evolution_time;
if (evolution_time > DTESN_PSYSTEM_EVOLUTION_THRESHOLD_US * 1000) {
return DTESN_PSYSTEM_ELATENCY;
}
return rules_applied;
}
bool dtesn_system_evolve(dtesn_psystem_t *system) {
if (!system) {
return false;
}
pthread_mutex_lock(&system->system_lock);
if (system->is_halted) {
pthread_mutex_unlock(&system->system_lock);
return false;
}
bool any_active = false;
for (uint32_t i = 0; i < system->membrane_count; i++) {
dtesn_psystem_membrane_t *membrane = system->membranes[i];
if (membrane && !membrane->is_dissolved && membrane->is_active) {
int rules_applied = dtesn_membrane_evolve(system, membrane->membrane_id);
if (rules_applied > 0) {
any_active = true;
}
}
}
system->global_evolution_step++;
if (!any_active) {
system->is_halted = true;
system->global_phase = DTESN_PHASE_HALTED;
}
bool is_active = !system->is_halted;
pthread_mutex_unlock(&system->system_lock);
return is_active;
}
int dtesn_membrane_communicate(dtesn_psystem_t *system,
uint32_t source_id,
uint32_t target_id,
dtesn_psystem_multiset_t *objects) {
if (!system || !objects || source_id == target_id) {
return DTESN_PSYSTEM_EINVAL;
}
uint64_t start_time = get_timestamp_ns();
dtesn_psystem_membrane_t *source = NULL;
dtesn_psystem_membrane_t *target = NULL;
for (uint32_t i = 0; i < system->membrane_count; i++) {
if (system->membranes[i]->membrane_id == source_id) {
source = system->membranes[i];
}
if (system->membranes[i]->membrane_id == target_id) {
target = system->membranes[i];
}
}
if (!source || !target || source->is_dissolved || target->is_dissolved) {
return DTESN_PSYSTEM_ENOTFOUND;
}
dtesn_psystem_object_t *obj = objects->objects;
while (obj) {
uint32_t removed = dtesn_membrane_remove_object(system, source_id,
obj->symbol, obj->multiplicity);
if (removed > 0) {
dtesn_membrane_add_object(system, target_id, obj->symbol, removed);
}
obj = obj->next;
}
uint64_t end_time = get_timestamp_ns();
uint64_t comm_time = end_time - start_time;
if (comm_time > DTESN_PSYSTEM_COMM_THRESHOLD_US * 1000) {
return DTESN_PSYSTEM_ELATENCY;
}
return 0;
}
bool dtesn_psystem_validate_a000081(dtesn_psystem_t *system) {
if (!system) {
return false;
}
return (validate_membrane_hierarchy(system) == 0);
}
int dtesn_psystem_get_stats(dtesn_psystem_t *system, dtesn_psystem_stats_t *stats) {
if (!system || !stats) {
return DTESN_PSYSTEM_EINVAL;
}
pthread_mutex_lock(&system->system_lock);
memset(stats, 0, sizeof(dtesn_psystem_stats_t));
for (uint32_t i = 0; i < system->membrane_count; i++) {
dtesn_psystem_membrane_t *membrane = system->membranes[i];
if (membrane && !membrane->is_dissolved) {
stats->total_evolution_time_ns += membrane->total_evolution_time_ns;
stats->total_rule_applications += membrane->total_rule_applications;
stats->total_communications += membrane->total_communications;
stats->active_membranes++;
stats->total_objects += membrane->objects.total_multiplicity;
stats->total_rules += membrane->rule_count;
}
}
if (g_performance_stats.total_evolutions > 0) {
stats->avg_evolution_time_ns = g_performance_stats.total_evolution_time_ns /
g_performance_stats.total_evolutions;
}
if (g_performance_stats.total_rule_applications > 0) {
stats->avg_rule_time_ns = g_performance_stats.total_rule_time_ns /
g_performance_stats.total_rule_applications;
}
if (g_performance_stats.total_communications > 0) {
stats->avg_comm_time_ns = g_performance_stats.total_communication_time_ns /
g_performance_stats.total_communications;
}
stats->meets_performance_targets =
(stats->avg_evolution_time_ns <= DTESN_PSYSTEM_EVOLUTION_THRESHOLD_US * 1000) &&
(stats->avg_rule_time_ns <= DTESN_PSYSTEM_RULE_THRESHOLD_US * 1000) &&
(stats->avg_comm_time_ns <= DTESN_PSYSTEM_COMM_THRESHOLD_US * 1000);
pthread_mutex_unlock(&system->system_lock);
return 0;
}
void dtesn_psystem_shutdown(void) {
pthread_mutex_lock(&g_psystem_global_lock);
if (!g_psystem_initialized) {
pthread_mutex_unlock(&g_psystem_global_lock);
return;
}
memset(&g_performance_stats, 0, sizeof(g_performance_stats));
g_psystem_initialized = false;
pthread_mutex_unlock(&g_psystem_global_lock);
}