#ifndef DTESN_PSYSTEM_H
#define DTESN_PSYSTEM_H
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <pthread.h>
#ifdef __cplusplus
extern "C" {
#endif
#define DTESN_PSYSTEM_MAX_MEMBRANES 1024
#define DTESN_PSYSTEM_MAX_OBJECTS 4096
#define DTESN_PSYSTEM_MAX_RULES 256
#define DTESN_PSYSTEM_MAX_SYMBOL_LEN 64
#define DTESN_PSYSTEM_MAX_RULE_LEN 512
#define DTESN_PSYSTEM_MAX_PRIORITY 255
#define DTESN_PSYSTEM_EVOLUTION_THRESHOLD_US 10
#define DTESN_PSYSTEM_RULE_THRESHOLD_US 1
#define DTESN_PSYSTEM_COMM_THRESHOLD_US 5
#define DTESN_PSYSTEM_PARALLEL_EFFICIENCY 85
#define DTESN_PSYSTEM_A000081_MAX_DEPTH 10
#define DTESN_PSYSTEM_A000081_SEQUENCE \
{ 1, 1, 2, 4, 9, 20, 48, 115, 286, 719 }
typedef enum {
DTESN_MEMBRANE_ROOT = 0,
DTESN_MEMBRANE_TRUNK = 1,
DTESN_MEMBRANE_BRANCH = 2,
DTESN_MEMBRANE_LEAF = 3,
DTESN_MEMBRANE_TERMINAL = 4,
DTESN_MEMBRANE_SKIN = 5,
DTESN_MEMBRANE_ELEMENTARY = 6
} dtesn_membrane_type_t;
typedef enum {
DTESN_RULE_EVOLUTION = 0,
DTESN_RULE_COMMUNICATION = 1,
DTESN_RULE_DISSOLUTION = 2,
DTESN_RULE_DIVISION = 3,
DTESN_RULE_CREATION = 4,
DTESN_RULE_SYMPORT = 5,
DTESN_RULE_ANTIPORT = 6
} dtesn_rule_type_t;
typedef enum {
DTESN_PHASE_INPUT = 0,
DTESN_PHASE_EVOLUTION = 1,
DTESN_PHASE_COMMUNICATION = 2,
DTESN_PHASE_OUTPUT = 3,
DTESN_PHASE_HALTED = 4
} dtesn_execution_phase_t;
typedef struct dtesn_psystem_object {
char symbol[DTESN_PSYSTEM_MAX_SYMBOL_LEN];
uint32_t multiplicity;
uint64_t creation_time_ns;
uint32_t properties;
struct dtesn_psystem_object *next;
} dtesn_psystem_object_t;
typedef struct dtesn_psystem_multiset {
dtesn_psystem_object_t *objects;
uint32_t object_count;
uint32_t total_multiplicity;
pthread_mutex_t lock;
} dtesn_psystem_multiset_t;
typedef struct dtesn_psystem_rule {
uint32_t rule_id;
char rule_string[DTESN_PSYSTEM_MAX_RULE_LEN];
dtesn_rule_type_t rule_type;
uint8_t priority;
dtesn_psystem_multiset_t lhs;
dtesn_psystem_multiset_t rhs;
uint32_t target_membrane_id;
uint32_t source_membrane_id;
uint64_t application_count;
uint64_t last_applied_ns;
bool is_applicable;
struct dtesn_psystem_rule *next;
} dtesn_psystem_rule_t;
typedef struct dtesn_psystem_membrane {
uint32_t membrane_id;
dtesn_membrane_type_t membrane_type;
char label[DTESN_PSYSTEM_MAX_SYMBOL_LEN];
uint32_t parent_id;
uint32_t *children_ids;
uint32_t children_count;
uint32_t children_capacity;
uint32_t depth_level;
dtesn_psystem_multiset_t objects;
dtesn_psystem_rule_t *rules;
uint32_t rule_count;
uint32_t neuron_count;
float spectral_radius;
float leak_rate;
float connectivity;
dtesn_execution_phase_t current_phase;
uint64_t evolution_step;
bool is_dissolved;
bool is_active;
uint64_t total_evolution_time_ns;
uint64_t total_rule_applications;
uint64_t total_communications;
pthread_mutex_t lock;
pthread_cond_t evolution_cond;
} dtesn_psystem_membrane_t;
typedef struct dtesn_psystem {
char system_name[DTESN_PSYSTEM_MAX_SYMBOL_LEN];
uint32_t system_id;
dtesn_psystem_membrane_t **membranes;
uint32_t membrane_count;
uint32_t membrane_capacity;
uint32_t next_membrane_id;
uint32_t skin_membrane_id;
uint64_t global_evolution_step;
dtesn_execution_phase_t global_phase;
bool is_halted;
uint64_t total_system_evolution_time_ns;
uint64_t parallel_efficiency_pct;
pthread_mutex_t system_lock;
pthread_t *evolution_threads;
uint32_t thread_count;
void *memory_pool;
size_t memory_pool_size;
size_t memory_used;
} dtesn_psystem_t;
typedef struct dtesn_psystem_stats {
uint64_t total_evolution_time_ns;
uint64_t avg_evolution_time_ns;
uint64_t max_evolution_time_ns;
uint64_t total_rule_applications;
uint64_t avg_rule_time_ns;
uint64_t max_rule_time_ns;
uint64_t total_communications;
uint64_t avg_comm_time_ns;
uint64_t max_comm_time_ns;
uint32_t parallel_efficiency_pct;
uint32_t active_membranes;
uint32_t total_objects;
uint32_t total_rules;
bool meets_performance_targets;
} dtesn_psystem_stats_t;
int dtesn_psystem_init(void);
dtesn_psystem_t *dtesn_psystem_create(const char *system_name, uint32_t max_membranes);
void dtesn_psystem_destroy(dtesn_psystem_t *system);
uint32_t dtesn_membrane_create(dtesn_psystem_t *system,
dtesn_membrane_type_t membrane_type,
const char *label,
uint32_t parent_id,
uint32_t neuron_count);
int dtesn_membrane_destroy(dtesn_psystem_t *system, uint32_t membrane_id);
int dtesn_membrane_add_object(dtesn_psystem_t *system,
uint32_t membrane_id,
const char *symbol,
uint32_t multiplicity);
uint32_t dtesn_membrane_remove_object(dtesn_psystem_t *system,
uint32_t membrane_id,
const char *symbol,
uint32_t multiplicity);
uint32_t dtesn_membrane_add_rule(dtesn_psystem_t *system,
uint32_t membrane_id,
dtesn_rule_type_t rule_type,
uint8_t priority,
const char *rule_string,
dtesn_psystem_multiset_t *lhs_objects,
dtesn_psystem_multiset_t *rhs_objects,
uint32_t target_membrane_id);
int dtesn_membrane_evolve(dtesn_psystem_t *system, uint32_t membrane_id);
bool dtesn_system_evolve(dtesn_psystem_t *system);
int dtesn_membrane_communicate(dtesn_psystem_t *system,
uint32_t source_id,
uint32_t target_id,
dtesn_psystem_multiset_t *objects);
bool dtesn_psystem_validate_a000081(dtesn_psystem_t *system);
int dtesn_psystem_get_stats(dtesn_psystem_t *system, dtesn_psystem_stats_t *stats);
int dtesn_psystem_get_membrane_state(dtesn_psystem_t *system,
uint32_t membrane_id,
void *buffer,
size_t buffer_size);
dtesn_psystem_multiset_t *dtesn_multiset_create(void);
void dtesn_multiset_destroy(dtesn_psystem_multiset_t *multiset);
int dtesn_multiset_add(dtesn_psystem_multiset_t *multiset,
const char *symbol,
uint32_t multiplicity);
void dtesn_psystem_shutdown(void);
#define DTESN_PSYSTEM_ENOMEM -10
#define DTESN_PSYSTEM_EINVAL -11
#define DTESN_PSYSTEM_ENOTFOUND -12
#define DTESN_PSYSTEM_ELATENCY -13
#define DTESN_PSYSTEM_ETHREAD -14
#define DTESN_PSYSTEM_EVALIDATION -15
#define DTESN_PSYSTEM_ECOMMUNICATION -16
#define DTESN_PSYSTEM_EEVOLUTION -17
#ifdef __cplusplus
}
#endif
#endif