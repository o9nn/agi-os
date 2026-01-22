#ifndef _KERN_DYNAMIC_PROBES_H_
#define _KERN_DYNAMIC_PROBES_H_
#include <kern/dtrace.h>
#include <kern/kern_types.h>
typedef struct dynamic_probe_config {
const char *module_name;
const char *function_name;
const char *probe_name;
dtrace_probe_type_t type;
vm_offset_t address;
boolean_t enabled;
} dynamic_probe_config_t;
typedef struct dynamic_probe_status {
uint32_t probe_id;
boolean_t active;
uint64_t fire_count;
uint64_t error_count;
vm_offset_t actual_address;
} dynamic_probe_status_t;
void dynamic_probes_init(void);
uint32_t dynamic_probe_create(const dynamic_probe_config_t *config);
boolean_t dynamic_probe_destroy(uint32_t probe_id);
uint32_t dynamic_probe_list(dynamic_probe_status_t *probes, uint32_t max_probes);
boolean_t dynamic_probe_enable_at_address(vm_offset_t address, const char *name);
boolean_t dynamic_probe_disable_at_address(vm_offset_t address);
vm_offset_t dynamic_probe_resolve_symbol(const char *module, const char *function);
boolean_t dynamic_probe_install_at_address(vm_offset_t address, uint32_t probe_id);
boolean_t dynamic_probe_remove_from_address(vm_offset_t address);
#endif