#include <kern/dynamic_probes.h>
#include <kern/kalloc.h>
#include <kern/printf.h>
#include <kern/lock.h>
#include <string.h>
#define MAX_DYNAMIC_PROBES 128
typedef struct dynamic_probe_entry {
uint32_t probe_id;
dynamic_probe_config_t config;
dynamic_probe_status_t status;
boolean_t in_use;
} dynamic_probe_entry_t;
static struct {
dynamic_probe_entry_t probes[MAX_DYNAMIC_PROBES];
uint32_t next_index;
simple_lock_irq_data_t lock;
boolean_t initialized;
} dynamic_state;
typedef struct kernel_symbol {
const char *name;
vm_offset_t address;
} kernel_symbol_t;
static kernel_symbol_t kernel_symbols[] = {
{"thread_invoke", 0},
{"mach_msg_trap", 0},
{"vm_fault", 0},
{"task_create", 0},
{"task_terminate", 0},
{NULL, 0}
};
void
dynamic_probes_init(void)
{
int i;
for (i = 0; i < MAX_DYNAMIC_PROBES; i++) {
dynamic_state.probes[i].in_use = FALSE;
dynamic_state.probes[i].probe_id = 0;
memset(&dynamic_state.probes[i].config, 0, sizeof(dynamic_probe_config_t));
memset(&dynamic_state.probes[i].status, 0, sizeof(dynamic_probe_status_t));
}
dynamic_state.next_index = 0;
simple_lock_irq_init(&dynamic_state.lock);
dynamic_state.initialized = TRUE;
printf("Dynamic probe subsystem initialized (%d slots available)\n",
MAX_DYNAMIC_PROBES);
}
vm_offset_t
dynamic_probe_resolve_symbol(const char *module, const char *function)
{
int i;
for (i = 0; kernel_symbols[i].name != NULL; i++) {
if (strcmp(kernel_symbols[i].name, function) == 0) {
return kernel_symbols[i].address;
}
}
return 0;
}
uint32_t
dynamic_probe_create(const dynamic_probe_config_t *config)
{
spl_t s;
uint32_t probe_id = 0;
dynamic_probe_entry_t *entry = NULL;
vm_offset_t address;
int i;
if (!dynamic_state.initialized || !config) {
return 0;
}
s = simple_lock_irq(&dynamic_state.lock);
for (i = 0; i < MAX_DYNAMIC_PROBES; i++) {
if (!dynamic_state.probes[i].in_use) {
entry = &dynamic_state.probes[i];
break;
}
}
if (!entry) {
simple_unlock_irq(s, &dynamic_state.lock);
printf("Dynamic probes: No available slots\n");
return 0;
}
if (config->address != 0) {
address = config->address;
} else {
address = dynamic_probe_resolve_symbol(config->module_name,
config->function_name);
if (address == 0) {
simple_unlock_irq(s, &dynamic_state.lock);
printf("Dynamic probes: Could not resolve symbol %s\n",
config->function_name);
return 0;
}
}
probe_id = dtrace_probe_register(config->type, config->probe_name,
config->function_name, NULL);
if (probe_id == 0) {
simple_unlock_irq(s, &dynamic_state.lock);
printf("Dynamic probes: Failed to register DTrace probe\n");
return 0;
}
entry->probe_id = probe_id;
entry->config = *config;
entry->status.probe_id = probe_id;
entry->status.active = config->enabled;
entry->status.fire_count = 0;
entry->status.error_count = 0;
entry->status.actual_address = address;
entry->in_use = TRUE;
if (config->enabled) {
dtrace_probe_enable(probe_id);
}
simple_unlock_irq(s, &dynamic_state.lock);
printf("Dynamic probe created: ID=%u, name='%s', address=0x%lx\n",
probe_id, config->probe_name, address);
return probe_id;
}
boolean_t
dynamic_probe_destroy(uint32_t probe_id)
{
spl_t s;
boolean_t found = FALSE;
int i;
if (!dynamic_state.initialized) {
return FALSE;
}
s = simple_lock_irq(&dynamic_state.lock);
for (i = 0; i < MAX_DYNAMIC_PROBES; i++) {
if (dynamic_state.probes[i].in_use &&
dynamic_state.probes[i].probe_id == probe_id) {
dtrace_probe_remove(probe_id);
dynamic_state.probes[i].in_use = FALSE;
dynamic_state.probes[i].probe_id = 0;
memset(&dynamic_state.probes[i].config, 0,
sizeof(dynamic_probe_config_t));
memset(&dynamic_state.probes[i].status, 0,
sizeof(dynamic_probe_status_t));
found = TRUE;
break;
}
}
simple_unlock_irq(s, &dynamic_state.lock);
if (found) {
printf("Dynamic probe destroyed: ID=%u\n", probe_id);
}
return found;
}
uint32_t
dynamic_probe_list(dynamic_probe_status_t *probes, uint32_t max_probes)
{
spl_t s;
uint32_t count = 0;
int i;
if (!dynamic_state.initialized || !probes) {
return 0;
}
s = simple_lock_irq(&dynamic_state.lock);
for (i = 0; i < MAX_DYNAMIC_PROBES && count < max_probes; i++) {
if (dynamic_state.probes[i].in_use) {
probes[count] = dynamic_state.probes[i].status;
count++;
}
}
simple_unlock_irq(s, &dynamic_state.lock);
return count;
}
boolean_t
dynamic_probe_enable_at_address(vm_offset_t address, const char *name)
{
printf("Dynamic probe: Enable at address 0x%lx (name: %s)\n", address, name);
return TRUE;
}
boolean_t
dynamic_probe_disable_at_address(vm_offset_t address)
{
printf("Dynamic probe: Disable at address 0x%lx\n", address);
return TRUE;
}
boolean_t
dynamic_probe_install_at_address(vm_offset_t address, uint32_t probe_id)
{
printf("Installing probe %u at address 0x%lx\n", probe_id, address);
return TRUE;
}
boolean_t
dynamic_probe_remove_from_address(vm_offset_t address)
{
printf("Removing probe from address 0x%lx\n", address);
return TRUE;
}