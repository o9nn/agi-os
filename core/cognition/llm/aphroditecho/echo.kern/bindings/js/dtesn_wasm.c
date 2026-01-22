#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <emscripten/emscripten.h>
typedef struct {
uint32_t depth;
uint32_t max_order;
uint32_t neuron_count;
uint32_t membrane_count;
uint32_t input_dim;
uint32_t output_dim;
uint32_t flags;
char label[64];
} wasm_dtesn_params_t;
typedef struct {
uint32_t instance_id;
wasm_dtesn_params_t params;
float *reservoir_state;
float *output_weights;
uint32_t evolution_count;
uint64_t total_runtime_ms;
int is_valid;
} wasm_dtesn_handle_t;
static wasm_dtesn_handle_t g_instances[100];
static int g_next_instance_id = 1;
static int g_library_initialized = 0;
static const uint32_t oeis_a000081[] = {
0, 1, 1, 2, 4, 9, 20, 48, 115, 286, 719, 1842, 4766, 12486, 32973, 86810
};
static int find_free_instance_slot(void)
{
for (int i = 0; i < 100; i++) {
if (g_instances[i].instance_id == 0) {
return i;
}
}
return -1;
}
static wasm_dtesn_handle_t *find_instance(uint32_t instance_id)
{
for (int i = 0; i < 100; i++) {
if (g_instances[i].instance_id == instance_id) {
return &g_instances[i];
}
}
return NULL;
}
static int validate_oeis_compliance(uint32_t depth, uint32_t membrane_count)
{
if (depth >= sizeof(oeis_a000081) / sizeof(oeis_a000081[0])) {
return -1;
}
return (membrane_count == oeis_a000081[depth]) ? 0 : -1;
}
EMSCRIPTEN_KEEPALIVE
int wasm_dtesn_init(void)
{
if (g_library_initialized) {
return -1;
}
memset(g_instances, 0, sizeof(g_instances));
g_next_instance_id = 1;
g_library_initialized = 1;
printf("WASM DTESN library initialized\n");
return 0;
}
EMSCRIPTEN_KEEPALIVE
int wasm_dtesn_cleanup(void)
{
if (!g_library_initialized) {
return -1;
}
for (int i = 0; i < 100; i++) {
if (g_instances[i].instance_id != 0) {
if (g_instances[i].reservoir_state) {
free(g_instances[i].reservoir_state);
}
if (g_instances[i].output_weights) {
free(g_instances[i].output_weights);
}
memset(&g_instances[i], 0, sizeof(g_instances[i]));
}
}
g_library_initialized = 0;
printf("WASM DTESN library cleaned up\n");
return 0;
}
EMSCRIPTEN_KEEPALIVE
int wasm_dtesn_create(uint32_t depth, uint32_t max_order, uint32_t neuron_count,
uint32_t membrane_count, uint32_t input_dim, uint32_t output_dim,
uint32_t flags, const char *label)
{
if (!g_library_initialized) {
return -1;
}
if (depth < 1 || depth > 16 || max_order < 1 || max_order > 10) {
return -2;
}
if (neuron_count > 10000 || membrane_count > 1024) {
return -3;
}
if (flags & 0x0010) {
if (validate_oeis_compliance(depth, membrane_count) != 0) {
return -4;
}
}
int slot = find_free_instance_slot();
if (slot < 0) {
return -5;
}
wasm_dtesn_handle_t *instance = &g_instances[slot];
instance->instance_id = g_next_instance_id++;
instance->params.depth = depth;
instance->params.max_order = max_order;
instance->params.neuron_count = neuron_count;
instance->params.membrane_count = membrane_count;
instance->params.input_dim = input_dim;
instance->params.output_dim = output_dim;
instance->params.flags = flags;
strncpy(instance->params.label, label ? label : "wasm_instance", 63);
instance->params.label[63] = '\0';
instance->reservoir_state = calloc(neuron_count, sizeof(float));
if (!instance->reservoir_state) {
memset(instance, 0, sizeof(*instance));
return -6;
}
instance->output_weights = calloc(neuron_count * output_dim, sizeof(float));
if (!instance->output_weights) {
free(instance->reservoir_state);
memset(instance, 0, sizeof(*instance));
return -7;
}
for (uint32_t i = 0; i < neuron_count * output_dim; i++) {
instance->output_weights[i] = ((float)rand() / RAND_MAX) * 0.1f - 0.05f;
}
instance->evolution_count = 0;
instance->total_runtime_ms = 0;
instance->is_valid = 1;
printf("Created WASM DTESN instance %u\n", instance->instance_id);
return (int)instance->instance_id;
}
EMSCRIPTEN_KEEPALIVE
int wasm_dtesn_destroy(uint32_t instance_id)
{
if (!g_library_initialized) {
return -1;
}
wasm_dtesn_handle_t *instance = find_instance(instance_id);
if (!instance) {
return -2;
}
if (instance->reservoir_state) {
free(instance->reservoir_state);
}
if (instance->output_weights) {
free(instance->output_weights);
}
memset(instance, 0, sizeof(*instance));
printf("Destroyed WASM DTESN instance %u\n", instance_id);
return 0;
}
EMSCRIPTEN_KEEPALIVE
int wasm_dtesn_evolve(uint32_t instance_id, float *input, uint32_t input_size, uint32_t steps)
{
if (!g_library_initialized) {
return -1;
}
wasm_dtesn_handle_t *instance = find_instance(instance_id);
if (!instance || !instance->is_valid) {
return -2;
}
if (!input || input_size == 0 || input_size > instance->params.input_dim) {
return -3;
}
for (uint32_t step = 0; step < steps; step++) {
for (uint32_t i = 0; i < instance->params.neuron_count; i++) {
float input_contribution = 0.0f;
for (uint32_t j = 0; j < input_size; j++) {
input_contribution += input[j] * 0.1f;
}
instance->reservoir_state[i] = 0.95f * instance->reservoir_state[i] +
0.05f * tanhf(input_contribution + instance->reservoir_state[i] * 0.9f);
}
instance->evolution_count++;
}
return 0;
}
EMSCRIPTEN_KEEPALIVE
int wasm_dtesn_predict(uint32_t instance_id, float *input, uint32_t input_size,
float *output, uint32_t output_size)
{
if (!g_library_initialized) {
return -1;
}
wasm_dtesn_handle_t *instance = find_instance(instance_id);
if (!instance || !instance->is_valid) {
return -2;
}
if (!input || !output || input_size > instance->params.input_dim ||
output_size > instance->params.output_dim) {
return -3;
}
int evolve_result = wasm_dtesn_evolve(instance_id, input, input_size, 1);
if (evolve_result != 0) {
return evolve_result;
}
for (uint32_t i = 0; i < output_size; i++) {
output[i] = 0.0f;
for (uint32_t j = 0; j < instance->params.neuron_count; j++) {
output[i] += instance->reservoir_state[j] *
instance->output_weights[j * output_size + i];
}
output[i] = tanhf(output[i]);
}
return 0;
}
EMSCRIPTEN_KEEPALIVE
int wasm_dtesn_get_state_info(uint32_t instance_id, uint32_t *evolution_count,
uint64_t *total_runtime_ms, int *is_valid)
{
if (!g_library_initialized) {
return -1;
}
wasm_dtesn_handle_t *instance = find_instance(instance_id);
if (!instance) {
return -2;
}
if (evolution_count) *evolution_count = instance->evolution_count;
if (total_runtime_ms) *total_runtime_ms = instance->total_runtime_ms;
if (is_valid) *is_valid = instance->is_valid;
return 0;
}
EMSCRIPTEN_KEEPALIVE
int wasm_dtesn_membrane_create(uint32_t instance_id, uint32_t parent_id)
{
if (!g_library_initialized) {
return -1;
}
wasm_dtesn_handle_t *instance = find_instance(instance_id);
if (!instance || !instance->is_valid) {
return -2;
}
uint32_t new_count = instance->params.membrane_count + 1;
if (validate_oeis_compliance(instance->params.depth, new_count) != 0) {
return -3;
}
instance->params.membrane_count = new_count;
return (int)new_count;
}
EMSCRIPTEN_KEEPALIVE
int wasm_dtesn_bseries_get_tree_count(uint32_t order)
{
if (order < 1 || order >= sizeof(oeis_a000081) / sizeof(oeis_a000081[0])) {
return -1;
}
return (int)oeis_a000081[order];
}
EMSCRIPTEN_KEEPALIVE
int wasm_dtesn_bseries_compute(uint32_t instance_id, uint32_t order,
double *coefficients, uint32_t coeff_count,
double *results, uint32_t result_size)
{
if (!g_library_initialized) {
return -1;
}
wasm_dtesn_handle_t *instance = find_instance(instance_id);
if (!instance || !instance->is_valid) {
return -2;
}
if (order < 1 || order > instance->params.max_order) {
return -3;
}
if (!coefficients || !results || coeff_count == 0) {
return -4;
}
int tree_count = wasm_dtesn_bseries_get_tree_count(order);
if (tree_count < 0 || result_size < (uint32_t)tree_count) {
return -5;
}
for (uint32_t i = 0; i < (uint32_t)tree_count; i++) {
results[i] = 0.0;
for (uint32_t j = 0; j < coeff_count && j < order; j++) {
double tree_weight = 1.0 / (i + 1.0);
results[i] += coefficients[j] * tree_weight;
}
}
return tree_count;
}
EMSCRIPTEN_KEEPALIVE
char *wasm_get_version_string(void)
{
static char version[] = "1.0.0-wasm";
return version;
}
EMSCRIPTEN_KEEPALIVE
int wasm_get_instance_count(void)
{
if (!g_library_initialized) {
return -1;
}
int count = 0;
for (int i = 0; i < 100; i++) {
if (g_instances[i].instance_id != 0) {
count++;
}
}
return count;
}
EMSCRIPTEN_KEEPALIVE
int wasm_validate_oeis_sequence(uint32_t depth, uint32_t membrane_count)
{
return validate_oeis_compliance(depth, membrane_count);
}
EMSCRIPTEN_KEEPALIVE
float *wasm_alloc_float_array(uint32_t size)
{
return malloc(size * sizeof(float));
}
EMSCRIPTEN_KEEPALIVE
double *wasm_alloc_double_array(uint32_t size)
{
return malloc(size * sizeof(double));
}
EMSCRIPTEN_KEEPALIVE
void wasm_free_array(void *ptr)
{
if (ptr) {
free(ptr);
}
}
EMSCRIPTEN_KEEPALIVE
void wasm_set_debug_level(int level)
{
printf("WASM DTESN debug level set to %d\n", level);
}
EMSCRIPTEN_KEEPALIVE
void wasm_print_instance_info(uint32_t instance_id)
{
wasm_dtesn_handle_t *instance = find_instance(instance_id);
if (!instance) {
printf("Instance %u not found\n", instance_id);
return;
}
printf("Instance %u:\n", instance->instance_id);
printf("  Label: %s\n", instance->params.label);
printf("  Depth: %u, Order: %u\n", instance->params.depth, instance->params.max_order);
printf("  Neurons: %u, Membranes: %u\n", instance->params.neuron_count, instance->params.membrane_count);
printf("  Input/Output: %u/%u\n", instance->params.input_dim, instance->params.output_dim);
printf("  Evolution count: %u\n", instance->evolution_count);
printf("  Valid: %s\n", instance->is_valid ? "Yes" : "No");
}
int main(void)
{
printf("WASM DTESN library compiled and ready\n");
return 0;
}