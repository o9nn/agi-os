#define _GNU_SOURCE
#define _POSIX_C_SOURCE 199309L
#include "include/dtesn/dtesn_cognitive.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <time.h>
#include <errno.h>
#include <math.h>
#include <float.h>
#define DTESN_ATTENTION_DEFAULT_FOCUS_SIZE 256
#define DTESN_ATTENTION_COMPETITIVE_THRESHOLD 0.1f
#define DTESN_ATTENTION_COOPERATIVE_WEIGHT 0.8f
#define DTESN_ATTENTION_DECAY_RATE 0.95f
#define DTESN_ATTENTION_MIN_WEIGHT 0.001f
#define DTESN_ATTENTION_MAX_FOCUS_HISTORY 10
typedef struct attention_focus_history {
uint32_t channel_id;
uint64_t timestamp_ns;
float duration_ns;
float weight;
} attention_focus_history_t;
static uint64_t get_time_ns(void);
static int validate_system(const dtesn_cognitive_system_t *system);
static int validate_channel_id(const dtesn_cognitive_system_t *system, uint32_t channel_id);
static float compute_saliency_score(const float *input_data, uint32_t size);
static int apply_bottom_up_attention(dtesn_cognitive_system_t *system, uint32_t channel_id);
static int apply_top_down_attention(dtesn_cognitive_system_t *system, uint32_t channel_id,
const float *focus_vector, uint32_t focus_size);
static int apply_competitive_attention(dtesn_cognitive_system_t *system);
static int apply_cooperative_attention(dtesn_cognitive_system_t *system);
static int normalize_attention_weights(dtesn_cognitive_system_t *system);
static int update_focus_history(dtesn_cognitive_system_t *system, uint32_t prev_channel_id,
uint64_t switch_time);
static float compute_attention_efficiency(const dtesn_cognitive_system_t *system);
static int apply_attention_decay(dtesn_cognitive_system_t *system);
static uint64_t get_time_ns(void) {
struct timespec ts;
if (clock_gettime(CLOCK_MONOTONIC, &ts) != 0) {
return 0;
}
return (uint64_t)ts.tv_sec * 1000000000ULL + (uint64_t)ts.tv_nsec;
}
static int validate_system(const dtesn_cognitive_system_t *system) {
if (!system) {
return -EINVAL;
}
if (!system->initialized) {
return -ENODEV;
}
if (!system->attention_channels || system->num_attention_channels == 0) {
return -EINVAL;
}
return 0;
}
static int validate_channel_id(const dtesn_cognitive_system_t *system, uint32_t channel_id) {
if (channel_id >= system->num_attention_channels) {
return -EINVAL;
}
return 0;
}
static float compute_saliency_score(const float *input_data, uint32_t size) {
if (!input_data || size == 0) {
return 0.0f;
}
float mean = 0.0f;
float variance = 0.0f;
float max_val = input_data[0];
for (uint32_t i = 0; i < size; i++) {
mean += input_data[i];
if (input_data[i] > max_val) {
max_val = input_data[i];
}
}
mean /= size;
for (uint32_t i = 0; i < size; i++) {
float diff = input_data[i] - mean;
variance += diff * diff;
}
variance /= size;
float saliency = (0.6f * sqrtf(variance) + 0.4f * max_val);
return fmaxf(0.0f, fminf(1.0f, saliency));
}
static int apply_bottom_up_attention(dtesn_cognitive_system_t *system, uint32_t channel_id) {
dtesn_cognitive_attention_channel_t *channel = &system->attention_channels[channel_id];
if (system->reservoir && system->reservoir->u_current) {
uint32_t input_size = system->reservoir->config.input_size;
float saliency = compute_saliency_score(system->reservoir->u_current, input_size);
channel->weight = fminf(1.0f, channel->weight + 0.1f * saliency);
}
channel->type = DTESN_COGNITIVE_ATTENTION_BOTTOM_UP;
return 0;
}
static int apply_top_down_attention(dtesn_cognitive_system_t *system, uint32_t channel_id,
const float *focus_vector, uint32_t focus_size) {
dtesn_cognitive_attention_channel_t *channel = &system->attention_channels[channel_id];
if (channel->focus_vector && channel->focus_size != focus_size) {
free(channel->focus_vector);
channel->focus_vector = NULL;
}
if (!channel->focus_vector) {
channel->focus_vector = malloc(focus_size * sizeof(float));
if (!channel->focus_vector) {
return -ENOMEM;
}
channel->focus_size = focus_size;
}
if (focus_vector) {
memcpy(channel->focus_vector, focus_vector, focus_size * sizeof(float));
} else {
float center = focus_size / 2.0f;
float sigma = focus_size / 6.0f;
for (uint32_t i = 0; i < focus_size; i++) {
float x = (float)i - center;
channel->focus_vector[i] = expf(-x * x / (2.0f * sigma * sigma));
}
}
channel->weight = fminf(1.0f, channel->weight + 0.2f);
channel->type = DTESN_COGNITIVE_ATTENTION_TOP_DOWN;
return 0;
}
static int apply_competitive_attention(dtesn_cognitive_system_t *system) {
uint32_t winner_channel = 0;
float max_weight = 0.0f;
for (uint32_t i = 0; i < system->num_attention_channels; i++) {
if (system->attention_channels[i].weight > max_weight) {
max_weight = system->attention_channels[i].weight;
winner_channel = i;
}
}
for (uint32_t i = 0; i < system->num_attention_channels; i++) {
if (i == winner_channel) {
system->attention_channels[i].weight = 1.0f;
system->attention_channels[i].active = true;
system->attention_channels[i].type = DTESN_COGNITIVE_ATTENTION_COMPETITIVE;
} else {
system->attention_channels[i].weight *= (1.0f - DTESN_ATTENTION_COMPETITIVE_THRESHOLD);
system->attention_channels[i].active = false;
}
}
system->active_channel_id = winner_channel;
return 0;
}
static int apply_cooperative_attention(dtesn_cognitive_system_t *system) {
float total_weight = 0.0f;
for (uint32_t i = 0; i < system->num_attention_channels; i++) {
total_weight += system->attention_channels[i].weight;
}
if (total_weight <= 0.0f) {
return -EINVAL;
}
for (uint32_t i = 0; i < system->num_attention_channels; i++) {
float normalized_weight = system->attention_channels[i].weight / total_weight;
if (normalized_weight > DTESN_ATTENTION_MIN_WEIGHT) {
system->attention_channels[i].weight = normalized_weight * DTESN_ATTENTION_COOPERATIVE_WEIGHT;
system->attention_channels[i].active = true;
} else {
system->attention_channels[i].weight = DTESN_ATTENTION_MIN_WEIGHT;
system->attention_channels[i].active = false;
}
system->attention_channels[i].type = DTESN_COGNITIVE_ATTENTION_COOPERATIVE;
}
uint32_t active_channel = 0;
float max_weight = system->attention_channels[0].weight;
for (uint32_t i = 1; i < system->num_attention_channels; i++) {
if (system->attention_channels[i].weight > max_weight) {
max_weight = system->attention_channels[i].weight;
active_channel = i;
}
}
system->active_channel_id = active_channel;
return 0;
}
static int normalize_attention_weights(dtesn_cognitive_system_t *system) {
float total_weight = 0.0f;
for (uint32_t i = 0; i < system->num_attention_channels; i++) {
total_weight += system->attention_channels[i].weight;
}
if (total_weight <= 0.0f) {
float equal_weight = 1.0f / system->num_attention_channels;
for (uint32_t i = 0; i < system->num_attention_channels; i++) {
system->attention_channels[i].weight = equal_weight;
}
} else {
for (uint32_t i = 0; i < system->num_attention_channels; i++) {
system->attention_channels[i].weight /= total_weight;
}
}
return 0;
}
static int update_focus_history(dtesn_cognitive_system_t *system, uint32_t prev_channel_id,
uint64_t switch_time) {
if (prev_channel_id < system->num_attention_channels) {
system->attention_channels[prev_channel_id].switch_time_ns = switch_time;
}
return 0;
}
static float compute_attention_efficiency(const dtesn_cognitive_system_t *system) {
float entropy = 0.0f;
for (uint32_t i = 0; i < system->num_attention_channels; i++) {
float weight = system->attention_channels[i].weight;
if (weight > DTESN_ATTENTION_MIN_WEIGHT) {
entropy -= weight * log2f(weight);
}
}
float max_entropy = log2f((float)system->num_attention_channels);
float efficiency = 1.0f - (entropy / max_entropy);
return fmaxf(0.0f, fminf(1.0f, efficiency));
}
static int apply_attention_decay(dtesn_cognitive_system_t *system) {
uint64_t current_time = get_time_ns();
for (uint32_t i = 0; i < system->num_attention_channels; i++) {
dtesn_cognitive_attention_channel_t *channel = &system->attention_channels[i];
uint64_t time_since_update = current_time - channel->switch_time_ns;
float decay_seconds = time_since_update / 1000000000.0f;
float decay_factor = expf(-decay_seconds * (1.0f - DTESN_ATTENTION_DECAY_RATE));
channel->weight *= decay_factor;
if (channel->weight < DTESN_ATTENTION_MIN_WEIGHT) {
channel->weight = DTESN_ATTENTION_MIN_WEIGHT;
}
}
return 0;
}
int dtesn_attention_focus(dtesn_cognitive_system_t *system,
uint32_t channel_id,
const float *focus_vector,
uint32_t focus_size) {
int result;
uint64_t start_time, end_time;
uint32_t prev_channel_id;
result = validate_system(system);
if (result != 0) {
return result;
}
result = validate_channel_id(system, channel_id);
if (result != 0) {
return result;
}
start_time = get_time_ns();
pthread_mutex_lock(&system->attention_lock);
prev_channel_id = system->active_channel_id;
apply_attention_decay(system);
dtesn_cognitive_attention_channel_t *channel = &system->attention_channels[channel_id];
switch (channel->type) {
case DTESN_COGNITIVE_ATTENTION_BOTTOM_UP:
result = apply_bottom_up_attention(system, channel_id);
break;
case DTESN_COGNITIVE_ATTENTION_TOP_DOWN:
result = apply_top_down_attention(system, channel_id, focus_vector, focus_size);
break;
case DTESN_COGNITIVE_ATTENTION_COMPETITIVE:
result = apply_competitive_attention(system);
break;
case DTESN_COGNITIVE_ATTENTION_COOPERATIVE:
result = apply_cooperative_attention(system);
break;
default:
result = apply_top_down_attention(system, channel_id, focus_vector, focus_size);
break;
}
if (result == 0) {
system->active_channel_id = channel_id;
channel->active = true;
channel->switch_time_ns = get_time_ns();
update_focus_history(system, prev_channel_id, channel->switch_time_ns);
}
end_time = get_time_ns();
if (result == 0) {
system->total_attention_switches++;
system->total_attention_switch_time_ns += (end_time - start_time);
uint64_t switch_time_us = (end_time - start_time) / 1000;
if (switch_time_us > DTESN_COGNITIVE_ATTENTION_SWITCH_US) {
printf("Warning: Attention switch took %lu μs (target: ≤%u μs)\n",
(unsigned long)switch_time_us, DTESN_COGNITIVE_ATTENTION_SWITCH_US);
}
}
pthread_mutex_unlock(&system->attention_lock);
if (result == 0) {
printf("Attention focused on channel %u (%.2f μs, efficiency: %.2f)\n",
channel_id, (end_time - start_time) / 1000.0,
compute_attention_efficiency(system));
}
return result;
}
int dtesn_attention_distribute(dtesn_cognitive_system_t *system,
const float *weights,
uint32_t num_weights) {
int result;
uint64_t start_time, end_time;
float weight_sum = 0.0f;
result = validate_system(system);
if (result != 0) {
return result;
}
if (!weights || num_weights != system->num_attention_channels) {
return -EINVAL;
}
for (uint32_t i = 0; i < num_weights; i++) {
if (weights[i] < 0.0f || weights[i] > 1.0f) {
return -EINVAL;
}
weight_sum += weights[i];
}
if (fabsf(weight_sum - 1.0f) > 0.01f) {
return -EINVAL;
}
start_time = get_time_ns();
pthread_mutex_lock(&system->attention_lock);
apply_attention_decay(system);
uint32_t active_channel = 0;
float max_weight = 0.0f;
for (uint32_t i = 0; i < system->num_attention_channels; i++) {
system->attention_channels[i].weight = weights[i];
system->attention_channels[i].active = (weights[i] > DTESN_ATTENTION_MIN_WEIGHT);
system->attention_channels[i].switch_time_ns = get_time_ns();
if (weights[i] > max_weight) {
max_weight = weights[i];
active_channel = i;
}
}
system->active_channel_id = active_channel;
result = apply_cooperative_attention(system);
end_time = get_time_ns();
if (result == 0) {
system->total_attention_switches++;
system->total_attention_switch_time_ns += (end_time - start_time);
}
pthread_mutex_unlock(&system->attention_lock);
if (result == 0) {
printf("Attention distributed across %u channels (primary: %u, %.2f μs)\n",
system->num_attention_channels, active_channel,
(end_time - start_time) / 1000.0);
}
return result;
}