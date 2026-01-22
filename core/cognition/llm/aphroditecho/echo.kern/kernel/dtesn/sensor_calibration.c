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
#define DTESN_CALIBRATION_HISTORY_SIZE          100
#define DTESN_CALIBRATION_MIN_SAMPLES           10
#define DTESN_CALIBRATION_ADAPTATION_RATE       0.01f
#define DTESN_CALIBRATION_NOISE_THRESHOLD       0.8f
#define DTESN_CALIBRATION_RECALIBRATE_INTERVAL  1000000000ULL
typedef enum {
DTESN_NOISE_GAUSSIAN = 0,
DTESN_NOISE_UNIFORM = 1,
DTESN_NOISE_IMPULSE = 2,
DTESN_NOISE_ADAPTIVE = 3
} dtesn_noise_model_type_t;
typedef struct dtesn_calibration_entry {
uint64_t timestamp_ns;
float signal_mean;
float signal_variance;
float noise_level;
float reliability_score;
} dtesn_calibration_entry_t;
typedef struct dtesn_sensor_calibration {
uint32_t sensor_id;
dtesn_noise_model_type_t noise_model;
float baseline_mean;
float baseline_variance;
float noise_variance;
float signal_to_noise_ratio;
float adaptation_rate;
float reliability_threshold;
uint64_t last_calibration_ns;
dtesn_calibration_entry_t *history;
uint32_t history_size;
uint32_t history_index;
float *noise_filter_kernel;
uint32_t filter_kernel_size;
float filter_cutoff_frequency;
uint64_t total_calibrations;
uint64_t successful_adaptations;
float average_reliability;
} dtesn_sensor_calibration_t;
static uint64_t get_time_ns(void);
static int estimate_noise_parameters(const dtesn_cognitive_modality_data_t *modality,
dtesn_sensor_calibration_t *calibration);
static int apply_adaptive_noise_filter(const dtesn_cognitive_modality_data_t *modality,
dtesn_sensor_calibration_t *calibration,
float *filtered_data);
static int update_calibration_history(dtesn_sensor_calibration_t *calibration,
float signal_mean, float signal_variance,
float noise_level, float reliability);
static float compute_signal_to_noise_ratio(const dtesn_cognitive_modality_data_t *modality);
static int adapt_sensor_parameters(dtesn_sensor_calibration_t *calibration,
const dtesn_cognitive_modality_data_t *modality);
static int generate_noise_filter_kernel(dtesn_sensor_calibration_t *calibration);
static float compute_adaptive_threshold(const dtesn_sensor_calibration_t *calibration);
static uint64_t get_time_ns(void) {
struct timespec ts;
if (clock_gettime(CLOCK_MONOTONIC, &ts) != 0) {
return 0;
}
return (uint64_t)ts.tv_sec * 1000000000ULL + (uint64_t)ts.tv_nsec;
}
dtesn_sensor_calibration_t *dtesn_sensor_calibration_create(uint32_t sensor_id,
dtesn_noise_model_type_t noise_model) {
dtesn_sensor_calibration_t *calibration = malloc(sizeof(dtesn_sensor_calibration_t));
if (!calibration) {
return NULL;
}
memset(calibration, 0, sizeof(dtesn_sensor_calibration_t));
calibration->sensor_id = sensor_id;
calibration->noise_model = noise_model;
calibration->adaptation_rate = DTESN_CALIBRATION_ADAPTATION_RATE;
calibration->reliability_threshold = 0.5f;
calibration->last_calibration_ns = get_time_ns();
calibration->history = calloc(DTESN_CALIBRATION_HISTORY_SIZE,
sizeof(dtesn_calibration_entry_t));
if (!calibration->history) {
free(calibration);
return NULL;
}
calibration->filter_kernel_size = 5;
calibration->filter_cutoff_frequency = 0.5f;
if (generate_noise_filter_kernel(calibration) != 0) {
free(calibration->history);
free(calibration);
return NULL;
}
printf("Created sensor calibration system for sensor %u with %s noise model\n",
sensor_id, (noise_model == DTESN_NOISE_ADAPTIVE) ? "adaptive" : "fixed");
return calibration;
}
void dtesn_sensor_calibration_destroy(dtesn_sensor_calibration_t *calibration) {
if (!calibration) {
return;
}
free(calibration->history);
free(calibration->noise_filter_kernel);
free(calibration);
}
int dtesn_sensor_calibrate(dtesn_sensor_calibration_t *calibration,
const dtesn_cognitive_modality_data_t *modality) {
if (!calibration || !modality || !modality->data || modality->data_size == 0) {
return -EINVAL;
}
uint64_t current_time = get_time_ns();
if (current_time - calibration->last_calibration_ns < DTESN_CALIBRATION_RECALIBRATE_INTERVAL &&
calibration->total_calibrations > 0) {
return 0;
}
int result = estimate_noise_parameters(modality, calibration);
if (result != 0) {
return result;
}
result = adapt_sensor_parameters(calibration, modality);
if (result != 0) {
return result;
}
calibration->last_calibration_ns = current_time;
calibration->total_calibrations++;
printf("Sensor %u calibrated: SNR=%.2f, reliability=%.2f\n",
calibration->sensor_id, calibration->signal_to_noise_ratio,
calibration->average_reliability);
return 0;
}
int dtesn_sensor_filter_noise(dtesn_sensor_calibration_t *calibration,
const dtesn_cognitive_modality_data_t *input_modality,
dtesn_cognitive_modality_data_t *filtered_modality) {
if (!calibration || !input_modality || !filtered_modality) {
return -EINVAL;
}
if (!input_modality->data || input_modality->data_size == 0) {
return -EINVAL;
}
if (!filtered_modality->data) {
filtered_modality->data = malloc(input_modality->data_size * sizeof(float));
if (!filtered_modality->data) {
return -ENOMEM;
}
filtered_modality->data_size = input_modality->data_size;
}
filtered_modality->modality_id = input_modality->modality_id;
strcpy(filtered_modality->name, input_modality->name);
filtered_modality->timestamp_ns = input_modality->timestamp_ns;
filtered_modality->valid = input_modality->valid;
int result = apply_adaptive_noise_filter(input_modality, calibration, filtered_modality->data);
if (result != 0) {
return result;
}
float noise_reduction_factor = 1.0f + calibration->signal_to_noise_ratio * 0.1f;
filtered_modality->confidence = fminf(1.0f, input_modality->confidence * noise_reduction_factor);
return 0;
}
static int estimate_noise_parameters(const dtesn_cognitive_modality_data_t *modality,
dtesn_sensor_calibration_t *calibration) {
float signal_mean = 0.0f;
float signal_variance = 0.0f;
for (uint32_t i = 0; i < modality->data_size; i++) {
signal_mean += modality->data[i];
}
signal_mean /= modality->data_size;
for (uint32_t i = 0; i < modality->data_size; i++) {
float diff = modality->data[i] - signal_mean;
signal_variance += diff * diff;
}
signal_variance /= modality->data_size;
float noise_estimate = 0.0f;
if (modality->data_size > 2) {
for (uint32_t i = 1; i < modality->data_size - 1; i++) {
float laplacian = fabsf(modality->data[i-1] - 2.0f * modality->data[i] + modality->data[i+1]);
noise_estimate += laplacian;
}
noise_estimate /= (modality->data_size - 2);
}
if (calibration->total_calibrations == 0) {
calibration->baseline_mean = signal_mean;
calibration->baseline_variance = signal_variance;
}
if (noise_estimate > 0.0f) {
calibration->signal_to_noise_ratio = signal_variance / noise_estimate;
} else {
calibration->signal_to_noise_ratio = 100.0f;
}
float reliability = 1.0f / (1.0f + noise_estimate);
reliability *= modality->confidence;
update_calibration_history(calibration, signal_mean, signal_variance,
noise_estimate, reliability);
return 0;
}
static int apply_adaptive_noise_filter(const dtesn_cognitive_modality_data_t *modality,
dtesn_sensor_calibration_t *calibration,
float *filtered_data) {
int half_kernel = calibration->filter_kernel_size / 2;
for (uint32_t i = 0; i < modality->data_size; i++) {
filtered_data[i] = 0.0f;
float norm_factor = 0.0f;
for (int k = -half_kernel; k <= half_kernel; k++) {
int idx = (int)i + k;
if (idx >= 0 && idx < (int)modality->data_size) {
float kernel_weight = calibration->noise_filter_kernel[k + half_kernel];
filtered_data[i] += kernel_weight * modality->data[idx];
norm_factor += kernel_weight;
}
}
if (norm_factor > 0.0f) {
filtered_data[i] /= norm_factor;
} else {
filtered_data[i] = modality->data[i];
}
}
if (calibration->noise_model == DTESN_NOISE_ADAPTIVE) {
float adaptive_threshold = compute_adaptive_threshold(calibration);
if (calibration->signal_to_noise_ratio < 5.0f) {
for (uint32_t i = 1; i < modality->data_size - 1; i++) {
float values[3] = {filtered_data[i-1], filtered_data[i], filtered_data[i+1]};
for (int j = 0; j < 2; j++) {
for (int k = 0; k < 2-j; k++) {
if (values[k] > values[k+1]) {
float temp = values[k];
values[k] = values[k+1];
values[k+1] = temp;
}
}
}
if (fabsf(filtered_data[i] - values[1]) > adaptive_threshold) {
filtered_data[i] = values[1];
}
}
}
}
return 0;
}
static int update_calibration_history(dtesn_sensor_calibration_t *calibration,
float signal_mean, float signal_variance,
float noise_level, float reliability) {
uint32_t idx = calibration->history_index % DTESN_CALIBRATION_HISTORY_SIZE;
calibration->history[idx].timestamp_ns = get_time_ns();
calibration->history[idx].signal_mean = signal_mean;
calibration->history[idx].signal_variance = signal_variance;
calibration->history[idx].noise_level = noise_level;
calibration->history[idx].reliability_score = reliability;
calibration->history_index++;
if (calibration->history_size < DTESN_CALIBRATION_HISTORY_SIZE) {
calibration->history_size++;
}
float total_reliability = 0.0f;
for (uint32_t i = 0; i < calibration->history_size; i++) {
total_reliability += calibration->history[i].reliability_score;
}
calibration->average_reliability = total_reliability / calibration->history_size;
return 0;
}
static int adapt_sensor_parameters(dtesn_sensor_calibration_t *calibration,
const dtesn_cognitive_modality_data_t *modality) {
if (calibration->history_size < DTESN_CALIBRATION_MIN_SAMPLES) {
return 0;
}
float recent_reliability = 0.0f;
float older_reliability = 0.0f;
uint32_t half_history = calibration->history_size / 2;
for (uint32_t i = half_history; i < calibration->history_size; i++) {
uint32_t idx = (calibration->history_index - calibration->history_size + i) % DTESN_CALIBRATION_HISTORY_SIZE;
recent_reliability += calibration->history[idx].reliability_score;
}
recent_reliability /= (calibration->history_size - half_history);
for (uint32_t i = 0; i < half_history; i++) {
uint32_t idx = (calibration->history_index - calibration->history_size + i) % DTESN_CALIBRATION_HISTORY_SIZE;
older_reliability += calibration->history[idx].reliability_score;
}
older_reliability /= half_history;
float reliability_trend = recent_reliability - older_reliability;
if (reliability_trend < -0.1f) {
calibration->filter_cutoff_frequency *= 0.95f;
calibration->adaptation_rate *= 1.05f;
generate_noise_filter_kernel(calibration);
printf("Sensor %u: Reliability declining (%.3f), increasing filtering\n",
calibration->sensor_id, reliability_trend);
} else if (reliability_trend > 0.1f) {
calibration->filter_cutoff_frequency *= 1.02f;
calibration->filter_cutoff_frequency = fminf(calibration->filter_cutoff_frequency, 0.8f);
generate_noise_filter_kernel(calibration);
printf("Sensor %u: Reliability improving (%.3f), reducing filtering\n",
calibration->sensor_id, reliability_trend);
}
calibration->successful_adaptations++;
return 0;
}
static int generate_noise_filter_kernel(dtesn_sensor_calibration_t *calibration) {
if (calibration->noise_filter_kernel) {
free(calibration->noise_filter_kernel);
}
calibration->noise_filter_kernel = malloc(calibration->filter_kernel_size * sizeof(float));
if (!calibration->noise_filter_kernel) {
return -ENOMEM;
}
int half_kernel = calibration->filter_kernel_size / 2;
float sum = 0.0f;
for (int i = 0; i < calibration->filter_kernel_size; i++) {
int offset = i - half_kernel;
float x = M_PI * calibration->filter_cutoff_frequency * offset;
float sinc_val = (offset == 0) ? 1.0f : sinf(x) / x;
float window = 0.5f * (1.0f + cosf(2.0f * M_PI * i / (calibration->filter_kernel_size - 1)));
calibration->noise_filter_kernel[i] = sinc_val * window;
sum += calibration->noise_filter_kernel[i];
}
for (int i = 0; i < calibration->filter_kernel_size; i++) {
calibration->noise_filter_kernel[i] /= sum;
}
return 0;
}
static float compute_adaptive_threshold(const dtesn_sensor_calibration_t *calibration) {
if (calibration->history_size == 0) {
return 0.1f;
}
float mean_noise = 0.0f;
for (uint32_t i = 0; i < calibration->history_size; i++) {
mean_noise += calibration->history[i].noise_level;
}
mean_noise /= calibration->history_size;
float noise_variance = 0.0f;
for (uint32_t i = 0; i < calibration->history_size; i++) {
float diff = calibration->history[i].noise_level - mean_noise;
noise_variance += diff * diff;
}
noise_variance /= calibration->history_size;
return mean_noise + 2.0f * sqrtf(noise_variance);
}
int dtesn_sensor_calibration_get_stats(const dtesn_sensor_calibration_t *calibration,
void *stats_buffer, size_t buffer_size) {
if (!calibration || !stats_buffer || buffer_size == 0) {
return -EINVAL;
}
int written = snprintf((char*)stats_buffer, buffer_size,
"Sensor %u Calibration Stats:\n"
"  Total calibrations: %lu\n"
"  Successful adaptations: %lu\n"
"  Average reliability: %.3f\n"
"  Current SNR: %.2f\n"
"  Filter cutoff: %.3f\n"
"  Adaptation rate: %.4f\n",
calibration->sensor_id,
calibration->total_calibrations,
calibration->successful_adaptations,
calibration->average_reliability,
calibration->signal_to_noise_ratio,
calibration->filter_cutoff_frequency,
calibration->adaptation_rate);
return (written < (int)buffer_size) ? 0 : -ENOSPC;
}