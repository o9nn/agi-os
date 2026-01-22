#ifndef DTESN_NEURO_HAL_H
#define DTESN_NEURO_HAL_H
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <pthread.h>
#ifdef __cplusplus
extern "C" {
#endif
#define DTESN_NEURO_MAX_DEVICES 16
#define DTESN_NEURO_MAX_EVENTS 1000000
#define DTESN_NEURO_MAX_CHANNELS 256
#define DTESN_NEURO_MAX_CORES 1024
#define DTESN_NEURO_MAX_SPIKE_RATE 100000
#define DTESN_NEURO_EVENT_QUEUE_SIZE 4096
#define DTESN_NEURO_EVENT_LATENCY_THRESHOLD_US 1
#define DTESN_NEURO_THROUGHPUT_THRESHOLD_HZ 1000000
#define DTESN_NEURO_POWER_EFFICIENCY_MW_GOPS 10
#define DTESN_NEURO_CONTEXT_SWITCH_THRESHOLD_US 5
#define DTESN_NEURO_A000081_MAX_DEPTH 12
#define DTESN_NEURO_A000081_SEQUENCE \
{ 1, 1, 2, 4, 9, 20, 48, 115, 286, 719, 1842, 4766 }
typedef enum {
DTESN_NEURO_DEVICE_UNKNOWN = 0,
DTESN_NEURO_DEVICE_LOIHI = 1,
DTESN_NEURO_DEVICE_SPINNAKER = 2,
DTESN_NEURO_DEVICE_AKIDA = 3,
DTESN_NEURO_DEVICE_DYNAP = 4,
DTESN_NEURO_DEVICE_TRUENORTH = 5,
DTESN_NEURO_DEVICE_GENERIC = 6
} dtesn_neuro_device_type_t;
typedef enum {
DTESN_NEURO_STATE_UNINITIALIZED = 0,
DTESN_NEURO_STATE_INITIALIZING = 1,
DTESN_NEURO_STATE_READY = 2,
DTESN_NEURO_STATE_ACTIVE = 3,
DTESN_NEURO_STATE_SUSPENDED = 4,
DTESN_NEURO_STATE_ERROR = 5
} dtesn_neuro_state_t;
typedef enum {
DTESN_NEURO_EVENT_SPIKE = 0,
DTESN_NEURO_EVENT_SYNAPSE = 1,
DTESN_NEURO_EVENT_LEARNING = 2,
DTESN_NEURO_EVENT_RESET = 3,
DTESN_NEURO_EVENT_CONFIG = 4,
DTESN_NEURO_EVENT_TIMER = 5
} dtesn_neuro_event_type_t;
typedef enum {
DTESN_NEURO_POWER_FULL = 0,
DTESN_NEURO_POWER_REDUCED = 1,
DTESN_NEURO_POWER_SLEEP = 2,
DTESN_NEURO_POWER_SUSPEND = 3,
DTESN_NEURO_POWER_OFF = 4
} dtesn_neuro_power_mode_t;
typedef enum {
DTESN_NEURO_DMA_SYNCHRONOUS = 0,
DTESN_NEURO_DMA_ASYNCHRONOUS = 1,
DTESN_NEURO_DMA_SCATTER_GATHER = 2
} dtesn_neuro_dma_mode_t;
typedef struct dtesn_neuro_event {
dtesn_neuro_event_type_t type;
uint64_t timestamp_ns;
uint32_t source_id;
uint32_t target_id;
uint32_t channel;
float weight;
uint32_t flags;
void *data;
} dtesn_neuro_event_t;
typedef struct dtesn_neuro_capabilities {
uint32_t max_neurons;
uint32_t max_synapses;
uint32_t max_cores;
uint32_t max_spike_rate_hz;
bool supports_stdp;
bool supports_dma;
bool supports_power_mgmt;
bool supports_realtime;
uint32_t memory_size_bytes;
float power_consumption_mw;
float gops_rating;
} dtesn_neuro_capabilities_t;
typedef struct dtesn_neuro_config {
uint32_t num_neurons;
uint32_t num_cores;
uint32_t spike_threshold_mv;
uint32_t refractory_period_us;
float learning_rate;
uint32_t timestep_us;
dtesn_neuro_power_mode_t power_mode;
bool enable_learning;
bool enable_monitoring;
} dtesn_neuro_config_t;
typedef struct dtesn_neuro_stats {
uint64_t total_events_processed;
uint64_t total_spikes_generated;
uint64_t total_learning_updates;
uint64_t avg_event_latency_ns;
uint64_t max_event_latency_ns;
uint64_t min_event_latency_ns;
uint32_t throughput_events_per_sec;
uint32_t peak_throughput_events_per_sec;
float power_consumption_mw;
float power_efficiency_mw_gops;
uint64_t context_switch_count;
uint64_t avg_context_switch_time_ns;
uint64_t dma_transfers_completed;
uint64_t dma_bytes_transferred;
bool event_latency_threshold_met;
bool throughput_threshold_met;
bool power_efficiency_threshold_met;
bool context_switch_threshold_met;
} dtesn_neuro_stats_t;
typedef struct dtesn_neuro_device {
uint32_t device_id;
char name[64];
dtesn_neuro_device_type_t type;
dtesn_neuro_state_t state;
dtesn_neuro_capabilities_t caps;
dtesn_neuro_config_t config;
dtesn_neuro_stats_t stats;
void *hw_context;
void *driver_context;
dtesn_neuro_event_t *event_queue;
uint32_t event_queue_head;
uint32_t event_queue_tail;
uint32_t event_queue_size;
void *dma_buffer;
size_t dma_buffer_size;
bool dma_in_progress;
pthread_mutex_t device_lock;
pthread_cond_t event_cond;
pthread_t event_thread;
uint32_t tree_depth;
bool oeis_validated;
uint64_t creation_time_ns;
uint64_t last_activity_ns;
} dtesn_neuro_device_t;
typedef struct dtesn_neuro_driver {
dtesn_neuro_device_type_t type;
char name[64];
int (*probe)(dtesn_neuro_device_t *device);
int (*init)(dtesn_neuro_device_t *device);
int (*start)(dtesn_neuro_device_t *device);
int (*stop)(dtesn_neuro_device_t *device);
int (*reset)(dtesn_neuro_device_t *device);
int (*configure)(dtesn_neuro_device_t *device, const dtesn_neuro_config_t *config);
int (*send_event)(dtesn_neuro_device_t *device, const dtesn_neuro_event_t *event);
int (*receive_event)(dtesn_neuro_device_t *device, dtesn_neuro_event_t *event);
int (*process_events)(dtesn_neuro_device_t *device);
int (*dma_transfer)(dtesn_neuro_device_t *device, void *src, void *dst,
size_t size, dtesn_neuro_dma_mode_t mode);
int (*dma_status)(dtesn_neuro_device_t *device, bool *complete, size_t *transferred);
int (*set_power_mode)(dtesn_neuro_device_t *device, dtesn_neuro_power_mode_t mode);
int (*get_power_stats)(dtesn_neuro_device_t *device, float *power_mw, float *efficiency);
int (*get_stats)(dtesn_neuro_device_t *device, dtesn_neuro_stats_t *stats);
int (*reset_stats)(dtesn_neuro_device_t *device);
} dtesn_neuro_driver_t;
typedef struct dtesn_neuro_dma_desc {
void *src_addr;
void *dst_addr;
size_t size;
dtesn_neuro_dma_mode_t mode;
uint32_t flags;
void (*callback)(void *context);
void *callback_context;
} dtesn_neuro_dma_desc_t;
typedef void (*dtesn_neuro_event_callback_t)(dtesn_neuro_device_t *device,
const dtesn_neuro_event_t *event,
void *context);
typedef void (*dtesn_neuro_power_callback_t)(dtesn_neuro_device_t *device,
dtesn_neuro_power_mode_t old_mode,
dtesn_neuro_power_mode_t new_mode,
void *context);
int neuro_hal_init(void);
void neuro_hal_shutdown(void);
int neuro_device_register(const dtesn_neuro_driver_t *driver);
int neuro_device_unregister(dtesn_neuro_device_type_t type);
int neuro_device_discover(dtesn_neuro_device_t *devices, uint32_t max_devices);
dtesn_neuro_device_t *neuro_device_get_by_id(uint32_t device_id);
dtesn_neuro_device_t *neuro_device_get_by_type(dtesn_neuro_device_type_t type);
int neuro_device_open(dtesn_neuro_device_t *device, const dtesn_neuro_config_t *config);
int neuro_device_close(dtesn_neuro_device_t *device);
int neuro_event_process(dtesn_neuro_device_t *device,
const dtesn_neuro_event_t *events,
uint32_t num_events);
int neuro_event_send(dtesn_neuro_device_t *device, const dtesn_neuro_event_t *event);
int neuro_event_receive(dtesn_neuro_device_t *device,
dtesn_neuro_event_t *event,
uint32_t timeout_us);
int neuro_event_register_callback(dtesn_neuro_device_t *device,
dtesn_neuro_event_callback_t callback,
void *context);
int neuro_dma_transfer(dtesn_neuro_device_t *device, const dtesn_neuro_dma_desc_t *desc);
int neuro_dma_status(dtesn_neuro_device_t *device, bool *complete, size_t *bytes_transferred);
int neuro_dma_wait(dtesn_neuro_device_t *device, uint32_t timeout_us);
int neuro_power_manage(dtesn_neuro_device_t *device, dtesn_neuro_power_mode_t mode);
int neuro_power_get_stats(dtesn_neuro_device_t *device,
float *power_mw,
float *efficiency_mw_gops);
int neuro_power_register_callback(dtesn_neuro_device_t *device,
dtesn_neuro_power_callback_t callback,
void *context);
bool neuro_device_validate_a000081(const dtesn_neuro_device_t *devices, uint32_t num_devices);
int neuro_device_get_stats(dtesn_neuro_device_t *device, dtesn_neuro_stats_t *stats);
int neuro_device_reset_stats(dtesn_neuro_device_t *device);
bool neuro_hal_check_thresholds(dtesn_neuro_device_t *device);
int neuro_config_default(dtesn_neuro_config_t *config, dtesn_neuro_device_type_t type);
dtesn_neuro_event_t neuro_event_create(dtesn_neuro_event_type_t type,
uint32_t source_id,
uint32_t target_id,
float weight);
const char *neuro_device_type_name(dtesn_neuro_device_type_t type);
void neuro_hal_get_version(uint32_t *major, uint32_t *minor, uint32_t *patch);
#define DTESN_NEURO_ENOMEM -50
#define DTESN_NEURO_EINVAL -51
#define DTESN_NEURO_ENOTFOUND -52
#define DTESN_NEURO_EBUSY -53
#define DTESN_NEURO_ETIMEDOUT -54
#define DTESN_NEURO_ELATENCY -55
#define DTESN_NEURO_ETHROUGHPUT -56
#define DTESN_NEURO_EPOWER -57
#define DTESN_NEURO_EVALIDATION -58
#define DTESN_NEURO_EHARDWARE -59
#define DTESN_NEURO_EDRIVER -60
#define DTESN_NEURO_EDMA -61
#define DTESN_NEURO_EEVENT -62
#define DTESN_NEURO_ENOTSUPPORTED -63
#ifdef __cplusplus
}
#endif
#endif