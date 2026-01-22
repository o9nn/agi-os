#ifndef _UAPI_DTESN_H
#define _UAPI_DTESN_H
#include <linux/types.h>
#include <linux/ioctl.h>
#ifdef __cplusplus
extern "C" {
#endif
#define __NR_sys_dtesn_create       600
#define __NR_sys_dtesn_evolve       601
#define __NR_sys_dtesn_get_state    602
#define __NR_sys_dtesn_destroy      603
#define __NR_sys_membrane_op        604
#define __NR_sys_bseries_compute    605
#define __NR_sys_esn_update         606
#define DTESN_MAX_DEPTH             16
#define DTESN_MAX_ORDER             10
#define DTESN_MAX_NEURONS           10000
#define DTESN_MAX_MEMBRANES         1024
#define DTESN_MAX_INPUT_SIZE        1000
#define DTESN_MAX_OUTPUT_SIZE       1000
#define DTESN_MAX_LABEL_LEN         64
#define DTESN_CREATE_DEFAULT        0x0000
#define DTESN_CREATE_HARDWARE_ACCEL 0x0001
#define DTESN_CREATE_HIGH_PRECISION 0x0002
#define DTESN_CREATE_SPARSE_MODE    0x0004
#define DTESN_CREATE_REAL_TIME      0x0008
#define DTESN_CREATE_VALIDATE_OEIS  0x0010
#define DTESN_EVOLVE_SYNCHRONOUS    0x0000
#define DTESN_EVOLVE_ASYNCHRONOUS   0x0001
#define DTESN_EVOLVE_CONTINUOUS     0x0002
#define DTESN_EVOLVE_STEP_BY_STEP   0x0004
#define DTESN_MEMBRANE_CREATE       1
#define DTESN_MEMBRANE_EVOLVE       2
#define DTESN_MEMBRANE_COMMUNICATE  3
#define DTESN_MEMBRANE_DISSOLVE     4
#define DTESN_MEMBRANE_DIVIDE       5
#define DTESN_SUCCESS               0
#define DTESN_ERROR_INVALID_DEPTH   -1001
#define DTESN_ERROR_INVALID_ORDER   -1002
#define DTESN_ERROR_OEIS_VIOLATION  -1003
#define DTESN_ERROR_PERFORMANCE     -1004
#define DTESN_ERROR_HARDWARE        -1005
#define DTESN_ERROR_MEMBRANE        -1006
#define DTESN_ERROR_ESN             -1007
#define DTESN_ERROR_BSERIES         -1008
struct dtesn_create_params {
__u32 depth;
__u32 max_order;
__u32 neuron_count;
__u32 membrane_count;
__u32 input_dim;
__u32 output_dim;
__u32 flags;
char label[DTESN_MAX_LABEL_LEN];
};
struct dtesn_evolve_params {
__s32 fd;
const float *input;
__u32 input_size;
__u32 steps;
__u32 mode;
__u64 timeout_ns;
};
struct dtesn_state_info {
__u32 depth;
__u32 active_membranes;
__u32 total_neurons;
__u32 evolution_steps;
__u64 creation_time_ns;
__u64 last_update_ns;
double spectral_radius;
double membrane_activity;
__u32 oeis_compliance;
__u32 performance_violations;
};
struct dtesn_membrane_op_params {
__s32 fd;
__u32 operation;
__u32 membrane_id;
__u32 parent_id;
__u32 steps;
const void *data;
__u32 data_size;
};
struct dtesn_membrane_params {
__s32 fd;
__u32 operation;
__u32 membrane_id;
__u32 parent_id;
__u32 steps;
const void *data;
__u32 data_size;
};
struct dtesn_bseries_params {
__s32 fd;
__u32 order;
const double *coefficients;
__u32 coeff_count;
double *result;
__u32 result_size;
__u32 tree_count;
};
struct dtesn_esn_params {
__s32 fd;
const float *input;
__u32 input_size;
float *state;
__u32 state_size;
float *output;
__u32 output_size;
float learning_rate;
float regularization;
};
struct dtesn_device_info {
__u32 device_id;
__u32 device_type;
__u32 compute_units;
__u64 memory_size_bytes;
__u32 max_frequency_mhz;
__u32 capabilities;
char name[64];
char vendor[32];
};
struct dtesn_performance_metrics {
__u64 syscall_overhead_ns;
__u64 validation_time_ns;
__u64 copy_bandwidth_bps;
__u64 error_path_time_ns;
__u64 evolution_time_ns;
__u64 membrane_op_time_ns;
__u64 bseries_comp_time_ns;
__u64 esn_update_time_ns;
__u32 cache_hits;
__u32 cache_misses;
__u32 hw_accelerations;
};
#define DTESN_IOC_MAGIC             'D'
#define DTESN_IOC_GET_METRICS       _IOR(DTESN_IOC_MAGIC, 1, struct dtesn_performance_metrics)
#define DTESN_IOC_SET_DEBUG         _IOW(DTESN_IOC_MAGIC, 2, __u32)
#define DTESN_IOC_VALIDATE_OEIS     _IO(DTESN_IOC_MAGIC, 3)
#define DTESN_IOC_RESET_STATS       _IO(DTESN_IOC_MAGIC, 4)
#define DTESN_IOC_GET_VERSION       _IOR(DTESN_IOC_MAGIC, 5, __u32)
#define DTESN_API_VERSION_MAJOR     1
#define DTESN_API_VERSION_MINOR     0
#define DTESN_API_VERSION_PATCH     0
#define DTESN_API_VERSION           ((DTESN_API_VERSION_MAJOR << 16) | \
(DTESN_API_VERSION_MINOR << 8) | \
DTESN_API_VERSION_PATCH)
#define DTESN_VALID_DEPTH(d)        ((d) >= 1 && (d) <= DTESN_MAX_DEPTH)
#define DTESN_VALID_ORDER(o)        ((o) >= 1 && (o) <= DTESN_MAX_ORDER)
#define DTESN_VALID_NEURONS(n)      ((n) >= 1 && (n) <= DTESN_MAX_NEURONS)
#define DTESN_VALID_MEMBRANES(m)    ((m) >= 1 && (m) <= DTESN_MAX_MEMBRANES)
#define DTESN_VALID_INPUT_SIZE(s)   ((s) >= 1 && (s) <= DTESN_MAX_INPUT_SIZE)
#define DTESN_VALID_OUTPUT_SIZE(s)  ((s) >= 1 && (s) <= DTESN_MAX_OUTPUT_SIZE)
#define DTESN_OEIS_A000081_SEQUENCE_INIT \
{ 0, 1, 1, 2, 4, 9, 20, 48, 115, 286, 719, 1842, 4766, 12486, 32973, 86810 }
#ifdef __cplusplus
}
#endif
#endif