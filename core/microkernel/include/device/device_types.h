#ifndef DEVICE_TYPES_H
#define DEVICE_TYPES_H
#include <mach/std_types.h>
#ifdef MACH_KERNEL
#include <device/device_types_kernel.h>
#else
typedef mach_port_t device_t;
#endif
typedef char dev_name_t[128];
typedef const char *const_dev_name_t;
typedef unsigned int dev_mode_t;
#define D_READ 0x1
#define D_WRITE 0x2
#define D_NODELAY 0x4
#define D_NOWAIT 0x8
typedef char * io_buf_ptr_t;
typedef const char * const_io_buf_ptr_t;
#define IO_INBAND_MAX (128)
typedef char io_buf_ptr_inband_t[IO_INBAND_MAX];
typedef const char *const_io_buf_ptr_inband_t;
typedef struct {
vm_offset_t data;
vm_size_t count;
} io_buf_vec_t;
typedef struct {
rpc_vm_offset_t data;
rpc_vm_size_t count;
} rpc_io_buf_vec_t;
typedef long_natural_t recnum_t;
typedef rpc_long_natural_t rpc_recnum_t;
typedef unsigned int dev_flavor_t;
typedef int *dev_status_t;
#define DEV_STATUS_MAX (1024)
typedef int dev_status_data_t[DEV_STATUS_MAX];
#define DEV_GET_SIZE 0
# define DEV_GET_SIZE_DEVICE_SIZE 0
# define DEV_GET_SIZE_RECORD_SIZE 1
#define DEV_GET_SIZE_COUNT 2
#define DEV_GET_RECORDS 1
# define DEV_GET_RECORDS_DEVICE_RECORDS 0
# define DEV_GET_RECORDS_RECORD_SIZE 1
#define DEV_GET_RECORDS_COUNT 2
typedef int io_return_t;
#define D_IO_QUEUED (-1)
#define D_SUCCESS 0
#define D_IO_ERROR 2500
#define D_WOULD_BLOCK 2501
#define D_NO_SUCH_DEVICE 2502
#define D_ALREADY_OPEN 2503
#define D_DEVICE_DOWN 2504
#define D_INVALID_OPERATION 2505
#define D_INVALID_RECNUM 2506
#define D_INVALID_SIZE 2507
#define D_NO_MEMORY 2508
#define D_READ_ONLY 2509
void device_deallocate(device_t);
#endif