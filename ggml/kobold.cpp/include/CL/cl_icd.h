#ifndef OPENCL_CL_ICD_H
#define OPENCL_CL_ICD_H
#include <CL/cl.h>
#include <CL/cl_egl.h>
#include <CL/cl_ext.h>
#include <CL/cl_gl.h>
#if defined(_WIN32)
#include <CL/cl_d3d11.h>
#include <CL/cl_d3d10.h>
#include <CL/cl_dx9_media_sharing.h>
#endif
#ifdef __cplusplus
extern "C" {
#endif
typedef cl_int(CL_API_CALL *cl_api_clGetPlatformIDs)(
cl_uint num_entries, cl_platform_id *platforms,
cl_uint *num_platforms) CL_API_SUFFIX__VERSION_1_0;
typedef cl_int(CL_API_CALL *cl_api_clGetPlatformInfo)(
cl_platform_id platform, cl_platform_info param_name,
size_t param_value_size, void *param_value,
size_t *param_value_size_ret) CL_API_SUFFIX__VERSION_1_0;
typedef cl_int(CL_API_CALL *cl_api_clGetDeviceIDs)(
cl_platform_id platform, cl_device_type device_type, cl_uint num_entries,
cl_device_id *devices, cl_uint *num_devices) CL_API_SUFFIX__VERSION_1_0;
typedef cl_int(CL_API_CALL *cl_api_clGetDeviceInfo)(
cl_device_id device, cl_device_info param_name, size_t param_value_size,
void *param_value, size_t *param_value_size_ret) CL_API_SUFFIX__VERSION_1_0;
#ifdef CL_VERSION_1_2
typedef cl_int(CL_API_CALL *cl_api_clCreateSubDevices)(
cl_device_id in_device,
const cl_device_partition_property *partition_properties,
cl_uint num_entries, cl_device_id *out_devices, cl_uint *num_devices);
typedef cl_int(CL_API_CALL *cl_api_clRetainDevice)(
cl_device_id device) CL_API_SUFFIX__VERSION_1_2;
typedef cl_int(CL_API_CALL *cl_api_clReleaseDevice)(
cl_device_id device) CL_API_SUFFIX__VERSION_1_2;
#else
typedef void *cl_api_clCreateSubDevices;
typedef void *cl_api_clRetainDevice;
typedef void *cl_api_clReleaseDevice;
#endif
typedef cl_context(CL_API_CALL *cl_api_clCreateContext)(
const cl_context_properties *properties, cl_uint num_devices,
const cl_device_id *devices,
void(CL_CALLBACK *pfn_notify)(const char *, const void *, size_t, void *),
void *user_data, cl_int *errcode_ret) CL_API_SUFFIX__VERSION_1_0;
typedef cl_context(CL_API_CALL *cl_api_clCreateContextFromType)(
const cl_context_properties *properties, cl_device_type device_type,
void(CL_CALLBACK *pfn_notify)(const char *, const void *, size_t, void *),
void *user_data, cl_int *errcode_ret) CL_API_SUFFIX__VERSION_1_0;
typedef cl_int(CL_API_CALL *cl_api_clRetainContext)(
cl_context context) CL_API_SUFFIX__VERSION_1_0;
typedef cl_int(CL_API_CALL *cl_api_clReleaseContext)(
cl_context context) CL_API_SUFFIX__VERSION_1_0;
typedef cl_int(CL_API_CALL *cl_api_clGetContextInfo)(
cl_context context, cl_context_info param_name, size_t param_value_size,
void *param_value, size_t *param_value_size_ret) CL_API_SUFFIX__VERSION_1_0;
typedef cl_command_queue(CL_API_CALL *cl_api_clCreateCommandQueue)(
cl_context context, cl_device_id device,
cl_command_queue_properties properties,
cl_int *errcode_ret) CL_API_SUFFIX__VERSION_1_0;
#ifdef CL_VERSION_2_0
typedef
cl_command_queue(CL_API_CALL *cl_api_clCreateCommandQueueWithProperties)(
cl_context , cl_device_id ,
const cl_queue_properties * ,
cl_int * ) CL_API_SUFFIX__VERSION_2_0;
#else
typedef void *cl_api_clCreateCommandQueueWithProperties;
#endif
typedef cl_int(CL_API_CALL *cl_api_clRetainCommandQueue)(
cl_command_queue command_queue) CL_API_SUFFIX__VERSION_1_0;
typedef cl_int(CL_API_CALL *cl_api_clReleaseCommandQueue)(
cl_command_queue command_queue) CL_API_SUFFIX__VERSION_1_0;
typedef cl_int(CL_API_CALL *cl_api_clGetCommandQueueInfo)(
cl_command_queue command_queue, cl_command_queue_info param_name,
size_t param_value_size, void *param_value,
size_t *param_value_size_ret) CL_API_SUFFIX__VERSION_1_0;
typedef cl_mem(CL_API_CALL *cl_api_clCreateBuffer)(
cl_context context, cl_mem_flags flags, size_t size, void *host_ptr,
cl_int *errcode_ret) CL_API_SUFFIX__VERSION_1_0;
#ifdef CL_VERSION_1_2
typedef cl_mem(CL_API_CALL *cl_api_clCreateImage)(
cl_context context, cl_mem_flags flags, const cl_image_format *image_format,
const cl_image_desc *image_desc, void *host_ptr,
cl_int *errcode_ret) CL_API_SUFFIX__VERSION_1_2;
#else
typedef void *cl_api_clCreateImage;
#endif
#ifdef CL_VERSION_3_0
typedef cl_mem(CL_API_CALL *cl_api_clCreateBufferWithProperties)(
cl_context context, const cl_mem_properties *properties, cl_mem_flags flags,
size_t size, void *host_ptr,
cl_int *errcode_ret) CL_API_SUFFIX__VERSION_3_0;
typedef cl_mem(CL_API_CALL *cl_api_clCreateImageWithProperties)(
cl_context context, const cl_mem_properties *properties, cl_mem_flags flags,
const cl_image_format *image_format, const cl_image_desc *image_desc,
void *host_ptr, cl_int *errcode_ret) CL_API_SUFFIX__VERSION_3_0;
typedef cl_int(CL_API_CALL* cl_api_clSetContextDestructorCallback)(
cl_context context,
void(CL_CALLBACK* pfn_notify)(cl_context context, void* user_data),
void* user_data) CL_API_SUFFIX__VERSION_3_0;
#else
typedef void *cl_api_clCreateBufferWithProperties;
typedef void *cl_api_clCreateImageWithProperties;
typedef void *cl_api_clSetContextDestructorCallback;
#endif
typedef cl_int(CL_API_CALL *cl_api_clRetainMemObject)(
cl_mem memobj) CL_API_SUFFIX__VERSION_1_0;
typedef cl_int(CL_API_CALL *cl_api_clReleaseMemObject)(
cl_mem memobj) CL_API_SUFFIX__VERSION_1_0;
typedef cl_int(CL_API_CALL *cl_api_clGetSupportedImageFormats)(
cl_context context, cl_mem_flags flags, cl_mem_object_type image_type,
cl_uint num_entries, cl_image_format *image_formats,
cl_uint *num_image_formats) CL_API_SUFFIX__VERSION_1_0;
typedef cl_int(CL_API_CALL *cl_api_clGetMemObjectInfo)(
cl_mem memobj, cl_mem_info param_name, size_t param_value_size,
void *param_value, size_t *param_value_size_ret) CL_API_SUFFIX__VERSION_1_0;
typedef cl_int(CL_API_CALL *cl_api_clGetImageInfo)(
cl_mem image, cl_image_info param_name, size_t param_value_size,
void *param_value, size_t *param_value_size_ret) CL_API_SUFFIX__VERSION_1_0;
#ifdef CL_VERSION_2_0
typedef cl_mem(CL_API_CALL *cl_api_clCreatePipe)(
cl_context , cl_mem_flags ,
cl_uint , cl_uint ,
const cl_pipe_properties * ,
cl_int * ) CL_API_SUFFIX__VERSION_2_0;
typedef cl_int(CL_API_CALL *cl_api_clGetPipeInfo)(
cl_mem , cl_pipe_info ,
size_t , void * ,
size_t * ) CL_API_SUFFIX__VERSION_2_0;
typedef void *(CL_API_CALL *cl_api_clSVMAlloc)(
cl_context , cl_svm_mem_flags , size_t ,
unsigned int )CL_API_SUFFIX__VERSION_2_0;
typedef void(CL_API_CALL *cl_api_clSVMFree)(
cl_context ,
void * ) CL_API_SUFFIX__VERSION_2_0;
#else
typedef void *cl_api_clCreatePipe;
typedef void *cl_api_clGetPipeInfo;
typedef void *cl_api_clSVMAlloc;
typedef void *cl_api_clSVMFree;
#endif
typedef cl_sampler(CL_API_CALL *cl_api_clCreateSampler)(
cl_context context, cl_bool normalized_coords,
cl_addressing_mode addressing_mode, cl_filter_mode filter_mode,
cl_int *errcode_ret) CL_API_SUFFIX__VERSION_1_0;
typedef cl_int(CL_API_CALL *cl_api_clRetainSampler)(
cl_sampler sampler) CL_API_SUFFIX__VERSION_1_0;
typedef cl_int(CL_API_CALL *cl_api_clReleaseSampler)(
cl_sampler sampler) CL_API_SUFFIX__VERSION_1_0;
typedef cl_int(CL_API_CALL *cl_api_clGetSamplerInfo)(
cl_sampler sampler, cl_sampler_info param_name, size_t param_value_size,
void *param_value, size_t *param_value_size_ret) CL_API_SUFFIX__VERSION_1_0;
#ifdef CL_VERSION_2_0
typedef
cl_sampler(CL_API_CALL *cl_api_clCreateSamplerWithProperties)(
cl_context ,
const cl_sampler_properties * ,
cl_int * ) CL_API_SUFFIX__VERSION_2_0;
#else
typedef void *cl_api_clCreateSamplerWithProperties;
#endif
typedef cl_program(CL_API_CALL *cl_api_clCreateProgramWithSource)(
cl_context context, cl_uint count, const char **strings,
const size_t *lengths, cl_int *errcode_ret) CL_API_SUFFIX__VERSION_1_0;
typedef cl_program(CL_API_CALL *cl_api_clCreateProgramWithBinary)(
cl_context context, cl_uint num_devices, const cl_device_id *device_list,
const size_t *lengths, const unsigned char **binaries,
cl_int *binary_status, cl_int *errcode_ret) CL_API_SUFFIX__VERSION_1_0;
#ifdef CL_VERSION_1_2
typedef
cl_program(CL_API_CALL *cl_api_clCreateProgramWithBuiltInKernels)(
cl_context context, cl_uint num_devices, const cl_device_id *device_list,
const char *kernel_names, cl_int *errcode_ret) CL_API_SUFFIX__VERSION_1_2;
#else
typedef void *cl_api_clCreateProgramWithBuiltInKernels;
#endif
typedef cl_int(CL_API_CALL *cl_api_clRetainProgram)(
cl_program program) CL_API_SUFFIX__VERSION_1_0;
typedef cl_int(CL_API_CALL *cl_api_clReleaseProgram)(
cl_program program) CL_API_SUFFIX__VERSION_1_0;
typedef cl_int(CL_API_CALL *cl_api_clBuildProgram)(
cl_program program, cl_uint num_devices, const cl_device_id *device_list,
const char *options,
void(CL_CALLBACK *pfn_notify)(cl_program program, void *user_data),
void *user_data) CL_API_SUFFIX__VERSION_1_0;
#ifdef CL_VERSION_1_2
typedef cl_int(CL_API_CALL *cl_api_clCompileProgram)(
cl_program program, cl_uint num_devices, const cl_device_id *device_list,
const char *options, cl_uint num_input_headers,
const cl_program *input_headers, const char **header_include_names,
void(CL_CALLBACK *pfn_notify)(cl_program program, void *user_data),
void *user_data) CL_API_SUFFIX__VERSION_1_2;
typedef cl_program(CL_API_CALL *cl_api_clLinkProgram)(
cl_context context, cl_uint num_devices, const cl_device_id *device_list,
const char *options, cl_uint num_input_programs,
const cl_program *input_programs,
void(CL_CALLBACK *pfn_notify)(cl_program program, void *user_data),
void *user_data, cl_int *errcode_ret) CL_API_SUFFIX__VERSION_1_2;
#else
typedef void *cl_api_clCompileProgram;
typedef void *cl_api_clLinkProgram;
#endif
#ifdef CL_VERSION_2_2
typedef
cl_int(CL_API_CALL *cl_api_clSetProgramSpecializationConstant)(
cl_program program, cl_uint spec_id, size_t spec_size,
const void *spec_value) CL_API_SUFFIX__VERSION_2_2;
typedef cl_int(CL_API_CALL *cl_api_clSetProgramReleaseCallback)(
cl_program program,
void(CL_CALLBACK *pfn_notify)(cl_program program, void *user_data),
void *user_data) CL_API_SUFFIX__VERSION_2_2;
#else
typedef void *cl_api_clSetProgramSpecializationConstant;
typedef void *cl_api_clSetProgramReleaseCallback;
#endif
#ifdef CL_VERSION_1_2
typedef cl_int(CL_API_CALL *cl_api_clUnloadPlatformCompiler)(
cl_platform_id platform) CL_API_SUFFIX__VERSION_1_2;
#else
typedef void *cl_api_clUnloadPlatformCompiler;
#endif
typedef cl_int(CL_API_CALL *cl_api_clGetProgramInfo)(
cl_program program, cl_program_info param_name, size_t param_value_size,
void *param_value, size_t *param_value_size_ret) CL_API_SUFFIX__VERSION_1_0;
typedef cl_int(CL_API_CALL *cl_api_clGetProgramBuildInfo)(
cl_program program, cl_device_id device, cl_program_build_info param_name,
size_t param_value_size, void *param_value,
size_t *param_value_size_ret) CL_API_SUFFIX__VERSION_1_0;
typedef cl_kernel(CL_API_CALL *cl_api_clCreateKernel)(
cl_program program, const char *kernel_name,
cl_int *errcode_ret) CL_API_SUFFIX__VERSION_1_0;
typedef cl_int(CL_API_CALL *cl_api_clCreateKernelsInProgram)(
cl_program program, cl_uint num_kernels, cl_kernel *kernels,
cl_uint *num_kernels_ret) CL_API_SUFFIX__VERSION_1_0;
typedef cl_int(CL_API_CALL *cl_api_clRetainKernel)(
cl_kernel kernel) CL_API_SUFFIX__VERSION_1_0;
typedef cl_int(CL_API_CALL *cl_api_clReleaseKernel)(
cl_kernel kernel) CL_API_SUFFIX__VERSION_1_0;
typedef cl_int(CL_API_CALL *cl_api_clSetKernelArg)(
cl_kernel kernel, cl_uint arg_index, size_t arg_size,
const void *arg_value) CL_API_SUFFIX__VERSION_1_0;
typedef cl_int(CL_API_CALL *cl_api_clGetKernelInfo)(
cl_kernel kernel, cl_kernel_info param_name, size_t param_value_size,
void *param_value, size_t *param_value_size_ret) CL_API_SUFFIX__VERSION_1_0;
#ifdef CL_VERSION_1_2
typedef cl_int(CL_API_CALL *cl_api_clGetKernelArgInfo)(
cl_kernel kernel, cl_uint arg_indx, cl_kernel_arg_info param_name,
size_t param_value_size, void *param_value,
size_t *param_value_size_ret) CL_API_SUFFIX__VERSION_1_2;
#else
typedef void *cl_api_clGetKernelArgInfo;
#endif
typedef cl_int(CL_API_CALL *cl_api_clGetKernelWorkGroupInfo)(
cl_kernel kernel, cl_device_id device, cl_kernel_work_group_info param_name,
size_t param_value_size, void *param_value,
size_t *param_value_size_ret) CL_API_SUFFIX__VERSION_1_0;
#ifdef CL_VERSION_2_0
typedef cl_int(CL_API_CALL *cl_api_clSetKernelArgSVMPointer)(
cl_kernel , cl_uint ,
const void * ) CL_API_SUFFIX__VERSION_2_0;
typedef cl_int(CL_API_CALL *cl_api_clSetKernelExecInfo)(
cl_kernel , cl_kernel_exec_info ,
size_t ,
const void * ) CL_API_SUFFIX__VERSION_2_0;
typedef cl_int(CL_API_CALL *cl_api_clGetKernelSubGroupInfoKHR)(
cl_kernel , cl_device_id ,
cl_kernel_sub_group_info , size_t ,
const void * , size_t ,
void * ,
size_t * ) CL_API_SUFFIX__VERSION_2_0;
#else
typedef void *cl_api_clSetKernelArgSVMPointer;
typedef void *cl_api_clSetKernelExecInfo;
typedef void *cl_api_clGetKernelSubGroupInfoKHR;
#endif
typedef cl_int(CL_API_CALL *cl_api_clWaitForEvents)(
cl_uint num_events, const cl_event *event_list) CL_API_SUFFIX__VERSION_1_0;
typedef cl_int(CL_API_CALL *cl_api_clGetEventInfo)(
cl_event event, cl_event_info param_name, size_t param_value_size,
void *param_value, size_t *param_value_size_ret) CL_API_SUFFIX__VERSION_1_0;
typedef cl_int(CL_API_CALL *cl_api_clRetainEvent)(cl_event event)
CL_API_SUFFIX__VERSION_1_0;
typedef cl_int(CL_API_CALL *cl_api_clReleaseEvent)(cl_event event)
CL_API_SUFFIX__VERSION_1_0;
typedef cl_int(CL_API_CALL *cl_api_clGetEventProfilingInfo)(
cl_event event, cl_profiling_info param_name, size_t param_value_size,
void *param_value, size_t *param_value_size_ret) CL_API_SUFFIX__VERSION_1_0;
typedef cl_int(CL_API_CALL *cl_api_clFlush)(
cl_command_queue command_queue) CL_API_SUFFIX__VERSION_1_0;
typedef cl_int(CL_API_CALL *cl_api_clFinish)(
cl_command_queue command_queue) CL_API_SUFFIX__VERSION_1_0;
typedef cl_int(CL_API_CALL *cl_api_clEnqueueReadBuffer)(
cl_command_queue command_queue, cl_mem buffer, cl_bool blocking_read,
size_t offset, size_t cb, void *ptr, cl_uint num_events_in_wait_list,
const cl_event *event_wait_list,
cl_event *event) CL_API_SUFFIX__VERSION_1_0;
#ifdef CL_VERSION_1_1
typedef cl_int(CL_API_CALL *cl_api_clEnqueueReadBufferRect)(
cl_command_queue command_queue, cl_mem buffer, cl_bool blocking_read,
const size_t *buffer_origin, const size_t *host_origin,
const size_t *region, size_t buffer_row_pitch, size_t buffer_slice_pitch,
size_t host_row_pitch, size_t host_slice_pitch, void *ptr,
cl_uint num_events_in_wait_list, const cl_event *event_wait_list,
cl_event *event) CL_API_SUFFIX__VERSION_1_1;
#else
typedef void *cl_api_clEnqueueReadBufferRect;
#endif
typedef cl_int(CL_API_CALL *cl_api_clEnqueueWriteBuffer)(
cl_command_queue command_queue, cl_mem buffer, cl_bool blocking_write,
size_t offset, size_t cb, const void *ptr, cl_uint num_events_in_wait_list,
const cl_event *event_wait_list,
cl_event *event) CL_API_SUFFIX__VERSION_1_0;
#ifdef CL_VERSION_1_1
typedef cl_int(CL_API_CALL *cl_api_clEnqueueWriteBufferRect)(
cl_command_queue command_queue, cl_mem buffer, cl_bool blocking_read,
const size_t *buffer_origin, const size_t *host_origin,
const size_t *region, size_t buffer_row_pitch, size_t buffer_slice_pitch,
size_t host_row_pitch, size_t host_slice_pitch, const void *ptr,
cl_uint num_events_in_wait_list, const cl_event *event_wait_list,
cl_event *event) CL_API_SUFFIX__VERSION_1_1;
#else
typedef void *cl_api_clEnqueueWriteBufferRect;
#endif
#ifdef CL_VERSION_1_2
typedef cl_int(CL_API_CALL *cl_api_clEnqueueFillBuffer)(
cl_command_queue command_queue, cl_mem buffer, const void *pattern,
size_t pattern_size, size_t offset, size_t cb,
cl_uint num_events_in_wait_list, const cl_event *event_wait_list,
cl_event *event) CL_API_SUFFIX__VERSION_1_2;
#else
typedef void *cl_api_clEnqueueFillBuffer;
#endif
typedef cl_int(CL_API_CALL *cl_api_clEnqueueCopyBuffer)(
cl_command_queue command_queue, cl_mem src_buffer, cl_mem dst_buffer,
size_t src_offset, size_t dst_offset, size_t cb,
cl_uint num_events_in_wait_list, const cl_event *event_wait_list,
cl_event *event) CL_API_SUFFIX__VERSION_1_0;
#ifdef CL_VERSION_1_1
typedef cl_int(CL_API_CALL *cl_api_clEnqueueCopyBufferRect)(
cl_command_queue command_queue, cl_mem src_buffer, cl_mem dst_buffer,
const size_t *src_origin, const size_t *dst_origin, const size_t *region,
size_t src_row_pitch, size_t src_slice_pitch, size_t dst_row_pitch,
size_t dst_slice_pitch, cl_uint num_events_in_wait_list,
const cl_event *event_wait_list,
cl_event *event) CL_API_SUFFIX__VERSION_1_1;
#else
typedef void *cl_api_clEnqueueCopyBufferRect;
#endif
typedef cl_int(CL_API_CALL *cl_api_clEnqueueReadImage)(
cl_command_queue command_queue, cl_mem image, cl_bool blocking_read,
const size_t *origin, const size_t *region, size_t row_pitch,
size_t slice_pitch, void *ptr, cl_uint num_events_in_wait_list,
const cl_event *event_wait_list,
cl_event *event) CL_API_SUFFIX__VERSION_1_0;
typedef cl_int(CL_API_CALL *cl_api_clEnqueueWriteImage)(
cl_command_queue command_queue, cl_mem image, cl_bool blocking_write,
const size_t *origin, const size_t *region, size_t input_row_pitch,
size_t input_slice_pitch, const void *ptr, cl_uint num_events_in_wait_list,
const cl_event *event_wait_list,
cl_event *event) CL_API_SUFFIX__VERSION_1_0;
#ifdef CL_VERSION_1_2
typedef cl_int(CL_API_CALL *cl_api_clEnqueueFillImage)(
cl_command_queue command_queue, cl_mem image, const void *fill_color,
const size_t origin[3], const size_t region[3],
cl_uint num_events_in_wait_list, const cl_event *event_wait_list,
cl_event *event) CL_API_SUFFIX__VERSION_1_2;
#else
typedef void *cl_api_clEnqueueFillImage;
#endif
typedef cl_int(CL_API_CALL *cl_api_clEnqueueCopyImage)(
cl_command_queue command_queue, cl_mem src_image, cl_mem dst_image,
const size_t *src_origin, const size_t *dst_origin, const size_t *region,
cl_uint num_events_in_wait_list, const cl_event *event_wait_list,
cl_event *event) CL_API_SUFFIX__VERSION_1_0;
typedef cl_int(CL_API_CALL *cl_api_clEnqueueCopyImageToBuffer)(
cl_command_queue command_queue, cl_mem src_image, cl_mem dst_buffer,
const size_t *src_origin, const size_t *region, size_t dst_offset,
cl_uint num_events_in_wait_list, const cl_event *event_wait_list,
cl_event *event) CL_API_SUFFIX__VERSION_1_0;
typedef cl_int(CL_API_CALL *cl_api_clEnqueueCopyBufferToImage)(
cl_command_queue command_queue, cl_mem src_buffer, cl_mem dst_image,
size_t src_offset, const size_t *dst_origin, const size_t *region,
cl_uint num_events_in_wait_list, const cl_event *event_wait_list,
cl_event *event) CL_API_SUFFIX__VERSION_1_0;
typedef void *(CL_API_CALL *cl_api_clEnqueueMapBuffer)(
cl_command_queue command_queue, cl_mem buffer, cl_bool blocking_map,
cl_map_flags map_flags, size_t offset, size_t cb,
cl_uint num_events_in_wait_list, const cl_event *event_wait_list,
cl_event *event, cl_int *errcode_ret)CL_API_SUFFIX__VERSION_1_0;
typedef void *(CL_API_CALL *cl_api_clEnqueueMapImage)(
cl_command_queue command_queue, cl_mem image, cl_bool blocking_map,
cl_map_flags map_flags, const size_t *origin, const size_t *region,
size_t *image_row_pitch, size_t *image_slice_pitch,
cl_uint num_events_in_wait_list, const cl_event *event_wait_list,
cl_event *event, cl_int *errcode_ret)CL_API_SUFFIX__VERSION_1_0;
typedef cl_int(CL_API_CALL *cl_api_clEnqueueUnmapMemObject)(
cl_command_queue command_queue, cl_mem memobj, void *mapped_ptr,
cl_uint num_events_in_wait_list, const cl_event *event_wait_list,
cl_event *event) CL_API_SUFFIX__VERSION_1_0;
#ifdef CL_VERSION_1_2
typedef cl_int(CL_API_CALL *cl_api_clEnqueueMigrateMemObjects)(
cl_command_queue command_queue, cl_uint num_mem_objects,
const cl_mem *mem_objects, cl_mem_migration_flags flags,
cl_uint num_events_in_wait_list, const cl_event *event_wait_list,
cl_event *event) CL_API_SUFFIX__VERSION_1_2;
#else
typedef void *cl_api_clEnqueueMigrateMemObjects;
#endif
typedef cl_int(CL_API_CALL *cl_api_clEnqueueNDRangeKernel)(
cl_command_queue command_queue, cl_kernel kernel, cl_uint work_dim,
const size_t *global_work_offset, const size_t *global_work_size,
const size_t *local_work_size, cl_uint num_events_in_wait_list,
const cl_event *event_wait_list,
cl_event *event) CL_API_SUFFIX__VERSION_1_0;
typedef cl_int(CL_API_CALL *cl_api_clEnqueueTask)(
cl_command_queue command_queue, cl_kernel kernel,
cl_uint num_events_in_wait_list, const cl_event *event_wait_list,
cl_event *event) CL_API_SUFFIX__VERSION_1_0;
typedef cl_int(CL_API_CALL *cl_api_clEnqueueNativeKernel)(
cl_command_queue command_queue, void(CL_CALLBACK *user_func)(void *),
void *args, size_t cb_args, cl_uint num_mem_objects, const cl_mem *mem_list,
const void **args_mem_loc, cl_uint num_events_in_wait_list,
const cl_event *event_wait_list,
cl_event *event) CL_API_SUFFIX__VERSION_1_0;
#ifdef CL_VERSION_1_2
typedef cl_int(CL_API_CALL *cl_api_clEnqueueMarkerWithWaitList)(
cl_command_queue command_queue, cl_uint num_events_in_wait_list,
const cl_event *event_wait_list,
cl_event *event) CL_API_SUFFIX__VERSION_1_2;
typedef cl_int(CL_API_CALL *cl_api_clEnqueueBarrierWithWaitList)(
cl_command_queue command_queue, cl_uint num_events_in_wait_list,
const cl_event *event_wait_list,
cl_event *event) CL_API_SUFFIX__VERSION_1_2;
typedef void *(
CL_API_CALL *cl_api_clGetExtensionFunctionAddressForPlatform)(
cl_platform_id platform,
const char *function_name)CL_API_SUFFIX__VERSION_1_2;
#else
typedef void *cl_api_clEnqueueMarkerWithWaitList;
typedef void *cl_api_clEnqueueBarrierWithWaitList;
typedef void *cl_api_clGetExtensionFunctionAddressForPlatform;
#endif
#ifdef CL_VERSION_2_0
typedef cl_int(CL_API_CALL *cl_api_clEnqueueSVMFree)(
cl_command_queue , cl_uint ,
void ** ,
void(CL_CALLBACK *pfn_free_func)(cl_command_queue ,
cl_uint ,
void ** ,
void * ),
void * , cl_uint ,
const cl_event * ,
cl_event * ) CL_API_SUFFIX__VERSION_2_0;
typedef cl_int(CL_API_CALL *cl_api_clEnqueueSVMMemcpy)(
cl_command_queue , cl_bool ,
void * , const void * , size_t ,
cl_uint ,
const cl_event * ,
cl_event * ) CL_API_SUFFIX__VERSION_2_0;
typedef cl_int(CL_API_CALL *cl_api_clEnqueueSVMMemFill)(
cl_command_queue , void * ,
const void * , size_t , size_t ,
cl_uint ,
const cl_event * ,
cl_event * ) CL_API_SUFFIX__VERSION_2_0;
typedef cl_int(CL_API_CALL *cl_api_clEnqueueSVMMap)(
cl_command_queue , cl_bool ,
cl_map_flags , void * , size_t ,
cl_uint ,
const cl_event * ,
cl_event * ) CL_API_SUFFIX__VERSION_2_0;
typedef cl_int(CL_API_CALL *cl_api_clEnqueueSVMUnmap)(
cl_command_queue , void * ,
cl_uint ,
const cl_event * ,
cl_event * ) CL_API_SUFFIX__VERSION_2_0;
#else
typedef void *cl_api_clEnqueueSVMFree;
typedef void *cl_api_clEnqueueSVMMemcpy;
typedef void *cl_api_clEnqueueSVMMemFill;
typedef void *cl_api_clEnqueueSVMMap;
typedef void *cl_api_clEnqueueSVMUnmap;
#endif
typedef cl_int(CL_API_CALL *cl_api_clSetCommandQueueProperty)(
cl_command_queue command_queue, cl_command_queue_properties properties,
cl_bool enable, cl_command_queue_properties *old_properties)
CL_API_SUFFIX__VERSION_1_0_DEPRECATED;
typedef cl_mem(CL_API_CALL *cl_api_clCreateImage2D)(
cl_context context, cl_mem_flags flags, const cl_image_format *image_format,
size_t image_width, size_t image_height, size_t image_row_pitch,
void *host_ptr, cl_int *errcode_ret) CL_API_SUFFIX__VERSION_1_1_DEPRECATED;
typedef cl_mem(CL_API_CALL *cl_api_clCreateImage3D)(
cl_context context, cl_mem_flags flags, const cl_image_format *image_format,
size_t image_width, size_t image_height, size_t image_depth,
size_t image_row_pitch, size_t image_slice_pitch, void *host_ptr,
cl_int *errcode_ret) CL_API_SUFFIX__VERSION_1_1_DEPRECATED;
typedef cl_int(CL_API_CALL *cl_api_clUnloadCompiler)(void)
CL_API_SUFFIX__VERSION_1_1_DEPRECATED;
typedef cl_int(CL_API_CALL *cl_api_clEnqueueMarker)(
cl_command_queue command_queue,
cl_event *event) CL_API_SUFFIX__VERSION_1_1_DEPRECATED;
typedef cl_int(CL_API_CALL *cl_api_clEnqueueWaitForEvents)(
cl_command_queue command_queue, cl_uint num_events,
const cl_event *event_list) CL_API_SUFFIX__VERSION_1_1_DEPRECATED;
typedef cl_int(CL_API_CALL *cl_api_clEnqueueBarrier)(
cl_command_queue command_queue) CL_API_SUFFIX__VERSION_1_1_DEPRECATED;
typedef void *(CL_API_CALL *cl_api_clGetExtensionFunctionAddress)(
const char *function_name)CL_API_SUFFIX__VERSION_1_1_DEPRECATED;
typedef cl_mem(CL_API_CALL *cl_api_clCreateFromGLBuffer)(
cl_context context, cl_mem_flags flags, cl_GLuint bufobj,
int *errcode_ret) CL_API_SUFFIX__VERSION_1_0;
typedef cl_mem(CL_API_CALL *cl_api_clCreateFromGLTexture)(
cl_context context, cl_mem_flags flags, cl_GLenum target, cl_GLint miplevel,
cl_GLuint texture, cl_int *errcode_ret) CL_API_SUFFIX__VERSION_1_2;
typedef cl_mem(CL_API_CALL *cl_api_clCreateFromGLTexture2D)(
cl_context context, cl_mem_flags flags, cl_GLenum target, cl_GLint miplevel,
cl_GLuint texture, cl_int *errcode_ret) CL_API_SUFFIX__VERSION_1_0;
typedef cl_mem(CL_API_CALL *cl_api_clCreateFromGLTexture3D)(
cl_context context, cl_mem_flags flags, cl_GLenum target, cl_GLint miplevel,
cl_GLuint texture, cl_int *errcode_ret) CL_API_SUFFIX__VERSION_1_0;
typedef cl_mem(CL_API_CALL *cl_api_clCreateFromGLRenderbuffer)(
cl_context context, cl_mem_flags flags, cl_GLuint renderbuffer,
cl_int *errcode_ret) CL_API_SUFFIX__VERSION_1_0;
typedef cl_int(CL_API_CALL *cl_api_clGetGLObjectInfo)(
cl_mem memobj, cl_gl_object_type *gl_object_type,
cl_GLuint *gl_object_name) CL_API_SUFFIX__VERSION_1_0;
typedef cl_int(CL_API_CALL *cl_api_clGetGLTextureInfo)(
cl_mem memobj, cl_gl_texture_info param_name, size_t param_value_size,
void *param_value, size_t *param_value_size_ret) CL_API_SUFFIX__VERSION_1_0;
typedef cl_int(CL_API_CALL *cl_api_clEnqueueAcquireGLObjects)(
cl_command_queue command_queue, cl_uint num_objects,
const cl_mem *mem_objects, cl_uint num_events_in_wait_list,
const cl_event *event_wait_list,
cl_event *event) CL_API_SUFFIX__VERSION_1_0;
typedef cl_int(CL_API_CALL *cl_api_clEnqueueReleaseGLObjects)(
cl_command_queue command_queue, cl_uint num_objects,
const cl_mem *mem_objects, cl_uint num_events_in_wait_list,
const cl_event *event_wait_list,
cl_event *event) CL_API_SUFFIX__VERSION_1_0;
typedef cl_int(CL_API_CALL *cl_api_clGetGLContextInfoKHR)(
const cl_context_properties *properties, cl_gl_context_info param_name,
size_t param_value_size, void *param_value, size_t *param_value_size_ret);
typedef cl_event(CL_API_CALL *cl_api_clCreateEventFromGLsyncKHR)(
cl_context context, cl_GLsync sync, cl_int *errcode_ret);
#if defined(_WIN32)
typedef cl_int(CL_API_CALL *cl_api_clGetDeviceIDsFromD3D10KHR)(
cl_platform_id platform, cl_d3d10_device_source_khr d3d_device_source,
void *d3d_object, cl_d3d10_device_set_khr d3d_device_set,
cl_uint num_entries, cl_device_id *devices,
cl_uint *num_devices) CL_API_SUFFIX__VERSION_1_0;
typedef cl_mem(CL_API_CALL *cl_api_clCreateFromD3D10BufferKHR)(
cl_context context, cl_mem_flags flags, ID3D10Buffer *resource,
cl_int *errcode_ret) CL_API_SUFFIX__VERSION_1_0;
typedef cl_mem(CL_API_CALL *cl_api_clCreateFromD3D10Texture2DKHR)(
cl_context context, cl_mem_flags flags, ID3D10Texture2D *resource,
UINT subresource, cl_int *errcode_ret) CL_API_SUFFIX__VERSION_1_0;
typedef cl_mem(CL_API_CALL *cl_api_clCreateFromD3D10Texture3DKHR)(
cl_context context, cl_mem_flags flags, ID3D10Texture3D *resource,
UINT subresource, cl_int *errcode_ret) CL_API_SUFFIX__VERSION_1_0;
typedef
cl_int(CL_API_CALL *cl_api_clEnqueueAcquireD3D10ObjectsKHR)(
cl_command_queue command_queue, cl_uint num_objects,
const cl_mem *mem_objects, cl_uint num_events_in_wait_list,
const cl_event *event_wait_list,
cl_event *event) CL_API_SUFFIX__VERSION_1_0;
typedef
cl_int(CL_API_CALL *cl_api_clEnqueueReleaseD3D10ObjectsKHR)(
cl_command_queue command_queue, cl_uint num_objects,
const cl_mem *mem_objects, cl_uint num_events_in_wait_list,
const cl_event *event_wait_list,
cl_event *event) CL_API_SUFFIX__VERSION_1_0;
extern CL_API_ENTRY cl_int CL_API_CALL clGetDeviceIDsFromD3D10KHR(
cl_platform_id platform, cl_d3d10_device_source_khr d3d_device_source,
void *d3d_object, cl_d3d10_device_set_khr d3d_device_set,
cl_uint num_entries, cl_device_id *devices, cl_uint *num_devices);
extern CL_API_ENTRY cl_mem CL_API_CALL
clCreateFromD3D10BufferKHR(cl_context context, cl_mem_flags flags,
ID3D10Buffer *resource, cl_int *errcode_ret);
extern CL_API_ENTRY cl_mem CL_API_CALL clCreateFromD3D10Texture2DKHR(
cl_context context, cl_mem_flags flags, ID3D10Texture2D *resource,
UINT subresource, cl_int *errcode_ret);
extern CL_API_ENTRY cl_mem CL_API_CALL clCreateFromD3D10Texture3DKHR(
cl_context context, cl_mem_flags flags, ID3D10Texture3D *resource,
UINT subresource, cl_int *errcode_ret);
extern CL_API_ENTRY cl_int CL_API_CALL clEnqueueAcquireD3D10ObjectsKHR(
cl_command_queue command_queue, cl_uint num_objects,
const cl_mem *mem_objects, cl_uint num_events_in_wait_list,
const cl_event *event_wait_list, cl_event *event);
extern CL_API_ENTRY cl_int CL_API_CALL clEnqueueReleaseD3D10ObjectsKHR(
cl_command_queue command_queue, cl_uint num_objects,
const cl_mem *mem_objects, cl_uint num_events_in_wait_list,
const cl_event *event_wait_list, cl_event *event);
typedef cl_int(CL_API_CALL *cl_api_clGetDeviceIDsFromD3D11KHR)(
cl_platform_id platform, cl_d3d11_device_source_khr d3d_device_source,
void *d3d_object, cl_d3d11_device_set_khr d3d_device_set,
cl_uint num_entries, cl_device_id *devices,
cl_uint *num_devices) CL_API_SUFFIX__VERSION_1_2;
typedef cl_mem(CL_API_CALL *cl_api_clCreateFromD3D11BufferKHR)(
cl_context context, cl_mem_flags flags, ID3D11Buffer *resource,
cl_int *errcode_ret) CL_API_SUFFIX__VERSION_1_2;
typedef cl_mem(CL_API_CALL *cl_api_clCreateFromD3D11Texture2DKHR)(
cl_context context, cl_mem_flags flags, ID3D11Texture2D *resource,
UINT subresource, cl_int *errcode_ret) CL_API_SUFFIX__VERSION_1_2;
typedef cl_mem(CL_API_CALL *cl_api_clCreateFromD3D11Texture3DKHR)(
cl_context context, cl_mem_flags flags, ID3D11Texture3D *resource,
UINT subresource, cl_int *errcode_ret) CL_API_SUFFIX__VERSION_1_2;
typedef
cl_int(CL_API_CALL *cl_api_clEnqueueAcquireD3D11ObjectsKHR)(
cl_command_queue command_queue, cl_uint num_objects,
const cl_mem *mem_objects, cl_uint num_events_in_wait_list,
const cl_event *event_wait_list,
cl_event *event) CL_API_SUFFIX__VERSION_1_2;
typedef
cl_int(CL_API_CALL *cl_api_clEnqueueReleaseD3D11ObjectsKHR)(
cl_command_queue command_queue, cl_uint num_objects,
const cl_mem *mem_objects, cl_uint num_events_in_wait_list,
const cl_event *event_wait_list,
cl_event *event) CL_API_SUFFIX__VERSION_1_2;
typedef
cl_int(CL_API_CALL *cl_api_clGetDeviceIDsFromDX9MediaAdapterKHR)(
cl_platform_id platform, cl_uint num_media_adapters,
cl_dx9_media_adapter_type_khr *media_adapters_type, void *media_adapters,
cl_dx9_media_adapter_set_khr media_adapter_set, cl_uint num_entries,
cl_device_id *devices, cl_uint *num_devices) CL_API_SUFFIX__VERSION_1_2;
typedef cl_mem(CL_API_CALL *cl_api_clCreateFromDX9MediaSurfaceKHR)(
cl_context context, cl_mem_flags flags,
cl_dx9_media_adapter_type_khr adapter_type, void *surface_info,
cl_uint plane, cl_int *errcode_ret) CL_API_SUFFIX__VERSION_1_2;
typedef
cl_int(CL_API_CALL *cl_api_clEnqueueAcquireDX9MediaSurfacesKHR)(
cl_command_queue command_queue, cl_uint num_objects,
const cl_mem *mem_objects, cl_uint num_events_in_wait_list,
const cl_event *event_wait_list,
cl_event *event) CL_API_SUFFIX__VERSION_1_2;
typedef
cl_int(CL_API_CALL *cl_api_clEnqueueReleaseDX9MediaSurfacesKHR)(
cl_command_queue command_queue, cl_uint num_objects,
const cl_mem *mem_objects, cl_uint num_events_in_wait_list,
const cl_event *event_wait_list,
cl_event *event) CL_API_SUFFIX__VERSION_1_2;
extern CL_API_ENTRY cl_int CL_API_CALL clGetDeviceIDsFromD3D11KHR(
cl_platform_id platform, cl_d3d11_device_source_khr d3d_device_source,
void *d3d_object, cl_d3d11_device_set_khr d3d_device_set,
cl_uint num_entries, cl_device_id *devices, cl_uint *num_devices);
extern CL_API_ENTRY cl_mem CL_API_CALL
clCreateFromD3D11BufferKHR(cl_context context, cl_mem_flags flags,
ID3D11Buffer *resource, cl_int *errcode_ret);
extern CL_API_ENTRY cl_mem CL_API_CALL clCreateFromD3D11Texture2DKHR(
cl_context context, cl_mem_flags flags, ID3D11Texture2D *resource,
UINT subresource, cl_int *errcode_ret);
extern CL_API_ENTRY cl_mem CL_API_CALL clCreateFromD3D11Texture3DKHR(
cl_context context, cl_mem_flags flags, ID3D11Texture3D *resource,
UINT subresource, cl_int *errcode_ret);
extern CL_API_ENTRY cl_int CL_API_CALL clEnqueueAcquireD3D11ObjectsKHR(
cl_command_queue command_queue, cl_uint num_objects,
const cl_mem *mem_objects, cl_uint num_events_in_wait_list,
const cl_event *event_wait_list, cl_event *event);
extern CL_API_ENTRY cl_int CL_API_CALL clEnqueueReleaseD3D11ObjectsKHR(
cl_command_queue command_queue, cl_uint num_objects,
const cl_mem *mem_objects, cl_uint num_events_in_wait_list,
const cl_event *event_wait_list, cl_event *event);
extern CL_API_ENTRY cl_int CL_API_CALL clGetDeviceIDsFromDX9MediaAdapterKHR(
cl_platform_id platform, cl_uint num_media_adapters,
cl_dx9_media_adapter_type_khr *media_adapter_type, void *media_adapters,
cl_dx9_media_adapter_set_khr media_adapter_set, cl_uint num_entries,
cl_device_id *devices, cl_uint *num_devices);
extern CL_API_ENTRY cl_mem CL_API_CALL clCreateFromDX9MediaSurfaceKHR(
cl_context context, cl_mem_flags flags,
cl_dx9_media_adapter_type_khr adapter_type, void *surface_info,
cl_uint plane, cl_int *errcode_ret);
extern CL_API_ENTRY cl_int CL_API_CALL clEnqueueAcquireDX9MediaSurfacesKHR(
cl_command_queue command_queue, cl_uint num_objects,
const cl_mem *mem_objects, cl_uint num_events_in_wait_list,
const cl_event *event_wait_list, cl_event *event);
extern CL_API_ENTRY cl_int CL_API_CALL clEnqueueReleaseDX9MediaSurfacesKHR(
cl_command_queue command_queue, cl_uint num_objects,
const cl_mem *mem_objects, cl_uint num_events_in_wait_list,
const cl_event *event_wait_list, cl_event *event);
#else
typedef void *cl_api_clGetDeviceIDsFromD3D10KHR;
typedef void *cl_api_clCreateFromD3D10BufferKHR;
typedef void *cl_api_clCreateFromD3D10Texture2DKHR;
typedef void *cl_api_clCreateFromD3D10Texture3DKHR;
typedef void *cl_api_clEnqueueAcquireD3D10ObjectsKHR;
typedef void *cl_api_clEnqueueReleaseD3D10ObjectsKHR;
typedef void *cl_api_clGetDeviceIDsFromD3D11KHR;
typedef void *cl_api_clCreateFromD3D11BufferKHR;
typedef void *cl_api_clCreateFromD3D11Texture2DKHR;
typedef void *cl_api_clCreateFromD3D11Texture3DKHR;
typedef void *cl_api_clEnqueueAcquireD3D11ObjectsKHR;
typedef void *cl_api_clEnqueueReleaseD3D11ObjectsKHR;
typedef void *cl_api_clCreateFromDX9MediaSurfaceKHR;
typedef void *cl_api_clEnqueueAcquireDX9MediaSurfacesKHR;
typedef void *cl_api_clEnqueueReleaseDX9MediaSurfacesKHR;
typedef void *cl_api_clGetDeviceIDsFromDX9MediaAdapterKHR;
#endif
#ifdef CL_VERSION_1_1
typedef cl_int(CL_API_CALL *cl_api_clSetEventCallback)(
cl_event , cl_int ,
void(CL_CALLBACK * )(cl_event, cl_int, void *),
void * ) CL_API_SUFFIX__VERSION_1_1;
typedef cl_mem(CL_API_CALL *cl_api_clCreateSubBuffer)(
cl_mem , cl_mem_flags ,
cl_buffer_create_type ,
const void * ,
cl_int * ) CL_API_SUFFIX__VERSION_1_1;
typedef
cl_int(CL_API_CALL *cl_api_clSetMemObjectDestructorCallback)(
cl_mem ,
void(CL_CALLBACK * )(cl_mem ,
void * ),
void * ) CL_API_SUFFIX__VERSION_1_1;
typedef cl_event(CL_API_CALL *cl_api_clCreateUserEvent)(
cl_context ,
cl_int * ) CL_API_SUFFIX__VERSION_1_1;
typedef cl_int(CL_API_CALL *cl_api_clSetUserEventStatus)(
cl_event ,
cl_int ) CL_API_SUFFIX__VERSION_1_1;
#else
typedef void *cl_api_clSetEventCallback;
typedef void *cl_api_clCreateSubBuffer;
typedef void *cl_api_clSetMemObjectDestructorCallback;
typedef void *cl_api_clCreateUserEvent;
typedef void *cl_api_clSetUserEventStatus;
#endif
typedef cl_int(CL_API_CALL *cl_api_clCreateSubDevicesEXT)(
cl_device_id in_device,
const cl_device_partition_property_ext *partition_properties,
cl_uint num_entries, cl_device_id *out_devices, cl_uint *num_devices);
typedef cl_int(CL_API_CALL *cl_api_clRetainDeviceEXT)(
cl_device_id device) CL_API_SUFFIX__VERSION_1_0;
typedef cl_int(CL_API_CALL *cl_api_clReleaseDeviceEXT)(
cl_device_id device) CL_API_SUFFIX__VERSION_1_0;
typedef cl_mem(CL_API_CALL *cl_api_clCreateFromEGLImageKHR)(
cl_context context, CLeglDisplayKHR display, CLeglImageKHR image,
cl_mem_flags flags, const cl_egl_image_properties_khr *properties,
cl_int *errcode_ret);
typedef cl_int(CL_API_CALL *cl_api_clEnqueueAcquireEGLObjectsKHR)(
cl_command_queue command_queue, cl_uint num_objects,
const cl_mem *mem_objects, cl_uint num_events_in_wait_list,
const cl_event *event_wait_list, cl_event *event);
typedef cl_int(CL_API_CALL *cl_api_clEnqueueReleaseEGLObjectsKHR)(
cl_command_queue command_queue, cl_uint num_objects,
const cl_mem *mem_objects, cl_uint num_events_in_wait_list,
const cl_event *event_wait_list, cl_event *event);
typedef cl_event(CL_API_CALL *cl_api_clCreateEventFromEGLSyncKHR)(
cl_context context, CLeglSyncKHR sync, CLeglDisplayKHR display,
cl_int *errcode_ret);
#ifdef CL_VERSION_2_1
typedef cl_int(CL_API_CALL *cl_api_clSetDefaultDeviceCommandQueue)(
cl_context context, cl_device_id device,
cl_command_queue command_queue) CL_API_SUFFIX__VERSION_2_1;
typedef cl_program(CL_API_CALL *cl_api_clCreateProgramWithIL)(
cl_context context, const void *il, size_t length,
cl_int *errcode_ret) CL_API_SUFFIX__VERSION_2_1;
typedef cl_int(CL_API_CALL *cl_api_clGetKernelSubGroupInfo)(
cl_kernel kernel, cl_device_id device, cl_kernel_sub_group_info param_name,
size_t input_value_size, const void *input_value, size_t param_value_size,
void *param_value, size_t *param_value_size_ret) CL_API_SUFFIX__VERSION_2_1;
typedef cl_kernel(CL_API_CALL *cl_api_clCloneKernel)(
cl_kernel source_kernel, cl_int *errcode_ret) CL_API_SUFFIX__VERSION_2_1;
typedef cl_int(CL_API_CALL *cl_api_clEnqueueSVMMigrateMem)(
cl_command_queue command_queue, cl_uint num_svm_pointers,
const void **svm_pointers, const size_t *sizes,
cl_mem_migration_flags flags, cl_uint num_events_in_wait_list,
const cl_event *event_wait_list,
cl_event *event) CL_API_SUFFIX__VERSION_2_1;
typedef cl_int(CL_API_CALL *cl_api_clGetDeviceAndHostTimer)(
cl_device_id device, cl_ulong *device_timestamp,
cl_ulong *host_timestamp) CL_API_SUFFIX__VERSION_2_1;
typedef cl_int(CL_API_CALL *cl_api_clGetHostTimer)(
cl_device_id device, cl_ulong *host_timestamp) CL_API_SUFFIX__VERSION_2_1;
#else
typedef void *cl_api_clSetDefaultDeviceCommandQueue;
typedef void *cl_api_clCreateProgramWithIL;
typedef void *cl_api_clGetKernelSubGroupInfo;
typedef void *cl_api_clCloneKernel;
typedef void *cl_api_clEnqueueSVMMigrateMem;
typedef void *cl_api_clGetDeviceAndHostTimer;
typedef void *cl_api_clGetHostTimer;
#endif
typedef struct _cl_icd_dispatch {
cl_api_clGetPlatformIDs clGetPlatformIDs;
cl_api_clGetPlatformInfo clGetPlatformInfo;
cl_api_clGetDeviceIDs clGetDeviceIDs;
cl_api_clGetDeviceInfo clGetDeviceInfo;
cl_api_clCreateContext clCreateContext;
cl_api_clCreateContextFromType clCreateContextFromType;
cl_api_clRetainContext clRetainContext;
cl_api_clReleaseContext clReleaseContext;
cl_api_clGetContextInfo clGetContextInfo;
cl_api_clCreateCommandQueue clCreateCommandQueue;
cl_api_clRetainCommandQueue clRetainCommandQueue;
cl_api_clReleaseCommandQueue clReleaseCommandQueue;
cl_api_clGetCommandQueueInfo clGetCommandQueueInfo;
cl_api_clSetCommandQueueProperty clSetCommandQueueProperty;
cl_api_clCreateBuffer clCreateBuffer;
cl_api_clCreateImage2D clCreateImage2D;
cl_api_clCreateImage3D clCreateImage3D;
cl_api_clRetainMemObject clRetainMemObject;
cl_api_clReleaseMemObject clReleaseMemObject;
cl_api_clGetSupportedImageFormats clGetSupportedImageFormats;
cl_api_clGetMemObjectInfo clGetMemObjectInfo;
cl_api_clGetImageInfo clGetImageInfo;
cl_api_clCreateSampler clCreateSampler;
cl_api_clRetainSampler clRetainSampler;
cl_api_clReleaseSampler clReleaseSampler;
cl_api_clGetSamplerInfo clGetSamplerInfo;
cl_api_clCreateProgramWithSource clCreateProgramWithSource;
cl_api_clCreateProgramWithBinary clCreateProgramWithBinary;
cl_api_clRetainProgram clRetainProgram;
cl_api_clReleaseProgram clReleaseProgram;
cl_api_clBuildProgram clBuildProgram;
cl_api_clUnloadCompiler clUnloadCompiler;
cl_api_clGetProgramInfo clGetProgramInfo;
cl_api_clGetProgramBuildInfo clGetProgramBuildInfo;
cl_api_clCreateKernel clCreateKernel;
cl_api_clCreateKernelsInProgram clCreateKernelsInProgram;
cl_api_clRetainKernel clRetainKernel;
cl_api_clReleaseKernel clReleaseKernel;
cl_api_clSetKernelArg clSetKernelArg;
cl_api_clGetKernelInfo clGetKernelInfo;
cl_api_clGetKernelWorkGroupInfo clGetKernelWorkGroupInfo;
cl_api_clWaitForEvents clWaitForEvents;
cl_api_clGetEventInfo clGetEventInfo;
cl_api_clRetainEvent clRetainEvent;
cl_api_clReleaseEvent clReleaseEvent;
cl_api_clGetEventProfilingInfo clGetEventProfilingInfo;
cl_api_clFlush clFlush;
cl_api_clFinish clFinish;
cl_api_clEnqueueReadBuffer clEnqueueReadBuffer;
cl_api_clEnqueueWriteBuffer clEnqueueWriteBuffer;
cl_api_clEnqueueCopyBuffer clEnqueueCopyBuffer;
cl_api_clEnqueueReadImage clEnqueueReadImage;
cl_api_clEnqueueWriteImage clEnqueueWriteImage;
cl_api_clEnqueueCopyImage clEnqueueCopyImage;
cl_api_clEnqueueCopyImageToBuffer clEnqueueCopyImageToBuffer;
cl_api_clEnqueueCopyBufferToImage clEnqueueCopyBufferToImage;
cl_api_clEnqueueMapBuffer clEnqueueMapBuffer;
cl_api_clEnqueueMapImage clEnqueueMapImage;
cl_api_clEnqueueUnmapMemObject clEnqueueUnmapMemObject;
cl_api_clEnqueueNDRangeKernel clEnqueueNDRangeKernel;
cl_api_clEnqueueTask clEnqueueTask;
cl_api_clEnqueueNativeKernel clEnqueueNativeKernel;
cl_api_clEnqueueMarker clEnqueueMarker;
cl_api_clEnqueueWaitForEvents clEnqueueWaitForEvents;
cl_api_clEnqueueBarrier clEnqueueBarrier;
cl_api_clGetExtensionFunctionAddress clGetExtensionFunctionAddress;
cl_api_clCreateFromGLBuffer clCreateFromGLBuffer;
cl_api_clCreateFromGLTexture2D clCreateFromGLTexture2D;
cl_api_clCreateFromGLTexture3D clCreateFromGLTexture3D;
cl_api_clCreateFromGLRenderbuffer clCreateFromGLRenderbuffer;
cl_api_clGetGLObjectInfo clGetGLObjectInfo;
cl_api_clGetGLTextureInfo clGetGLTextureInfo;
cl_api_clEnqueueAcquireGLObjects clEnqueueAcquireGLObjects;
cl_api_clEnqueueReleaseGLObjects clEnqueueReleaseGLObjects;
cl_api_clGetGLContextInfoKHR clGetGLContextInfoKHR;
cl_api_clGetDeviceIDsFromD3D10KHR clGetDeviceIDsFromD3D10KHR;
cl_api_clCreateFromD3D10BufferKHR clCreateFromD3D10BufferKHR;
cl_api_clCreateFromD3D10Texture2DKHR clCreateFromD3D10Texture2DKHR;
cl_api_clCreateFromD3D10Texture3DKHR clCreateFromD3D10Texture3DKHR;
cl_api_clEnqueueAcquireD3D10ObjectsKHR clEnqueueAcquireD3D10ObjectsKHR;
cl_api_clEnqueueReleaseD3D10ObjectsKHR clEnqueueReleaseD3D10ObjectsKHR;
cl_api_clSetEventCallback clSetEventCallback;
cl_api_clCreateSubBuffer clCreateSubBuffer;
cl_api_clSetMemObjectDestructorCallback clSetMemObjectDestructorCallback;
cl_api_clCreateUserEvent clCreateUserEvent;
cl_api_clSetUserEventStatus clSetUserEventStatus;
cl_api_clEnqueueReadBufferRect clEnqueueReadBufferRect;
cl_api_clEnqueueWriteBufferRect clEnqueueWriteBufferRect;
cl_api_clEnqueueCopyBufferRect clEnqueueCopyBufferRect;
cl_api_clCreateSubDevicesEXT clCreateSubDevicesEXT;
cl_api_clRetainDeviceEXT clRetainDeviceEXT;
cl_api_clReleaseDeviceEXT clReleaseDeviceEXT;
cl_api_clCreateEventFromGLsyncKHR clCreateEventFromGLsyncKHR;
cl_api_clCreateSubDevices clCreateSubDevices;
cl_api_clRetainDevice clRetainDevice;
cl_api_clReleaseDevice clReleaseDevice;
cl_api_clCreateImage clCreateImage;
cl_api_clCreateProgramWithBuiltInKernels clCreateProgramWithBuiltInKernels;
cl_api_clCompileProgram clCompileProgram;
cl_api_clLinkProgram clLinkProgram;
cl_api_clUnloadPlatformCompiler clUnloadPlatformCompiler;
cl_api_clGetKernelArgInfo clGetKernelArgInfo;
cl_api_clEnqueueFillBuffer clEnqueueFillBuffer;
cl_api_clEnqueueFillImage clEnqueueFillImage;
cl_api_clEnqueueMigrateMemObjects clEnqueueMigrateMemObjects;
cl_api_clEnqueueMarkerWithWaitList clEnqueueMarkerWithWaitList;
cl_api_clEnqueueBarrierWithWaitList clEnqueueBarrierWithWaitList;
cl_api_clGetExtensionFunctionAddressForPlatform
clGetExtensionFunctionAddressForPlatform;
cl_api_clCreateFromGLTexture clCreateFromGLTexture;
cl_api_clGetDeviceIDsFromD3D11KHR clGetDeviceIDsFromD3D11KHR;
cl_api_clCreateFromD3D11BufferKHR clCreateFromD3D11BufferKHR;
cl_api_clCreateFromD3D11Texture2DKHR clCreateFromD3D11Texture2DKHR;
cl_api_clCreateFromD3D11Texture3DKHR clCreateFromD3D11Texture3DKHR;
cl_api_clCreateFromDX9MediaSurfaceKHR clCreateFromDX9MediaSurfaceKHR;
cl_api_clEnqueueAcquireD3D11ObjectsKHR clEnqueueAcquireD3D11ObjectsKHR;
cl_api_clEnqueueReleaseD3D11ObjectsKHR clEnqueueReleaseD3D11ObjectsKHR;
cl_api_clGetDeviceIDsFromDX9MediaAdapterKHR
clGetDeviceIDsFromDX9MediaAdapterKHR;
cl_api_clEnqueueAcquireDX9MediaSurfacesKHR
clEnqueueAcquireDX9MediaSurfacesKHR;
cl_api_clEnqueueReleaseDX9MediaSurfacesKHR
clEnqueueReleaseDX9MediaSurfacesKHR;
cl_api_clCreateFromEGLImageKHR clCreateFromEGLImageKHR;
cl_api_clEnqueueAcquireEGLObjectsKHR clEnqueueAcquireEGLObjectsKHR;
cl_api_clEnqueueReleaseEGLObjectsKHR clEnqueueReleaseEGLObjectsKHR;
cl_api_clCreateEventFromEGLSyncKHR clCreateEventFromEGLSyncKHR;
cl_api_clCreateCommandQueueWithProperties clCreateCommandQueueWithProperties;
cl_api_clCreatePipe clCreatePipe;
cl_api_clGetPipeInfo clGetPipeInfo;
cl_api_clSVMAlloc clSVMAlloc;
cl_api_clSVMFree clSVMFree;
cl_api_clEnqueueSVMFree clEnqueueSVMFree;
cl_api_clEnqueueSVMMemcpy clEnqueueSVMMemcpy;
cl_api_clEnqueueSVMMemFill clEnqueueSVMMemFill;
cl_api_clEnqueueSVMMap clEnqueueSVMMap;
cl_api_clEnqueueSVMUnmap clEnqueueSVMUnmap;
cl_api_clCreateSamplerWithProperties clCreateSamplerWithProperties;
cl_api_clSetKernelArgSVMPointer clSetKernelArgSVMPointer;
cl_api_clSetKernelExecInfo clSetKernelExecInfo;
cl_api_clGetKernelSubGroupInfoKHR clGetKernelSubGroupInfoKHR;
cl_api_clCloneKernel clCloneKernel;
cl_api_clCreateProgramWithIL clCreateProgramWithIL;
cl_api_clEnqueueSVMMigrateMem clEnqueueSVMMigrateMem;
cl_api_clGetDeviceAndHostTimer clGetDeviceAndHostTimer;
cl_api_clGetHostTimer clGetHostTimer;
cl_api_clGetKernelSubGroupInfo clGetKernelSubGroupInfo;
cl_api_clSetDefaultDeviceCommandQueue clSetDefaultDeviceCommandQueue;
cl_api_clSetProgramReleaseCallback clSetProgramReleaseCallback;
cl_api_clSetProgramSpecializationConstant clSetProgramSpecializationConstant;
cl_api_clCreateBufferWithProperties clCreateBufferWithProperties;
cl_api_clCreateImageWithProperties clCreateImageWithProperties;
cl_api_clSetContextDestructorCallback clSetContextDestructorCallback;
} cl_icd_dispatch;
#ifdef __cplusplus
}
#endif
#endif