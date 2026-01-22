#ifndef __OPENCL_CL_VA_API_MEDIA_SHARING_INTEL_H
#define __OPENCL_CL_VA_API_MEDIA_SHARING_INTEL_H
#include <CL/cl.h>
#include <CL/cl_platform.h>
#include <va/va.h>
#ifdef __cplusplus
extern "C" {
#endif
#define cl_intel_sharing_format_query_va_api 1
extern CL_API_ENTRY cl_int CL_API_CALL
clGetSupportedVA_APIMediaSurfaceFormatsINTEL(
cl_context context,
cl_mem_flags flags,
cl_mem_object_type image_type,
cl_uint plane,
cl_uint num_entries,
VAImageFormat* va_api_formats,
cl_uint* num_surface_formats) ;
typedef cl_int (CL_API_CALL *
clGetSupportedVA_APIMediaSurfaceFormatsINTEL_fn)(
cl_context context,
cl_mem_flags flags,
cl_mem_object_type image_type,
cl_uint plane,
cl_uint num_entries,
VAImageFormat* va_api_formats,
cl_uint* num_surface_formats) ;
#define cl_intel_va_api_media_sharing 1
#define CL_INVALID_VA_API_MEDIA_ADAPTER_INTEL -1098
#define CL_INVALID_VA_API_MEDIA_SURFACE_INTEL -1099
#define CL_VA_API_MEDIA_SURFACE_ALREADY_ACQUIRED_INTEL -1100
#define CL_VA_API_MEDIA_SURFACE_NOT_ACQUIRED_INTEL -1101
#define CL_VA_API_DISPLAY_INTEL 0x4094
#define CL_PREFERRED_DEVICES_FOR_VA_API_INTEL 0x4095
#define CL_ALL_DEVICES_FOR_VA_API_INTEL 0x4096
#define CL_CONTEXT_VA_API_DISPLAY_INTEL 0x4097
#define CL_MEM_VA_API_MEDIA_SURFACE_INTEL 0x4098
#define CL_IMAGE_VA_API_PLANE_INTEL 0x4099
#define CL_COMMAND_ACQUIRE_VA_API_MEDIA_SURFACES_INTEL 0x409A
#define CL_COMMAND_RELEASE_VA_API_MEDIA_SURFACES_INTEL 0x409B
typedef cl_uint cl_va_api_device_source_intel;
typedef cl_uint cl_va_api_device_set_intel;
extern CL_API_ENTRY cl_int CL_API_CALL
clGetDeviceIDsFromVA_APIMediaAdapterINTEL(
cl_platform_id platform,
cl_va_api_device_source_intel media_adapter_type,
void* media_adapter,
cl_va_api_device_set_intel media_adapter_set,
cl_uint num_entries,
cl_device_id* devices,
cl_uint* num_devices) CL_API_SUFFIX__VERSION_1_2;
typedef cl_int (CL_API_CALL * clGetDeviceIDsFromVA_APIMediaAdapterINTEL_fn)(
cl_platform_id platform,
cl_va_api_device_source_intel media_adapter_type,
void* media_adapter,
cl_va_api_device_set_intel media_adapter_set,
cl_uint num_entries,
cl_device_id* devices,
cl_uint* num_devices) CL_API_SUFFIX__VERSION_1_2;
extern CL_API_ENTRY cl_mem CL_API_CALL
clCreateFromVA_APIMediaSurfaceINTEL(
cl_context context,
cl_mem_flags flags,
VASurfaceID* surface,
cl_uint plane,
cl_int* errcode_ret) CL_API_SUFFIX__VERSION_1_2;
typedef cl_mem (CL_API_CALL * clCreateFromVA_APIMediaSurfaceINTEL_fn)(
cl_context context,
cl_mem_flags flags,
VASurfaceID* surface,
cl_uint plane,
cl_int* errcode_ret) CL_API_SUFFIX__VERSION_1_2;
extern CL_API_ENTRY cl_int CL_API_CALL
clEnqueueAcquireVA_APIMediaSurfacesINTEL(
cl_command_queue command_queue,
cl_uint num_objects,
const cl_mem* mem_objects,
cl_uint num_events_in_wait_list,
const cl_event* event_wait_list,
cl_event* event) CL_API_SUFFIX__VERSION_1_2;
typedef cl_int (CL_API_CALL *clEnqueueAcquireVA_APIMediaSurfacesINTEL_fn)(
cl_command_queue command_queue,
cl_uint num_objects,
const cl_mem* mem_objects,
cl_uint num_events_in_wait_list,
const cl_event* event_wait_list,
cl_event* event) CL_API_SUFFIX__VERSION_1_2;
extern CL_API_ENTRY cl_int CL_API_CALL
clEnqueueReleaseVA_APIMediaSurfacesINTEL(
cl_command_queue command_queue,
cl_uint num_objects,
const cl_mem* mem_objects,
cl_uint num_events_in_wait_list,
const cl_event* event_wait_list,
cl_event* event) CL_API_SUFFIX__VERSION_1_2;
typedef cl_int (CL_API_CALL *clEnqueueReleaseVA_APIMediaSurfacesINTEL_fn)(
cl_command_queue command_queue,
cl_uint num_objects,
const cl_mem* mem_objects,
cl_uint num_events_in_wait_list,
const cl_event* event_wait_list,
cl_event* event) CL_API_SUFFIX__VERSION_1_2;
#ifdef __cplusplus
}
#endif
#endif