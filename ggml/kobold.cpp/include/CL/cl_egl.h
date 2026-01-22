#ifndef __OPENCL_CL_EGL_H
#define __OPENCL_CL_EGL_H
#include <CL/cl.h>
#ifdef __cplusplus
extern "C" {
#endif
#define CL_COMMAND_EGL_FENCE_SYNC_OBJECT_KHR 0x202F
#define CL_COMMAND_ACQUIRE_EGL_OBJECTS_KHR 0x202D
#define CL_COMMAND_RELEASE_EGL_OBJECTS_KHR 0x202E
#define CL_INVALID_EGL_OBJECT_KHR -1093
#define CL_EGL_RESOURCE_NOT_ACQUIRED_KHR -1092
typedef void* CLeglImageKHR;
typedef void* CLeglDisplayKHR;
typedef void* CLeglSyncKHR;
typedef intptr_t cl_egl_image_properties_khr;
#define cl_khr_egl_image 1
extern CL_API_ENTRY cl_mem CL_API_CALL
clCreateFromEGLImageKHR(cl_context context,
CLeglDisplayKHR egldisplay,
CLeglImageKHR eglimage,
cl_mem_flags flags,
const cl_egl_image_properties_khr * properties,
cl_int * errcode_ret) CL_API_SUFFIX__VERSION_1_0;
typedef cl_mem (CL_API_CALL *clCreateFromEGLImageKHR_fn)(
cl_context context,
CLeglDisplayKHR egldisplay,
CLeglImageKHR eglimage,
cl_mem_flags flags,
const cl_egl_image_properties_khr * properties,
cl_int * errcode_ret);
extern CL_API_ENTRY cl_int CL_API_CALL
clEnqueueAcquireEGLObjectsKHR(cl_command_queue command_queue,
cl_uint num_objects,
const cl_mem * mem_objects,
cl_uint num_events_in_wait_list,
const cl_event * event_wait_list,
cl_event * event) CL_API_SUFFIX__VERSION_1_0;
typedef cl_int (CL_API_CALL *clEnqueueAcquireEGLObjectsKHR_fn)(
cl_command_queue command_queue,
cl_uint num_objects,
const cl_mem * mem_objects,
cl_uint num_events_in_wait_list,
const cl_event * event_wait_list,
cl_event * event);
extern CL_API_ENTRY cl_int CL_API_CALL
clEnqueueReleaseEGLObjectsKHR(cl_command_queue command_queue,
cl_uint num_objects,
const cl_mem * mem_objects,
cl_uint num_events_in_wait_list,
const cl_event * event_wait_list,
cl_event * event) CL_API_SUFFIX__VERSION_1_0;
typedef cl_int (CL_API_CALL *clEnqueueReleaseEGLObjectsKHR_fn)(
cl_command_queue command_queue,
cl_uint num_objects,
const cl_mem * mem_objects,
cl_uint num_events_in_wait_list,
const cl_event * event_wait_list,
cl_event * event);
#define cl_khr_egl_event 1
extern CL_API_ENTRY cl_event CL_API_CALL
clCreateEventFromEGLSyncKHR(cl_context context,
CLeglSyncKHR sync,
CLeglDisplayKHR display,
cl_int * errcode_ret) CL_API_SUFFIX__VERSION_1_0;
typedef cl_event (CL_API_CALL *clCreateEventFromEGLSyncKHR_fn)(
cl_context context,
CLeglSyncKHR sync,
CLeglDisplayKHR display,
cl_int * errcode_ret);
#ifdef __cplusplus
}
#endif
#endif