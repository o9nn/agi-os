#pragma once
#include "OpenCLUtils_Export.h"
#include <CL/cl.h>
UTILS_EXPORT
char* cl_util_read_text_file(const char* const filename, size_t* const length,
cl_int* const error);
UTILS_EXPORT
unsigned char* cl_util_read_binary_file(const char* const filename,
size_t* const length,
cl_int* const error);
UTILS_EXPORT
cl_int cl_util_write_binaries(const cl_program program,
const char* const program_file_name);
UTILS_EXPORT
cl_program cl_util_read_binaries(const cl_context context,
const cl_device_id* const devices,
const cl_uint num_devices,
const char* const program_file_name,
cl_int* const error);