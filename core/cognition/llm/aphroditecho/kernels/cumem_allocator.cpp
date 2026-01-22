#include <iostream>
extern "C" {
#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <sys/types.h>
#include <cuda_runtime_api.h>
#include <cuda.h>
char error_msg[10240];
CUresult no_error = CUresult(0);
CUresult error_code = no_error;
#define CUDA_CHECK(condition) \
do { \
CUresult error = condition; \
if (error != 0) { \
error_code = error; \
char* error_string; \
cuGetErrorString(error, (const char**)&error_string); \
snprintf(error_msg, sizeof(error_msg), "CUDA Error: %s at %s:%d", \
error_string, __FILE__, __LINE__); \
std::cerr << error_msg << std::endl; \
} \
} while (0)
static PyObject* g_python_malloc_callback = nullptr;
static PyObject* g_python_free_callback = nullptr;
void ensure_context(unsigned long long device) {
CUcontext pctx;
CUDA_CHECK(cuCtxGetCurrent(&pctx));
if (!pctx) {
CUDA_CHECK(cuDevicePrimaryCtxRetain(&pctx, device));
CUDA_CHECK(cuCtxSetCurrent(pctx));
}
}
void create_and_map(unsigned long long device, ssize_t size, CUdeviceptr d_mem,
CUmemGenericAllocationHandle* p_memHandle) {
ensure_context(device);
CUmemAllocationProp prop = {};
prop.type = CU_MEM_ALLOCATION_TYPE_PINNED;
prop.location.type = CU_MEM_LOCATION_TYPE_DEVICE;
prop.location.id = device;
prop.allocFlags.compressionType = CU_MEM_ALLOCATION_COMP_NONE;
CUDA_CHECK(cuMemCreate(p_memHandle, size, &prop, 0));
if (error_code != 0) {
return;
}
CUDA_CHECK(cuMemMap(d_mem, size, 0, *p_memHandle, 0));
if (error_code != 0) {
return;
}
CUmemAccessDesc accessDesc = {};
accessDesc.location.type = CU_MEM_LOCATION_TYPE_DEVICE;
accessDesc.location.id = device;
accessDesc.flags = CU_MEM_ACCESS_FLAGS_PROT_READWRITE;
CUDA_CHECK(cuMemSetAccess(d_mem, size, &accessDesc, 1));
if (error_code != 0) {
return;
}
}
void unmap_and_release(unsigned long long device, ssize_t size,
CUdeviceptr d_mem,
CUmemGenericAllocationHandle* p_memHandle) {
ensure_context(device);
CUDA_CHECK(cuMemUnmap(d_mem, size));
if (error_code != 0) {
return;
}
CUDA_CHECK(cuMemRelease(*p_memHandle));
if (error_code != 0) {
return;
}
}
PyObject* create_tuple_from_c_integers(unsigned long long a,
unsigned long long b,
unsigned long long c,
unsigned long long d) {
PyObject* tuple = PyTuple_New(4);
if (!tuple) {
return NULL;
}
PyTuple_SetItem(
tuple, 0,
PyLong_FromUnsignedLongLong(a));
PyTuple_SetItem(tuple, 1, PyLong_FromUnsignedLongLong(b));
PyTuple_SetItem(tuple, 2, PyLong_FromUnsignedLongLong(c));
PyTuple_SetItem(tuple, 3, PyLong_FromUnsignedLongLong(d));
return tuple;
}
void* my_malloc(ssize_t size, int device, CUstream stream) {
ensure_context(device);
CUmemAllocationProp prop = {};
prop.type = CU_MEM_ALLOCATION_TYPE_PINNED;
prop.location.type = CU_MEM_LOCATION_TYPE_DEVICE;
prop.location.id = device;
prop.allocFlags.compressionType = CU_MEM_ALLOCATION_COMP_NONE;
size_t granularity;
CUDA_CHECK(cuMemGetAllocationGranularity(&granularity, &prop,
CU_MEM_ALLOC_GRANULARITY_MINIMUM));
if (error_code != 0) {
return nullptr;
}
size_t alignedSize = ((size + granularity - 1) / granularity) * granularity;
CUdeviceptr d_mem;
CUDA_CHECK(cuMemAddressReserve(&d_mem, alignedSize, 0, 0, 0));
if (error_code != 0) {
return nullptr;
}
CUmemGenericAllocationHandle* p_memHandle =
(CUmemGenericAllocationHandle*)malloc(
sizeof(CUmemGenericAllocationHandle));
if (!g_python_malloc_callback) {
std::cerr << "ERROR: g_python_malloc_callback not set.\n";
return nullptr;
}
PyGILState_STATE gstate = PyGILState_Ensure();
PyObject* arg_tuple = create_tuple_from_c_integers(
(unsigned long long)device, (unsigned long long)alignedSize,
(unsigned long long)d_mem, (unsigned long long)p_memHandle);
PyObject* py_result =
PyObject_CallFunctionObjArgs(g_python_malloc_callback, arg_tuple, NULL);
Py_DECREF(arg_tuple);
if (!py_result) {
PyErr_Print();
PyGILState_Release(gstate);
return nullptr;
}
PyGILState_Release(gstate);
create_and_map(device, alignedSize, d_mem, p_memHandle);
return (void*)d_mem;
}
void my_free(void* ptr, ssize_t size, int device, CUstream stream) {
if (!g_python_free_callback) {
std::cerr << "ERROR: g_python_free_callback not set.\n";
return;
}
PyGILState_STATE gstate = PyGILState_Ensure();
PyObject* py_ptr =
PyLong_FromUnsignedLongLong(reinterpret_cast<unsigned long long>(ptr));
PyObject* py_result =
PyObject_CallFunctionObjArgs(g_python_free_callback, py_ptr, NULL);
if (!py_result || !PyTuple_Check(py_result) || PyTuple_Size(py_result) != 4) {
PyErr_SetString(PyExc_TypeError, "Expected a tuple of size 4");
return;
}
unsigned long long recv_device, recv_size;
unsigned long long recv_d_mem, recv_p_memHandle;
if (!PyArg_ParseTuple(py_result, "KKKK", &recv_device, &recv_size,
&recv_d_mem, &recv_p_memHandle)) {
return;
}
PyGILState_Release(gstate);
CUdeviceptr d_mem = (CUdeviceptr)recv_d_mem;
CUmemGenericAllocationHandle* p_memHandle =
(CUmemGenericAllocationHandle*)recv_p_memHandle;
unmap_and_release(device, size, d_mem, p_memHandle);
CUDA_CHECK(cuMemAddressFree(d_mem, size));
if (error_code != 0) {
return;
}
free(p_memHandle);
}
static PyObject* py_init_module(PyObject* self, PyObject* args) {
PyObject* malloc_callback = nullptr;
PyObject* free_callback = nullptr;
if (!PyArg_ParseTuple(args, "OO", &malloc_callback, &free_callback)) {
return nullptr;
}
if (!PyCallable_Check(malloc_callback) || !PyCallable_Check(free_callback)) {
PyErr_SetString(PyExc_TypeError, "Both arguments must be callables");
return nullptr;
}
g_python_malloc_callback = malloc_callback;
g_python_free_callback = free_callback;
Py_RETURN_NONE;
}
static PyObject* python_unmap_and_release(PyObject* self, PyObject* args) {
if (!args || !PyTuple_Check(args) || PyTuple_Size(args) != 4) {
PyErr_SetString(PyExc_TypeError, "Expected a tuple of size 4");
return nullptr;
}
unsigned long long recv_device, recv_size;
unsigned long long recv_d_mem, recv_p_memHandle;
if (!PyArg_ParseTuple(args, "KKKK", &recv_device, &recv_size, &recv_d_mem,
&recv_p_memHandle)) {
return nullptr;
}
CUdeviceptr d_mem_ptr = (CUdeviceptr)recv_d_mem;
CUmemGenericAllocationHandle* p_memHandle =
(CUmemGenericAllocationHandle*)recv_p_memHandle;
unmap_and_release(recv_device, recv_size, d_mem_ptr, p_memHandle);
if (error_code != 0) {
error_code = no_error;
PyErr_SetString(PyExc_RuntimeError, error_msg);
return nullptr;
}
Py_RETURN_NONE;
}
static PyObject* python_create_and_map(PyObject* self, PyObject* args) {
if (!args || !PyTuple_Check(args) || PyTuple_Size(args) != 4) {
PyErr_SetString(PyExc_TypeError, "Expected a tuple of size 4");
return nullptr;
}
unsigned long long recv_device, recv_size;
unsigned long long recv_d_mem, recv_p_memHandle;
if (!PyArg_ParseTuple(args, "KKKK", &recv_device, &recv_size, &recv_d_mem,
&recv_p_memHandle)) {
return nullptr;
}
CUdeviceptr d_mem_ptr = (CUdeviceptr)recv_d_mem;
CUmemGenericAllocationHandle* p_memHandle =
(CUmemGenericAllocationHandle*)recv_p_memHandle;
create_and_map(recv_device, recv_size, d_mem_ptr, p_memHandle);
if (error_code != 0) {
error_code = no_error;
PyErr_SetString(PyExc_RuntimeError, error_msg);
return nullptr;
}
Py_RETURN_NONE;
}
static PyMethodDef module_methods[] = {
{"init_module", (PyCFunction)py_init_module, METH_VARARGS,
"Initialize module with python_malloc and python_free callables."},
{"python_create_and_map", (PyCFunction)python_create_and_map, METH_VARARGS,
"Create and map memory on the device."},
{"python_unmap_and_release", (PyCFunction)python_unmap_and_release,
METH_VARARGS, "Unmap and release memory on the device."},
{NULL, NULL, 0, NULL}
};
static struct PyModuleDef cumem_allocator_module = {
PyModuleDef_HEAD_INIT, "cumem_allocator",
"cumem-based allocator for CUDAPluggableAllocator", -1, module_methods};
PyMODINIT_FUNC PyInit_cumem_allocator(void) {
PyObject* module = PyModule_Create(&cumem_allocator_module);
if (!module) {
return NULL;
}
return module;
}
}