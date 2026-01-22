package discover
import "C"
import (
"fmt"
"log/slog"
"os"
"path/filepath"
"runtime"
"strconv"
"strings"
"sync"
"unsafe"
"github.com/EchoCog/echollama/envconfig"
"github.com/EchoCog/echollama/format"
)
type cudaHandles struct {
deviceCount int
cudart      *C.cudart_handle_t
nvcuda      *C.nvcuda_handle_t
nvml        *C.nvml_handle_t
}
type oneapiHandles struct {
oneapi      *C.oneapi_handle_t
deviceCount int
}
var (
gpuMutex      sync.Mutex
bootstrapped  bool
cpus          []CPUInfo
cudaGPUs      []CudaGPUInfo
nvcudaLibPath string
cudartLibPath string
oneapiLibPath string
nvmlLibPath   string
rocmGPUs      []RocmGPUInfo
oneapiGPUs    []OneapiGPUInfo
)
func initCudaHandles() *cudaHandles {
cHandles := &cudaHandles{}
if nvmlLibPath != "" {
cHandles.nvml, _, _ = loadNVMLMgmt([]string{nvmlLibPath})
return cHandles
}
if nvcudaLibPath != "" {
cHandles.deviceCount, cHandles.nvcuda, _, _ = loadNVCUDAMgmt([]string{nvcudaLibPath})
return cHandles
}
if cudartLibPath != "" {
cHandles.deviceCount, cHandles.cudart, _, _ = loadCUDARTMgmt([]string{cudartLibPath})
return cHandles
}
slog.Debug("searching for GPU discovery libraries for NVIDIA")
var cudartMgmtPatterns []string
nvcudaMgmtPatterns := NvcudaGlobs
cudartMgmtPatterns = append(cudartMgmtPatterns, filepath.Join(LibOllamaPath, "cuda_v*", CudartMgmtName))
cudartMgmtPatterns = append(cudartMgmtPatterns, CudartGlobs...)
if len(NvmlGlobs) > 0 {
nvmlLibPaths := FindGPULibs(NvmlMgmtName, NvmlGlobs)
if len(nvmlLibPaths) > 0 {
nvml, libPath, err := loadNVMLMgmt(nvmlLibPaths)
if nvml != nil {
slog.Debug("nvidia-ml loaded", "library", libPath)
cHandles.nvml = nvml
nvmlLibPath = libPath
}
if err != nil {
bootstrapErrors = append(bootstrapErrors, err)
}
}
}
nvcudaLibPaths := FindGPULibs(NvcudaMgmtName, nvcudaMgmtPatterns)
if len(nvcudaLibPaths) > 0 {
deviceCount, nvcuda, libPath, err := loadNVCUDAMgmt(nvcudaLibPaths)
if nvcuda != nil {
slog.Debug("detected GPUs", "count", deviceCount, "library", libPath)
cHandles.nvcuda = nvcuda
cHandles.deviceCount = deviceCount
nvcudaLibPath = libPath
return cHandles
}
if err != nil {
bootstrapErrors = append(bootstrapErrors, err)
}
}
cudartLibPaths := FindGPULibs(CudartMgmtName, cudartMgmtPatterns)
if len(cudartLibPaths) > 0 {
deviceCount, cudart, libPath, err := loadCUDARTMgmt(cudartLibPaths)
if cudart != nil {
slog.Debug("detected GPUs", "library", libPath, "count", deviceCount)
cHandles.cudart = cudart
cHandles.deviceCount = deviceCount
cudartLibPath = libPath
return cHandles
}
if err != nil {
bootstrapErrors = append(bootstrapErrors, err)
}
}
return cHandles
}
func initOneAPIHandles() *oneapiHandles {
oHandles := &oneapiHandles{}
if oneapiLibPath != "" {
oHandles.deviceCount, oHandles.oneapi, _, _ = loadOneapiMgmt([]string{oneapiLibPath})
return oHandles
}
oneapiLibPaths := FindGPULibs(OneapiMgmtName, OneapiGlobs)
if len(oneapiLibPaths) > 0 {
var err error
oHandles.deviceCount, oHandles.oneapi, oneapiLibPath, err = loadOneapiMgmt(oneapiLibPaths)
if err != nil {
bootstrapErrors = append(bootstrapErrors, err)
}
}
return oHandles
}
func GetCPUInfo() GpuInfoList {
gpuMutex.Lock()
if !bootstrapped {
gpuMutex.Unlock()
GetGPUInfo()
} else {
gpuMutex.Unlock()
}
return GpuInfoList{cpus[0].GpuInfo}
}
func GetGPUInfo() GpuInfoList {
gpuMutex.Lock()
defer gpuMutex.Unlock()
needRefresh := true
var cHandles *cudaHandles
var oHandles *oneapiHandles
defer func() {
if cHandles != nil {
if cHandles.cudart != nil {
C.cudart_release(*cHandles.cudart)
}
if cHandles.nvcuda != nil {
C.nvcuda_release(*cHandles.nvcuda)
}
if cHandles.nvml != nil {
C.nvml_release(*cHandles.nvml)
}
}
if oHandles != nil {
if oHandles.oneapi != nil {
C.oneapi_release(*oHandles.oneapi)
}
}
}()
if !bootstrapped {
slog.Info("looking for compatible GPUs")
cudaComputeMajorMin, err := strconv.Atoi(CudaComputeMajorMin)
if err != nil {
slog.Error("invalid CudaComputeMajorMin setting", "value", CudaComputeMajorMin, "error", err)
}
cudaComputeMinorMin, err := strconv.Atoi(CudaComputeMinorMin)
if err != nil {
slog.Error("invalid CudaComputeMinorMin setting", "value", CudaComputeMinorMin, "error", err)
}
bootstrapErrors = []error{}
needRefresh = false
var memInfo C.mem_info_t
mem, err := GetCPUMem()
if err != nil {
slog.Warn("error looking up system memory", "error", err)
}
details, err := GetCPUDetails()
if err != nil {
slog.Warn("failed to lookup CPU details", "error", err)
}
cpus = []CPUInfo{
{
GpuInfo: GpuInfo{
memInfo: mem,
Library: "cpu",
ID:      "0",
},
CPUs: details,
},
}
cHandles = initCudaHandles()
for i := range cHandles.deviceCount {
if cHandles.cudart != nil || cHandles.nvcuda != nil {
gpuInfo := CudaGPUInfo{
GpuInfo: GpuInfo{
Library: "cuda",
},
index: i,
}
var driverMajor int
var driverMinor int
if cHandles.cudart != nil {
C.cudart_bootstrap(*cHandles.cudart, C.int(i), &memInfo)
} else {
C.nvcuda_bootstrap(*cHandles.nvcuda, C.int(i), &memInfo)
driverMajor = int(cHandles.nvcuda.driver_major)
driverMinor = int(cHandles.nvcuda.driver_minor)
}
if memInfo.err != nil {
slog.Info("error looking up nvidia GPU memory", "error", C.GoString(memInfo.err))
C.free(unsafe.Pointer(memInfo.err))
continue
}
gpuInfo.TotalMemory = uint64(memInfo.total)
gpuInfo.FreeMemory = uint64(memInfo.free)
gpuInfo.ID = C.GoString(&memInfo.gpu_id[0])
gpuInfo.Compute = fmt.Sprintf("%d.%d", memInfo.major, memInfo.minor)
gpuInfo.computeMajor = int(memInfo.major)
gpuInfo.computeMinor = int(memInfo.minor)
gpuInfo.MinimumMemory = cudaMinimumMemory
gpuInfo.DriverMajor = driverMajor
gpuInfo.DriverMinor = driverMinor
variant := cudaVariant(gpuInfo)
if variant != "" {
variantPath := filepath.Join(LibOllamaPath, "cuda_"+variant)
if _, err := os.Stat(variantPath); err == nil {
gpuInfo.DependencyPath = append([]string{variantPath}, gpuInfo.DependencyPath...)
}
}
gpuInfo.Name = C.GoString(&memInfo.gpu_name[0])
gpuInfo.Variant = variant
if int(memInfo.major) < cudaComputeMajorMin || (int(memInfo.major) == cudaComputeMajorMin && int(memInfo.minor) < cudaComputeMinorMin) {
unsupportedGPUs = append(unsupportedGPUs,
UnsupportedGPUInfo{
GpuInfo: gpuInfo.GpuInfo,
})
slog.Info(fmt.Sprintf("[%d] CUDA GPU is too old. Compute Capability detected: %d.%d", i, memInfo.major, memInfo.minor))
continue
}
if cHandles.nvml != nil {
uuid := C.CString(gpuInfo.ID)
defer C.free(unsafe.Pointer(uuid))
C.nvml_get_free(*cHandles.nvml, uuid, &memInfo.free, &memInfo.total, &memInfo.used)
if memInfo.err != nil {
slog.Warn("error looking up nvidia GPU memory", "error", C.GoString(memInfo.err))
C.free(unsafe.Pointer(memInfo.err))
} else {
if memInfo.free != 0 && uint64(memInfo.free) > gpuInfo.FreeMemory {
gpuInfo.OSOverhead = uint64(memInfo.free) - gpuInfo.FreeMemory
slog.Info("detected OS VRAM overhead",
"id", gpuInfo.ID,
"library", gpuInfo.Library,
"compute", gpuInfo.Compute,
"driver", fmt.Sprintf("%d.%d", gpuInfo.DriverMajor, gpuInfo.DriverMinor),
"name", gpuInfo.Name,
"overhead", format.HumanBytes2(gpuInfo.OSOverhead),
)
}
}
}
cudaGPUs = append(cudaGPUs, gpuInfo)
}
}
if envconfig.IntelGPU() {
oHandles = initOneAPIHandles()
if oHandles != nil && oHandles.oneapi != nil {
for d := range oHandles.oneapi.num_drivers {
if oHandles.oneapi == nil {
slog.Warn("nil oneapi handle with driver count", "count", int(oHandles.oneapi.num_drivers))
continue
}
devCount := C.oneapi_get_device_count(*oHandles.oneapi, C.int(d))
for i := range devCount {
gpuInfo := OneapiGPUInfo{
GpuInfo: GpuInfo{
Library: "oneapi",
},
driverIndex: int(d),
gpuIndex:    int(i),
}
C.oneapi_check_vram(*oHandles.oneapi, C.int(d), i, &memInfo)
var totalFreeMem float64 = float64(memInfo.free) * 0.95
memInfo.free = C.uint64_t(totalFreeMem)
gpuInfo.TotalMemory = uint64(memInfo.total)
gpuInfo.FreeMemory = uint64(memInfo.free)
gpuInfo.ID = C.GoString(&memInfo.gpu_id[0])
gpuInfo.Name = C.GoString(&memInfo.gpu_name[0])
gpuInfo.DependencyPath = []string{LibOllamaPath}
oneapiGPUs = append(oneapiGPUs, gpuInfo)
}
}
}
}
rocmGPUs, err = AMDGetGPUInfo()
if err != nil {
bootstrapErrors = append(bootstrapErrors, err)
}
bootstrapped = true
if len(cudaGPUs) == 0 && len(rocmGPUs) == 0 && len(oneapiGPUs) == 0 {
slog.Info("no compatible GPUs were discovered")
}
}
if needRefresh {
mem, err := GetCPUMem()
if err != nil {
slog.Warn("error looking up system memory", "error", err)
} else {
slog.Debug("updating system memory data",
slog.Group(
"before",
"total", format.HumanBytes2(cpus[0].TotalMemory),
"free", format.HumanBytes2(cpus[0].FreeMemory),
"free_swap", format.HumanBytes2(cpus[0].FreeSwap),
),
slog.Group(
"now",
"total", format.HumanBytes2(mem.TotalMemory),
"free", format.HumanBytes2(mem.FreeMemory),
"free_swap", format.HumanBytes2(mem.FreeSwap),
),
)
cpus[0].FreeMemory = mem.FreeMemory
cpus[0].FreeSwap = mem.FreeSwap
}
var memInfo C.mem_info_t
if cHandles == nil && len(cudaGPUs) > 0 {
cHandles = initCudaHandles()
}
for i, gpu := range cudaGPUs {
if cHandles.nvml != nil {
uuid := C.CString(gpu.ID)
defer C.free(unsafe.Pointer(uuid))
C.nvml_get_free(*cHandles.nvml, uuid, &memInfo.free, &memInfo.total, &memInfo.used)
} else if cHandles.cudart != nil {
C.cudart_bootstrap(*cHandles.cudart, C.int(gpu.index), &memInfo)
} else if cHandles.nvcuda != nil {
C.nvcuda_get_free(*cHandles.nvcuda, C.int(gpu.index), &memInfo.free, &memInfo.total)
memInfo.used = memInfo.total - memInfo.free
} else {
slog.Warn("no valid cuda library loaded to refresh vram usage")
break
}
if memInfo.err != nil {
slog.Warn("error looking up nvidia GPU memory", "error", C.GoString(memInfo.err))
C.free(unsafe.Pointer(memInfo.err))
continue
}
if memInfo.free == 0 {
slog.Warn("error looking up nvidia GPU memory")
continue
}
if cHandles.nvml != nil && gpu.OSOverhead > 0 {
memInfo.free -= C.uint64_t(gpu.OSOverhead)
}
slog.Debug("updating cuda memory data",
"gpu", gpu.ID,
"name", gpu.Name,
"overhead", format.HumanBytes2(gpu.OSOverhead),
slog.Group(
"before",
"total", format.HumanBytes2(gpu.TotalMemory),
"free", format.HumanBytes2(gpu.FreeMemory),
),
slog.Group(
"now",
"total", format.HumanBytes2(uint64(memInfo.total)),
"free", format.HumanBytes2(uint64(memInfo.free)),
"used", format.HumanBytes2(uint64(memInfo.used)),
),
)
cudaGPUs[i].FreeMemory = uint64(memInfo.free)
}
if oHandles == nil && len(oneapiGPUs) > 0 {
oHandles = initOneAPIHandles()
}
for i, gpu := range oneapiGPUs {
if oHandles.oneapi == nil {
slog.Warn("nil oneapi handle with device count", "count", oHandles.deviceCount)
continue
}
C.oneapi_check_vram(*oHandles.oneapi, C.int(gpu.driverIndex), C.int(gpu.gpuIndex), &memInfo)
var totalFreeMem float64 = float64(memInfo.free) * 0.95
memInfo.free = C.uint64_t(totalFreeMem)
oneapiGPUs[i].FreeMemory = uint64(memInfo.free)
}
err = RocmGPUInfoList(rocmGPUs).RefreshFreeMemory()
if err != nil {
slog.Debug("problem refreshing ROCm free memory", "error", err)
}
}
resp := []GpuInfo{}
for _, gpu := range cudaGPUs {
resp = append(resp, gpu.GpuInfo)
}
for _, gpu := range rocmGPUs {
resp = append(resp, gpu.GpuInfo)
}
for _, gpu := range oneapiGPUs {
resp = append(resp, gpu.GpuInfo)
}
if len(resp) == 0 {
resp = append(resp, cpus[0].GpuInfo)
}
return resp
}
func FindGPULibs(baseLibName string, defaultPatterns []string) []string {
gpuLibPaths := []string{}
slog.Debug("Searching for GPU library", "name", baseLibName)
patterns := []string{filepath.Join(LibOllamaPath, baseLibName)}
var ldPaths []string
switch runtime.GOOS {
case "windows":
ldPaths = strings.Split(os.Getenv("PATH"), string(os.PathListSeparator))
case "linux":
ldPaths = strings.Split(os.Getenv("LD_LIBRARY_PATH"), string(os.PathListSeparator))
}
for _, p := range ldPaths {
p, err := filepath.Abs(p)
if err != nil {
continue
}
patterns = append(patterns, filepath.Join(p, baseLibName))
}
patterns = append(patterns, defaultPatterns...)
slog.Debug("gpu library search", "globs", patterns)
for _, pattern := range patterns {
if strings.Contains(pattern, "PhysX") {
slog.Debug("skipping PhysX cuda library path", "path", pattern)
continue
}
matches, _ := filepath.Glob(pattern)
for _, match := range matches {
libPath := match
tmp := match
var err error
for ; err == nil; tmp, err = os.Readlink(libPath) {
if !filepath.IsAbs(tmp) {
tmp = filepath.Join(filepath.Dir(libPath), tmp)
}
libPath = tmp
}
new := true
for _, cmp := range gpuLibPaths {
if cmp == libPath {
new = false
break
}
}
if new {
gpuLibPaths = append(gpuLibPaths, libPath)
}
}
}
slog.Debug("discovered GPU libraries", "paths", gpuLibPaths)
return gpuLibPaths
}
func loadCUDARTMgmt(cudartLibPaths []string) (int, *C.cudart_handle_t, string, error) {
var resp C.cudart_init_resp_t
resp.ch.verbose = getVerboseState()
var err error
for _, libPath := range cudartLibPaths {
lib := C.CString(libPath)
defer C.free(unsafe.Pointer(lib))
C.cudart_init(lib, &resp)
if resp.err != nil {
err = fmt.Errorf("Unable to load cudart library %s: %s", libPath, C.GoString(resp.err))
slog.Debug(err.Error())
C.free(unsafe.Pointer(resp.err))
} else {
err = nil
return int(resp.num_devices), &resp.ch, libPath, err
}
}
return 0, nil, "", err
}
func loadNVCUDAMgmt(nvcudaLibPaths []string) (int, *C.nvcuda_handle_t, string, error) {
var resp C.nvcuda_init_resp_t
resp.ch.verbose = getVerboseState()
var err error
for _, libPath := range nvcudaLibPaths {
lib := C.CString(libPath)
defer C.free(unsafe.Pointer(lib))
C.nvcuda_init(lib, &resp)
if resp.err != nil {
switch resp.cudaErr {
case C.CUDA_ERROR_INSUFFICIENT_DRIVER, C.CUDA_ERROR_SYSTEM_DRIVER_MISMATCH:
err = fmt.Errorf("version mismatch between driver and cuda driver library - reboot or upgrade may be required: library %s", libPath)
slog.Warn(err.Error())
case C.CUDA_ERROR_NO_DEVICE:
err = fmt.Errorf("no nvidia devices detected by library %s", libPath)
slog.Info(err.Error())
case C.CUDA_ERROR_UNKNOWN:
err = fmt.Errorf("unknown error initializing cuda driver library %s: %s. see https:
slog.Warn(err.Error())
default:
msg := C.GoString(resp.err)
if strings.Contains(msg, "wrong ELF class") {
slog.Debug("skipping 32bit library", "library", libPath)
} else {
err = fmt.Errorf("Unable to load cudart library %s: %s", libPath, C.GoString(resp.err))
slog.Info(err.Error())
}
}
C.free(unsafe.Pointer(resp.err))
} else {
err = nil
return int(resp.num_devices), &resp.ch, libPath, err
}
}
return 0, nil, "", err
}
func loadNVMLMgmt(nvmlLibPaths []string) (*C.nvml_handle_t, string, error) {
var resp C.nvml_init_resp_t
resp.ch.verbose = getVerboseState()
var err error
for _, libPath := range nvmlLibPaths {
lib := C.CString(libPath)
defer C.free(unsafe.Pointer(lib))
C.nvml_init(lib, &resp)
if resp.err != nil {
err = fmt.Errorf("Unable to load NVML management library %s: %s", libPath, C.GoString(resp.err))
slog.Info(err.Error())
C.free(unsafe.Pointer(resp.err))
} else {
err = nil
return &resp.ch, libPath, err
}
}
return nil, "", err
}
func loadOneapiMgmt(oneapiLibPaths []string) (int, *C.oneapi_handle_t, string, error) {
var resp C.oneapi_init_resp_t
num_devices := 0
resp.oh.verbose = getVerboseState()
var err error
for _, libPath := range oneapiLibPaths {
lib := C.CString(libPath)
defer C.free(unsafe.Pointer(lib))
C.oneapi_init(lib, &resp)
if resp.err != nil {
err = fmt.Errorf("Unable to load oneAPI management library %s: %s", libPath, C.GoString(resp.err))
slog.Debug(err.Error())
C.free(unsafe.Pointer(resp.err))
} else {
err = nil
for i := range resp.oh.num_drivers {
num_devices += int(C.oneapi_get_device_count(resp.oh, C.int(i)))
}
return num_devices, &resp.oh, libPath, err
}
}
return 0, nil, "", err
}
func getVerboseState() C.uint16_t {
if envconfig.LogLevel() < slog.LevelInfo {
return C.uint16_t(1)
}
return C.uint16_t(0)
}
func (l GpuInfoList) GetVisibleDevicesEnv() (string, string) {
if len(l) == 0 {
return "", ""
}
switch l[0].Library {
case "cuda":
return cudaGetVisibleDevicesEnv(l)
case "rocm":
return rocmGetVisibleDevicesEnv(l)
case "oneapi":
return oneapiGetVisibleDevicesEnv(l)
default:
slog.Debug("no filter required for library " + l[0].Library)
return "", ""
}
}
func GetSystemInfo() SystemInfo {
gpus := GetGPUInfo()
gpuMutex.Lock()
defer gpuMutex.Unlock()
discoveryErrors := []string{}
for _, err := range bootstrapErrors {
discoveryErrors = append(discoveryErrors, err.Error())
}
if len(gpus) == 1 && gpus[0].Library == "cpu" {
gpus = []GpuInfo{}
}
return SystemInfo{
System:          cpus[0],
GPUs:            gpus,
UnsupportedGPUs: unsupportedGPUs,
DiscoveryErrors: discoveryErrors,
}
}