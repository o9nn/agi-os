package discover
import (
"fmt"
"log/slog"
"github.com/EchoCog/echollama/format"
)
type memInfo struct {
TotalMemory uint64 `json:"total_memory,omitempty"`
FreeMemory  uint64 `json:"free_memory,omitempty"`
FreeSwap    uint64 `json:"free_swap,omitempty"`
}
type GpuInfo struct {
memInfo
Library string `json:"library,omitempty"`
Variant string `json:"variant"`
MinimumMemory uint64 `json:"-"`
DependencyPath []string `json:"lib_path,omitempty"`
EnvWorkarounds [][2]string `json:"envs,omitempty"`
UnreliableFreeMemory bool
ID      string `json:"gpu_id"`
Name    string `json:"name"`
Compute string `json:"compute"`
DriverMajor int `json:"driver_major,omitempty"`
DriverMinor int `json:"driver_minor,omitempty"`
}
func (gpu GpuInfo) RunnerName() string {
if gpu.Variant != "" {
return gpu.Library + "_" + gpu.Variant
}
return gpu.Library
}
type CPUInfo struct {
GpuInfo
CPUs []CPU
}
type CPU struct {
ID                  string `cpuinfo:"processor"`
VendorID            string `cpuinfo:"vendor_id"`
ModelName           string `cpuinfo:"model name"`
CoreCount           int
EfficiencyCoreCount int
ThreadCount         int
}
type CudaGPUInfo struct {
GpuInfo
OSOverhead   uint64
index        int
computeMajor int
computeMinor int
}
type CudaGPUInfoList []CudaGPUInfo
type RocmGPUInfo struct {
GpuInfo
usedFilepath string
index        int
}
type RocmGPUInfoList []RocmGPUInfo
type OneapiGPUInfo struct {
GpuInfo
driverIndex int
gpuIndex    int
}
type OneapiGPUInfoList []OneapiGPUInfo
type GpuInfoList []GpuInfo
type UnsupportedGPUInfo struct {
GpuInfo
Reason string `json:"reason"`
}
func (l GpuInfoList) ByLibrary() []GpuInfoList {
resp := []GpuInfoList{}
libs := []string{}
for _, info := range l {
found := false
requested := info.Library
if info.Variant != "" {
requested += "_" + info.Variant
}
for i, lib := range libs {
if lib == requested {
resp[i] = append(resp[i], info)
found = true
break
}
}
if !found {
libs = append(libs, requested)
resp = append(resp, []GpuInfo{info})
}
}
return resp
}
func (l GpuInfoList) LogDetails() {
for _, g := range l {
slog.Info("inference compute",
"id", g.ID,
"library", g.Library,
"variant", g.Variant,
"compute", g.Compute,
"driver", fmt.Sprintf("%d.%d", g.DriverMajor, g.DriverMinor),
"name", g.Name,
"total", format.HumanBytes2(g.TotalMemory),
"available", format.HumanBytes2(g.FreeMemory),
)
}
}
type ByFreeMemory []GpuInfo
func (a ByFreeMemory) Len() int           { return len(a) }
func (a ByFreeMemory) Swap(i, j int)      { a[i], a[j] = a[j], a[i] }
func (a ByFreeMemory) Less(i, j int) bool { return a[i].FreeMemory < a[j].FreeMemory }
type SystemInfo struct {
System          CPUInfo              `json:"system"`
GPUs            []GpuInfo            `json:"gpus"`
UnsupportedGPUs []UnsupportedGPUInfo `json:"unsupported_gpus"`
DiscoveryErrors []string             `json:"discovery_errors"`
}
func (si SystemInfo) GetOptimalThreadCount() int {
if len(si.System.CPUs) == 0 {
return 0
}
coreCount := 0
for _, c := range si.System.CPUs {
coreCount += c.CoreCount - c.EfficiencyCoreCount
}
return coreCount
}
func (l GpuInfoList) FlashAttentionSupported() bool {
for _, gpu := range l {
supportsFA := gpu.Library == "metal" ||
(gpu.Library == "cuda" && gpu.DriverMajor >= 7) ||
gpu.Library == "rocm"
if !supportsFA {
return false
}
}
return true
}