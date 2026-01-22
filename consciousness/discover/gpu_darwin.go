package discover
import "C"
import (
"log/slog"
"runtime"
"syscall"
"github.com/EchoCog/echollama/format"
)
const (
metalMinimumMemory = 512 * format.MebiByte
)
func GetGPUInfo() GpuInfoList {
mem, _ := GetCPUMem()
if runtime.GOARCH == "amd64" {
return []GpuInfo{
{
Library: "cpu",
memInfo: mem,
},
}
}
info := GpuInfo{
Library: "metal",
ID:      "0",
}
info.TotalMemory = uint64(C.getRecommendedMaxVRAM())
info.FreeMemory = info.TotalMemory
info.MinimumMemory = metalMinimumMemory
return []GpuInfo{info}
}
func GetCPUInfo() GpuInfoList {
mem, _ := GetCPUMem()
return []GpuInfo{
{
Library: "cpu",
memInfo: mem,
},
}
}
func GetCPUMem() (memInfo, error) {
return memInfo{
TotalMemory: uint64(C.getPhysicalMemory()),
FreeMemory:  uint64(C.getFreeMemory()),
}, nil
}
func (l GpuInfoList) GetVisibleDevicesEnv() (string, string) {
return "", ""
}
func GetSystemInfo() SystemInfo {
mem, _ := GetCPUMem()
query := "hw.perflevel0.physicalcpu"
perfCores, err := syscall.SysctlUint32(query)
if err != nil {
slog.Warn("failed to discover physical CPU details", "query", query, "error", err)
}
query = "hw.perflevel1.physicalcpu"
efficiencyCores, _ := syscall.SysctlUint32(query)
query = "hw.logicalcpu"
logicalCores, _ := syscall.SysctlUint32(query)
return SystemInfo{
System: CPUInfo{
GpuInfo: GpuInfo{
memInfo: mem,
},
CPUs: []CPU{
{
CoreCount:           int(perfCores + efficiencyCores),
EfficiencyCoreCount: int(efficiencyCores),
ThreadCount:         int(logicalCores),
},
},
},
GPUs: GetGPUInfo(),
}
}