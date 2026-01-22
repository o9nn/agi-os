package discover
import (
	"bytes"
	"errors"
	"fmt"
	"log/slog"
	"path/filepath"
	"slices"
	"strconv"
	"strings"
	"github.com/EchoCog/echollama/envconfig"
	"github.com/EchoCog/echollama/format"
)
const (
	iGPUName = "AMD Radeon(TM) Graphics"
)
var (
	ROCmLibGlobs          = []string{"hipblas.dll", "rocblas"}                 
	RocmStandardLocations = []string{"C:\\Program Files\\AMD\\ROCm\\6.1\\bin"} 
)
func AMDGetGPUInfo() ([]RocmGPUInfo, error) {
	resp := []RocmGPUInfo{}
	hl, err := NewHipLib()
	if err != nil {
		slog.Debug(err.Error())
		return nil, err
	}
	defer hl.Release()
	driverMajor, driverMinor, err := hl.AMDDriverVersion()
	if err != nil {
		slog.Debug("error looking up amd driver version", "error", err)
	}
	count := hl.HipGetDeviceCount()
	if count == 0 {
		err := fmt.Errorf("no compatible amdgpu devices detected")
		slog.Info(err.Error())
		return nil, err
	}
	libDir, err := AMDValidateLibDir()
	if err != nil {
		err = fmt.Errorf("unable to verify rocm library: %w", err)
		slog.Warn(err.Error())
		return nil, err
	}
	var supported []string
	gfxOverride := envconfig.HsaOverrideGfxVersion()
	if gfxOverride == "" {
		supported, err = GetSupportedGFX(libDir)
		if err != nil {
			err = fmt.Errorf("failed to lookup supported GFX types: %w", err)
			slog.Warn(err.Error())
			return nil, err
		}
	} else {
		slog.Info("skipping rocm gfx compatibility check", "HSA_OVERRIDE_GFX_VERSION", gfxOverride)
	}
	slog.Debug("detected hip devices", "count", count)
	for i := range count {
		err = hl.HipSetDevice(i)
		if err != nil {
			slog.Warn("set device", "id", i, "error", err)
			continue
		}
		props, err := hl.HipGetDeviceProperties(i)
		if err != nil {
			slog.Warn("get properties", "id", i, "error", err)
			continue
		}
		n := bytes.IndexByte(props.Name[:], 0)
		name := string(props.Name[:n])
		n = bytes.IndexByte(props.GcnArchName[:], 0)
		gfx := string(props.GcnArchName[:n])
		slog.Debug("hip device", "id", i, "name", name, "gfx", gfx)
		freeMemory, totalMemory, err := hl.HipMemGetInfo()
		if err != nil {
			slog.Warn("get mem info", "id", i, "error", err)
			continue
		}
		gpuInfo := RocmGPUInfo{
			GpuInfo: GpuInfo{
				Library: "rocm",
				memInfo: memInfo{
					TotalMemory: totalMemory,
					FreeMemory:  freeMemory,
				},
				UnreliableFreeMemory: true,
				ID:             strconv.Itoa(i), 
				DependencyPath: []string{libDir},
				MinimumMemory:  rocmMinimumMemory,
				Name:           name,
				Compute:        gfx,
				DriverMajor:    driverMajor,
				DriverMinor:    driverMinor,
			},
			index: i,
		}
		if strings.EqualFold(name, iGPUName) || totalMemory < IGPUMemLimit {
			reason := "unsupported Radeon iGPU detected skipping"
			slog.Info(reason, "id", gpuInfo.ID, "total", format.HumanBytes2(totalMemory))
			unsupportedGPUs = append(unsupportedGPUs, UnsupportedGPUInfo{
				GpuInfo: gpuInfo.GpuInfo,
				Reason:  reason,
			})
			continue
		}
		if !slices.Contains[[]string, string](supported, strings.Split(gfx, ":")[0]) {
			reason := fmt.Sprintf("amdgpu is not supported (supported types:%s)", supported)
			slog.Warn(reason, "gpu_type", gfx, "gpu", gpuInfo.ID, "library", libDir)
			unsupportedGPUs = append(unsupportedGPUs, UnsupportedGPUInfo{
				GpuInfo: gpuInfo.GpuInfo,
				Reason:  reason,
			})
			continue
		} else {
			slog.Debug("amdgpu is supported", "gpu", i, "gpu_type", gfx)
		}
		slog.Debug("amdgpu memory", "gpu", i, "total", format.HumanBytes2(totalMemory))
		slog.Debug("amdgpu memory", "gpu", i, "available", format.HumanBytes2(freeMemory))
		resp = append(resp, gpuInfo)
	}
	return resp, nil
}
func AMDValidateLibDir() (string, error) {
	libDir, err := commonAMDValidateLibDir()
	if err == nil {
		return libDir, nil
	}
	rocmTargetDir := filepath.Join(LibOllamaPath, "rocm")
	if rocmLibUsable(rocmTargetDir) {
		slog.Debug("detected ollama installed ROCm at " + rocmTargetDir)
		return rocmTargetDir, nil
	}
	slog.Warn("amdgpu detected, but no compatible rocm library found.  Please install ROCm")
	return "", errors.New("no suitable rocm found, falling back to CPU")
}
func (gpus RocmGPUInfoList) RefreshFreeMemory() error {
	if len(gpus) == 0 {
		return nil
	}
	hl, err := NewHipLib()
	if err != nil {
		slog.Debug(err.Error())
		return err
	}
	defer hl.Release()
	for i := range gpus {
		err := hl.HipSetDevice(gpus[i].index)
		if err != nil {
			return err
		}
		freeMemory, _, err := hl.HipMemGetInfo()
		if err != nil {
			slog.Warn("get mem info", "id", i, "error", err)
			continue
		}
		slog.Debug("updating rocm free memory", "gpu", gpus[i].ID, "name", gpus[i].Name, "before", format.HumanBytes2(gpus[i].FreeMemory), "now", format.HumanBytes2(freeMemory))
		gpus[i].FreeMemory = freeMemory
	}
	return nil
}
func rocmGetVisibleDevicesEnv(gpuInfo []GpuInfo) (string, string) {
	ids := []string{}
	for _, info := range gpuInfo {
		if info.Library != "rocm" {
			slog.Debug("rocmGetVisibleDevicesEnv skipping over non-rocm device", "library", info.Library)
			continue
		}
		ids = append(ids, info.ID)
	}
	return "HIP_VISIBLE_DEVICES", strings.Join(ids, ",")
}