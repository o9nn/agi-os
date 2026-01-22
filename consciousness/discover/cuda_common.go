package discover
import (
	"fmt"
	"log/slog"
	"os"
	"regexp"
	"runtime"
	"strconv"
	"strings"
)
var CudaTegra string = os.Getenv("JETSON_JETPACK")
func cudaGetVisibleDevicesEnv(gpuInfo []GpuInfo) (string, string) {
	ids := []string{}
	for _, info := range gpuInfo {
		if info.Library != "cuda" {
			slog.Debug("cudaGetVisibleDevicesEnv skipping over non-cuda device", "library", info.Library)
			continue
		}
		ids = append(ids, info.ID)
	}
	return "CUDA_VISIBLE_DEVICES", strings.Join(ids, ",")
}
func cudaVariant(gpuInfo CudaGPUInfo) string {
	if runtime.GOARCH == "arm64" && runtime.GOOS == "linux" {
		if CudaTegra != "" {
			ver := strings.Split(CudaTegra, ".")
			if len(ver) > 0 {
				return "jetpack" + ver[0]
			}
		} else if data, err := os.ReadFile("/etc/nv_tegra_release"); err == nil {
			r := regexp.MustCompile(` R(\d+) `)
			m := r.FindSubmatch(data)
			if len(m) != 2 {
				slog.Info("Unexpected format for /etc/nv_tegra_release.  Set JETSON_JETPACK to select version")
			} else {
				if l4t, err := strconv.Atoi(string(m[1])); err == nil {
					switch l4t {
					case 35:
						return "jetpack5"
					case 36:
						return "jetpack6"
					default:
						slog.Info("unsupported L4T version", "nv_tegra_release", string(data))
					}
				}
			}
		}
		return "sbsa"
	}
	if gpuInfo.DriverMajor < 12 || (gpuInfo.DriverMajor == 12 && gpuInfo.DriverMinor == 0) {
		slog.Warn("old CUDA driver detected - please upgrade to a newer driver", "version", fmt.Sprintf("%d.%d", gpuInfo.DriverMajor, gpuInfo.DriverMinor))
		return "v11"
	}
	return "v12"
}