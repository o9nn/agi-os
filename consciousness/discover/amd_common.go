package discover
import (
	"errors"
	"log/slog"
	"os"
	"path/filepath"
	"runtime"
	"strings"
)
func rocmLibUsable(libDir string) bool {
	slog.Debug("evaluating potential rocm lib dir " + libDir)
	for _, g := range ROCmLibGlobs {
		res, _ := filepath.Glob(filepath.Join(libDir, g))
		if len(res) == 0 {
			return false
		}
	}
	return true
}
func GetSupportedGFX(libDir string) ([]string, error) {
	var ret []string
	files, err := filepath.Glob(filepath.Join(libDir, "rocblas", "library", "TensileLibrary_lazy_gfx*.dat"))
	if err != nil {
		return nil, err
	}
	for _, file := range files {
		ret = append(ret, strings.TrimSuffix(strings.TrimPrefix(filepath.Base(file), "TensileLibrary_lazy_"), ".dat"))
	}
	return ret, nil
}
func commonAMDValidateLibDir() (string, error) {
	rocmTargetDir := filepath.Join(LibOllamaPath, "rocm")
	if rocmLibUsable(rocmTargetDir) {
		slog.Debug("detected ROCM next to ollama executable " + rocmTargetDir)
		return rocmTargetDir, nil
	}
	hipPath := os.Getenv("HIP_PATH")
	if hipPath != "" {
		hipLibDir := filepath.Join(hipPath, "bin")
		if rocmLibUsable(hipLibDir) {
			slog.Debug("detected ROCM via HIP_PATH=" + hipPath)
			return hipLibDir, nil
		}
	}
	pathEnv := "LD_LIBRARY_PATH"
	if runtime.GOOS == "windows" {
		pathEnv = "PATH"
	}
	paths := os.Getenv(pathEnv)
	for _, path := range filepath.SplitList(paths) {
		d, err := filepath.Abs(path)
		if err != nil {
			continue
		}
		if rocmLibUsable(d) {
			return d, nil
		}
	}
	for _, path := range RocmStandardLocations {
		if rocmLibUsable(path) {
			return path, nil
		}
	}
	return "", errors.New("no suitable rocm found, falling back to CPU")
}