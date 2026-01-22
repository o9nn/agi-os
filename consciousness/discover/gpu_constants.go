package discover
import "github.com/EchoCog/echollama/format"
const (
cudaMinimumMemory = 457 * format.MebiByte
rocmMinimumMemory = 457 * format.MebiByte
)
const IGPUMemLimit = 1 * format.GibiByte
var (
CudaComputeMajorMin = "5"
CudaComputeMinorMin = "0"
)
var RocmComputeMajorMin = "9"
var (
unsupportedGPUs []UnsupportedGPUInfo
bootstrapErrors []error
)