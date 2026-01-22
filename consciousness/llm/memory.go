package llm
import (
"fmt"
"log/slog"
"os"
"strconv"
"strings"
"github.com/EchoCog/echollama/api"
"github.com/EchoCog/echollama/discover"
"github.com/EchoCog/echollama/envconfig"
"github.com/EchoCog/echollama/format"
"github.com/EchoCog/echollama/fs/ggml"
)
func PredictServerFit(allGpus discover.GpuInfoList, f *ggml.GGML, adapters, projectors []string, opts api.Options, numParallel int) (bool, uint64) {
var estimatedVRAM uint64
for _, gpus := range allGpus.ByLibrary() {
var layerCount int
estimate := EstimateGPULayers(gpus, f, projectors, opts, numParallel)
layerCount, estimatedVRAM = estimate.Layers, estimate.VRAMSize
if opts.NumGPU < 0 {
if layerCount > 0 && layerCount >= int(f.KV().BlockCount()+1) {
return true, estimatedVRAM
}
} else {
if layerCount > 0 && layerCount >= opts.NumGPU {
return true, estimatedVRAM
}
}
}
return false, estimatedVRAM
}
type MemoryEstimate struct {
Layers int
Graph uint64
VRAMSize uint64
TotalSize uint64
TensorSplit string
GPUSizes []uint64
inferenceLibrary    string
layersRequested     int
layersModel         int
availableList       []string
kv                  uint64
allocationsList     []string
memoryWeights       uint64
memoryLayerOutput   uint64
graphFullOffload    uint64
graphPartialOffload uint64
projectorWeights, projectorGraph uint64
}
func EstimateGPULayers(gpus []discover.GpuInfo, f *ggml.GGML, projectors []string, opts api.Options, numParallel int) MemoryEstimate {
var graphPartialOffload uint64
var graphFullOffload uint64
var graphOffload uint64
var llamaEngineProjectorWeights uint64
var ollamaEngineProjectorWeights uint64
var ollamaEngineProjectorGraph uint64
var memoryLayerOutput uint64
var layerSize uint64
var memoryWeights uint64
var fullyLoaded bool
var overflow uint64
overhead := envconfig.GpuOverhead()
availableList := make([]string, len(gpus))
for i, gpu := range gpus {
availableList[i] = format.HumanBytes2(gpu.FreeMemory)
}
slog.Debug("evaluating", "library", gpus[0].Library, "gpu_count", len(gpus), "available", availableList)
for _, projector := range projectors {
llamaEngineProjectorWeights += projectorMemoryRequirements(projector)
opts.NumCtx = max(opts.NumCtx, 2048)
}
if llamaEngineProjectorWeights == 0 {
ollamaEngineProjectorWeights, ollamaEngineProjectorGraph = f.VisionGraphSize()
opts.NumCtx = max(opts.NumCtx, 2048)
}
layers := f.Tensors().GroupLayers()
if blk0, ok := layers["blk.0"]; ok {
layerSize = blk0.Size()
} else {
slog.Warn("model missing blk.0 layer size")
}
var kvct string
if envconfig.FlashAttention() &&
discover.GetGPUInfo().FlashAttentionSupported() &&
f.SupportsFlashAttention() {
requested := strings.ToLower(envconfig.KvCacheType())
if requested != "" && f.SupportsKVCacheType(requested) {
kvct = requested
}
}
kv, graphPartialOffload, graphFullOffload := f.GraphSize(uint64(opts.NumCtx), uint64(min(opts.NumCtx, opts.NumBatch)), numParallel, kvct)
if len(kv) > 0 {
layerSize += kv[0]
}
var kvTotal uint64
for _, kvLayer := range kv {
kvTotal += kvLayer
}
if graphPartialOffload == 0 {
headsKV := f.KV().HeadCountKVMin()
if headsKV == 0 {
headsKV = 1
}
gqa := f.KV().HeadCountMax() / headsKV
graphPartialOffload = gqa * kvTotal / 6
}
if graphFullOffload == 0 {
graphFullOffload = graphPartialOffload
}
if gpus[0].Library == "metal" {
graphPartialOffload = graphFullOffload
} else if len(gpus) > 1 {
graphFullOffload = graphPartialOffload
}
if layer, ok := layers["output_norm"]; ok {
memoryLayerOutput += layer.Size()
}
if layer, ok := layers["output"]; ok {
memoryLayerOutput += layer.Size()
} else if layer, ok := layers["token_embd"]; ok {
memoryLayerOutput += layer.Size()
}
gpuZeroOverhead := llamaEngineProjectorWeights
var layerCount int
layerCounts := make([]int, len(gpus))
gpuAllocations := make([]uint64, len(gpus))
type gs struct {
i int
g *discover.GpuInfo
}
gpusWithSpace := []gs{}
for i := range gpus {
var gzo uint64
if len(gpusWithSpace) == 0 {
gzo = gpuZeroOverhead
}
if gpus[i].FreeMemory < overhead+gzo+max(graphPartialOffload, graphFullOffload)+gpus[i].MinimumMemory+2*layerSize {
slog.Debug("gpu has too little memory to allocate any layers",
"id", gpus[i].ID,
"library", gpus[i].Library,
"variant", gpus[i].Variant,
"compute", gpus[i].Compute,
"driver", fmt.Sprintf("%d.%d", gpus[i].DriverMajor, gpus[i].DriverMinor),
"name", gpus[i].Name,
"total", format.HumanBytes2(gpus[i].TotalMemory),
"available", format.HumanBytes2(gpus[i].FreeMemory),
"minimum_memory", gpus[i].MinimumMemory,
"layer_size", format.HumanBytes2(layerSize),
"gpu_zer_overhead", format.HumanBytes2(gzo),
"partial_offload", format.HumanBytes2(graphPartialOffload),
"full_offload", format.HumanBytes2(graphFullOffload),
)
continue
}
gpusWithSpace = append(gpusWithSpace, gs{i, &gpus[i]})
gpuAllocations[i] += gpus[i].MinimumMemory + layerSize
}
var gpuZeroID int
if len(gpusWithSpace) > 0 {
gpuZeroID = gpusWithSpace[0].i
gpuAllocations[gpuZeroID] += gpuZeroOverhead
} else {
overflow += gpuZeroOverhead
}
for i := int(f.KV().BlockCount()) - 1; i >= 0; i-- {
if blk, ok := layers[fmt.Sprintf("blk.%d", i)]; ok {
layerSize = blk.Size()
layerSize += kv[i]
memoryWeights += blk.Size()
}
if opts.NumGPU >= 0 && layerCount >= opts.NumGPU {
overflow += layerSize
continue
}
for j := len(gpusWithSpace); j > 0; j-- {
g := gpusWithSpace[i%j]
used := gpuAllocations[g.i] + max(graphPartialOffload, graphFullOffload)
if g.g.FreeMemory > overhead+used+layerSize {
gpuAllocations[g.i] += layerSize
layerCounts[g.i]++
layerCount++
break
} else {
gpusWithSpace = append(gpusWithSpace[:i%j], gpusWithSpace[i%j+1:]...)
}
}
if len(gpusWithSpace) == 0 {
overflow += layerSize
}
}
if layerCount >= int(f.KV().BlockCount()) {
fullyLoaded = true
}
memoryLastLayer := memoryLayerOutput + ollamaEngineProjectorWeights + ollamaEngineProjectorGraph
if memoryLastLayer > 0 {
if opts.NumGPU < 0 || layerCount < opts.NumGPU {
for j := len(gpusWithSpace); j > 0; j-- {
g := gpusWithSpace[layerCount%j]
used := gpuAllocations[g.i] + max(graphPartialOffload, graphFullOffload)
if g.g.FreeMemory > overhead+used+memoryLastLayer {
gpuAllocations[g.i] += memoryLastLayer
layerCounts[g.i]++
layerCount++
break
}
}
}
if layerCount < int(f.KV().BlockCount())+1 {
fullyLoaded = false
overflow += memoryLastLayer
}
}
for i := range gpus {
if layerCounts[i] <= 0 {
continue
}
if fullyLoaded {
gpuAllocations[i] += graphFullOffload
} else {
gpuAllocations[i] += graphPartialOffload
}
}
if fullyLoaded {
graphOffload = graphFullOffload
} else {
graphOffload = graphPartialOffload
}
var memoryRequiredPartial, memoryRequiredTotal uint64
for i := range gpuAllocations {
memoryRequiredPartial += gpuAllocations[i]
}
memoryRequiredTotal = memoryRequiredPartial + overflow
tensorSplit := ""
if len(gpus) > 1 {
splits := make([]string, len(gpus))
for i, count := range layerCounts {
splits[i] = strconv.Itoa(count)
}
tensorSplit = strings.Join(splits, ",")
}
allocationsList := []string{}
for _, a := range gpuAllocations {
allocationsList = append(allocationsList, format.HumanBytes2(a))
}
estimate := MemoryEstimate{
TotalSize: memoryRequiredTotal,
Layers:    0,
Graph:     0,
VRAMSize:  0,
GPUSizes:  []uint64{},
inferenceLibrary:    gpus[0].Library,
layersRequested:     opts.NumGPU,
layersModel:         int(f.KV().BlockCount()) + 1,
availableList:       availableList,
kv:                  kvTotal,
allocationsList:     allocationsList,
memoryWeights:       memoryWeights,
memoryLayerOutput:   memoryLayerOutput,
graphFullOffload:    graphFullOffload,
graphPartialOffload: graphPartialOffload,
projectorWeights:    llamaEngineProjectorWeights + ollamaEngineProjectorWeights,
projectorGraph:      ollamaEngineProjectorGraph,
}
if gpus[0].Library == "cpu" {
return estimate
}
if layerCount == 0 {
slog.Debug("insufficient VRAM to load any model layers")
return estimate
}
estimate.Layers = layerCount
estimate.Graph = graphOffload
estimate.VRAMSize = memoryRequiredPartial
estimate.TotalSize = memoryRequiredTotal
estimate.TensorSplit = tensorSplit
estimate.GPUSizes = gpuAllocations
return estimate
}
func (m MemoryEstimate) LogValue() slog.Value {
attrs := []slog.Attr{
slog.String("library", m.inferenceLibrary),
slog.Group(
"layers",
"requested", m.layersRequested,
"model", m.layersModel,
"offload", m.Layers,
"split", m.TensorSplit,
),
slog.Group(
"memory",
"available", m.availableList,
"gpu_overhead", format.HumanBytes2(envconfig.GpuOverhead()),
slog.Group(
"required",
"full", format.HumanBytes2(m.TotalSize),
"partial", format.HumanBytes2(m.VRAMSize),
"kv", format.HumanBytes2(m.kv),
"allocations", m.allocationsList,
),
slog.Group(
"weights",
"total", format.HumanBytes2(m.memoryWeights+m.memoryLayerOutput),
"repeating", format.HumanBytes2(m.memoryWeights),
"nonrepeating", format.HumanBytes2(m.memoryLayerOutput),
),
slog.Group(
"graph",
"full", format.HumanBytes2(m.graphFullOffload),
"partial", format.HumanBytes2(m.graphPartialOffload),
),
),
}
if m.projectorWeights > 0 {
attrs = append(attrs, slog.Group(
"projector",
"weights", format.HumanBytes2(m.projectorWeights),
"graph", format.HumanBytes2(m.projectorGraph),
))
}
return slog.GroupValue(attrs...)
}
func projectorMemoryRequirements(filename string) (weights uint64) {
file, err := os.Open(filename)
if err != nil {
return 0
}
defer file.Close()
ggml, err := ggml.Decode(file, 1024)
if err != nil {
return 0
}
for _, layer := range ggml.Tensors().GroupLayers() {
weights += layer.Size()
}
return weights
}