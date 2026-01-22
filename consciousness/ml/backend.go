package ml
import (
"bytes"
"context"
"encoding/binary"
"fmt"
"log/slog"
"math"
"slices"
"strconv"
"strings"
"github.com/EchoCog/echollama/fs"
)
type Backend interface {
Close()
Load(ctx context.Context, progress func(float32)) error
BackendMemory() BackendMemory
Config() fs.Config
Get(name string) Tensor
NewContext() Context
NewContextSize(size int) Context
}
type BackendCacheConfig interface {
CacheConfig() CacheConfig
}
type CacheConfig struct {
CachePadding int
PermutedV bool
MaskDType DType
MaskBatchPadding int
}
type BackendParams struct {
NumThreads int
MainGPU int
NumGPULayers int
TensorSplit []float32
FlashAttention bool
}
type ErrNoMem struct {
BackendMemory
}
func (e ErrNoMem) Error() string {
return fmt.Sprintf("insufficient memory - required allocations: %+v", e.BackendMemory)
}
type AllocationStatus int
const (
Unallocated AllocationStatus = iota
Failed
Allocated
)
type Memory struct {
Size   uint64
Status AllocationStatus
}
func (m Memory) String() string {
s := fmt.Sprint(m.Size)
switch m.Status {
case Unallocated:
s += "U"
case Failed:
s += "F"
case Allocated:
s += "A"
}
return s
}
type DeviceMemory struct {
Name string
ID string
Weights []Memory
Cache []Memory
Graph Memory
}
func memoryPresent(mem []Memory) bool {
return slices.ContainsFunc(mem, func(m Memory) bool { return m.Size != 0 })
}
func (m DeviceMemory) LogValue() slog.Value {
var attrs []slog.Attr
if memoryPresent(m.Weights) {
attrs = append(attrs, slog.Any("Weights", m.Weights))
}
if memoryPresent(m.Cache) {
attrs = append(attrs, slog.Any("Cache", m.Cache))
}
if m.Graph.Size != 0 {
attrs = append(attrs, slog.Any("Graph", m.Graph))
}
if len(attrs) > 0 && m.ID != "" {
attrs = append([]slog.Attr{slog.String("ID", m.ID)}, attrs...)
}
return slog.GroupValue(attrs...)
}
type BackendMemory struct {
InputWeights Memory
CPU DeviceMemory
GPUs []DeviceMemory
}
func (m BackendMemory) LogValue() slog.Value {
var attrs []slog.Attr
if m.InputWeights.Size != 0 {
attrs = append(attrs, slog.Any("InputWeights", m.InputWeights))
}
attrs = append(attrs, slog.Any(m.CPU.Name, m.CPU))
for _, g := range m.GPUs {
attrs = append(attrs, slog.Any(g.Name, g))
}
return slog.GroupValue(attrs...)
}
var backends = make(map[string]func(string, BackendParams) (Backend, error))
func RegisterBackend(name string, f func(string, BackendParams) (Backend, error)) {
if _, ok := backends[name]; ok {
panic("backend: backend already registered")
}
backends[name] = f
}
func NewBackend(modelPath string, params BackendParams) (Backend, error) {
if backend, ok := backends["ggml"]; ok {
return backend(modelPath, params)
}
return nil, fmt.Errorf("unsupported backend")
}
type Context interface {
Empty(dtype DType, shape ...int) Tensor
Zeros(dtype DType, shape ...int) Tensor
FromFloatSlice(s []float32, shape ...int) Tensor
FromIntSlice(s []int32, shape ...int) Tensor
Arange(start, stop, step float32, dtype DType) Tensor
Forward(...Tensor) Context
Compute(...Tensor)
Reserve()
MaxGraphNodes() int
Close()
Input() Context
Layer(int) Context
}
type Tensor interface {
Dim(n int) int
Stride(n int) int
Shape() []int
DType() DType
Bytes() []byte
Floats() []float32
Neg(ctx Context) Tensor
Add(ctx Context, t2 Tensor) Tensor
Sub(ctx Context, t2 Tensor) Tensor
Mul(ctx Context, t2 Tensor) Tensor
Div(ctx Context, t2 Tensor) Tensor
Mulmat(ctx Context, t2 Tensor) Tensor
MulmatFullPrec(ctx Context, t2 Tensor) Tensor
MulmatID(ctx Context, t2, ids Tensor) Tensor
Softmax(ctx Context) Tensor
LayerNorm(ctx Context, weight, bias Tensor, eps float32) Tensor
RMSNorm(ctx Context, weight Tensor, eps float32) Tensor
Scale(ctx Context, s float64) Tensor
SumRows(ctx Context) Tensor
AvgPool2D(ctx Context, k, s int, p float32) Tensor
Conv2D(ctx Context, weight Tensor, s0, s1, p0, p1, d0, d1 int) Tensor
IM2Col(ctx Context, weight Tensor, s0, s1, p0, p1, d0, d1 int) Tensor
Sin(ctx Context) Tensor
Cos(ctx Context) Tensor
Tanh(ctx Context) Tensor
GELU(ctx Context) Tensor
QuickGELU(ctx Context) Tensor
SILU(ctx Context) Tensor
RELU(ctx Context) Tensor
Sigmoid(ctx Context) Tensor
Reshape(ctx Context, shape ...int) Tensor
View(ctx Context, offset int, shape ...int) Tensor
Permute(ctx Context, shape ...int) Tensor
Contiguous(ctx Context, shape ...int) Tensor
Set(ctx Context, t2 Tensor, offset int, strides ...int) Tensor
Pad(ctx Context, shape ...int) Tensor
Stack(ctx Context, dim int, s ...Tensor) Tensor
Repeat(ctx Context, dim, n int) Tensor
Concat(ctx Context, t2 Tensor, dim int) Tensor
Rows(ctx Context, t2 Tensor) Tensor
Copy(ctx Context, t2 Tensor) Tensor
Duplicate(ctx Context) Tensor
TopK(ctx Context, k int) Tensor
Argsort(ctx Context) Tensor
Mean(ctx Context) Tensor
Variance(ctx Context) Tensor
Stddev(ctx Context) Tensor
Sqr(ctx Context) Tensor
Sqrt(ctx Context) Tensor
Clamp(ctx Context, min, max float32) Tensor
}
type ScaledDotProductAttention interface {
ScaledDotProductAttention(ctx Context, key, value, mask Tensor, scale float64) Tensor
}
type number interface {
~int | ~int8 | ~int16 | ~int32 | ~int64 |
~uint | ~uint8 | ~uint16 | ~uint32 | ~uint64 |
~float32 | ~float64 |
~complex64 | ~complex128
}
func mul[T number](s ...T) T {
p := T(1)
for _, v := range s {
p *= v
}
return p
}
type DumpOptions func(*dumpOptions)
func DumpWithPrecision(n int) DumpOptions {
return func(opts *dumpOptions) {
opts.Precision = n
}
}
func DumpWithThreshold(n int) DumpOptions {
return func(opts *dumpOptions) {
opts.Threshold = n
}
}
func DumpWithEdgeItems(n int) DumpOptions {
return func(opts *dumpOptions) {
opts.EdgeItems = n
}
}
type dumpOptions struct {
Precision, Threshold, EdgeItems int
}
func Dump(ctx Context, t Tensor, optsFuncs ...DumpOptions) string {
opts := dumpOptions{Precision: 4, Threshold: 1000, EdgeItems: 3}
for _, optsFunc := range optsFuncs {
optsFunc(&opts)
}
if mul(t.Shape()...) <= opts.Threshold {
opts.EdgeItems = math.MaxInt
}
switch t.DType() {
case DTypeF32:
return dump[[]float32](ctx, t, opts.EdgeItems, func(f float32) string {
return strconv.FormatFloat(float64(f), 'f', opts.Precision, 32)
})
case DTypeF16, DTypeQ80, DTypeQ40:
f32 := ctx.Input().Empty(DTypeF32, t.Shape()...)
f32 = t.Copy(ctx, f32)
return dump[[]float32](ctx, f32, opts.EdgeItems, func(f float32) string {
return strconv.FormatFloat(float64(f), 'f', opts.Precision, 32)
})
case DTypeI32:
return dump[[]int32](ctx, t, opts.EdgeItems, func(i int32) string {
return strconv.FormatInt(int64(i), 10)
})
default:
return "<unsupported>"
}
}
func dump[S ~[]E, E number](ctx Context, t Tensor, items int, fn func(E) string) string {
if t.Bytes() == nil {
ctx.Forward(t).Compute(t)
}
s := make(S, mul(t.Shape()...))
if err := binary.Read(bytes.NewBuffer(t.Bytes()), binary.LittleEndian, &s); err != nil {
panic(err)
}
shape := t.Shape()
slices.Reverse(shape)
var sb strings.Builder
var f func([]int, int)
f = func(dims []int, stride int) {
prefix := strings.Repeat(" ", len(shape)-len(dims)+1)
sb.WriteString("[")
defer func() { sb.WriteString("]") }()
for i := 0; i < dims[0]; i++ {
if i >= items && i < dims[0]-items {
sb.WriteString("..., ")
skip := dims[0] - 2*items
if len(dims) > 1 {
stride += mul(append(dims[1:], skip)...)
fmt.Fprint(&sb, strings.Repeat("\n", len(dims)-1), prefix)
}
i += skip - 1
} else if len(dims) > 1 {
f(dims[1:], stride)
stride += mul(dims[1:]...)
if i < dims[0]-1 {
fmt.Fprint(&sb, ",", strings.Repeat("\n", len(dims)-1), prefix)
}
} else {
text := fn(s[stride+i])
if len(text) > 0 && text[0] != '-' {
sb.WriteString(" ")
}
sb.WriteString(text)
if i < dims[0]-1 {
sb.WriteString(", ")
}
}
}
}
f(shape, 0)
return sb.String()
}
type DType int
const (
DTypeOther DType = iota
DTypeF32
DTypeF16
DTypeQ80
DTypeQ40
DTypeI32
DTypeMXFP4
)