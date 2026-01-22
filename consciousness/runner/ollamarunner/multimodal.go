package ollamarunner
import (
"errors"
"github.com/EchoCog/echollama/ml"
"github.com/EchoCog/echollama/model/input"
)
type multimodalEntry struct {
mm []input.Multimodal
data [][]float32
}
type multimodalStore map[ml.Tensor]*multimodalEntry
func newMultimodalStore() multimodalStore {
return make(multimodalStore)
}
func (m multimodalStore) addMultimodal(embedding []input.Multimodal) {
entry := &multimodalEntry{mm: embedding}
for _, e := range embedding {
if e.Tensor != nil {
m[e.Tensor] = entry
}
}
}
func (m multimodalStore) getMultimodal(backend ml.Backend, ctx ml.Context, in []input.Multimodal, reserve bool) ([]input.Multimodal, error) {
out := make([]input.Multimodal, len(in))
for i := range out {
if in[i].Tensor != nil {
var err error
out[i].Tensor, err = m.getTensor(backend, ctx, in[i].Tensor, reserve)
if err != nil {
return nil, err
}
}
out[i].Data = in[i].Data
}
return out, nil
}
func (m multimodalStore) getTensor(backend ml.Backend, ctx ml.Context, in ml.Tensor, reserve bool) (ml.Tensor, error) {
entry := m[in]
if entry.data == nil {
computeCtx := backend.NewContext()
defer computeCtx.Close()
var tensors []ml.Tensor
for _, t := range entry.mm {
if t.Tensor != nil {
tensors = append(tensors, t.Tensor)
}
}
if len(tensors) == 0 {
return nil, nil
}
computeCtx.Forward(tensors...)
entry.data = make([][]float32, len(entry.mm))
if !reserve {
computeCtx.Compute(tensors...)
for i, t := range entry.mm {
if t.Tensor != nil {
entry.data[i] = t.Tensor.Floats()
}
}
} else {
computeCtx.Reserve()
}
}
for i, t := range entry.mm {
if in == t.Tensor {
if !reserve {
return ctx.Input().FromFloatSlice(entry.data[i], t.Tensor.Shape()...), nil
} else {
return ctx.Input().Empty(t.Tensor.DType(), t.Tensor.Shape()...), nil
}
}
}
return nil, errors.New("multimodal tensor not found")
}