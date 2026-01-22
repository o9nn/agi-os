package convert
import (
	"cmp"
	"io"
	"iter"
	"path"
	"slices"
	"strings"
	"github.com/pdevine/tensor"
	"github.com/pdevine/tensor/native"
	"github.com/EchoCog/echollama/fs/ggml"
)
type split struct {
	*strings.Replacer
	dim int
	fn func(tensor.Tensor) (tensor.Tensor, error)
}
func splitDim(t Tensor, dim int, splits ...split) iter.Seq[*ggml.Tensor] {
	return func(yield func(*ggml.Tensor) bool) {
		var offset int
		for _, split := range splits {
			t := t.Clone()
			shape := slices.Clone(t.Shape())
			shape[dim] = cmp.Or(uint64(split.dim), shape[dim]/uint64(len(splits)))
			slice := slices.Repeat([]tensor.Slice{nil}, len(shape))
			slice[dim] = tensor.S(offset, offset+int(shape[dim]))
			offset += int(shape[dim])
			t.SetRepacker(func(_ string, data []float32, shape []uint64) ([]float32, error) {
				dims := make([]int, len(shape))
				for i := range shape {
					dims[i] = int(shape[i])
				}
				var tt tensor.Tensor = tensor.New(tensor.WithShape(dims...), tensor.WithBacking(data))
				tt, err := tt.Slice(slice...)
				if err != nil {
					return nil, err
				}
				tt = tensor.Materialize(tt)
				if split.fn != nil {
					tt, err = split.fn(tt)
					if err != nil {
						return nil, err
					}
				}
				if err := tt.Reshape(tt.Shape().TotalSize()); err != nil {
					return nil, err
				}
				return native.VectorF32(tt.(*tensor.Dense))
			})
			if !yield(&ggml.Tensor{
				Name:     split.Replace(t.Name()),
				Kind:     t.Kind(),
				Shape:    shape,
				WriterTo: t,
			}) {
				break
			}
		}
	}
}
type merge struct {
	pattern, name string
}
func mergeTensors(unmatched []Tensor, merges ...merge) (out []*ggml.Tensor, _ []Tensor) {
	var matched []Tensor
	for i := range merges {
		matched, unmatched = slicesSplitFunc(unmatched, func(t Tensor) bool {
			matched, _ := path.Match(merges[i].pattern, t.Name())
			return matched
		})
		if len(matched) > 0 {
			out = append(out, &ggml.Tensor{
				Name:     merges[i].name,
				Kind:     matched[0].Kind(),
				Shape:    append([]uint64{uint64(len(matched))}, matched[0].Shape()...),
				WriterTo: mergeGroup(matched),
			})
		}
	}
	return out, unmatched
}
func slicesSplitFunc[S ~[]E, E comparable](s S, fn func(e E) bool) (matched, unmatched S) {
	for _, e := range s {
		if fn(e) {
			matched = append(matched, e)
		} else {
			unmatched = append(unmatched, e)
		}
	}
	return matched, unmatched
}
type mergeGroup []Tensor
func (g mergeGroup) WriteTo(w io.Writer) (int64, error) {
	for _, t := range g {
		if _, err := t.WriteTo(w); err != nil {
			return 0, err
		}
	}
	return 0, nil
}