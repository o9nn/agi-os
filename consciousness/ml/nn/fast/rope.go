package fast
import (
	"github.com/EchoCog/echollama/ml"
	"github.com/EchoCog/echollama/ml/nn/rope"
)
type fastRoPE interface {
	RoPE(ctx ml.Context, positionIDs ml.Tensor, dim int, base, scale float32, options ...func(*rope.Options)) ml.Tensor
}
func RoPE(ctx ml.Context, t, positions ml.Tensor, dim int, base, scale float32, options ...func(*rope.Options)) ml.Tensor {
	if t, ok := t.(fastRoPE); ok {
		return t.RoPE(ctx, positions, dim, base, scale, options...)
	}
	panic("RoPE not implemented for this tensor type")
}