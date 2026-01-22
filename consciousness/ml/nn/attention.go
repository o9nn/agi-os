package nn
import (
"fmt"
"github.com/EchoCog/echollama/kvcache"
"github.com/EchoCog/echollama/ml"
)
func Attention(ctx ml.Context, query, key, value ml.Tensor, scale float64, cache kvcache.Cache) ml.Tensor {
if key != nil && value != nil {
if query.Dim(0) != key.Dim(0) {
panic(fmt.Errorf("d_k in attention operation does not match between query(%v) and key(%v)", query.Dim(0), key.Dim(0)))
}
if key.Dim(1) != value.Dim(1) {
panic(fmt.Errorf("kv_heads in attention operation does not match between key(%v) and value(%v)", key.Dim(1), value.Dim(1)))
}
if key.Dim(2) != value.Dim(2) {
panic(fmt.Errorf("seq_len_k in attention operation does not match between key(%v) and value(%v)", key.Dim(2), value.Dim(2)))
}
if cache != nil {
cache.Put(ctx, key, value)
}
} else if cache == nil {
panic("key & value tensors must be provided if cache is nil")
}
var mask ml.Tensor
if cache != nil {
key, value, mask = cache.Get(ctx)
}
if sdpa, ok := query.(ml.ScaledDotProductAttention); ok && cache != nil {
return sdpa.ScaledDotProductAttention(ctx, key, value, mask, scale)
} else {
query = query.Permute(ctx, 0, 2, 1, 3)
key = key.Permute(ctx, 0, 2, 1, 3)
value = value.Permute(ctx, 1, 2, 0, 3).Contiguous(ctx)
kq := key.MulmatFullPrec(ctx, query)
kq = kq.Scale(ctx, scale)
if mask != nil {
kq = kq.Add(ctx, mask)
}
kq = kq.Softmax(ctx)
kqv := value.Mulmat(ctx, kq)
return kqv.Permute(ctx, 0, 2, 1, 3).Contiguous(ctx)
}
}