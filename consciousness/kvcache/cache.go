package kvcache
import (
"errors"
"github.com/EchoCog/echollama/ml"
"github.com/EchoCog/echollama/model/input"
)
var (
ErrKvCacheFull  = errors.New("could not find a kv cache slot")
ErrNotSupported = errors.New("model does not support operation")
)
type Cache interface {
SetLayer(layer int)
Get(ctx ml.Context) (ml.Tensor, ml.Tensor, ml.Tensor)
Put(ctx ml.Context, key, value ml.Tensor)
SetConfig(ml.CacheConfig)
Init(backend ml.Backend, dtype ml.DType, maxSequences, capacity, maxBatch int)
Close()
StartForward(ctx ml.Context, batch input.Batch, reserve bool) error
CopyPrefix(srcSeq, dstSeq int, len int32)
CanResume(seq int, pos int32) bool
Remove(seq int, beginIndex, endIndex int32) error
}