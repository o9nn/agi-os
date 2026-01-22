package backoff
import (
"context"
"iter"
"math/rand/v2"
"time"
)
func Loop(ctx context.Context, maxBackoff time.Duration) iter.Seq2[int, error] {
var n int
return func(yield func(int, error) bool) {
var t *time.Timer
for {
if ctx.Err() != nil {
yield(n, ctx.Err())
return
}
if !yield(n, nil) {
return
}
n++
d := time.Duration(n*n) * 10 * time.Millisecond
if d > maxBackoff {
d = maxBackoff
}
d = time.Duration(float64(d) * (rand.Float64() + 0.5))
if t == nil {
t = time.NewTimer(d)
} else {
t.Reset(d)
}
select {
case <-ctx.Done():
t.Stop()
case <-t.C:
}
}
}
}