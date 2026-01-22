package ollama
import (
"context"
)
type Trace struct {
Update func(_ *Layer, n int64, _ error)
}
func (t *Trace) update(l *Layer, n int64, err error) {
if t.Update != nil {
t.Update(l, n, err)
}
}
type traceKey struct{}
func WithTrace(ctx context.Context, t *Trace) context.Context {
old := traceFromContext(ctx)
if old == t {
return ctx
}
composed := &Trace{
Update: func(l *Layer, n int64, err error) {
if old != nil {
old.update(l, n, err)
}
t.update(l, n, err)
},
}
return context.WithValue(ctx, traceKey{}, composed)
}
var emptyTrace = &Trace{}
func traceFromContext(ctx context.Context) *Trace {
t, _ := ctx.Value(traceKey{}).(*Trace)
if t == nil {
return emptyTrace
}
return t
}