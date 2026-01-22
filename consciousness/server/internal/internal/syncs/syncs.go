package syncs
import (
"sync"
"sync/atomic"
)
type Group struct {
wg sync.WaitGroup
n  atomic.Int64
}
func (g *Group) Go(f func()) {
g.wg.Add(1)
go func() {
g.n.Add(1)
defer func() {
g.wg.Done()
g.n.Add(-1)
}()
f()
}()
}
func (g *Group) Running() int64 {
return g.n.Load()
}
func (g *Group) Wait() {
g.wg.Wait()
}