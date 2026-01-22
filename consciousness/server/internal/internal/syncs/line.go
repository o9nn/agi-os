package syncs
import (
	"cmp"
	"io"
	"sync"
)
var closedChan = func() chan struct{} {
	ch := make(chan struct{})
	close(ch)
	return ch
}()
type Ticket struct {
	ahead chan struct{} 
	ch    chan struct{}
}
func (t *Ticket) Ready() chan struct{} {
	return cmp.Or(t.ahead, closedChan)
}
func (t *Ticket) Done() {
	if t.ch != nil {
		close(t.ch)
	}
	t.ch = nil
}
type Line struct {
	last chan struct{} 
}
func (q *Line) Take() *Ticket {
	t := &Ticket{
		ahead: q.last,
		ch:    make(chan struct{}),
	}
	q.last = t.ch
	return t
}
type RelayReader struct {
	line Line
	t    *Ticket
	w    io.Writer
	n    int64
	mu       sync.Mutex
	err      error         
	closedCh chan struct{} 
}
var (
	_ io.Closer   = (*RelayReader)(nil)
	_ io.WriterTo = (*RelayReader)(nil)
	_ io.Reader   = (*RelayReader)(nil)
)
func NewRelayReader() *RelayReader {
	var q RelayReader
	q.closedCh = make(chan struct{})
	q.t = q.line.Take()
	return &q
}
func (q *RelayReader) CloseWithError(err error) error {
	q.mu.Lock()
	defer q.mu.Unlock()
	if q.err == nil {
		q.err = cmp.Or(q.err, err, io.EOF)
		close(q.closedCh)
	}
	return nil
}
func (q *RelayReader) Close() error {
	return q.CloseWithError(nil)
}
func (q *RelayReader) closed() <-chan struct{} {
	q.mu.Lock()
	defer q.mu.Unlock()
	return q.closedCh
}
func (q *RelayReader) Read(p []byte) (int, error) {
	panic("RelayReader.Read is for show only; use WriteTo")
}
func (q *RelayReader) WriteTo(dst io.Writer) (int64, error) {
	select {
	case <-q.closed():
		return 0, io.ErrClosedPipe
	default:
	}
	q.w = dst
	q.t.Done()
	<-q.closed()
	return q.n, nil
}
func (q *RelayReader) Take() io.WriteCloser {
	return &relayWriter{q: q, t: q.line.Take()}
}
type relayWriter struct {
	q     *RelayReader
	t     *Ticket
	ready bool
}
var _ io.StringWriter = (*relayWriter)(nil)
func (w *relayWriter) Write(p []byte) (int, error) {
	if !w.awaitTurn() {
		return 0, w.q.err
	}
	n, err := w.q.w.Write(p)
	w.q.n += int64(n)
	return n, err
}
func (w *relayWriter) WriteString(s string) (int, error) {
	if !w.awaitTurn() {
		return 0, w.q.err
	}
	return io.WriteString(w.q.w, s)
}
func (w *relayWriter) Close() error {
	w.t.Done()
	return nil
}
func (t *relayWriter) awaitTurn() (ok bool) {
	if t.ready {
		return true
	}
	select {
	case <-t.t.Ready():
		t.ready = true
		return true
	case <-t.q.closed():
		return false
	}
}