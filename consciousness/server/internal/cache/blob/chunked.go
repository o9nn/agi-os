package blob
import (
"crypto/sha256"
"errors"
"io"
"os"
)
type Chunk struct {
Start int64
End   int64
}
func (c Chunk) Size() int64 {
return c.End - c.Start + 1
}
type Chunker struct {
digest Digest
size   int64
f      *os.File
}
func (c *DiskCache) Chunked(d Digest, size int64) (*Chunker, error) {
name := c.GetFile(d)
info, err := os.Stat(name)
if err == nil && info.Size() == size {
return &Chunker{}, nil
}
f, err := os.OpenFile(name, os.O_CREATE|os.O_WRONLY, 0o666)
if err != nil {
return nil, err
}
return &Chunker{digest: d, size: size, f: f}, nil
}
func (c *Chunker) Put(chunk Chunk, d Digest, r io.Reader) error {
if c.f == nil {
return nil
}
cw := &checkWriter{
d:    d,
size: chunk.Size(),
h:    sha256.New(),
f:    c.f,
w:    io.NewOffsetWriter(c.f, chunk.Start),
}
_, err := io.CopyN(cw, r, chunk.Size())
if err != nil && errors.Is(err, io.EOF) {
return io.ErrUnexpectedEOF
}
return err
}
func (c *Chunker) Close() error {
return c.f.Close()
}