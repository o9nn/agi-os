package blob
import (
	"bytes"
	"crypto/sha256"
	"errors"
	"fmt"
	"hash"
	"io"
	"io/fs"
	"iter"
	"os"
	"path/filepath"
	"strings"
	"time"
	"github.com/EchoCog/echollama/server/internal/internal/names"
)
type Entry struct {
	Digest Digest
	Size   int64
	Time   time.Time 
}
type DiskCache struct {
	dir string
	now func() time.Time
	testHookBeforeFinalWrite func(f *os.File)
}
func PutBytes[S string | []byte](c *DiskCache, d Digest, data S) error {
	return c.Put(d, bytes.NewReader([]byte(data)), int64(len(data)))
}
func Open(dir string) (*DiskCache, error) {
	if dir == "" {
		return nil, errors.New("blob: empty directory name")
	}
	info, err := os.Stat(dir)
	if err == nil && !info.IsDir() {
		return nil, fmt.Errorf("%q is not a directory", dir)
	}
	if err := os.MkdirAll(dir, 0o777); err != nil {
		return nil, err
	}
	subdirs := []string{"blobs", "manifests"}
	for _, subdir := range subdirs {
		if err := os.MkdirAll(filepath.Join(dir, subdir), 0o777); err != nil {
			return nil, err
		}
	}
	c := &DiskCache{
		dir: dir,
		now: time.Now,
	}
	return c, nil
}
func readAndSum(filename string, limit int64) (data []byte, _ Digest, err error) {
	f, err := os.Open(filename)
	if err != nil {
		return nil, Digest{}, err
	}
	defer f.Close()
	h := sha256.New()
	r := io.TeeReader(f, h)
	data, err = io.ReadAll(io.LimitReader(r, limit))
	if err != nil {
		return nil, Digest{}, err
	}
	var d Digest
	h.Sum(d.sum[:0])
	return data, d, nil
}
var debug = false
func debugger(err *error) func(step string) {
	if !debug {
		return func(string) {}
	}
	var steps []string
	return func(step string) {
		if step == "" && *err != nil {
			*err = fmt.Errorf("%q: %w", steps, *err)
			return
		}
		steps = append(steps, step)
		if len(steps) > 100 {
			copy(steps, steps[1:])
			steps = steps[:100]
		}
	}
}
func (c *DiskCache) Resolve(name string) (Digest, error) {
	name, digest := splitNameDigest(name)
	if digest != "" {
		return ParseDigest(digest)
	}
	file, err := c.manifestPath(name)
	if err != nil {
		return Digest{}, err
	}
	data, d, err := readAndSum(file, 1<<20)
	if err != nil {
		return Digest{}, err
	}
	if err := PutBytes(c, d, data); err != nil {
		return Digest{}, err
	}
	return d, nil
}
func (c *DiskCache) Put(d Digest, r io.Reader, size int64) error {
	return c.copyNamedFile(c.GetFile(d), r, d, size)
}
func (c *DiskCache) Import(r io.Reader, size int64) (Digest, error) {
	f, err := os.CreateTemp("", "blob-")
	if err != nil {
		return Digest{}, err
	}
	defer os.Remove(f.Name())
	h := sha256.New()
	r = io.TeeReader(r, h)
	n, err := io.Copy(f, r)
	if err != nil {
		return Digest{}, err
	}
	if n != size {
		return Digest{}, fmt.Errorf("blob: expected %d bytes, got %d", size, n)
	}
	var d Digest
	h.Sum(d.sum[:0])
	if err := f.Close(); err != nil {
		return Digest{}, err
	}
	name := c.GetFile(d)
	if err := os.Rename(f.Name(), name); err != nil {
		return Digest{}, err
	}
	os.Chtimes(name, c.now(), c.now()) 
	return d, nil
}
func (c *DiskCache) Get(d Digest) (Entry, error) {
	name := c.GetFile(d)
	info, err := os.Stat(name)
	if err != nil {
		return Entry{}, err
	}
	if info.Size() == 0 {
		return Entry{}, fs.ErrNotExist
	}
	return Entry{
		Digest: d,
		Size:   info.Size(),
		Time:   info.ModTime(),
	}, nil
}
func (c *DiskCache) Link(name string, d Digest) error {
	manifest, err := c.manifestPath(name)
	if err != nil {
		return err
	}
	f, err := os.OpenFile(c.GetFile(d), os.O_RDONLY, 0)
	if err != nil {
		return err
	}
	defer f.Close()
	if err := os.MkdirAll(filepath.Dir(manifest), 0o777); err != nil {
		return err
	}
	info, err := f.Stat()
	if err != nil {
		return err
	}
	return c.copyNamedFile(manifest, f, d, info.Size())
}
func (c *DiskCache) Unlink(name string) (ok bool, _ error) {
	manifest, err := c.manifestPath(name)
	if err != nil {
		return false, err
	}
	err = os.Remove(manifest)
	if errors.Is(err, fs.ErrNotExist) {
		return false, nil
	}
	return true, err
}
func (c *DiskCache) GetFile(d Digest) string {
	filename := fmt.Sprintf("sha256-%x", d.sum)
	return absJoin(c.dir, "blobs", filename)
}
func (c *DiskCache) Links() iter.Seq2[string, error] {
	return func(yield func(string, error) bool) {
		for path, err := range c.links() {
			if err != nil {
				yield("", err)
				return
			}
			if !yield(pathToName(path), nil) {
				return
			}
		}
	}
}
func pathToName(s string) string {
	s = strings.TrimPrefix(s, "manifests/")
	rr := []rune(s)
	for i := len(rr) - 1; i > 0; i-- {
		if rr[i] == '/' {
			rr[i] = ':'
			return string(rr)
		}
	}
	return s
}
func (c *DiskCache) manifestPath(name string) (string, error) {
	np, err := nameToPath(name)
	if err != nil {
		return "", err
	}
	maybe := filepath.Join("manifests", np)
	for l, err := range c.links() {
		if err != nil {
			return "", err
		}
		if strings.EqualFold(maybe, l) {
			return filepath.Join(c.dir, l), nil
		}
	}
	return filepath.Join(c.dir, maybe), nil
}
func (c *DiskCache) links() iter.Seq2[string, error] {
	return func(yield func(string, error) bool) {
		fsys := os.DirFS(c.dir)
		manifests, err := fs.Glob(fsys, "manifests*/*")
		if err != nil {
			yield("", err)
			return
		}
		for _, manifest := range manifests {
			if !yield(manifest, nil) {
				return
			}
		}
	}
}
type checkWriter struct {
	size int64
	d    Digest
	f    *os.File
	h    hash.Hash
	w   io.Writer 
	n   int64
	err error
	testHookBeforeFinalWrite func(*os.File)
}
func (w *checkWriter) seterr(err error) error {
	if w.err == nil {
		w.err = err
	}
	return err
}
func (w *checkWriter) Write(p []byte) (int, error) {
	if w.err != nil {
		return 0, w.err
	}
	_, err := w.h.Write(p)
	if err != nil {
		return 0, w.seterr(err)
	}
	nextSize := w.n + int64(len(p))
	if nextSize == w.size {
		sum := w.h.Sum(nil)
		if !bytes.Equal(sum, w.d.sum[:]) {
			return 0, w.seterr(fmt.Errorf("file content changed underfoot"))
		}
		if w.testHookBeforeFinalWrite != nil {
			w.testHookBeforeFinalWrite(w.f)
		}
	}
	if nextSize > w.size {
		return 0, w.seterr(fmt.Errorf("content exceeds expected size: %d > %d", nextSize, w.size))
	}
	n, err := w.w.Write(p)
	w.n += int64(n)
	return n, w.seterr(err)
}
func (c *DiskCache) copyNamedFile(name string, file io.Reader, out Digest, size int64) error {
	info, err := os.Stat(name)
	if err == nil && info.Size() == size {
		return nil
	}
	mode := os.O_RDWR | os.O_CREATE
	if err == nil && info.Size() > size { 
		mode |= os.O_TRUNC
	}
	f, err := os.OpenFile(name, mode, 0o666)
	if err != nil {
		return err
	}
	defer f.Close()
	if size == 0 {
		return nil
	}
	cw := &checkWriter{
		d:    out,
		size: size,
		h:    sha256.New(),
		f:    f,
		w:    f,
		testHookBeforeFinalWrite: c.testHookBeforeFinalWrite,
	}
	n, err := io.Copy(cw, file)
	if err != nil {
		f.Truncate(0)
		return err
	}
	if n < size {
		f.Truncate(0)
		return io.ErrUnexpectedEOF
	}
	if err := f.Close(); err != nil {
		os.Remove(name)
		return err
	}
	os.Chtimes(name, c.now(), c.now()) 
	return nil
}
func splitNameDigest(s string) (name, digest string) {
	i := strings.LastIndexByte(s, '@')
	if i < 0 {
		return s, ""
	}
	return s[:i], s[i+1:]
}
var errInvalidName = errors.New("invalid name")
func nameToPath(name string) (_ string, err error) {
	n := names.Parse(name)
	if !n.IsFullyQualified() {
		return "", errInvalidName
	}
	return filepath.Join(n.Host(), n.Namespace(), n.Model(), n.Tag()), nil
}
func absJoin(pp ...string) string {
	abs, err := filepath.Abs(filepath.Join(pp...))
	if err != nil {
		panic(err) 
	}
	return abs
}