package ollama
import (
	"bufio"
	"bytes"
	"cmp"
	"context"
	"crypto"
	"crypto/ed25519"
	"crypto/sha256"
	"crypto/tls"
	"encoding/base64"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"iter"
	"log/slog"
	"net/http"
	"os"
	"path/filepath"
	"runtime"
	"runtime/debug"
	"slices"
	"strconv"
	"strings"
	"sync"
	"sync/atomic"
	"time"
	"golang.org/x/crypto/ssh"
	"golang.org/x/sync/errgroup"
	"github.com/EchoCog/echollama/server/internal/cache/blob"
	"github.com/EchoCog/echollama/server/internal/internal/names"
	_ "embed"
)
var (
	ErrModelNotFound = errors.New("model not found")
	ErrManifestInvalid = errors.New("invalid manifest")
	ErrNameInvalid = errors.New("invalid or missing name")
	ErrCached = errors.New("cached")
	ErrIncomplete = errors.New("incomplete")
)
const (
	DefaultChunkingThreshold = 64 << 20
)
var defaultCache = sync.OnceValues(func() (*blob.DiskCache, error) {
	dir := os.Getenv("OLLAMA_MODELS")
	if dir == "" {
		home, _ := os.UserHomeDir()
		home = cmp.Or(home, ".")
		dir = filepath.Join(home, ".ollama", "models")
	}
	return blob.Open(dir)
})
func DefaultCache() (*blob.DiskCache, error) {
	return defaultCache()
}
type Error struct {
	status  int    `json:"-"` 
	Code    string `json:"code"`
	Message string `json:"message"`
}
func (e *Error) Temporary() bool {
	return e.status >= 500
}
func (e *Error) Error() string {
	var b strings.Builder
	b.WriteString("registry responded with status ")
	b.WriteString(strconv.Itoa(e.status))
	if e.Code != "" {
		b.WriteString(": code ")
		b.WriteString(e.Code)
	}
	if e.Message != "" {
		b.WriteString(": ")
		b.WriteString(e.Message)
	}
	return b.String()
}
func (e *Error) LogValue() slog.Value {
	return slog.GroupValue(
		slog.Int("status", e.status),
		slog.String("code", e.Code),
		slog.String("message", e.Message),
	)
}
func (e *Error) UnmarshalJSON(b []byte) error {
	type E Error
	var v struct {
		Code  string
		Error string
		Errors []E
	}
	if err := json.Unmarshal(b, &v); err != nil {
		return err
	}
	if v.Error != "" {
		e.Code = v.Code
		e.Message = v.Error
		return nil
	}
	if len(v.Errors) == 0 {
		return fmt.Errorf("no messages in error response: %s", string(b))
	}
	*e = Error(v.Errors[0]) 
	return nil
}
const DefaultMask = "registry.ollama.ai/library/_:latest"
var defaultMask = func() names.Name {
	n := names.Parse(DefaultMask)
	if !n.IsFullyQualified() {
		panic("default mask is not fully qualified")
	}
	return n
}()
func CompleteName(name string) string {
	return names.Merge(names.Parse(name), defaultMask).String()
}
type Registry struct {
	Cache *blob.DiskCache
	UserAgent string
	Key crypto.PrivateKey
	HTTPClient *http.Client
	MaxStreams int
	ChunkingThreshold int64
	Mask string
	ReadTimeout time.Duration
}
func (r *Registry) readTimeout() time.Duration {
	if r.ReadTimeout > 0 {
		return r.ReadTimeout
	}
	return 1<<63 - 1 
}
func (r *Registry) cache() (*blob.DiskCache, error) {
	if r.Cache != nil {
		return r.Cache, nil
	}
	return defaultCache()
}
func (r *Registry) parseName(name string) (names.Name, error) {
	mask := defaultMask
	if r.Mask != "" {
		mask = names.Parse(r.Mask)
	}
	n := names.Merge(names.Parse(name), mask)
	if !n.IsFullyQualified() {
		return names.Name{}, fmt.Errorf("%w: %q", ErrNameInvalid, name)
	}
	return n, nil
}
func DefaultRegistry() (*Registry, error) {
	home, err := os.UserHomeDir()
	if err != nil {
		return nil, err
	}
	keyPEM, err := os.ReadFile(filepath.Join(home, ".ollama/id_ed25519"))
	if err != nil && errors.Is(err, fs.ErrNotExist) {
		return nil, err
	}
	var rc Registry
	rc.ReadTimeout = 30 * time.Second
	rc.UserAgent = UserAgent()
	rc.Key, err = ssh.ParseRawPrivateKey(keyPEM)
	if err != nil {
		return nil, err
	}
	maxStreams := os.Getenv("OLLAMA_REGISTRY_MAXSTREAMS")
	if maxStreams != "" {
		var err error
		rc.MaxStreams, err = strconv.Atoi(maxStreams)
		if err != nil {
			return nil, fmt.Errorf("invalid OLLAMA_REGISTRY_MAXSTREAMS: %w", err)
		}
	}
	return &rc, nil
}
func UserAgent() string {
	buildinfo, _ := debug.ReadBuildInfo()
	version := buildinfo.Main.Version
	if version == "(devel)" {
		version = "v0.0.0"
	}
	return fmt.Sprintf("ollama/%s (%s %s) Go/%s",
		version,
		runtime.GOARCH,
		runtime.GOOS,
		runtime.Version(),
	)
}
func (r *Registry) maxStreams() int {
	return cmp.Or(r.MaxStreams, runtime.GOMAXPROCS(0))
}
func (r *Registry) maxChunkingThreshold() int64 {
	return cmp.Or(r.ChunkingThreshold, DefaultChunkingThreshold)
}
type PushParams struct {
	From string
}
func (r *Registry) Push(ctx context.Context, name string, p *PushParams) error {
	if p == nil {
		p = &PushParams{}
	}
	c, err := r.cache()
	if err != nil {
		return err
	}
	m, err := r.ResolveLocal(cmp.Or(p.From, name))
	if err != nil {
		return err
	}
	for _, l := range m.Layers {
		if l == nil {
			return fmt.Errorf("%w: null layer", ErrManifestInvalid)
		}
		info, err := c.Get(l.Digest)
		if err != nil {
			return fmt.Errorf("error getting %s: %w", l.Digest.Short(), err)
		}
		if info.Size != l.Size {
			return fmt.Errorf("size mismatch for %s: %d != %d", l.Digest.Short(), info.Size, l.Size)
		}
	}
	t := traceFromContext(ctx)
	scheme, n, _, err := r.parseNameExtended(name)
	if err != nil {
		panic(err)
	}
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()
	var g errgroup.Group
	g.SetLimit(r.maxStreams())
	for _, l := range m.Layers {
		var progress atomic.Int64
		g.Go(func() (err error) {
			defer func() { t.update(l, progress.Load(), err) }()
			t.update(l, 0, nil)
			startURL := fmt.Sprintf("%s:
				scheme,
				n.Host(),
				n.Namespace(),
				n.Model(),
				l.Digest,
			)
			res, err := r.send(ctx, "POST", startURL, nil)
			if err != nil {
				return err
			}
			res.Body.Close()
			f, err := os.Open(c.GetFile(l.Digest))
			if err != nil {
				return err
			}
			defer f.Close()
			uploadURL := res.Header.Get("Location")
			if uploadURL == "" {
				t.update(l, l.Size, ErrCached)
				return nil
			}
			req, err := r.newRequest(ctx, "PUT", uploadURL, f)
			if err != nil {
				return fmt.Errorf("invalid upload URL returned from registry: %q: %w", uploadURL, err)
			}
			req.ContentLength = l.Size
			res, err = sendRequest(r.client(), req)
			if err == nil {
				res.Body.Close()
			}
			return err
		})
	}
	if err := g.Wait(); err != nil {
		return err
	}
	path := fmt.Sprintf("%s:
		scheme,
		n.Host(),
		n.Namespace(),
		n.Model(),
		n.Tag(),
	)
	res, err := r.send(ctx, "PUT", path, bytes.NewReader(m.Data))
	if err == nil {
		res.Body.Close()
	}
	return err
}
type trackingReader struct {
	r      io.Reader
	update func(n int64, err error) 
}
func (r *trackingReader) Read(p []byte) (n int, err error) {
	n, err = r.r.Read(p)
	r.update(int64(n), nil)
	return
}
func (r *Registry) Pull(ctx context.Context, name string) error {
	m, err := r.Resolve(ctx, name)
	if err != nil {
		return err
	}
	if len(m.Layers) == 0 {
		return fmt.Errorf("%w: no layers", ErrManifestInvalid)
	}
	c, err := r.cache()
	if err != nil {
		return err
	}
	layers := m.Layers
	if m.Config != nil && m.Config.Digest.IsValid() {
		layers = append(layers, m.Config)
	}
	var expected int64
	t := traceFromContext(ctx)
	for _, l := range layers {
		t.update(l, 0, nil)
		expected += l.Size
	}
	var g errgroup.Group
	g.SetLimit(r.maxStreams())
	var completed atomic.Int64
	for _, l := range layers {
		var received atomic.Int64
		update := func(n int64, err error) {
			if n == 0 && err == nil {
				return
			}
			completed.Add(n)
			t.update(l, received.Add(n), err)
		}
		info, err := c.Get(l.Digest)
		if err == nil && info.Size == l.Size {
			update(l.Size, ErrCached)
			continue
		}
		func() (err error) {
			defer func() {
				if err != nil {
					update(0, err)
				}
			}()
			var wg sync.WaitGroup
			chunked, err := c.Chunked(l.Digest, l.Size)
			if err != nil {
				return err
			}
			defer func() {
				g.Go(func() error {
					wg.Wait()
					chunked.Close()
					return nil
				})
			}()
			for cs, err := range r.chunksums(ctx, name, l) {
				if err != nil {
					update(0, err)
					break
				}
				cacheKey := fmt.Sprintf(
					"v1 pull chunksum %s %s %d-%d",
					l.Digest,
					cs.Digest,
					cs.Chunk.Start,
					cs.Chunk.End,
				)
				cacheKeyDigest := blob.DigestFromBytes(cacheKey)
				_, err := c.Get(cacheKeyDigest)
				if err == nil {
					update(cs.Chunk.Size(), ErrCached)
					continue
				}
				wg.Add(1)
				g.Go(func() (err error) {
					defer func() {
						defer wg.Done()
						if err != nil {
							update(0, err)
						}
					}()
					ctx, cancel := context.WithCancelCause(ctx)
					defer cancel(nil)
					timer := time.AfterFunc(r.readTimeout(), func() {
						cancel(fmt.Errorf("%w: downloading %s %d-%d/%d",
							context.DeadlineExceeded,
							cs.Digest.Short(),
							cs.Chunk.Start,
							cs.Chunk.End,
							l.Size,
						))
					})
					defer timer.Stop()
					req, err := http.NewRequestWithContext(ctx, "GET", cs.URL, nil)
					if err != nil {
						return err
					}
					req.Header.Set("Range", fmt.Sprintf("bytes=%d-%d", cs.Chunk.Start, cs.Chunk.End))
					res, err := sendRequest(r.client(), req)
					if err != nil {
						return err
					}
					defer res.Body.Close()
					tr := &trackingReader{
						r: res.Body,
						update: func(n int64, err error) {
							timer.Reset(r.readTimeout())
							update(n, err)
						},
					}
					if err := chunked.Put(cs.Chunk, cs.Digest, tr); err != nil {
						return err
					}
					return blob.PutBytes(c, cacheKeyDigest, cacheKey)
				})
			}
			return nil
		}()
	}
	if err := g.Wait(); err != nil {
		return err
	}
	if recv := completed.Load(); recv != expected {
		return fmt.Errorf("%w: received %d/%d bytes", ErrIncomplete, recv, expected)
	}
	md := blob.DigestFromBytes(m.Data)
	if err := blob.PutBytes(c, md, m.Data); err != nil {
		return err
	}
	return c.Link(m.Name, md)
}
func (r *Registry) Unlink(name string) (ok bool, _ error) {
	n, err := r.parseName(name)
	if err != nil {
		return false, err
	}
	c, err := r.cache()
	if err != nil {
		return false, err
	}
	return c.Unlink(n.String())
}
type Manifest struct {
	Name   string   `json:"-"` 
	Data   []byte   `json:"-"` 
	Layers []*Layer `json:"layers"`
	Config *Layer `json:"config"`
}
func (m *Manifest) Layer(d blob.Digest) *Layer {
	for _, l := range m.Layers {
		if l.Digest == d {
			return l
		}
	}
	return nil
}
func (m *Manifest) All() iter.Seq[*Layer] {
	return func(yield func(*Layer) bool) {
		if !yield(m.Config) {
			return
		}
		for _, l := range m.Layers {
			if !yield(l) {
				return
			}
		}
	}
}
func (m *Manifest) Size() int64 {
	var size int64
	if m.Config != nil {
		size += m.Config.Size
	}
	for _, l := range m.Layers {
		size += l.Size
	}
	return size
}
func (m Manifest) MarshalJSON() ([]byte, error) {
	type M Manifest
	v := struct {
		M
		Config Layer `json:"config"`
	}{
		M: M(m),
	}
	return json.Marshal(v)
}
func unmarshalManifest(n names.Name, data []byte) (*Manifest, error) {
	if !n.IsFullyQualified() {
		panic(fmt.Sprintf("unmarshalManifest: name is not fully qualified: %s", n.String()))
	}
	var m Manifest
	if err := json.Unmarshal(data, &m); err != nil {
		return nil, err
	}
	m.Name = n.String()
	m.Data = data
	return &m, nil
}
type Layer struct {
	Digest    blob.Digest `json:"digest"`
	MediaType string      `json:"mediaType"`
	Size      int64       `json:"size"`
}
func (r *Registry) ResolveLocal(name string) (*Manifest, error) {
	_, n, d, err := r.parseNameExtended(name)
	if err != nil {
		return nil, err
	}
	c, err := r.cache()
	if err != nil {
		return nil, err
	}
	if !d.IsValid() {
		d, err = c.Resolve(n.String())
		if err != nil {
			return nil, err
		}
	}
	data, err := os.ReadFile(c.GetFile(d))
	if err != nil {
		if errors.Is(err, fs.ErrNotExist) {
			return nil, fmt.Errorf("%w: %s", ErrModelNotFound, name)
		}
		return nil, err
	}
	m, err := unmarshalManifest(n, data)
	if err != nil {
		return nil, fmt.Errorf("%s: %w", name, errors.Join(ErrManifestInvalid, err))
	}
	return m, nil
}
func (r *Registry) Resolve(ctx context.Context, name string) (*Manifest, error) {
	scheme, n, d, err := r.parseNameExtended(name)
	if err != nil {
		return nil, err
	}
	manifestURL := fmt.Sprintf("%s:
	if d.IsValid() {
		manifestURL = fmt.Sprintf("%s:
	}
	res, err := r.send(ctx, "GET", manifestURL, nil)
	if err != nil {
		return nil, err
	}
	defer res.Body.Close()
	data, err := io.ReadAll(res.Body)
	if err != nil {
		return nil, err
	}
	m, err := unmarshalManifest(n, data)
	if err != nil {
		return nil, fmt.Errorf("%s: %w", name, errors.Join(ErrManifestInvalid, err))
	}
	return m, nil
}
type chunksum struct {
	URL    string
	Chunk  blob.Chunk
	Digest blob.Digest
}
func (r *Registry) chunksums(ctx context.Context, name string, l *Layer) iter.Seq2[chunksum, error] {
	return func(yield func(chunksum, error) bool) {
		scheme, n, _, err := r.parseNameExtended(name)
		if err != nil {
			yield(chunksum{}, err)
			return
		}
		if l.Size < r.maxChunkingThreshold() {
			cs := chunksum{
				URL: fmt.Sprintf("%s:
					scheme,
					n.Host(),
					n.Namespace(),
					n.Model(),
					l.Digest,
				),
				Chunk:  blob.Chunk{Start: 0, End: l.Size - 1},
				Digest: l.Digest,
			}
			yield(cs, nil)
			return
		}
		chunksumsURL := fmt.Sprintf("%s:
			scheme,
			n.Host(),
			n.Namespace(),
			n.Model(),
			l.Digest,
		)
		req, err := r.newRequest(ctx, "GET", chunksumsURL, nil)
		if err != nil {
			yield(chunksum{}, err)
			return
		}
		res, err := sendRequest(r.client(), req)
		if err != nil {
			yield(chunksum{}, err)
			return
		}
		defer res.Body.Close()
		if res.StatusCode != 200 {
			err := fmt.Errorf("chunksums: unexpected status code %d", res.StatusCode)
			yield(chunksum{}, err)
			return
		}
		blobURL := res.Header.Get("Content-Location")
		s := bufio.NewScanner(res.Body)
		s.Split(bufio.ScanWords)
		for {
			if !s.Scan() {
				if s.Err() != nil {
					yield(chunksum{}, s.Err())
				}
				return
			}
			d, err := blob.ParseDigest(s.Bytes())
			if err != nil {
				yield(chunksum{}, fmt.Errorf("invalid digest: %q", s.Bytes()))
				return
			}
			if !s.Scan() {
				err := s.Err()
				if err == nil {
					err = fmt.Errorf("missing chunk range for digest %s", d)
				}
				yield(chunksum{}, err)
				return
			}
			chunk, err := parseChunk(s.Bytes())
			if err != nil {
				yield(chunksum{}, fmt.Errorf("invalid chunk range for digest %s: %q", d, s.Bytes()))
				return
			}
			cs := chunksum{
				URL:    blobURL,
				Chunk:  chunk,
				Digest: d,
			}
			if !yield(cs, nil) {
				return
			}
		}
	}
}
func (r *Registry) client() *http.Client {
	if r.HTTPClient != nil {
		return r.HTTPClient
	}
	return http.DefaultClient
}
func (r *Registry) newRequest(ctx context.Context, method, url string, body io.Reader) (*http.Request, error) {
	req, err := http.NewRequestWithContext(ctx, method, url, body)
	if err != nil {
		return nil, err
	}
	if r.UserAgent != "" {
		req.Header.Set("User-Agent", r.UserAgent)
	}
	if r.Key != nil {
		token, err := makeAuthToken(r.Key)
		if err != nil {
			return nil, err
		}
		req.Header.Set("Authorization", "Bearer "+token)
	}
	return req, nil
}
func sendRequest(c *http.Client, r *http.Request) (_ *http.Response, err error) {
	if r.URL.Scheme == "https+insecure" {
		type cloner interface {
			Clone() *http.Transport
		}
		x, ok := cmp.Or(c.Transport, http.DefaultTransport).(cloner)
		if ok {
			tr := x.Clone()
			tr.TLSClientConfig = cmp.Or(tr.TLSClientConfig, &tls.Config{})
			tr.TLSClientConfig.InsecureSkipVerify = true
			cc := *c 
			cc.Transport = tr
			c = &cc
			r = r.Clone(r.Context())
			r.URL.Scheme = "https"
		}
	}
	res, err := c.Do(r)
	if err != nil {
		return nil, err
	}
	if res.StatusCode/100 != 2 {
		out, err := io.ReadAll(res.Body)
		if err != nil {
			return nil, err
		}
		var re Error
		if err := json.Unmarshal(out, &re); err != nil {
			re.Message = string(out)
		}
		if strings.EqualFold(re.Code, "MANIFEST_UNKNOWN") {
			return nil, ErrModelNotFound
		}
		re.status = res.StatusCode
		return nil, &re
	}
	return res, nil
}
func (r *Registry) send(ctx context.Context, method, path string, body io.Reader) (*http.Response, error) {
	req, err := r.newRequest(ctx, method, path, body)
	if err != nil {
		return nil, err
	}
	return sendRequest(r.client(), req)
}
func makeAuthToken(key crypto.PrivateKey) (string, error) {
	privKey, _ := key.(*ed25519.PrivateKey)
	if privKey == nil {
		return "", fmt.Errorf("unsupported private key type: %T", key)
	}
	url := fmt.Sprintf("https:
	pubKeyShort, err := func() ([]byte, error) {
		sshPubKey, err := ssh.NewPublicKey(privKey.Public())
		if err != nil {
			return nil, err
		}
		pubKeyParts := bytes.Fields(ssh.MarshalAuthorizedKey(sshPubKey))
		if len(pubKeyParts) < 2 {
			return nil, fmt.Errorf("malformed public key: %q", pubKeyParts)
		}
		pubKeyShort := pubKeyParts[1]
		return pubKeyShort, nil
	}()
	if err != nil {
		return "", err
	}
	sig := ed25519.Sign(*privKey, []byte(checkData(url)))
	var b strings.Builder
	io.WriteString(&b, base64.StdEncoding.EncodeToString([]byte(url)))
	b.WriteByte(':')
	b.Write(pubKeyShort)
	b.WriteByte(':')
	io.WriteString(&b, base64.StdEncoding.EncodeToString(sig))
	return b.String(), nil
}
var zeroSum = func() string {
	sha256sum := sha256.Sum256(nil)
	x := base64.StdEncoding.EncodeToString([]byte(hex.EncodeToString(sha256sum[:])))
	return x
}()
func checkData(url string) string {
	return fmt.Sprintf("GET,%s,%s", url, zeroSum)
}
type publicError struct {
	wrapped error
	message string
}
func withPublicMessagef(err error, message string, args ...any) error {
	return publicError{wrapped: err, message: fmt.Sprintf(message, args...)}
}
func (e publicError) Error() string { return e.message }
func (e publicError) Unwrap() error { return e.wrapped }
var supportedSchemes = []string{
	"http",
	"https",
	"https+insecure",
}
var supportedSchemesMessage = fmt.Sprintf("supported schemes are %v", strings.Join(supportedSchemes, ", "))
func (r *Registry) parseNameExtended(s string) (scheme string, _ names.Name, _ blob.Digest, _ error) {
	scheme, name, digest := splitExtended(s)
	scheme = cmp.Or(scheme, "https")
	if !slices.Contains(supportedSchemes, scheme) {
		err := withPublicMessagef(ErrNameInvalid, "unsupported scheme: %q: %s", scheme, supportedSchemesMessage)
		return "", names.Name{}, blob.Digest{}, err
	}
	var d blob.Digest
	if digest != "" {
		var err error
		d, err = blob.ParseDigest(digest)
		if err != nil {
			err = withPublicMessagef(ErrNameInvalid, "invalid digest: %q", digest)
			return "", names.Name{}, blob.Digest{}, err
		}
		if name == "" {
			return scheme, names.Name{}, d, nil
		}
	}
	n, err := r.parseName(name)
	if err != nil {
		return "", names.Name{}, blob.Digest{}, err
	}
	return scheme, n, d, nil
}
func splitExtended(s string) (scheme, name, digest string) {
	i := strings.Index(s, ":
	if i >= 0 {
		scheme = s[:i]
		s = s[i+3:]
	}
	i = strings.LastIndex(s, "@")
	if i >= 0 {
		digest = s[i+1:]
		s = s[:i]
	}
	return scheme, s, digest
}
func parseChunk[S ~string | ~[]byte](s S) (blob.Chunk, error) {
	startPart, endPart, found := strings.Cut(string(s), "-")
	if !found {
		return blob.Chunk{}, fmt.Errorf("chunks: invalid range %q: missing '-'", s)
	}
	start, err := strconv.ParseInt(startPart, 10, 64)
	if err != nil {
		return blob.Chunk{}, fmt.Errorf("chunks: invalid start to %q: %v", s, err)
	}
	end, err := strconv.ParseInt(endPart, 10, 64)
	if err != nil {
		return blob.Chunk{}, fmt.Errorf("chunks: invalid end to %q: %v", s, err)
	}
	if start > end {
		return blob.Chunk{}, fmt.Errorf("chunks: invalid range %q: start > end", s)
	}
	return blob.Chunk{Start: start, End: end}, nil
}