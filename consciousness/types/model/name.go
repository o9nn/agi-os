package model
import (
	"cmp"
	"errors"
	"fmt"
	"log/slog"
	"path/filepath"
	"strings"
)
var (
	ErrUnqualifiedName = errors.New("unqualified name")
)
func Unqualified(n Name) error {
	return fmt.Errorf("%w: %s", ErrUnqualifiedName, n)
}
const MissingPart = "!MISSING!"
const (
	defaultHost      = "registry.ollama.ai"
	defaultNamespace = "library"
	defaultTag       = "latest"
)
func DefaultName() Name {
	return Name{
		Host:      defaultHost,
		Namespace: defaultNamespace,
		Tag:       defaultTag,
	}
}
type partKind int
const (
	kindHost partKind = iota
	kindNamespace
	kindModel
	kindTag
	kindDigest
)
func (k partKind) String() string {
	switch k {
	case kindHost:
		return "host"
	case kindNamespace:
		return "namespace"
	case kindModel:
		return "model"
	case kindTag:
		return "tag"
	case kindDigest:
		return "digest"
	default:
		return "unknown"
	}
}
type Name struct {
	Host      string
	Namespace string
	Model     string
	Tag       string
}
func ParseName(s string) Name {
	return Merge(ParseNameBare(s), DefaultName())
}
func ParseNameBare(s string) Name {
	var n Name
	var promised bool
	if strings.LastIndex(s, ":") > strings.LastIndex(s, "/") {
		s, n.Tag, _ = cutPromised(s, ":")
	}
	s, n.Model, promised = cutPromised(s, "/")
	if !promised {
		n.Model = s
		return n
	}
	s, n.Namespace, promised = cutPromised(s, "/")
	if !promised {
		n.Namespace = s
		return n
	}
	scheme, host, ok := strings.Cut(s, ":
	if !ok {
		host = scheme
	}
	n.Host = host
	return n
}
func ParseNameFromFilepath(s string) (n Name) {
	parts := strings.Split(s, string(filepath.Separator))
	if len(parts) != 4 {
		return Name{}
	}
	n.Host = parts[0]
	n.Namespace = parts[1]
	n.Model = parts[2]
	n.Tag = parts[3]
	if !n.IsFullyQualified() {
		return Name{}
	}
	return n
}
func Merge(a, b Name) Name {
	a.Host = cmp.Or(a.Host, b.Host)
	a.Namespace = cmp.Or(a.Namespace, b.Namespace)
	a.Tag = cmp.Or(a.Tag, b.Tag)
	return a
}
func (n Name) String() string {
	var b strings.Builder
	if n.Host != "" {
		b.WriteString(n.Host)
		b.WriteByte('/')
	}
	if n.Namespace != "" {
		b.WriteString(n.Namespace)
		b.WriteByte('/')
	}
	b.WriteString(n.Model)
	if n.Tag != "" {
		b.WriteByte(':')
		b.WriteString(n.Tag)
	}
	return b.String()
}
func (n Name) DisplayShortest() string {
	var sb strings.Builder
	if !strings.EqualFold(n.Host, defaultHost) {
		sb.WriteString(n.Host)
		sb.WriteByte('/')
		sb.WriteString(n.Namespace)
		sb.WriteByte('/')
	} else if !strings.EqualFold(n.Namespace, defaultNamespace) {
		sb.WriteString(n.Namespace)
		sb.WriteByte('/')
	}
	sb.WriteString(n.Model)
	sb.WriteString(":")
	sb.WriteString(n.Tag)
	return sb.String()
}
func IsValidNamespace(s string) bool {
	return isValidPart(kindNamespace, s)
}
func (n Name) IsValid() bool {
	return n.IsFullyQualified()
}
func (n Name) IsFullyQualified() bool {
	parts := []string{
		n.Host,
		n.Namespace,
		n.Model,
		n.Tag,
	}
	for i, part := range parts {
		if !isValidPart(partKind(i), part) {
			return false
		}
	}
	return true
}
func (n Name) Filepath() string {
	if !n.IsFullyQualified() {
		panic("illegal attempt to get filepath of invalid name")
	}
	return filepath.Join(
		n.Host,
		n.Namespace,
		n.Model,
		n.Tag,
	)
}
func (n Name) LogValue() slog.Value {
	return slog.StringValue(n.String())
}
func (n Name) EqualFold(o Name) bool {
	return strings.EqualFold(n.Host, o.Host) &&
		strings.EqualFold(n.Namespace, o.Namespace) &&
		strings.EqualFold(n.Model, o.Model) &&
		strings.EqualFold(n.Tag, o.Tag)
}
func isValidLen(kind partKind, s string) bool {
	switch kind {
	case kindHost:
		return len(s) >= 1 && len(s) <= 350
	case kindTag:
		return len(s) >= 1 && len(s) <= 80
	default:
		return len(s) >= 1 && len(s) <= 80
	}
}
func isValidPart(kind partKind, s string) bool {
	if !isValidLen(kind, s) {
		return false
	}
	for i := range s {
		if i == 0 {
			if !isAlphanumericOrUnderscore(s[i]) {
				return false
			}
			continue
		}
		switch s[i] {
		case '_', '-':
		case '.':
			if kind == kindNamespace {
				return false
			}
		case ':':
			if kind != kindHost && kind != kindDigest {
				return false
			}
		default:
			if !isAlphanumericOrUnderscore(s[i]) {
				return false
			}
		}
	}
	return true
}
func isAlphanumericOrUnderscore(c byte) bool {
	return c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z' || c >= '0' && c <= '9' || c == '_'
}
func cutLast(s, sep string) (before, after string, ok bool) {
	i := strings.LastIndex(s, sep)
	if i >= 0 {
		return s[:i], s[i+len(sep):], true
	}
	return s, "", false
}
func cutPromised(s, sep string) (before, after string, ok bool) {
	before, after, ok = cutLast(s, sep)
	if !ok {
		return before, after, false
	}
	return cmp.Or(before, MissingPart), cmp.Or(after, MissingPart), true
}