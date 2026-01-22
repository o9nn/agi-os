package names
import (
	"cmp"
	"fmt"
	"strings"
	"github.com/EchoCog/echollama/server/internal/internal/stringsx"
)
const MaxNameLength = 350 + 1 + 80 + 1 + 80 + 1 + 80 
type Name struct {
	_ [0]func()
	h string
	n string
	m string
	t string
}
func Parse(s string) Name {
	if len(s) > MaxNameLength {
		return Name{}
	}
	var n Name
	var tail string
	var c byte
	for {
		s, tail, c = cutLastAny(s, "/:")
		switch c {
		case ':':
			n.t = tail
			continue 
		case '/':
			n.h, n.n, _ = cutLastAny(s, "/")
			n.m = tail
			return n
		case 0:
			n.m = tail
			return n
		}
	}
}
func Split(s string) (scheme, name, digest string) {
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
func Merge(a, b Name) Name {
	a.h = cmp.Or(a.h, b.h)
	a.n = cmp.Or(a.n, b.n)
	a.t = cmp.Or(a.t, b.t)
	return a
}
func (n Name) IsValid() bool {
	if n.h != "" && !isValidPart(partHost, n.h) {
		return false
	}
	if n.n != "" && !isValidPart(partNamespace, n.n) {
		return false
	}
	if n.t != "" && !isValidPart(partTag, n.t) {
		return false
	}
	return n.m != "" && isValidPart(partModel, n.m)
}
func (n Name) IsFullyQualified() bool {
	return n.IsValid() && n.h != "" && n.n != "" && n.m != "" && n.t != ""
}
const (
	partHost = iota
	partNamespace
	partModel
	partTag
)
func isValidPart(kind int, s string) bool {
	maxlen := 80
	if kind == partHost {
		maxlen = 350
	}
	if len(s) > maxlen {
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
			if kind == partNamespace {
				return false
			}
		case ':':
			if kind != partHost {
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
func (n Name) Host() string      { return n.h }
func (n Name) Namespace() string { return n.n }
func (n Name) Model() string     { return n.m }
func (n Name) Tag() string       { return n.t }
func (n Name) Compare(o Name) int {
	return cmp.Or(
		stringsx.CompareFold(n.h, o.h),
		stringsx.CompareFold(n.n, o.n),
		stringsx.CompareFold(n.m, o.m),
		stringsx.CompareFold(n.t, o.t),
	)
}
func (n Name) String() string {
	var b strings.Builder
	if n.h != "" {
		b.WriteString(n.h)
		b.WriteByte('/')
	}
	if n.n != "" {
		b.WriteString(n.n)
		b.WriteByte('/')
	}
	b.WriteString(n.m)
	if n.t != "" {
		b.WriteByte(':')
		b.WriteString(n.t)
	}
	return b.String()
}
func (n Name) GoString() string {
	return fmt.Sprintf("<Name %q %q %q %q>", n.h, n.n, n.m, n.t)
}
func cutLastAny(s, chars string) (before, after string, sep byte) {
	i := strings.LastIndexAny(s, chars)
	if i >= 0 {
		return s[:i], s[i+1:], s[i]
	}
	return "", s, 0
}