package blob
import (
"crypto/sha256"
"encoding/hex"
"errors"
"fmt"
"slices"
"strings"
)
var ErrInvalidDigest = errors.New("invalid digest")
type Digest struct {
sum [32]byte
}
func ParseDigest[S ~[]byte | ~string](v S) (Digest, error) {
s := string(v)
i := strings.IndexAny(s, ":-")
var zero Digest
if i < 0 {
return zero, ErrInvalidDigest
}
prefix, sum := s[:i], s[i+1:]
if prefix != "sha256" || len(sum) != 64 {
return zero, ErrInvalidDigest
}
var d Digest
_, err := hex.Decode(d.sum[:], []byte(sum))
if err != nil {
return zero, ErrInvalidDigest
}
return d, nil
}
func DigestFromBytes[S ~[]byte | ~string](v S) Digest {
return Digest{sha256.Sum256([]byte(v))}
}
func (d Digest) String() string {
return fmt.Sprintf("sha256:%x", d.sum[:])
}
func (d Digest) Short() string {
return fmt.Sprintf("%x", d.sum[:4])
}
func (d Digest) Sum() [32]byte {
return d.sum
}
func (d Digest) Compare(other Digest) int {
return slices.Compare(d.sum[:], other.sum[:])
}
func (d Digest) IsValid() bool {
return d != (Digest{})
}
func (d Digest) MarshalText() ([]byte, error) {
return []byte(d.String()), nil
}
func (d *Digest) UnmarshalText(text []byte) error {
if *d != (Digest{}) {
return errors.New("digest: illegal UnmarshalText on valid digest")
}
v, err := ParseDigest(string(text))
if err != nil {
return err
}
*d = v
return nil
}