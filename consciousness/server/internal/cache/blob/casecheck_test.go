package blob
import (
"fmt"
"os"
"path/filepath"
"runtime"
"strings"
"testing"
)
func isCaseSensitive(dir string) bool {
defer func() {
os.Remove(filepath.Join(dir, "_casecheck"))
}()
exists := func(file string) bool {
_, err := os.Stat(file)
return err == nil
}
file := filepath.Join(dir, "_casecheck")
FILE := filepath.Join(dir, "_CASECHECK")
if exists(file) || exists(FILE) {
panic(fmt.Sprintf("_casecheck already exists in %q; remove and try again.", dir))
}
err := os.WriteFile(file, nil, 0o666)
if err != nil {
panic(err)
}
return !exists(FILE)
}
func isCI() bool {
return os.Getenv("CI") != ""
}
const volumeHint = `
Unable to locate case-insensitive TMPDIR on darwin.
To run tests, create the case-insensitive volume /Volumes/data:
$ sudo diskutil apfs addVolume disk1 APFSX data -mountpoint /Volumes/data
or run with:
CI=1 go test ./...
`
func useCaseInsensitiveTempDir(t *testing.T) bool {
if isCaseSensitive(os.TempDir()) {
return true
}
if runtime.GOOS == "darwin" {
const volume = "/Volumes/data"
_, err := os.Stat(volume)
if err == nil {
tmpdir := filepath.Join(volume, "tmp")
os.MkdirAll(tmpdir, 0o700)
t.Setenv("TMPDIR", tmpdir)
return true
}
if isCI() {
t.Skip("Skipping test in CI for darwin; TMPDIR is not case-insensitive.")
}
}
if !isCI() {
lines := strings.Split(volumeHint, "\n")
for _, line := range lines {
t.Skip(line)
}
}
return false
}