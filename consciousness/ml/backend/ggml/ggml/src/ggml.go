package ggml
import "C"
import (
"context"
"fmt"
"log/slog"
"os"
"path/filepath"
"runtime"
"strconv"
"strings"
"sync"
"unsafe"
_ "github.com/EchoCog/echollama/ml/backend/ggml/ggml/src/ggml-cpu"
)
func init() {
C.ggml_log_set(C.ggml_log_callback(C.sink), nil)
}
func sink(level C.int, text *C.char, _ unsafe.Pointer) {
if slog.Default().Enabled(context.TODO(), slog.Level(int(level-C.GGML_LOG_LEVEL_INFO)*4)) {
fmt.Fprint(os.Stderr, C.GoString(text))
}
}
var OnceLoad = sync.OnceFunc(func() {
exe, err := os.Executable()
if err != nil {
slog.Warn("failed to get executable path", "error", err)
exe = "."
}
var value string
switch runtime.GOOS {
case "darwin":
value = filepath.Dir(exe)
case "windows":
value = filepath.Join(filepath.Dir(exe), "lib", "ollama")
default:
value = filepath.Join(filepath.Dir(exe), "..", "lib", "ollama")
}
paths, ok := os.LookupEnv("OLLAMA_LIBRARY_PATH")
if !ok {
slog.Debug("OLLAMA_LIBRARY_PATH not set, falling back to default", "search", value)
paths = value
}
split := filepath.SplitList(paths)
visited := make(map[string]struct{}, len(split))
for _, path := range split {
abspath, err := filepath.Abs(path)
if err != nil {
slog.Error("failed to get absolute path", "error", err)
continue
}
if abspath != filepath.Dir(exe) && !strings.Contains(abspath, filepath.FromSlash("lib/ollama")) {
slog.Debug("skipping path which is not part of ollama", "path", abspath)
continue
}
if _, ok := visited[abspath]; !ok {
func() {
slog.Debug("ggml backend load all from path", "path", abspath)
cpath := C.CString(abspath)
defer C.free(unsafe.Pointer(cpath))
C.ggml_backend_load_all_from_path(cpath)
}()
visited[abspath] = struct{}{}
}
}
slog.Info("system", "", system{})
})
type system struct{}
func (system) LogValue() slog.Value {
var attrs []slog.Attr
names := make(map[string]int)
for i := range C.ggml_backend_dev_count() {
r := C.ggml_backend_dev_backend_reg(C.ggml_backend_dev_get(i))
func() {
fName := C.CString("ggml_backend_get_features")
defer C.free(unsafe.Pointer(fName))
if fn := C.ggml_backend_reg_get_proc_address(r, fName); fn != nil {
var features []any
for f := C.first_feature(C.ggml_backend_get_features_t(fn), r); f.name != nil; f = C.next_feature(f) {
features = append(features, C.GoString(f.name), C.GoString(f.value))
}
name := C.GoString(C.ggml_backend_reg_name(r))
attrs = append(attrs, slog.Group(name+"."+strconv.Itoa(names[name]), features...))
names[name] += 1
}
}()
}
switch C.compiler_name() {
case C.COMPILER_CLANG:
attrs = append(attrs, slog.String("compiler", "cgo(clang)"))
case C.COMPILER_GNUC:
attrs = append(attrs, slog.String("compiler", "cgo(gcc)"))
default:
attrs = append(attrs, slog.String("compiler", "cgo(unknown)"))
}
return slog.GroupValue(attrs...)
}