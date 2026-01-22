package discover
import (
"os"
"path/filepath"
"runtime"
)
var LibOllamaPath string = func() string {
exe, err := os.Executable()
if err != nil {
return ""
}
if eval, err := filepath.EvalSymlinks(exe); err == nil {
exe = eval
}
var libPath string
switch runtime.GOOS {
case "windows":
libPath = filepath.Join(filepath.Dir(exe), "lib", "ollama")
case "linux":
libPath = filepath.Join(filepath.Dir(exe), "..", "lib", "ollama")
case "darwin":
libPath = filepath.Dir(exe)
}
cwd, err := os.Getwd()
if err != nil {
return ""
}
paths := []string{
libPath,
filepath.Join(filepath.Dir(exe), "build", "lib", "ollama"),
filepath.Join(cwd, "build", "lib", "ollama"),
}
for _, p := range paths {
if _, err := os.Stat(p); err == nil {
return p
}
}
return filepath.Dir(exe)
}()