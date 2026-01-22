package envconfig
import (
"log/slog"
"math"
"testing"
"time"
"github.com/google/go-cmp/cmp"
"github.com/EchoCog/echollama/logutil"
)
func TestHost(t *testing.T) {
cases := map[string]struct {
value  string
expect string
}{
"empty":               {"", "http:
"only address":        {"1.2.3.4", "http:
"only port":           {":1234", "http:
"address and port":    {"1.2.3.4:1234", "http:
"hostname":            {"example.com", "http:
"hostname and port":   {"example.com:1234", "http:
"zero port":           {":0", "http:
"too large port":      {":66000", "http:
"too small port":      {":-1", "http:
"ipv6 localhost":      {"[::1]", "http:
"ipv6 world open":     {"[::]", "http:
"ipv6 no brackets":    {"::1", "http:
"ipv6 + port":         {"[::1]:1337", "http:
"extra space":         {" 1.2.3.4 ", "http:
"extra quotes":        {"\"1.2.3.4\"", "http:
"extra space+quotes":  {" \" 1.2.3.4 \" ", "http:
"extra single quotes": {"'1.2.3.4'", "http:
"http":                {"http:
"http port":           {"http:
"https":               {"https:
"https port":          {"https:
"proxy path":          {"https:
}
for name, tt := range cases {
t.Run(name, func(t *testing.T) {
t.Setenv("OLLAMA_HOST", tt.value)
if host := Host(); host.String() != tt.expect {
t.Errorf("%s: expected %s, got %s", name, tt.expect, host.String())
}
})
}
}
func TestOrigins(t *testing.T) {
cases := []struct {
value  string
expect []string
}{
{"", []string{
"http:
"https:
"http:
"https:
"http:
"https:
"http:
"https:
"http:
"https:
"http:
"https:
"app:
"file:
"tauri:
"vscode-webview:
"vscode-file:
}},
{"http:
"http:
"http:
"https:
"http:
"https:
"http:
"https:
"http:
"https:
"http:
"https:
"http:
"https:
"app:
"file:
"tauri:
"vscode-webview:
"vscode-file:
}},
{"http:
"http:
"https:
"http:
"https:
"http:
"https:
"http:
"https:
"http:
"https:
"http:
"https:
"http:
"https:
"app:
"file:
"tauri:
"vscode-webview:
"vscode-file:
}},
{"http:
"http:
"http:
"http:
"https:
"http:
"https:
"http:
"https:
"http:
"https:
"http:
"https:
"http:
"https:
"app:
"file:
"tauri:
"vscode-webview:
"vscode-file:
}},
}
for _, tt := range cases {
t.Run(tt.value, func(t *testing.T) {
t.Setenv("OLLAMA_ORIGINS", tt.value)
if diff := cmp.Diff(AllowedOrigins(), tt.expect); diff != "" {
t.Errorf("%s: mismatch (-want +got):\n%s", tt.value, diff)
}
})
}
}
func TestBool(t *testing.T) {
cases := map[string]bool{
"":      false,
"true":  true,
"false": false,
"1":     true,
"0":     false,
"random":    true,
"something": true,
}
for k, v := range cases {
t.Run(k, func(t *testing.T) {
t.Setenv("OLLAMA_BOOL", k)
if b := Bool("OLLAMA_BOOL")(); b != v {
t.Errorf("%s: expected %t, got %t", k, v, b)
}
})
}
}
func TestUint(t *testing.T) {
cases := map[string]uint{
"0":    0,
"1":    1,
"1337": 1337,
"":       11434,
"-1":     11434,
"0o10":   11434,
"0x10":   11434,
"string": 11434,
}
for k, v := range cases {
t.Run(k, func(t *testing.T) {
t.Setenv("OLLAMA_UINT", k)
if i := Uint("OLLAMA_UINT", 11434)(); i != v {
t.Errorf("%s: expected %d, got %d", k, v, i)
}
})
}
}
func TestKeepAlive(t *testing.T) {
cases := map[string]time.Duration{
"":       5 * time.Minute,
"1s":     time.Second,
"1m":     time.Minute,
"1h":     time.Hour,
"5m0s":   5 * time.Minute,
"1h2m3s": 1*time.Hour + 2*time.Minute + 3*time.Second,
"0":      time.Duration(0),
"60":     60 * time.Second,
"120":    2 * time.Minute,
"3600":   time.Hour,
"-0":     time.Duration(0),
"-1":     time.Duration(math.MaxInt64),
"-1m":    time.Duration(math.MaxInt64),
" ":   5 * time.Minute,
"???": 5 * time.Minute,
"1d":  5 * time.Minute,
"1y":  5 * time.Minute,
"1w":  5 * time.Minute,
}
for tt, expect := range cases {
t.Run(tt, func(t *testing.T) {
t.Setenv("OLLAMA_KEEP_ALIVE", tt)
if actual := KeepAlive(); actual != expect {
t.Errorf("%s: expected %s, got %s", tt, expect, actual)
}
})
}
}
func TestLoadTimeout(t *testing.T) {
defaultTimeout := 5 * time.Minute
cases := map[string]time.Duration{
"":       defaultTimeout,
"1s":     time.Second,
"1m":     time.Minute,
"1h":     time.Hour,
"5m0s":   defaultTimeout,
"1h2m3s": 1*time.Hour + 2*time.Minute + 3*time.Second,
"0":      time.Duration(math.MaxInt64),
"60":     60 * time.Second,
"120":    2 * time.Minute,
"3600":   time.Hour,
"-0":     time.Duration(math.MaxInt64),
"-1":     time.Duration(math.MaxInt64),
"-1m":    time.Duration(math.MaxInt64),
" ":   defaultTimeout,
"???": defaultTimeout,
"1d":  defaultTimeout,
"1y":  defaultTimeout,
"1w":  defaultTimeout,
}
for tt, expect := range cases {
t.Run(tt, func(t *testing.T) {
t.Setenv("OLLAMA_LOAD_TIMEOUT", tt)
if actual := LoadTimeout(); actual != expect {
t.Errorf("%s: expected %s, got %s", tt, expect, actual)
}
})
}
}
func TestVar(t *testing.T) {
cases := map[string]string{
"value":       "value",
" value ":     "value",
" 'value' ":   "value",
` "value" `:   "value",
" ' value ' ": " value ",
` " value " `: " value ",
}
for k, v := range cases {
t.Run(k, func(t *testing.T) {
t.Setenv("OLLAMA_VAR", k)
if s := Var("OLLAMA_VAR"); s != v {
t.Errorf("%s: expected %q, got %q", k, v, s)
}
})
}
}
func TestContextLength(t *testing.T) {
cases := map[string]uint{
"":     4096,
"2048": 2048,
}
for k, v := range cases {
t.Run(k, func(t *testing.T) {
t.Setenv("OLLAMA_CONTEXT_LENGTH", k)
if i := ContextLength(); i != v {
t.Errorf("%s: expected %d, got %d", k, v, i)
}
})
}
}
func TestLogLevel(t *testing.T) {
cases := map[string]slog.Level{
"":      slog.LevelInfo,
"false": slog.LevelInfo,
"f":     slog.LevelInfo,
"0":     slog.LevelInfo,
"true": slog.LevelDebug,
"t":    slog.LevelDebug,
"1": slog.LevelDebug,
"2": logutil.LevelTrace,
"-1": slog.LevelWarn,
"-2": slog.LevelError,
}
for k, v := range cases {
t.Run(k, func(t *testing.T) {
t.Setenv("OLLAMA_DEBUG", k)
if i := LogLevel(); i != v {
t.Errorf("%s: expected %d, got %d", k, v, i)
}
})
}
}