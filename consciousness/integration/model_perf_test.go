package integration
import (
"context"
"fmt"
"io/ioutil"
"log/slog"
"math"
"os"
"path/filepath"
"strconv"
"strings"
"testing"
"time"
"github.com/EchoCog/echollama/api"
"github.com/EchoCog/echollama/format"
)
var (
longContextFlakes = []string{
"granite-code:latest",
"nemotron-mini:latest",
"falcon:latest",
"falcon2:latest",
"minicpm-v:latest",
"qwen:latest",
"solar-pro:latest",
}
)
func TestModelsPerf(t *testing.T) {
softTimeout, hardTimeout := getTimeouts(t)
slog.Info("Setting timeouts", "soft", softTimeout, "hard", hardTimeout)
ctx, cancel := context.WithTimeout(context.Background(), hardTimeout)
defer cancel()
client, _, cleanup := InitServerConnection(ctx, t)
defer cleanup()
var maxVram uint64
var err error
if s := os.Getenv("OLLAMA_MAX_VRAM"); s != "" {
maxVram, err = strconv.ParseUint(s, 10, 64)
if err != nil {
t.Fatalf("invalid  OLLAMA_MAX_VRAM %v", err)
}
} else {
slog.Warn("No VRAM info available, testing all models, so larger ones might timeout...")
}
data, err := ioutil.ReadFile(filepath.Join("testdata", "shakespeare.txt"))
if err != nil {
t.Fatalf("failed to open test data file: %s", err)
}
longPrompt := "summarize the following: " + string(data)
var chatModels []string
if s := os.Getenv("OLLAMA_NEW_ENGINE"); s != "" {
chatModels = ollamaEngineChatModels
} else {
chatModels = append(ollamaEngineChatModels, llamaRunnerChatModels...)
}
for _, model := range chatModels {
t.Run(model, func(t *testing.T) {
if time.Now().Sub(started) > softTimeout {
t.Skip("skipping remaining tests to avoid excessive runtime")
}
if err := PullIfMissing(ctx, client, model); err != nil {
t.Fatalf("pull failed %s", err)
}
var maxContext int
resp, err := client.Show(ctx, &api.ShowRequest{Model: model})
if err != nil {
t.Fatalf("show failed: %s", err)
}
arch := resp.ModelInfo["general.architecture"].(string)
maxContext = int(resp.ModelInfo[fmt.Sprintf("%s.context_length", arch)].(float64))
if maxVram > 0 {
resp, err := client.List(ctx)
if err != nil {
t.Fatalf("list models failed %v", err)
}
for _, m := range resp.Models {
if m.Name == model && float32(m.Size)*0.75 > float32(maxVram) {
t.Skipf("model %s is too large %s for available VRAM %s", model, format.HumanBytes(m.Size), format.HumanBytes(int64(maxVram)))
}
}
}
slog.Info("scneario", "model", model, "max_context", maxContext)
loaded := false
defer func() {
if loaded {
client.Generate(ctx, &api.GenerateRequest{Model: model, KeepAlive: &api.Duration{Duration: 0}}, func(rsp api.GenerateResponse) error { return nil })
}
}()
longContextFlake := false
for _, flake := range longContextFlakes {
if model == flake {
longContextFlake = true
break
}
}
var contexts []int
keepGoing := true
if maxContext > 16384 {
contexts = []int{4096, 8192, 16384, maxContext}
} else if maxContext > 8192 {
contexts = []int{4096, 8192, maxContext}
} else if maxContext > 4096 {
contexts = []int{4096, maxContext}
} else if maxContext > 0 {
contexts = []int{maxContext}
} else {
t.Fatal("unknown max context size")
}
for _, numCtx := range contexts {
if !keepGoing && numCtx > 8192 {
break
}
skipLongPrompt := false
maxPrompt := longPrompt
if len(maxPrompt) > numCtx*2 {
maxPrompt = maxPrompt[:numCtx*2]
}
testCases := []struct {
prompt  string
anyResp []string
}{
{"why is the sky blue?", []string{"rayleigh", "scattering", "atmosphere", "nitrogen", "oxygen"}},
{maxPrompt, []string{"shakespeare", "oppression", "sorrows", "gutenberg", "child", "license", "sonnet", "melancholy"}},
}
var gpuPercent int
for _, tc := range testCases {
if len(tc.prompt) > 100 && (longContextFlake || skipLongPrompt) {
slog.Info("skipping long prompt", "model", model, "num_ctx", numCtx, "gpu_percent", gpuPercent)
continue
}
req := api.GenerateRequest{
Model:     model,
Prompt:    tc.prompt,
KeepAlive: &api.Duration{Duration: 20 * time.Second},
Options: map[string]interface{}{
"temperature": 0,
"seed":        123,
"num_ctx":     numCtx,
},
}
atLeastOne := false
var resp api.GenerateResponse
stream := false
req.Stream = &stream
limit := 5 * time.Minute
genCtx, cancel := context.WithDeadlineCause(
ctx,
time.Now().Add(limit),
fmt.Errorf("generate on model %s with ctx %d took longer than %v", model, numCtx, limit),
)
defer cancel()
err = client.Generate(genCtx, &req, func(rsp api.GenerateResponse) error {
resp = rsp
return nil
})
if err != nil {
if numCtx > 16384 && strings.Contains(err.Error(), "took longer") {
slog.Warn("max context was taking too long, skipping", "error", err)
keepGoing = false
skipLongPrompt = true
continue
}
t.Fatalf("generate error: ctx:%d err:%s", numCtx, err)
}
loaded = true
for _, expResp := range tc.anyResp {
if strings.Contains(strings.ToLower(resp.Response), expResp) {
atLeastOne = true
break
}
}
if !atLeastOne {
t.Fatalf("response didn't contain expected values: ctx:%d  expected:%v response:%s ", numCtx, tc.anyResp, resp.Response)
}
models, err := client.ListRunning(ctx)
if err != nil {
slog.Warn("failed to list running models", "error", err)
continue
}
if len(models.Models) > 1 {
slog.Warn("multiple models loaded, may impact performance results", "loaded", models.Models)
}
for _, m := range models.Models {
if m.Name == model {
if m.SizeVRAM == 0 {
slog.Info("Model fully loaded into CPU")
gpuPercent = 0
keepGoing = false
skipLongPrompt = true
} else if m.SizeVRAM == m.Size {
slog.Info("Model fully loaded into GPU")
gpuPercent = 100
} else {
sizeCPU := m.Size - m.SizeVRAM
cpuPercent := math.Round(float64(sizeCPU) / float64(m.Size) * 100)
gpuPercent = int(100 - cpuPercent)
slog.Info("Model split between CPU/GPU", "CPU", cpuPercent, "GPU", gpuPercent)
keepGoing = false
if gpuPercent < 90 {
skipLongPrompt = true
}
}
}
}
fmt.Fprintf(os.Stderr, "MODEL_PERF_HEADER:%s,%s,%s,%s,%s,%s,%s\n",
"MODEL",
"CONTEXT",
"GPU PERCENT",
"PROMPT COUNT",
"LOAD TIME",
"PROMPT EVAL TPS",
"EVAL TPS",
)
fmt.Fprintf(os.Stderr, "MODEL_PERF_DATA:%s,%d,%d,%d,%0.2f,%0.2f,%0.2f\n",
model,
numCtx,
gpuPercent,
resp.PromptEvalCount,
float64(resp.LoadDuration)/1000000000.0,
float64(resp.PromptEvalCount)/(float64(resp.PromptEvalDuration)/1000000000.0),
float64(resp.EvalCount)/(float64(resp.EvalDuration)/1000000000.0),
)
}
}
})
}
}