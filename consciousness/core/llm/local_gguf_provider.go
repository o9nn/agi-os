package llm
import (
"context"
"fmt"
"os"
"strings"
"sync"
llama "github.com/go-skynet/go-llama.cpp"
)
type LocalGGUFProvider struct {
modelPath  string
model      *llama.LLama
mu         sync.Mutex
threads    int
gpuLayers  int
contextSize int
}
func NewLocalGGUFProvider(modelPath string) *LocalGGUFProvider {
if modelPath == "" {
modelPath = os.Getenv("LOCAL_MODEL_PATH")
}
threads := 4
if threadsEnv := os.Getenv("LOCAL_MODEL_THREADS"); threadsEnv != "" {
fmt.Sscanf(threadsEnv, "%d", &threads)
}
gpuLayers := 0
if gpuEnv := os.Getenv("LOCAL_MODEL_GPU_LAYERS"); gpuEnv != "" {
fmt.Sscanf(gpuEnv, "%d", &gpuLayers)
}
contextSize := 2048
if ctxEnv := os.Getenv("LOCAL_MODEL_CONTEXT"); ctxEnv != "" {
fmt.Sscanf(ctxEnv, "%d", &contextSize)
}
provider := &LocalGGUFProvider{
modelPath:   modelPath,
threads:     threads,
gpuLayers:   gpuLayers,
contextSize: contextSize,
}
if modelPath != "" {
if err := provider.loadModel(); err != nil {
fmt.Printf("⚠️  Warning: Failed to load local model: %v\n", err)
}
}
return provider
}
func (lgp *LocalGGUFProvider) loadModel() error {
lgp.mu.Lock()
defer lgp.mu.Unlock()
if lgp.model != nil {
return nil
}
if lgp.modelPath == "" {
return fmt.Errorf("no model path specified")
}
if _, err := os.Stat(lgp.modelPath); os.IsNotExist(err) {
return fmt.Errorf("model file not found: %s", lgp.modelPath)
}
fmt.Printf("🔄 Loading local GGUF model from: %s\n", lgp.modelPath)
opts := []llama.ModelOption{
llama.SetContext(lgp.contextSize),
llama.SetGPULayers(lgp.gpuLayers),
}
if lgp.gpuLayers > 0 {
opts = append(opts, llama.EnableF16Memory)
}
model, err := llama.New(lgp.modelPath, opts...)
if err != nil {
return fmt.Errorf("failed to load model: %w", err)
}
lgp.model = model
fmt.Printf("✓ Local GGUF model loaded successfully\n")
fmt.Printf("   Context size: %d\n", lgp.contextSize)
fmt.Printf("   GPU layers: %d\n", lgp.gpuLayers)
fmt.Printf("   Threads: %d\n", lgp.threads)
return nil
}
func (lgp *LocalGGUFProvider) Generate(ctx context.Context, prompt string, opts GenerateOptions) (string, error) {
lgp.mu.Lock()
defer lgp.mu.Unlock()
if lgp.model == nil {
return "", fmt.Errorf("model not loaded")
}
if opts.MaxTokens == 0 {
opts.MaxTokens = 150
}
if opts.Temperature == 0 {
opts.Temperature = 0.8
}
predictOpts := []llama.PredictOption{
llama.SetTokens(opts.MaxTokens),
llama.SetThreads(lgp.threads),
llama.SetTemperature(float32(opts.Temperature)),
llama.SetTopK(40),
llama.SetTopP(0.9),
llama.SetStopWords("</s>", "[INST]", "[/INST]"),
}
var result strings.Builder
predictOpts = append(predictOpts, llama.SetTokenCallback(func(token string) bool {
select {
case <-ctx.Done():
return false
default:
}
result.WriteString(token)
return true
}))
_, err := lgp.model.Predict(prompt, predictOpts...)
if err != nil {
return "", fmt.Errorf("prediction failed: %w", err)
}
return strings.TrimSpace(result.String()), nil
}
func (lgp *LocalGGUFProvider) StreamGenerate(ctx context.Context, prompt string, opts GenerateOptions) (<-chan string, <-chan error) {
resultChan := make(chan string, 1)
errChan := make(chan error, 1)
go func() {
defer close(resultChan)
defer close(errChan)
result, err := lgp.Generate(ctx, prompt, opts)
if err != nil {
errChan <- err
return
}
resultChan <- result
}()
return resultChan, errChan
}
func (lgp *LocalGGUFProvider) Name() string {
return "local_gguf"
}
func (lgp *LocalGGUFProvider) Available() bool {
if lgp.modelPath == "" {
return false
}
if _, err := os.Stat(lgp.modelPath); os.IsNotExist(err) {
return false
}
if lgp.model == nil {
if err := lgp.loadModel(); err != nil {
return false
}
}
return lgp.model != nil
}
func (lgp *LocalGGUFProvider) MaxTokens() int {
return lgp.contextSize
}
func (lgp *LocalGGUFProvider) Close() error {
lgp.mu.Lock()
defer lgp.mu.Unlock()
if lgp.model != nil {
lgp.model.Free()
lgp.model = nil
fmt.Println("✓ Local GGUF model resources released")
}
return nil
}
func (lgp *LocalGGUFProvider) GetEmbeddings(text string) ([]float32, error) {
lgp.mu.Lock()
defer lgp.mu.Unlock()
if lgp.model == nil {
return nil, fmt.Errorf("model not loaded")
}
embeddings, err := lgp.model.Embeddings(text)
if err != nil {
return nil, fmt.Errorf("failed to generate embeddings: %w", err)
}
return embeddings, nil
}