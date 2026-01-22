package llm
import (
	"context"
	"fmt"
)
type LocalGGUFProvider struct {
	modelPath string
}
func NewLocalGGUFProvider(modelPath string) *LocalGGUFProvider {
	return &LocalGGUFProvider{
		modelPath: modelPath,
	}
}
func (lgp *LocalGGUFProvider) Generate(ctx context.Context, prompt string, opts GenerateOptions) (string, error) {
	return "", fmt.Errorf("local GGUF support not built (rebuild without -tags nollama)")
}
func (lgp *LocalGGUFProvider) StreamGenerate(ctx context.Context, prompt string, opts GenerateOptions) (<-chan string, <-chan error) {
	resultChan := make(chan string)
	errChan := make(chan error, 1)
	close(resultChan)
	errChan <- fmt.Errorf("local GGUF support not built (rebuild without -tags nollama)")
	close(errChan)
	return resultChan, errChan
}
func (lgp *LocalGGUFProvider) Name() string {
	return "local_gguf_stub"
}
func (lgp *LocalGGUFProvider) Available() bool {
	return false
}
func (lgp *LocalGGUFProvider) MaxTokens() int {
	return 0
}
func (lgp *LocalGGUFProvider) Close() error {
	return nil
}