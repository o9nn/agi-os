//go:build nollama
// +build nollama

package llm

import (
	"context"
	"fmt"
)

// LocalGGUFProvider stub when llama.cpp is not built
type LocalGGUFProvider struct {
	modelPath string
}

// NewLocalGGUFProvider creates a stub provider
func NewLocalGGUFProvider(modelPath string) *LocalGGUFProvider {
	return &LocalGGUFProvider{
		modelPath: modelPath,
	}
}

// Generate returns an error indicating llama.cpp support is not built
func (lgp *LocalGGUFProvider) Generate(ctx context.Context, prompt string, opts GenerateOptions) (string, error) {
	return "", fmt.Errorf("local GGUF support not built (rebuild without -tags nollama)")
}

// StreamGenerate returns an error
func (lgp *LocalGGUFProvider) StreamGenerate(ctx context.Context, prompt string, opts GenerateOptions) (<-chan string, <-chan error) {
	resultChan := make(chan string)
	errChan := make(chan error, 1)
	close(resultChan)
	errChan <- fmt.Errorf("local GGUF support not built (rebuild without -tags nollama)")
	close(errChan)
	return resultChan, errChan
}

// Name returns the provider name
func (lgp *LocalGGUFProvider) Name() string {
	return "local_gguf_stub"
}

// Available always returns false for stub
func (lgp *LocalGGUFProvider) Available() bool {
	return false
}

// MaxTokens returns 0 for stub
func (lgp *LocalGGUFProvider) MaxTokens() int {
	return 0
}

// Close does nothing for stub
func (lgp *LocalGGUFProvider) Close() error {
	return nil
}
