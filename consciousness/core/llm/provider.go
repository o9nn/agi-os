package llm
import (
	"context"
	"errors"
	"fmt"
	"sync"
	"time"
)
type LLMProvider interface {
	Generate(ctx context.Context, prompt string, opts GenerateOptions) (string, error)
	StreamGenerate(ctx context.Context, prompt string, opts GenerateOptions) (<-chan StreamChunk, error)
	Name() string
	Available() bool
	MaxTokens() int
}
type GenerateOptions struct {
	MaxTokens   int
	Temperature float64
	TopP        float64
	Stop        []string
	SystemPrompt string
}
func DefaultGenerateOptions() GenerateOptions {
	return GenerateOptions{
		MaxTokens:   500,
		Temperature: 0.7,
		TopP:        0.9,
		Stop:        []string{},
	}
}
type StreamChunk struct {
	Content string
	Done    bool
	Error   error
}
type ProviderManager struct {
	mu            sync.RWMutex
	providers     map[string]LLMProvider
	fallbackChain []string
	defaultProvider string
	requestCount  map[string]uint64
	errorCount    map[string]uint64
	totalLatency  map[string]time.Duration
}
func NewProviderManager() *ProviderManager {
	return &ProviderManager{
		providers:     make(map[string]LLMProvider),
		fallbackChain: []string{},
		requestCount:  make(map[string]uint64),
		errorCount:    make(map[string]uint64),
		totalLatency:  make(map[string]time.Duration),
	}
}
func (pm *ProviderManager) RegisterProvider(provider LLMProvider) error {
	pm.mu.Lock()
	defer pm.mu.Unlock()
	name := provider.Name()
	if _, exists := pm.providers[name]; exists {
		return fmt.Errorf("provider %s already registered", name)
	}
	pm.providers[name] = provider
	if pm.defaultProvider == "" && provider.Available() {
		pm.defaultProvider = name
	}
	return nil
}
func (pm *ProviderManager) SetFallbackChain(chain []string) error {
	pm.mu.Lock()
	defer pm.mu.Unlock()
	for _, name := range chain {
		if _, exists := pm.providers[name]; !exists {
			return fmt.Errorf("provider %s not registered", name)
		}
	}
	pm.fallbackChain = chain
	for _, name := range chain {
		if pm.providers[name].Available() {
			pm.defaultProvider = name
			break
		}
	}
	return nil
}
func (pm *ProviderManager) Generate(ctx context.Context, prompt string, opts GenerateOptions) (string, error) {
	return pm.GenerateWithProvider(ctx, "", prompt, opts)
}
func (pm *ProviderManager) GenerateWithProvider(ctx context.Context, providerName string, prompt string, opts GenerateOptions) (string, error) {
	pm.mu.RLock()
	providersToTry := []string{}
	if providerName != "" {
		providersToTry = append(providersToTry, providerName)
	} else if pm.defaultProvider != "" {
		providersToTry = append(providersToTry, pm.defaultProvider)
	}
	for _, name := range pm.fallbackChain {
		if name != providerName && name != pm.defaultProvider {
			providersToTry = append(providersToTry, name)
		}
	}
	pm.mu.RUnlock()
	if len(providersToTry) == 0 {
		return "", errors.New("no LLM providers available")
	}
	var lastErr error
	for _, name := range providersToTry {
		pm.mu.RLock()
		provider, exists := pm.providers[name]
		pm.mu.RUnlock()
		if !exists || !provider.Available() {
			continue
		}
		start := time.Now()
		result, err := provider.Generate(ctx, prompt, opts)
		latency := time.Since(start)
		pm.mu.Lock()
		pm.requestCount[name]++
		pm.totalLatency[name] += latency
		if err != nil {
			pm.errorCount[name]++
		}
		pm.mu.Unlock()
		if err == nil {
			return result, nil
		}
		lastErr = err
	}
	if lastErr != nil {
		return "", fmt.Errorf("all providers failed, last error: %w", lastErr)
	}
	return "", errors.New("no available providers")
}
func (pm *ProviderManager) StreamGenerate(ctx context.Context, prompt string, opts GenerateOptions) (<-chan StreamChunk, error) {
	return pm.StreamGenerateWithProvider(ctx, "", prompt, opts)
}
func (pm *ProviderManager) StreamGenerateWithProvider(ctx context.Context, providerName string, prompt string, opts GenerateOptions) (<-chan StreamChunk, error) {
	pm.mu.RLock()
	defer pm.mu.RUnlock()
	targetProvider := providerName
	if targetProvider == "" {
		targetProvider = pm.defaultProvider
	}
	if targetProvider == "" {
		outChan := make(chan StreamChunk, 1)
		outChan <- StreamChunk{Error: errors.New("no LLM providers available")}
		close(outChan)
		return outChan, errors.New("no LLM providers available")
	}
	provider, exists := pm.providers[targetProvider]
	if !exists || !provider.Available() {
		outChan := make(chan StreamChunk, 1)
		outChan <- StreamChunk{Error: fmt.Errorf("provider %s not available", targetProvider)}
		close(outChan)
		return outChan, fmt.Errorf("provider %s not available", targetProvider)
	}
	return provider.StreamGenerate(ctx, prompt, opts)
}
func (pm *ProviderManager) GetProvider(name string) (LLMProvider, error) {
	pm.mu.RLock()
	defer pm.mu.RUnlock()
	provider, exists := pm.providers[name]
	if !exists {
		return nil, fmt.Errorf("provider %s not found", name)
	}
	return provider, nil
}
func (pm *ProviderManager) ListProviders() []string {
	pm.mu.RLock()
	defer pm.mu.RUnlock()
	names := make([]string, 0, len(pm.providers))
	for name := range pm.providers {
		names = append(names, name)
	}
	return names
}
func (pm *ProviderManager) GetMetrics() map[string]ProviderMetrics {
	pm.mu.RLock()
	defer pm.mu.RUnlock()
	metrics := make(map[string]ProviderMetrics)
	for name := range pm.providers {
		requests := pm.requestCount[name]
		errors := pm.errorCount[name]
		totalLatency := pm.totalLatency[name]
		var avgLatency time.Duration
		if requests > 0 {
			avgLatency = totalLatency / time.Duration(requests)
		}
		var errorRate float64
		if requests > 0 {
			errorRate = float64(errors) / float64(requests)
		}
		metrics[name] = ProviderMetrics{
			RequestCount:   requests,
			ErrorCount:     errors,
			ErrorRate:      errorRate,
			AverageLatency: avgLatency,
		}
	}
	return metrics
}
type ProviderMetrics struct {
	RequestCount   uint64
	ErrorCount     uint64
	ErrorRate      float64
	AverageLatency time.Duration
}
func (pm *ProviderManager) Name() string {
	return "ProviderManager"
}
func (pm *ProviderManager) Available() bool {
	pm.mu.RLock()
	defer pm.mu.RUnlock()
	for _, provider := range pm.providers {
		if provider.Available() {
			return true
		}
	}
	return false
}
func (pm *ProviderManager) MaxTokens() int {
	pm.mu.RLock()
	defer pm.mu.RUnlock()
	if pm.defaultProvider != "" {
		if provider, exists := pm.providers[pm.defaultProvider]; exists {
			return provider.MaxTokens()
		}
	}
	return 4096
}