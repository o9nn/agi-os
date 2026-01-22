package llm
import (
	"context"
	"fmt"
	"os"
	"sync"
	"time"
)
type MultiProviderLLM struct {
	providers []Provider
	mu        sync.RWMutex
	stats     map[string]*ProviderStats
}
type ProviderStats struct {
	TotalCalls    int64
	SuccessCalls  int64
	FailedCalls   int64
	TotalLatency  time.Duration
	LastUsed      time.Time
	Available     bool
}
type Provider interface {
	Generate(ctx context.Context, prompt string, opts GenerateOptions) (string, error)
	Name() string
	Available() bool
	MaxTokens() int
}
func NewMultiProviderLLM() *MultiProviderLLM {
	mp := &MultiProviderLLM{
		providers: make([]Provider, 0),
		stats:     make(map[string]*ProviderStats),
	}
	mp.initializeProviders()
	return mp
}
func (mp *MultiProviderLLM) initializeProviders() {
	if modelPath := os.Getenv("LOCAL_MODEL_PATH"); modelPath != "" {
		provider := NewLocalGGUFProvider(modelPath)
		if provider.Available() {
			mp.AddProvider(provider)
			fmt.Println("✓ Local GGUF model provider initialized")
		}
	}
	if apiKey := os.Getenv("ANTHROPIC_API_KEY"); apiKey != "" {
		provider := NewAnthropicProvider("claude-3-5-sonnet-20241022")
		mp.AddProvider(provider)
		fmt.Println("✓ Anthropic Claude provider initialized")
	}
	if apiKey := os.Getenv("OPENROUTER_API_KEY"); apiKey != "" {
		provider := NewOpenRouterProvider("anthropic/claude-3.5-sonnet")
		mp.AddProvider(provider)
		fmt.Println("✓ OpenRouter provider initialized")
	}
	if apiKey := os.Getenv("OPENAI_API_KEY"); apiKey != "" {
		provider := NewOpenAIProvider("gpt-4")
		mp.AddProvider(provider)
		fmt.Println("✓ OpenAI provider initialized")
	}
	if len(mp.providers) == 0 {
		provider := &SimpleFallbackProvider{}
		mp.AddProvider(provider)
		fmt.Println("⚠ Using simple fallback provider (no API keys found)")
	}
}
func (mp *MultiProviderLLM) AddProvider(provider Provider) {
	mp.mu.Lock()
	defer mp.mu.Unlock()
	mp.providers = append(mp.providers, provider)
	mp.stats[provider.Name()] = &ProviderStats{
		Available: provider.Available(),
		LastUsed:  time.Now(),
	}
}
func (mp *MultiProviderLLM) Generate(ctx context.Context, prompt string, opts GenerateOptions) (string, error) {
	mp.mu.RLock()
	providers := make([]Provider, len(mp.providers))
	copy(providers, mp.providers)
	mp.mu.RUnlock()
	var lastErr error
	for _, provider := range providers {
		if !provider.Available() {
			continue
		}
		start := time.Now()
		result, err := provider.Generate(ctx, prompt, opts)
		latency := time.Since(start)
		mp.updateStats(provider.Name(), err == nil, latency)
		if err == nil {
			return result, nil
		}
		lastErr = err
	}
	if lastErr != nil {
		return "", fmt.Errorf("all providers failed, last error: %w", lastErr)
	}
	return "", fmt.Errorf("no available providers")
}
func (mp *MultiProviderLLM) StreamGenerate(ctx context.Context, prompt string, opts GenerateOptions) (<-chan StreamChunk, error) {
	mp.mu.RLock()
	providers := make([]Provider, len(mp.providers))
	copy(providers, mp.providers)
	mp.mu.RUnlock()
	var lastErr error
	for _, provider := range providers {
		if !provider.Available() {
			continue
		}
		if streamer, ok := provider.(interface {
			StreamGenerate(context.Context, string, GenerateOptions) (<-chan StreamChunk, error)
		}); ok {
			ch, err := streamer.StreamGenerate(ctx, prompt, opts)
			if err == nil {
				return ch, nil
			}
			lastErr = err
		} else {
			ch := make(chan StreamChunk, 1)
			go func() {
				defer close(ch)
				result, err := provider.Generate(ctx, prompt, opts)
				if err != nil {
					ch <- StreamChunk{Error: err, Done: true}
				} else {
					ch <- StreamChunk{Content: result, Done: true}
				}
			}()
			return ch, nil
		}
	}
	if lastErr != nil {
		return nil, fmt.Errorf("all providers failed, last error: %w", lastErr)
	}
	return nil, fmt.Errorf("no available providers")
}
func (mp *MultiProviderLLM) updateStats(providerName string, success bool, latency time.Duration) {
	mp.mu.Lock()
	defer mp.mu.Unlock()
	stats, exists := mp.stats[providerName]
	if !exists {
		stats = &ProviderStats{Available: true}
		mp.stats[providerName] = stats
	}
	stats.TotalCalls++
	stats.LastUsed = time.Now()
	stats.TotalLatency += latency
	if success {
		stats.SuccessCalls++
		stats.Available = true
	} else {
		stats.FailedCalls++
		if stats.FailedCalls > 3 && float64(stats.FailedCalls)/float64(stats.TotalCalls) > 0.5 {
			stats.Available = false
		}
	}
}
func (mp *MultiProviderLLM) GetStats() map[string]*ProviderStats {
	mp.mu.RLock()
	defer mp.mu.RUnlock()
	statsCopy := make(map[string]*ProviderStats)
	for k, v := range mp.stats {
		statsCopy[k] = &ProviderStats{
			TotalCalls:   v.TotalCalls,
			SuccessCalls: v.SuccessCalls,
			FailedCalls:  v.FailedCalls,
			TotalLatency: v.TotalLatency,
			LastUsed:     v.LastUsed,
			Available:    v.Available,
		}
	}
	return statsCopy
}
func (mp *MultiProviderLLM) Name() string {
	return "MultiProvider"
}
func (mp *MultiProviderLLM) Available() bool {
	mp.mu.RLock()
	defer mp.mu.RUnlock()
	for _, provider := range mp.providers {
		if provider.Available() {
			return true
		}
	}
	return false
}
func (mp *MultiProviderLLM) MaxTokens() int {
	mp.mu.RLock()
	defer mp.mu.RUnlock()
	maxTokens := 0
	for _, provider := range mp.providers {
		if provider.Available() && provider.MaxTokens() > maxTokens {
			maxTokens = provider.MaxTokens()
		}
	}
	if maxTokens == 0 {
		return 4096 
	}
	return maxTokens
}