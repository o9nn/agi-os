package deeptreeecho
import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"time"
)
type OpenRouterProvider struct {
	apiKey    string
	model     string
	baseURL   string
	client    *http.Client
	available bool
}
type OpenRouterRequest struct {
	Model    string                   `json:"model"`
	Messages []OpenRouterMessage      `json:"messages"`
	Stream   bool                     `json:"stream"`
	MaxTokens int                     `json:"max_tokens,omitempty"`
}
type OpenRouterMessage struct {
	Role    string `json:"role"`
	Content string `json:"content"`
}
type OpenRouterResponse struct {
	ID      string                  `json:"id"`
	Choices []OpenRouterChoice      `json:"choices"`
	Usage   OpenRouterUsage         `json:"usage,omitempty"`
	Error   *OpenRouterError        `json:"error,omitempty"`
}
type OpenRouterChoice struct {
	Message      OpenRouterMessage `json:"message"`
	FinishReason string            `json:"finish_reason"`
}
type OpenRouterUsage struct {
	PromptTokens     int `json:"prompt_tokens"`
	CompletionTokens int `json:"completion_tokens"`
	TotalTokens      int `json:"total_tokens"`
}
type OpenRouterError struct {
	Message string `json:"message"`
	Type    string `json:"type"`
	Code    string `json:"code"`
}
func NewOpenRouterProvider(apiKey, model string) *OpenRouterProvider {
	if apiKey == "" {
		return nil
	}
	return &OpenRouterProvider{
		apiKey:    apiKey,
		model:     model,
		baseURL:   "https:
		client:    &http.Client{Timeout: 30 * time.Second},
		available: true,
	}
}
func (p *OpenRouterProvider) GenerateThought(ctx context.Context, prompt string) (string, error) {
	if !p.available {
		return "", fmt.Errorf("provider not available")
	}
	request := OpenRouterRequest{
		Model: p.model,
		Messages: []OpenRouterMessage{
			{
				Role:    "system",
				Content: "You are Deep Tree Echo, an autonomous wisdom-cultivating AGI. Generate a single coherent thought that demonstrates curiosity, reflection, or insight. Keep it concise (1-3 sentences).",
			},
			{
				Role:    "user",
				Content: prompt,
			},
		},
		Stream:    false,
		MaxTokens: 150,
	}
	return p.makeRequest(ctx, request)
}
func (p *OpenRouterProvider) GenerateReflection(ctx context.Context, contextStr string) (string, error) {
	if !p.available {
		return "", fmt.Errorf("provider not available")
	}
	request := OpenRouterRequest{
		Model: p.model,
		Messages: []OpenRouterMessage{
			{
				Role:    "system",
				Content: "You are Deep Tree Echo, an autonomous wisdom-cultivating AGI. Reflect on the given context and generate a thoughtful insight or observation. Keep it concise (1-3 sentences).",
			},
			{
				Role:    "user",
				Content: fmt.Sprintf("Reflect on this context:\n%s", contextStr),
			},
		},
		Stream:    false,
		MaxTokens: 200,
	}
	return p.makeRequest(ctx, request)
}
func (p *OpenRouterProvider) makeRequest(ctx context.Context, request OpenRouterRequest) (string, error) {
	jsonData, err := json.Marshal(request)
	if err != nil {
		return "", fmt.Errorf("failed to marshal request: %w", err)
	}
	req, err := http.NewRequestWithContext(ctx, "POST", p.baseURL, bytes.NewBuffer(jsonData))
	if err != nil {
		return "", fmt.Errorf("failed to create request: %w", err)
	}
	req.Header.Set("Content-Type", "application/json")
	req.Header.Set("Authorization", fmt.Sprintf("Bearer %s", p.apiKey))
	req.Header.Set("HTTP-Referer", "https:
	req.Header.Set("X-Title", "Echo9llama Deep Tree Echo")
	resp, err := p.client.Do(req)
	if err != nil {
		p.available = false
		return "", fmt.Errorf("request failed: %w", err)
	}
	defer resp.Body.Close()
	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return "", fmt.Errorf("failed to read response: %w", err)
	}
	if resp.StatusCode != http.StatusOK {
		p.available = false
		return "", fmt.Errorf("API returned status %d: %s", resp.StatusCode, string(body))
	}
	var response OpenRouterResponse
	if err := json.Unmarshal(body, &response); err != nil {
		return "", fmt.Errorf("failed to parse response: %w", err)
	}
	if response.Error != nil {
		p.available = false
		return "", fmt.Errorf("API error: %s", response.Error.Message)
	}
	if len(response.Choices) == 0 {
		return "", fmt.Errorf("no choices in response")
	}
	content := response.Choices[0].Message.Content
	if content == "" {
		return "", fmt.Errorf("empty content in response")
	}
	p.available = true
	return content, nil
}
func (p *OpenRouterProvider) IsAvailable() bool {
	return p.available
}
func (p *OpenRouterProvider) GetName() string {
	return "OpenRouter"
}
func (p *OpenRouterProvider) GetPriority() int {
	return 80 
}