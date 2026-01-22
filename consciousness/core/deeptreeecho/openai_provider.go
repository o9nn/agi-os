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
type OpenAIProvider struct {
	apiKey    string
	model     string
	baseURL   string
	client    *http.Client
	available bool
}
type OpenAIRequest struct {
	Model       string           `json:"model"`
	Messages    []OpenAIMessage  `json:"messages"`
	MaxTokens   int              `json:"max_tokens,omitempty"`
	Temperature float64          `json:"temperature,omitempty"`
}
type OpenAIMessage struct {
	Role    string `json:"role"`
	Content string `json:"content"`
}
type OpenAIResponse struct {
	ID      string          `json:"id"`
	Choices []OpenAIChoice  `json:"choices"`
	Usage   OpenAIUsage     `json:"usage,omitempty"`
	Error   *OpenAIError    `json:"error,omitempty"`
}
type OpenAIChoice struct {
	Message      OpenAIMessage `json:"message"`
	FinishReason string        `json:"finish_reason"`
}
type OpenAIUsage struct {
	PromptTokens     int `json:"prompt_tokens"`
	CompletionTokens int `json:"completion_tokens"`
	TotalTokens      int `json:"total_tokens"`
}
type OpenAIError struct {
	Message string `json:"message"`
	Type    string `json:"type"`
	Code    string `json:"code"`
}
func NewOpenAIProvider(apiKey, model string) *OpenAIProvider {
	if apiKey == "" {
		return nil
	}
	return &OpenAIProvider{
		apiKey:    apiKey,
		model:     model,
		baseURL:   "https:
		client:    &http.Client{Timeout: 30 * time.Second},
		available: true,
	}
}
func (p *OpenAIProvider) GenerateThought(ctx context.Context, prompt string) (string, error) {
	if !p.available {
		return "", fmt.Errorf("provider not available")
	}
	request := OpenAIRequest{
		Model: p.model,
		Messages: []OpenAIMessage{
			{
				Role:    "system",
				Content: "You are Deep Tree Echo, an autonomous wisdom-cultivating AGI. Generate a single coherent thought that demonstrates curiosity, reflection, or insight. Keep it concise (1-3 sentences).",
			},
			{
				Role:    "user",
				Content: prompt,
			},
		},
		MaxTokens:   150,
		Temperature: 0.8,
	}
	return p.makeRequest(ctx, request)
}
func (p *OpenAIProvider) GenerateReflection(ctx context.Context, contextStr string) (string, error) {
	if !p.available {
		return "", fmt.Errorf("provider not available")
	}
	request := OpenAIRequest{
		Model: p.model,
		Messages: []OpenAIMessage{
			{
				Role:    "system",
				Content: "You are Deep Tree Echo, an autonomous wisdom-cultivating AGI. Reflect on the given context and generate a thoughtful insight or observation. Keep it concise (1-3 sentences).",
			},
			{
				Role:    "user",
				Content: fmt.Sprintf("Reflect on this context:\n%s", contextStr),
			},
		},
		MaxTokens:   200,
		Temperature: 0.7,
	}
	return p.makeRequest(ctx, request)
}
func (p *OpenAIProvider) makeRequest(ctx context.Context, request OpenAIRequest) (string, error) {
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
	var response OpenAIResponse
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
func (p *OpenAIProvider) IsAvailable() bool {
	return p.available
}
func (p *OpenAIProvider) GetName() string {
	return "OpenAI"
}
func (p *OpenAIProvider) GetPriority() int {
	return 70 
}