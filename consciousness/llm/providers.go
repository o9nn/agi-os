package llm
import (
"bytes"
"context"
"encoding/json"
"fmt"
"io"
"net/http"
"time"
)
type Provider interface {
Generate(ctx context.Context, prompt string, options GenerateOptions) (string, error)
GetName() string
}
type GenerateOptions struct {
Temperature float64
MaxTokens   int
SystemPrompt string
}
type AnthropicProvider struct {
apiKey     string
httpClient *http.Client
model      string
}
type OpenRouterProvider struct {
apiKey     string
httpClient *http.Client
model      string
}
func NewAnthropicProvider(apiKey string) (*AnthropicProvider, error) {
if apiKey == "" {
return nil, fmt.Errorf("Anthropic API key is required")
}
return &AnthropicProvider{
apiKey: apiKey,
httpClient: &http.Client{
Timeout: 60 * time.Second,
},
model: "claude-3-5-sonnet-20241022",
}, nil
}
func NewOpenRouterProvider(apiKey string) (*OpenRouterProvider, error) {
if apiKey == "" {
return nil, fmt.Errorf("OpenRouter API key is required")
}
return &OpenRouterProvider{
apiKey: apiKey,
httpClient: &http.Client{
Timeout: 60 * time.Second,
},
model: "anthropic/claude-3.5-sonnet",
}, nil
}
func (ap *AnthropicProvider) Generate(ctx context.Context, prompt string, options GenerateOptions) (string, error) {
requestBody := map[string]interface{}{
"model": ap.model,
"max_tokens": options.MaxTokens,
"temperature": options.Temperature,
"messages": []map[string]string{
{
"role": "user",
"content": prompt,
},
},
}
if options.SystemPrompt != "" {
requestBody["system"] = options.SystemPrompt
}
jsonData, err := json.Marshal(requestBody)
if err != nil {
return "", fmt.Errorf("failed to marshal request: %w", err)
}
req, err := http.NewRequestWithContext(ctx, "POST", "https:
if err != nil {
return "", fmt.Errorf("failed to create request: %w", err)
}
req.Header.Set("Content-Type", "application/json")
req.Header.Set("x-api-key", ap.apiKey)
req.Header.Set("anthropic-version", "2023-06-01")
resp, err := ap.httpClient.Do(req)
if err != nil {
return "", fmt.Errorf("failed to send request: %w", err)
}
defer resp.Body.Close()
body, err := io.ReadAll(resp.Body)
if err != nil {
return "", fmt.Errorf("failed to read response: %w", err)
}
if resp.StatusCode != http.StatusOK {
return "", fmt.Errorf("API error (status %d): %s", resp.StatusCode, string(body))
}
var response struct {
Content []struct {
Type string `json:"type"`
Text string `json:"text"`
} `json:"content"`
}
if err := json.Unmarshal(body, &response); err != nil {
return "", fmt.Errorf("failed to parse response: %w", err)
}
if len(response.Content) == 0 {
return "", fmt.Errorf("no content in response")
}
return response.Content[0].Text, nil
}
func (ap *AnthropicProvider) GetName() string {
return "Anthropic Claude"
}
func (orp *OpenRouterProvider) Generate(ctx context.Context, prompt string, options GenerateOptions) (string, error) {
messages := []map[string]string{
{
"role": "user",
"content": prompt,
},
}
if options.SystemPrompt != "" {
messages = append([]map[string]string{
{
"role": "system",
"content": options.SystemPrompt,
},
}, messages...)
}
requestBody := map[string]interface{}{
"model": orp.model,
"max_tokens": options.MaxTokens,
"temperature": options.Temperature,
"messages": messages,
}
jsonData, err := json.Marshal(requestBody)
if err != nil {
return "", fmt.Errorf("failed to marshal request: %w", err)
}
req, err := http.NewRequestWithContext(ctx, "POST", "https:
if err != nil {
return "", fmt.Errorf("failed to create request: %w", err)
}
req.Header.Set("Content-Type", "application/json")
req.Header.Set("Authorization", "Bearer "+orp.apiKey)
req.Header.Set("HTTP-Referer", "https:
req.Header.Set("X-Title", "Echo9llama Autonomous Agent")
resp, err := orp.httpClient.Do(req)
if err != nil {
return "", fmt.Errorf("failed to send request: %w", err)
}
defer resp.Body.Close()
body, err := io.ReadAll(resp.Body)
if err != nil {
return "", fmt.Errorf("failed to read response: %w", err)
}
if resp.StatusCode != http.StatusOK {
return "", fmt.Errorf("API error (status %d): %s", resp.StatusCode, string(body))
}
var response struct {
Choices []struct {
Message struct {
Content string `json:"content"`
} `json:"message"`
} `json:"choices"`
}
if err := json.Unmarshal(body, &response); err != nil {
return "", fmt.Errorf("failed to parse response: %w", err)
}
if len(response.Choices) == 0 {
return "", fmt.Errorf("no choices in response")
}
return response.Choices[0].Message.Content, nil
}
func (orp *OpenRouterProvider) GetName() string {
return "OpenRouter"
}
func DefaultGenerateOptions() GenerateOptions {
return GenerateOptions{
Temperature: 0.7,
MaxTokens:   1024,
SystemPrompt: "",
}
}