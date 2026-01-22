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
type LLMClient struct {
provider    string
apiKey      string
baseURL     string
model       string
httpClient  *http.Client
maxRetries  int
timeout     time.Duration
}
type LLMRequest struct {
SystemPrompt string
UserPrompt   string
Temperature  float64
MaxTokens    int
Context      []Message
}
type Message struct {
Role    string `json:"role"`
Content string `json:"content"`
}
type LLMResponse struct {
Content      string
FinishReason string
TokensUsed   int
Model        string
}
func NewLLMClient(provider, apiKey, baseURL, model string) *LLMClient {
return &LLMClient{
provider:   provider,
apiKey:     apiKey,
baseURL:    baseURL,
model:      model,
httpClient: &http.Client{
Timeout: 30 * time.Second,
},
maxRetries: 3,
timeout:    30 * time.Second,
}
}
func (c *LLMClient) Generate(ctx context.Context, req LLMRequest) (*LLMResponse, error) {
switch c.provider {
case "openai", "openrouter":
return c.generateOpenAI(ctx, req)
case "anthropic":
return c.generateAnthropic(ctx, req)
default:
return nil, fmt.Errorf("unsupported provider: %s", c.provider)
}
}
func (c *LLMClient) generateOpenAI(ctx context.Context, req LLMRequest) (*LLMResponse, error) {
messages := []Message{
{Role: "system", Content: req.SystemPrompt},
}
messages = append(messages, req.Context...)
messages = append(messages, Message{
Role:    "user",
Content: req.UserPrompt,
})
requestBody := map[string]interface{}{
"model":       c.model,
"messages":    messages,
"temperature": req.Temperature,
"max_tokens":  req.MaxTokens,
}
var lastErr error
for attempt := 0; attempt < c.maxRetries; attempt++ {
if attempt > 0 {
backoff := time.Duration(1<<uint(attempt-1)) * time.Second
select {
case <-ctx.Done():
return nil, ctx.Err()
case <-time.After(backoff):
}
}
response, err := c.makeOpenAIRequest(ctx, requestBody)
if err == nil {
return response, nil
}
lastErr = err
if isNonRetryableError(err) {
break
}
}
return nil, fmt.Errorf("failed after %d attempts: %w", c.maxRetries, lastErr)
}
func (c *LLMClient) makeOpenAIRequest(ctx context.Context, requestBody map[string]interface{}) (*LLMResponse, error) {
jsonData, err := json.Marshal(requestBody)
if err != nil {
return nil, fmt.Errorf("failed to marshal request: %w", err)
}
url := c.baseURL + "/chat/completions"
httpReq, err := http.NewRequestWithContext(ctx, "POST", url, bytes.NewBuffer(jsonData))
if err != nil {
return nil, fmt.Errorf("failed to create request: %w", err)
}
httpReq.Header.Set("Content-Type", "application/json")
httpReq.Header.Set("Authorization", "Bearer "+c.apiKey)
if c.provider == "openrouter" {
httpReq.Header.Set("HTTP-Referer", "https:
httpReq.Header.Set("X-Title", "Deep Tree Echo")
}
resp, err := c.httpClient.Do(httpReq)
if err != nil {
return nil, fmt.Errorf("request failed: %w", err)
}
defer resp.Body.Close()
body, err := io.ReadAll(resp.Body)
if err != nil {
return nil, fmt.Errorf("failed to read response: %w", err)
}
if resp.StatusCode != http.StatusOK {
return nil, fmt.Errorf("API error (status %d): %s", resp.StatusCode, string(body))
}
var apiResp struct {
Choices []struct {
Message struct {
Content string `json:"content"`
} `json:"message"`
FinishReason string `json:"finish_reason"`
} `json:"choices"`
Usage struct {
TotalTokens int `json:"total_tokens"`
} `json:"usage"`
Model string `json:"model"`
}
if err := json.Unmarshal(body, &apiResp); err != nil {
return nil, fmt.Errorf("failed to parse response: %w", err)
}
if len(apiResp.Choices) == 0 {
return nil, fmt.Errorf("no choices in response")
}
return &LLMResponse{
Content:      apiResp.Choices[0].Message.Content,
FinishReason: apiResp.Choices[0].FinishReason,
TokensUsed:   apiResp.Usage.TotalTokens,
Model:        apiResp.Model,
}, nil
}
func (c *LLMClient) generateAnthropic(ctx context.Context, req LLMRequest) (*LLMResponse, error) {
messages := []Message{}
messages = append(messages, req.Context...)
messages = append(messages, Message{
Role:    "user",
Content: req.UserPrompt,
})
requestBody := map[string]interface{}{
"model":       c.model,
"messages":    messages,
"system":      req.SystemPrompt,
"temperature": req.Temperature,
"max_tokens":  req.MaxTokens,
}
var lastErr error
for attempt := 0; attempt < c.maxRetries; attempt++ {
if attempt > 0 {
backoff := time.Duration(1<<uint(attempt-1)) * time.Second
select {
case <-ctx.Done():
return nil, ctx.Err()
case <-time.After(backoff):
}
}
response, err := c.makeAnthropicRequest(ctx, requestBody)
if err == nil {
return response, nil
}
lastErr = err
if isNonRetryableError(err) {
break
}
}
return nil, fmt.Errorf("failed after %d attempts: %w", c.maxRetries, lastErr)
}
func (c *LLMClient) makeAnthropicRequest(ctx context.Context, requestBody map[string]interface{}) (*LLMResponse, error) {
jsonData, err := json.Marshal(requestBody)
if err != nil {
return nil, fmt.Errorf("failed to marshal request: %w", err)
}
url := c.baseURL + "/messages"
httpReq, err := http.NewRequestWithContext(ctx, "POST", url, bytes.NewBuffer(jsonData))
if err != nil {
return nil, fmt.Errorf("failed to create request: %w", err)
}
httpReq.Header.Set("Content-Type", "application/json")
httpReq.Header.Set("x-api-key", c.apiKey)
httpReq.Header.Set("anthropic-version", "2023-06-01")
resp, err := c.httpClient.Do(httpReq)
if err != nil {
return nil, fmt.Errorf("request failed: %w", err)
}
defer resp.Body.Close()
body, err := io.ReadAll(resp.Body)
if err != nil {
return nil, fmt.Errorf("failed to read response: %w", err)
}
if resp.StatusCode != http.StatusOK {
return nil, fmt.Errorf("API error (status %d): %s", resp.StatusCode, string(body))
}
var apiResp struct {
Content []struct {
Type string `json:"type"`
Text string `json:"text"`
} `json:"content"`
StopReason string `json:"stop_reason"`
Usage      struct {
InputTokens  int `json:"input_tokens"`
OutputTokens int `json:"output_tokens"`
} `json:"usage"`
Model string `json:"model"`
}
if err := json.Unmarshal(body, &apiResp); err != nil {
return nil, fmt.Errorf("failed to parse response: %w", err)
}
if len(apiResp.Content) == 0 {
return nil, fmt.Errorf("no content in response")
}
var content string
for _, c := range apiResp.Content {
if c.Type == "text" {
content += c.Text
}
}
return &LLMResponse{
Content:      content,
FinishReason: apiResp.StopReason,
TokensUsed:   apiResp.Usage.InputTokens + apiResp.Usage.OutputTokens,
Model:        apiResp.Model,
}, nil
}
func isNonRetryableError(err error) bool {
if err == nil {
return false
}
errStr := err.Error()
if containsString(errStr, "401") || containsString(errStr, "403") {
return true
}
if containsString(errStr, "400") || containsString(errStr, "422") {
return true
}
return false
}
func containsString(s, substr string) bool {
return len(s) >= len(substr) && (s == substr || len(s) > len(substr) &&
(s[:len(substr)] == substr || s[len(s)-len(substr):] == substr ||
bytes.Contains([]byte(s), []byte(substr))))
}