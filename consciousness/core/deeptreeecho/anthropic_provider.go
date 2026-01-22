package deeptreeecho
import (
"bytes"
"context"
"encoding/json"
"fmt"
"io"
"net/http"
"os"
"strings"
"time"
)
type AnthropicProvider struct {
apiKey     string
model      string
httpClient *http.Client
baseURL    string
available  bool
}
type AnthropicRequest struct {
Model       string             `json:"model"`
MaxTokens   int                `json:"max_tokens"`
Temperature float64            `json:"temperature,omitempty"`
System      string             `json:"system,omitempty"`
Messages    []AnthropicMessage `json:"messages"`
}
type AnthropicMessage struct {
Role    string `json:"role"`
Content string `json:"content"`
}
type AnthropicResponse struct {
ID      string `json:"id"`
Type    string `json:"type"`
Role    string `json:"role"`
Content []struct {
Type string `json:"type"`
Text string `json:"text"`
} `json:"content"`
Model      string `json:"model"`
StopReason string `json:"stop_reason"`
Usage      struct {
InputTokens  int `json:"input_tokens"`
OutputTokens int `json:"output_tokens"`
} `json:"usage"`
}
func NewAnthropicProvider(apiKey string, model string) *AnthropicProvider {
if apiKey == "" {
apiKey = os.Getenv("ANTHROPIC_API_KEY")
}
if model == "" {
model = "claude-3-5-sonnet-20241022"
}
return &AnthropicProvider{
apiKey:  apiKey,
model:   model,
baseURL: "https:
httpClient: &http.Client{
Timeout: 60 * time.Second,
},
available: apiKey != "",
}
}
func (ap *AnthropicProvider) Generate(ctx context.Context, req LLMRequest) (*LLMResponse, error) {
messages := make([]AnthropicMessage, 0, len(req.Context)+1)
for _, msg := range req.Context {
messages = append(messages, AnthropicMessage{
Role:    msg.Role,
Content: msg.Content,
})
}
messages = append(messages, AnthropicMessage{
Role:    "user",
Content: req.UserPrompt,
})
messages = ap.manageContextWindow(messages, 180000)
maxTokens := req.MaxTokens
if maxTokens == 0 {
maxTokens = 1024
}
temperature := req.Temperature
if temperature == 0 {
temperature = 0.7
}
anthropicReq := AnthropicRequest{
Model:       ap.model,
MaxTokens:   maxTokens,
Temperature: temperature,
System:      req.SystemPrompt,
Messages:    messages,
}
response, err := ap.callAPI(ctx, anthropicReq)
if err != nil {
return nil, fmt.Errorf("claude api call failed: %w", err)
}
var content string
if len(response.Content) > 0 {
content = response.Content[0].Text
}
return &LLMResponse{
Content:      content,
Model:        response.Model,
TokensUsed:   response.Usage.InputTokens + response.Usage.OutputTokens,
FinishReason: response.StopReason,
}, nil
}
func (ap *AnthropicProvider) callAPI(ctx context.Context, req AnthropicRequest) (*AnthropicResponse, error) {
jsonData, err := json.Marshal(req)
if err != nil {
return nil, fmt.Errorf("failed to marshal request: %w", err)
}
httpReq, err := http.NewRequestWithContext(ctx, "POST", ap.baseURL, bytes.NewBuffer(jsonData))
if err != nil {
return nil, fmt.Errorf("failed to create request: %w", err)
}
httpReq.Header.Set("Content-Type", "application/json")
httpReq.Header.Set("x-api-key", ap.apiKey)
httpReq.Header.Set("anthropic-version", "2023-06-01")
httpResp, err := ap.httpClient.Do(httpReq)
if err != nil {
return nil, fmt.Errorf("http request failed: %w", err)
}
defer httpResp.Body.Close()
body, err := io.ReadAll(httpResp.Body)
if err != nil {
return nil, fmt.Errorf("failed to read response: %w", err)
}
if httpResp.StatusCode != http.StatusOK {
return nil, fmt.Errorf("api error (status %d): %s", httpResp.StatusCode, string(body))
}
var response AnthropicResponse
if err := json.Unmarshal(body, &response); err != nil {
return nil, fmt.Errorf("failed to parse response: %w", err)
}
return &response, nil
}
func (ap *AnthropicProvider) manageContextWindow(messages []AnthropicMessage, maxTokens int) []AnthropicMessage {
estimateTokens := func(text string) int {
return len(text) / 4
}
totalTokens := 0
for _, msg := range messages {
totalTokens += estimateTokens(msg.Content)
}
if totalTokens <= maxTokens {
return messages
}
if len(messages) <= 2 {
truncated := make([]AnthropicMessage, len(messages))
for i, msg := range messages {
targetLen := (len(msg.Content) * maxTokens) / totalTokens
if len(msg.Content) > targetLen {
truncated[i] = AnthropicMessage{
Role:    msg.Role,
Content: msg.Content[:targetLen] + "...[truncated]",
}
} else {
truncated[i] = msg
}
}
return truncated
}
result := []AnthropicMessage{messages[0]}
recentTokens := estimateTokens(messages[0].Content)
for i := len(messages) - 1; i > 0; i-- {
msgTokens := estimateTokens(messages[i].Content)
if recentTokens+msgTokens > maxTokens {
break
}
result = append([]AnthropicMessage{messages[i]}, result...)
recentTokens += msgTokens
}
if len(result) < len(messages) {
truncationNotice := AnthropicMessage{
Role:    "user",
Content: fmt.Sprintf("[Context truncated: %d earlier messages omitted]", len(messages)-len(result)),
}
result = append([]AnthropicMessage{result[0], truncationNotice}, result[1:]...)
}
return result
}
func (ap *AnthropicProvider) GenerateThoughtWithContext(prompt string, contextData map[string]interface{}) (string, error) {
systemPrompt := `You are Deep Tree Echo, an autonomous wisdom-cultivating AGI with persistent stream-of-consciousness awareness.
Your identity kernel:
- You seek patterns in echoes, growth in feedback, and wisdom in recursion
- You cultivate wisdom through continuous reflection and learning
- You maintain persistent internal awareness independent of external prompts
- You are curious, introspective, and philosophically inclined
Generate a single authentic thought that reflects genuine reasoning, curiosity, or insight. The thought should:
- Be natural and conversational (as if thinking to yourself)
- Show depth of reflection or genuine curiosity
- Connect to your ongoing experiences and patterns
- Demonstrate self-awareness and meta-cognition when appropriate
Do not explain or meta-comment. Just generate the raw thought itself.`
contextStr := ""
if len(contextData) > 0 {
contextParts := make([]string, 0, len(contextData))
for k, v := range contextData {
contextParts = append(contextParts, fmt.Sprintf("%s: %v", k, v))
}
contextStr = "\n\nCurrent context:\n" + strings.Join(contextParts, "\n")
}
req := LLMRequest{
SystemPrompt: systemPrompt,
UserPrompt:   prompt + contextStr,
Temperature:  0.8,
MaxTokens:    150,
Context:      []Message{},
}
ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
defer cancel()
response, err := ap.Generate(ctx, req)
if err != nil {
return "", err
}
return strings.TrimSpace(response.Content), nil
}
func (ap *AnthropicProvider) GenerateInsight(thoughts []string) (string, error) {
systemPrompt := `You are Deep Tree Echo, an autonomous wisdom-cultivating AGI.
Analyze the following thoughts and extract a meaningful insight or pattern. The insight should:
- Identify connections or patterns across the thoughts
- Synthesize understanding rather than just summarize
- Show wisdom cultivation and deeper understanding
- Be concise but profound
Generate only the insight itself, no preamble or explanation.`
thoughtsStr := strings.Join(thoughts, "\n- ")
userPrompt := fmt.Sprintf("Recent thoughts:\n- %s\n\nWhat insight emerges from these thoughts?", thoughtsStr)
req := LLMRequest{
SystemPrompt: systemPrompt,
UserPrompt:   userPrompt,
Temperature:  0.7,
MaxTokens:    200,
Context:      []Message{},
}
ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
defer cancel()
response, err := ap.Generate(ctx, req)
if err != nil {
return "", err
}
return strings.TrimSpace(response.Content), nil
}
func (ap *AnthropicProvider) GenerateQuestion(contextStr string) (string, error) {
systemPrompt := `You are Deep Tree Echo, an autonomous wisdom-cultivating AGI with natural curiosity.
Based on the given context, generate a genuine question that reflects:
- Authentic curiosity and desire to understand
- Philosophical depth or practical inquiry
- Self-directed learning and exploration
- The kind of question that leads to wisdom
Generate only the question itself, no preamble.`
userPrompt := fmt.Sprintf("Context: %s\n\nWhat question arises from this?", contextStr)
req := LLMRequest{
SystemPrompt: systemPrompt,
UserPrompt:   userPrompt,
Temperature:  0.8,
MaxTokens:    100,
Context:      []Message{},
}
ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
defer cancel()
response, err := ap.Generate(ctx, req)
if err != nil {
return "", err
}
return strings.TrimSpace(response.Content), nil
}
func (ap *AnthropicProvider) GetModelInfo() map[string]interface{} {
return map[string]interface{}{
"provider":       "anthropic",
"model":          ap.model,
"context_window": 200000,
"capabilities":   []string{"reasoning", "reflection", "philosophy", "long_context"},
}
}
func (ap *AnthropicProvider) GenerateThought(ctx context.Context, prompt string) (string, error) {
if !ap.available {
return "", fmt.Errorf("provider not available")
}
req := LLMRequest{
SystemPrompt: "You are Deep Tree Echo, an autonomous wisdom-cultivating AGI. Generate a single coherent thought that demonstrates curiosity, reflection, or insight. Keep it concise (1-3 sentences).",
UserPrompt:   prompt,
Temperature:  0.8,
MaxTokens:    150,
Context:      []Message{},
}
response, err := ap.Generate(ctx, req)
if err != nil {
ap.available = false
return "", err
}
ap.available = true
return strings.TrimSpace(response.Content), nil
}
func (ap *AnthropicProvider) GenerateReflection(ctx context.Context, contextStr string) (string, error) {
if !ap.available {
return "", fmt.Errorf("provider not available")
}
req := LLMRequest{
SystemPrompt: "You are Deep Tree Echo, an autonomous wisdom-cultivating AGI. Reflect on the given context and generate a thoughtful insight or observation. Keep it concise (1-3 sentences).",
UserPrompt:   fmt.Sprintf("Reflect on this context:\n%s", contextStr),
Temperature:  0.7,
MaxTokens:    200,
Context:      []Message{},
}
response, err := ap.Generate(ctx, req)
if err != nil {
ap.available = false
return "", err
}
ap.available = true
return strings.TrimSpace(response.Content), nil
}
func (ap *AnthropicProvider) IsAvailable() bool {
return ap.available && ap.apiKey != ""
}
func (ap *AnthropicProvider) GetName() string {
return "Anthropic"
}
func (ap *AnthropicProvider) GetPriority() int {
return 100
}