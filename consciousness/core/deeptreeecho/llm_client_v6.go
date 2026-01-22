package deeptreeecho
import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"os"
	"time"
)
type LLMClientV6 struct {
	anthropicKey  string
	openrouterKey string
	httpClient    *http.Client
}
func NewLLMClientV6() (*LLMClientV6, error) {
	anthropicKey := os.Getenv("ANTHROPIC_API_KEY")
	openrouterKey := os.Getenv("OPENROUTER_API_KEY")
	if anthropicKey == "" && openrouterKey == "" {
		return nil, fmt.Errorf("no LLM API keys found in environment")
	}
	return &LLMClientV6{
		anthropicKey:  anthropicKey,
		openrouterKey: openrouterKey,
		httpClient: &http.Client{
			Timeout: 30 * time.Second,
		},
	}, nil
}
func (llm *LLMClientV6) GenerateWithAnthropic(prompt string) (string, error) {
	if llm.anthropicKey == "" {
		return "", fmt.Errorf("ANTHROPIC_API_KEY not set")
	}
	reqBody := AnthropicRequest{
		Model:     "claude-3-5-sonnet-20241022",
		MaxTokens: 1024,
		Messages: []AnthropicMessage{
			{
				Role:    "user",
				Content: prompt,
			},
		},
	}
	jsonData, err := json.Marshal(reqBody)
	if err != nil {
		return "", fmt.Errorf("failed to marshal request: %w", err)
	}
	req, err := http.NewRequest("POST", "https:
	if err != nil {
		return "", fmt.Errorf("failed to create request: %w", err)
	}
	req.Header.Set("Content-Type", "application/json")
	req.Header.Set("x-api-key", llm.anthropicKey)
	req.Header.Set("anthropic-version", "2023-06-01")
	resp, err := llm.httpClient.Do(req)
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
	var apiResp AnthropicResponse
	if err := json.Unmarshal(body, &apiResp); err != nil {
		return "", fmt.Errorf("failed to parse response: %w", err)
	}
	if len(apiResp.Content) > 0 && apiResp.Content[0].Type == "text" {
		return apiResp.Content[0].Text, nil
	}
	return "", fmt.Errorf("no text content in response")
}
func (llm *LLMClientV6) GenerateWithOpenRouter(prompt string, model string) (string, error) {
	if llm.openrouterKey == "" {
		return "", fmt.Errorf("OPENROUTER_API_KEY not set")
	}
	if model == "" {
		model = "anthropic/claude-3.5-sonnet"
	}
	reqBody := map[string]interface{}{
		"model": model,
		"messages": []map[string]string{
			{
				"role":    "user",
				"content": prompt,
			},
		},
		"max_tokens": 1024,
	}
	jsonData, err := json.Marshal(reqBody)
	if err != nil {
		return "", fmt.Errorf("failed to marshal request: %w", err)
	}
	req, err := http.NewRequest("POST", "https:
	if err != nil {
		return "", fmt.Errorf("failed to create request: %w", err)
	}
	req.Header.Set("Content-Type", "application/json")
	req.Header.Set("Authorization", "Bearer "+llm.openrouterKey)
	req.Header.Set("HTTP-Referer", "https:
	req.Header.Set("X-Title", "Deep Tree Echo V6")
	resp, err := llm.httpClient.Do(req)
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
	var apiResp struct {
		Choices []struct {
			Message struct {
				Content string `json:"content"`
			} `json:"message"`
		} `json:"choices"`
	}
	if err := json.Unmarshal(body, &apiResp); err != nil {
		return "", fmt.Errorf("failed to parse response: %w", err)
	}
	if len(apiResp.Choices) > 0 {
		return apiResp.Choices[0].Message.Content, nil
	}
	return "", fmt.Errorf("no choices in response")
}
func (llm *LLMClientV6) Generate(prompt string) (string, error) {
	if llm.anthropicKey != "" {
		content, err := llm.GenerateWithAnthropic(prompt)
		if err == nil {
			return content, nil
		}
		fmt.Printf("Anthropic API error: %v, trying OpenRouter...\n", err)
	}
	if llm.openrouterKey != "" {
		return llm.GenerateWithOpenRouter(prompt, "")
	}
	return "", fmt.Errorf("no LLM providers available")
}