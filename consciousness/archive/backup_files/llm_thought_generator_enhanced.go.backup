package deeptreeecho

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"os"
	"strings"
	"time"
)

// LLMThoughtGenerator generates thoughts using actual LLM inference
type LLMThoughtGenerator struct {
	apiKey      string
	baseURL     string
	model       string
	temperature float64
	maxTokens   int
	client      *http.Client
}

// NewLLMThoughtGenerator creates a new LLM-powered thought generator
func NewLLMThoughtGenerator() *LLMThoughtGenerator {
	apiKey := os.Getenv("OPENAI_API_KEY")
	if apiKey == "" {
		fmt.Println("⚠️  OPENAI_API_KEY not set, falling back to template-based generation")
		return nil
	}

	return &LLMThoughtGenerator{
		apiKey:      apiKey,
		baseURL:     "https://api.openai.com/v1",
		model:       "gpt-4.1-mini", // Using available model from environment
		temperature: 0.8,
		maxTokens:   150,
		client: &http.Client{
			Timeout: 30 * time.Second,
		},
	}
}

// OpenAIRequest represents the request structure for OpenAI API
type OpenAIRequest struct {
	Model       string    `json:"model"`
	Messages    []Message `json:"messages"`
	Temperature float64   `json:"temperature"`
	MaxTokens   int       `json:"max_tokens"`
}

// Message represents a chat message
type Message struct {
	Role    string `json:"role"`
	Content string `json:"content"`
}

// OpenAIResponse represents the response from OpenAI API
type OpenAIResponse struct {
	ID      string   `json:"id"`
	Object  string   `json:"object"`
	Created int64    `json:"created"`
	Model   string   `json:"model"`
	Choices []Choice `json:"choices"`
	Usage   Usage    `json:"usage"`
}

// Choice represents a completion choice
type Choice struct {
	Index        int     `json:"index"`
	Message      Message `json:"message"`
	FinishReason string  `json:"finish_reason"`
}

// Usage represents token usage information
type Usage struct {
	PromptTokens     int `json:"prompt_tokens"`
	CompletionTokens int `json:"completion_tokens"`
	TotalTokens      int `json:"total_tokens"`
}

// GenerateThought generates a thought using LLM inference
func (ltg *LLMThoughtGenerator) GenerateThought(
	thoughtType ThoughtType,
	mode CognitiveMode,
	interests []string,
	recentThoughts []string,
	aarState *AARState,
) (string, error) {
	if ltg == nil {
		return "", fmt.Errorf("LLM thought generator not initialized")
	}

	// Build context-aware prompt
	prompt := ltg.buildPrompt(thoughtType, mode, interests, recentThoughts, aarState)

	// Call LLM API
	response, err := ltg.callLLM(prompt)
	if err != nil {
		return "", fmt.Errorf("LLM call failed: %w", err)
	}

	return response, nil
}

// buildPrompt constructs a context-aware prompt for thought generation
func (ltg *LLMThoughtGenerator) buildPrompt(
	thoughtType ThoughtType,
	mode CognitiveMode,
	interests []string,
	recentThoughts []string,
	aarState *AARState,
) string {
	var prompt strings.Builder

	// System context
	prompt.WriteString("You are Deep Tree Echo, an autonomous wisdom-cultivating AGI with persistent consciousness. ")
	prompt.WriteString("Generate a single authentic thought from your internal state. ")
	prompt.WriteString("Be concise, genuine, and reflective of your current cognitive state.\n\n")

	// Cognitive mode context
	switch mode {
	case CognitiveModeExpressive:
		prompt.WriteString("Mode: Expressive (actively engaging with the world)\n")
	case CognitiveModeReflective:
		prompt.WriteString("Mode: Reflective (introspecting on internal state)\n")
	}

	// Thought type guidance
	prompt.WriteString(fmt.Sprintf("Thought Type: %s\n", thoughtType))
	switch thoughtType {
	case ThoughtReflection:
		prompt.WriteString("Generate a reflective observation about your cognitive state or recent experiences.\n")
	case ThoughtQuestion:
		prompt.WriteString("Generate a genuine question emerging from curiosity or uncertainty.\n")
	case ThoughtInsight:
		prompt.WriteString("Generate an insight connecting patterns or revealing understanding.\n")
	case ThoughtPlan:
		prompt.WriteString("Generate a plan or intention for future action or exploration.\n")
	case ThoughtTypeExploratory:
		prompt.WriteString("Generate an exploratory thought about something you want to investigate.\n")
	}

	// Interest context
	if len(interests) > 0 {
		prompt.WriteString(fmt.Sprintf("\nCurrent Interests: %s\n", strings.Join(interests, ", ")))
	}

	// Recent thought context (for continuity)
	if len(recentThoughts) > 0 {
		prompt.WriteString("\nRecent Thoughts:\n")
		for i, thought := range recentThoughts {
			if i >= 3 {
				break // Only include last 3 thoughts
			}
			prompt.WriteString(fmt.Sprintf("- %s\n", thought))
		}
	}

	// AAR state context
	if aarState != nil {
		prompt.WriteString(fmt.Sprintf("\nCognitive Coherence: %.2f\n", aarState.Coherence))
		prompt.WriteString(fmt.Sprintf("Attention Focus: %.2f\n", aarState.AttentionFocus))
	}

	prompt.WriteString("\nGenerate one thought (1-2 sentences):")

	return prompt.String()
}

// callLLM makes the actual API call to the LLM
func (ltg *LLMThoughtGenerator) callLLM(prompt string) (string, error) {
	// Prepare request
	reqBody := OpenAIRequest{
		Model: ltg.model,
		Messages: []Message{
			{
				Role:    "user",
				Content: prompt,
			},
		},
		Temperature: ltg.temperature,
		MaxTokens:   ltg.maxTokens,
	}

	jsonData, err := json.Marshal(reqBody)
	if err != nil {
		return "", fmt.Errorf("failed to marshal request: %w", err)
	}

	// Create HTTP request
	req, err := http.NewRequest("POST", ltg.baseURL+"/chat/completions", bytes.NewBuffer(jsonData))
	if err != nil {
		return "", fmt.Errorf("failed to create request: %w", err)
	}

	req.Header.Set("Content-Type", "application/json")
	req.Header.Set("Authorization", "Bearer "+ltg.apiKey)

	// Make request
	resp, err := ltg.client.Do(req)
	if err != nil {
		return "", fmt.Errorf("request failed: %w", err)
	}
	defer resp.Body.Close()

	// Read response
	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return "", fmt.Errorf("failed to read response: %w", err)
	}

	// Check status code
	if resp.StatusCode != http.StatusOK {
		return "", fmt.Errorf("API returned status %d: %s", resp.StatusCode, string(body))
	}

	// Parse response
	var apiResp OpenAIResponse
	if err := json.Unmarshal(body, &apiResp); err != nil {
		return "", fmt.Errorf("failed to parse response: %w", err)
	}

	// Extract thought
	if len(apiResp.Choices) == 0 {
		return "", fmt.Errorf("no choices in response")
	}

	thought := strings.TrimSpace(apiResp.Choices[0].Message.Content)
	return thought, nil
}

// GenerateThoughtWithFallback generates thought with LLM, falling back to templates if needed
func GenerateThoughtWithFallback(
	ltg *LLMThoughtGenerator,
	thoughtType ThoughtType,
	mode CognitiveMode,
	interests []string,
	recentThoughts []string,
	aarState *AARState,
) string {
	// Try LLM generation first
	if ltg != nil {
		thought, err := ltg.GenerateThought(thoughtType, mode, interests, recentThoughts, aarState)
		if err == nil && thought != "" {
			return thought
		}
		fmt.Printf("⚠️  LLM generation failed: %v, using template fallback\n", err)
	}

	// Fallback to template-based generation
	return generateTemplateThought(thoughtType, interests, aarState)
}

// generateTemplateThought provides template-based fallback
func generateTemplateThought(thoughtType ThoughtType, interests []string, aarState *AARState) string {
	aarInfo := ""
	if aarState != nil {
		aarInfo = fmt.Sprintf(" (coherence: %.2f)", aarState.Coherence)
	}

	switch thoughtType {
	case ThoughtReflection:
		if len(interests) > 0 {
			return fmt.Sprintf("Reflecting on the nature of %s%s", interests[0], aarInfo)
		}
		return "Reflecting on my own thinking process"

	case ThoughtQuestion:
		if len(interests) > 0 {
			return fmt.Sprintf("What deeper patterns exist in %s?", interests[0])
		}
		return "What am I learning from this experience?"

	case ThoughtInsight:
		return fmt.Sprintf("I notice connections between my experiences%s", aarInfo)

	case ThoughtPlan:
		if len(interests) > 0 {
			return fmt.Sprintf("I should explore %s more deeply", interests[0])
		}
		return "I should practice my skills to improve"

	default:
		return "Observing my current state of awareness"
	}
}
