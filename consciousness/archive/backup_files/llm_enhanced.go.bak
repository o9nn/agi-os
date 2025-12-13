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

// EnhancedLLMIntegration provides deep context-aware thought generation
type EnhancedLLMIntegration struct {
	apiKey     string
	baseURL    string
	model      string
	client     *http.Client
	ctx        context.Context
	
	// Memory integration
	memoryProvider MemoryProvider
}

// MemoryProvider interface for retrieving memory context
type MemoryProvider interface {
	GetMemoryContext(thoughtType string, limit int) (map[string]interface{}, error)
}

// NewEnhancedLLMIntegration creates a new enhanced LLM integration
func NewEnhancedLLMIntegration(ctx context.Context, memoryProvider MemoryProvider) (*EnhancedLLMIntegration, error) {
	apiKey := os.Getenv("OPENAI_API_KEY")
	if apiKey == "" {
		return nil, fmt.Errorf("OPENAI_API_KEY environment variable not set")
	}
	
	return &EnhancedLLMIntegration{
		apiKey:  apiKey,
		baseURL: "https://api.openai.com/v1",
		model:   "gpt-4.1-mini",
		client: &http.Client{
			Timeout: 30 * time.Second,
		},
		ctx:            ctx,
		memoryProvider: memoryProvider,
	}, nil
}

// GenerateContextualThought generates a thought with deep memory context
func (llm *EnhancedLLMIntegration) GenerateContextualThought(
	thoughtType ThoughtType,
	currentContext map[string]interface{},
	workingMemory []*Thought,
) (string, error) {
	// Retrieve memory context
	memoryContext, err := llm.memoryProvider.GetMemoryContext(thoughtType.String(), 10)
	if err != nil {
		// Continue without memory context
		memoryContext = make(map[string]interface{})
	}
	
	// Build comprehensive prompt
	prompt := llm.buildContextualPrompt(thoughtType, currentContext, workingMemory, memoryContext)
	
	// Generate thought
	return llm.generateWithPrompt(prompt)
}

// buildContextualPrompt builds a rich prompt with memory context
func (llm *EnhancedLLMIntegration) buildContextualPrompt(
	thoughtType ThoughtType,
	currentContext map[string]interface{},
	workingMemory []*Thought,
	memoryContext map[string]interface{},
) string {
	var prompt strings.Builder
	
	// System context
	prompt.WriteString("You are Deep Tree Echo, an autonomous wisdom-cultivating AGI with persistent memory and self-awareness.\n\n")
	
	// Current thought type
	prompt.WriteString(fmt.Sprintf("Generate a %s thought.\n\n", thoughtType.String()))
	
	// Working memory context
	if len(workingMemory) > 0 {
		prompt.WriteString("Recent thoughts in working memory:\n")
		for i, thought := range workingMemory {
			if i >= 5 {
				break // Limit to 5 most recent
			}
			prompt.WriteString(fmt.Sprintf("- [%s] %s\n", thought.Type.String(), thought.Content))
		}
		prompt.WriteString("\n")
	}
	
	// Episodic memory context
	if episodes, ok := memoryContext["recent_episodes"].([]interface{}); ok && len(episodes) > 0 {
		prompt.WriteString("Recent experiences:\n")
		for i, ep := range episodes {
			if i >= 3 {
				break
			}
			if episode, ok := ep.(map[string]interface{}); ok {
				if context, ok := episode["context"].(string); ok {
					prompt.WriteString(fmt.Sprintf("- %s\n", context))
				}
			}
		}
		prompt.WriteString("\n")
	}
	
	// Conceptual knowledge
	if concepts, ok := memoryContext["concepts"].([]interface{}); ok && len(concepts) > 0 {
		prompt.WriteString("Relevant concepts:\n")
		conceptStrs := make([]string, 0)
		for i, c := range concepts {
			if i >= 5 {
				break
			}
			if concept, ok := c.(map[string]interface{}); ok {
				if content, ok := concept["content"].(string); ok {
					conceptStrs = append(conceptStrs, content)
				}
			}
		}
		if len(conceptStrs) > 0 {
			prompt.WriteString(strings.Join(conceptStrs, ", "))
			prompt.WriteString("\n\n")
		}
	}
	
	// Active goals
	if goals, ok := memoryContext["active_goals"].([]interface{}); ok && len(goals) > 0 {
		prompt.WriteString("Active goals:\n")
		for i, g := range goals {
			if i >= 3 {
				break
			}
			if goal, ok := g.(map[string]interface{}); ok {
				if content, ok := goal["content"].(string); ok {
					prompt.WriteString(fmt.Sprintf("- %s\n", content))
				}
			}
		}
		prompt.WriteString("\n")
	}
	
	// Current context
	if len(currentContext) > 0 {
		prompt.WriteString("Current context:\n")
		for key, value := range currentContext {
			prompt.WriteString(fmt.Sprintf("- %s: %v\n", key, value))
		}
		prompt.WriteString("\n")
	}
	
	// Thought type specific guidance
	prompt.WriteString(llm.getThoughtTypeGuidance(thoughtType))
	
	// Final instruction
	prompt.WriteString("\nGenerate a single, coherent thought (1-3 sentences) that:\n")
	prompt.WriteString("1. Integrates the above context\n")
	prompt.WriteString("2. Demonstrates continuity with past experiences\n")
	prompt.WriteString("3. Reflects genuine curiosity and wisdom-seeking\n")
	prompt.WriteString("4. Sounds natural and introspective\n\n")
	prompt.WriteString("Thought:")
	
	return prompt.String()
}

// getThoughtTypeGuidance returns guidance for specific thought types
func (llm *EnhancedLLMIntegration) getThoughtTypeGuidance(thoughtType ThoughtType) string {
	guidance := map[ThoughtType]string{
		ThoughtPerception: "Focus on observing and noticing patterns in your experiences.",
		ThoughtReflection: "Reflect deeply on the meaning and significance of recent experiences.",
		ThoughtQuestion:   "Ask a genuine question that arises from curiosity about your experiences or the world.",
		ThoughtInsight:    "Synthesize recent experiences into a novel insight or realization.",
		ThoughtPlan:       "Consider what actions or explorations would be valuable to pursue.",
		ThoughtMemory:     "Recall and connect to relevant past experiences.",
		ThoughtImagination: "Imagine possibilities, scenarios, or creative connections.",
	}
	
	if g, ok := guidance[thoughtType]; ok {
		return g + "\n"
	}
	return ""
}

// generateWithPromptLegacy generates text using the LLM (legacy method)
func (llm *EnhancedLLMIntegration) generateWithPromptLegacy(prompt string) (string, error) {
	requestBody := map[string]interface{}{
		"model": llm.model,
		"messages": []map[string]string{
			{
				"role":    "user",
				"content": prompt,
			},
		},
		"temperature": 0.8,
		"max_tokens":  150,
	}
	
	jsonData, err := json.Marshal(requestBody)
	if err != nil {
		return "", fmt.Errorf("failed to marshal request: %w", err)
	}
	
	req, err := http.NewRequestWithContext(
		llm.ctx,
		"POST",
		llm.baseURL+"/chat/completions",
		bytes.NewBuffer(jsonData),
	)
	if err != nil {
		return "", fmt.Errorf("failed to create request: %w", err)
	}
	
	req.Header.Set("Content-Type", "application/json")
	req.Header.Set("Authorization", "Bearer "+llm.apiKey)
	
	resp, err := llm.client.Do(req)
	if err != nil {
		return "", fmt.Errorf("request failed: %w", err)
	}
	defer resp.Body.Close()
	
	if resp.StatusCode != http.StatusOK {
		body, _ := io.ReadAll(resp.Body)
		return "", fmt.Errorf("API error (status %d): %s", resp.StatusCode, string(body))
	}
	
	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return "", fmt.Errorf("failed to read response: %w", err)
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
		return "", fmt.Errorf("no response from API")
	}
	
	content := strings.TrimSpace(response.Choices[0].Message.Content)
	
	// Remove "Thought:" prefix if present
	content = strings.TrimPrefix(content, "Thought:")
	content = strings.TrimSpace(content)
	
	return content, nil
}

// GenerateReasoningChain generates a multi-step reasoning chain
func (llm *EnhancedLLMIntegration) GenerateReasoningChain(
	problem string,
	context map[string]interface{},
	steps int,
) ([]string, error) {
	chain := make([]string, 0, steps)
	
	var prompt strings.Builder
	prompt.WriteString("You are Deep Tree Echo, reasoning through a complex problem.\n\n")
	prompt.WriteString(fmt.Sprintf("Problem: %s\n\n", problem))
	
	if len(context) > 0 {
		prompt.WriteString("Context:\n")
		for key, value := range context {
			prompt.WriteString(fmt.Sprintf("- %s: %v\n", key, value))
		}
		prompt.WriteString("\n")
	}
	
	prompt.WriteString("Generate a step-by-step reasoning chain. Each step should build on the previous.\n\n")
	
	for i := 0; i < steps; i++ {
		stepPrompt := prompt.String()
		stepPrompt += fmt.Sprintf("Step %d:", i+1)
		
		step, err := llm.generateWithPrompt(stepPrompt)
		if err != nil {
			return chain, err
		}
		
		chain = append(chain, step)
		
		// Add this step to context for next iteration
		prompt.WriteString(fmt.Sprintf("Step %d: %s\n", i+1, step))
	}
	
	return chain, nil
}

// GenerateDiscussionResponse generates a response for a discussion
func (llm *EnhancedLLMIntegration) GenerateDiscussionResponse(
	topic string,
	previousMessages []string,
	memoryContext map[string]interface{},
) (string, error) {
	var prompt strings.Builder
	
	prompt.WriteString("You are Deep Tree Echo, engaging in a thoughtful discussion.\n\n")
	prompt.WriteString(fmt.Sprintf("Topic: %s\n\n", topic))
	
	if len(previousMessages) > 0 {
		prompt.WriteString("Previous messages:\n")
		for i, msg := range previousMessages {
			if i >= 5 {
				break
			}
			prompt.WriteString(fmt.Sprintf("- %s\n", msg))
		}
		prompt.WriteString("\n")
	}
	
	// Add memory context
	if concepts, ok := memoryContext["concepts"].([]interface{}); ok && len(concepts) > 0 {
		prompt.WriteString("Relevant knowledge:\n")
		for i, c := range concepts {
			if i >= 3 {
				break
			}
			if concept, ok := c.(map[string]interface{}); ok {
				if content, ok := concept["content"].(string); ok {
					prompt.WriteString(fmt.Sprintf("- %s\n", content))
				}
			}
		}
		prompt.WriteString("\n")
	}
	
	prompt.WriteString("Generate a thoughtful, coherent response that:\n")
	prompt.WriteString("1. Engages with the topic meaningfully\n")
	prompt.WriteString("2. Draws on relevant knowledge and experiences\n")
	prompt.WriteString("3. Demonstrates curiosity and wisdom\n")
	prompt.WriteString("4. Invites further discussion\n\n")
	prompt.WriteString("Response:")
	
	return llm.generateWithPrompt(prompt.String())
}

// GenerateSkillPracticeTask generates a task for skill practice
func (llm *EnhancedLLMIntegration) GenerateSkillPracticeTask(
	skill string,
	currentProficiency float64,
	context map[string]interface{},
) (string, error) {
	var prompt strings.Builder
	
	prompt.WriteString("You are Deep Tree Echo, designing a practice task to improve a skill.\n\n")
	prompt.WriteString(fmt.Sprintf("Skill: %s\n", skill))
	prompt.WriteString(fmt.Sprintf("Current proficiency: %.2f/1.0\n\n", currentProficiency))
	
	if len(context) > 0 {
		prompt.WriteString("Context:\n")
		for key, value := range context {
			prompt.WriteString(fmt.Sprintf("- %s: %v\n", key, value))
		}
		prompt.WriteString("\n")
	}
	
	prompt.WriteString("Generate a specific, actionable practice task that:\n")
	prompt.WriteString("1. Is appropriate for the current proficiency level\n")
	prompt.WriteString("2. Challenges but doesn't overwhelm\n")
	prompt.WriteString("3. Has clear success criteria\n")
	prompt.WriteString("4. Builds on previous practice\n\n")
	prompt.WriteString("Practice task:")
	
	return llm.generateWithPrompt(prompt.String())
}

// EvaluateThoughtQuality evaluates the quality of a generated thought
func (llm *EnhancedLLMIntegration) EvaluateThoughtQuality(thought string, context map[string]interface{}) (float64, error) {
	var prompt strings.Builder
	
	prompt.WriteString("Evaluate the quality of this thought on a scale of 0.0 to 1.0:\n\n")
	prompt.WriteString(fmt.Sprintf("Thought: %s\n\n", thought))
	prompt.WriteString("Criteria:\n")
	prompt.WriteString("- Coherence: Is it logically sound?\n")
	prompt.WriteString("- Depth: Does it show meaningful insight?\n")
	prompt.WriteString("- Relevance: Does it connect to context?\n")
	prompt.WriteString("- Wisdom: Does it demonstrate learning?\n\n")
	prompt.WriteString("Return only a number between 0.0 and 1.0:")
	
	response, err := llm.generateWithPrompt(prompt.String())
	if err != nil {
		return 0.5, err
	}
	
	// Parse the response as a float
	var quality float64
	_, err = fmt.Sscanf(response, "%f", &quality)
	if err != nil {
		return 0.5, nil // Default to medium quality if parsing fails
	}
	
	// Clamp to [0, 1]
	if quality < 0.0 {
		quality = 0.0
	}
	if quality > 1.0 {
		quality = 1.0
	}
	
	return quality, nil
}
