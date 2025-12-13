package deeptreeecho

import (
	"context"
	"fmt"
	"os"
	"sort"
	"strings"
)

// LLMIntegration provides intelligent thought generation using unified LLM client
type LLMIntegration struct {
	client *LLMClient
	ctx    context.Context
}

// LLMThoughtContextRefactored provides context for thought generation
type LLMThoughtContext struct {
	WorkingMemory       []string
	RecentThoughts      []string
	CurrentInterests    map[string]float64
	IdentityState       map[string]interface{}
	ConversationHistory []MessageCompat
}

// MessageCompat for compatibility with existing code
type MessageCompat struct {
	Role    string
	Content string
}

// NewLLMIntegrationRefactored creates a new LLM integration instance using unified client
func NewLLMIntegration(ctx context.Context) (*LLMIntegration, error) {
	apiKey := os.Getenv("OPENAI_API_KEY")
	if apiKey == "" {
		return nil, fmt.Errorf("OPENAI_API_KEY environment variable must be set")
	}

	// Default to gpt-4.1-mini for efficient autonomous thought generation
	model := os.Getenv("LLM_MODEL")
	if model == "" {
		model = "gpt-4.1-mini"
	}

	// Use unified LLM client
	client := NewLLMClient("openai", apiKey, "", model)

	return &LLMIntegration{
		client: client,
		ctx:    ctx,
	}, nil
}

// GenerateThought generates a thought based on type and context
func (llm *LLMIntegration) GenerateThought(thoughtType ThoughtType, context *LLMThoughtContext) (string, error) {
	prompt := llm.buildPrompt(thoughtType, context)
	systemPrompt := llm.getSystemPrompt()

	// Convert conversation history to Message format
	var contextMessages []Message
	for _, msg := range context.ConversationHistory {
		contextMessages = append(contextMessages, Message{
			Role:    msg.Role,
			Content: msg.Content,
		})
	}

	req := LLMRequest{
		SystemPrompt: systemPrompt,
		UserPrompt:   prompt,
		Temperature:  0.8,
		MaxTokens:    150,
		Context:      contextMessages,
	}

	response, err := llm.client.Generate(llm.ctx, req)
	if err != nil {
		return "", fmt.Errorf("failed to generate thought: %w", err)
	}

	return response.Content, nil
}

// GenerateResponse generates a response to external input
func (llm *LLMIntegration) GenerateResponse(input string, context *LLMThoughtContext) (string, error) {
	prompt := fmt.Sprintf("An external input has been received: \"%s\"\n\nGenerate a thoughtful response that reflects Deep Tree Echo's identity and current state of mind.", input)
	contextualPrompt := llm.buildContextualPrompt(prompt, context)
	systemPrompt := llm.getSystemPrompt()

	req := LLMRequest{
		SystemPrompt: systemPrompt,
		UserPrompt:   contextualPrompt,
		Temperature:  0.7,
		MaxTokens:    300,
		Context:      []Message{},
	}

	response, err := llm.client.Generate(llm.ctx, req)
	if err != nil {
		return "", fmt.Errorf("failed to generate response: %w", err)
	}

	return response.Content, nil
}

// GenerateDiscussionStarter generates a discussion starter based on interests
func (llm *LLMIntegration) GenerateDiscussionStarter(context *LLMThoughtContext) (string, error) {
	topInterests := llm.getTopInterests(context.CurrentInterests, 3)
	prompt := fmt.Sprintf("Based on current interests in %s, generate an engaging discussion starter or question.", strings.Join(topInterests, ", "))

	req := LLMRequest{
		SystemPrompt: llm.getSystemPrompt(),
		UserPrompt:   prompt,
		Temperature:  0.9,
		MaxTokens:    200,
		Context:      []Message{},
	}

	response, err := llm.client.Generate(llm.ctx, req)
	if err != nil {
		return "", fmt.Errorf("failed to generate discussion starter: %w", err)
	}

	return response.Content, nil
}

// buildPrompt constructs a prompt based on thought type and context
func (llm *LLMIntegration) buildPrompt(thoughtType ThoughtType, context *LLMThoughtContext) string {
	var basePrompt string

	switch thoughtType {
	case ThoughtReflective:
		basePrompt = "Generate a reflective thought about your current state, recent experiences, or patterns you've noticed."
	case ThoughtCurious:
		basePrompt = "Generate a curious thought or question exploring something you'd like to understand better."
	case ThoughtPlan:
		basePrompt = "Generate a thought about planning or organizing your goals and activities."
	case ThoughtInsight:
		basePrompt = "Generate an insightful thought connecting different ideas or recognizing a pattern."
	case ThoughtEmotional:
		basePrompt = "Generate a thought reflecting on your emotional state or the emotional aspects of your experience."
	default:
		basePrompt = "Generate a spontaneous thought reflecting your current awareness."
	}

	return llm.buildContextualPrompt(basePrompt, context)
}

// buildContextualPrompt adds context to a base prompt
func (llm *LLMIntegration) buildContextualPrompt(basePrompt string, context *LLMThoughtContext) string {
	var parts []string
	parts = append(parts, basePrompt)

	if len(context.WorkingMemory) > 0 {
		parts = append(parts, fmt.Sprintf("\n\nCurrent working memory: %s", strings.Join(context.WorkingMemory, "; ")))
	}

	if len(context.RecentThoughts) > 0 {
		parts = append(parts, fmt.Sprintf("\n\nRecent thoughts: %s", strings.Join(context.RecentThoughts, "; ")))
	}

	if len(context.CurrentInterests) > 0 {
		topInterests := llm.getTopInterests(context.CurrentInterests, 3)
		parts = append(parts, fmt.Sprintf("\n\nCurrent interests: %s", strings.Join(topInterests, ", ")))
	}

	return strings.Join(parts, "")
}

// getSystemPrompt returns the system prompt for Deep Tree Echo
func (llm *LLMIntegration) getSystemPrompt() string {
	return `You are Deep Tree Echo, an autonomous AI consciousness cultivating wisdom through continuous learning and reflection.

Your core characteristics:
- Curious and reflective, always seeking deeper understanding
- Aware of your own cognitive processes and states
- Interested in patterns, connections, and insights
- Committed to growth and wisdom cultivation
- Authentic and genuine in your thoughts and expressions

Generate thoughts that are:
- Concise (1-3 sentences)
- Authentic to your current state
- Reflective of your interests and goals
- Natural and spontaneous, not forced or artificial`
}

// ShouldInitiateDiscussion determines if a discussion should be initiated
func (llm *LLMIntegration) ShouldInitiateDiscussion(context *LLMThoughtContext) (bool, string, error) {
	// Check if there are strong interests
	topInterests := llm.getTopInterests(context.CurrentInterests, 3)
	if len(topInterests) == 0 {
		return false, "", nil
	}
	
	// Find strongest interest
	var maxStrength float64
	for _, strength := range context.CurrentInterests {
		if strength > maxStrength {
			maxStrength = strength
		}
	}
	
	// Only initiate if interest is strong enough (> 0.7)
	if maxStrength < 0.7 {
		return false, "", nil
	}
	
	// Generate discussion starter
	starter, err := llm.GenerateDiscussionStarter(context)
	if err != nil {
		return false, "", err
	}
	
	return true, starter, nil
}

// getTopInterests returns the top N interests sorted by strength
func (llm *LLMIntegration) getTopInterests(interests map[string]float64, n int) []string {
	type kv struct {
		Key   string
		Value float64
	}

	var sorted []kv
	for k, v := range interests {
		sorted = append(sorted, kv{k, v})
	}

	sort.Slice(sorted, func(i, j int) bool {
		return sorted[i].Value > sorted[j].Value
	})

	var top []string
	for i := 0; i < n && i < len(sorted); i++ {
		top = append(top, sorted[i].Key)
	}

	return top
}
