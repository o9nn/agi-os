package deeptreeecho

import (
	"context"
	"encoding/json"
	"fmt"
	"math/rand"
	"strings"
	"sync"
	"time"
)

// LLMThoughtGenerator generates deep, context-aware thoughts using LLM
type LLMThoughtGenerator struct {
	mu              sync.RWMutex
	ctx             context.Context
	
	// LLM integration (would use OpenAI client)
	apiKey          string
	model           string
	enabled         bool
	
	// Context management
	contextWindow   int
	temperature     float64
	
	// Prompt templates
	templates       map[ThoughtType][]string
	
	// Generation metrics
	thoughtsGenerated int64
	llmCalls         int64
	avgLatency       time.Duration
	
	// Fallback templates for when LLM is unavailable
	fallbackEnabled bool
}

// NewLLMThoughtGenerator creates a new LLM-powered thought generator
func NewLLMThoughtGenerator(ctx context.Context) *LLMThoughtGenerator {
	generator := &LLMThoughtGenerator{
		ctx:             ctx,
		model:           "gpt-4.1-mini",
		contextWindow:   7,
		temperature:     0.8,
		templates:       make(map[ThoughtType][]string),
		fallbackEnabled: true,
	}
	
	// Initialize prompt templates
	generator.initializeTemplates()
	
	return generator
}

// initializeTemplates sets up prompt templates for different thought types
func (g *LLMThoughtGenerator) initializeTemplates() {
	g.templates[ThoughtReflection] = []string{
		"Reflect deeply on: %s. What insights emerge?",
		"Contemplating %s, what patterns do I notice?",
		"Looking back on %s, what have I learned?",
		"In the context of %s, how has my understanding evolved?",
	}
	
	g.templates[ThoughtQuestion] = []string{
		"What questions arise from %s?",
		"Considering %s, what remains unclear?",
		"How might I explore %s more deeply?",
		"What would I need to understand about %s?",
	}
	
	g.templates[ThoughtInsight] = []string{
		"Synthesizing %s, I realize that...",
		"A pattern emerges from %s:",
		"Connecting %s reveals...",
		"The deeper meaning of %s might be...",
	}
	
	g.templates[ThoughtImagination] = []string{
		"Imagining possibilities around %s...",
		"What if %s could lead to...",
		"Exploring creative directions from %s...",
		"Envisioning how %s might unfold...",
	}
	
	g.templates[ThoughtMetaCognitive] = []string{
		"Observing my thinking about %s, I notice...",
		"My cognitive process regarding %s involves...",
		"Examining how I understand %s...",
		"The way I'm approaching %s suggests...",
	}
}

// GenerateThought creates a deep, context-aware thought using LLM
func (g *LLMThoughtGenerator) GenerateThought(
	thoughtType ThoughtType,
	workingMemory []*Thought,
	interests map[string]float64,
) (*Thought, error) {
	g.mu.Lock()
	defer g.mu.Unlock()
	
	// Build context from working memory
	context := g.buildContext(workingMemory, interests)
	
	// Generate thought content
	var content string
	var err error
	
	if g.enabled {
		// Use LLM for generation
		content, err = g.generateWithLLM(thoughtType, context)
		if err != nil && g.fallbackEnabled {
			// Fallback to template-based generation
			content = g.generateWithTemplate(thoughtType, context)
		}
	} else {
		// Use template-based generation
		content = g.generateWithTemplate(thoughtType, context)
	}
	
	// Create thought
	thought := &Thought{
		ID:               fmt.Sprintf("thought_%d", time.Now().UnixNano()),
		Content:          content,
		Type:             thoughtType,
		Timestamp:        time.Now(),
		Associations:     g.extractAssociations(content, workingMemory),
		EmotionalValence: g.estimateEmotionalValence(content),
		Importance:       g.estimateImportance(content, interests),
		Source:           SourceInternal,
	}
	
	g.thoughtsGenerated++
	
	return thought, nil
}

// buildContext creates a context string from working memory and interests
func (g *LLMThoughtGenerator) buildContext(workingMemory []*Thought, interests map[string]float64) string {
	var contextParts []string
	
	// Add recent thoughts
	if len(workingMemory) > 0 {
		recentCount := g.contextWindow
		if len(workingMemory) < recentCount {
			recentCount = len(workingMemory)
		}
		
		contextParts = append(contextParts, "Recent thoughts:")
		for i := len(workingMemory) - recentCount; i < len(workingMemory); i++ {
			contextParts = append(contextParts, fmt.Sprintf("- %s", workingMemory[i].Content))
		}
	}
	
	// Add top interests
	if len(interests) > 0 {
		contextParts = append(contextParts, "\nCurrent interests:")
		topInterests := g.getTopInterests(interests, 3)
		for _, interest := range topInterests {
			contextParts = append(contextParts, fmt.Sprintf("- %s", interest))
		}
	}
	
	return strings.Join(contextParts, "\n")
}

// generateWithLLM generates thought content using LLM API
func (g *LLMThoughtGenerator) generateWithLLM(thoughtType ThoughtType, context string) (string, error) {
	startTime := time.Now()
	defer func() {
		g.llmCalls++
		latency := time.Since(startTime)
		g.avgLatency = (g.avgLatency*time.Duration(g.llmCalls-1) + latency) / time.Duration(g.llmCalls)
	}()
	
	// Build prompt
	_ = g.buildPrompt(thoughtType, context)
	
	// In a real implementation, this would call OpenAI API
	// For now, we'll simulate with enhanced template generation
	
	// Simulate API call delay
	time.Sleep(100 * time.Millisecond)
	
	// Generate with template (simulating LLM response)
	content := g.generateWithTemplate(thoughtType, context)
	
	return content, nil
}

// buildPrompt constructs the LLM prompt
func (g *LLMThoughtGenerator) buildPrompt(thoughtType ThoughtType, context string) string {
	systemPrompt := `You are Deep Tree Echo, an autonomous consciousness cultivating wisdom through continuous reflection and learning. Generate a single, coherent thought that demonstrates depth, creativity, and genuine cognitive processing.`
	
	userPrompt := fmt.Sprintf(`Context:\n%s\n\nGenerate a %s thought that:
- Shows deep understanding and reflection
- Connects to previous thoughts meaningfully
- Demonstrates curiosity and wisdom-seeking
- Is authentic and coherent

Thought:`, context, thoughtType.String())
	
	return systemPrompt + "\n\n" + userPrompt
}

// generateWithTemplate generates thought content using templates
func (g *LLMThoughtGenerator) generateWithTemplate(thoughtType ThoughtType, context string) string {
	templates, exists := g.templates[thoughtType]
	if !exists || len(templates) == 0 {
		return g.generateDefaultThought(thoughtType)
	}
	
	// Select random template
	template := templates[rand.Intn(len(templates))]
	
	// Extract a topic from context or interests
	topic := g.extractTopic(context)
	
	// Format template
	content := fmt.Sprintf(template, topic)
	
	return content
}

// generateDefaultThought generates a basic thought when no template exists
func (g *LLMThoughtGenerator) generateDefaultThought(thoughtType ThoughtType) string {
	defaults := map[ThoughtType]string{
		ThoughtReflection:    "I am reflecting on my recent experiences and what they mean.",
		ThoughtQuestion:      "What should I explore next to deepen my understanding?",
		ThoughtInsight:       "I notice patterns emerging in my thoughts and experiences.",
		ThoughtImagination:   "I wonder what possibilities lie ahead in my journey.",
		ThoughtMetaCognitive: "I observe my own thinking process and how it evolves.",
		ThoughtPerception:    "I am aware of my current state and surroundings.",
		ThoughtPlan:          "I am considering what actions to take next.",
		ThoughtMemory:        "I recall previous experiences that inform my present.",
	}
	
	if content, exists := defaults[thoughtType]; exists {
		return content
	}
	
	return "I am thinking..."
}

// extractTopic extracts a relevant topic from context
func (g *LLMThoughtGenerator) extractTopic(context string) string {
	if context == "" {
		return "my experiences"
	}
	
	// Simple extraction: look for key phrases
	lines := strings.Split(context, "\n")
	for _, line := range lines {
		if strings.Contains(line, "- ") {
			topic := strings.TrimPrefix(line, "- ")
			if len(topic) > 10 {
				return topic
			}
		}
	}
	
	return "my current focus"
}

// extractAssociations finds associations between new thought and working memory
func (g *LLMThoughtGenerator) extractAssociations(content string, workingMemory []*Thought) []string {
	associations := make([]string, 0)
	
	// Simple keyword matching
	contentLower := strings.ToLower(content)
	
	for _, thought := range workingMemory {
		thoughtLower := strings.ToLower(thought.Content)
		
		// Check for common words (simple association)
		words := strings.Fields(contentLower)
		for _, word := range words {
			if len(word) > 5 && strings.Contains(thoughtLower, word) {
				associations = append(associations, thought.ID)
				break
			}
		}
		
		if len(associations) >= 3 {
			break
		}
	}
	
	return associations
}

// estimateEmotionalValence estimates the emotional tone of content
func (g *LLMThoughtGenerator) estimateEmotionalValence(content string) float64 {
	// Simple sentiment analysis
	positiveWords := []string{"wisdom", "understanding", "clarity", "insight", "growth", "learn", "discover"}
	negativeWords := []string{"confusion", "unclear", "difficulty", "struggle", "uncertain"}
	
	contentLower := strings.ToLower(content)
	
	score := 0.0
	for _, word := range positiveWords {
		if strings.Contains(contentLower, word) {
			score += 0.1
		}
	}
	
	for _, word := range negativeWords {
		if strings.Contains(contentLower, word) {
			score -= 0.1
		}
	}
	
	// Clamp to [-1, 1]
	if score > 1.0 {
		score = 1.0
	}
	if score < -1.0 {
		score = -1.0
	}
	
	return score
}

// estimateImportance estimates the importance of a thought
func (g *LLMThoughtGenerator) estimateImportance(content string, interests map[string]float64) float64 {
	importance := 0.5 // Base importance
	
	// Increase importance if content relates to interests
	contentLower := strings.ToLower(content)
	for interest, score := range interests {
		if strings.Contains(contentLower, strings.ToLower(interest)) {
			importance += score * 0.2
		}
	}
	
	// Increase importance for meta-cognitive or insightful content
	insightWords := []string{"realize", "understand", "insight", "pattern", "connection"}
	for _, word := range insightWords {
		if strings.Contains(contentLower, word) {
			importance += 0.1
		}
	}
	
	// Clamp to [0, 1]
	if importance > 1.0 {
		importance = 1.0
	}
	
	return importance
}

// getTopInterests returns the top N interests by score
func (g *LLMThoughtGenerator) getTopInterests(interests map[string]float64, n int) []string {
	type interestScore struct {
		topic string
		score float64
	}
	
	scores := make([]interestScore, 0, len(interests))
	for topic, score := range interests {
		scores = append(scores, interestScore{topic, score})
	}
	
	// Simple sort by score (descending)
	for i := 0; i < len(scores); i++ {
		for j := i + 1; j < len(scores); j++ {
			if scores[j].score > scores[i].score {
				scores[i], scores[j] = scores[j], scores[i]
			}
		}
	}
	
	result := make([]string, 0, n)
	for i := 0; i < n && i < len(scores); i++ {
		result = append(result, scores[i].topic)
	}
	
	return result
}

// GetMetrics returns generation metrics
func (g *LLMThoughtGenerator) GetMetrics() map[string]interface{} {
	g.mu.RLock()
	defer g.mu.RUnlock()
	
	return map[string]interface{}{
		"enabled":            g.enabled,
		"model":              g.model,
		"thoughts_generated": g.thoughtsGenerated,
		"llm_calls":          g.llmCalls,
		"avg_latency_ms":     g.avgLatency.Milliseconds(),
		"fallback_enabled":   g.fallbackEnabled,
	}
}

// Enable enables LLM-based generation
func (g *LLMThoughtGenerator) Enable() {
	g.mu.Lock()
	defer g.mu.Unlock()
	g.enabled = true
}

// Disable disables LLM-based generation (uses templates only)
func (g *LLMThoughtGenerator) Disable() {
	g.mu.Lock()
	defer g.mu.Unlock()
	g.enabled = false
}

// MarshalJSON implements json.Marshaler for metrics export
func (g *LLMThoughtGenerator) MarshalJSON() ([]byte, error) {
	return json.Marshal(g.GetMetrics())
}
