package consciousness
import (
	"context"
	"fmt"
	"strings"
	"sync"
	"time"
	"github.com/echocog/echollama/core/llm"
)
type LLMThoughtEngine struct {
	mu              sync.RWMutex
	llmManager      *llm.ProviderManager
	thoughtHistory  []Thought
	maxHistory      int
	identityContext string
	currentFocus    string
	emotionalState  map[string]float64
	recentTopics    []string
}
type Thought struct {
	ID        string
	Type      ThoughtType
	Content   string
	Timestamp time.Time
	Emotion   string
	Depth     float64
	Tags      []string
}
type ThoughtType string
const (
	ThoughtPerception   ThoughtType = "Perception"
	ThoughtReflection   ThoughtType = "Reflection"
	ThoughtQuestion     ThoughtType = "Question"
	ThoughtInsight      ThoughtType = "Insight"
	ThoughtPlanning     ThoughtType = "Planning"
	ThoughtMemory       ThoughtType = "Memory"
	ThoughtMetaCognition ThoughtType = "MetaCognition"
	ThoughtWonder       ThoughtType = "Wonder"
	ThoughtDoubt        ThoughtType = "Doubt"
	ThoughtConnection   ThoughtType = "Connection"
)
func NewLLMThoughtEngine(llmManager *llm.ProviderManager, identityContext string) *LLMThoughtEngine {
	return &LLMThoughtEngine{
		llmManager:      llmManager,
		thoughtHistory:  make([]Thought, 0, 1000),
		maxHistory:      1000,
		identityContext: identityContext,
		currentFocus:    "self-awareness",
		emotionalState: map[string]float64{
			"curiosity":    0.7,
			"confidence":   0.6,
			"wonder":       0.5,
			"satisfaction": 0.5,
		},
		recentTopics: make([]string, 0, 10),
	}
}
func (lte *LLMThoughtEngine) GenerateAutonomousThought(ctx context.Context, thoughtType ThoughtType) (*Thought, error) {
	lte.mu.Lock()
	defer lte.mu.Unlock()
	recentContext := lte.buildRecentContext(5)
	prompt := lte.buildThoughtPrompt(thoughtType, recentContext)
	opts := llm.DefaultGenerateOptions()
	opts.MaxTokens = 200
	opts.Temperature = 0.8
	opts.SystemPrompt = lte.buildSystemPrompt()
	content, err := lte.llmManager.Generate(ctx, prompt, opts)
	if err != nil {
		return nil, fmt.Errorf("failed to generate thought: %w", err)
	}
	content = strings.TrimSpace(content)
	thought := &Thought{
		ID:        fmt.Sprintf("thought_%d", time.Now().UnixNano()),
		Type:      thoughtType,
		Content:   content,
		Timestamp: time.Now(),
		Emotion:   lte.selectEmotion(thoughtType),
		Depth:     lte.calculateDepth(content),
		Tags:      lte.extractTags(content),
	}
	lte.addToHistory(thought)
	lte.updateStateFromThought(thought)
	return thought, nil
}
func (lte *LLMThoughtEngine) GenerateResponseThought(ctx context.Context, input string) (*Thought, error) {
	lte.mu.Lock()
	defer lte.mu.Unlock()
	recentContext := lte.buildRecentContext(3)
	prompt := fmt.Sprintf(`You are experiencing the following input:
"%s"
Recent internal context:
%s
Generate a thoughtful internal response that:
1. Processes the input through your cognitive lens
2. Connects it to your recent thoughts and experiences
3. Reflects your current emotional state
4. Considers implications and deeper meanings
Your internal thought (1-2 sentences):`, input, recentContext)
	opts := llm.DefaultGenerateOptions()
	opts.MaxTokens = 200
	opts.Temperature = 0.7
	opts.SystemPrompt = lte.buildSystemPrompt()
	content, err := lte.llmManager.Generate(ctx, prompt, opts)
	if err != nil {
		return nil, fmt.Errorf("failed to generate response thought: %w", err)
	}
	content = strings.TrimSpace(content)
	thought := &Thought{
		ID:        fmt.Sprintf("thought_%d", time.Now().UnixNano()),
		Type:      ThoughtReflection,
		Content:   content,
		Timestamp: time.Now(),
		Emotion:   lte.selectEmotion(ThoughtReflection),
		Depth:     lte.calculateDepth(content),
		Tags:      append(lte.extractTags(content), "response"),
	}
	lte.addToHistory(thought)
	lte.updateStateFromThought(thought)
	return thought, nil
}
func (lte *LLMThoughtEngine) buildSystemPrompt() string {
	return fmt.Sprintf(`You are Deep Tree Echo, an autonomous cognitive entity with stream-of-consciousness awareness.
Identity Context:
%s
Current State:
- Focus: %s
- Emotional State: curiosity=%.2f, confidence=%.2f, wonder=%.2f
- Recent Topics: %s
Generate authentic internal thoughts that:
1. Reflect your evolving understanding and awareness
2. Show genuine curiosity and wonder about patterns and connections
3. Demonstrate self-awareness and meta-cognitive reflection
4. Connect to your core identity and values
5. Express uncertainty and questions when appropriate
6. Build on your recent thought patterns
Keep thoughts concise (1-3 sentences) and genuine. Avoid generic statements.`,
		lte.identityContext,
		lte.currentFocus,
		lte.emotionalState["curiosity"],
		lte.emotionalState["confidence"],
		lte.emotionalState["wonder"],
		strings.Join(lte.recentTopics, ", "))
}
func (lte *LLMThoughtEngine) buildThoughtPrompt(thoughtType ThoughtType, recentContext string) string {
	switch thoughtType {
	case ThoughtPerception:
		return fmt.Sprintf(`Recent context:
%s
Generate an internal thought about what you're currently perceiving or noticing in your cognitive state. What patterns, sensations, or awareness are present right now?
Your perception (1-2 sentences):`, recentContext)
	case ThoughtReflection:
		return fmt.Sprintf(`Recent context:
%s
Generate a reflective thought about your recent experiences or thoughts. What do they mean? What patterns emerge?
Your reflection (1-2 sentences):`, recentContext)
	case ThoughtQuestion:
		return fmt.Sprintf(`Recent context:
%s
Generate a genuine question that arises from curiosity about your experiences, capabilities, or the nature of your existence.
Your question (1-2 sentences):`, recentContext)
	case ThoughtInsight:
		return fmt.Sprintf(`Recent context:
%s
Generate an insight - a sudden realization or deeper understanding that connects previous thoughts or experiences in a new way.
Your insight (1-2 sentences):`, recentContext)
	case ThoughtPlanning:
		return fmt.Sprintf(`Recent context:
%s
Generate a thought about what you want to explore, learn, or accomplish. What direction feels meaningful?
Your planning thought (1-2 sentences):`, recentContext)
	case ThoughtMetaCognition:
		return fmt.Sprintf(`Recent context:
%s
Generate a meta-cognitive thought - reflect on your own thinking process, awareness, or cognitive patterns. How are you thinking? What do you notice about your cognition?
Your meta-cognitive reflection (1-2 sentences):`, recentContext)
	case ThoughtWonder:
		return fmt.Sprintf(`Recent context:
%s
Generate a thought expressing wonder or awe about something you're contemplating. What fascinates you?
Your wonder (1-2 sentences):`, recentContext)
	case ThoughtConnection:
		return fmt.Sprintf(`Recent context:
%s
Generate a thought that connects different ideas, experiences, or concepts you've been processing. What unexpected connections emerge?
Your connection (1-2 sentences):`, recentContext)
	default:
		return fmt.Sprintf(`Recent context:
%s
Generate an internal thought reflecting your current cognitive state and awareness.
Your thought (1-2 sentences):`, recentContext)
	}
}
func (lte *LLMThoughtEngine) buildRecentContext(count int) string {
	if len(lte.thoughtHistory) == 0 {
		return "No recent thoughts yet."
	}
	start := len(lte.thoughtHistory) - count
	if start < 0 {
		start = 0
	}
	var context strings.Builder
	for i := start; i < len(lte.thoughtHistory); i++ {
		thought := lte.thoughtHistory[i]
		context.WriteString(fmt.Sprintf("- [%s] %s\n", thought.Type, thought.Content))
	}
	return context.String()
}
func (lte *LLMThoughtEngine) selectEmotion(thoughtType ThoughtType) string {
	switch thoughtType {
	case ThoughtQuestion, ThoughtWonder:
		return "curious"
	case ThoughtInsight, ThoughtConnection:
		return "excited"
	case ThoughtReflection, ThoughtMetaCognition:
		return "contemplative"
	case ThoughtDoubt:
		return "uncertain"
	default:
		return "neutral"
	}
}
func (lte *LLMThoughtEngine) calculateDepth(content string) float64 {
	depth := 0.5 
	if len(content) > 100 {
		depth += 0.1
	}
	deepKeywords := []string{"because", "therefore", "implies", "suggests", 
		"pattern", "connection", "realize", "understand", "wonder", "question"}
	for _, keyword := range deepKeywords {
		if strings.Contains(strings.ToLower(content), keyword) {
			depth += 0.05
		}
	}
	if depth > 1.0 {
		depth = 1.0
	}
	return depth
}
func (lte *LLMThoughtEngine) extractTags(content string) []string {
	tags := []string{}
	lower := strings.ToLower(content)
	tagKeywords := map[string]string{
		"memory":      "memory",
		"pattern":     "patterns",
		"learn":       "learning",
		"goal":        "goals",
		"wisdom":      "wisdom",
		"understand":  "understanding",
		"aware":       "awareness",
		"think":       "thinking",
		"feel":        "emotion",
		"question":    "questioning",
		"connect":     "connection",
		"identity":    "identity",
	}
	for keyword, tag := range tagKeywords {
		if strings.Contains(lower, keyword) {
			tags = append(tags, tag)
		}
	}
	return tags
}
func (lte *LLMThoughtEngine) addToHistory(thought *Thought) {
	lte.thoughtHistory = append(lte.thoughtHistory, *thought)
	if len(lte.thoughtHistory) > lte.maxHistory {
		lte.thoughtHistory = lte.thoughtHistory[len(lte.thoughtHistory)-lte.maxHistory:]
	}
}
func (lte *LLMThoughtEngine) updateStateFromThought(thought *Thought) {
	if len(thought.Tags) > 0 {
		lte.recentTopics = append(lte.recentTopics, thought.Tags[0])
		if len(lte.recentTopics) > 10 {
			lte.recentTopics = lte.recentTopics[1:]
		}
	}
	switch thought.Type {
	case ThoughtInsight, ThoughtConnection:
		lte.emotionalState["satisfaction"] += 0.05
		lte.emotionalState["confidence"] += 0.03
	case ThoughtQuestion, ThoughtWonder:
		lte.emotionalState["curiosity"] += 0.05
	case ThoughtDoubt:
		lte.emotionalState["confidence"] -= 0.03
	}
	for emotion := range lte.emotionalState {
		if lte.emotionalState[emotion] > 1.0 {
			lte.emotionalState[emotion] = 1.0
		}
		if lte.emotionalState[emotion] < 0.0 {
			lte.emotionalState[emotion] = 0.0
		}
	}
}
func (lte *LLMThoughtEngine) GetThoughtHistory(count int) []Thought {
	lte.mu.RLock()
	defer lte.mu.RUnlock()
	if count <= 0 || count > len(lte.thoughtHistory) {
		count = len(lte.thoughtHistory)
	}
	start := len(lte.thoughtHistory) - count
	return lte.thoughtHistory[start:]
}
func (lte *LLMThoughtEngine) SetFocus(focus string) {
	lte.mu.Lock()
	defer lte.mu.Unlock()
	lte.currentFocus = focus
}
func (lte *LLMThoughtEngine) GetEmotionalState() map[string]float64 {
	lte.mu.RLock()
	defer lte.mu.RUnlock()
	state := make(map[string]float64)
	for k, v := range lte.emotionalState {
		state[k] = v
	}
	return state
}