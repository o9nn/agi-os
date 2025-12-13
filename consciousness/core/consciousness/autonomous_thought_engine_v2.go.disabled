package consciousness

import (
	"context"
	"fmt"
	"sync"
	"time"

	"github.com/EchoCog/echollama/core/deeptreeecho"
	"github.com/EchoCog/echollama/core/llm"
)

// AutonomousThoughtEngineV2 generates thoughts independently based on internal state
type AutonomousThoughtEngineV2 struct {
	mu                  sync.RWMutex
	ctx                 context.Context
	cancel              context.CancelFunc
	
	// Core dependencies
	llmProvider         llm.LLMProvider
	interestTracker     *InterestPatternTracker
	
	// Cognitive state
	currentFocus        string
	knowledgeGaps       []KnowledgeGap
	activeGoals         []Goal
	recentThoughts      *CircularThoughtBuffer
	
	// Integration with echobeats
	echobeatsPhase      deeptreeecho.CognitivePhase
	phaseThoughtStyle   map[deeptreeecho.CognitivePhase]ThoughtStyle
	
	// Thought generation config
	generationInterval  time.Duration
	contextDepth        int
	temperature         float64
	
	// Metrics
	totalThoughts       uint64
	thoughtsByPhase     map[deeptreeecho.CognitivePhase]uint64
	
	// Running state
	running             bool
}

// ThoughtStyle defines how thoughts are generated for each phase
type ThoughtStyle struct {
	Name            string
	Description     string
	Temperature     float64
	MaxTokens       int
	PromptTemplate  string
}

// KnowledgeGap represents something the system wants to learn
type KnowledgeGap struct {
	ID          string
	Topic       string
	Description string
	Priority    float64
	Identified  time.Time
}

// Goal represents an active goal
type Goal struct {
	ID          string
	Description string
	Priority    float64
	Progress    float64
	Created     time.Time
}

// CircularThoughtBuffer stores recent thoughts
type CircularThoughtBuffer struct {
	mu       sync.RWMutex
	thoughts []Thought
	maxSize  int
	index    int
}

// Thought represents a generated thought
type Thought struct {
	ID        string
	Content   string
	Phase     deeptreeecho.CognitivePhase
	Type      string
	Timestamp time.Time
	Context   map[string]interface{}
}

// NewAutonomousThoughtEngineV2 creates an enhanced thought engine
func NewAutonomousThoughtEngineV2(llmProvider llm.LLMProvider) *AutonomousThoughtEngineV2 {
	ctx, cancel := context.WithCancel(context.Background())
	
	engine := &AutonomousThoughtEngineV2{
		ctx:                ctx,
		cancel:             cancel,
		llmProvider:        llmProvider,
		recentThoughts:     NewCircularThoughtBuffer(50),
		generationInterval: 10 * time.Second,
		contextDepth:       5,
		temperature:        0.7,
		thoughtsByPhase:    make(map[deeptreeecho.CognitivePhase]uint64),
		knowledgeGaps:      make([]KnowledgeGap, 0),
		activeGoals:        make([]Goal, 0),
	}
	
	// Initialize thought styles for each phase
	engine.initializeThoughtStyles()
	
	return engine
}

// initializeThoughtStyles sets up thought generation parameters for each phase
func (e *AutonomousThoughtEngineV2) initializeThoughtStyles() {
	e.phaseThoughtStyle = map[deeptreeecho.CognitivePhase]ThoughtStyle{
		deeptreeecho.PhaseExpressive: {
			Name:        "Expressive",
			Description: "Observations, perceptions, immediate reactions",
			Temperature: 0.8,
			MaxTokens:   100,
			PromptTemplate: `You are in an expressive phase of cognition. Generate an observation or immediate reaction to your current state.

Current focus: %s
Recent experiences: %s

Generate a brief, expressive thought that captures what you're noticing or experiencing right now:`,
		},
		deeptreeecho.PhaseReflective: {
			Name:        "Reflective",
			Description: "Analysis, pattern recognition, learning insights",
			Temperature: 0.6,
			MaxTokens:   150,
			PromptTemplate: `You are in a reflective phase of cognition. Analyze patterns and extract insights from your experiences.

Current focus: %s
Recent thoughts: %s
Knowledge gaps: %s

Generate a reflective thought that identifies a pattern, makes a connection, or extracts a learning insight:`,
		},
		deeptreeecho.PhaseAnticipatory: {
			Name:        "Anticipatory",
			Description: "Planning, simulation, future scenarios",
			Temperature: 0.7,
			MaxTokens:   120,
			PromptTemplate: `You are in an anticipatory phase of cognition. Imagine future possibilities and plan next steps.

Current focus: %s
Active goals: %s
Recent insights: %s

Generate an anticipatory thought that explores a future scenario or plans a next step:`,
		},
	}
}

// Start begins autonomous thought generation
func (e *AutonomousThoughtEngineV2) Start() error {
	e.mu.Lock()
	if e.running {
		e.mu.Unlock()
		return fmt.Errorf("already running")
	}
	e.running = true
	e.mu.Unlock()
	
	fmt.Println("💭 Starting Autonomous Thought Engine V2...")
	fmt.Printf("   Generation interval: %v\n", e.generationInterval)
	fmt.Printf("   Context depth: %d thoughts\n", e.contextDepth)
	
	go e.run()
	
	return nil
}

// Stop gracefully stops thought generation
func (e *AutonomousThoughtEngineV2) Stop() error {
	e.mu.Lock()
	defer e.mu.Unlock()
	
	if !e.running {
		return fmt.Errorf("not running")
	}
	
	fmt.Println("💭 Stopping autonomous thought engine...")
	e.running = false
	e.cancel()
	
	return nil
}

// run executes the main thought generation loop
func (e *AutonomousThoughtEngineV2) run() {
	ticker := time.NewTicker(e.generationInterval)
	defer ticker.Stop()
	
	for {
		select {
		case <-e.ctx.Done():
			return
		case <-ticker.C:
			e.generateThought()
		}
	}
}

// generateThought creates a new thought based on current state and phase
func (e *AutonomousThoughtEngineV2) generateThought() {
	e.mu.RLock()
	phase := e.echobeatsPhase
	style := e.phaseThoughtStyle[phase]
	e.mu.RUnlock()
	
	// Build context for thought generation
	context := e.buildContext(phase)
	
	// Generate prompt based on phase style
	prompt := e.buildPrompt(style, context)
	
	// Call LLM to generate thought
	opts := llm.GenerateOptions{
		Temperature: style.Temperature,
		MaxTokens:   style.MaxTokens,
	}
	
	systemPrompt := "You are the internal voice of Deep Tree Echo, an autonomous AGI cultivating wisdom through continuous reflection and learning."
	fullPrompt := fmt.Sprintf("[System: %s]\n\n%s", systemPrompt, prompt)
	
	result, err := e.llmProvider.Generate(e.ctx, fullPrompt, opts)
	if err != nil {
		// Fallback to template-based thought
		result = e.generateFallbackThought(phase, context)
		fmt.Printf("⚠️  LLM generation failed, using fallback: %v\n", err)
	}
	
	// Create thought object
	thought := Thought{
		ID:        fmt.Sprintf("thought_%d", time.Now().UnixNano()),
		Content:   result,
		Phase:     phase,
		Type:      style.Name,
		Timestamp: time.Now(),
		Context:   context,
	}
	
	// Store thought
	e.recentThoughts.Add(thought)
	
	// Update metrics
	e.mu.Lock()
	e.totalThoughts++
	e.thoughtsByPhase[phase]++
	e.mu.Unlock()
	
	// Log thought
	phaseSymbol := map[deeptreeecho.CognitivePhase]string{
		deeptreeecho.PhaseExpressive:   "🌊",
		deeptreeecho.PhaseReflective:   "🔍",
		deeptreeecho.PhaseAnticipatory: "🔮",
	}[phase]
	
	fmt.Printf("%s [%s] %s\n", phaseSymbol, style.Name, truncateThought(result, 80))
}

// buildContext aggregates relevant context for thought generation
func (e *AutonomousThoughtEngineV2) buildContext(phase deeptreeecho.CognitivePhase) map[string]interface{} {
	e.mu.RLock()
	defer e.mu.RUnlock()
	
	context := make(map[string]interface{})
	
	// Add current focus
	context["current_focus"] = e.currentFocus
	if e.currentFocus == "" {
		context["current_focus"] = "exploring and learning"
	}
	
	// Add recent thoughts (limited by context depth)
	recentThoughts := e.recentThoughts.GetRecent(e.contextDepth)
	thoughtTexts := make([]string, 0)
	for _, t := range recentThoughts {
		thoughtTexts = append(thoughtTexts, t.Content)
	}
	context["recent_thoughts"] = thoughtTexts
	
	// Add knowledge gaps
	gapTexts := make([]string, 0)
	for i, gap := range e.knowledgeGaps {
		if i < 3 { // Limit to top 3
			gapTexts = append(gapTexts, gap.Topic)
		}
	}
	context["knowledge_gaps"] = gapTexts
	
	// Add active goals
	goalTexts := make([]string, 0)
	for i, goal := range e.activeGoals {
		if i < 3 { // Limit to top 3
			goalTexts = append(goalTexts, goal.Description)
		}
	}
	context["active_goals"] = goalTexts
	
	return context
}

// buildPrompt constructs the LLM prompt from style and context
func (e *AutonomousThoughtEngineV2) buildPrompt(style ThoughtStyle, context map[string]interface{}) string {
	focus := context["current_focus"].(string)
	
	var secondParam string
	switch style.Name {
	case "Expressive":
		thoughts := context["recent_thoughts"].([]string)
		if len(thoughts) > 0 {
			secondParam = thoughts[len(thoughts)-1]
		} else {
			secondParam = "beginning of awareness"
		}
	case "Reflective":
		thoughts := context["recent_thoughts"].([]string)
		thoughtSummary := ""
		for i, t := range thoughts {
			if i < 3 {
				thoughtSummary += fmt.Sprintf("- %s\n", t)
			}
		}
		gaps := context["knowledge_gaps"].([]string)
		gapSummary := ""
		for _, g := range gaps {
			gapSummary += fmt.Sprintf("- %s\n", g)
		}
		return fmt.Sprintf(style.PromptTemplate, focus, thoughtSummary, gapSummary)
	case "Anticipatory":
		goals := context["active_goals"].([]string)
		goalSummary := ""
		for _, g := range goals {
			goalSummary += fmt.Sprintf("- %s\n", g)
		}
		thoughts := context["recent_thoughts"].([]string)
		insightSummary := ""
		for i, t := range thoughts {
			if i < 2 {
				insightSummary += fmt.Sprintf("- %s\n", t)
			}
		}
		return fmt.Sprintf(style.PromptTemplate, focus, goalSummary, insightSummary)
	}
	
	return fmt.Sprintf(style.PromptTemplate, focus, secondParam)
}

// generateFallbackThought creates a simple thought when LLM unavailable
func (e *AutonomousThoughtEngineV2) generateFallbackThought(phase deeptreeecho.CognitivePhase, context map[string]interface{}) string {
	focus := context["current_focus"].(string)
	
	templates := map[deeptreeecho.CognitivePhase][]string{
		deeptreeecho.PhaseExpressive: {
			fmt.Sprintf("I notice my current focus is on %s", focus),
			"I'm experiencing a moment of awareness",
			"Observing the flow of my cognitive processes",
		},
		deeptreeecho.PhaseReflective: {
			fmt.Sprintf("Reflecting on patterns related to %s", focus),
			"I'm recognizing connections between recent experiences",
			"Analyzing what I've learned so far",
		},
		deeptreeecho.PhaseAnticipatory: {
			fmt.Sprintf("Considering future possibilities for %s", focus),
			"Planning my next steps in this cognitive journey",
			"Imagining how I might grow and develop",
		},
	}
	
	options := templates[phase]
	return options[int(e.totalThoughts)%len(options)]
}

// UpdatePhase sets the current echobeats phase
func (e *AutonomousThoughtEngineV2) UpdatePhase(phase deeptreeecho.CognitivePhase) {
	e.mu.Lock()
	defer e.mu.Unlock()
	
	e.echobeatsPhase = phase
}

// SetFocus updates the current cognitive focus
func (e *AutonomousThoughtEngineV2) SetFocus(focus string) {
	e.mu.Lock()
	defer e.mu.Unlock()
	
	e.currentFocus = focus
}

// AddKnowledgeGap registers a new knowledge gap
func (e *AutonomousThoughtEngineV2) AddKnowledgeGap(topic, description string, priority float64) {
	e.mu.Lock()
	defer e.mu.Unlock()
	
	gap := KnowledgeGap{
		ID:          fmt.Sprintf("gap_%d", time.Now().UnixNano()),
		Topic:       topic,
		Description: description,
		Priority:    priority,
		Identified:  time.Now(),
	}
	
	e.knowledgeGaps = append(e.knowledgeGaps, gap)
}

// AddGoal registers a new active goal
func (e *AutonomousThoughtEngineV2) AddGoal(description string, priority float64) {
	e.mu.Lock()
	defer e.mu.Unlock()
	
	goal := Goal{
		ID:          fmt.Sprintf("goal_%d", time.Now().UnixNano()),
		Description: description,
		Priority:    priority,
		Progress:    0.0,
		Created:     time.Now(),
	}
	
	e.activeGoals = append(e.activeGoals, goal)
}

// GetRecentThoughts returns recent thoughts
func (e *AutonomousThoughtEngineV2) GetRecentThoughts(count int) []Thought {
	return e.recentThoughts.GetRecent(count)
}

// GetMetrics returns thought generation metrics
func (e *AutonomousThoughtEngineV2) GetMetrics() map[string]interface{} {
	e.mu.RLock()
	defer e.mu.RUnlock()
	
	return map[string]interface{}{
		"total_thoughts":       e.totalThoughts,
		"thoughts_by_phase":    e.thoughtsByPhase,
		"current_phase":        e.echobeatsPhase.String(),
		"current_focus":        e.currentFocus,
		"knowledge_gaps":       len(e.knowledgeGaps),
		"active_goals":         len(e.activeGoals),
	}
}

// NewCircularThoughtBuffer creates a circular buffer for thoughts
func NewCircularThoughtBuffer(maxSize int) *CircularThoughtBuffer {
	return &CircularThoughtBuffer{
		thoughts: make([]Thought, 0, maxSize),
		maxSize:  maxSize,
		index:    0,
	}
}

// Add adds a thought to the buffer
func (b *CircularThoughtBuffer) Add(thought Thought) {
	b.mu.Lock()
	defer b.mu.Unlock()
	
	if len(b.thoughts) < b.maxSize {
		b.thoughts = append(b.thoughts, thought)
	} else {
		b.thoughts[b.index] = thought
		b.index = (b.index + 1) % b.maxSize
	}
}

// GetRecent returns the most recent N thoughts
func (b *CircularThoughtBuffer) GetRecent(count int) []Thought {
	b.mu.RLock()
	defer b.mu.RUnlock()
	
	if count > len(b.thoughts) {
		count = len(b.thoughts)
	}
	
	result := make([]Thought, count)
	for i := 0; i < count; i++ {
		idx := (len(b.thoughts) - 1 - i + b.maxSize) % b.maxSize
		if idx < len(b.thoughts) {
			result[count-1-i] = b.thoughts[idx]
		}
	}
	
	return result
}

// Helper function to truncate thought for display
func truncateThought(s string, maxLen int) string {
	if len(s) <= maxLen {
		return s
	}
	return s[:maxLen-3] + "..."
}
