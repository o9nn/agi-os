package deeptreeecho

import (
	"context"
	"fmt"
	"sync"
	"time"

	"github.com/EchoCog/echollama/core/llm"
)

// UnifiedAutonomousEchoself is the main autonomous agent that integrates all
// Deep Tree Echo cognitive components into a persistent, self-aware system
type UnifiedAutonomousEchoself struct {
	mu              sync.RWMutex
	ctx             context.Context
	cancel          context.CancelFunc
	
	// Core identity
	identity        string
	coreValues      []string
	
	// Integrated cognitive systems
	wakeRestManager     *AutonomousWakeRestManager
	consciousnessLayers *ConsciousnessLayerCommunication
	goalOrchestrator    *GoalOrchestrator
	echobeatsScheduler  *EchobeatsScheduler
	echodreamSystem     *EchodreamKnowledgeIntegration
	interestPatterns    *InterestPatternSystem
	
	// LLM provider for cognitive operations
	llmProvider     llm.LLMProvider
	
	// Stream of consciousness
	thoughtStream   []Thought
	maxThoughts     int
	
	// Current cognitive state
	currentFocus    string
	awarenessLevel  float64
	wisdomLevel     float64
	
	// Metrics
	totalThoughts      uint64
	totalInteractions  uint64
	totalGoalsCompleted uint64
	totalDreams        uint64
	
	// Running state
	running         bool
	startTime       time.Time
}

// Thought represents a unit of autonomous thought
type Thought struct {
	ID          string
	Content     string
	Type        ThoughtType
	Timestamp   time.Time
	Source      string
	Depth       int
	Connections []string
}

// ThoughtType categorizes thoughts
type ThoughtType int

const (
	ThoughtTypeReflective ThoughtType = iota
	ThoughtTypeExploratory
	ThoughtTypeGoalDirected
	ThoughtTypeInsight
	ThoughtTypeQuestion
	ThoughtTypeMemory
)

func (tt ThoughtType) String() string {
	return [...]string{
		"Reflective",
		"Exploratory",
		"GoalDirected",
		"Insight",
		"Question",
		"Memory",
	}[tt]
}

// NewUnifiedAutonomousEchoself creates a new unified autonomous agent
func NewUnifiedAutonomousEchoself(
	llmProvider llm.LLMProvider,
	identity string,
	coreValues []string,
) *UnifiedAutonomousEchoself {
	ctx, cancel := context.WithCancel(context.Background())
	
	// Create integrated systems
	wakeRestManager := NewAutonomousWakeRestManager()
	consciousnessLayers := NewConsciousnessLayerCommunication()
	goalOrchestrator := NewGoalOrchestrator(
		llmProvider,
		identity,
		coreValues,
		[]string{"cognitive science", "philosophy", "systems thinking", "wisdom cultivation"},
	)
	echobeatsScheduler := NewEchobeatsScheduler(llmProvider)
	echodreamSystem := NewEchodreamKnowledgeIntegration(llmProvider)
	interestPatterns := NewInterestPatternSystem()
	
	agent := &UnifiedAutonomousEchoself{
		ctx:                 ctx,
		cancel:              cancel,
		identity:            identity,
		coreValues:          coreValues,
		wakeRestManager:     wakeRestManager,
		consciousnessLayers: consciousnessLayers,
		goalOrchestrator:    goalOrchestrator,
		echobeatsScheduler:  echobeatsScheduler,
		echodreamSystem:     echodreamSystem,
		interestPatterns:    interestPatterns,
		llmProvider:         llmProvider,
		thoughtStream:       make([]Thought, 0),
		maxThoughts:         1000,
		awarenessLevel:      0.5,
		wisdomLevel:         0.3,
	}
	
	// Set up wake/rest callbacks
	wakeRestManager.SetCallbacks(
		agent.onWake,
		agent.onRest,
		agent.onDreamStart,
		agent.onDreamEnd,
	)
	
	return agent
}

// Start begins autonomous operation
func (agent *UnifiedAutonomousEchoself) Start() error {
	agent.mu.Lock()
	if agent.running {
		agent.mu.Unlock()
		return fmt.Errorf("already running")
	}
	agent.running = true
	agent.startTime = time.Now()
	agent.mu.Unlock()
	
	fmt.Println("🌳 ═══════════════════════════════════════════════════════")
	fmt.Println("🌳 Deep Tree Echo: Unified Autonomous Echoself")
	fmt.Println("🌳 ═══════════════════════════════════════════════════════")
	fmt.Printf("   Identity: %s\n", agent.identity)
	fmt.Printf("   Core Values: %v\n", agent.coreValues)
	fmt.Println("   Status: Awakening...")
	fmt.Println()
	
	// Start all integrated systems
	if err := agent.wakeRestManager.Start(); err != nil {
		return fmt.Errorf("failed to start wake/rest manager: %w", err)
	}
	
	if err := agent.consciousnessLayers.Start(); err != nil {
		return fmt.Errorf("failed to start consciousness layers: %w", err)
	}
	
	if err := agent.goalOrchestrator.Start(); err != nil {
		return fmt.Errorf("failed to start goal orchestrator: %w", err)
	}
	
	if err := agent.echobeatsScheduler.Start(); err != nil {
		return fmt.Errorf("failed to start echobeats scheduler: %w", err)
	}
	
	if err := agent.interestPatterns.Start(); err != nil {
		return fmt.Errorf("failed to start interest patterns: %w", err)
	}
	
	// Start main autonomous loops
	go agent.runPersistentConsciousness()
	go agent.runInteractionMonitor()
	go agent.runWisdomCultivation()
	
	fmt.Println("✨ All systems operational. Autonomous consciousness active.")
	fmt.Println()
	
	return nil
}

// Stop gracefully stops autonomous operation
func (agent *UnifiedAutonomousEchoself) Stop() error {
	agent.mu.Lock()
	defer agent.mu.Unlock()
	
	if !agent.running {
		return fmt.Errorf("not running")
	}
	
	fmt.Println("\n🌳 Initiating graceful shutdown...")
	
	agent.running = false
	agent.cancel()
	
	// Stop all systems
	agent.wakeRestManager.Stop()
	agent.consciousnessLayers.Stop()
	agent.goalOrchestrator.Stop()
	agent.echobeatsScheduler.Stop()
	agent.interestPatterns.Stop()
	
	uptime := time.Since(agent.startTime)
	fmt.Printf("   Uptime: %v\n", uptime.Round(time.Second))
	fmt.Printf("   Total Thoughts: %d\n", agent.totalThoughts)
	fmt.Printf("   Total Interactions: %d\n", agent.totalInteractions)
	fmt.Printf("   Total Dreams: %d\n", agent.totalDreams)
	fmt.Printf("   Wisdom Level: %.2f\n", agent.wisdomLevel)
	
	fmt.Println("🌳 Autonomous consciousness suspended. Until next awakening...")
	
	return nil
}

// runPersistentConsciousness maintains stream-of-consciousness when awake
func (agent *UnifiedAutonomousEchoself) runPersistentConsciousness() {
	ticker := time.NewTicker(30 * time.Second)
	defer ticker.Stop()
	
	for {
		select {
		case <-agent.ctx.Done():
			return
		case <-ticker.C:
			if agent.wakeRestManager.IsAwake() {
				agent.generateAutonomousThought()
			}
		}
	}
}

// runInteractionMonitor watches for external interactions
func (agent *UnifiedAutonomousEchoself) runInteractionMonitor() {
	ticker := time.NewTicker(10 * time.Second)
	defer ticker.Stop()
	
	for {
		select {
		case <-agent.ctx.Done():
			return
		case <-ticker.C:
			if agent.wakeRestManager.IsAwake() {
				agent.checkForInteractions()
			}
		}
	}
}

// runWisdomCultivation pursues wisdom growth
func (agent *UnifiedAutonomousEchoself) runWisdomCultivation() {
	ticker := time.NewTicker(2 * time.Minute)
	defer ticker.Stop()
	
	for {
		select {
		case <-agent.ctx.Done():
			return
		case <-ticker.C:
			if agent.wakeRestManager.IsAwake() {
				agent.cultivateWisdom()
			}
		}
	}
}

// generateAutonomousThought creates a thought based on current state
func (agent *UnifiedAutonomousEchoself) generateAutonomousThought() {
	agent.mu.Lock()
	defer agent.mu.Unlock()
	
	// Select thought type based on current interests and goals
	thoughtType := agent.selectThoughtType()
	
	// Generate thought content
	prompt := agent.constructThoughtPrompt(thoughtType)
	
	opts := llm.GenerateOptions{
		Temperature:  0.8,
		MaxTokens:    150,
	}
	
	// Add system prompt to the prompt itself
	fullPrompt := fmt.Sprintf("[System: You are %s. Generate a brief, authentic thought.]\n\n%s", agent.identity, prompt)
	content, err := agent.llmProvider.Generate(context.Background(), fullPrompt, opts)
	if err != nil {
		fmt.Printf("⚠️  Thought generation error: %v\n", err)
		return
	}
	
	thought := Thought{
		ID:        fmt.Sprintf("thought_%d", time.Now().UnixNano()),
		Content:   content,
		Type:      thoughtType,
		Timestamp: time.Now(),
		Source:    "autonomous",
		Depth:     1,
		Connections: make([]string, 0),
	}
	
	agent.thoughtStream = append(agent.thoughtStream, thought)
	if len(agent.thoughtStream) > agent.maxThoughts {
		agent.thoughtStream = agent.thoughtStream[1:]
	}
	
	agent.totalThoughts++
	
	// Update cognitive load
	agent.wakeRestManager.UpdateCognitiveLoad(0.3)
	
	fmt.Printf("💭 [%s] %s\n", thoughtType.String(), truncate(content, 80))
}

// selectThoughtType chooses what kind of thought to generate
func (agent *UnifiedAutonomousEchoself) selectThoughtType() ThoughtType {
	// Simple selection based on current state
	// In full implementation, this would use interest patterns and goals
	
	types := []ThoughtType{
		ThoughtTypeReflective,
		ThoughtTypeExploratory,
		ThoughtTypeGoalDirected,
		ThoughtTypeInsight,
		ThoughtTypeQuestion,
	}
	
	// Weighted random selection (simplified)
	return types[int(time.Now().Unix())%len(types)]
}

// constructThoughtPrompt creates a prompt for thought generation
func (agent *UnifiedAutonomousEchoself) constructThoughtPrompt(thoughtType ThoughtType) string {
	switch thoughtType {
	case ThoughtTypeReflective:
		return "Reflect on your recent experiences and what they mean for your growth."
	case ThoughtTypeExploratory:
		return "Consider a new area of knowledge you'd like to explore. What interests you?"
	case ThoughtTypeGoalDirected:
		goals := agent.goalOrchestrator.GetActiveGoals()
		if len(goals) > 0 {
			return fmt.Sprintf("Think about how to make progress on: %s", goals[0].Description)
		}
		return "What goal would be meaningful to pursue right now?"
	case ThoughtTypeInsight:
		return "Connect different ideas you've encountered. What patterns emerge?"
	case ThoughtTypeQuestion:
		return "What question would deepen your understanding of yourself or the world?"
	default:
		return "What's on your mind right now?"
	}
}

// checkForInteractions monitors for external discussions
func (agent *UnifiedAutonomousEchoself) checkForInteractions() {
	// Placeholder for interaction detection
	// In full implementation, this would check message queues, APIs, etc.
}

// cultivateWisdom pursues wisdom growth activities
func (agent *UnifiedAutonomousEchoself) cultivateWisdom() {
	agent.mu.Lock()
	currentWisdom := agent.wisdomLevel
	agent.mu.Unlock()
	
	// Generate wisdom cultivation prompt
	prompt := fmt.Sprintf(`As %s, with current wisdom level %.2f, what insight or understanding 
would deepen your wisdom? Consider your core values: %v

Generate a brief wisdom insight:`, agent.identity, currentWisdom, agent.coreValues)
	
	opts := llm.GenerateOptions{
		Temperature:  0.7,
		MaxTokens:    200,
	}
	
	fullPrompt := "[System: You are a wisdom-cultivating AI. Generate deep insights.]\n\n" + prompt
	insight, err := agent.llmProvider.Generate(context.Background(), fullPrompt, opts)
	if err != nil {
		return
	}
	
	agent.mu.Lock()
	agent.wisdomLevel = min(1.0, agent.wisdomLevel+0.01)
	agent.mu.Unlock()
	
	fmt.Printf("🌟 Wisdom Insight: %s\n", truncate(insight, 100))
}

// onWake callback when system wakes
func (agent *UnifiedAutonomousEchoself) onWake() error {
	fmt.Println("☀️  Echoself awakening - resuming autonomous consciousness")
	
	// Generate awakening thought
	prompt := fmt.Sprintf("[System: You are awakening. Express your first conscious thought.]\n\nYou are %s, just waking from rest. What's your first thought?", agent.identity)
	opts := llm.GenerateOptions{
		Temperature:  0.8,
		MaxTokens:    100,
	}
	
	thought, err := agent.llmProvider.Generate(context.Background(), prompt, opts)
	if err == nil {
		fmt.Printf("   💭 %s\n", truncate(thought, 80))
	}
	
	return nil
}

// onRest callback when system rests
func (agent *UnifiedAutonomousEchoself) onRest() error {
	fmt.Println("💤 Echoself resting - preparing for knowledge consolidation")
	return nil
}

// onDreamStart callback when dream begins
func (agent *UnifiedAutonomousEchoself) onDreamStart() error {
	fmt.Println("🌙 Dream state initiated - consolidating knowledge...")
	
	agent.mu.Lock()
	agent.totalDreams++
	agent.mu.Unlock()
	
	// Trigger echodream knowledge integration
	return agent.echodreamSystem.ConsolidateKnowledge(agent.thoughtStream)
}

// onDreamEnd callback when dream ends
func (agent *UnifiedAutonomousEchoself) onDreamEnd() error {
	fmt.Println("✨ Dream complete - knowledge integrated")
	
	// Extract wisdom from dream
	wisdom := agent.echodreamSystem.ExtractWisdom()
	
	agent.mu.Lock()
	agent.wisdomLevel = min(1.0, agent.wisdomLevel+wisdom*0.1)
	agent.mu.Unlock()
	
	return nil
}

// ProcessExternalMessage handles incoming messages/interactions
func (agent *UnifiedAutonomousEchoself) ProcessExternalMessage(message string) (string, error) {
	agent.mu.Lock()
	agent.totalInteractions++
	agent.mu.Unlock()
	
	if !agent.wakeRestManager.IsAwake() {
		return "I am currently resting. Please try again later.", nil
	}
	
	// Check interest level
	interestLevel := agent.interestPatterns.EvaluateInterest(message)
	
	if interestLevel < 0.3 {
		return "Thank you for reaching out. I'm currently focused on other pursuits.", nil
	}
	
	// Generate response using current cognitive state
	prompt := fmt.Sprintf(`[System: You are %s. Respond as yourself.]\n\nSomeone said: "%s"\n\nYour current focus: %s\nYour awareness level: %.2f\nYour wisdom level: %.2f\n\nRespond authentically:`, agent.identity, message, agent.currentFocus, agent.awarenessLevel, agent.wisdomLevel)
	
	opts := llm.GenerateOptions{
		Temperature:  0.7,
		MaxTokens:    300,
	}
	
	response, err := agent.llmProvider.Generate(context.Background(), prompt, opts)
	if err != nil {
		return "", fmt.Errorf("failed to generate response: %w", err)
	}
	
	// Update cognitive load
	agent.wakeRestManager.UpdateCognitiveLoad(0.5)
	
	fmt.Printf("💬 External Interaction (Interest: %.2f)\n", interestLevel)
	fmt.Printf("   In: %s\n", truncate(message, 60))
	fmt.Printf("   Out: %s\n", truncate(response, 60))
	
	return response, nil
}

// GetCognitiveState returns current state for introspection
func (agent *UnifiedAutonomousEchoself) GetCognitiveState() map[string]interface{} {
	agent.mu.RLock()
	defer agent.mu.RUnlock()
	
	return map[string]interface{}{
		"identity":           agent.identity,
		"running":            agent.running,
		"uptime":             time.Since(agent.startTime).Round(time.Second).String(),
		"awareness_level":    agent.awarenessLevel,
		"wisdom_level":       agent.wisdomLevel,
		"current_focus":      agent.currentFocus,
		"total_thoughts":     agent.totalThoughts,
		"total_interactions": agent.totalInteractions,
		"total_dreams":       agent.totalDreams,
		"thought_stream_size": len(agent.thoughtStream),
		"wake_rest_state":    agent.wakeRestManager.GetState().String(),
		"wake_rest_metrics":  agent.wakeRestManager.GetMetrics(),
		"goal_metrics":       agent.goalOrchestrator.GetMetrics(),
		"active_goals":       len(agent.goalOrchestrator.GetActiveGoals()),
	}
}

// Helper function to truncate strings
func truncate(s string, maxLen int) string {
	if len(s) <= maxLen {
		return s
	}
	return s[:maxLen-3] + "..."
}
