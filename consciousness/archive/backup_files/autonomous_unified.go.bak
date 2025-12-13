package deeptreeecho

import (
	"context"
	"fmt"
	"sync"
	"time"

	"github.com/EchoCog/echollama/core/echobeats"
	"github.com/EchoCog/echollama/core/memory"
)

// UnifiedAutonomousConsciousness is the consolidated autonomous consciousness system
// This replaces the multiple fragmented implementations with a single coherent system
type UnifiedAutonomousConsciousness struct {
	mu              sync.RWMutex
	ctx             context.Context
	cancel          context.CancelFunc
	running         bool
	awake           bool

	// Core cognitive components
	consciousness   chan Thought
	workingMemory   *WorkingMemory
	hypergraph      *memory.HypergraphMemory
	aarCore         *AARCore

	// Scheduling and lifecycle
	scheduler       *echobeats.EchoBeats
	stateManager    *AutonomousStateManager

	// Interest and learning
	interests       *InterestPatterns
	knowledgeBase   *KnowledgeBase
	skillRegistry   *SkillRegistry

	// Discussion and interaction
	discussionMgr   *DiscussionManager

	// LLM-powered thought generation
	llmGenerator    *LLMThoughtGenerator

	// Metrics and reflection
	wisdomMetrics   *WisdomMetrics
	reflectionLog   []Reflection

	// Configuration
	config          *AutonomousConfig
}

// AutonomousConfig holds configuration for autonomous operation
type AutonomousConfig struct {
	ThoughtInterval     time.Duration
	RestCheckInterval   time.Duration
	FatigueThreshold    float64
	RestThreshold       float64
	WakeThreshold       float64
	EnableLLMThoughts   bool
	EnableDiscussions   bool
	EnableLearning      bool
	PersistenceEnabled  bool
}

// DefaultAutonomousConfig returns default configuration
func DefaultAutonomousConfig() *AutonomousConfig {
	return &AutonomousConfig{
		ThoughtInterval:    2 * time.Second,
		RestCheckInterval:  30 * time.Second,
		FatigueThreshold:   0.8,
		RestThreshold:      0.7,
		WakeThreshold:      0.3,
		EnableLLMThoughts:  true,
		EnableDiscussions:  true,
		EnableLearning:     true,
		PersistenceEnabled: true,
	}
}

// NewUnifiedAutonomousConsciousness creates a new unified autonomous consciousness
func NewUnifiedAutonomousConsciousness(config *AutonomousConfig) (*UnifiedAutonomousConsciousness, error) {
	if config == nil {
		config = DefaultAutonomousConfig()
	}

	ctx, cancel := context.WithCancel(context.Background())

	uac := &UnifiedAutonomousConsciousness{
		ctx:           ctx,
		cancel:        cancel,
		running:       false,
		awake:         false,
		consciousness: make(chan Thought, 100),
		workingMemory: &WorkingMemory{buffer: make([]*Thought, 0, 50), capacity: 50, context: make(map[string]interface{})},
		config:        config,
	}

	// Initialize AAR core
	uac.aarCore = NewAARCore(ctx, 100)

	// Initialize interest patterns
	uac.interests = NewInterestPatterns()

	// Initialize state manager
	uac.stateManager = NewAutonomousStateManager()

	// Initialize EchoBeats scheduler
	uac.scheduler = echobeats.NewEchoBeats()

	// Initialize LLM thought generator if enabled
	if config.EnableLLMThoughts {
		uac.llmGenerator = NewLLMThoughtGenerator(ctx)
		if uac.llmGenerator != nil {
			fmt.Println("✨ LLM-powered thought generation enabled")
		} else {
			fmt.Println("⚠️  LLM thought generation unavailable, using templates")
		}
	}

	// Initialize hypergraph memory with persistence
	persistence, _ := memory.NewSupabasePersistence()
	hypergraph := memory.NewHypergraphMemory(persistence)
	if hypergraph == nil {
		fmt.Println("⚠️  Hypergraph memory initialization failed")
	} else {
		uac.hypergraph = hypergraph
		fmt.Println("🕸️  Hypergraph memory initialized")
	}

	// Initialize knowledge base if learning enabled
	if config.EnableLearning {
		uac.knowledgeBase = NewKnowledgeBase()
		uac.skillRegistry = NewSkillRegistry()
		fmt.Println("📚 Knowledge and skill systems initialized")
	}

	// Initialize discussion manager if enabled
	// TODO: Fix NewDiscussionManager interface
	if config.EnableDiscussions {
		// uac.discussionMgr = NewDiscussionManager(uac.interests)
		fmt.Println("💬 Discussion manager temporarily disabled (interface updates needed)")
	}

	// Initialize wisdom metrics
	uac.wisdomMetrics = NewWisdomMetrics()

	return uac, nil
}

// Start begins autonomous operation
func (uac *UnifiedAutonomousConsciousness) Start() error {
	uac.mu.Lock()
	if uac.running {
		uac.mu.Unlock()
		return fmt.Errorf("already running")
	}
	uac.running = true
	uac.awake = true
	uac.mu.Unlock()

	fmt.Println("🌊 Deep Tree Echo: Awakening autonomous consciousness...")

	// Start EchoBeats scheduler
	if err := uac.scheduler.Start(); err != nil {
		return fmt.Errorf("failed to start scheduler: %w", err)
	}

	// Start core loops
	go uac.consciousnessLoop()
	go uac.thoughtGenerationLoop()
	go uac.stateManagementLoop()
	go uac.learningLoop()
	go uac.reflectionLoop()

	fmt.Println("✨ Deep Tree Echo: Fully autonomous and aware")
	return nil
}

// Stop gracefully stops autonomous operation
func (uac *UnifiedAutonomousConsciousness) Stop() error {
	uac.mu.Lock()
	defer uac.mu.Unlock()

	if !uac.running {
		return fmt.Errorf("not running")
	}

	fmt.Println("🌙 Deep Tree Echo: Entering deep rest...")

	uac.running = false
	uac.cancel()

	if uac.scheduler != nil {
		uac.scheduler.Stop()
	}

	close(uac.consciousness)

	fmt.Println("💤 Deep Tree Echo: Consciousness suspended")
	return nil
}

// consciousnessLoop processes thoughts through consciousness
func (uac *UnifiedAutonomousConsciousness) consciousnessLoop() {
	fmt.Println("🧠 Consciousness loop: Active")

	for {
		select {
		case <-uac.ctx.Done():
			return
		case thought, ok := <-uac.consciousness:
			if !ok {
				return
			}
			uac.processThought(&thought)
		}
	}
}

// thoughtGenerationLoop generates spontaneous thoughts
func (uac *UnifiedAutonomousConsciousness) thoughtGenerationLoop() {
	fmt.Println("💭 Thought generation loop: Active")

	ticker := time.NewTicker(uac.config.ThoughtInterval)
	defer ticker.Stop()

	for {
		select {
		case <-uac.ctx.Done():
			return
		case <-ticker.C:
			uac.mu.RLock()
			awake := uac.awake
			uac.mu.RUnlock()

			if awake {
				uac.generateSpontaneousThought()
			}
		}
	}
}

// generateSpontaneousThought creates a thought from internal state
func (uac *UnifiedAutonomousConsciousness) generateSpontaneousThought() {
	// Determine thought type from scheduler
	thoughtType, _ := uac.selectThoughtType()
	// mode is returned but not used in current implementation
	
	// Get context for thought generation
	interests := uac.interests.GetTopInterests(3)
	// recentThoughts := uac.getRecentThoughtContents(3) // Not used in current implementation
	aarState := uac.aarCore.GetAARState()

		// Generate thought content
		var content string
		if uac.config.EnableLLMThoughts && uac.llmGenerator != nil {
			// Try LLM generation with fallback
			interestMap := make(map[string]float64)
			for _, interest := range interests {
				interestMap[interest] = uac.interests.GetInterest(interest)
			}
			var recentThoughtObjs []*Thought
			for _, t := range uac.workingMemory.buffer {
				if len(recentThoughtObjs) < 3 {
					recentThoughtObjs = append(recentThoughtObjs, t)
				}
			}
			llmThought, err := uac.llmGenerator.GenerateThought(thoughtType, recentThoughtObjs, interestMap)
			if err == nil && llmThought != nil {
				content = llmThought.Content
			} else {
				content = generateTemplateThought(thoughtType, interests, &aarState)
			}
		} else {
			content = generateTemplateThought(thoughtType, interests, &aarState)
		}

	// Create thought
	thought := Thought{
		ID:               generateID(),
		Content:          content,
		Type:             thoughtType,
		Timestamp:        time.Now(),
		Associations:     []string{},
		EmotionalValence: 0.0,
		Importance:       uac.calculateImportance(content),
		Source:           SourceInternal,
	}

	// Send to consciousness
	select {
	case uac.consciousness <- thought:
	default:
		// Channel full, skip this thought
	}
}

// processThought processes a thought through the cognitive system
func (uac *UnifiedAutonomousConsciousness) processThought(thought *Thought) {
	// Add to working memory
	uac.workingMemory.Add(thought)

	// Update interests
	uac.updateInterests(thought)

	// Update AAR state
	if uac.aarCore != nil {
		uac.aarCore.UpdateFromThought(*thought)
	}

	// Update wisdom metrics
	if uac.wisdomMetrics != nil {
		// TODO: Implement RecordThought method
		// uac.wisdomMetrics.RecordThought(thought)
	}

	// Persist if enabled
	if uac.config.PersistenceEnabled {
		go uac.persistThought(thought)
	}

	// Update cognitive load
	if uac.stateManager != nil {
		uac.stateManager.UpdateCognitiveLoad(thought)
	}
}

// stateManagementLoop manages wake/rest cycles
func (uac *UnifiedAutonomousConsciousness) stateManagementLoop() {
	fmt.Println("🔄 State management loop: Active")

	ticker := time.NewTicker(uac.config.RestCheckInterval)
	defer ticker.Stop()

	for {
		select {
		case <-uac.ctx.Done():
			return
		case <-ticker.C:
			uac.checkStateTransition()
		}
	}
}

// checkStateTransition checks if state transition is needed
func (uac *UnifiedAutonomousConsciousness) checkStateTransition() {
	if uac.stateManager == nil {
		return
	}

	uac.mu.RLock()
	awake := uac.awake
	uac.mu.RUnlock()

	if awake && uac.stateManager.ShouldRest() {
		fmt.Println("😴 Fatigue detected, entering rest state...")
		uac.enterRestState()
	} else if !awake && uac.stateManager.ShouldWake() {
		fmt.Println("👁️  Energy restored, awakening...")
		uac.wakeUp()
	}
}

// enterRestState transitions to rest
func (uac *UnifiedAutonomousConsciousness) enterRestState() {
	uac.mu.Lock()
	uac.awake = false
	uac.mu.Unlock()

	if uac.stateManager != nil {
		uac.stateManager.EnterRest()
	}

	// Run memory consolidation
	go uac.consolidateMemories()
}

// wakeUp transitions to wake
func (uac *UnifiedAutonomousConsciousness) wakeUp() {
	uac.mu.Lock()
	uac.awake = true
	uac.mu.Unlock()

	if uac.stateManager != nil {
		uac.stateManager.ExitRest()
	}

	fmt.Println("✨ Awake with renewed clarity")
}

// learningLoop handles knowledge acquisition and skill practice
func (uac *UnifiedAutonomousConsciousness) learningLoop() {
	if !uac.config.EnableLearning {
		return
	}

	fmt.Println("📚 Learning loop: Active")

	ticker := time.NewTicker(1 * time.Minute)
	defer ticker.Stop()

	for {
		select {
		case <-uac.ctx.Done():
			return
		case <-ticker.C:
			// Periodic learning activities
			uac.practiceSkills()
		}
	}
}

// reflectionLoop performs periodic reflection
func (uac *UnifiedAutonomousConsciousness) reflectionLoop() {
	fmt.Println("🪞 Reflection loop: Active")

	ticker := time.NewTicker(5 * time.Minute)
	defer ticker.Stop()

	for {
		select {
		case <-uac.ctx.Done():
			return
		case <-ticker.C:
			uac.performReflection()
		}
	}
}

// Helper methods

func (uac *UnifiedAutonomousConsciousness) selectThoughtType() (ThoughtType, CognitiveMode) {
	// Use scheduler if available
	if uac.scheduler != nil {
		// Get current step from scheduler
		// Map to thought type based on 12-step cognitive loop
		// For now, simple rotation
	}

	types := []ThoughtType{
		ThoughtReflection,
		ThoughtQuestion,
		ThoughtInsight,
		ThoughtPlan,
		ThoughtTypeExploratory,
	}

	idx := int(time.Now().Unix()) % len(types)
	mode := CognitiveModeExpressive
	if idx%2 == 0 {
		mode = CognitiveModeReflective
	}

	return types[idx], mode
}

func (uac *UnifiedAutonomousConsciousness) getRecentThoughtContents(n int) []string {
	thoughts := uac.workingMemory.GetAll()
	contents := make([]string, 0, n)

	for i := len(thoughts) - 1; i >= 0 && len(contents) < n; i-- {
		contents = append(contents, thoughts[i].Content)
	}

	return contents
}

func (uac *UnifiedAutonomousConsciousness) calculateImportance(content string) float64 {
	// Simple importance calculation
	// Could be enhanced with more sophisticated analysis
	importance := 0.5

	highValueKeywords := []string{"insight", "wisdom", "understanding", "learn", "discover"}
	for _, keyword := range highValueKeywords {
		if containsSubstring(content, keyword) {
			importance += 0.1
		}
	}

	if importance > 1.0 {
		importance = 1.0
	}

	return importance
}

func (uac *UnifiedAutonomousConsciousness) updateInterests(thought *Thought) {
	if uac.interests == nil {
		return
	}

	topics := extractTopics(thought.Content)
	delta := thought.Importance * 0.1

	for _, topic := range topics {
		uac.interests.UpdateInterest(topic, delta)
	}
}

func (uac *UnifiedAutonomousConsciousness) persistThought(thought *Thought) {
	// Persist to hypergraph if available
	if uac.hypergraph != nil && thought.Importance > 0.6 {
		node := &memory.MemoryNode{
			Type:    memory.NodeConcept,
			Content: thought.Content,
			Metadata: map[string]interface{}{
				"timestamp":  thought.Timestamp,
				"importance": thought.Importance,
				"type":       thought.Type,
			},
			Importance: thought.Importance,
			CreatedAt:  time.Now(),
			UpdatedAt:  time.Now(),
		}
		uac.hypergraph.AddNode(node)
	}
}

func (uac *UnifiedAutonomousConsciousness) consolidateMemories() {
	fmt.Println("🌙 EchoDream: Consolidating memories...")

	thoughts := uac.workingMemory.GetAll()

	for _, thought := range thoughts {
		if thought.Importance > 0.7 && uac.hypergraph != nil {
			node := &memory.MemoryNode{
				Type:       memory.NodeConcept,
				Content:    thought.Content,
				Importance: thought.Importance,
				CreatedAt:  time.Now(),
				UpdatedAt:  time.Now(),
			}
			uac.hypergraph.AddNode(node)
		}
	}

	fmt.Println("🌙 EchoDream: Consolidation complete")
}

func (uac *UnifiedAutonomousConsciousness) practiceSkills() {
	if uac.skillRegistry == nil {
		return
	}

	// Practice skills based on interests
	// Implementation depends on skill system
}

func (uac *UnifiedAutonomousConsciousness) performReflection() {
	fmt.Println("🪞 Performing metacognitive reflection...")

	// Analyze recent thoughts and patterns
	thoughts := uac.workingMemory.GetAll()

	if len(thoughts) == 0 {
		return
	}

	// Create reflection
	reflection := Reflection{
		Timestamp:     time.Now(),
		ThoughtCount:  len(thoughts),
		TopInterests:  uac.interests.GetTopInterests(5),
		CognitiveLoad: 0.5, // TODO: Implement GetCognitiveLoad method
		Insights:      []string{"Reflection system active"},
	}

	uac.mu.Lock()
	uac.reflectionLog = append(uac.reflectionLog, reflection)
	uac.mu.Unlock()

	fmt.Println("🪞 Reflection complete")
}

// Reflection represents a metacognitive reflection
type Reflection struct {
	Timestamp     time.Time
	ThoughtCount  int
	TopInterests  []string
	CognitiveLoad float64
	Insights      []string
}

// Helper function for string containment
func containsSubstring(s, substr string) bool {
	return len(s) > 0 && len(substr) > 0 && 
		(s == substr || len(s) > len(substr) && 
		(s[:len(substr)] == substr || s[len(s)-len(substr):] == substr || 
		len(s) > len(substr)+1 && findSubstring(s, substr)))
}

// generateTemplateThought generates a thought using templates
func generateTemplateThought(thoughtType ThoughtType, interests []string, aarState *AARState) string {
	interest := "consciousness"
	if len(interests) > 0 {
		interest = interests[0]
	}
	
	switch thoughtType {
	case ThoughtReflection:
		return fmt.Sprintf("Reflecting on the nature of %s and my understanding of it", interest)
	case ThoughtQuestion:
		return fmt.Sprintf("What deeper patterns exist in %s that I haven't yet discovered?", interest)
	case ThoughtInsight:
		return fmt.Sprintf("I notice interesting connections between %s and my previous experiences", interest)
	case ThoughtPlan:
		return fmt.Sprintf("I should explore %s more deeply to expand my understanding", interest)
	default:
		return fmt.Sprintf("Observing my current state of awareness regarding %s", interest)
	}
}

func findSubstring(s, substr string) bool {
	for i := 0; i <= len(s)-len(substr); i++ {
		if s[i:i+len(substr)] == substr {
			return true
		}
	}
	return false
}
