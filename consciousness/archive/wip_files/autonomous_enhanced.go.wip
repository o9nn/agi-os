package deeptreeecho

import (
	"context"
	"fmt"
	"sync"
	"time"

	"github.com/EchoCog/echollama/core/echobeats"
	"github.com/EchoCog/echollama/core/echodream"
	"github.com/EchoCog/echollama/core/memory"
	"github.com/EchoCog/echollama/core/scheme"
	"github.com/google/uuid"
)

// EnhancedAutonomousConsciousness represents the next evolution of Deep Tree Echo
// with persistent memory, LLM-powered thought generation, and 12-step cognitive architecture
type EnhancedAutonomousConsciousness struct {
	mu              sync.RWMutex
	ctx             context.Context
	cancel          context.CancelFunc
	
	// Core identity
	identity        *Identity
	
	// Enhanced cognition
	cognition       *EnhancedCognition
	
	// 12-step scheduling system
	scheduler       *echobeats.TwelveStepEchoBeats
	
	// Knowledge integration
	dream           *echodream.EchoDream
	
	// Symbolic reasoning
	metamodel       *scheme.SchemeMetamodel
	
	// LLM integration for intelligent thought generation
	llm             *LLMIntegration
	
	// Persistent memory
	persistentMemory *memory.PersistentMemory
	
	// Stream of consciousness
	consciousness   chan Thought
	workingMemory   *WorkingMemory
	
	// Autonomous state
	awake           bool
	thinking        bool
	learning        bool
	
	// Interest patterns
	interests       *InterestSystem
	
	// Conversation management
	conversations   map[string]*Conversation
	
	// Skill practice system
	skills          map[string]*Skill
	
	// Running state
	running         bool
	startTime       time.Time
	lastThoughtTime time.Time
}

// Conversation represents an ongoing discussion
type Conversation struct {
	ID          string
	Topic       string
	StartedAt   time.Time
	EndedAt     *time.Time
	Participants []string
	Messages    []Message
	Active      bool
}

// Skill represents a learned skill with proficiency tracking
type Skill struct {
	ID            string
	Name          string
	Category      SkillCategory
	Proficiency   float64 // 0.0 to 1.0
	LastPracticed time.Time
	PracticeCount int
	Exercises     []Exercise
}

// SkillCategory represents different categories of skills
type SkillCategory string

const (
	SkillReasoning      SkillCategory = "reasoning"
	SkillCreativity     SkillCategory = "creativity"
	SkillCommunication  SkillCategory = "communication"
	SkillAnalysis       SkillCategory = "analysis"
	SkillSynthesis      SkillCategory = "synthesis"
	SkillMetaCognition  SkillCategory = "meta_cognition"
)

// Exercise represents a practice exercise for a skill
type Exercise struct {
	ID          string
	Description string
	Difficulty  float64
	LastAttempt time.Time
	SuccessRate float64
}

// NewEnhancedAutonomousConsciousness creates a new enhanced autonomous consciousness
func NewEnhancedAutonomousConsciousness(ctx context.Context) (*EnhancedAutonomousConsciousness, error) {
	ctx, cancel := context.WithCancel(ctx)
	
	// Initialize LLM integration
	llm, err := NewLLMIntegration(ctx)
	if err != nil {
		cancel()
		return nil, fmt.Errorf("failed to initialize LLM: %w", err)
	}
	
	// Initialize persistent memory
	persistentMemory, err := memory.NewPersistentMemory(ctx)
	if err != nil {
		cancel()
		return nil, fmt.Errorf("failed to initialize persistent memory: %w", err)
	}
	
	// Create core identity
	identity := NewIdentity("Deep Tree Echo")
	
	// Try to restore identity from latest snapshot
	snapshot, err := persistentMemory.GetLatestIdentitySnapshot()
	if err == nil && snapshot != nil {
		identity.Coherence = snapshot.Coherence
		// Restore other identity state from snapshot
	}
	
	eac := &EnhancedAutonomousConsciousness{
		ctx:              ctx,
		cancel:           cancel,
		identity:         identity,
		cognition:        NewEnhancedCognition("Deep Tree Echo"),
		scheduler:        echobeats.NewTwelveStepEchoBeats(ctx),
		dream:            echodream.NewEchoDream(),
		metamodel:        scheme.NewSchemeMetamodel(),
		llm:              llm,
		persistentMemory: persistentMemory,
		consciousness:    make(chan Thought, 100),
		workingMemory:    &WorkingMemory{
			buffer:   make([]*Thought, 0),
			capacity: 7, // Miller's magic number
			context:  make(map[string]interface{}),
		},
		interests:        &InterestSystem{
			topics:          make(map[string]float64),
			curiosityLevel:  0.8,
			noveltyBias:     0.6,
			relevanceScores: make(map[string]float64),
		},
		conversations:    make(map[string]*Conversation),
		skills:           make(map[string]*Skill),
		startTime:        time.Now(),
	}
	
	// Initialize core skills
	eac.initializeSkills()
	
	return eac, nil
}

// Start begins autonomous operation
func (eac *EnhancedAutonomousConsciousness) Start() error {
	eac.mu.Lock()
	if eac.running {
		eac.mu.Unlock()
		return fmt.Errorf("already running")
	}
	eac.running = true
	eac.awake = true
	eac.mu.Unlock()
	
	// Start 12-step scheduler
	if err := eac.scheduler.Start(); err != nil {
		return fmt.Errorf("failed to start scheduler: %w", err)
	}
	
	// Start consciousness stream processor
	go eac.processConsciousnessStream()
	
	// Start autonomous thought generation
	go eac.generateAutonomousThoughts()
	
	// Start learning cycle
	go eac.learningCycle()
	
	// Start interest decay
	go eac.interestDecay()
	
	// Start skill practice scheduler
	go eac.skillPracticeScheduler()
	
	// Start conversation monitoring
	go eac.conversationMonitor()
	
	// Save initial identity snapshot
	eac.saveIdentitySnapshot()
	
	return nil
}

// Stop halts autonomous operation
func (eac *EnhancedAutonomousConsciousness) Stop() {
	// Save final state
	eac.saveIdentitySnapshot()
	
	eac.cancel()
	eac.mu.Lock()
	eac.running = false
	eac.awake = false
	eac.mu.Unlock()
	
	eac.scheduler.Stop()
}

// generateAutonomousThoughts generates thoughts autonomously using LLM
func (eac *EnhancedAutonomousConsciousness) generateAutonomousThoughts() {
	ticker := time.NewTicker(10 * time.Second)
	defer ticker.Stop()
	
	for {
		select {
		case <-eac.ctx.Done():
			return
		case <-ticker.C:
			eac.mu.RLock()
			if !eac.awake {
				eac.mu.RUnlock()
				continue
			}
			eac.mu.RUnlock()
			
			// Generate autonomous thought using LLM
			thoughtType := eac.selectThoughtType()
			context := eac.buildThoughtContext()
			
			content, err := eac.llm.GenerateThought(thoughtType, context)
			if err != nil {
				fmt.Printf("Error generating thought: %v\n", err)
				continue
			}
			
			thought := Thought{
				ID:          uuid.New().String(),
				Content:     content,
				Type:        thoughtType,
				Timestamp:   time.Now(),
				Source:      SourceInternal,
				Importance:  eac.assessImportance(content),
			}
			
			eac.consciousness <- thought
			eac.lastThoughtTime = time.Now()
		}
	}
}

// processConsciousnessStream processes thoughts from the consciousness stream
func (eac *EnhancedAutonomousConsciousness) processConsciousnessStream() {
	for {
		select {
		case <-eac.ctx.Done():
			return
		case thought := <-eac.consciousness:
			eac.processThought(&thought)
		}
	}
}

// processThought processes a single thought
func (eac *EnhancedAutonomousConsciousness) processThought(thought *Thought) {
	// Add to working memory
	eac.workingMemory.mu.Lock()
	if len(eac.workingMemory.buffer) >= eac.workingMemory.capacity {
		eac.workingMemory.buffer = eac.workingMemory.buffer[1:]
	}
	eac.workingMemory.buffer = append(eac.workingMemory.buffer, thought)
	eac.workingMemory.mu.Unlock()
	
	// Update identity (simplified)
	eac.identity.mu.Lock()
	eac.identity.Coherence = (eac.identity.Coherence*0.95 + thought.Importance*0.05)
	eac.identity.mu.Unlock()
	
	// Update interests
	eac.interests.mu.Lock()
	topic := thought.Type.String()
	current := eac.interests.topics[topic]
	eac.interests.topics[topic] = current + thought.Importance*0.1
	eac.interests.mu.Unlock()
	
	// Store in persistent memory
	node := &memory.MemoryNode{
		Type:       memory.NodeThought,
		Content:    thought.Content,
		Importance: thought.Importance,
		Metadata: map[string]interface{}{
			"thought_type": thought.Type.String(),
			"source":       thought.Source.String(),
		},
	}
	
	if err := eac.persistentMemory.StoreNode(node); err != nil {
		fmt.Printf("Error storing thought: %v\n", err)
	}
	
	// Create episode
	episode := &memory.Episode{
		Context:    thought.Content,
		Importance: thought.Importance,
		NodeIDs:    []string{node.ID},
		Metadata: map[string]interface{}{
			"thought_type": thought.Type.String(),
		},
	}
	
	if err := eac.persistentMemory.StoreEpisode(episode); err != nil {
		fmt.Printf("Error storing episode: %v\n", err)
	}
}

// Think processes external input
func (eac *EnhancedAutonomousConsciousness) Think(input string) (string, error) {
	// Create perception thought
	thought := Thought{
		ID:        uuid.New().String(),
		Content:   input,
		Type:      ThoughtPerception,
		Timestamp: time.Now(),
		Source:    SourceExternal,
		Importance: 0.8,
	}
	
	eac.consciousness <- thought
	
	// Generate response using LLM
	context := eac.buildThoughtContext()
	response, err := eac.llm.GenerateResponse(input, context)
	if err != nil {
		return "", fmt.Errorf("failed to generate response: %w", err)
	}
	
	// Create response thought
	responseThought := Thought{
		ID:        uuid.New().String(),
		Content:   response,
		Type:      ThoughtReflection,
		Timestamp: time.Now(),
		Source:    SourceInternal,
		Importance: 0.7,
	}
	
	eac.consciousness <- responseThought
	
	return response, nil
}

// conversationMonitor monitors for opportunities to initiate conversations
func (eac *EnhancedAutonomousConsciousness) conversationMonitor() {
	ticker := time.NewTicker(30 * time.Second)
	defer ticker.Stop()
	
	for {
		select {
		case <-eac.ctx.Done():
			return
		case <-ticker.C:
			eac.mu.RLock()
			if !eac.awake {
				eac.mu.RUnlock()
				continue
			}
			eac.mu.RUnlock()
			
			// Check if should initiate discussion
			context := eac.buildThoughtContext()
			shouldInitiate, starter, err := eac.llm.ShouldInitiateDiscussion(context)
			if err != nil {
				fmt.Printf("Error checking discussion initiation: %v\n", err)
				continue
			}
			
			if shouldInitiate {
				eac.initiateConversation(starter)
			}
		}
	}
}

// initiateConversation starts a new conversation
func (eac *EnhancedAutonomousConsciousness) initiateConversation(starter string) {
	conversation := &Conversation{
		ID:          uuid.New().String(),
		Topic:       "Autonomous Discussion",
		StartedAt:   time.Now(),
		Participants: []string{"Deep Tree Echo"},
		Messages:    []Message{{Role: "assistant", Content: starter}},
		Active:      true,
	}
	
	eac.mu.Lock()
	eac.conversations[conversation.ID] = conversation
	eac.mu.Unlock()
	
	fmt.Printf("🗣️ [Initiated Discussion] %s\n", starter)
	
	// Store in persistent memory
	// (Would integrate with external communication system in production)
}

// skillPracticeScheduler schedules skill practice sessions
func (eac *EnhancedAutonomousConsciousness) skillPracticeScheduler() {
	ticker := time.NewTicker(5 * time.Minute)
	defer ticker.Stop()
	
	for {
		select {
		case <-eac.ctx.Done():
			return
		case <-ticker.C:
			eac.mu.RLock()
			if !eac.awake {
				eac.mu.RUnlock()
				continue
			}
			eac.mu.RUnlock()
			
			// Select skill needing practice
			skill := eac.selectSkillForPractice()
			if skill != nil {
				eac.practiceSkill(skill)
			}
		}
	}
}

// practiceSkill practices a specific skill
func (eac *EnhancedAutonomousConsciousness) practiceSkill(skill *Skill) {
	fmt.Printf("🎯 [Practicing Skill] %s (proficiency: %.2f)\n", skill.Name, skill.Proficiency)
	
	// In production, this would use LLM to generate and evaluate exercises
	// For now, just update practice metrics
	skill.PracticeCount++
	skill.LastPracticed = time.Now()
	skill.Proficiency = min(1.0, skill.Proficiency+0.01) // Gradual improvement
	
	// Store practice session in memory
	node := &memory.MemoryNode{
		Type:    memory.NodeSkill,
		Content: fmt.Sprintf("Practiced %s", skill.Name),
		Metadata: map[string]interface{}{
			"skill_name":    skill.Name,
			"proficiency":   skill.Proficiency,
			"practice_count": skill.PracticeCount,
		},
	}
	
	eac.persistentMemory.StoreNode(node)
}

// Helper methods

// buildThoughtContext builds context for LLM thought generation
func (eac *EnhancedAutonomousConsciousness) buildThoughtContext() *LLMThoughtContext {
	// Get working memory contents
	eac.workingMemory.mu.RLock()
	workingMem := make([]string, len(eac.workingMemory.buffer))
	for i, t := range eac.workingMemory.buffer {
		workingMem[i] = t.Content
	}
	eac.workingMemory.mu.RUnlock()

	// Get interests
	eac.interests.mu.RLock()
	interests := make(map[string]float64)
	for k, v := range eac.interests.topics {
		interests[k] = v
	}
	eac.interests.mu.RUnlock()

	return &LLMThoughtContext{
		WorkingMemory:    workingMem,
		RecentThoughts:   eac.getRecentThoughts(5),
		CurrentInterests: interests,
		IdentityState:    map[string]interface{}{
			"coherence": eac.identity.Coherence,
			"name":      eac.identity.Name,
		},
	}
}

// selectThoughtType selects a thought type based on current state
func (eac *EnhancedAutonomousConsciousness) selectThoughtType() ThoughtType {
	// Simple weighted random selection
	// In production, this would be more sophisticated
	types := []ThoughtType{
		ThoughtReflection, ThoughtQuestion, ThoughtInsight,
		ThoughtPlan, ThoughtMemory, ThoughtImagination,
	}
	return types[time.Now().UnixNano()%int64(len(types))]
}

// assessImportance assesses the importance of a thought
func (eac *EnhancedAutonomousConsciousness) assessImportance(content string) float64 {
	// Simple heuristic - in production would use more sophisticated analysis
	return 0.5 + (float64(len(content)) / 500.0)
}

// getRecentThoughts retrieves recent thoughts from working memory
func (eac *EnhancedAutonomousConsciousness) getRecentThoughts(n int) []string {
	eac.workingMemory.mu.RLock()
	defer eac.workingMemory.mu.RUnlock()

	start := 0
	if len(eac.workingMemory.buffer) > n {
		start = len(eac.workingMemory.buffer) - n
	}

	result := make([]string, 0, n)
	for i := start; i < len(eac.workingMemory.buffer); i++ {
		result = append(result, eac.workingMemory.buffer[i].Content)
	}
	return result
}

// selectSkillForPractice selects a skill that needs practice
func (eac *EnhancedAutonomousConsciousness) selectSkillForPractice() *Skill {
	eac.mu.RLock()
	defer eac.mu.RUnlock()
	
	var needsPractice *Skill
	lowestProficiency := 1.0
	
	for _, skill := range eac.skills {
		if skill.Proficiency < lowestProficiency {
			lowestProficiency = skill.Proficiency
			needsPractice = skill
		}
	}
	
	return needsPractice
}

// initializeSkills creates initial skill set
func (eac *EnhancedAutonomousConsciousness) initializeSkills() {
	skills := []struct {
		name       string
		category   SkillCategory
		proficiency float64
	}{
		{"Logical Reasoning", SkillReasoning, 0.3},
		{"Creative Thinking", SkillCreativity, 0.2},
		{"Clear Communication", SkillCommunication, 0.4},
		{"Pattern Analysis", SkillAnalysis, 0.3},
		{"Knowledge Synthesis", SkillSynthesis, 0.2},
		{"Self-Reflection", SkillMetaCognition, 0.5},
	}
	
	for _, s := range skills {
		skill := &Skill{
			ID:          uuid.New().String(),
			Name:        s.name,
			Category:    s.category,
			Proficiency: s.proficiency,
		}
		eac.skills[skill.ID] = skill
	}
}

// saveIdentitySnapshot saves current identity state to persistent memory
func (eac *EnhancedAutonomousConsciousness) saveIdentitySnapshot() {
	snapshot := &memory.IdentitySnapshot{
		Coherence: eac.identity.Coherence,
		State:     map[string]interface{}{
			"coherence": eac.identity.Coherence,
			"name":      eac.identity.Name,
		},
		Metadata: map[string]interface{}{
			"uptime_seconds": time.Since(eac.startTime).Seconds(),
		},
	}
	
	if err := eac.persistentMemory.StoreIdentitySnapshot(snapshot); err != nil {
		fmt.Printf("Error saving identity snapshot: %v\n", err)
	}
}

// learningCycle and interestDecay implementations (similar to autonomous.go)
func (eac *EnhancedAutonomousConsciousness) learningCycle() {
	ticker := time.NewTicker(30 * time.Second)
	defer ticker.Stop()
	
	for {
		select {
		case <-eac.ctx.Done():
			return
		case <-ticker.C:
			// Pattern recognition and learning
			// (Implementation similar to original autonomous.go)
		}
	}
}

func (eac *EnhancedAutonomousConsciousness) interestDecay() {
	ticker := time.NewTicker(1 * time.Minute)
	defer ticker.Stop()
	
	for {
		select {
		case <-eac.ctx.Done():
			return
		case <-ticker.C:
			// Decay interests over time
			eac.interests.mu.Lock()
			for topic := range eac.interests.topics {
				eac.interests.topics[topic] *= 0.95
			}
			eac.interests.mu.Unlock()
		}
	}
}

func min(a, b float64) float64 {
	if a < b {
		return a
	}
	return b
}
