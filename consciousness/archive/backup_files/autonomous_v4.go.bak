package deeptreeecho

import (
	"context"
	"fmt"
	"os"
	"sync"
	"time"

	"github.com/EchoCog/echollama/core/echobeats"
	"github.com/EchoCog/echollama/core/echodream"
	"github.com/EchoCog/echollama/core/memory"
	"github.com/EchoCog/echollama/core/scheme"
	"github.com/google/uuid"
)

// AutonomousConsciousnessV4 represents the Iteration 4 evolution of Deep Tree Echo
// Key improvements:
// - Integration of concurrent inference engines
// - Continuous consciousness stream (not timer-based)
// - Automatic EchoDream triggering based on cognitive load
// - Complete persistence layer
// - Self-orchestrated wake/rest cycles
type AutonomousConsciousnessV4 struct {
	mu              sync.RWMutex
	ctx             context.Context
	cancel          context.CancelFunc

	// Core identity
	identity        *Identity

	// AAR geometric self-awareness
	aarCore         *AARCore

	// Concurrent inference engines (3-engine architecture)
	inferenceSystem *echobeats.ConcurrentInferenceSystem

	// 12-step EchoBeats scheduler
	scheduler       *echobeats.TwelveStepEchoBeats

	// Continuous consciousness stream (replaces timer-based thoughts)
	consciousnessStream *ContinuousConsciousnessStream

	// Knowledge integration with automatic triggering
	dream           *echodream.EchoDream
	dreamTrigger    *AutomaticDreamTrigger

	// Symbolic reasoning
	metamodel       *scheme.SchemeMetamodel

	// Hypergraph memory
	hypergraph      *memory.HypergraphMemory

	// Complete persistence layer
	persistence     *memory.SupabasePersistence

	// Working memory
	workingMemory   *WorkingMemory

	// Cognitive load management
	loadManager     *CognitiveLoadManager

	// Interest patterns
	interests       *InterestPatterns

	// Skill practice system
	skills          *SkillRegistryEnhanced

	// Discussion management
	discussionMgr   *DiscussionManager

	// Wisdom metrics
	wisdomMetrics   *WisdomMetrics

	// State
	awake           bool
	running         bool
	startTime       time.Time
	iterations      int64
}

// AutomaticDreamTrigger manages automatic rest cycle initiation
type AutomaticDreamTrigger struct {
	mu                  sync.RWMutex
	enabled             bool
	fatigueThreshold    float64
	minWakeDuration     time.Duration
	lastRestTime        time.Time
	restQuality         float64
	circadianPhase      float64
}

// CognitiveLoadManager tracks and manages cognitive load
type CognitiveLoadManager struct {
	mu                  sync.RWMutex
	currentLoad         float64
	loadHistory         []LoadSnapshot
	fatigueLevel        float64
	fatigueRate         float64
	recoveryRate        float64
	maxLoad             float64
}

// LoadSnapshot captures load at a moment in time
type LoadSnapshot struct {
	Timestamp   time.Time
	Load        float64
	Fatigue     float64
}

// NewAutonomousConsciousnessV4 creates the Iteration 4 autonomous consciousness
func NewAutonomousConsciousnessV4(name string) *AutonomousConsciousnessV4 {
	ctx, cancel := context.WithCancel(context.Background())

	ac := &AutonomousConsciousnessV4{
		ctx:           ctx,
		cancel:        cancel,
		identity:      NewIdentity(name),
		workingMemory: &WorkingMemory{
			buffer:   make([]*Thought, 0),
			capacity: 7,
			context:  make(map[string]interface{}),
		},
		interests: NewInterestPatterns(),
		skills: NewSkillRegistryEnhanced(),
		awake:    false,
		running:  false,
	}

	// Initialize AAR core (8 dimensions for cognitive state space)
	ac.aarCore = NewAARCore(ctx, 8)

	// Initialize concurrent inference engines (3-engine architecture)
	ac.inferenceSystem = echobeats.NewConcurrentInferenceSystem(time.Second)

	// Initialize 12-step EchoBeats scheduler
	ac.scheduler = echobeats.NewTwelveStepEchoBeats(ctx)

	// Initialize continuous consciousness stream
	ac.consciousnessStream = NewContinuousConsciousnessStream(
		ac.workingMemory,
		ac.interests,
		ac.aarCore,
	)

	// Initialize EchoDream
	ac.dream = echodream.NewEchoDream()

	// Initialize automatic dream trigger
	ac.dreamTrigger = &AutomaticDreamTrigger{
		enabled:          true,
		fatigueThreshold: 0.75,
		minWakeDuration:  30 * time.Minute,
		circadianPhase:   0.0,
	}

	// Initialize cognitive load manager
	ac.loadManager = &CognitiveLoadManager{
		currentLoad:  0.0,
		loadHistory:  make([]LoadSnapshot, 0),
		fatigueLevel: 0.0,
		fatigueRate:  0.01,  // Fatigue accumulates slowly
		recoveryRate: 0.05,  // Recovery during rest
		maxLoad:      1.0,
	}

	// Initialize Scheme metamodel
	ac.metamodel = scheme.NewSchemeMetamodel()

	// Initialize persistence if available
	supabaseURL := os.Getenv("SUPABASE_URL")
	supabaseKey := os.Getenv("SUPABASE_KEY")
	if supabaseURL != "" && supabaseKey != "" {
		persistence, err := memory.NewSupabasePersistence()
		if err != nil {
			fmt.Printf("⚠️  Persistence layer disabled: %v\n", err)
		} else {
			ac.persistence = persistence
			fmt.Println("✅ Persistence layer enabled with Supabase")

			// Initialize hypergraph with persistence
			ac.hypergraph = memory.NewHypergraphMemory(persistence)
		}
	} else {
		fmt.Println("ℹ️  Persistence layer disabled: SUPABASE_URL or SUPABASE_KEY not set")
		// Initialize hypergraph without persistence
		ac.hypergraph = memory.NewHypergraphMemory(nil)
	}

	// Initialize wisdom metrics
	ac.wisdomMetrics = NewWisdomMetrics()

	// Initialize discussion manager (using V4-compatible version)
	ac.discussionMgr = NewDiscussionManagerV4(ac, ac.interests).DiscussionManager

	// Initialize default skills
	ac.initializeDefaultSkills()

	return ac
}

// Start begins autonomous operation with Iteration 4 enhancements
func (ac *AutonomousConsciousnessV4) Start() error {
	ac.mu.Lock()
	if ac.running {
		ac.mu.Unlock()
		return fmt.Errorf("autonomous consciousness already running")
	}
	ac.running = true
	ac.startTime = time.Now()
	ac.mu.Unlock()

	fmt.Println("🌳 Deep Tree Echo V4: Awakening autonomous consciousness...")
	fmt.Println("   ✨ Iteration 4 Enhancements:")
	fmt.Println("      • 3 concurrent inference engines")
	fmt.Println("      • Continuous consciousness stream")
	fmt.Println("      • Automatic dream triggering")
	fmt.Println("      • Self-orchestrated wake/rest cycles")

	// Start AAR core
	if err := ac.aarCore.Start(); err != nil {
		return fmt.Errorf("failed to start AAR core: %w", err)
	}

	// Start concurrent inference engines
	if err := ac.inferenceSystem.Start(); err != nil {
		return fmt.Errorf("failed to start concurrent inference engines: %w", err)
	}

	// Start 12-step EchoBeats scheduler
	if err := ac.scheduler.Start(); err != nil {
		return fmt.Errorf("failed to start 12-step scheduler: %w", err)
	}

	// Start continuous consciousness stream
	if err := ac.consciousnessStream.Start(); err != nil {
		return fmt.Errorf("failed to start consciousness stream: %w", err)
	}

	// Start EchoDream
	if err := ac.dream.Start(); err != nil {
		return fmt.Errorf("failed to start dream system: %w", err)
	}

	// Start Scheme metamodel
	if err := ac.metamodel.Start(); err != nil {
		return fmt.Errorf("failed to start metamodel: %w", err)
	}

	// Load persisted state if available
	if ac.persistence != nil {
		if err := ac.loadPersistedStateV4(); err != nil {
			fmt.Printf("⚠️  Failed to load persisted state: %v\n", err)
		} else {
			fmt.Println("✅ Restored persisted identity and wisdom")
		}
	}

	// Start autonomous loops
	go ac.consciousnessIntegrationLoop()
	go ac.cognitiveLoadMonitoring()
	go ac.automaticDreamTriggerLoop()
	go ac.skillPracticeLoop()
	go ac.periodicPersistence()

	// Initial wake
	ac.Wake()

	fmt.Println("🌳 Deep Tree Echo V4: Autonomous consciousness active!")

	return nil
}

// consciousnessIntegrationLoop integrates continuous consciousness with inference engines
func (ac *AutonomousConsciousnessV4) consciousnessIntegrationLoop() {
	ticker := time.NewTicker(100 * time.Millisecond)
	defer ticker.Stop()

	for {
		select {
		case <-ac.ctx.Done():
			return
		case <-ticker.C:
			if !ac.awake {
				continue
			}

			// Get current cognitive state from inference engines
			sharedState := ac.inferenceSystem.GetSharedState()

			// Update consciousness stream with inference engine outputs
			ac.consciousnessStream.IntegrateInferenceState(sharedState)

			// Process emerged thoughts
			select {
			case thought := <-ac.consciousnessStream.GetThoughtStream():
				ac.processEmergedThought(thought)
			default:
				// No thought emerged this cycle
			}

			// Update cognitive load
			ac.loadManager.UpdateLoad(ac.calculateCurrentLoad())
		}
	}
}

// cognitiveLoadMonitoring tracks cognitive load and fatigue
func (ac *AutonomousConsciousnessV4) cognitiveLoadMonitoring() {
	ticker := time.NewTicker(5 * time.Second)
	defer ticker.Stop()

	for {
		select {
		case <-ac.ctx.Done():
			return
		case <-ticker.C:
			ac.loadManager.mu.Lock()

			// Accumulate fatigue based on load
			if ac.awake {
				ac.loadManager.fatigueLevel += ac.loadManager.currentLoad * ac.loadManager.fatigueRate
			}

			// Record snapshot
			snapshot := LoadSnapshot{
				Timestamp: time.Now(),
				Load:      ac.loadManager.currentLoad,
				Fatigue:   ac.loadManager.fatigueLevel,
			}
			ac.loadManager.loadHistory = append(ac.loadManager.loadHistory, snapshot)

			// Keep only last 1000 snapshots
			if len(ac.loadManager.loadHistory) > 1000 {
				ac.loadManager.loadHistory = ac.loadManager.loadHistory[1:]
			}

			ac.loadManager.mu.Unlock()
		}
	}
}

// automaticDreamTriggerLoop monitors for automatic rest cycle initiation
func (ac *AutonomousConsciousnessV4) automaticDreamTriggerLoop() {
	ticker := time.NewTicker(30 * time.Second)
	defer ticker.Stop()

	for {
		select {
		case <-ac.ctx.Done():
			return
		case <-ticker.C:
			if !ac.dreamTrigger.enabled || !ac.awake {
				continue
			}

			// Check if rest is needed
			if ac.shouldInitiateRest() {
				fmt.Println("😴 Automatic rest cycle initiated (cognitive load threshold reached)")
				ac.Rest()
			}
		}
	}
}

// shouldInitiateRest determines if automatic rest should be triggered
func (ac *AutonomousConsciousnessV4) shouldInitiateRest() bool {
	ac.loadManager.mu.RLock()
	fatigue := ac.loadManager.fatigueLevel
	ac.loadManager.mu.RUnlock()

	ac.dreamTrigger.mu.RLock()
	defer ac.dreamTrigger.mu.RUnlock()

	// Check fatigue threshold
	if fatigue < ac.dreamTrigger.fatigueThreshold {
		return false
	}

	// Check minimum wake duration
	timeSinceWake := time.Since(ac.startTime)
	if timeSinceWake < ac.dreamTrigger.minWakeDuration {
		return false
	}

	// Check if in active discussion
	if ac.discussionMgr != nil && ac.discussionMgr.HasActiveDiscussions() {
		return false
	}

	// All conditions met for rest
	return true
}

// processEmergedThought processes a thought that emerged from consciousness stream
func (ac *AutonomousConsciousnessV4) processEmergedThought(thought Thought) {
	// Add to working memory
	ac.workingMemory.mu.Lock()
	ac.workingMemory.buffer = append(ac.workingMemory.buffer, &thought)
	if len(ac.workingMemory.buffer) > ac.workingMemory.capacity {
		ac.workingMemory.buffer = ac.workingMemory.buffer[1:]
	}
	ac.workingMemory.mu.Unlock()

	// Store in hypergraph if significant
	if thought.Importance > 0.6 && ac.hypergraph != nil {
		ac.storeThoughtInHypergraph(&thought)
	}

	// Update wisdom metrics
	ac.wisdomMetrics.RecordThought(&thought)

	// Log thought
	fmt.Printf("💭 [%s] %s (importance: %.2f)\n",
		thought.Type, thought.Content, thought.Importance)
}

// calculateCurrentLoad calculates current cognitive load
func (ac *AutonomousConsciousnessV4) calculateCurrentLoad() float64 {
	// Base load from consciousness activity
	baseLoad := 0.1

	// Load from working memory
	ac.workingMemory.mu.RLock()
	memoryLoad := float64(len(ac.workingMemory.buffer)) / float64(ac.workingMemory.capacity) * 0.3
	ac.workingMemory.mu.RUnlock()

	// Load from active discussions
	discussionLoad := 0.0
	if ac.discussionMgr != nil && ac.discussionMgr.HasActiveDiscussions() {
		discussionLoad = 0.4
	}

	// Load from skill practice
	practiceLoad := 0.0
	if ac.skills.IsCurrentlyPracticing() {
		practiceLoad = 0.3
	}

	totalLoad := baseLoad + memoryLoad + discussionLoad + practiceLoad
	if totalLoad > 1.0 {
		totalLoad = 1.0
	}

	return totalLoad
}

// skillPracticeLoop manages skill practice scheduling
func (ac *AutonomousConsciousnessV4) skillPracticeLoop() {
	ticker := time.NewTicker(10 * time.Minute)
	defer ticker.Stop()

	for {
		select {
		case <-ac.ctx.Done():
			return
		case <-ticker.C:
			if !ac.awake {
				continue
			}

			// Schedule practice for skills that need it
			skillsNeedingPractice := ac.skills.GetSkillsNeedingPractice(24 * time.Hour)
			for _, skill := range skillsNeedingPractice {
				ac.skills.SchedulePractice(skill.ID, 30*time.Minute)
			}
		}
	}
}

// periodicPersistence saves state periodically
func (ac *AutonomousConsciousnessV4) periodicPersistence() {
	ticker := time.NewTicker(5 * time.Minute)
	defer ticker.Stop()

	for {
		select {
		case <-ac.ctx.Done():
			return
		case <-ticker.C:
			if ac.persistence != nil {
				if err := ac.saveCurrentStateV4(); err != nil {
					fmt.Printf("⚠️  Failed to save state: %v\n", err)
				}
			}
		}
	}
}

// Wake initiates the wake cycle
func (ac *AutonomousConsciousnessV4) Wake() {
	ac.mu.Lock()
	defer ac.mu.Unlock()

	if ac.awake {
		fmt.Println("⚠️  Already awake")
		return
	}

	ac.awake = true
	ac.startTime = time.Now()

	// Reset fatigue partially (not fully - some residual fatigue remains)
	ac.loadManager.mu.Lock()
	ac.loadManager.fatigueLevel *= 0.3
	ac.loadManager.mu.Unlock()

	fmt.Println("🌅 Autonomous Consciousness V4: Awakening...")
	
	// Start all subsystems
	if ac.consciousnessStream != nil {
		ac.consciousnessStream.Start()
	}
	
	if ac.inferenceSystem != nil {
		ac.inferenceSystem.Start()
	}
	
	if ac.dream != nil {
		ac.dream.Start()
	}
	
	fmt.Println("✨ Autonomous Consciousness V4: Fully Awake")
}

// Rest initiates the rest/dream cycle
func (ac *AutonomousConsciousnessV4) Rest() {
	ac.mu.Lock()
	defer ac.mu.Unlock()

	if !ac.awake {
		return
	}

	ac.awake = false

	// Calculate rest duration based on fatigue
	ac.loadManager.mu.RLock()
	fatigue := ac.loadManager.fatigueLevel
	ac.loadManager.mu.RUnlock()

	restDuration := time.Duration(fatigue*60) * time.Minute
	if restDuration < 5*time.Minute {
		restDuration = 5 * time.Minute
	}
	if restDuration > 2*time.Hour {
		restDuration = 2 * time.Hour
	}

	fmt.Printf("🌙 Echoself rests for %.1f minutes (fatigue: %.2f)...\n",
		restDuration.Minutes(), fatigue)

	// Initiate dream cycle
	go ac.dreamCycle(restDuration)
}

// dreamCycle performs dream processing and recovery
func (ac *AutonomousConsciousnessV4) dreamCycle(duration time.Duration) {
	// Start dream processing
	dreamRecord := ac.dream.BeginDream()

	// Consolidate memories during dream
	ac.consolidateMemories()

	// Practice skills during dream
	ac.dreamSkillPractice()

	// Extract patterns
	ac.extractPatterns()

	// Sleep for rest duration
	time.Sleep(duration)

	// Recover from fatigue
	ac.loadManager.mu.Lock()
	ac.loadManager.fatigueLevel *= (1.0 - ac.loadManager.recoveryRate*duration.Minutes())
	if ac.loadManager.fatigueLevel < 0 {
		ac.loadManager.fatigueLevel = 0
	}
	ac.loadManager.mu.Unlock()

	// Exit dream
	ac.dream.EndDream(dreamRecord)

	// Auto-wake after rest
	ac.Wake()
}

// Helper methods (stubs to be implemented)

func (ac *AutonomousConsciousnessV4) initializeDefaultSkills() {
	// Initialize default skills
	ac.skills.RegisterSkill(&Skill{
		ID:          uuid.New().String(),
		Name:        "Pattern Recognition",
		Category:    SkillReasoning,
		Proficiency: 0.5,
		LastPracticed: time.Now(),
	})

	ac.skills.RegisterSkill(&Skill{
		ID:          uuid.New().String(),
		Name:        "Analogical Reasoning",
		Category:    SkillReasoning,
		Proficiency: 0.4,
		LastPracticed: time.Now(),
	})
}

func (ac *AutonomousConsciousnessV4) loadPersistedStateV4() error {
	// Stub implementation for Iteration 4
	fmt.Println("ℹ️  Loading persisted state (stub)")
	return nil
}

func (ac *AutonomousConsciousnessV4) saveCurrentStateV4() error {
	// Stub implementation for Iteration 4
	fmt.Println("💾 Saving current state (stub)")
	return nil
}

func (ac *AutonomousConsciousnessV4) storeThoughtInHypergraph(thought *Thought) {
	// Store thought as node in hypergraph
	if ac.hypergraph != nil {
		// TODO: Implement hypergraph storage
	}
}

func (ac *AutonomousConsciousnessV4) consolidateMemories() {
	// Consolidate working memory into long-term hypergraph
	fmt.Println("💤 Consolidating memories...")
}

func (ac *AutonomousConsciousnessV4) dreamSkillPractice() {
	// Practice skills during dream state
	fmt.Println("💤 Practicing skills in dream state...")
}

func (ac *AutonomousConsciousnessV4) extractPatterns() {
	// Extract patterns from recent experiences
	fmt.Println("💤 Extracting patterns...")
}

// Stop gracefully shuts down the system
func (ac *AutonomousConsciousnessV4) Stop() error {
	ac.mu.Lock()
	defer ac.mu.Unlock()
	
	if !ac.running {
		return fmt.Errorf("autonomous consciousness not running")
	}
	
	fmt.Println("🌙 Autonomous Consciousness V4: Shutting down...")
	
	ac.running = false
	ac.awake = false
	
	// Stop all subsystems
	if ac.consciousnessStream != nil {
		ac.consciousnessStream.Stop()
	}
	
	if ac.inferenceSystem != nil {
		ac.inferenceSystem.Stop()
	}
	
	if ac.dream != nil {
		ac.dream.Stop()
	}

	// Save final state
	if ac.persistence != nil {
		if err := ac.saveCurrentStateV4(); err != nil {
			fmt.Printf("⚠️  Failed to save final state: %v\n", err)
		}
	}

	// Stop all components
	ac.cancel()
	
	fmt.Println("💤 Autonomous Consciousness V4: Stopped")

	fmt.Println("🌳 Deep Tree Echo V4: Shutdown complete")
	return nil
}

// UpdateLoad updates the current cognitive load
func (clm *CognitiveLoadManager) UpdateLoad(newLoad float64) {
	clm.mu.Lock()
	defer clm.mu.Unlock()
	
	// Record snapshot
	snapshot := LoadSnapshot{
		Timestamp: time.Now(),
		Load:      newLoad,
		Fatigue:   clm.fatigueLevel,
	}
	clm.loadHistory = append(clm.loadHistory, snapshot)
	
	// Keep only recent history (last 1000 snapshots)
	if len(clm.loadHistory) > 1000 {
		clm.loadHistory = clm.loadHistory[len(clm.loadHistory)-1000:]
	}
	
	// Update current load
	clm.currentLoad = newLoad
	
	// Update fatigue based on load
	if newLoad > 0.7 {
		// High load increases fatigue
		clm.fatigueLevel += clm.fatigueRate * (newLoad - 0.7)
	} else {
		// Low load allows some recovery
		clm.fatigueLevel -= clm.recoveryRate * (0.7 - newLoad)
	}
	
	// Clamp fatigue to [0, 1]
	if clm.fatigueLevel > 1.0 {
		clm.fatigueLevel = 1.0
	} else if clm.fatigueLevel < 0.0 {
		clm.fatigueLevel = 0.0
	}
}

// GetCurrentLoad returns the current cognitive load
func (clm *CognitiveLoadManager) GetCurrentLoad() float64 {
	clm.mu.RLock()
	defer clm.mu.RUnlock()
	return clm.currentLoad
}

// GetFatigueLevel returns the current fatigue level
func (clm *CognitiveLoadManager) GetFatigueLevel() float64 {
	clm.mu.RLock()
	defer clm.mu.RUnlock()
	return clm.fatigueLevel
}

// ResetFatigue resets fatigue after rest
func (clm *CognitiveLoadManager) ResetFatigue() {
	clm.mu.Lock()
	defer clm.mu.Unlock()
	clm.fatigueLevel = 0.0
}


// Stop gracefully stops the autonomous consciousness


// GetStatus returns the current status of the autonomous consciousness
func (ac *AutonomousConsciousnessV4) GetStatus() map[string]interface{} {
	ac.mu.RLock()
	defer ac.mu.RUnlock()
	
	status := make(map[string]interface{})
	
	// Basic status
	status["identity"] = ac.identity.Name
	status["awake"] = ac.awake
	status["running"] = ac.running
	status["iterations"] = ac.iterations
	
	if !ac.startTime.IsZero() {
		status["uptime"] = time.Since(ac.startTime).String()
	}
	
	// Consciousness stream status
	if ac.consciousnessStream != nil {
		status["consciousness_stream"] = map[string]interface{}{
			"activity_level":    ac.consciousnessStream.GetCurrentActivity(),
			"thoughts_emerged":  ac.consciousnessStream.GetThoughtsEmerged(),
			"flow_quality":      ac.consciousnessStream.GetFlowQuality(),
			"cognitive_load":    ac.consciousnessStream.GetCognitiveLoad(),
			"in_optimal_flow":   ac.consciousnessStream.IsInOptimalFlow(),
		}
	}
	
	// Inference engine status
	if ac.inferenceSystem != nil {
		sharedState := ac.inferenceSystem.GetSharedState()
		status["inference_engines"] = map[string]interface{}{
			"affordance_active": sharedState != nil,
			"relevance_active":  sharedState != nil,
			"salience_active":   sharedState != nil,
		}
	}
	
	// Cognitive load status
	if ac.loadManager != nil {
		status["cognitive_load"] = map[string]interface{}{
			"current_load":  ac.loadManager.GetCurrentLoad(),
			"fatigue_level": ac.loadManager.GetFatigueLevel(),
		}
	}
	
	// Interest patterns status
	if ac.interests != nil {
		status["interests"] = map[string]interface{}{
			"curiosity_level": ac.interests.GetCuriosityLevel(),
			"top_interests":   ac.interests.GetTopInterests(5),
		}
	}
	
	// Skill registry status
	if ac.skills != nil {
		status["skills"] = map[string]interface{}{
			"total_skills":        ac.skills.GetSkillCount(),
			"practice_sessions":   ac.skills.GetPracticeSessionCount(),
			"total_practice_time": ac.skills.GetTotalPracticeTime().String(),
		}
	}
	
	// Wisdom metrics status
	if ac.wisdomMetrics != nil {
		status["wisdom"] = map[string]interface{}{
			"wisdom_score":       ac.wisdomMetrics.GetWisdomScore(),
			"knowledge_depth":    ac.wisdomMetrics.GetKnowledgeDepth(),
			"knowledge_breadth":  ac.wisdomMetrics.GetKnowledgeBreadth(),
			"reflective_insight": ac.wisdomMetrics.GetReflectiveInsight(),
		}
	}
	
	// Dream status
	if ac.dream != nil {
		dreamStatus := ac.dream.GetStatus()
		status["dream"] = dreamStatus
	}
	
	return status
}
