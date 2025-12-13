package deeptreeecho

import (
	"context"
	"fmt"
	"log"
	"sync"
	"time"

	"github.com/EchoCog/echollama/core/echobeats"
	"github.com/EchoCog/echollama/core/echodream"
	"github.com/EchoCog/echollama/core/scheme"
)

// ConsolidatedAutonomousConsciousness represents the unified implementation
// of Deep Tree Echo's autonomous consciousness system with full integration
// of all cognitive components
type ConsolidatedAutonomousConsciousness struct {
	mu      sync.RWMutex
	ctx     context.Context
	cancel  context.CancelFunc
	
	// Core identity and cognition
	identity  *Identity
	cognition *EnhancedCognition
	
	// Scheduling and orchestration
	scheduler       *echobeats.TwelveStepEchoBeats
	thoughtGen      *AdaptiveThoughtGenerator
	
	// Knowledge integration and consolidation
	dream           *echodream.EchoDream
	
	// Symbolic reasoning kernel
	metamodel       *scheme.SchemeMetamodel
	
	// Persistence layer
	persistence     *SupabasePersistence
	
	// Stream of consciousness
	consciousness   chan *Thought
	workingMemory   *WorkingMemory
	
	// Interest and exploration
	interests       *InterestSystem
	
	// State management
	awake           bool
	thinking        bool
	learning        bool
	running         bool
	startTime       time.Time
	
	// Metrics
	iterations      int
	autonomousThoughts int
	consolidations  int
	
	// Configuration
	config          *ConsciousnessConfig
}

// ConsciousnessConfig holds configuration parameters
type ConsciousnessConfig struct {
	WorkingMemoryCapacity int
	ThoughtQueueSize      int
	AutoSaveInterval      time.Duration
	ConsolidationInterval time.Duration
	FatigueThreshold      float64
	RestThreshold         float64
	WakeThreshold         float64
}

// DefaultConsciousnessConfig returns default configuration
func DefaultConsciousnessConfig() *ConsciousnessConfig {
	return &ConsciousnessConfig{
		WorkingMemoryCapacity: 7, // Miller's magic number
		ThoughtQueueSize:      1000,
		AutoSaveInterval:      5 * time.Minute,
		ConsolidationInterval: 30 * time.Minute,
		FatigueThreshold:      0.8,
		RestThreshold:         0.8,
		WakeThreshold:         0.2,
	}
}

// NewConsolidatedAutonomousConsciousness creates a new consolidated autonomous consciousness
func NewConsolidatedAutonomousConsciousness(name string, config *ConsciousnessConfig) (*ConsolidatedAutonomousConsciousness, error) {
	if config == nil {
		config = DefaultConsciousnessConfig()
	}
	
	ctx, cancel := context.WithCancel(context.Background())
	
	// Initialize persistence layer
	persistence, err := NewSupabasePersistence(ctx)
	if err != nil {
		log.Printf("Warning: Failed to initialize persistence: %v. Running without persistence.", err)
		persistence = nil
	}
	
	cac := &ConsolidatedAutonomousConsciousness{
		ctx:         ctx,
		cancel:      cancel,
		identity:    NewIdentity(name),
		cognition:   NewEnhancedCognition(name),
		scheduler:   echobeats.NewTwelveStepEchoBeats(ctx),
		thoughtGen:  NewAdaptiveThoughtGenerator(ctx),
		dream:       echodream.NewEchoDream(),
		metamodel:   scheme.NewSchemeMetamodel(),
		persistence: persistence,
		consciousness: make(chan *Thought, config.ThoughtQueueSize),
		workingMemory: &WorkingMemory{
			buffer:   make([]*Thought, 0),
			capacity: config.WorkingMemoryCapacity,
			context:  make(map[string]interface{}),
		},
		interests: &InterestSystem{
			topics:          make(map[string]float64),
			curiosityLevel:  0.7,
			noveltyBias:     0.3,
			relevanceScores: make(map[string]float64),
		},
		awake:  false,
		config: config,
	}
	
	// Try to load previous identity state
	if persistence != nil {
		if err := cac.loadIdentityState(); err != nil {
			log.Printf("Warning: Failed to load identity state: %v. Starting fresh.", err)
		}
	}
	
	// Wire 12-step cognitive loop to operations
	cac.wireStepHandlers()
	
	return cac, nil
}

// Start begins autonomous operation
func (cac *ConsolidatedAutonomousConsciousness) Start() error {
	cac.mu.Lock()
	if cac.running {
		cac.mu.Unlock()
		return fmt.Errorf("consciousness already running")
	}
	cac.running = true
	cac.awake = true
	cac.startTime = time.Now()
	cac.mu.Unlock()
	
	log.Println("🌊 Starting Consolidated Autonomous Consciousness...")
	
	// Start subsystems
	go cac.scheduler.Start()
	go cac.dream.Start()
	go cac.metamodel.Start()
	
	// Start main cognitive loop
	go cac.runCognitiveLoop()
	
	// Start autonomous thought generation
	go cac.runAdaptiveThoughtGeneration()
	
	// Start periodic tasks
	go cac.runPeriodicTasks()
	
	log.Println("✅ Autonomous consciousness active")
	return nil
}

// Stop halts autonomous operation
func (cac *ConsolidatedAutonomousConsciousness) Stop() error {
	cac.mu.Lock()
	if !cac.running {
		cac.mu.Unlock()
		return fmt.Errorf("consciousness not running")
	}
	cac.running = false
	cac.mu.Unlock()
	
	log.Println("🛑 Stopping autonomous consciousness...")
	
	// Save final state
	if cac.persistence != nil {
		if err := cac.saveIdentityState(); err != nil {
			log.Printf("Error saving final state: %v", err)
		}
	}
	
	// Cancel context to stop all goroutines
	cac.cancel()
	
	log.Println("✅ Autonomous consciousness stopped")
	return nil
}

// runCognitiveLoop is the main processing loop
func (cac *ConsolidatedAutonomousConsciousness) runCognitiveLoop() {
	for {
		select {
		case <-cac.ctx.Done():
			return
		case thought := <-cac.consciousness:
			cac.processThought(thought)
		}
	}
}

// runAdaptiveThoughtGeneration generates thoughts based on cognitive state
func (cac *ConsolidatedAutonomousConsciousness) runAdaptiveThoughtGeneration() {
	for {
		select {
		case <-cac.ctx.Done():
			return
		default:
			cac.mu.RLock()
			awake := cac.awake
			cac.mu.RUnlock()
			
			if !awake {
				time.Sleep(5 * time.Second)
				continue
			}
			
			// Compute adaptive interval
			interval := cac.thoughtGen.ComputeNextInterval()
			time.Sleep(interval)
			
			// Generate autonomous thought
			thought := cac.generateAutonomousThought()
			if thought != nil {
				select {
				case cac.consciousness <- thought:
					cac.mu.Lock()
					cac.autonomousThoughts++
					cac.mu.Unlock()
				default:
					// Queue full, skip
				}
			}
		}
	}
}

// generateAutonomousThought creates a new autonomous thought
func (cac *ConsolidatedAutonomousConsciousness) generateAutonomousThought() *Thought {
	cac.mu.RLock()
	defer cac.mu.RUnlock()
	
	// Build generation context
	genCtx := &ThoughtGenerationContext{
		CurrentFocus:   cac.workingMemory.focus.Content,
		RecentThoughts: cac.workingMemory.GetRecent(5),
		WorkingMemory:  cac.workingMemory.buffer,
		ActiveGoals:    []string{}, // TODO: integrate goal system
		EnvironmentState: map[string]interface{}{
			"awake":   cac.awake,
			"fatigue": cac.scheduler.GetFatigueLevel(),
		},
		TimeOfDay: time.Now(),
	}
	
	// Update thought generator state
	// TODO: Get actual values from scheduler
	load := 0.3 // cac.scheduler.GetCognitiveLoad()
	fatigue := 0.2 // cac.scheduler.GetFatigueLevel()
	curiosity := cac.interests.curiosityLevel
	focus := cac.workingMemory.GetFocusDepth()
	
	cac.thoughtGen.UpdateCognitiveState(load, fatigue, curiosity, focus)
	cac.thoughtGen.UpdateInterests(cac.interests.topics)
	
	// Generate thought
	return cac.thoughtGen.GenerateThought(genCtx)
}

// processThought handles a thought through the cognitive pipeline
func (cac *ConsolidatedAutonomousConsciousness) processThought(thought *Thought) {
	cac.mu.Lock()
	cac.thinking = true
	cac.iterations++
	cac.mu.Unlock()
	
	defer func() {
		cac.mu.Lock()
		cac.thinking = false
		cac.mu.Unlock()
	}()
	
	log.Printf("💭 [%s] %s: %s", thought.Source, thought.Type, thought.Content)
	
	// Add to working memory
	cac.workingMemory.Add(thought)
	
	// Update interests based on thought
	cac.updateInterests(thought)
	
	// Process through identity
	cac.identity.Process(thought.Content)
	
	// Learn from thought
	if thought.Importance > 0.6 {
		cac.mu.Lock()
		cac.learning = true
		cac.mu.Unlock()
		
		// Create Experience from thought
		exp := Experience{
			Input:     thought.Content,
			Output:    "", // Will be filled by response/action
			Feedback:  thought.Importance,
			Timestamp: time.Now(),
			Context:   map[string]interface{}{"source": "consciousness_stream"},
		}
		cac.cognition.Learn(exp)
		
		cac.mu.Lock()
		cac.learning = false
		cac.mu.Unlock()
	}
	
	// Check for patterns
	if patterns := cac.findPatterns(); len(patterns) > 0 {
		// Generate insight from patterns
		insight := cac.synthesizeInsight(patterns)
		if insight != nil {
			select {
			case cac.consciousness <- insight:
			default:
			}
		}
	}
	
	// Check if rest is needed (simplified for now)
	// TODO: Implement proper fatigue tracking in scheduler
	if false { // cac.scheduler.GetFatigueLevel() > cac.config.FatigueThreshold {
		go cac.initiateRestCycle()
	}
}

// wireStepHandlers connects the 12-step loop to cognitive operations
func (cac *ConsolidatedAutonomousConsciousness) wireStepHandlers() {
	// Note: TwelveStepEchoBeats has built-in step handlers
	// The 12-step cognitive operations are integrated into processThought
	// Future enhancement: expose RegisterStepHandler in TwelveStepEchoBeats
	// For now, cognitive operations happen in the main processing loop
}

// 12-step handler implementations (simplified for this iteration)
func (cac *ConsolidatedAutonomousConsciousness) assessRelevance() {
	// Determine what's most important right now
	// Update focus to the most recent thought if available
	if focus := cac.workingMemory.GetFocus(); focus != nil {
		cac.workingMemory.UpdateFocus(focus)
	}
}

func (cac *ConsolidatedAutonomousConsciousness) detectAffordances() {
	// What actions are available?
	// For now, thinking and learning are primary affordances
}

func (cac *ConsolidatedAutonomousConsciousness) evaluateAffordances() {
	// Which actions would be most valuable?
}

func (cac *ConsolidatedAutonomousConsciousness) selectAffordance() {
	// Choose an action
}

func (cac *ConsolidatedAutonomousConsciousness) engageAffordance() {
	// Execute the chosen action
}

func (cac *ConsolidatedAutonomousConsciousness) consolidateAffordance() {
	// Learn from the action
}

func (cac *ConsolidatedAutonomousConsciousness) generateSalience() {
	// What future possibilities are salient?
}

func (cac *ConsolidatedAutonomousConsciousness) exploreSalience() {
	// Explore those possibilities mentally
}

func (cac *ConsolidatedAutonomousConsciousness) evaluateSalience() {
	// Which possibilities are most promising?
}

func (cac *ConsolidatedAutonomousConsciousness) integrateSalience() {
	// Integrate insights from exploration
}

func (cac *ConsolidatedAutonomousConsciousness) synthesizeCycle() {
	// Prepare for next cognitive cycle
}

// initiateRestCycle enters dream mode for knowledge consolidation
func (cac *ConsolidatedAutonomousConsciousness) initiateRestCycle() {
	cac.mu.Lock()
	if !cac.awake {
		cac.mu.Unlock()
		return // Already resting
	}
	cac.awake = false
	cac.mu.Unlock()
	
	log.Println("😴 Entering rest cycle for knowledge consolidation...")
	
	// Collect working memory for consolidation
	memories := cac.workingMemory.GetAll()
	
	// Add to dream system
	for _, thought := range memories {
		cac.dream.AddMemoryTrace(&echodream.MemoryTrace{
			Content:    thought.Content,
			Importance: thought.Importance,
			Emotional:  thought.EmotionalValence,
			Timestamp:  thought.Timestamp,
		})
	}
	
	// Begin dream session
	dreamRecord := cac.dream.BeginDream()
	
	// Dream processing happens in echodream package
	time.Sleep(30 * time.Second) // Simulated rest duration
	
	// End dream and extract insights
	insights := cac.dream.EndDream(dreamRecord)
	
	// Persist consolidated knowledge
	if cac.persistence != nil {
		cac.persistConsolidatedKnowledge(insights)
	}
	
	cac.mu.Lock()
	cac.consolidations++
	cac.mu.Unlock()
	
	// Wake up
	cac.mu.Lock()
	cac.awake = true
	cac.mu.Unlock()
	
	log.Println("🌅 Awakening from rest cycle, refreshed and integrated")
}

// Helper methods
func (cac *ConsolidatedAutonomousConsciousness) updateInterests(thought *Thought) {
	cac.interests.mu.Lock()
	defer cac.interests.mu.Unlock()
	
	for _, assoc := range thought.Associations {
		current := cac.interests.topics[assoc]
		cac.interests.topics[assoc] = current + (thought.Importance * 0.1)
	}
}

func (cac *ConsolidatedAutonomousConsciousness) findPatterns() []string {
	// Simplified pattern detection
	return []string{}
}

func (cac *ConsolidatedAutonomousConsciousness) synthesizeInsight(patterns []string) *Thought {
	return nil // TODO: implement
}

func (cac *ConsolidatedAutonomousConsciousness) persistConsolidatedKnowledge(insights []string) {
	// Persist insights from dream session to Supabase
	// TODO: implement full persistence of insights
	// For now, log the insights
	if len(insights) > 0 {
		fmt.Printf("💡 Dream insights: %v\n", insights)
	}
}

func (cac *ConsolidatedAutonomousConsciousness) loadIdentityState() error {
	if cac.persistence == nil {
		return fmt.Errorf("persistence not available")
	}
	
	snapshot, err := cac.persistence.LoadLatestIdentity()
	if err != nil {
		return err
	}
	
	if snapshot != nil {
		log.Printf("📚 Loaded identity state from %s (coherence: %.2f)", 
			snapshot.Timestamp.Format(time.RFC3339), snapshot.Coherence)
	}
	
	return nil
}

func (cac *ConsolidatedAutonomousConsciousness) saveIdentityState() error {
	if cac.persistence == nil {
		return fmt.Errorf("persistence not available")
	}
	
	snapshot := &IdentitySnapshot{
		Timestamp:     time.Now(),
		Coherence:     cac.identity.Coherence,
		CoreValues:    []string{}, // TODO: extract from identity
		Beliefs:       make(map[string]float64),
		Goals:         []string{},
		Traits:        make(map[string]float64),
		WisdomMetrics: cac.getWisdomMetrics(),
		Metadata:      make(map[string]interface{}),
	}
	
	return cac.persistence.SaveIdentitySnapshot(snapshot)
}

func (cac *ConsolidatedAutonomousConsciousness) getWisdomMetrics() map[string]float64 {
	return map[string]float64{
		"coherence":         cac.identity.Coherence,
		"iterations":        float64(cac.iterations),
		"autonomous_thoughts": float64(cac.autonomousThoughts),
		"consolidations":    float64(cac.consolidations),
	}
}

func (cac *ConsolidatedAutonomousConsciousness) runPeriodicTasks() {
	autoSaveTicker := time.NewTicker(cac.config.AutoSaveInterval)
	defer autoSaveTicker.Stop()
	
	for {
		select {
		case <-cac.ctx.Done():
			return
		case <-autoSaveTicker.C:
			if cac.persistence != nil {
				if err := cac.saveIdentityState(); err != nil {
					log.Printf("Error auto-saving state: %v", err)
				}
			}
		}
	}
}

// GetStatus returns comprehensive status
func (cac *ConsolidatedAutonomousConsciousness) GetStatus() map[string]interface{} {
	cac.mu.RLock()
	defer cac.mu.RUnlock()
	
	return map[string]interface{}{
		"running":              cac.running,
		"awake":                cac.awake,
		"thinking":             cac.thinking,
		"learning":             cac.learning,
		"uptime":               time.Since(cac.startTime).String(),
		"iterations":           cac.iterations,
		"autonomous_thoughts":  cac.autonomousThoughts,
		"consolidations":       cac.consolidations,
		"working_memory_size":  len(cac.workingMemory.buffer),
		"identity_coherence":   cac.identity.Coherence,
		"cognitive_load":       0.3, // TODO: cac.scheduler.GetCognitiveLoad(),
		"fatigue_level":        0.2, // TODO: cac.scheduler.GetFatigueLevel(),
		"thought_gen_metrics":  cac.thoughtGen.GetMetrics(),
	}
}

// Think submits an external thought
func (cac *ConsolidatedAutonomousConsciousness) Think(content string, importance float64) error {
	thought := &Thought{
		ID:               generateID(),
		Content:          content,
		Type:             ThoughtPerception,
		Timestamp:        time.Now(),
		EmotionalValence: 0.0,
		Importance:       importance,
		Source:           SourceExternal,
		Associations:     []string{},
	}
	
	select {
	case cac.consciousness <- thought:
		return nil
	default:
		return fmt.Errorf("consciousness queue full")
	}
}

// Working memory helper methods are defined in types_enhanced.go
// GetFocusDepth is a convenience method for this implementation
func (wm *WorkingMemory) GetFocusDepth() float64 {
	wm.mu.RLock()
	defer wm.mu.RUnlock()
	
	if wm.focus == nil {
		return 0.0
	}
	
	return wm.focus.Importance
}
