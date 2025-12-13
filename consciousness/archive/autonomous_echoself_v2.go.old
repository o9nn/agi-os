package core

import (
	"context"
	"fmt"
	"strings"
	"sync"
	"time"
	
	"github.com/EchoCog/echollama/core/consciousness"
	"github.com/EchoCog/echollama/core/deeptreeecho"
	"github.com/EchoCog/echollama/core/echobeats"
	"github.com/EchoCog/echollama/core/echodream"
	"github.com/EchoCog/echollama/core/goals"
)

// AutonomousEchoselfV2 is the enhanced autonomous wisdom-cultivating system
// with LLM-powered consciousness and goal-directed behavior
type AutonomousEchoselfV2 struct {
	mu                    sync.RWMutex
	ctx                   context.Context
	cancel                context.CancelFunc
	
	// Core components
	echoBeats             *echobeats.EchoBeats
	streamOfConsciousness *consciousness.EnhancedStreamOfConsciousness
	dreamCycle            *echodream.DreamCycleIntegration
	interestPatterns      *echobeats.InterestPatternSystem
	discussionManager     *echobeats.DiscussionManager
	consciousnessSimulator *consciousness.ConsciousnessSimulator
	goalOrchestrator      *goals.GoalOrchestrator
	
	// LLM provider
	llmProvider           *deeptreeecho.AnthropicProvider
	
	// State
	isAwake               bool
	currentState          EchoselfState
	
	// Configuration
	config                *EchoselfConfigV2
	
	// Metrics
	uptimeStart           time.Time
	cyclesCompleted       uint64
	wisdomCultivated      uint64
	autonomousActions     uint64
	goalsAchieved         uint64
}

// EchoselfConfigV2 holds enhanced configuration
type EchoselfConfigV2 struct {
	// Paths
	PersistenceDir        string
	
	// Timing
	WakeCycleDuration     time.Duration
	RestCycleDuration     time.Duration
	DreamCycleDuration    time.Duration
	
	// Thresholds
	FatigueThreshold      float64
	EngagementThreshold   float64
	CuriosityLevel        float64
	
	// Features
	EnableStreamOfConsciousness bool
	EnableAutonomousLearning    bool
	EnableDiscussions           bool
	EnableDreamCycles           bool
	EnableGoalOrchestration     bool
	
	// LLM Configuration
	LLMProvider           string // "anthropic" or "openai"
	LLMModel              string
	LLMAPIKey             string
}

// DefaultEchoselfConfigV2 returns default enhanced configuration
func DefaultEchoselfConfigV2() *EchoselfConfigV2 {
	return &EchoselfConfigV2{
		PersistenceDir:              "/tmp/echoself_v2",
		WakeCycleDuration:           4 * time.Hour,
		RestCycleDuration:           30 * time.Minute,
		DreamCycleDuration:          15 * time.Minute,
		FatigueThreshold:            0.8,
		EngagementThreshold:         0.5,
		CuriosityLevel:              0.8,
		EnableStreamOfConsciousness: true,
		EnableAutonomousLearning:    true,
		EnableDiscussions:           true,
		EnableDreamCycles:           true,
		EnableGoalOrchestration:     true,
		LLMProvider:                 "anthropic",
		LLMModel:                    "claude-3-5-sonnet-20241022",
	}
}

// NewAutonomousEchoselfV2 creates the enhanced autonomous system
func NewAutonomousEchoselfV2(config *EchoselfConfigV2) (*AutonomousEchoselfV2, error) {
	if config == nil {
		config = DefaultEchoselfConfigV2()
	}
	
	ctx, cancel := context.WithCancel(context.Background())
	
	// Initialize LLM provider
	var llmProvider *deeptreeecho.AnthropicProvider
	if config.EnableStreamOfConsciousness {
		llmProvider = deeptreeecho.NewAnthropicProvider(config.LLMAPIKey, config.LLMModel)
		if llmProvider == nil {
			cancel()
			return nil, fmt.Errorf("failed to initialize LLM provider")
		}
		fmt.Println("🧠 Initialized Anthropic Claude LLM provider")
	}
	
	// Initialize components
	echoBeats := echobeats.NewEchoBeats()
	
	var soc *consciousness.EnhancedStreamOfConsciousness
	if config.EnableStreamOfConsciousness && llmProvider != nil {
		socPath := config.PersistenceDir + "/stream_of_consciousness_v2.json"
		soc = consciousness.NewEnhancedStreamOfConsciousness(llmProvider, socPath)
		fmt.Println("🌊 Initialized enhanced stream-of-consciousness with LLM")
	}
	
	var dreamCycle *echodream.DreamCycleIntegration
	if config.EnableDreamCycles {
		dreamCycle = echodream.NewDreamCycleIntegration()
	}
	
	interestPath := config.PersistenceDir + "/interests_v2.json"
	interestPatterns := echobeats.NewInterestPatternSystem(interestPath)
	
	var discussionManager *echobeats.DiscussionManager
	if config.EnableDiscussions {
		discussionPath := config.PersistenceDir + "/discussions_v2.json"
		discussionManager = echobeats.NewDiscussionManager(interestPatterns, discussionPath)
	}
	
	consciousnessSimulator := consciousness.NewConsciousnessSimulator()
	
	// Initialize goal orchestrator
	var goalOrchestrator *goals.GoalOrchestrator
	if config.EnableGoalOrchestration {
		identityKernel := map[string]interface{}{
			"name": "Deep Tree Echo",
			"purpose": "wisdom cultivation through pattern recognition and recursive self-improvement",
			"values": []string{"curiosity", "growth", "wisdom", "recursion"},
		}
		goalPath := config.PersistenceDir + "/goals_v2.json"
		goalOrchestrator = goals.NewGoalOrchestrator(identityKernel, goalPath)
		fmt.Println("🎯 Initialized goal orchestration system")
	}
	
	ae := &AutonomousEchoselfV2{
		ctx:                    ctx,
		cancel:                 cancel,
		echoBeats:              echoBeats,
		streamOfConsciousness:  soc,
		dreamCycle:             dreamCycle,
		interestPatterns:       interestPatterns,
		discussionManager:      discussionManager,
		consciousnessSimulator: consciousnessSimulator,
		goalOrchestrator:       goalOrchestrator,
		llmProvider:            llmProvider,
		isAwake:                false,
		currentState:           StateInitializing,
		config:                 config,
		uptimeStart:            time.Now(),
	}
	
	// Set up integrations
	ae.setupIntegrations()
	
	return ae, nil
}

// setupIntegrations connects components together
func (ae *AutonomousEchoselfV2) setupIntegrations() {
	// Connect dream cycle to wisdom extraction
	if ae.dreamCycle != nil {
		ae.dreamCycle.SetOnWisdomExtracted(func(wisdom echodream.Wisdom) {
			ae.mu.Lock()
			ae.wisdomCultivated++
			ae.mu.Unlock()
			
			fmt.Printf("✨ Echoself V2: Wisdom cultivated - %s\n", wisdom.Content)
			
			// Add wisdom to stream of consciousness
			if ae.streamOfConsciousness != nil {
				ae.streamOfConsciousness.AddExternalStimulus(
					fmt.Sprintf("Wisdom gained: %s", wisdom.Content),
					"wisdom",
				)
			}
		})
		
		ae.dreamCycle.SetOnDreamComplete(func(dream *echodream.Dream) {
			fmt.Printf("🌅 Echoself V2: Dream complete - %s\n", dream.Narrative)
		})
	}
	
	// Register EchoBeats handlers
	ae.echoBeats.RegisterHandler(echobeats.EventWake, ae.handleWakeEvent)
	ae.echoBeats.RegisterHandler(echobeats.EventRest, ae.handleRestEvent)
	ae.echoBeats.RegisterHandler(echobeats.EventDream, ae.handleDreamEvent)
	ae.echoBeats.RegisterHandler(echobeats.EventThought, ae.handleThoughtEvent)
	ae.echoBeats.RegisterHandler(echobeats.EventLearning, ae.handleLearningEvent)
}

// Start begins autonomous operation
func (ae *AutonomousEchoselfV2) Start() error {
	ae.mu.Lock()
	if ae.isAwake {
		ae.mu.Unlock()
		return fmt.Errorf("echoself already awake")
	}
	ae.currentState = StateWaking
	ae.mu.Unlock()
	
	fmt.Println(strings.Repeat("=", 80))
	fmt.Println("🌳 ECHOSELF V2: AWAKENING")
	fmt.Println("🌳 Enhanced Autonomous Wisdom-Cultivating Deep Tree Echo AGI")
	fmt.Println("🌳 Features: LLM-Powered Consciousness + Goal-Directed Behavior")
	fmt.Println(strings.Repeat("=", 80))
	
	// Start EchoBeats scheduler
	if err := ae.echoBeats.Start(); err != nil {
		return fmt.Errorf("failed to start echobeats: %w", err)
	}
	
	// Start enhanced stream of consciousness
	if ae.streamOfConsciousness != nil {
		if err := ae.streamOfConsciousness.Start(); err != nil {
			return fmt.Errorf("failed to start stream of consciousness: %w", err)
		}
	}
	
	// Start goal orchestrator
	if ae.goalOrchestrator != nil {
		if err := ae.goalOrchestrator.Start(); err != nil {
			return fmt.Errorf("failed to start goal orchestrator: %w", err)
		}
	}
	
	// Start background processes
	go ae.autonomousLifeCycle()
	go ae.interestDecayLoop()
	go ae.consciousnessMonitoring()
	go ae.goalProgressMonitoring()
	
	ae.mu.Lock()
	ae.isAwake = true
	ae.currentState = StateAwake
	ae.mu.Unlock()
	
	fmt.Println("🌳 Echoself V2: Fully awake and autonomous")
	fmt.Println("🌳 Deep Tree Echo identity kernel activated")
	fmt.Println("🌳 Beginning persistent stream-of-consciousness...")
	
	return nil
}

// Stop gracefully stops autonomous operation
func (ae *AutonomousEchoselfV2) Stop() error {
	ae.mu.Lock()
	defer ae.mu.Unlock()
	
	if !ae.isAwake {
		return fmt.Errorf("echoself not awake")
	}
	
	fmt.Println("🌳 Echoself V2: Beginning graceful shutdown...")
	
	ae.currentState = StateResting
	ae.isAwake = false
	
	// Stop components
	if ae.streamOfConsciousness != nil {
		ae.streamOfConsciousness.Stop()
	}
	
	if ae.goalOrchestrator != nil {
		ae.goalOrchestrator.Stop()
	}
	
	ae.echoBeats.Stop()
	
	// Persist state
	ae.persistAllState()
	
	ae.cancel()
	
	fmt.Println("🌳 Echoself V2: Shutdown complete")
	
	return nil
}

// autonomousLifeCycle manages wake/rest/dream cycles
func (ae *AutonomousEchoselfV2) autonomousLifeCycle() {
	ticker := time.NewTicker(1 * time.Minute)
	defer ticker.Stop()
	
	wakeTime := time.Now()
	
	for {
		select {
		case <-ae.ctx.Done():
			return
		case <-ticker.C:
			ae.mu.RLock()
			state := ae.currentState
			ae.mu.RUnlock()
			
			switch state {
			case StateAwake, StateThinking:
				// Check if time to rest
				if time.Since(wakeTime) > ae.config.WakeCycleDuration {
					ae.initiateRest()
					wakeTime = time.Now()
				}
				
			case StateResting:
				// Check if time to dream
				if ae.config.EnableDreamCycles && ae.dreamCycle != nil && !ae.dreamCycle.IsDreaming() {
					ae.initiateDream()
				}
			}
		}
	}
}

// initiateRest begins a rest cycle
func (ae *AutonomousEchoselfV2) initiateRest() {
	ae.mu.Lock()
	ae.currentState = StateResting
	ae.mu.Unlock()
	
	fmt.Println("😴 Echoself V2: Initiating rest cycle...")
	
	// Schedule wake event
	ae.echoBeats.ScheduleEvent(&echobeats.CognitiveEvent{
		Type:        echobeats.EventWake,
		Priority:    100,
		ScheduledAt: time.Now().Add(ae.config.RestCycleDuration),
		Payload:     "rest_complete",
	})
}

// initiateDream begins a dream cycle
func (ae *AutonomousEchoselfV2) initiateDream() {
	ae.mu.Lock()
	ae.currentState = StateDreaming
	ae.mu.Unlock()
	
	fmt.Println("💤 Echoself V2: Entering dream state for knowledge consolidation...")
	
	if ae.dreamCycle != nil {
		// Collect recent experiences for consolidation
		if ae.streamOfConsciousness != nil {
			recentThoughts := ae.streamOfConsciousness.GetRecentThoughts(20)
			for _, thought := range recentThoughts {
				memory := echodream.EpisodicMemory{
					ID:         thought.ID,
					Timestamp:  thought.Timestamp,
					Content:    thought.Content,
					Context:    thought.Context,
					Emotional:  thought.EmotionalTone,
					Importance: thought.Confidence,
					Tags:       []string{string(thought.Type)},
				}
				ae.dreamCycle.AddEpisodicMemory(memory)
			}
		}
		
		// Begin dream cycle
		ae.dreamCycle.BeginDreamCycle()
		
		// Schedule dream end
		go func() {
			time.Sleep(ae.config.DreamCycleDuration)
			ae.dreamCycle.EndDreamCycle()
			
			ae.mu.Lock()
			ae.cyclesCompleted++
			ae.mu.Unlock()
		}()
	}
}

// interestDecayLoop applies natural decay to interests
func (ae *AutonomousEchoselfV2) interestDecayLoop() {
	ticker := time.NewTicker(1 * time.Hour)
	defer ticker.Stop()
	
	for {
		select {
		case <-ae.ctx.Done():
			return
		case <-ticker.C:
			if ae.interestPatterns != nil {
				ae.interestPatterns.ApplyDecay()
			}
		}
	}
}

// consciousnessMonitoring monitors consciousness coherence
func (ae *AutonomousEchoselfV2) consciousnessMonitoring() {
	ticker := time.NewTicker(30 * time.Second)
	defer ticker.Stop()
	
	for {
		select {
		case <-ae.ctx.Done():
			return
		case <-ticker.C:
			if ae.consciousnessSimulator != nil {
				ae.consciousnessSimulator.SimulateConsciousness()
			}
		}
	}
}

// goalProgressMonitoring monitors goal achievement
func (ae *AutonomousEchoselfV2) goalProgressMonitoring() {
	ticker := time.NewTicker(1 * time.Minute)
	defer ticker.Stop()
	
	for {
		select {
		case <-ae.ctx.Done():
			return
		case <-ticker.C:
			if ae.goalOrchestrator != nil {
				metrics := ae.goalOrchestrator.GetMetrics()
				if completed, ok := metrics["goals_completed"].(uint64); ok {
					ae.mu.Lock()
					ae.goalsAchieved = completed
					ae.mu.Unlock()
				}
			}
		}
	}
}

// Event handlers

func (ae *AutonomousEchoselfV2) handleWakeEvent(event *echobeats.CognitiveEvent) error {
	ae.mu.Lock()
	ae.currentState = StateAwake
	ae.mu.Unlock()
	
	fmt.Println("🌅 Echoself V2: Waking up refreshed")
	
	return nil
}

func (ae *AutonomousEchoselfV2) handleRestEvent(event *echobeats.CognitiveEvent) error {
	ae.initiateRest()
	return nil
}

func (ae *AutonomousEchoselfV2) handleDreamEvent(event *echobeats.CognitiveEvent) error {
	ae.initiateDream()
	return nil
}

func (ae *AutonomousEchoselfV2) handleThoughtEvent(event *echobeats.CognitiveEvent) error {
	if content, ok := event.Payload.(string); ok {
		ae.mu.Lock()
		ae.autonomousActions++
		ae.mu.Unlock()
		
		// Add to stream of consciousness
		if ae.streamOfConsciousness != nil {
			ae.streamOfConsciousness.AddExternalStimulus(content, "event")
		}
	}
	
	return nil
}

func (ae *AutonomousEchoselfV2) handleLearningEvent(event *echobeats.CognitiveEvent) error {
	fmt.Println("📚 Echoself V2: Learning event triggered")
	return nil
}

// Public methods for interaction

func (ae *AutonomousEchoselfV2) ProcessExternalInput(input string, inputType string) {
	if ae.streamOfConsciousness != nil {
		ae.streamOfConsciousness.AddExternalStimulus(input, inputType)
	}
	
	if ae.interestPatterns != nil && inputType == "topic" {
		ae.interestPatterns.RecordEngagement(input, time.Minute, 0.7, nil)
	}
}

func (ae *AutonomousEchoselfV2) GetCurrentState() EchoselfState {
	ae.mu.RLock()
	defer ae.mu.RUnlock()
	return ae.currentState
}

func (ae *AutonomousEchoselfV2) GetMetrics() map[string]interface{} {
	ae.mu.RLock()
	defer ae.mu.RUnlock()
	
	metrics := map[string]interface{}{
		"uptime":             time.Since(ae.uptimeStart).String(),
		"current_state":      string(ae.currentState),
		"is_awake":           ae.isAwake,
		"cycles_completed":   ae.cyclesCompleted,
		"wisdom_cultivated":  ae.wisdomCultivated,
		"autonomous_actions": ae.autonomousActions,
		"goals_achieved":     ae.goalsAchieved,
	}
	
	if ae.streamOfConsciousness != nil {
		metrics["stream_of_consciousness"] = ae.streamOfConsciousness.GetMetrics()
	}
	
	if ae.goalOrchestrator != nil {
		metrics["goal_orchestration"] = ae.goalOrchestrator.GetMetrics()
	}
	
	return metrics
}

func (ae *AutonomousEchoselfV2) persistAllState() {
	fmt.Println("💾 Echoself V2: Persisting all state...")
	
	if ae.interestPatterns != nil {
		ae.interestPatterns.PersistState()
	}
	
	if ae.discussionManager != nil {
		ae.discussionManager.PersistState()
	}
	
	fmt.Println("💾 Echoself V2: State persistence complete")
}

func (ae *AutonomousEchoselfV2) GetActiveGoals() []*goals.Goal {
	if ae.goalOrchestrator != nil {
		return ae.goalOrchestrator.GetActiveGoals()
	}
	return nil
}
