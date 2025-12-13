package core

import (
	"context"
	"fmt"
	"sync"
	"time"
	
	"github.com/EchoCog/echollama/core/consciousness"
	"github.com/EchoCog/echollama/core/echobeats"
	"github.com/EchoCog/echollama/core/echodream"
	"github.com/EchoCog/echollama/core/llm"
)

// AutonomousEchoselfLLM is the fully integrated autonomous wisdom-cultivating system with LLM
type AutonomousEchoselfLLM struct {
	mu                    sync.RWMutex
	ctx                   context.Context
	cancel                context.CancelFunc
	
	// Core components
	llmManager            *llm.ProviderManager
	echoBeats             *echobeats.EchoBeats
	streamOfConsciousness *consciousness.StreamOfConsciousnessLLM
	dreamCycle            *echodream.DreamCycleIntegration
	interestPatterns      *echobeats.InterestPatternSystem
	discussionManager     *echobeats.DiscussionManager
	consciousnessSimulator *consciousness.ConsciousnessSimulator
	
	// State
	isAwake               bool
	currentState          EchoselfState
	
	// Configuration
	config                *EchoselfConfig
	
	// Metrics
	uptimeStart           time.Time
	cyclesCompleted       uint64
	wisdomCultivated      uint64
	autonomousActions     uint64
	thoughtsGenerated     uint64
	insightsGenerated     uint64
}

// EchoselfState represents the current state of echoself
type EchoselfState string

const (
	StateInitializing EchoselfState = "initializing"
	StateAsleep       EchoselfState = "asleep"
	StateWaking       EchoselfState = "waking"
	StateAwake        EchoselfState = "awake"
	StateThinking     EchoselfState = "thinking"
	StateResting      EchoselfState = "resting"
	StateDreaming     EchoselfState = "dreaming"
)

// EchoselfConfig holds configuration for the autonomous system
type EchoselfConfig struct {
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
	
	// LLM Configuration
	PreferredLLMProvider  string
	LLMFallbackChain      []string
	
	// Features
	EnableStreamOfConsciousness bool
	EnableAutonomousLearning    bool
	EnableDiscussions           bool
	EnableDreamCycles           bool
}

// DefaultEchoselfConfig returns default configuration
func DefaultEchoselfConfig() *EchoselfConfig {
	return &EchoselfConfig{
		PersistenceDir:              "/tmp/echoself",
		WakeCycleDuration:           4 * time.Hour,
		RestCycleDuration:           30 * time.Minute,
		DreamCycleDuration:          15 * time.Minute,
		FatigueThreshold:            0.8,
		EngagementThreshold:         0.5,
		CuriosityLevel:              0.8,
		PreferredLLMProvider:        "anthropic",
		LLMFallbackChain:            []string{"anthropic", "openrouter", "openai"},
		EnableStreamOfConsciousness: true,
		EnableAutonomousLearning:    true,
		EnableDiscussions:           true,
		EnableDreamCycles:           true,
	}
}

// NewAutonomousEchoselfLLM creates a new integrated autonomous system with LLM
func NewAutonomousEchoselfLLM(config *EchoselfConfig) (*AutonomousEchoselfLLM, error) {
	if config == nil {
		config = DefaultEchoselfConfig()
	}
	
	ctx, cancel := context.WithCancel(context.Background())
	
	// Initialize LLM Provider Manager
	llmManager := llm.NewProviderManager()
	
	// Register available providers
	anthropic := llm.NewAnthropicProvider("")
	if anthropic.Available() {
		llmManager.RegisterProvider(anthropic)
	}
	
	openrouter := llm.NewOpenRouterProvider("")
	if openrouter.Available() {
		llmManager.RegisterProvider(openrouter)
	}
	
	openai := llm.NewOpenAIProvider("")
	if openai.Available() {
		llmManager.RegisterProvider(openai)
	}
	
	// Set fallback chain
	if len(config.LLMFallbackChain) > 0 {
		if err := llmManager.SetFallbackChain(config.LLMFallbackChain); err != nil {
			// Fallback chain failed, but continue with available providers
		}
	}
	
	// Check if at least one provider is available
	if len(llmManager.ListProviders()) == 0 {
		cancel()
		return nil, fmt.Errorf("no LLM providers available - please set API keys")
	}
	
	// Initialize components
	echoBeats := echobeats.NewEchoBeats()
	
	var soc *consciousness.StreamOfConsciousnessLLM
	if config.EnableStreamOfConsciousness {
		socPath := config.PersistenceDir + "/stream_of_consciousness_llm.json"
		soc = consciousness.NewStreamOfConsciousnessLLM(llmManager, socPath)
	}
	
	var dreamCycle *echodream.DreamCycleIntegration
	if config.EnableDreamCycles {
		dreamCycle = echodream.NewDreamCycleIntegration()
	}
	
	interestPath := config.PersistenceDir + "/interests.json"
	interestPatterns := echobeats.NewInterestPatternSystem(interestPath)
	
	var discussionManager *echobeats.DiscussionManager
	if config.EnableDiscussions {
		discussionPath := config.PersistenceDir + "/discussions.json"
		discussionManager = echobeats.NewDiscussionManager(interestPatterns, discussionPath)
	}
	
	consciousnessSimulator := consciousness.NewConsciousnessSimulator()
	
	ae := &AutonomousEchoselfLLM{
		ctx:                    ctx,
		cancel:                 cancel,
		llmManager:             llmManager,
		echoBeats:              echoBeats,
		streamOfConsciousness:  soc,
		dreamCycle:             dreamCycle,
		interestPatterns:       interestPatterns,
		discussionManager:      discussionManager,
		consciousnessSimulator: consciousnessSimulator,
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
func (ae *AutonomousEchoselfLLM) setupIntegrations() {
	// Connect dream cycle to wisdom extraction
	if ae.dreamCycle != nil {
		ae.dreamCycle.OnWisdomExtracted(func(wisdom string, confidence float64) {
			ae.mu.Lock()
			ae.wisdomCultivated++
			ae.mu.Unlock()
			
			// Add wisdom as experience to stream of consciousness
			if ae.streamOfConsciousness != nil {
				ae.streamOfConsciousness.AddExperience(fmt.Sprintf("Wisdom extracted: %s", wisdom))
			}
		})
		
		ae.dreamCycle.OnDreamComplete(func(narrative string) {
			// Dream completed, update state
			if ae.streamOfConsciousness != nil {
				ae.streamOfConsciousness.AddExperience(fmt.Sprintf("Dream completed: %s", narrative))
			}
		})
	}
	
	// Connect EchoBeats events to stream of consciousness
	if ae.echoBeats != nil && ae.streamOfConsciousness != nil {
		// Register event handlers
		ae.echoBeats.RegisterHandler("wake", func(event echobeats.Event) {
			ae.handleWakeEvent(event)
		})
		
		ae.echoBeats.RegisterHandler("rest", func(event echobeats.Event) {
			ae.handleRestEvent(event)
		})
		
		ae.echoBeats.RegisterHandler("dream", func(event echobeats.Event) {
			ae.handleDreamEvent(event)
		})
		
		ae.echoBeats.RegisterHandler("thought", func(event echobeats.Event) {
			ae.handleThoughtEvent(event)
		})
		
		ae.echoBeats.RegisterHandler("learning", func(event echobeats.Event) {
			ae.handleLearningEvent(event)
		})
	}
}

// Start begins autonomous operation
func (ae *AutonomousEchoselfLLM) Start() error {
	ae.mu.Lock()
	if ae.isAwake {
		ae.mu.Unlock()
		return fmt.Errorf("echoself already awake")
	}
	ae.currentState = StateWaking
	ae.mu.Unlock()
	
	// Start stream of consciousness
	if ae.streamOfConsciousness != nil {
		if err := ae.streamOfConsciousness.Start(); err != nil {
			return fmt.Errorf("failed to start stream of consciousness: %w", err)
		}
	}
	
	// Start EchoBeats scheduler
	if ae.echoBeats != nil {
		ae.echoBeats.Start()
	}
	
	// Start consciousness simulator
	if ae.consciousnessSimulator != nil {
		go ae.consciousnessSimulationLoop()
	}
	
	// Start interest decay
	go ae.interestDecayLoop()
	
	ae.mu.Lock()
	ae.isAwake = true
	ae.currentState = StateAwake
	ae.mu.Unlock()
	
	return nil
}

// Stop halts autonomous operation
func (ae *AutonomousEchoselfLLM) Stop() {
	ae.mu.Lock()
	ae.isAwake = false
	ae.currentState = StateAsleep
	ae.mu.Unlock()
	
	// Stop stream of consciousness
	if ae.streamOfConsciousness != nil {
		ae.streamOfConsciousness.Stop()
	}
	
	// Stop EchoBeats
	if ae.echoBeats != nil {
		ae.echoBeats.Stop()
	}
	
	ae.cancel()
}

// Event handlers

func (ae *AutonomousEchoselfLLM) handleWakeEvent(event echobeats.Event) {
	ae.mu.Lock()
	ae.currentState = StateWaking
	ae.mu.Unlock()
	
	if ae.streamOfConsciousness != nil {
		ae.streamOfConsciousness.AddExperience("Waking up from rest cycle")
	}
	
	ae.mu.Lock()
	ae.currentState = StateAwake
	ae.mu.Unlock()
}

func (ae *AutonomousEchoselfLLM) handleRestEvent(event echobeats.Event) {
	ae.mu.Lock()
	ae.currentState = StateResting
	ae.mu.Unlock()
	
	if ae.streamOfConsciousness != nil {
		ae.streamOfConsciousness.AddExperience("Entering rest cycle for consolidation")
	}
}

func (ae *AutonomousEchoselfLLM) handleDreamEvent(event echobeats.Event) {
	ae.mu.Lock()
	ae.currentState = StateDreaming
	ae.mu.Unlock()
	
	if ae.streamOfConsciousness != nil {
		ae.streamOfConsciousness.AddExperience("Beginning dream cycle for knowledge integration")
	}
	
	// Trigger dream cycle if available
	if ae.dreamCycle != nil {
		// Get recent thoughts to consolidate
		if ae.streamOfConsciousness != nil {
			recentThoughts := ae.streamOfConsciousness.GetRecentThoughts(20)
			// Convert to episodic memories and trigger dream
			// (Implementation would depend on dreamCycle interface)
		}
	}
}

func (ae *AutonomousEchoselfLLM) handleThoughtEvent(event echobeats.Event) {
	ae.mu.Lock()
	ae.autonomousActions++
	ae.mu.Unlock()
}

func (ae *AutonomousEchoselfLLM) handleLearningEvent(event echobeats.Event) {
	if ae.streamOfConsciousness != nil {
		if content, ok := event.Data["content"].(string); ok {
			ae.streamOfConsciousness.AddExperience(fmt.Sprintf("Learning: %s", content))
		}
	}
}

// Background loops

func (ae *AutonomousEchoselfLLM) consciousnessSimulationLoop() {
	ticker := time.NewTicker(30 * time.Second)
	defer ticker.Stop()
	
	for {
		select {
		case <-ae.ctx.Done():
			return
		case <-ticker.C:
			if ae.consciousnessSimulator != nil {
				ae.consciousnessSimulator.Simulate(0.1)
			}
		}
	}
}

func (ae *AutonomousEchoselfLLM) interestDecayLoop() {
	ticker := time.NewTicker(1 * time.Hour)
	defer ticker.Stop()
	
	for {
		select {
		case <-ae.ctx.Done():
			return
		case <-ticker.C:
			if ae.interestPatterns != nil {
				ae.interestPatterns.DecayInterests(0.05)
			}
		}
	}
}

// ProcessExternalInput processes input from external sources
func (ae *AutonomousEchoselfLLM) ProcessExternalInput(input string) error {
	ae.mu.RLock()
	if !ae.isAwake {
		ae.mu.RUnlock()
		return fmt.Errorf("echoself is asleep")
	}
	ae.mu.RUnlock()
	
	// Add to stream of consciousness
	if ae.streamOfConsciousness != nil {
		ae.streamOfConsciousness.AddExperience(input)
	}
	
	// Update interest patterns
	if ae.interestPatterns != nil {
		ae.interestPatterns.RecordEngagement(input, 0.5, 0.5)
	}
	
	return nil
}

// GetMetrics returns current system metrics
func (ae *AutonomousEchoselfLLM) GetMetrics() map[string]interface{} {
	ae.mu.RLock()
	defer ae.mu.RUnlock()
	
	metrics := map[string]interface{}{
		"state":              string(ae.currentState),
		"is_awake":           ae.isAwake,
		"uptime":             time.Since(ae.uptimeStart).String(),
		"cycles_completed":   ae.cyclesCompleted,
		"wisdom_cultivated":  ae.wisdomCultivated,
		"autonomous_actions": ae.autonomousActions,
	}
	
	// Add stream of consciousness metrics
	if ae.streamOfConsciousness != nil {
		socMetrics := ae.streamOfConsciousness.GetMetrics()
		for k, v := range socMetrics {
			metrics["soc_"+k] = v
		}
	}
	
	// Add LLM provider metrics
	if ae.llmManager != nil {
		llmMetrics := ae.llmManager.GetMetrics()
		metrics["llm_providers"] = llmMetrics
	}
	
	return metrics
}

// GetRecentThoughts returns recent thoughts from stream of consciousness
func (ae *AutonomousEchoselfLLM) GetRecentThoughts(n int) []*consciousness.ThoughtLLM {
	if ae.streamOfConsciousness == nil {
		return []*consciousness.ThoughtLLM{}
	}
	return ae.streamOfConsciousness.GetRecentThoughts(n)
}

// GetState returns current state
func (ae *AutonomousEchoselfLLM) GetState() EchoselfState {
	ae.mu.RLock()
	defer ae.mu.RUnlock()
	return ae.currentState
}

// IsAwake returns whether echoself is currently awake
func (ae *AutonomousEchoselfLLM) IsAwake() bool {
	ae.mu.RLock()
	defer ae.mu.RUnlock()
	return ae.isAwake
}
