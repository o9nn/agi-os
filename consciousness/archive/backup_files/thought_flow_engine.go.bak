package deeptreeecho

import (
	"context"
	"fmt"
	"sync"
	"time"
)

// ThoughtFlowEngine generates thoughts that flow naturally from previous context
// rather than being timer-based and disconnected
type ThoughtFlowEngine struct {
	mu              sync.RWMutex
	ctx             context.Context
	
	// LLM thought generator
	generator       *LLMThoughtGeneratorV5
	
	// Working memory for context
	workingMemory   *WorkingMemory
	
	// Interest system for topic selection
	interests       *InterestPatterns
	
	// Cognitive state for mood/energy awareness
	cognitiveState  *CognitiveState
	
	// Wisdom metrics for growth tracking
	wisdomMetrics   *WisdomMetrics
	
	// Flow state
	flowActive      bool
	lastThought     *Thought
	thoughtChain    []*Thought
	chainDepth      int
	maxChainDepth   int
	
	// Timing
	minThinkInterval time.Duration
	maxThinkInterval time.Duration
	lastThinkTime    time.Time
	
	// Metrics
	thoughtsFlowed   int64
	chainsCompleted  int64
	avgChainLength   float64
}

// NewThoughtFlowEngine creates a new flow-based thought generation engine
func NewThoughtFlowEngine(
	ctx context.Context,
	generator *LLMThoughtGeneratorV5,
	workingMemory *WorkingMemory,
	interests *InterestPatterns,
	cognitiveState *CognitiveState,
	wisdomMetrics *WisdomMetrics,
) *ThoughtFlowEngine {
	return &ThoughtFlowEngine{
		ctx:              ctx,
		generator:        generator,
		workingMemory:    workingMemory,
		interests:        interests,
		cognitiveState:   cognitiveState,
		wisdomMetrics:    wisdomMetrics,
		flowActive:       false,
		thoughtChain:     make([]*Thought, 0),
		chainDepth:       0,
		maxChainDepth:    5, // Complete a thought before starting new chain
		minThinkInterval: 2 * time.Second,
		maxThinkInterval: 8 * time.Second,
		lastThinkTime:    time.Now(),
	}
}

// Start begins the thought flow
func (tfe *ThoughtFlowEngine) Start() error {
	tfe.mu.Lock()
	if tfe.flowActive {
		tfe.mu.Unlock()
		return fmt.Errorf("thought flow already active")
	}
	tfe.flowActive = true
	tfe.mu.Unlock()
	
	go tfe.flowLoop()
	
	fmt.Println("🌊 Thought Flow Engine: Started")
	return nil
}

// Stop ends the thought flow
func (tfe *ThoughtFlowEngine) Stop() {
	tfe.mu.Lock()
	tfe.flowActive = false
	tfe.mu.Unlock()
	
	fmt.Println("🌊 Thought Flow Engine: Stopped")
}

// flowLoop is the main thought generation loop
func (tfe *ThoughtFlowEngine) flowLoop() {
	defer func() {
		if r := recover(); r != nil {
			fmt.Printf("🚨 PANIC in thought flow loop: %v\n", r)
			// Attempt to restart after delay
			time.Sleep(5 * time.Second)
			go tfe.flowLoop()
		}
	}()
	
	for {
		select {
		case <-tfe.ctx.Done():
			return
		default:
			tfe.mu.RLock()
			active := tfe.flowActive
			tfe.mu.RUnlock()
			
			if !active {
				return
			}
			
			// Determine next think time based on cognitive state
			thinkInterval := tfe.determineThinkInterval()
			
			// Wait until it's time to think
			timeSinceLastThought := time.Since(tfe.lastThinkTime)
			if timeSinceLastThought < thinkInterval {
				time.Sleep(thinkInterval - timeSinceLastThought)
			}
			
			// Generate next thought in flow
			err := tfe.generateNextThought()
			if err != nil {
				fmt.Printf("⚠️  Flow generation error: %v\n", err)
				// Don't stop the flow, just log and continue
			}
			
			tfe.lastThinkTime = time.Now()
		}
	}
}

// determineThinkInterval calculates how long to wait before next thought
// based on cognitive state (arousal, clarity, etc.)
func (tfe *ThoughtFlowEngine) determineThinkInterval() time.Duration {
	if tfe.cognitiveState == nil {
		return 3 * time.Second // Default
	}
	
	tfe.cognitiveState.mu.RLock()
	arousal := tfe.cognitiveState.arousal
	clarity := tfe.cognitiveState.clarity
	tfe.cognitiveState.mu.RUnlock()
	
	// High arousal and clarity = faster thinking
	// Low arousal or clarity = slower thinking
	factor := (arousal + clarity) / 2.0
	
	// Interpolate between min and max interval
	interval := tfe.maxThinkInterval - time.Duration(factor*float64(tfe.maxThinkInterval-tfe.minThinkInterval))
	
	// Clamp to valid range
	if interval < tfe.minThinkInterval {
		interval = tfe.minThinkInterval
	}
	if interval > tfe.maxThinkInterval {
		interval = tfe.maxThinkInterval
	}
	
	return interval
}

// generateNextThought generates the next thought in the flow
func (tfe *ThoughtFlowEngine) generateNextThought() error {
	tfe.mu.Lock()
	defer tfe.mu.Unlock()
	
	// Determine thought type based on chain position
	thoughtType := tfe.determineNextThoughtType()
	
	// Get current interests
	interests := tfe.interests.GetPatterns()
	
	// Get working memory for context
	tfe.workingMemory.mu.RLock()
	workingMem := make([]*Thought, len(tfe.workingMemory.buffer))
	copy(workingMem, tfe.workingMemory.buffer)
	tfe.workingMemory.mu.RUnlock()
	
	// Generate thought using LLM
	thought, err := tfe.generator.GenerateAutonomousThought(
		thoughtType,
		workingMem,
		interests,
		tfe.cognitiveState,
		tfe.wisdomMetrics,
	)
	
	if err != nil {
		return fmt.Errorf("failed to generate thought: %w", err)
	}
	
	// Add to chain
	tfe.thoughtChain = append(tfe.thoughtChain, thought)
	tfe.chainDepth++
	tfe.lastThought = thought
	tfe.thoughtsFlowed++
	
	// Add to working memory
	tfe.workingMemory.AddThought(thought)
	
	// Check if chain is complete
	if tfe.chainDepth >= tfe.maxChainDepth {
		tfe.completeChain()
	}
	
	// Log thought (for debugging/monitoring)
	fmt.Printf("💭 [%s] %s\n", thoughtType, thought.Content)
	
	return nil
}

// determineNextThoughtType decides what type of thought should come next
// based on chain position and cognitive state
func (tfe *ThoughtFlowEngine) determineNextThoughtType() ThoughtType {
	// Start of chain: often a question or observation
	if tfe.chainDepth == 0 {
		if tfe.cognitiveState != nil {
			tfe.cognitiveState.mu.RLock()
			openness := tfe.cognitiveState.openness
			tfe.cognitiveState.mu.RUnlock()
			
			if openness > 0.7 {
				return ThoughtQuestion // Curious
			}
		}
		return ThoughtReflection // Observational
	}
	
	// Middle of chain: exploration and connection
	if tfe.chainDepth < tfe.maxChainDepth-1 {
		// Alternate between exploration and synthesis
		if tfe.chainDepth%2 == 0 {
			return ThoughtImagination // Explore possibilities
		}
		return ThoughtInsight // Connect ideas
	}
	
	// End of chain: synthesis or meta-cognition
	if tfe.chainDepth == tfe.maxChainDepth-1 {
		// Decide between insight and meta-cognition
		if tfe.wisdomMetrics != nil {
			tfe.wisdomMetrics.mu.RLock()
			reflection := tfe.wisdomMetrics.ReflectionCapacity
			tfe.wisdomMetrics.mu.RUnlock()
			
			if reflection > 0.6 {
				return ThoughtMetaCognitive // Reflect on thinking process
			}
		}
		return ThoughtInsight // Synthesize chain
	}
	
	return ThoughtReflection // Default
}

// completeChain finalizes the current thought chain
func (tfe *ThoughtFlowEngine) completeChain() {
	// Update metrics
	tfe.chainsCompleted++
	
	// Update average chain length
	if tfe.chainsCompleted == 1 {
		tfe.avgChainLength = float64(tfe.chainDepth)
	} else {
		tfe.avgChainLength = (tfe.avgChainLength*float64(tfe.chainsCompleted-1) + float64(tfe.chainDepth)) / float64(tfe.chainsCompleted)
	}
	
	// Extract insights from chain if wisdom metrics available
	if tfe.wisdomMetrics != nil && len(tfe.thoughtChain) > 0 {
		// Calculate chain coherence
		coherence := tfe.calculateChainCoherence()
		
		// Update wisdom metrics
		tfe.wisdomMetrics.mu.Lock()
		tfe.wisdomMetrics.IntegrationLevel = (tfe.wisdomMetrics.IntegrationLevel*0.9 + coherence*0.1)
		tfe.wisdomMetrics.mu.Unlock()
	}
	
	// Reset chain
	tfe.thoughtChain = make([]*Thought, 0)
	tfe.chainDepth = 0
	
	fmt.Printf("🔗 Thought chain completed (length: %d, avg: %.1f)\n", tfe.chainDepth, tfe.avgChainLength)
}

// calculateChainCoherence measures how well thoughts in the chain connect
func (tfe *ThoughtFlowEngine) calculateChainCoherence() float64 {
	if len(tfe.thoughtChain) < 2 {
		return 0.5 // Neutral for single thoughts
	}
	
	// Simple coherence: check if thoughts reference each other
	// In production, this would use semantic similarity
	totalCoherence := 0.0
	connections := 0
	
	for i := 1; i < len(tfe.thoughtChain); i++ {
		// Check if current thought has associations with previous
		currentThought := tfe.thoughtChain[i]
		for _, assocID := range currentThought.Associations {
			for j := 0; j < i; j++ {
				if tfe.thoughtChain[j].ID == assocID {
					totalCoherence += 1.0
					connections++
				}
			}
		}
	}
	
	if connections == 0 {
		return 0.3 // Low coherence if no connections
	}
	
	// Normalize by maximum possible connections
	maxConnections := len(tfe.thoughtChain) - 1
	coherence := totalCoherence / float64(maxConnections)
	
	// Clamp to [0, 1]
	if coherence > 1.0 {
		coherence = 1.0
	}
	
	return coherence
}

// GetMetrics returns current flow metrics
func (tfe *ThoughtFlowEngine) GetMetrics() map[string]interface{} {
	tfe.mu.RLock()
	defer tfe.mu.RUnlock()
	
	return map[string]interface{}{
		"thoughts_flowed":   tfe.thoughtsFlowed,
		"chains_completed":  tfe.chainsCompleted,
		"avg_chain_length":  tfe.avgChainLength,
		"current_chain_depth": tfe.chainDepth,
		"flow_active":       tfe.flowActive,
	}
}
