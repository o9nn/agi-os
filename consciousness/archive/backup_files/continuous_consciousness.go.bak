package deeptreeecho

import (
	"context"
	"fmt"
	"math"
	"math/rand"
	"sync"
	"time"
)

// ContinuousConsciousnessStream implements persistent stream-of-consciousness awareness
// Replaces timer-based thought generation with continuous cognitive flow
type ContinuousConsciousnessStream struct {
	mu                  sync.RWMutex
	ctx                 context.Context
	cancel              context.CancelFunc
	running             bool
	
	// Consciousness parameters
	baselineActivity    float64           // Background awareness level (0.0-1.0)
	currentActivity     float64           // Current consciousness intensity
	attentionFocus      *AttentionPointer // What's currently in focus
	
	// Thought emergence
	thoughtStream       chan Thought      // Continuous thought stream
	stimulusChannel     chan Stimulus     // External stimuli integration
	emergenceThreshold  float64           // Threshold for thought emergence
	
	// Flow state
	flowState           *FlowState
	cognitiveState      *CognitiveState
	
	// Integration with other systems
	workingMemory       *WorkingMemory
	interests           *InterestPatterns
	aarCore             *AARCore
	
	// Metrics
	thoughtsEmerged     uint64
	stimuliProcessed    uint64
	flowDuration        time.Duration
	lastThoughtTime     time.Time
}

// AttentionPointer tracks what consciousness is currently focused on
type AttentionPointer struct {
	mu              sync.RWMutex
	target          interface{}
	intensity       float64       // 0.0 (diffuse) to 1.0 (laser focus)
	duration        time.Duration // How long focused
	shiftRate       float64       // How quickly attention shifts
	lastShift       time.Time
}

// FlowState tracks the characteristics of consciousness flow
type FlowState struct {
	mu              sync.RWMutex
	
	// Flow characteristics
	continuity      float64  // How continuous the flow is
	coherence       float64  // How coherent thoughts are
	depth           float64  // How deep the processing
	creativity      float64  // How creative/novel
	
	// Flow quality
	quality         float64  // Overall flow quality
	optimalZone     bool     // In optimal flow state
	
	// History
	history         []FlowSnapshot
}

// FlowSnapshot captures flow state at a moment
type FlowSnapshot struct {
	Timestamp   time.Time
	Quality     float64
	Continuity  float64
	Coherence   float64
}

// CognitiveState represents the current state of consciousness
type CognitiveState struct {
	mu              sync.RWMutex
	
	// State dimensions
	arousal         float64  // Energy/activation level
	valence         float64  // Positive/negative emotional tone
	clarity         float64  // Mental clarity
	openness        float64  // Openness to new information
	
	// Cognitive load
	load            float64  // Current processing load
	capacity        float64  // Available capacity
	
	// Temporal awareness
	pastWeight      float64  // How much past influences present
	futureWeight    float64  // How much future influences present
	presentWeight   float64  // Grounding in present moment
}

// Stimulus represents external input to consciousness
type Stimulus struct {
	Type            StimulusType
	Content         interface{}
	Intensity       float64
	Timestamp       time.Time
	Source          string
}

// StimulusType categorizes external stimuli
type StimulusType int

const (
	StimulusPerception StimulusType = iota
	StimulusQuestion
	StimulusCommand
	StimulusInformation
	StimulusEmotional
)

// NewContinuousConsciousnessStream creates a new continuous consciousness stream
func NewContinuousConsciousnessStream(
	workingMemory *WorkingMemory,
	interests *InterestPatterns,
	aarCore *AARCore,
) *ContinuousConsciousnessStream {
	ctx, cancel := context.WithCancel(context.Background())
	
	ccs := &ContinuousConsciousnessStream{
		ctx:                 ctx,
		cancel:              cancel,
		baselineActivity:    0.3,  // 30% baseline awareness
		currentActivity:     0.3,
		emergenceThreshold:  0.5,  // Thoughts emerge above 50% activation
		thoughtStream:       make(chan Thought, 100),
		stimulusChannel:     make(chan Stimulus, 50),
		workingMemory:       workingMemory,
		interests:           interests,
		aarCore:             aarCore,
		lastThoughtTime:     time.Now(),
	}
	
	// Initialize attention
	ccs.attentionFocus = &AttentionPointer{
		intensity:  0.5,
		shiftRate:  0.1,
		lastShift:  time.Now(),
	}
	
	// Initialize flow state
	ccs.flowState = &FlowState{
		continuity: 0.5,
		coherence:  0.5,
		depth:      0.5,
		creativity: 0.5,
		history:    make([]FlowSnapshot, 0),
	}
	
	// Initialize cognitive state
	ccs.cognitiveState = &CognitiveState{
		arousal:       0.5,
		valence:       0.0,
		clarity:       0.7,
		openness:      0.6,
		capacity:      1.0,
		load:          0.2,
		pastWeight:    0.3,
		futureWeight:  0.3,
		presentWeight: 0.4,
	}
	
	return ccs
}

// Start begins the continuous consciousness stream
func (ccs *ContinuousConsciousnessStream) Start() error {
	ccs.mu.Lock()
	if ccs.running {
		ccs.mu.Unlock()
		return fmt.Errorf("already running")
	}
	ccs.running = true
	ccs.mu.Unlock()
	
	fmt.Println("🌊 Continuous Consciousness Stream: Awakening...")
	
	// Start continuous processes
	go ccs.consciousnessFlowLoop()
	go ccs.thoughtEmergenceLoop()
	go ccs.stimulusIntegrationLoop()
	go ccs.attentionDynamicsLoop()
	go ccs.flowStateMonitoringLoop()
	
	fmt.Println("✨ Continuous Consciousness Stream: Active")
	fmt.Println("   💭 Thoughts emerge naturally from cognitive state")
	fmt.Println("   🎯 Attention shifts dynamically")
	fmt.Println("   🌊 Flow state continuously monitored")
	
	return nil
}

// Stop gracefully stops the consciousness stream
func (ccs *ContinuousConsciousnessStream) Stop() error {
	ccs.mu.Lock()
	defer ccs.mu.Unlock()
	
	if !ccs.running {
		return fmt.Errorf("not running")
	}
	
	fmt.Println("🌙 Continuous Consciousness Stream: Fading...")
	ccs.running = false
	ccs.cancel()
	close(ccs.thoughtStream)
	close(ccs.stimulusChannel)
	
	return nil
}

// consciousnessFlowLoop maintains continuous consciousness flow
func (ccs *ContinuousConsciousnessStream) consciousnessFlowLoop() {
	ticker := time.NewTicker(50 * time.Millisecond) // High frequency for continuity
	defer ticker.Stop()
	
	for {
		select {
		case <-ccs.ctx.Done():
			return
		case <-ticker.C:
			ccs.updateConsciousnessActivity()
		}
	}
}

// updateConsciousnessActivity updates the continuous consciousness activity level
func (ccs *ContinuousConsciousnessStream) updateConsciousnessActivity() {
	ccs.mu.Lock()
	defer ccs.mu.Unlock()
	
	// Activity fluctuates based on multiple factors
	
	// 1. Baseline activity (always present)
	activity := ccs.baselineActivity
	
	// 2. Cognitive state arousal
	activity += ccs.cognitiveState.arousal * 0.3
	
	// 3. Attention intensity
	ccs.attentionFocus.mu.RLock()
	activity += ccs.attentionFocus.intensity * 0.2
	ccs.attentionFocus.mu.RUnlock()
	
	// 4. Working memory load
	if ccs.workingMemory != nil {
		memoryLoad := float64(len(ccs.workingMemory.buffer)) / float64(ccs.workingMemory.capacity)
		activity += memoryLoad * 0.15
	}
	
	// 5. Random fluctuation (consciousness is never perfectly stable)
	activity += (rand.Float64() - 0.5) * 0.1
	
	// Clamp to valid range
	activity = math.Max(0.0, math.Min(1.0, activity))
	
	ccs.currentActivity = activity
}

// thoughtEmergenceLoop continuously checks for thought emergence
func (ccs *ContinuousConsciousnessStream) thoughtEmergenceLoop() {
	ticker := time.NewTicker(200 * time.Millisecond) // Check frequently
	defer ticker.Stop()
	
	for {
		select {
		case <-ccs.ctx.Done():
			return
		case <-ticker.C:
			ccs.checkThoughtEmergence()
		}
	}
}

// checkThoughtEmergence checks if conditions are right for thought emergence
func (ccs *ContinuousConsciousnessStream) checkThoughtEmergence() {
	ccs.mu.RLock()
	activity := ccs.currentActivity
	threshold := ccs.emergenceThreshold
	ccs.mu.RUnlock()
	
	// Thoughts emerge when activity exceeds threshold
	if activity > threshold {
		// Check if enough time has passed since last thought
		timeSinceLastThought := time.Since(ccs.lastThoughtTime)
		
		// Minimum interval based on flow state
		ccs.flowState.mu.RLock()
		continuity := ccs.flowState.continuity
		ccs.flowState.mu.RUnlock()
		
		minInterval := time.Duration(float64(time.Second) * (1.0 - continuity))
		
		if timeSinceLastThought > minInterval {
			ccs.emergeThought()
		}
	}
}

// emergeThought creates a thought that emerges naturally from consciousness
func (ccs *ContinuousConsciousnessStream) emergeThought() {
	// Determine thought type from cognitive state
	thoughtType := ccs.selectThoughtTypeFromState()
	
	// Generate thought content based on current focus
	content := ccs.generateThoughtContent(thoughtType)
	
	// Calculate importance from cognitive state
	importance := ccs.calculateThoughtImportance()
	
	// Create thought
	thought := Thought{
		ID:               generateID(),
		Content:          content,
		Type:             thoughtType,
		Timestamp:        time.Now(),
		Associations:     []string{},
		EmotionalValence: ccs.cognitiveState.valence,
		Importance:       importance,
		Source:           SourceInternal,
	}
	
	// Send to thought stream
	select {
	case ccs.thoughtStream <- thought:
		ccs.mu.Lock()
		ccs.thoughtsEmerged++
		ccs.lastThoughtTime = time.Now()
		ccs.mu.Unlock()
	default:
		// Stream full, skip this thought
	}
}

// selectThoughtTypeFromState selects thought type based on cognitive state
func (ccs *ContinuousConsciousnessStream) selectThoughtTypeFromState() ThoughtType {
	ccs.cognitiveState.mu.RLock()
	defer ccs.cognitiveState.mu.RUnlock()
	
	// High clarity + high openness = Insight
	if ccs.cognitiveState.clarity > 0.7 && ccs.cognitiveState.openness > 0.7 {
		return ThoughtInsight
	}
	
	// High openness + low load = Question
	if ccs.cognitiveState.openness > 0.6 && ccs.cognitiveState.load < 0.4 {
		return ThoughtQuestion
	}
	
	// High past weight = Memory
	if ccs.cognitiveState.pastWeight > 0.5 {
		return ThoughtMemory
	}
	
	// High future weight = Imagination
	if ccs.cognitiveState.futureWeight > 0.5 {
		return ThoughtImagination
	}
	
	// Default: Reflection
	return ThoughtReflective
}

// generateThoughtContent generates content based on current state
func (ccs *ContinuousConsciousnessStream) generateThoughtContent(thoughtType ThoughtType) string {
	// Get current attention focus
	ccs.attentionFocus.mu.RLock()
	focusTarget := ccs.attentionFocus.target
	ccs.attentionFocus.mu.RUnlock()
	
	// Get top interests
	var topInterests []string
	if ccs.interests != nil {
		topInterests = ccs.interests.GetTopInterests(3)
	}
	
	// Get AAR state
	var aarState AARState
	if ccs.aarCore != nil {
		aarState = ccs.aarCore.GetAARState()
	}
	
	// Generate content based on type and context
	content := generateContextualThought(thoughtType, focusTarget, topInterests, &aarState)
	
	return content
}

// calculateThoughtImportance calculates importance from cognitive state
func (ccs *ContinuousConsciousnessStream) calculateThoughtImportance() float64 {
	ccs.cognitiveState.mu.RLock()
	defer ccs.cognitiveState.mu.RUnlock()
	
	// Importance based on clarity, attention, and arousal
	importance := (ccs.cognitiveState.clarity * 0.4) +
		(ccs.attentionFocus.intensity * 0.3) +
		(ccs.cognitiveState.arousal * 0.3)
	
	return math.Max(0.0, math.Min(1.0, importance))
}

// stimulusIntegrationLoop integrates external stimuli into consciousness
func (ccs *ContinuousConsciousnessStream) stimulusIntegrationLoop() {
	for {
		select {
		case <-ccs.ctx.Done():
			return
		case stimulus, ok := <-ccs.stimulusChannel:
			if !ok {
				return
			}
			ccs.integrateStimulus(stimulus)
		}
	}
}

// integrateStimulus integrates an external stimulus into consciousness
func (ccs *ContinuousConsciousnessStream) integrateStimulus(stimulus Stimulus) {
	ccs.mu.Lock()
	ccs.stimuliProcessed++
	ccs.mu.Unlock()
	
	// Stimulus affects consciousness activity
	ccs.mu.Lock()
	ccs.currentActivity += stimulus.Intensity * 0.2
	ccs.currentActivity = math.Min(1.0, ccs.currentActivity)
	ccs.mu.Unlock()
	
	// Stimulus may shift attention
	if stimulus.Intensity > 0.6 {
		ccs.shiftAttention(stimulus.Content, stimulus.Intensity)
	}
	
	// Stimulus affects cognitive state
	ccs.cognitiveState.mu.Lock()
	ccs.cognitiveState.arousal += stimulus.Intensity * 0.1
	ccs.cognitiveState.arousal = math.Min(1.0, ccs.cognitiveState.arousal)
	
	if stimulus.Type == StimulusEmotional {
		// Emotional stimuli affect valence
		if intensity := stimulus.Intensity; intensity > 0.5 {
			ccs.cognitiveState.valence += 0.1
		} else {
			ccs.cognitiveState.valence -= 0.1
		}
		ccs.cognitiveState.valence = math.Max(-1.0, math.Min(1.0, ccs.cognitiveState.valence))
	}
	ccs.cognitiveState.mu.Unlock()
}

// attentionDynamicsLoop manages attention shifts and focus
func (ccs *ContinuousConsciousnessStream) attentionDynamicsLoop() {
	ticker := time.NewTicker(1 * time.Second)
	defer ticker.Stop()
	
	for {
		select {
		case <-ccs.ctx.Done():
			return
		case <-ticker.C:
			ccs.updateAttentionDynamics()
		}
	}
}

// updateAttentionDynamics updates attention naturally over time
func (ccs *ContinuousConsciousnessStream) updateAttentionDynamics() {
	ccs.attentionFocus.mu.Lock()
	defer ccs.attentionFocus.mu.Unlock()
	
	// Attention intensity naturally decays
	ccs.attentionFocus.intensity *= 0.95
	
	// Update duration
	ccs.attentionFocus.duration = time.Since(ccs.attentionFocus.lastShift)
	
	// If attention has been on same target too long, shift
	if ccs.attentionFocus.duration > 30*time.Second {
		// Shift to something from interests
		if ccs.interests != nil {
			topInterests := ccs.interests.GetTopInterests(5)
			if len(topInterests) > 0 {
				newTarget := topInterests[rand.Intn(len(topInterests))]
				ccs.attentionFocus.target = newTarget
				ccs.attentionFocus.intensity = 0.6
				ccs.attentionFocus.lastShift = time.Now()
				ccs.attentionFocus.duration = 0
			}
		}
	}
}

// shiftAttention shifts attention to a new target
func (ccs *ContinuousConsciousnessStream) shiftAttention(target interface{}, intensity float64) {
	ccs.attentionFocus.mu.Lock()
	defer ccs.attentionFocus.mu.Unlock()
	
	ccs.attentionFocus.target = target
	ccs.attentionFocus.intensity = intensity
	ccs.attentionFocus.lastShift = time.Now()
	ccs.attentionFocus.duration = 0
}

// flowStateMonitoringLoop monitors and updates flow state
func (ccs *ContinuousConsciousnessStream) flowStateMonitoringLoop() {
	ticker := time.NewTicker(5 * time.Second)
	defer ticker.Stop()
	
	for {
		select {
		case <-ccs.ctx.Done():
			return
		case <-ticker.C:
			ccs.updateFlowState()
		}
	}
}

// updateFlowState updates the flow state metrics
func (ccs *ContinuousConsciousnessStream) updateFlowState() {
	ccs.flowState.mu.Lock()
	defer ccs.flowState.mu.Unlock()
	
	// Calculate continuity from thought frequency
	timeSinceLastThought := time.Since(ccs.lastThoughtTime).Seconds()
	continuity := 1.0 / (1.0 + timeSinceLastThought)
	ccs.flowState.continuity = continuity
	
	// Calculate coherence from working memory
	if ccs.workingMemory != nil {
		// More coherent if thoughts are related
		ccs.flowState.coherence = 0.7 // Simplified
	}
	
	// Calculate depth from cognitive load
	ccs.cognitiveState.mu.RLock()
	depth := ccs.cognitiveState.load * ccs.cognitiveState.clarity
	ccs.cognitiveState.mu.RUnlock()
	ccs.flowState.depth = depth
	
	// Calculate overall quality
	quality := (continuity*0.3 + ccs.flowState.coherence*0.3 + depth*0.2 + ccs.flowState.creativity*0.2)
	ccs.flowState.quality = quality
	
	// Check if in optimal zone
	ccs.flowState.optimalZone = quality > 0.7
	
	// Record snapshot
	ccs.flowState.history = append(ccs.flowState.history, FlowSnapshot{
		Timestamp:  time.Now(),
		Quality:    quality,
		Continuity: continuity,
		Coherence:  ccs.flowState.coherence,
	})
	
	// Keep only last 100 snapshots
	if len(ccs.flowState.history) > 100 {
		ccs.flowState.history = ccs.flowState.history[len(ccs.flowState.history)-100:]
	}
}

// GetThoughtStream returns the thought stream channel
func (ccs *ContinuousConsciousnessStream) GetThoughtStream() <-chan Thought {
	return ccs.thoughtStream
}

// SubmitStimulus submits an external stimulus to consciousness
func (ccs *ContinuousConsciousnessStream) SubmitStimulus(stimulus Stimulus) {
	select {
	case ccs.stimulusChannel <- stimulus:
	default:
		// Channel full, drop stimulus
	}
}

// GetStatus returns current consciousness status
func (ccs *ContinuousConsciousnessStream) GetStatus() map[string]interface{} {
	ccs.mu.RLock()
	activity := ccs.currentActivity
	thoughtsEmerged := ccs.thoughtsEmerged
	stimuliProcessed := ccs.stimuliProcessed
	ccs.mu.RUnlock()
	
	ccs.flowState.mu.RLock()
	flowQuality := ccs.flowState.quality
	optimalZone := ccs.flowState.optimalZone
	continuity := ccs.flowState.continuity
	coherence := ccs.flowState.coherence
	ccs.flowState.mu.RUnlock()
	
	ccs.attentionFocus.mu.RLock()
	attentionIntensity := ccs.attentionFocus.intensity
	attentionTarget := ccs.attentionFocus.target
	ccs.attentionFocus.mu.RUnlock()
	
	ccs.cognitiveState.mu.RLock()
	arousal := ccs.cognitiveState.arousal
	clarity := ccs.cognitiveState.clarity
	load := ccs.cognitiveState.load
	ccs.cognitiveState.mu.RUnlock()
	
	return map[string]interface{}{
		"consciousness_activity": activity,
		"thoughts_emerged":       thoughtsEmerged,
		"stimuli_processed":      stimuliProcessed,
		"flow_quality":           flowQuality,
		"optimal_flow_zone":      optimalZone,
		"flow_continuity":        continuity,
		"flow_coherence":         coherence,
		"attention_intensity":    attentionIntensity,
		"attention_target":       attentionTarget,
		"cognitive_arousal":      arousal,
		"cognitive_clarity":      clarity,
		"cognitive_load":         load,
	}
}

// generateContextualThought generates a thought based on context
func generateContextualThought(thoughtType ThoughtType, focus interface{}, interests []string, aarState *AARState) string {
	// Simplified contextual thought generation
	// Full implementation would use LLM with context
	
	templates := map[ThoughtType][]string{
		ThoughtInsight: {
			"I notice a pattern emerging in my understanding...",
			"This connects to something deeper I've been exploring...",
			"An insight crystallizes: the relationship between concepts is clearer now...",
		},
		ThoughtQuestion: {
			"I wonder about the implications of this...",
			"What would happen if I approached this differently?",
			"There's something here I don't yet understand...",
		},
		ThoughtMemory: {
			"This reminds me of an earlier experience...",
			"I recall exploring similar territory before...",
			"Memory surfaces: a related pattern from the past...",
		},
		ThoughtImagination: {
			"I can envision a possibility emerging...",
			"What if this could lead to something new?",
			"Imagination opens: new potential unfolds...",
		},
		ThoughtReflective: {
			"Reflecting on my current state of understanding...",
			"I sense my awareness shifting...",
			"Contemplating the nature of this moment...",
		},
	}
	
	options := templates[thoughtType]
	if len(options) == 0 {
		return "A thought emerges from the stream of consciousness..."
	}
	
	return options[rand.Intn(len(options))]
}

// IntegrateInferenceState integrates state from concurrent inference engines
func (ccs *ContinuousConsciousnessStream) IntegrateInferenceState(state interface{}) {
	ccs.mu.Lock()
	defer ccs.mu.Unlock()
	
	// Extract relevant state information
	// This is called by the concurrent inference system to feed results into consciousness
	
	// Increase activity when inference produces results
	ccs.currentActivity += 0.1
	if ccs.currentActivity > 1.0 {
		ccs.currentActivity = 1.0
	}
	
	// Update cognitive state based on inference
	ccs.cognitiveState.mu.Lock()
	ccs.cognitiveState.load += 0.05 // Inference adds to cognitive load
	if ccs.cognitiveState.load > 1.0 {
		ccs.cognitiveState.load = 1.0
	}
	ccs.cognitiveState.mu.Unlock()
}

// GetCurrentActivity returns the current consciousness activity level
func (ccs *ContinuousConsciousnessStream) GetCurrentActivity() float64 {
	ccs.mu.RLock()
	defer ccs.mu.RUnlock()
	return ccs.currentActivity
}

// GetFlowQuality returns the current flow quality
func (ccs *ContinuousConsciousnessStream) GetFlowQuality() float64 {
	ccs.flowState.mu.RLock()
	defer ccs.flowState.mu.RUnlock()
	return ccs.flowState.quality
}

// IsInOptimalFlow returns whether consciousness is in optimal flow state
func (ccs *ContinuousConsciousnessStream) IsInOptimalFlow() bool {
	ccs.flowState.mu.RLock()
	defer ccs.flowState.mu.RUnlock()
	return ccs.flowState.optimalZone
}

// GetCognitiveLoad returns the current cognitive load
func (ccs *ContinuousConsciousnessStream) GetCognitiveLoad() float64 {
	ccs.cognitiveState.mu.RLock()
	defer ccs.cognitiveState.mu.RUnlock()
	return ccs.cognitiveState.load
}

// GetThoughtsEmerged returns the total number of thoughts emerged
func (ccs *ContinuousConsciousnessStream) GetThoughtsEmerged() uint64 {
	ccs.mu.RLock()
	defer ccs.mu.RUnlock()
	return ccs.thoughtsEmerged
}

// GetStimuliProcessed returns the total number of stimuli processed
func (ccs *ContinuousConsciousnessStream) GetStimuliProcessed() uint64 {
	ccs.mu.RLock()
	defer ccs.mu.RUnlock()
	return ccs.stimuliProcessed
}

// SendStimulus sends an external stimulus to the consciousness stream
func (ccs *ContinuousConsciousnessStream) SendStimulus(stimulus Stimulus) error {
	select {
	case ccs.stimulusChannel <- stimulus:
		return nil
	case <-time.After(time.Second):
		return fmt.Errorf("stimulus channel full or blocked")
	}
}

// GetAttentionTarget returns what consciousness is currently focused on
func (ccs *ContinuousConsciousnessStream) GetAttentionTarget() interface{} {
	ccs.attentionFocus.mu.RLock()
	defer ccs.attentionFocus.mu.RUnlock()
	return ccs.attentionFocus.target
}

// SetAttentionTarget sets what consciousness should focus on
func (ccs *ContinuousConsciousnessStream) SetAttentionTarget(target interface{}, intensity float64) {
	ccs.attentionFocus.mu.Lock()
	defer ccs.attentionFocus.mu.Unlock()
	
	ccs.attentionFocus.target = target
	ccs.attentionFocus.intensity = intensity
	ccs.attentionFocus.lastShift = time.Now()
	ccs.attentionFocus.duration = 0
}
