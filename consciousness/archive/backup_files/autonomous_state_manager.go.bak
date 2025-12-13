package deeptreeecho

import (
	"sync"
	"time"
)

// AutonomousStateManager manages the autonomous wake/rest cycles
// based on cognitive load, energy levels, and consolidation needs
type AutonomousStateManager struct {
	mu sync.RWMutex
	
	// State metrics (0.0 - 1.0)
	cognitiveLoad     float64   // Current cognitive processing load
	energyLevel       float64   // Available cognitive energy
	consolidationNeed float64   // Need for memory consolidation
	
	// Thresholds
	restThreshold     float64   // Cognitive load threshold for rest
	wakeThreshold     float64   // Energy threshold for waking
	
	// Timing
	lastRestTime      time.Time
	lastWakeTime      time.Time
	restDuration      time.Duration
	minRestInterval   time.Duration // Minimum time between rest cycles
	
	// Activity tracking
	thoughtsProcessed int64
	goalsCompleted    int64
	learningEvents    int64
	
	// Decay rates (per second)
	energyDecayRate   float64
	loadDecayRate     float64
}

// NewAutonomousStateManager creates a new state manager
func NewAutonomousStateManager() *AutonomousStateManager {
	return &AutonomousStateManager{
		cognitiveLoad:     0.3,
		energyLevel:       1.0,
		consolidationNeed: 0.0,
		
		restThreshold:     0.8,  // Rest when load exceeds 80%
		wakeThreshold:     0.7,  // Wake when energy exceeds 70%
		
		lastWakeTime:      time.Now(),
		minRestInterval:   5 * time.Minute, // At least 5 minutes between rests
		
		energyDecayRate:   0.01, // 1% per second when active
		loadDecayRate:     0.005, // 0.5% per second natural decay
	}
}

// UpdateCognitiveLoad updates the cognitive load based on activity
func (asm *AutonomousStateManager) UpdateCognitiveLoad(thought *Thought) {
	asm.mu.Lock()
	defer asm.mu.Unlock()
	
	// Increase load based on thought complexity
	loadIncrease := 0.05 // Base load per thought
	
	// More complex thoughts increase load more
	if thought.Importance > 0.7 {
		loadIncrease *= 1.5
	}
	
	// Reflective thoughts are more demanding
	if thought.Type == ThoughtReflective || thought.Type == ThoughtMetaCognitive {
		loadIncrease *= 1.3
	}
	
	asm.cognitiveLoad += loadIncrease
	
	// Clamp to 0.0-1.0
	if asm.cognitiveLoad > 1.0 {
		asm.cognitiveLoad = 1.0
	}
	
	// Increase consolidation need
	asm.consolidationNeed += 0.02
	if asm.consolidationNeed > 1.0 {
		asm.consolidationNeed = 1.0
	}
	
	asm.thoughtsProcessed++
}

// UpdateEnergy updates energy level based on activity
func (asm *AutonomousStateManager) UpdateEnergy(delta float64) {
	asm.mu.Lock()
	defer asm.mu.Unlock()
	
	asm.energyLevel += delta
	
	// Clamp to 0.0-1.0
	if asm.energyLevel < 0.0 {
		asm.energyLevel = 0.0
	}
	if asm.energyLevel > 1.0 {
		asm.energyLevel = 1.0
	}
}

// DecayOverTime applies natural decay to load and energy
func (asm *AutonomousStateManager) DecayOverTime(duration time.Duration) {
	asm.mu.Lock()
	defer asm.mu.Unlock()
	
	seconds := duration.Seconds()
	
	// Cognitive load naturally decreases over time
	asm.cognitiveLoad -= asm.loadDecayRate * seconds
	if asm.cognitiveLoad < 0.0 {
		asm.cognitiveLoad = 0.0
	}
	
	// Energy naturally decreases when awake
	asm.energyLevel -= asm.energyDecayRate * seconds
	if asm.energyLevel < 0.0 {
		asm.energyLevel = 0.0
	}
}

// ShouldRest determines if the system should enter rest cycle
func (asm *AutonomousStateManager) ShouldRest() bool {
	asm.mu.RLock()
	defer asm.mu.RUnlock()
	
	// Don't rest if we just woke up recently
	if time.Since(asm.lastWakeTime) < asm.minRestInterval {
		return false
	}
	
	// Rest if any of these conditions are met:
	// 1. Cognitive load is too high
	if asm.cognitiveLoad > asm.restThreshold {
		return true
	}
	
	// 2. Energy is too low
	if asm.energyLevel < 0.3 {
		return true
	}
	
	// 3. Consolidation need is high
	if asm.consolidationNeed > 0.7 {
		return true
	}
	
	// 4. Been awake for a long time (circadian-like rhythm)
	if time.Since(asm.lastWakeTime) > 30*time.Minute {
		return true
	}
	
	return false
}

// ShouldWake determines if the system should wake from rest
func (asm *AutonomousStateManager) ShouldWake() bool {
	asm.mu.RLock()
	defer asm.mu.RUnlock()
	
	// Wake if:
	// 1. Energy has been restored
	if asm.energyLevel > asm.wakeThreshold {
		return true
	}
	
	// 2. Consolidation is complete
	if asm.consolidationNeed < 0.2 {
		return true
	}
	
	// 3. Rested for sufficient duration
	if time.Since(asm.lastRestTime) > asm.restDuration {
		return true
	}
	
	return false
}

// EnterRest marks the beginning of a rest cycle
func (asm *AutonomousStateManager) EnterRest() {
	asm.mu.Lock()
	defer asm.mu.Unlock()
	
	asm.lastRestTime = time.Now()
	
	// Calculate rest duration based on needs
	// Base duration: 2 minutes
	// +1 minute per 0.2 cognitive load
	// +1 minute per 0.2 consolidation need
	baseDuration := 2.0
	loadFactor := asm.cognitiveLoad * 5.0
	consolidationFactor := asm.consolidationNeed * 5.0
	
	totalMinutes := baseDuration + loadFactor + consolidationFactor
	asm.restDuration = time.Duration(totalMinutes) * time.Minute
}

// ExitRest marks the end of a rest cycle
func (asm *AutonomousStateManager) ExitRest() {
	asm.mu.Lock()
	defer asm.mu.Unlock()
	
	asm.lastWakeTime = time.Now()
	
	// Restore energy
	asm.energyLevel = 1.0
	
	// Reduce cognitive load significantly
	asm.cognitiveLoad *= 0.3
	
	// Reset consolidation need
	asm.consolidationNeed = 0.0
}

// GetState returns current state metrics
func (asm *AutonomousStateManager) GetState() map[string]interface{} {
	asm.mu.RLock()
	defer asm.mu.RUnlock()
	
	return map[string]interface{}{
		"cognitive_load":      asm.cognitiveLoad,
		"energy_level":        asm.energyLevel,
		"consolidation_need":  asm.consolidationNeed,
		"thoughts_processed":  asm.thoughtsProcessed,
		"goals_completed":     asm.goalsCompleted,
		"learning_events":     asm.learningEvents,
		"time_since_wake":     time.Since(asm.lastWakeTime).String(),
		"time_since_rest":     time.Since(asm.lastRestTime).String(),
	}
}

// RecordGoalCompletion records a completed goal
func (asm *AutonomousStateManager) RecordGoalCompletion() {
	asm.mu.Lock()
	defer asm.mu.Unlock()
	
	asm.goalsCompleted++
	
	// Completing goals increases load slightly
	asm.cognitiveLoad += 0.03
	if asm.cognitiveLoad > 1.0 {
		asm.cognitiveLoad = 1.0
	}
}

// RecordLearningEvent records a learning event
func (asm *AutonomousStateManager) RecordLearningEvent() {
	asm.mu.Lock()
	defer asm.mu.Unlock()
	
	asm.learningEvents++
	
	// Learning increases consolidation need
	asm.consolidationNeed += 0.05
	if asm.consolidationNeed > 1.0 {
		asm.consolidationNeed = 1.0
	}
}
