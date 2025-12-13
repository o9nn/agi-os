package echobeats

import (
	"fmt"
	"sync"
	"time"
)

// TwelveStepCognitiveLoop implements the 12-step cognitive loop
// Based on the architecture: 3 concurrent inference engines
// 7 expressive mode steps + 5 reflective mode steps
// Phases: Affordance (past) → Relevance (present) → Salience (future)
type TwelveStepCognitiveLoop struct {
	mu              sync.RWMutex
	currentStep     int
	cycleCount      uint64
	stepDuration    time.Duration
	
	// Three phases, 4 steps apart
	phase1Step      int // Step 0: Pivotal relevance realization (orienting present)
	phase2Step      int // Step 6: Pivotal relevance realization (orienting present)
	
	// Cognitive modes
	expressiveSteps map[int]bool
	reflectiveSteps map[int]bool
	
	// Step handlers
	stepHandlers    map[int]StepHandler
	
	// Metrics
	metrics         *CognitiveLoopMetrics
}

// Types are now defined in shared_types.go to avoid redeclaration conflicts

// CognitiveLoopMetrics is now defined in shared_types.go

// NewTwelveStepCognitiveLoop creates a new 12-step cognitive loop
func NewTwelveStepCognitiveLoop(stepDuration time.Duration) *TwelveStepCognitiveLoop {
	loop := &TwelveStepCognitiveLoop{
		currentStep:     0,
		cycleCount:      0,
		stepDuration:    stepDuration,
		phase1Step:      0,
		phase2Step:      6,
		stepHandlers:    make(map[int]StepHandler),
		expressiveSteps: make(map[int]bool),
		reflectiveSteps: make(map[int]bool),
			metrics: NewCognitiveLoopMetrics(),
	}
	
	// Define expressive steps (7 total)
	// Steps 1-5: Actual affordance interaction (expressive)
	// Step 7-11: Virtual salience simulation (expressive, but 7-8 are reflective in some models)
	loop.expressiveSteps[1] = true
	loop.expressiveSteps[2] = true
	loop.expressiveSteps[3] = true
	loop.expressiveSteps[4] = true
	loop.expressiveSteps[5] = true
	loop.expressiveSteps[9] = true
	loop.expressiveSteps[10] = true
	loop.expressiveSteps[11] = true
	
	// Define reflective steps (5 total)
	// Step 0: Pivotal relevance realization
	// Step 6: Pivotal relevance realization
	// Steps 7-8: Transition/reflection
	loop.reflectiveSteps[0] = true
	loop.reflectiveSteps[6] = true
	loop.reflectiveSteps[7] = true
	loop.reflectiveSteps[8] = true
	
	return loop
}

// Start begins the cognitive loop
func (loop *TwelveStepCognitiveLoop) Start() {
	fmt.Println("🔄 12-Step Cognitive Loop: Starting...")
	go loop.run()
}

// run executes the cognitive loop
func (loop *TwelveStepCognitiveLoop) run() {
	ticker := time.NewTicker(loop.stepDuration)
	defer ticker.Stop()
	
	for range ticker.C {
		loop.executeStep()
	}
}

// executeStep executes the current step
func (loop *TwelveStepCognitiveLoop) executeStep() {
	loop.mu.Lock()
	step := loop.currentStep
	loop.mu.Unlock()
	
	// Determine phase and mode
	phase := loop.getPhase(step)
	mode := loop.getMode(step)
	
		// Execute step handler if registered
		if handler, exists := loop.stepHandlers[step]; exists {
			start := time.Now()
			context := &StepContext{
				StepNumber:      step,
				Phase:           int(phase),
				Mode:            mode,
				PreviousOutputs: make(map[int]interface{}),
				SharedState:     make(map[string]interface{}),
				Timestamp:       time.Now(),
			}
			if err := handler(context); err != nil {
				fmt.Printf("❌ Error in step %d: %v\n", step, err)
			}
			duration := time.Since(start)
		
		// Update metrics
		loop.metrics.mu.Lock()
		loop.metrics.StepsProcessed++
		loop.metrics.AverageStepDuration = (loop.metrics.AverageStepDuration + duration) / 2
		loop.metrics.ModeDistribution[mode]++
		loop.metrics.mu.Unlock()
	}
	
	// Log step execution
	fmt.Printf("🔄 Step %d | Phase: %s | Mode: %s\n", step, phase, mode)
	
	// Advance to next step
	loop.mu.Lock()
	loop.currentStep = (loop.currentStep + 1) % 12
	if loop.currentStep == 0 {
		loop.cycleCount++
		loop.metrics.mu.Lock()
		loop.metrics.CyclesCompleted++
		loop.metrics.mu.Unlock()
		fmt.Printf("🎯 Cognitive Cycle %d Complete\n", loop.cycleCount)
	}
	loop.mu.Unlock()
	
	// Track phase transitions
	if loop.isPhaseTransition(step) {
		loop.metrics.mu.Lock()
		loop.metrics.PhaseTransitions[phase]++
		loop.metrics.mu.Unlock()
	}
}

// getPhase determines the phase for a given step
func (loop *TwelveStepCognitiveLoop) getPhase(step int) CognitivePhaseType {
	// Steps 0-5: Affordance phase (conditioning past performance)
	// Step 0: Pivotal relevance realization
	// Steps 1-5: Actual affordance interaction
	if step >= 0 && step <= 5 {
			if step == 0 {
				return PhaseRelevance // Pivotal moment
			}
			return PhaseAffordance
		}
		
		// Step 6: Pivotal relevance realization (orienting present)
		if step == 6 {
			return PhaseRelevance
		}
		
		// Steps 7-11: Salience phase (anticipating future potential)
		return PhaseSalience
}

// getMode determines the mode for a given step
func (loop *TwelveStepCognitiveLoop) getMode(step int) CognitiveMode {
	if loop.expressiveSteps[step] {
		return ModeExpressive
	}
	return ModeReflective
}

// isPhaseTransition checks if step is a phase transition point
func (loop *TwelveStepCognitiveLoop) isPhaseTransition(step int) bool {
	return step == 0 || step == 6
}

// RegisterStepHandler registers a handler for a specific step
func (loop *TwelveStepCognitiveLoop) RegisterStepHandler(step int, handler StepHandler) {
	loop.mu.Lock()
	defer loop.mu.Unlock()
	loop.stepHandlers[step] = handler
}

// GetCurrentStep returns the current step
func (loop *TwelveStepCognitiveLoop) GetCurrentStep() int {
	loop.mu.RLock()
	defer loop.mu.RUnlock()
	return loop.currentStep
}

// GetCurrentPhase returns the current phase
func (loop *TwelveStepCognitiveLoop) GetCurrentPhase() CognitivePhaseType {
	loop.mu.RLock()
	step := loop.currentStep
	loop.mu.RUnlock()
	return loop.getPhase(step)
}

// GetCurrentMode returns the current mode
func (loop *TwelveStepCognitiveLoop) GetCurrentMode() CognitiveMode {
	loop.mu.RLock()
	step := loop.currentStep
	loop.mu.RUnlock()
	return loop.getMode(step)
}

// GetCycleCount returns the number of completed cycles
func (loop *TwelveStepCognitiveLoop) GetCycleCount() uint64 {
	loop.mu.RLock()
	defer loop.mu.RUnlock()
	return loop.cycleCount
}

// GetMetrics returns a copy of the metrics
func (loop *TwelveStepCognitiveLoop) GetMetrics() CognitiveLoopMetrics {
	loop.metrics.mu.RLock()
	defer loop.metrics.mu.RUnlock()
	
	metrics := CognitiveLoopMetrics{
		CyclesCompleted:     loop.metrics.CyclesCompleted,
		StepsProcessed:      loop.metrics.StepsProcessed,
		AverageStepDuration: loop.metrics.AverageStepDuration,
		PhaseTransitions:    make(map[CognitivePhaseType]uint64),
		ModeDistribution:    make(map[CognitiveMode]uint64),
	}
	
	for k, v := range loop.metrics.PhaseTransitions {
		metrics.PhaseTransitions[k] = v
	}
	for k, v := range loop.metrics.ModeDistribution {
		metrics.ModeDistribution[k] = v
	}
	
	return metrics
}

// GetStepDescription returns a description of the current step
func (loop *TwelveStepCognitiveLoop) GetStepDescription(step int) string {
	descriptions := map[int]string{
		0:  "Pivotal Relevance Realization - Orienting to present commitment",
		1:  "Affordance Interaction 1 - Engaging with actual possibilities",
		2:  "Affordance Interaction 2 - Conditioning from past performance",
		3:  "Affordance Interaction 3 - Deepening engagement",
		4:  "Affordance Interaction 4 - Refining action",
		5:  "Affordance Interaction 5 - Completing affordance phase",
		6:  "Pivotal Relevance Realization - Re-orienting to present",
		7:  "Salience Reflection 1 - Transitioning to future orientation",
		8:  "Salience Reflection 2 - Preparing for simulation",
		9:  "Virtual Salience Simulation 1 - Anticipating future potential",
		10: "Virtual Salience Simulation 2 - Exploring possibilities",
		11: "Virtual Salience Simulation 3 - Completing salience phase",
	}
	
	if desc, exists := descriptions[step]; exists {
		return desc
	}
	return fmt.Sprintf("Step %d", step)
}

// PrintStatus prints the current status of the cognitive loop
func (loop *TwelveStepCognitiveLoop) PrintStatus() {
	loop.mu.RLock()
	step := loop.currentStep
	cycle := loop.cycleCount
	loop.mu.RUnlock()
	
	phase := loop.getPhase(step)
	mode := loop.getMode(step)
	desc := loop.GetStepDescription(step)
	
	fmt.Println("\n╔════════════════════════════════════════════════════════════╗")
	fmt.Printf("║ 12-Step Cognitive Loop Status                             ║\n")
	fmt.Println("╠════════════════════════════════════════════════════════════╣")
	fmt.Printf("║ Current Step:  %-2d                                         ║\n", step)
	fmt.Printf("║ Current Cycle: %-10d                                 ║\n", cycle)
	fmt.Printf("║ Phase:         %-20s                       ║\n", phase)
	fmt.Printf("║ Mode:          %-20s                       ║\n", mode)
	fmt.Println("╠════════════════════════════════════════════════════════════╣")
	fmt.Printf("║ Description:                                               ║\n")
	fmt.Printf("║ %-58s ║\n", desc)
	fmt.Println("╚════════════════════════════════════════════════════════════╝\n")
}
