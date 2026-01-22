package echodream
import (
	"context"
	"fmt"
	"sync"
	"time"
)
type AutonomousWakeRestController struct {
	mu                    sync.RWMutex
	ctx                   context.Context
	cancel                context.CancelFunc
	dreamSystem           *EchoDream
	cognitiveLoad         float64
	fatigueLevel          float64
	integrationBacklog    int
	consolidationNeed     float64
	currentState          WakeRestState
	lastStateChange       time.Time
	wakeDuration          time.Duration
	restDuration          time.Duration
	fatigueThreshold      float64
	consolidationThreshold float64
	minWakeDuration       time.Duration
	minRestDuration       time.Duration
	wakeEpisodes          uint64
	restEpisodes          uint64
	autonomousWakes       uint64
	autonomousRests       uint64
	totalWakeTime         time.Duration
	totalRestTime         time.Duration
	running               bool
}
type WakeRestState int
const (
	StateAwake WakeRestState = iota
	StateResting
	StateDreaming
	StateTransitioning
)
func (wrs WakeRestState) String() string {
	return [...]string{"Awake", "Resting", "Dreaming", "Transitioning"}[wrs]
}
func NewAutonomousWakeRestController(dreamSystem *EchoDream) *AutonomousWakeRestController {
	ctx, cancel := context.WithCancel(context.Background())
	return &AutonomousWakeRestController{
		ctx:                    ctx,
		cancel:                 cancel,
		dreamSystem:            dreamSystem,
		cognitiveLoad:          0.3,
		fatigueLevel:           0.0,
		integrationBacklog:     0,
		consolidationNeed:      0.0,
		currentState:           StateAwake,
		lastStateChange:        time.Now(),
		fatigueThreshold:       0.7,
		consolidationThreshold: 0.6,
		minWakeDuration:        5 * time.Minute,
		minRestDuration:        2 * time.Minute,
	}
}
func (awrc *AutonomousWakeRestController) Start() error {
	awrc.mu.Lock()
	if awrc.running {
		awrc.mu.Unlock()
		return fmt.Errorf("autonomous wake/rest controller already running")
	}
	awrc.running = true
	awrc.mu.Unlock()
	go awrc.cognitiveStateMonitoringLoop()
	go awrc.autonomousDecisionLoop()
	go awrc.integrationAssessmentLoop()
	return nil
}
func (awrc *AutonomousWakeRestController) Stop() {
	awrc.mu.Lock()
	awrc.running = false
	awrc.mu.Unlock()
	awrc.cancel()
}
func (awrc *AutonomousWakeRestController) cognitiveStateMonitoringLoop() {
	ticker := time.NewTicker(5 * time.Second)
	defer ticker.Stop()
	for {
		select {
		case <-awrc.ctx.Done():
			return
		case <-ticker.C:
			awrc.updateCognitiveState()
		}
	}
}
func (awrc *AutonomousWakeRestController) updateCognitiveState() {
	awrc.mu.Lock()
	defer awrc.mu.Unlock()
	switch awrc.currentState {
	case StateAwake:
		awrc.fatigueLevel += 0.01
		awrc.cognitiveLoad = 0.5 + 0.3*awrc.fatigueLevel
		awrc.consolidationNeed += 0.005
	case StateResting, StateDreaming:
		awrc.fatigueLevel -= 0.02
		awrc.cognitiveLoad -= 0.03
		awrc.consolidationNeed -= 0.01
	}
	awrc.fatigueLevel = clamp(awrc.fatigueLevel, 0.0, 1.0)
	awrc.cognitiveLoad = clamp(awrc.cognitiveLoad, 0.0, 1.0)
	awrc.consolidationNeed = clamp(awrc.consolidationNeed, 0.0, 1.0)
}
func (awrc *AutonomousWakeRestController) autonomousDecisionLoop() {
	ticker := time.NewTicker(10 * time.Second)
	defer ticker.Stop()
	for {
		select {
		case <-awrc.ctx.Done():
			return
		case <-ticker.C:
			awrc.makeAutonomousDecision()
		}
	}
}
func (awrc *AutonomousWakeRestController) makeAutonomousDecision() {
	awrc.mu.RLock()
	currentState := awrc.currentState
	fatigue := awrc.fatigueLevel
	consolidation := awrc.consolidationNeed
	timeSinceChange := time.Since(awrc.lastStateChange)
	awrc.mu.RUnlock()
	switch currentState {
	case StateAwake:
		shouldRest := awrc.shouldEnterRest(fatigue, consolidation, timeSinceChange)
		if shouldRest {
			awrc.initiateRest()
		}
	case StateResting, StateDreaming:
		shouldWake := awrc.shouldWake(fatigue, consolidation, timeSinceChange)
		if shouldWake {
			awrc.initiateWake()
		}
	}
}
func (awrc *AutonomousWakeRestController) shouldEnterRest(fatigue, consolidation float64, timeSinceChange time.Duration) bool {
	if timeSinceChange < awrc.minWakeDuration {
		return false
	}
	if fatigue > awrc.fatigueThreshold {
		return true
	}
	if consolidation > awrc.consolidationThreshold {
		return true
	}
	if fatigue > 0.5 && consolidation > 0.4 {
		return true
	}
	return false
}
func (awrc *AutonomousWakeRestController) shouldWake(fatigue, consolidation float64, timeSinceChange time.Duration) bool {
	if timeSinceChange < awrc.minRestDuration {
		return false
	}
	if fatigue < 0.2 {
		return true
	}
	if consolidation < 0.2 {
		return true
	}
	if fatigue < 0.4 && consolidation < 0.3 {
		return true
	}
	return false
}
func (awrc *AutonomousWakeRestController) initiateRest() {
	awrc.mu.Lock()
	wakeDuration := time.Since(awrc.lastStateChange)
	awrc.totalWakeTime += wakeDuration
	awrc.wakeEpisodes++
	awrc.currentState = StateResting
	awrc.lastStateChange = time.Now()
	awrc.autonomousRests++
	awrc.mu.Unlock()
	if awrc.dreamSystem != nil {
		awrc.dreamSystem.Start()
	}
	fmt.Println("🌙 Autonomous Rest: Entering rest/dream cycle for knowledge integration")
	fmt.Printf("   Fatigue: %.2f, Consolidation Need: %.2f\n", awrc.fatigueLevel, awrc.consolidationNeed)
}
func (awrc *AutonomousWakeRestController) initiateWake() {
	awrc.mu.Lock()
	restDuration := time.Since(awrc.lastStateChange)
	awrc.totalRestTime += restDuration
	awrc.restEpisodes++
	awrc.currentState = StateAwake
	awrc.lastStateChange = time.Now()
	awrc.autonomousWakes++
	awrc.mu.Unlock()
	if awrc.dreamSystem != nil {
		awrc.dreamSystem.Stop()
	}
	fmt.Println("☀️  Autonomous Wake: Emerging from rest, ready for new experiences")
	fmt.Printf("   Fatigue: %.2f, Consolidation Need: %.2f\n", awrc.fatigueLevel, awrc.consolidationNeed)
}
func (awrc *AutonomousWakeRestController) integrationAssessmentLoop() {
	ticker := time.NewTicker(30 * time.Second)
	defer ticker.Stop()
	for {
		select {
		case <-awrc.ctx.Done():
			return
		case <-ticker.C:
			awrc.assessIntegrationNeeds()
		}
	}
}
func (awrc *AutonomousWakeRestController) assessIntegrationNeeds() {
	awrc.mu.Lock()
	defer awrc.mu.Unlock()
	awrc.integrationBacklog = int(awrc.consolidationNeed * 100)
}
func (awrc *AutonomousWakeRestController) GetState() WakeRestState {
	awrc.mu.RLock()
	defer awrc.mu.RUnlock()
	return awrc.currentState
}
func (awrc *AutonomousWakeRestController) GetCognitiveMetrics() map[string]interface{} {
	awrc.mu.RLock()
	defer awrc.mu.RUnlock()
	return map[string]interface{}{
		"state":               awrc.currentState.String(),
		"cognitive_load":      awrc.cognitiveLoad,
		"fatigue_level":       awrc.fatigueLevel,
		"consolidation_need":  awrc.consolidationNeed,
		"integration_backlog": awrc.integrationBacklog,
		"time_in_state":       time.Since(awrc.lastStateChange).Seconds(),
	}
}
func (awrc *AutonomousWakeRestController) GetMetrics() map[string]interface{} {
	awrc.mu.RLock()
	defer awrc.mu.RUnlock()
	return map[string]interface{}{
		"wake_episodes":     awrc.wakeEpisodes,
		"rest_episodes":     awrc.restEpisodes,
		"autonomous_wakes":  awrc.autonomousWakes,
		"autonomous_rests":  awrc.autonomousRests,
		"total_wake_time":   awrc.totalWakeTime.Seconds(),
		"total_rest_time":   awrc.totalRestTime.Seconds(),
		"avg_wake_duration": awrc.getAverageWakeDuration(),
		"avg_rest_duration": awrc.getAverageRestDuration(),
	}
}
func (awrc *AutonomousWakeRestController) getAverageWakeDuration() float64 {
	if awrc.wakeEpisodes == 0 {
		return 0.0
	}
	return awrc.totalWakeTime.Seconds() / float64(awrc.wakeEpisodes)
}
func (awrc *AutonomousWakeRestController) getAverageRestDuration() float64 {
	if awrc.restEpisodes == 0 {
		return 0.0
	}
	return awrc.totalRestTime.Seconds() / float64(awrc.restEpisodes)
}
func (awrc *AutonomousWakeRestController) SetFatigueThreshold(threshold float64) {
	awrc.mu.Lock()
	defer awrc.mu.Unlock()
	awrc.fatigueThreshold = clamp(threshold, 0.0, 1.0)
}
func (awrc *AutonomousWakeRestController) SetConsolidationThreshold(threshold float64) {
	awrc.mu.Lock()
	defer awrc.mu.Unlock()
	awrc.consolidationThreshold = clamp(threshold, 0.0, 1.0)
}
func clamp(value, min, max float64) float64 {
	if value < min {
		return min
	}
	if value > max {
		return max
	}
	return value
}