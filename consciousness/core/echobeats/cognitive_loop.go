package echobeats
import (
	"context"
	"fmt"
	"sync"
	"time"
)
type CognitiveLoop struct {
	mu              sync.RWMutex
	ctx             context.Context
	cancel          context.CancelFunc
	currentStep     int
	cycleCount      uint64
	stepHistory     []StepExecution
	maxHistory      int
	stepProcessors  map[int]StepProcessor
	currentState    *CognitiveState
	stateHistory    []*CognitiveState
	stepDuration    time.Duration
	cycleStartTime  time.Time
	onStepComplete  func(step int, result *StepResult)
	onCycleComplete func(cycle uint64)
	totalSteps      uint64
	avgStepTime     time.Duration
	running         bool
	paused          bool
}
type CognitiveState struct {
	Timestamp          time.Time              `json:"timestamp"`
	CycleNumber        uint64                 `json:"cycle_number"`
	StepNumber         int                    `json:"step_number"`
	Mode               CognitiveMode          `json:"mode"`
	Attention          []string               `json:"attention"`
	WorkingMemory      map[string]interface{} `json:"working_memory"`
	EmotionalTone      map[string]float64     `json:"emotional_tone"`
	CognitiveLoad      float64                `json:"cognitive_load"`
	RelevanceScores    map[string]float64     `json:"relevance_scores"`
	ActiveGoals        []string               `json:"active_goals"`
	PendingActions     []string               `json:"pending_actions"`
	Insights           []string               `json:"insights"`
}
type CognitiveMode string
const (
	ModeExpressive           CognitiveMode = "expressive"
	ModeReflective           CognitiveMode = "reflective"
	ModeRelevanceRealization CognitiveMode = "relevance_realization"
	ModeMetaCognitive        CognitiveMode = "metacognitive"
)
type StepResult struct {
	Success         bool
	Output          interface{}
	StateUpdates    map[string]interface{}
	NextStepHint    int
	RelevanceShift  float64
	CognitiveLoad   float64
	Insights        []string
	Error           error
}
type StepProcessor interface {
	Process(ctx context.Context, state *CognitiveState) (*StepResult, error)
	GetMode() CognitiveMode
	GetDescription() string
}
func NewCognitiveLoop() *CognitiveLoop {
	ctx, cancel := context.WithCancel(context.Background())
	cl := &CognitiveLoop{
		ctx:            ctx,
		cancel:         cancel,
		currentStep:    1,
		cycleCount:     0,
		stepHistory:    make([]StepExecution, 0),
		maxHistory:     1000,
		stepProcessors: make(map[int]StepProcessor),
		stateHistory:   make([]*CognitiveState, 0),
		stepDuration:   2 * time.Second,
	}
	cl.currentState = &CognitiveState{
		Timestamp:       time.Now(),
		CycleNumber:     0,
		StepNumber:      1,
		Mode:            ModeExpressive,
		Attention:       make([]string, 0),
		WorkingMemory:   make(map[string]interface{}),
		EmotionalTone:   make(map[string]float64),
		CognitiveLoad:   0.0,
		RelevanceScores: make(map[string]float64),
		ActiveGoals:     make([]string, 0),
		PendingActions:  make([]string, 0),
		Insights:        make([]string, 0),
	}
	cl.registerDefaultProcessors()
	return cl
}
func (cl *CognitiveLoop) registerDefaultProcessors() {
	cl.stepProcessors[1] = &PerceptionProcessor{}
	cl.stepProcessors[2] = &MemoryActivationProcessor{}
	cl.stepProcessors[3] = &ActionGenerationProcessor{}
	cl.stepProcessors[4] = &ActionExecutionProcessor{}
	cl.stepProcessors[5] = &RelevanceRealizationProcessor{phase: "present_commitment"}
	cl.stepProcessors[6] = &ScenarioSimulationProcessor{}
	cl.stepProcessors[7] = &OutcomeEvaluationProcessor{}
	cl.stepProcessors[8] = &ModelUpdateProcessor{}
	cl.stepProcessors[9] = &LearningConsolidationProcessor{}
	cl.stepProcessors[10] = &InsightGenerationProcessor{}
	cl.stepProcessors[11] = &RelevanceRealizationProcessor{phase: "future_commitment"}
	cl.stepProcessors[12] = &MetaCognitiveProcessor{}
}
func (cl *CognitiveLoop) RegisterStepProcessor(step int, processor StepProcessor) {
	cl.mu.Lock()
	defer cl.mu.Unlock()
	cl.stepProcessors[step] = processor
}
func (cl *CognitiveLoop) Start() error {
	cl.mu.Lock()
	if cl.running {
		cl.mu.Unlock()
		return fmt.Errorf("cognitive loop already running")
	}
	cl.running = true
	cl.cycleStartTime = time.Now()
	cl.mu.Unlock()
	fmt.Println("🔄 CognitiveLoop: Starting 12-step cognitive processing...")
	fmt.Printf("   Step Duration: %v\n", cl.stepDuration)
	fmt.Println("   Mode Sequence: Expressive(1-4) → Relevance(5) → Reflective(6-10) → Relevance(11) → MetaCognitive(12)")
	go cl.run()
	return nil
}
func (cl *CognitiveLoop) Stop() error {
	cl.mu.Lock()
	defer cl.mu.Unlock()
	if !cl.running {
		return fmt.Errorf("cognitive loop not running")
	}
	fmt.Println("🔄 CognitiveLoop: Stopping...")
	cl.running = false
	cl.cancel()
	return nil
}
func (cl *CognitiveLoop) Pause() {
	cl.mu.Lock()
	defer cl.mu.Unlock()
	cl.paused = true
	fmt.Println("⏸️  CognitiveLoop: Paused")
}
func (cl *CognitiveLoop) Resume() {
	cl.mu.Lock()
	defer cl.mu.Unlock()
	cl.paused = false
	fmt.Println("▶️  CognitiveLoop: Resumed")
}
func (cl *CognitiveLoop) run() {
	ticker := time.NewTicker(cl.stepDuration)
	defer ticker.Stop()
	for {
		select {
		case <-cl.ctx.Done():
			return
		case <-ticker.C:
			cl.mu.RLock()
			isPaused := cl.paused
			cl.mu.RUnlock()
			if !isPaused {
				cl.executeStep()
			}
		}
	}
}
func (cl *CognitiveLoop) executeStep() {
	cl.mu.Lock()
	step := cl.currentStep
	processor := cl.stepProcessors[step]
	state := cl.currentState
	cl.mu.Unlock()
	if processor == nil {
		fmt.Printf("⚠️  CognitiveLoop: No processor for step %d\n", step)
		cl.advanceStep()
		return
	}
	startTime := time.Now()
	state.StepNumber = step
	state.Mode = processor.GetMode()
	state.Timestamp = startTime
	result, err := processor.Process(cl.ctx, state)
	duration := time.Since(startTime)
	execution := StepExecution{
		StepNumber: step,
		StartTime:  startTime,
		Duration:   duration,
		Mode:       processor.GetMode(),
		Success:    err == nil && result != nil && result.Success,
		Output:     nil,
		Error:      err,
	}
	if result != nil {
		execution.Output = result.Output
		cl.applyStateUpdates(result.StateUpdates)
		state.CognitiveLoad = result.CognitiveLoad
		if len(result.Insights) > 0 {
			state.Insights = append(state.Insights, result.Insights...)
		}
	}
	cl.mu.Lock()
	cl.stepHistory = append(cl.stepHistory, execution)
	if len(cl.stepHistory) > cl.maxHistory {
		cl.stepHistory = cl.stepHistory[len(cl.stepHistory)-cl.maxHistory:]
	}
	cl.totalSteps++
	cl.mu.Unlock()
	if cl.onStepComplete != nil {
		cl.onStepComplete(step, result)
	}
	modeEmoji := cl.getModeEmoji(processor.GetMode())
	fmt.Printf("%s Step %2d/%2d: %s (%.2fs)\n", 
		modeEmoji, step, 12, processor.GetDescription(), duration.Seconds())
	cl.advanceStep()
}
func (cl *CognitiveLoop) advanceStep() {
	cl.mu.Lock()
	defer cl.mu.Unlock()
	cl.currentStep++
	if cl.currentStep > 12 {
		cl.currentStep = 1
		cl.cycleCount++
		cycleDuration := time.Since(cl.cycleStartTime)
		cl.cycleStartTime = time.Now()
		fmt.Printf("\n🔄 Cycle %d complete (duration: %s)\n", cl.cycleCount, cycleDuration)
		fmt.Printf("   Insights generated: %d\n", len(cl.currentState.Insights))
		fmt.Printf("   Cognitive load: %.2f\n\n", cl.currentState.CognitiveLoad)
		stateCopy := *cl.currentState
		cl.stateHistory = append(cl.stateHistory, &stateCopy)
		cl.currentState.Insights = make([]string, 0)
		cl.currentState.CycleNumber = cl.cycleCount
		if cl.onCycleComplete != nil {
			cl.onCycleComplete(cl.cycleCount)
		}
	}
}
func (cl *CognitiveLoop) applyStateUpdates(updates map[string]interface{}) {
	if updates == nil {
		return
	}
	for key, value := range updates {
		cl.currentState.WorkingMemory[key] = value
	}
}
func (cl *CognitiveLoop) getModeEmoji(mode CognitiveMode) string {
	switch mode {
	case ModeExpressive:
		return "🎭"
	case ModeReflective:
		return "🤔"
	case ModeRelevanceRealization:
		return "🎯"
	case ModeMetaCognitive:
		return "🧠"
	default:
		return "⚙️"
	}
}
func (cl *CognitiveLoop) GetCurrentState() *CognitiveState {
	cl.mu.RLock()
	defer cl.mu.RUnlock()
	stateCopy := *cl.currentState
	return &stateCopy
}
func (cl *CognitiveLoop) GetMetrics() map[string]interface{} {
	cl.mu.RLock()
	defer cl.mu.RUnlock()
	return map[string]interface{}{
		"current_step":    cl.currentStep,
		"cycle_count":     cl.cycleCount,
		"total_steps":     cl.totalSteps,
		"current_mode":    cl.currentState.Mode,
		"cognitive_load":  cl.currentState.CognitiveLoad,
		"insights_count":  len(cl.currentState.Insights),
		"running":         cl.running,
		"paused":          cl.paused,
	}
}
func (cl *CognitiveLoop) SetStepDuration(duration time.Duration) {
	cl.mu.Lock()
	defer cl.mu.Unlock()
	cl.stepDuration = duration
}
func (cl *CognitiveLoop) SetCallbacks(
	onStepComplete func(step int, result *StepResult),
	onCycleComplete func(cycle uint64),
) {
	cl.mu.Lock()
	defer cl.mu.Unlock()
	cl.onStepComplete = onStepComplete
	cl.onCycleComplete = onCycleComplete
}