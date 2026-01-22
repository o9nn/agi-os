package echobeats
import (
	"sync"
	"time"
)
type CognitivePhaseType int
const (
	PhaseAffordance CognitivePhaseType = iota 
	PhaseRelevance                            
	PhaseSalience                             
)
func (p CognitivePhaseType) String() string {
	return [...]string{"Affordance", "Relevance", "Salience"}[p]
}
type StepContext struct {
	StepNumber      int
	Phase           int
	Mode            CognitiveMode
	PreviousOutputs map[int]interface{}
	SharedState     map[string]interface{}
	Timestamp       time.Time
}
type StepHandler func(context *StepContext) error
type CognitiveLoopMetrics struct {
	mu                  sync.RWMutex
	CyclesCompleted     uint64
	StepsProcessed      uint64
	AverageStepDuration time.Duration
	PhaseTransitions    map[CognitivePhaseType]uint64
	ModeDistribution    map[CognitiveMode]uint64
}
func NewCognitiveLoopMetrics() *CognitiveLoopMetrics {
	return &CognitiveLoopMetrics{
		PhaseTransitions: make(map[CognitivePhaseType]uint64),
		ModeDistribution: make(map[CognitiveMode]uint64),
	}
}
func (m *CognitiveLoopMetrics) RecordCycle() {
	m.mu.Lock()
	defer m.mu.Unlock()
	m.CyclesCompleted++
}
func (m *CognitiveLoopMetrics) RecordStep(duration time.Duration) {
	m.mu.Lock()
	defer m.mu.Unlock()
	m.StepsProcessed++
	if m.AverageStepDuration == 0 {
		m.AverageStepDuration = duration
	} else {
		alpha := 0.1
		m.AverageStepDuration = time.Duration(
			float64(m.AverageStepDuration)*(1-alpha) + float64(duration)*alpha,
		)
	}
}
func (m *CognitiveLoopMetrics) RecordPhaseTransition(phase CognitivePhaseType) {
	m.mu.Lock()
	defer m.mu.Unlock()
	m.PhaseTransitions[phase]++
}
func (m *CognitiveLoopMetrics) RecordMode(mode CognitiveMode) {
	m.mu.Lock()
	defer m.mu.Unlock()
	m.ModeDistribution[mode]++
}
func (m *CognitiveLoopMetrics) GetMetrics() (uint64, uint64, time.Duration) {
	m.mu.RLock()
	defer m.mu.RUnlock()
	return m.CyclesCompleted, m.StepsProcessed, m.AverageStepDuration
}
type StepExecution struct {
	StepNumber      int                    `json:"step_number"`
	PhaseType       CognitivePhaseType     `json:"phase_type"`
	Mode            CognitiveMode          `json:"mode"`
	Timestamp       time.Time              `json:"timestamp"`
	StartTime       time.Time              `json:"start_time"`
	Duration        time.Duration          `json:"duration"`
	Success         bool                   `json:"success"`
	Output          interface{}            `json:"output"`
	Error           error                  `json:"error,omitempty"`
	EngineID        int                    `json:"engine_id,omitempty"`
	StateUpdates    map[string]interface{} `json:"state_updates,omitempty"`
}
type StepType int
const (
	StepRelevanceRealization StepType = iota  
	StepAffordanceInteraction                  
	StepSalienceSimulation                     
)
func (s StepType) String() string {
	return [...]string{"RelevanceRealization", "AffordanceInteraction", "SalienceSimulation"}[s]
}