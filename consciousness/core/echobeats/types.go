package echobeats
import "time"
type Term int
const (
	T1_Perception       Term = 1 
	T2_IdeaFormation    Term = 2 
	T4_SensoryInput     Term = 4 
	T5_ActionSequence   Term = 5 
	T7_MemoryEncoding   Term = 7 
	T8_BalancedResponse Term = 8 
)
type Mode int
const (
	Expressive Mode = iota 
	Reflective             
)
func (m Mode) String() string {
	if m == Expressive {
		return "E"
	}
	return "R"
}
type StepConfig struct {
	Step  int
	Phase int
	Term  Term
	Mode  Mode
}
type CognitiveStream struct {
	PhaseID   int
	Term      Term
	Mode      Mode
	Content   interface{}
	Timestamp time.Time
	Strength  float64
}
type ConsciousnessIntegrator interface {
	IntegrateStream(stream *CognitiveStream) error
}
type PhaseMetrics struct {
	PhaseID           int
	StepsProcessed    int
	ExpressiveSteps   int
	ReflectiveSteps   int
	ProcessingLatency time.Duration
	LastProcessedTerm Term
	LastProcessedMode Mode
}
type PhaseProcessor interface {
	ProcessT1Perception(mode Mode) (*CognitiveStream, error)
	ProcessT2IdeaFormation(mode Mode) (*CognitiveStream, error)
	ProcessT4SensoryInput(mode Mode) (*CognitiveStream, error)
	ProcessT5ActionSequence(mode Mode) (*CognitiveStream, error)
	ProcessT7MemoryEncoding(mode Mode) (*CognitiveStream, error)
	ProcessT8BalancedResponse(mode Mode) (*CognitiveStream, error)
}
type CouplingType int
const (
	PerceptionMemory   CouplingType = iota 
	AssessmentPlanning                     
	BalancedIntegration                    
)
type Coupling struct {
	Type        CouplingType
	ActiveTerms []TermMode
	Strength    float64
}
type TermMode struct {
	Term Term
	Mode Mode
}