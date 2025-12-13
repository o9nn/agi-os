package echobeats

import "time"

// Term represents a cognitive term in System 4 architecture
type Term int

const (
	T1_Perception       Term = 1 // Perception (Need vs Capacity)
	T2_IdeaFormation    Term = 2 // Idea Formation
	T4_SensoryInput     Term = 4 // Sensory Input
	T5_ActionSequence   Term = 5 // Action Sequence
	T7_MemoryEncoding   Term = 7 // Memory Encoding
	T8_BalancedResponse Term = 8 // Balanced Response
)

// Mode represents processing mode
type Mode int

const (
	Expressive Mode = iota // E - Reactive, Action-oriented
	Reflective             // R - Anticipatory, Simulation-oriented
)

func (m Mode) String() string {
	if m == Expressive {
		return "E"
	}
	return "R"
}

// StepConfig defines the configuration for a single step in the 12-step cycle
type StepConfig struct {
	Step  int
	Phase int
	Term  Term
	Mode  Mode
}

// CognitiveStream represents output from a phase
type CognitiveStream struct {
	PhaseID   int
	Term      Term
	Mode      Mode
	Content   interface{}
	Timestamp time.Time
	Strength  float64
}

// ConsciousnessIntegrator integrates cognitive streams into consciousness
type ConsciousnessIntegrator interface {
	IntegrateStream(stream *CognitiveStream) error
}

// PhaseMetrics tracks metrics for a single phase
type PhaseMetrics struct {
	PhaseID           int
	StepsProcessed    int
	ExpressiveSteps   int
	ReflectiveSteps   int
	ProcessingLatency time.Duration
	LastProcessedTerm Term
	LastProcessedMode Mode
}

// PhaseProcessor interface for processing cognitive terms
type PhaseProcessor interface {
	ProcessT1Perception(mode Mode) (*CognitiveStream, error)
	ProcessT2IdeaFormation(mode Mode) (*CognitiveStream, error)
	ProcessT4SensoryInput(mode Mode) (*CognitiveStream, error)
	ProcessT5ActionSequence(mode Mode) (*CognitiveStream, error)
	ProcessT7MemoryEncoding(mode Mode) (*CognitiveStream, error)
	ProcessT8BalancedResponse(mode Mode) (*CognitiveStream, error)
}

// CouplingType represents types of tensional couplings
type CouplingType int

const (
	PerceptionMemory   CouplingType = iota // T4E ↔ T7R
	AssessmentPlanning                     // T1R ↔ T2E
	BalancedIntegration                    // T8E
)

// Coupling represents a tensional coupling between cognitive streams
type Coupling struct {
	Type        CouplingType
	ActiveTerms []TermMode
	Strength    float64
}

// TermMode represents a term with its mode
type TermMode struct {
	Term Term
	Mode Mode
}
