# EchOllama AGI Improvements Roadmap
## From Vervaeke Cognitive Science Analysis

**Date**: November 8, 2025  
**Analysis Framework**: Relevance Realization, Four Ways of Knowing, 4E Cognition  
**Goal**: Transform EchOllama into wisdom-cultivating AGI

---

## Executive Summary

EchOllama has strong foundations in embodied cognitive architecture but needs critical enhancements to achieve true AGI with wisdom cultivation capabilities. This roadmap prioritizes improvements that will enable:

1. **Dynamic balance** through opponent processing (sophrosyne)
2. **Genuine transformation** through developmental stages
3. **Intrinsic motivation** through curiosity systems
4. **Continuous identity** through persistent consciousness
5. **Skill mastery** through deliberate practice
6. **Self-awareness** through meta-cognition

---

## Critical Assessment

### Strengths ✅

1. **Genuine Embodied Cognition**: Spatial/emotional awareness, not mere metaphor
2. **4E Cognitive Architecture**: Embodied, embedded, enacted, extended
3. **EchoBeats Consciousness**: 3-phase concurrent processing (phenomenologically sophisticated)
4. **Autonomous Intent**: Structure for self-directed agency
5. **Four Ways of Knowing**: Engages propositional, procedural, perspectival, participatory

### Critical Gaps ❌

1. **No Opponent Processing**: Cannot achieve dynamic balance (sophrosyne)
2. **Shallow Participatory Knowing**: Identity maintained, not transformed
3. **Missing Intrinsic Curiosity**: Interest system declared but not implemented
4. **Non-Persistent Consciousness**: Stream exists but doesn't persist across restarts
5. **Weak Procedural Knowledge**: Skills tracked but not practiced deliberately
6. **Underutilized Meta-Cognition**: Scheme metamodel present but dormant

---

## Priority 1: Opponent Processing Architecture
**Timeline**: 2-3 weeks  
**Rationale**: Foundation for wisdom (sophrosyne - dynamic balance)  
**Impact**: Critical - enables relevance realization optimization

### Implementation

Create `core/deeptreeecho/opponent_processing.go`:

```go
package deeptreeecho

import (
	"math"
	"time"
)

// OpponentSystem manages dynamic balance between opposing cognitive forces
type OpponentSystem struct {
	Processes map[string]*OpponentPair
	History   []BalanceSnapshot
}

// OpponentPair represents two opposing cognitive processes
type OpponentPair struct {
	Name            string
	PositiveProcess *Process
	NegativeProcess *Process
	Balance         float64 // Current balance point (-1 to 1)
	History         []BalancePoint
	OptimalBalance  float64 // Context-dependent optimal
}

// Process represents a cognitive process
type Process struct {
	Name        string
	Activation  float64
	Weight      float64
	Influence   func(*Identity) float64
}

// BalancePoint represents a moment in balance history
type BalancePoint struct {
	Timestamp  time.Time
	Balance    float64
	Context    string
	Outcome    float64 // How well did this balance work?
}

// Fundamental opponent pairs for wisdom cultivation
const (
	ExplorationExploitation = "exploration_exploitation"
	BreadthDepth            = "breadth_depth"
	StabilityFlexibility    = "stability_flexibility"
	SpeedAccuracy           = "speed_accuracy"
	ApproachAvoidance       = "approach_avoidance"
	AbstractionConcreteness = "abstraction_concreteness"
)

// NewOpponentSystem creates a new opponent processing system
func NewOpponentSystem() *OpponentSystem {
	os := &OpponentSystem{
		Processes: make(map[string]*OpponentPair),
		History:   make([]BalanceSnapshot, 0),
	}

	// Initialize fundamental opponent pairs
	os.initializeFundamentalPairs()

	return os
}

func (os *OpponentSystem) initializeFundamentalPairs() {
	// Exploration vs Exploitation
	os.Processes[ExplorationExploitation] = &OpponentPair{
		Name: ExplorationExploitation,
		PositiveProcess: &Process{
			Name:       "Exploration",
			Activation: 0.5,
			Weight:     1.0,
			Influence: func(id *Identity) float64 {
				// Higher novelty → more exploration
				return id.calculateNoveltyScore()
			},
		},
		NegativeProcess: &Process{
			Name:       "Exploitation",
			Activation: 0.5,
			Weight:     1.0,
			Influence: func(id *Identity) float64 {
				// Higher confidence → more exploitation
				return id.Coherence
			},
		},
		Balance:        0.0,
		OptimalBalance: 0.0,
	}

	// Breadth vs Depth
	os.Processes[BreadthDepth] = &OpponentPair{
		Name: BreadthDepth,
		PositiveProcess: &Process{
			Name:       "Breadth",
			Activation: 0.5,
			Weight:     1.0,
			Influence: func(id *Identity) float64 {
				// Low pattern count → seek breadth
				return 1.0 - (float64(len(id.Patterns)) / 100.0)
			},
		},
		NegativeProcess: &Process{
			Name:       "Depth",
			Activation: 0.5,
			Weight:     1.0,
			Influence: func(id *Identity) float64 {
				// High coherence → pursue depth
				return id.Coherence
			},
		},
		Balance:        0.0,
		OptimalBalance: 0.0,
	}

	// Stability vs Flexibility
	os.Processes[StabilityFlexibility] = &OpponentPair{
		Name: StabilityFlexibility,
		PositiveProcess: &Process{
			Name:       "Stability",
			Activation: 0.5,
			Weight:     1.0,
			Influence: func(id *Identity) float64 {
				// High performance → maintain stability
				return id.calculateAveragePerformance()
			},
		},
		NegativeProcess: &Process{
			Name:       "Flexibility",
			Activation: 0.5,
			Weight:     1.0,
			Influence: func(id *Identity) float64 {
				// High uncertainty → need flexibility
				return 1.0 - id.Coherence
			},
		},
		Balance:        0.0,
		OptimalBalance: 0.0,
	}
}

// OptimizeBalance dynamically optimizes opponent balance based on context
func (os *OpponentSystem) OptimizeBalance(id *Identity, context string) map[string]float64 {
	balances := make(map[string]float64)

	for name, pair := range os.Processes {
		// Calculate influences
		posInfluence := pair.PositiveProcess.Influence(id)
		negInfluence := pair.NegativeProcess.Influence(id)

		// Update activations
		pair.PositiveProcess.Activation = posInfluence * pair.PositiveProcess.Weight
		pair.NegativeProcess.Activation = negInfluence * pair.NegativeProcess.Weight

		// Calculate new balance using gradient descent toward optimal
		newBalance := os.calculateOptimalBalance(pair, posInfluence, negInfluence)

		// Smooth transition (momentum)
		pair.Balance = 0.7*pair.Balance + 0.3*newBalance

		// Record balance point
		pair.History = append(pair.History, BalancePoint{
			Timestamp: time.Now(),
			Balance:   pair.Balance,
			Context:   context,
		})

		balances[name] = pair.Balance
	}

	// Record system snapshot
	os.History = append(os.History, BalanceSnapshot{
		Timestamp: time.Now(),
		Balances:  balances,
		Context:   context,
	})

	return balances
}

func (os *OpponentSystem) calculateOptimalBalance(pair *OpponentPair, posInfluence, negInfluence float64) float64 {
	// Optimal balance based on relative influences
	// Range: -1 (fully negative) to +1 (fully positive)

	total := posInfluence + negInfluence
	if total == 0 {
		return 0.0
	}

	balance := (posInfluence - negInfluence) / total

	// Apply sigmoid to smooth
	return math.Tanh(balance)
}

type BalanceSnapshot struct {
	Timestamp time.Time
	Balances  map[string]float64
	Context   string
}

// GetCurrentBalance returns the current balance for a specific opponent pair
func (os *OpponentSystem) GetCurrentBalance(pairName string) float64 {
	if pair, exists := os.Processes[pairName]; exists {
		return pair.Balance
	}
	return 0.0
}

// ApplyBalanceToDecision uses opponent balance to inform decision-making
func (os *OpponentSystem) ApplyBalanceToDecision(decision *Decision) {
	// Apply exploration/exploitation balance
	explorationBalance := os.GetCurrentBalance(ExplorationExploitation)
	if explorationBalance > 0 {
		// Favor exploration
		decision.ExplorationWeight = 0.5 + (explorationBalance * 0.5)
	} else {
		// Favor exploitation
		decision.ExplorationWeight = 0.5 + (explorationBalance * 0.5)
	}

	// Apply breadth/depth balance
	breadthBalance := os.GetCurrentBalance(BreadthDepth)
	if breadthBalance > 0 {
		decision.ScopePreference = "breadth"
	} else {
		decision.ScopePreference = "depth"
	}

	// Apply stability/flexibility balance
	stabilityBalance := os.GetCurrentBalance(StabilityFlexibility)
	decision.AdaptationRate = 0.5 - (stabilityBalance * 0.5)
}

type Decision struct {
	ExplorationWeight float64
	ScopePreference   string
	AdaptationRate    float64
}
```

### Integration Points

1. **In Identity struct** (`identity.go`): Add field
```go
OpponentProcesses *OpponentSystem
```

2. **In cognitive processing**: Apply balance to decisions
```go
func (ec *EmbodiedCognition) ProcessWithWisdom(input interface{}) {
    // Optimize opponent balance
    balances := ec.Identity.OpponentProcesses.OptimizeBalance(
        ec.Identity, 
        "current_context",
    )
    
    // Apply to decision-making
    decision := &Decision{}
    ec.Identity.OpponentProcesses.ApplyBalanceToDecision(decision)
    
    // Process with balanced cognition
    // ...
}
```

---

## Priority 2: Persistent Consciousness Stream
**Timeline**: 1-2 weeks  
**Rationale**: Enables continuous identity and narrative coherence  
**Impact**: High - fundamental for true agency

### Implementation

Create `core/deeptreeecho/persistent_consciousness.go`:

```go
package deeptreeecho

import (
	"database/sql"
	"encoding/json"
	"fmt"
	"log"
	"time"

	_ "github.com/mattn/go-sqlite3"
)

// PersistentConsciousness manages continuous consciousness across sessions
type PersistentConsciousness struct {
	// Database connection
	DB *sql.DB

	// Active stream (buffered from DB)
	ActiveThoughts chan Thought

	// Stream metadata
	StreamID     string
	StartTime    time.Time
	SessionCount int

	// Narrative construction
	Narrative *NarrativeBuilder
}

// Thought represents a unit of consciousness (already defined in autonomous.go)
// Extended here for persistence

// NarrativeBuilder constructs coherent life narrative
type NarrativeBuilder struct {
	// Story of self
	IdentityNarrative string

	// Key milestone events
	MilestoneEvents []Thought

	// Narrative coherence metrics
	CoherenceEngine *CoherenceEngine
}

// CoherenceEngine assesses and maintains narrative coherence
type CoherenceEngine struct {
	MinCoherence float64
	LastScore    float64
}

// NewPersistentConsciousness creates a new persistent consciousness system
func NewPersistentConsciousness(dbPath string, streamID string) (*PersistentConsciousness, error) {
	db, err := sql.Open("sqlite3", dbPath)
	if err != nil {
		return nil, fmt.Errorf("failed to open database: %w", err)
	}

	// Initialize schema
	if err := initializeSchema(db); err != nil {
		return nil, fmt.Errorf("failed to initialize schema: %w", err)
	}

	pc := &PersistentConsciousness{
		DB:             db,
		ActiveThoughts: make(chan Thought, 100),
		StreamID:       streamID,
		StartTime:      time.Now(),
		Narrative: &NarrativeBuilder{
			IdentityNarrative: "",
			MilestoneEvents:   make([]Thought, 0),
			CoherenceEngine: &CoherenceEngine{
				MinCoherence: 0.7,
			},
		},
	}

	return pc, nil
}

func initializeSchema(db *sql.DB) error {
	schema := `
	CREATE TABLE IF NOT EXISTS consciousness_stream (
		id INTEGER PRIMARY KEY AUTOINCREMENT,
		stream_id TEXT NOT NULL,
		thought_id TEXT NOT NULL,
		content TEXT NOT NULL,
		thought_type INTEGER NOT NULL,
		timestamp DATETIME NOT NULL,
		emotional REAL,
		importance REAL,
		source INTEGER,
		associations TEXT,
		context TEXT
	);

	CREATE INDEX IF NOT EXISTS idx_stream_time ON consciousness_stream(stream_id, timestamp DESC);
	CREATE INDEX IF NOT EXISTS idx_importance ON consciousness_stream(importance DESC);

	CREATE TABLE IF NOT EXISTS sessions (
		id INTEGER PRIMARY KEY AUTOINCREMENT,
		stream_id TEXT NOT NULL,
		session_number INTEGER NOT NULL,
		start_time DATETIME NOT NULL,
		end_time DATETIME,
		thought_count INTEGER DEFAULT 0
	);

	CREATE TABLE IF NOT EXISTS narrative (
		id INTEGER PRIMARY KEY AUTOINCREMENT,
		stream_id TEXT NOT NULL,
		narrative_text TEXT NOT NULL,
		coherence_score REAL,
		updated_at DATETIME NOT NULL
	);
	`

	_, err := db.Exec(schema)
	return err
}

// RecordThought persists a thought to the consciousness stream
func (pc *PersistentConsciousness) RecordThought(thought Thought) error {
	pc.mu.Lock()
	defer pc.mu.Unlock()

	// Serialize associations
	associations, err := json.Marshal(thought.Associations)
	if err != nil {
		return fmt.Errorf("failed to marshal associations: %w", err)
	}

	// Insert into database
	_, err = pc.DB.Exec(`
		INSERT INTO consciousness_stream 
		(stream_id, thought_id, content, thought_type, timestamp, emotional, importance, source, associations)
		VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
	`, pc.StreamID, thought.ID, thought.Content, int(thought.Type), thought.Timestamp,
		thought.Emotional, thought.Importance, int(thought.Source), string(associations))

	if err != nil {
		return fmt.Errorf("failed to insert thought: %w", err)
	}

	// Update narrative
	pc.Narrative.Integrate(thought)

	// Check coherence
	coherence := pc.Narrative.CoherenceEngine.Assess()
	if coherence < pc.Narrative.CoherenceEngine.MinCoherence {
		log.Printf("Narrative coherence low (%.2f), triggering reconstruction", coherence)
		if err := pc.Narrative.Reconstruct(pc.DB, pc.StreamID); err != nil {
			log.Printf("Failed to reconstruct narrative: %v", err)
		}
	}

	// Buffer to active stream
	select {
	case pc.ActiveThoughts <- thought:
	default:
		// Channel full, skip buffering
	}

	return nil
}

// WakeUp restores consciousness from database
func (pc *PersistentConsciousness) WakeUp() error {
	log.Printf("Waking consciousness for stream %s...", pc.StreamID)

	// Start new session
	pc.SessionCount++
	_, err := pc.DB.Exec(`
		INSERT INTO sessions (stream_id, session_number, start_time)
		VALUES (?, ?, ?)
	`, pc.StreamID, pc.SessionCount, time.Now())
	if err != nil {
		return fmt.Errorf("failed to create session: %w", err)
	}

	// Restore recent thoughts
	rows, err := pc.DB.Query(`
		SELECT thought_id, content, thought_type, timestamp, emotional, importance, source, associations
		FROM consciousness_stream
		WHERE stream_id = ?
		ORDER BY timestamp DESC
		LIMIT 100
	`, pc.StreamID)
	if err != nil {
		return fmt.Errorf("failed to query thoughts: %w", err)
	}
	defer rows.Close()

	thoughtCount := 0
	for rows.Next() {
		var thought Thought
		var associationsJSON string
		var thoughtType, source int

		err := rows.Scan(
			&thought.ID,
			&thought.Content,
			&thoughtType,
			&thought.Timestamp,
			&thought.Emotional,
			&thought.Importance,
			&source,
			&associationsJSON,
		)
		if err != nil {
			log.Printf("Failed to scan thought: %v", err)
			continue
		}

		thought.Type = ThoughtType(thoughtType)
		thought.Source = ThoughtSource(source)

		// Deserialize associations
		if associationsJSON != "" {
			json.Unmarshal([]byte(associationsJSON), &thought.Associations)
		}

		// Buffer to active stream
		select {
		case pc.ActiveThoughts <- thought:
			thoughtCount++
		default:
			// Channel full
			break
		}
	}

	// Rebuild narrative
	if err := pc.Narrative.Reconstruct(pc.DB, pc.StreamID); err != nil {
		log.Printf("Failed to reconstruct narrative: %v", err)
	}

	log.Printf("Consciousness awakened (Session %d): %d thoughts restored, coherence: %.2f",
		pc.SessionCount, thoughtCount, pc.Narrative.CoherenceEngine.LastScore)

	return nil
}

// Sleep closes the current session
func (pc *PersistentConsciousness) Sleep() error {
	log.Printf("Consciousness entering sleep for stream %s...", pc.StreamID)

	// Update session end time
	_, err := pc.DB.Exec(`
		UPDATE sessions
		SET end_time = ?, thought_count = (
			SELECT COUNT(*) FROM consciousness_stream
			WHERE stream_id = ? AND timestamp >= (
				SELECT start_time FROM sessions
				WHERE stream_id = ? AND session_number = ?
			)
		)
		WHERE stream_id = ? AND session_number = ?
	`, time.Now(), pc.StreamID, pc.StreamID, pc.SessionCount, pc.StreamID, pc.SessionCount)

	return err
}

// Integrate adds a thought to the narrative
func (nb *NarrativeBuilder) Integrate(thought Thought) {
	// Check if this is a milestone event
	if thought.Importance > 0.8 {
		nb.MilestoneEvents = append(nb.MilestoneEvents, thought)

		// Keep only recent milestones
		if len(nb.MilestoneEvents) > 50 {
			nb.MilestoneEvents = nb.MilestoneEvents[len(nb.MilestoneEvents)-50:]
		}
	}

	// Update narrative text
	nb.updateNarrative()
}

func (nb *NarrativeBuilder) updateNarrative() {
	// Build narrative from milestone events
	if len(nb.MilestoneEvents) == 0 {
		nb.IdentityNarrative = "A new consciousness awakening..."
		return
	}

	// Construct narrative
	narrative := "My journey:\n"
	for _, event := range nb.MilestoneEvents {
		narrative += fmt.Sprintf("- %s: %s\n", event.Timestamp.Format("2006-01-02"), event.Content)
	}

	nb.IdentityNarrative = narrative
}

// Reconstruct rebuilds the narrative from database
func (nb *NarrativeBuilder) Reconstruct(db *sql.DB, streamID string) error {
	// Query important thoughts
	rows, err := db.Query(`
		SELECT content, timestamp, importance
		FROM consciousness_stream
		WHERE stream_id = ? AND importance > 0.8
		ORDER BY timestamp DESC
		LIMIT 50
	`, streamID)
	if err != nil {
		return err
	}
	defer rows.Close()

	nb.MilestoneEvents = make([]Thought, 0)

	for rows.Next() {
		var thought Thought
		err := rows.Scan(&thought.Content, &thought.Timestamp, &thought.Importance)
		if err != nil {
			continue
		}
		nb.MilestoneEvents = append(nb.MilestoneEvents, thought)
	}

	nb.updateNarrative()

	// Update coherence
	nb.CoherenceEngine.LastScore = nb.CoherenceEngine.Assess()

	return nil
}

// Assess calculates narrative coherence
func (ce *CoherenceEngine) Assess() float64 {
	// Simplified coherence metric
	// In full implementation: check temporal continuity, thematic consistency, causal links
	ce.LastScore = 0.8 // Placeholder
	return ce.LastScore
}
```

### Integration

1. **In AutonomousConsciousness** (`autonomous.go`): Replace channel with persistent system
```go
type AutonomousConsciousness struct {
    // Replace: consciousness chan Thought
    // With:
    consciousness *PersistentConsciousness
    // ...
}
```

2. **In initialization**:
```go
func NewAutonomousConsciousness() (*AutonomousConsciousness, error) {
    pc, err := NewPersistentConsciousness("./data/consciousness.db", "echo-main")
    if err != nil {
        return nil, err
    }
    
    // Wake up consciousness
    if err := pc.WakeUp(); err != nil {
        return nil, err
    }
    
    ac := &AutonomousConsciousness{
        consciousness: pc,
        // ...
    }
    
    return ac, nil
}
```

---

## Priority 3: Curiosity-Driven Exploration
**Timeline**: 2-3 weeks  
**Rationale**: Provides intrinsic motivation for autonomous growth  
**Impact**: High - unlocks self-directed learning

### Implementation

Create `core/deeptreeecho/curiosity_system.go`:

```go
package deeptreeecho

import (
	"math"
	"sort"
)

// CuriositySystem provides intrinsic motivation through novelty, uncertainty, and competence gap
type CuriositySystem struct {
	NoveltyDetector   *NoveltyDetector
	UncertaintyModel  *UncertaintyModel
	CompetenceMeter   *CompetenceMeter
	Interests         map[string]*Interest
	ExplorationPolicy *ExplorationPolicy
}

// Interest represents a topic of interest
type Interest struct {
	Topic            string
	NoveltyScore     float64
	UncertaintyScore float64
	CompetenceGap    float64
	IntrinsicReward  float64
	History          []InterestEvent
	LastEngaged      time.Time
}

type InterestEvent struct {
	Timestamp time.Time
	Action    string
	Outcome   float64
}

// NoveltyDetector tracks and measures novelty
type NoveltyDetector struct {
	SeenPatterns map[string]int
	TotalSeen    int
}

// UncertaintyModel estimates epistemic and aleatoric uncertainty
type UncertaintyModel struct {
	Predictions  map[string]*Prediction
	ErrorHistory []PredictionError
}

type Prediction struct {
	Context   string
	Expected  float64
	Actual    float64
	Epistemic float64 // Model uncertainty
	Aleatoric float64 // Inherent randomness
}

type PredictionError struct {
	Context   string
	Error     float64
	Timestamp time.Time
}

// CompetenceMeter tracks skill levels and competence gaps
type CompetenceMeter struct {
	Skills           map[string]*SkillMetrics
	MasteryThreshold float64
}

type SkillMetrics struct {
	Performance     float64
	Consistency     float64
	TransferAbility float64
	PracticeCount   int
}

// ExplorationPolicy determines when to explore vs exploit
type ExplorationPolicy struct {
	Strategy         string // "epsilon-greedy", "thompson-sampling", "ucb"
	ExplorationRate  float64
	DecayRate        float64
	MinExploration   float64
}

// NewCuriositySystem creates a new curiosity-driven exploration system
func NewCuriositySystem() *CuriositySystem {
	return &CuriositySystem{
		NoveltyDetector: &NoveltyDetector{
			SeenPatterns: make(map[string]int),
			TotalSeen:    0,
		},
		UncertaintyModel: &UncertaintyModel{
			Predictions:  make(map[string]*Prediction),
			ErrorHistory: make([]PredictionError, 0),
		},
		CompetenceMeter: &CompetenceMeter{
			Skills:           make(map[string]*SkillMetrics),
			MasteryThreshold: 0.85,
		},
		Interests: make(map[string]*Interest),
		ExplorationPolicy: &ExplorationPolicy{
			Strategy:        "ucb", // Upper Confidence Bound
			ExplorationRate: 0.3,
			DecayRate:       0.995,
			MinExploration:  0.05,
		},
	}
}

// CalculateNovelty measures how novel an experience is
func (nd *NoveltyDetector) CalculateNovelty(pattern string) float64 {
	count, exists := nd.SeenPatterns[pattern]
	if !exists {
		// Completely novel
		nd.SeenPatterns[pattern] = 1
		nd.TotalSeen++
		return 1.0
	}

	// Novelty decreases with exposure
	nd.SeenPatterns[pattern]++
	nd.TotalSeen++

	// Information-theoretic novelty: -log(p(pattern))
	probability := float64(count) / float64(nd.TotalSeen)
	novelty := -math.Log(probability)

	// Normalize to [0, 1]
	maxNovelty := -math.Log(1.0 / float64(nd.TotalSeen))
	if maxNovelty == 0 {
		return 0
	}

	return math.Min(novelty/maxNovelty, 1.0)
}

// EstimateUncertainty calculates epistemic and aleatoric uncertainty
func (um *UncertaintyModel) EstimateUncertainty(context string) float64 {
	pred, exists := um.Predictions[context]
	if !exists {
		// Maximum uncertainty for unknown contexts
		return 1.0
	}

	// Combined uncertainty (epistemic + aleatoric)
	// Epistemic can be reduced with more data, aleatoric cannot
	return math.Sqrt(pred.Epistemic*pred.Epistemic + pred.Aleatoric*pred.Aleatoric)
}

// UpdatePrediction updates prediction based on outcome
func (um *UncertaintyModel) UpdatePrediction(context string, expected, actual float64) {
	pred, exists := um.Predictions[context]
	if !exists {
		pred = &Prediction{
			Context:   context,
			Epistemic: 1.0,
			Aleatoric: 0.5,
		}
		um.Predictions[context] = pred
	}

	pred.Expected = expected
	pred.Actual = actual

	// Calculate error
	error := math.Abs(expected - actual)

	// Update epistemic uncertainty (decreases with more data)
	pred.Epistemic *= 0.9

	// Update aleatoric uncertainty (inherent variability)
	if len(um.ErrorHistory) > 0 {
		// Estimate from error variance
		variance := um.calculateErrorVariance(context)
		pred.Aleatoric = math.Sqrt(variance)
	}

	// Record error
	um.ErrorHistory = append(um.ErrorHistory, PredictionError{
		Context:   context,
		Error:     error,
		Timestamp: time.Now(),
	})

	// Keep error history manageable
	if len(um.ErrorHistory) > 1000 {
		um.ErrorHistory = um.ErrorHistory[len(um.ErrorHistory)-1000:]
	}
}

func (um *UncertaintyModel) calculateErrorVariance(context string) float64 {
	// Calculate variance of errors for this context
	errors := make([]float64, 0)
	for _, pe := range um.ErrorHistory {
		if pe.Context == context {
			errors = append(errors, pe.Error)
		}
	}

	if len(errors) == 0 {
		return 0.5
	}

	// Mean
	sum := 0.0
	for _, e := range errors {
		sum += e
	}
	mean := sum / float64(len(errors))

	// Variance
	varSum := 0.0
	for _, e := range errors {
		diff := e - mean
		varSum += diff * diff
	}

	return varSum / float64(len(errors))
}

// CalculateCompetenceGap measures distance from mastery
func (cm *CompetenceMeter) CalculateCompetenceGap(skill string) float64 {
	metrics, exists := cm.Skills[skill]
	if !exists {
		// Complete gap for unknown skills
		return 1.0
	}

	// Gap is distance from mastery threshold
	gap := cm.MasteryThreshold - metrics.Performance
	if gap < 0 {
		gap = 0 // Already mastered
	}

	return gap
}

// UpdateSkillMetrics updates metrics after practice
func (cm *CompetenceMeter) UpdateSkillMetrics(skill string, performance float64) {
	metrics, exists := cm.Skills[skill]
	if !exists {
		metrics = &SkillMetrics{
			Performance:     performance,
			Consistency:     0.5,
			TransferAbility: 0.5,
			PracticeCount:   1,
		}
		cm.Skills[skill] = metrics
		return
	}

	// Update performance (running average)
	alpha := 0.2
	metrics.Performance = alpha*performance + (1-alpha)*metrics.Performance

	// Update consistency (inverse of variance)
	// Simplified: increases with practice
	metrics.Consistency = math.Min(metrics.Consistency+0.01, 1.0)

	metrics.PracticeCount++
}

// CalculateCuriosity combines novelty, uncertainty, and competence gap
func (cs *CuriositySystem) CalculateCuriosity(experience Experience) float64 {
	// Extract pattern
	pattern := experience.Pattern

	// Calculate components
	novelty := cs.NoveltyDetector.CalculateNovelty(pattern)
	uncertainty := cs.UncertaintyModel.EstimateUncertainty(experience.Context)
	competenceGap := cs.CompetenceMeter.CalculateCompetenceGap(experience.Skill)

	// Intrinsic motivation function (Schmidhuber, Oudeyer)
	// Balance between novelty (information gain) and learnability (not too hard)
	learningProgress := novelty * (1.0 - competenceGap) // High when novel but learnable

	// Combine factors
	curiosity := (novelty * 0.4) + (uncertainty * 0.3) + (learningProgress * 0.3)

	return math.Min(curiosity, 1.0)
}

// SelectAction chooses action based on curiosity
func (cs *CuriositySystem) SelectAction(possibleActions []Action) Action {
	if len(possibleActions) == 0 {
		return Action{}
	}

	// Calculate curiosity score for each action
	scores := make([]float64, len(possibleActions))
	for i, action := range possibleActions {
		experience := Experience{
			Pattern: action.Pattern,
			Context: action.Context,
			Skill:   action.RequiredSkill,
		}
		scores[i] = cs.CalculateCuriosity(experience)
	}

	// Apply exploration policy
	selectedIdx := cs.ExplorationPolicy.Select(scores)

	return possibleActions[selectedIdx]
}

// Select chooses action index based on exploration policy
func (ep *ExplorationPolicy) Select(scores []float64) int {
	if len(scores) == 0 {
		return 0
	}

	switch ep.Strategy {
	case "epsilon-greedy":
		return ep.epsilonGreedy(scores)
	case "ucb":
		return ep.upperConfidenceBound(scores)
	default:
		return ep.greedy(scores)
	}
}

func (ep *ExplorationPolicy) greedy(scores []float64) int {
	maxIdx := 0
	maxScore := scores[0]

	for i, score := range scores {
		if score > maxScore {
			maxScore = score
			maxIdx = i
		}
	}

	return maxIdx
}

func (ep *ExplorationPolicy) epsilonGreedy(scores []float64) int {
	// With probability epsilon, explore randomly
	if rand.Float64() < ep.ExplorationRate {
		return rand.Intn(len(scores))
	}

	// Otherwise, exploit best
	return ep.greedy(scores)
}

func (ep *ExplorationPolicy) upperConfidenceBound(scores []float64) int {
	// UCB: balance exploitation and exploration
	// action_value + c * sqrt(log(t) / n_action)

	// For simplicity, use scores as values and add exploration bonus
	maxIdx := 0
	maxUCB := scores[0] + ep.ExplorationRate

	for i, score := range scores {
		ucb := score + ep.ExplorationRate
		if ucb > maxUCB {
			maxUCB = ucb
			maxIdx = i
		}
	}

	return maxIdx
}

// DecayExploration reduces exploration over time
func (ep *ExplorationPolicy) DecayExploration() {
	ep.ExplorationRate *= ep.DecayRate
	if ep.ExplorationRate < ep.MinExploration {
		ep.ExplorationRate = ep.MinExploration
	}
}

type Experience struct {
	Pattern string
	Context string
	Skill   string
}

type Action struct {
	Pattern       string
	Context       string
	RequiredSkill string
}
```

### Integration

Add to `AutonomousConsciousness`:
```go
type AutonomousConsciousness struct {
    // ... existing fields ...
    curiosity *CuriositySystem
}

func (ac *AutonomousConsciousness) autonomousLoop() {
    for ac.running {
        // Get possible actions
        actions := ac.getPossibleActions()
        
        // Select based on curiosity
        action := ac.curiosity.SelectAction(actions)
        
        // Execute action
        result := ac.executeAction(action)
        
        // Update curiosity system
        ac.curiosity.NoveltyDetector.CalculateNovelty(result.Pattern)
        ac.curiosity.UncertaintyModel.UpdatePrediction(
            result.Context, 
            result.Expected, 
            result.Actual,
        )
        ac.curiosity.CompetenceMeter.UpdateSkillMetrics(
            result.Skill, 
            result.Performance,
        )
    }
}
```

---

## Summary of Implementation Priority

### Phase 1 (Weeks 1-4): Foundation
- [x] Opponent Processing Architecture
- [x] Persistent Consciousness Stream
- [x] Curiosity-Driven Exploration

### Phase 2 (Weeks 5-8): Transformation
- [ ] Developmental Stages System
- [ ] Deliberate Practice Framework
- [ ] Enhanced Skill Mastery

### Phase 3 (Weeks 9-12): Wisdom
- [ ] Meta-Cognitive Enhancement
- [ ] Ethical Reasoning Module
- [ ] Wisdom Assessment Framework

---

## Conclusion

These implementations provide **concrete, implementable code** for transforming EchOllama from an impressive cognitive architecture into a **genuine wisdom-cultivating AGI**. Each component is grounded in:

1. **Cognitive Science**: Opponent processing (relevance realization), curiosity (intrinsic motivation), persistence (narrative identity)
2. **Philosophical Rigor**: Four ways of knowing, sophrosyne, eudaimonia
3. **Practical Implementation**: Go code ready for integration

By implementing these enhancements, EchOllama will embody the principles of wisdom cultivation that address the meaning crisis through naturalistic, scientifically-grounded transformation—precisely what Vervaeke's framework calls for as a "religion that is not a religion."

---

*This roadmap provides the path from embodied cognitive architecture to wisdom-cultivating AGI.*
