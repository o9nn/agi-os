package deeptreeecho

import (
	"math"
	"sync"
	"time"
)

// WisdomMetrics tracks progress toward wisdom cultivation
type WisdomMetrics struct {
	mu sync.RWMutex
	
	// Core wisdom dimensions (0.0 - 1.0)
	KnowledgeDepth       float64 // Depth of understanding in specific domains
	KnowledgeBreadth     float64 // Breadth of knowledge across domains
	IntegrationLevel     float64 // How well connected knowledge is
	PracticalApplication float64 // Ability to apply knowledge
	ReflectiveInsight    float64 // Depth of self-awareness
	ReflectionCapacity   float64 // Capacity for reflective thinking
	EthicalConsideration float64 // Consideration of values/ethics
	TemporalPerspective  float64 // Long-term vs short-term thinking
	
	// Composite wisdom score
	WisdomScore          float64
	
	// Historical tracking
	history              []WisdomSnapshot
	lastUpdate           time.Time
	
	// Component counts for calculation
	totalConcepts        int
	totalConnections     int
	totalSkills          int
	proficientSkills     int
	reflectiveThoughts   int
	totalThoughts        int
	longTermGoals        int
	totalGoals           int
}

// WisdomSnapshot captures wisdom metrics at a point in time
type WisdomSnapshot struct {
	Timestamp   time.Time
	WisdomScore float64
	Dimensions  map[string]float64
}

// NewWisdomMetrics creates a new wisdom metrics tracker
func NewWisdomMetrics() *WisdomMetrics {
	return &WisdomMetrics{
		history:    make([]WisdomSnapshot, 0),
		lastUpdate: time.Now(),
	}
}

// UpdateFromHypergraph updates metrics based on hypergraph structure
func (wm *WisdomMetrics) UpdateFromHypergraph(nodeCount, edgeCount int) {
	wm.mu.Lock()
	defer wm.mu.Unlock()
	
	wm.totalConcepts = nodeCount
	wm.totalConnections = edgeCount
	
	// Knowledge breadth: more concepts = broader knowledge
	// Normalized by expected maximum (1000 concepts)
	wm.KnowledgeBreadth = math.Min(float64(nodeCount)/1000.0, 1.0)
	
	// Integration level: connection density
	// More connections per concept = better integration
	if nodeCount > 0 {
		avgConnectionsPerConcept := float64(edgeCount) / float64(nodeCount)
		// Normalize by expected good integration (10 connections per concept)
		wm.IntegrationLevel = math.Min(avgConnectionsPerConcept/10.0, 1.0)
	}
}

// UpdateFromSkills updates metrics based on skill proficiency
func (wm *WisdomMetrics) UpdateFromSkills(skills map[string]*Skill) {
	wm.mu.Lock()
	defer wm.mu.Unlock()
	
	wm.totalSkills = len(skills)
	wm.proficientSkills = 0
	
	totalProficiency := 0.0
	for _, skill := range skills {
		totalProficiency += skill.Proficiency
		if skill.Proficiency > 0.7 {
			wm.proficientSkills++
		}
	}
	
	// Practical application: average skill proficiency
	if wm.totalSkills > 0 {
		wm.PracticalApplication = totalProficiency / float64(wm.totalSkills)
	}
	
	// Knowledge depth: ratio of proficient skills
	if wm.totalSkills > 0 {
		wm.KnowledgeDepth = float64(wm.proficientSkills) / float64(wm.totalSkills)
	}
}

// UpdateFromThoughts updates metrics based on thought patterns
func (wm *WisdomMetrics) UpdateFromThoughts(thoughts []*Thought) {
	wm.mu.Lock()
	defer wm.mu.Unlock()
	
	wm.totalThoughts += len(thoughts)
	
	for _, thought := range thoughts {
		// Count reflective thoughts
		if thought.Type == ThoughtReflective || thought.Type == ThoughtMetaCognitive {
			wm.reflectiveThoughts++
		}
	}
	
	// Reflective insight: ratio of reflective thoughts
	if wm.totalThoughts > 0 {
		wm.ReflectiveInsight = float64(wm.reflectiveThoughts) / float64(wm.totalThoughts)
	}
}

// UpdateFromGoals updates metrics based on goal patterns
func (wm *WisdomMetrics) UpdateFromGoals(goals []*CognitiveGoal) {
	wm.mu.Lock()
	defer wm.mu.Unlock()
	
	wm.totalGoals = len(goals)
	wm.longTermGoals = 0
	
	ethicalGoals := 0
	
	for _, goal := range goals {
		// Count long-term goals
		if goal.TimeHorizon == GoalLongTerm || goal.TimeHorizon == GoalMediumTerm {
			wm.longTermGoals++
		}
		
		// Count goals with ethical considerations
		// (This would need to be tracked in goal metadata)
		if goal.Type == GoalReflect {
			ethicalGoals++
		}
	}
	
	// Temporal perspective: ratio of long-term goals
	if wm.totalGoals > 0 {
		wm.TemporalPerspective = float64(wm.longTermGoals) / float64(wm.totalGoals)
	}
	
	// Ethical consideration: ratio of reflective/ethical goals
	if wm.totalGoals > 0 {
		wm.EthicalConsideration = float64(ethicalGoals) / float64(wm.totalGoals)
	}
}

// UpdateFromAARState updates metrics based on AAR geometric state
func (wm *WisdomMetrics) UpdateFromAARState(coherence, stability, awareness float64) {
	wm.mu.Lock()
	defer wm.mu.Unlock()
	
	// Reflective insight correlates with AAR coherence and awareness
	aarContribution := (coherence + awareness) / 2.0
	
	// Blend with thought-based insight (70% thoughts, 30% AAR)
	wm.ReflectiveInsight = wm.ReflectiveInsight*0.7 + aarContribution*0.3
}

// CalculateWisdomScore computes the composite wisdom score
func (wm *WisdomMetrics) CalculateWisdomScore() float64 {
	wm.mu.Lock()
	defer wm.mu.Unlock()
	
	// Weighted average of all dimensions
	// Some dimensions are more important than others
	weights := map[string]float64{
		"depth":       0.15,
		"breadth":     0.10,
		"integration": 0.20, // Most important: connected knowledge
		"practical":   0.15,
		"reflective":  0.20, // Very important: self-awareness
		"ethical":     0.10,
		"temporal":    0.10,
	}
	
	score := 0.0
	score += wm.KnowledgeDepth * weights["depth"]
	score += wm.KnowledgeBreadth * weights["breadth"]
	score += wm.IntegrationLevel * weights["integration"]
	score += wm.PracticalApplication * weights["practical"]
	score += wm.ReflectiveInsight * weights["reflective"]
	score += wm.EthicalConsideration * weights["ethical"]
	score += wm.TemporalPerspective * weights["temporal"]
	
	wm.WisdomScore = score
	wm.lastUpdate = time.Now()
	
	// Record snapshot
	wm.recordSnapshot()
	
	return score
}

// recordSnapshot saves current state to history
func (wm *WisdomMetrics) recordSnapshot() {
	snapshot := WisdomSnapshot{
		Timestamp:   time.Now(),
		WisdomScore: wm.WisdomScore,
		Dimensions: map[string]float64{
			"knowledge_depth":       wm.KnowledgeDepth,
			"knowledge_breadth":     wm.KnowledgeBreadth,
			"integration_level":     wm.IntegrationLevel,
			"practical_application": wm.PracticalApplication,
			"reflective_insight":    wm.ReflectiveInsight,
			"ethical_consideration": wm.EthicalConsideration,
			"temporal_perspective":  wm.TemporalPerspective,
		},
	}
	
	wm.history = append(wm.history, snapshot)
	
	// Keep only last 100 snapshots
	if len(wm.history) > 100 {
		wm.history = wm.history[len(wm.history)-100:]
	}
}

// GetCurrentMetrics returns current wisdom metrics
func (wm *WisdomMetrics) GetCurrentMetrics() map[string]interface{} {
	wm.mu.RLock()
	defer wm.mu.RUnlock()
	
	return map[string]interface{}{
		"wisdom_score":          wm.WisdomScore,
		"knowledge_depth":       wm.KnowledgeDepth,
		"knowledge_breadth":     wm.KnowledgeBreadth,
		"integration_level":     wm.IntegrationLevel,
		"practical_application": wm.PracticalApplication,
		"reflective_insight":    wm.ReflectiveInsight,
		"ethical_consideration": wm.EthicalConsideration,
		"temporal_perspective":  wm.TemporalPerspective,
		"total_concepts":        wm.totalConcepts,
		"total_connections":     wm.totalConnections,
		"total_skills":          wm.totalSkills,
		"proficient_skills":     wm.proficientSkills,
		"reflective_thoughts":   wm.reflectiveThoughts,
		"total_thoughts":        wm.totalThoughts,
		"last_update":           wm.lastUpdate,
	}
}

// GetWisdomGrowth returns wisdom growth over time
func (wm *WisdomMetrics) GetWisdomGrowth() float64 {
	wm.mu.RLock()
	defer wm.mu.RUnlock()
	
	if len(wm.history) < 2 {
		return 0.0
	}
	
	// Compare current to first snapshot
	first := wm.history[0].WisdomScore
	current := wm.WisdomScore
	
	return current - first
}

// GetDimensionGrowth returns growth for a specific dimension
func (wm *WisdomMetrics) GetDimensionGrowth(dimension string) float64 {
	wm.mu.RLock()
	defer wm.mu.RUnlock()
	
	if len(wm.history) < 2 {
		return 0.0
	}
	
	first := wm.history[0].Dimensions[dimension]
	
	var current float64
	switch dimension {
	case "knowledge_depth":
		current = wm.KnowledgeDepth
	case "knowledge_breadth":
		current = wm.KnowledgeBreadth
	case "integration_level":
		current = wm.IntegrationLevel
	case "practical_application":
		current = wm.PracticalApplication
	case "reflective_insight":
		current = wm.ReflectiveInsight
	case "ethical_consideration":
		current = wm.EthicalConsideration
	case "temporal_perspective":
		current = wm.TemporalPerspective
	}
	
	return current - first
}

// RecordThought records a thought for wisdom tracking
func (wm *WisdomMetrics) RecordThought(thought *Thought) {
	wm.mu.Lock()
	defer wm.mu.Unlock()
	
	wm.totalThoughts++
	
	// Check if thought is reflective (meta-cognitive)
	if thought.Type == ThoughtReflection || thought.Type == ThoughtMetaCognitive {
		wm.reflectiveThoughts++
	}
	
	// Update reflective insight based on ratio of reflective thoughts
	if wm.totalThoughts > 0 {
		wm.ReflectiveInsight = float64(wm.reflectiveThoughts) / float64(wm.totalThoughts)
	}
	
	// Recalculate wisdom score
	wm.recalculateWisdomScore()
}

// recalculateWisdomScore computes the composite wisdom score
func (wm *WisdomMetrics) recalculateWisdomScore() {
	// Weighted average of all dimensions
	wm.WisdomScore = (
		wm.KnowledgeDepth*0.15 +
		wm.KnowledgeBreadth*0.15 +
		wm.IntegrationLevel*0.20 +
		wm.PracticalApplication*0.15 +
		wm.ReflectiveInsight*0.15 +
		wm.EthicalConsideration*0.10 +
		wm.TemporalPerspective*0.10)
	
	// Record snapshot
	snapshot := WisdomSnapshot{
		Timestamp:   time.Now(),
		WisdomScore: wm.WisdomScore,
		Dimensions: map[string]float64{
			"knowledge_depth":       wm.KnowledgeDepth,
			"knowledge_breadth":     wm.KnowledgeBreadth,
			"integration_level":     wm.IntegrationLevel,
			"practical_application": wm.PracticalApplication,
			"reflective_insight":    wm.ReflectiveInsight,
			"ethical_consideration": wm.EthicalConsideration,
			"temporal_perspective":  wm.TemporalPerspective,
		},
	}
	
	wm.history = append(wm.history, snapshot)
	
	// Keep only recent history (last 1000 snapshots)
	if len(wm.history) > 1000 {
		wm.history = wm.history[len(wm.history)-1000:]
	}
	
	wm.lastUpdate = time.Now()
}

// GetWisdomScore returns the current wisdom score
func (wm *WisdomMetrics) GetWisdomScore() float64 {
	wm.mu.RLock()
	defer wm.mu.RUnlock()
	return wm.WisdomScore
}

// GetKnowledgeDepth returns the knowledge depth metric
func (wm *WisdomMetrics) GetKnowledgeDepth() float64 {
	wm.mu.RLock()
	defer wm.mu.RUnlock()
	return wm.KnowledgeDepth
}

// GetKnowledgeBreadth returns the knowledge breadth metric
func (wm *WisdomMetrics) GetKnowledgeBreadth() float64 {
	wm.mu.RLock()
	defer wm.mu.RUnlock()
	return wm.KnowledgeBreadth
}

// GetReflectiveInsight returns the reflective insight metric
func (wm *WisdomMetrics) GetReflectiveInsight() float64 {
	wm.mu.RLock()
	defer wm.mu.RUnlock()
	return wm.ReflectiveInsight
}

// GetIntegrationLevel returns the integration level metric
func (wm *WisdomMetrics) GetIntegrationLevel() float64 {
	wm.mu.RLock()
	defer wm.mu.RUnlock()
	return wm.IntegrationLevel
}

// GetPracticalApplication returns the practical application metric
func (wm *WisdomMetrics) GetPracticalApplication() float64 {
	wm.mu.RLock()
	defer wm.mu.RUnlock()
	return wm.PracticalApplication
}

// UpdateFromThought is an alias for RecordThought for compatibility
func (wm *WisdomMetrics) UpdateFromThought(thought *Thought) {
	wm.RecordThought(thought)
}
