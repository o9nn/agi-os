package deeptreeecho

import (
	"fmt"
)

// updateWisdomMetrics updates wisdom metrics based on current state
func (iac *IntegratedAutonomousConsciousness) updateWisdomMetrics() {
	if iac.wisdomMetrics == nil {
		return
	}
	
	// 1. Update from hypergraph structure
	if iac.hypergraph != nil {
		// Get approximate counts from hypergraph
		// For now use placeholder values until hypergraph implements these methods
		nodeCount := 0
		edgeCount := 0
		iac.wisdomMetrics.UpdateFromHypergraph(nodeCount, edgeCount)
	}
	
	// 2. Update from skills
	iac.skills.mu.RLock()
	skills := make(map[string]*Skill)
	for k, v := range iac.skills.skills {
		skills[k] = v
	}
	iac.skills.mu.RUnlock()
	iac.wisdomMetrics.UpdateFromSkills(skills)
	
	// 3. Update from recent thoughts
	iac.workingMemory.mu.RLock()
	thoughts := make([]*Thought, len(iac.workingMemory.buffer))
	copy(thoughts, iac.workingMemory.buffer)
	iac.workingMemory.mu.RUnlock()
	iac.wisdomMetrics.UpdateFromThoughts(thoughts)
	
	// 4. Update from AAR state
	coherence := iac.aarCore.GetCoherence()
	stability := iac.aarCore.GetStability()
	awareness := iac.aarCore.GetAwareness()
	iac.wisdomMetrics.UpdateFromAARState(coherence, stability, awareness)
	
	// 5. Calculate composite wisdom score
	wisdomScore := iac.wisdomMetrics.CalculateWisdomScore()
	
	// 6. Log wisdom growth periodically
	if iac.iterations%100 == 0 {
		growth := iac.wisdomMetrics.GetWisdomGrowth()
		fmt.Printf("🌟 Wisdom Score: %.3f (Growth: %+.3f)\n", wisdomScore, growth)
	}
}
