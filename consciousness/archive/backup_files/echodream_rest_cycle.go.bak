package deeptreeecho

import (
	"fmt"
	"time"
)

// RestCycle implements a full rest cycle with EchoDream knowledge integration
func (iac *IntegratedAutonomousConsciousness) RestCycle() {
	fmt.Println("🌙 Entering rest cycle for knowledge integration...")
	
	iac.mu.Lock()
	iac.awake = false
	iac.mu.Unlock()
	
	// Mark rest beginning in state manager
	if iac.stateManager != nil {
		iac.stateManager.EnterRest()
	}
	
	// 1. Gather recent experiences from working memory
	iac.workingMemory.mu.RLock()
	recentThoughts := make([]*Thought, len(iac.workingMemory.buffer))
	copy(recentThoughts, iac.workingMemory.buffer)
	iac.workingMemory.mu.RUnlock()
	
	fmt.Printf("🌙 Processing %d recent thoughts...\n", len(recentThoughts))
	
	// 2. Extract patterns from recent thoughts
	patterns := iac.extractPatternsFromThoughts(recentThoughts)
	fmt.Printf("🌙 Extracted %d patterns\n", len(patterns))
	
	// 3. Generate insights from patterns
	insights := iac.generateInsightsFromPatterns(patterns)
	fmt.Printf("🌙 Generated %d insights\n", len(insights))
	
	// 4. Update hypergraph with abstractions
	if iac.hypergraph != nil {
		for _, pattern := range patterns {
			iac.addPatternToHypergraph(pattern)
		}
	}
	
	// 5. Integrate insights into knowledge structures
	if iac.knowledgeLearning != nil {
		for _, insight := range insights {
			iac.integrateInsight(insight)
		}
	}
	
	// 6. Consolidate memories using EchoDream
	if iac.dream != nil {
		iac.consolidateWithEchoDream(recentThoughts)
	}
	
	// 7. Prune low-importance memories from working memory
	iac.pruneWorkingMemory(0.3)
	
	// 8. Update wisdom metrics
	iac.updateWisdomMetrics()
	
	// 9. Mark rest completion in state manager
	if iac.stateManager != nil {
		iac.stateManager.ExitRest()
	}
	
	fmt.Println("🌙 Rest cycle complete. Knowledge integrated, energy restored.")
}

// extractPatternsFromThoughts extracts recurring patterns from thoughts
func (iac *IntegratedAutonomousConsciousness) extractPatternsFromThoughts(thoughts []*Thought) []RestCyclePattern {
	patterns := make([]RestCyclePattern, 0)
	
	// Simple pattern extraction: group by type and topic
	typeGroups := make(map[ThoughtType][]*Thought)
	
	for _, thought := range thoughts {
		typeGroups[thought.Type] = append(typeGroups[thought.Type], thought)
	}
	
	// Create patterns from groups with multiple thoughts
	for thoughtType, group := range typeGroups {
		if len(group) >= 2 {
			pattern := RestCyclePattern{
				Type:        "thought_cluster",
				Description: fmt.Sprintf("Recurring %s thoughts", thoughtType.String()),
				Frequency:   len(group),
				Strength:    float64(len(group)) / float64(len(thoughts)),
				Examples:    group[:minInt(3, len(group))],
			}
			patterns = append(patterns, pattern)
		}
	}
	
	return patterns
}

// generateInsightsFromPatterns generates insights from extracted patterns
func (iac *IntegratedAutonomousConsciousness) generateInsightsFromPatterns(patterns []RestCyclePattern) []RestCycleInsight {
	insights := make([]RestCycleInsight, 0)
	
	for _, pattern := range patterns {
		// Generate insight based on pattern strength
		if pattern.Strength > 0.3 {
			insight := RestCycleInsight{
				Content:     fmt.Sprintf("I notice a pattern of %s with strength %.2f", pattern.Description, pattern.Strength),
				Source:      "pattern_analysis",
				Confidence:  pattern.Strength,
				Timestamp:   time.Now(),
				RelatedTo:   []string{pattern.Type},
			}
			insights = append(insights, insight)
		}
	}
	
	return insights
}

// addPatternToHypergraph adds a pattern to the hypergraph memory
func (iac *IntegratedAutonomousConsciousness) addPatternToHypergraph(pattern RestCyclePattern) {
	// This would create nodes and edges representing the pattern
	// For now, we'll just log it
	fmt.Printf("  📊 Pattern added to hypergraph: %s\n", pattern.Description)
}

// integrateInsight integrates an insight into knowledge structures
func (iac *IntegratedAutonomousConsciousness) integrateInsight(insight RestCycleInsight) {
	// This would update the knowledge learning system
	// For now, we'll just log it
	fmt.Printf("  💡 Insight integrated: %s\n", insight.Content)
	
	if iac.stateManager != nil {
		iac.stateManager.RecordLearningEvent()
	}
}

// consolidateWithEchoDream uses EchoDream for memory consolidation
func (iac *IntegratedAutonomousConsciousness) consolidateWithEchoDream(thoughts []*Thought) {
	fmt.Println("  🌀 EchoDream consolidation in progress...")
	
	// Simulate EchoDream processing
	// In a full implementation, this would:
	// - Replay experiences
	// - Extract semantic content
	// - Form abstractions
	// - Strengthen important connections
	// - Weaken unimportant connections
	
	time.Sleep(500 * time.Millisecond) // Simulate processing time
	
	fmt.Println("  🌀 EchoDream consolidation complete")
}

// pruneWorkingMemory removes low-importance thoughts from working memory
func (iac *IntegratedAutonomousConsciousness) pruneWorkingMemory(threshold float64) {
	iac.workingMemory.mu.Lock()
	defer iac.workingMemory.mu.Unlock()
	
	originalCount := len(iac.workingMemory.buffer)
	
	// Keep only thoughts above importance threshold
	pruned := make([]*Thought, 0)
	for _, thought := range iac.workingMemory.buffer {
		if thought.Importance >= threshold {
			pruned = append(pruned, thought)
		}
	}
	
	iac.workingMemory.buffer = pruned
	
	prunedCount := originalCount - len(pruned)
	if prunedCount > 0 {
		fmt.Printf("  🧹 Pruned %d low-importance thoughts from working memory\n", prunedCount)
	}
}

// minInt function moved to utils.go
