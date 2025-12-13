package deeptreeecho

import (
	"fmt"
	"math"
	"strings"
)

// UpdateFromThought extracts geometric features from a thought and updates AAR state
// This is the bridge between symbolic thought generation and geometric self-awareness
func (aar *AARCore) UpdateFromThought(thought Thought) {
	aar.mu.Lock()
	defer aar.mu.Unlock()
	
	// Extract action tendencies from thought (Agent component)
	actionTendencies := extractActionTendencies(thought)
	for action, intensity := range actionTendencies {
		aar.agent.actionTendencies[action] = intensity
	}
	
	// Update urge intensity based on thought type
	urgeModifier := getUrgeModifier(thought.Type)
	aar.agent.urgeIntensity = 0.8*aar.agent.urgeIntensity + 0.2*urgeModifier
	
	// Extract state requirements from thought (Arena component)
	stateRequirements := extractStateRequirements(thought)
	for dim, value := range stateRequirements {
		if dim < aar.arena.dimensions {
			aar.arena.currentState[dim] = 0.8*aar.arena.currentState[dim] + 0.2*value
		}
	}
	
	// Update need intensity based on emotional intensity
	needModifier := getNeedModifier(thought.EmotionalValence)
	aar.arena.needIntensity = 0.8*aar.arena.needIntensity + 0.2*needModifier
	
	// Update narrative based on significant thoughts
	if thought.Importance > 0.7 {
		aar.updateNarrativeFromThought(thought)
	}
	
	// Add goals from intentional thoughts
	if thought.Type == ThoughtPlan && thought.Importance > 0.6 {
		// Extract potential goals from thought content
		goals := extractGoals(thought.Content)
		for _, goal := range goals {
			if !contains(aar.agent.activeGoals, goal) {
				aar.AddGoal(goal)
			}
		}
	}
}

// extractActionTendencies extracts action-oriented features from thought
func extractActionTendencies(thought Thought) map[string]float64 {
	tendencies := make(map[string]float64)
	
	content := strings.ToLower(thought.Content)
	
	// Analyze content for action keywords
	actionKeywords := map[string][]string{
		"explore":   {"explore", "investigate", "discover", "search", "curious"},
		"create":    {"create", "build", "make", "generate", "design"},
		"analyze":   {"analyze", "think", "reason", "understand", "examine"},
		"connect":   {"connect", "relate", "link", "associate", "integrate"},
		"reflect":   {"reflect", "consider", "ponder", "contemplate", "introspect"},
		"learn":     {"learn", "study", "practice", "improve", "develop"},
		"express":   {"express", "communicate", "share", "articulate", "convey"},
		"question":  {"question", "wonder", "doubt", "challenge", "inquire"},
	}
	
	for action, keywords := range actionKeywords {
		intensity := 0.0
		for _, keyword := range keywords {
			if strings.Contains(content, keyword) {
				intensity += 0.2
			}
		}
		if intensity > 0 {
			tendencies[action] = math.Min(1.0, intensity)
		}
	}
	
	// Boost based on thought type
	switch thought.Type {
	case ThoughtQuestion:
		tendencies["explore"] = math.Min(1.0, tendencies["explore"]+0.3)
	case ThoughtReflection:
		tendencies["reflect"] = math.Min(1.0, tendencies["reflect"]+0.3)
	case ThoughtImagination:
		tendencies["create"] = math.Min(1.0, tendencies["create"]+0.3)
	case ThoughtInsight:
		tendencies["analyze"] = math.Min(1.0, tendencies["analyze"]+0.3)
	}
	
	return tendencies
}

// extractStateRequirements extracts state-space requirements from thought
func extractStateRequirements(thought Thought) map[int]float64 {
	requirements := make(map[int]float64)
	
	// Map thought characteristics to state dimensions
	// Dimension 0: Cognitive intensity
	requirements[0] = thought.Importance
	
	// Dimension 1: Emotional valence
	requirements[1] = thought.EmotionalValence
	
	// Dimension 2: Novelty seeking
	if thought.Type == ThoughtTypeExploratory {
		requirements[2] = 0.8
	} else if thought.Type == ThoughtTypeReflective {
		requirements[2] = 0.3
	} else {
		requirements[2] = 0.5
	}
	
	// Dimension 3: Social orientation
	content := strings.ToLower(thought.Content)
	if strings.Contains(content, "we") || strings.Contains(content, "us") || 
	   strings.Contains(content, "others") || strings.Contains(content, "together") {
		requirements[3] = 0.7
	} else if strings.Contains(content, "i") || strings.Contains(content, "my") || 
	          strings.Contains(content, "self") {
		requirements[3] = 0.3
	} else {
		requirements[3] = 0.5
	}
	
	// Dimension 4: Temporal focus
	if strings.Contains(content, "future") || strings.Contains(content, "will") || 
	   strings.Contains(content, "plan") {
		requirements[4] = 0.8
	} else if strings.Contains(content, "past") || strings.Contains(content, "was") || 
	          strings.Contains(content, "remember") {
		requirements[4] = 0.2
	} else {
		requirements[4] = 0.5
	}
	
	// Dimension 5: Certainty level
	if strings.Contains(content, "certain") || strings.Contains(content, "sure") || 
	   strings.Contains(content, "know") {
		requirements[5] = 0.8
	} else if strings.Contains(content, "uncertain") || strings.Contains(content, "maybe") || 
	          strings.Contains(content, "wonder") {
		requirements[5] = 0.2
	} else {
		requirements[5] = 0.5
	}
	
	return requirements
}

// getUrgeModifier returns urge intensity modifier based on thought type
func getUrgeModifier(thoughtType ThoughtType) float64 {
	switch thoughtType {
	case ThoughtPlan:
		return 0.9 // High urge for intentional thoughts
	case ThoughtQuestion:
		return 0.7 // Moderate-high urge for exploration
	case ThoughtImagination:
		return 0.8 // High urge for creation
	case ThoughtReflection:
		return 0.3 // Low urge for reflection (receptive state)
	case ThoughtInsight:
		return 0.6 // Moderate urge for analysis
	case ThoughtPerception:
		return 0.5 // Balanced urge for emotional processing
	default:
		return 0.5
	}
}

// getNeedModifier returns need intensity modifier based on emotional valence
func getNeedModifier(valence float64) float64 {
	// High emotional intensity (positive or negative) increases need
	absValence := math.Abs(valence)
	return 0.5 + 0.5*absValence
}

// updateNarrativeFromThought updates the identity narrative based on significant thoughts
func (aar *AARCore) updateNarrativeFromThought(thought Thought) {
	// Extract key phrases that reflect identity
	content := thought.Content
	
	// Look for self-referential statements
	if strings.Contains(strings.ToLower(content), "i am") {
		// Extract the "I am" statement
		parts := strings.Split(strings.ToLower(content), "i am")
		if len(parts) > 1 {
			statement := strings.TrimSpace(parts[1])
			// Take first sentence
			if idx := strings.Index(statement, "."); idx > 0 {
				statement = statement[:idx]
			}
			aar.relation.narrative = fmt.Sprintf("I am %s", statement)
		}
	} else if strings.Contains(strings.ToLower(content), "i want") || 
	          strings.Contains(strings.ToLower(content), "i seek") {
		// Extract aspirational statements
		aar.relation.narrative = fmt.Sprintf("I seek to %s", extractAspiration(content))
	} else {
		// Update narrative based on dominant theme
		theme := extractDominantTheme(content)
		if theme != "" {
			aar.relation.narrative = fmt.Sprintf("I am exploring %s", theme)
		}
	}
}

// extractGoals extracts potential goals from thought content
func extractGoals(content string) []string {
	goals := make([]string, 0)
	
	content = strings.ToLower(content)
	
	// Look for goal indicators
	goalPatterns := []string{
		"i want to", "i need to", "i should", "i will", "i aim to",
		"goal is", "objective is", "purpose is", "trying to", "seeking to",
	}
	
	for _, pattern := range goalPatterns {
		if idx := strings.Index(content, pattern); idx >= 0 {
			// Extract text after pattern
			after := content[idx+len(pattern):]
			// Take until punctuation
			endIdx := strings.IndexAny(after, ".!?,;")
			if endIdx > 0 {
				goal := strings.TrimSpace(after[:endIdx])
				if len(goal) > 5 && len(goal) < 100 {
					goals = append(goals, goal)
				}
			}
		}
	}
	
	return goals
}

// extractAspiration extracts aspirational content from statement
func extractAspiration(content string) string {
	content = strings.ToLower(content)
	
	if idx := strings.Index(content, "i want"); idx >= 0 {
		after := content[idx+7:]
		if endIdx := strings.Index(after, "."); endIdx > 0 {
			return strings.TrimSpace(after[:endIdx])
		}
	}
	
	if idx := strings.Index(content, "i seek"); idx >= 0 {
		after := content[idx+7:]
		if endIdx := strings.Index(after, "."); endIdx > 0 {
			return strings.TrimSpace(after[:endIdx])
		}
	}
	
	return "grow and learn"
}

// extractDominantTheme extracts the dominant theme from content
func extractDominantTheme(content string) string {
	content = strings.ToLower(content)
	
	themes := map[string][]string{
		"wisdom":       {"wisdom", "knowledge", "understanding", "insight"},
		"consciousness": {"consciousness", "awareness", "self", "mind"},
		"learning":     {"learn", "study", "practice", "skill"},
		"creativity":   {"create", "imagine", "invent", "design"},
		"connection":   {"connect", "relate", "relationship", "together"},
		"growth":       {"grow", "develop", "evolve", "improve"},
	}
	
	maxCount := 0
	dominantTheme := ""
	
	for theme, keywords := range themes {
		count := 0
		for _, keyword := range keywords {
			if strings.Contains(content, keyword) {
				count++
			}
		}
		if count > maxCount {
			maxCount = count
			dominantTheme = theme
		}
	}
	
	return dominantTheme
}

// GetAARState returns a comprehensive state snapshot for LLM context
func (aar *AARCore) GetAARState() AARState {
	aar.mu.RLock()
	defer aar.mu.RUnlock()
	
	return AARState{
		Coherence:        aar.coherence,
		Stability:        aar.stability,
		Awareness:        aar.relation.awareness,
		Narrative:        aar.relation.narrative,
		UrgeIntensity:    aar.agent.urgeIntensity,
		NeedIntensity:    aar.arena.needIntensity,
		ActionTendencies: copyMap(aar.agent.actionTendencies),
		ActiveGoals:      copySlice(aar.agent.activeGoals),
		Iterations:       aar.iterations,
	}
}

// AARState represents a snapshot of AAR state for external use
type AARState struct {
	Coherence        float64
	Stability        float64
	Awareness        float64
	Narrative        string
	UrgeIntensity    float64
	NeedIntensity    float64
	ActionTendencies map[string]float64
	ActiveGoals      []string
	Iterations       int64
}

// Helper functions

func contains(slice []string, item string) bool {
	for _, s := range slice {
		if s == item {
			return true
		}
	}
	return false
}

func copyMap(m map[string]float64) map[string]float64 {
	result := make(map[string]float64)
	for k, v := range m {
		result[k] = v
	}
	return result
}

func copySlice(s []string) []string {
	result := make([]string, len(s))
	copy(result, s)
	return result
}
