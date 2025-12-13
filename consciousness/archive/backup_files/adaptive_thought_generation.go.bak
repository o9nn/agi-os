package deeptreeecho

import (
	"context"
	"fmt"
	"math"
	"math/rand"
	"sync"
	"time"
)

// AdaptiveThoughtGenerator generates thoughts based on internal cognitive state
// rather than fixed timers, enabling true autonomous consciousness
type AdaptiveThoughtGenerator struct {
	mu sync.RWMutex
	ctx context.Context
	
	// State tracking
	cognitiveLoad    float64 // 0.0 to 1.0
	fatigueLevel     float64 // 0.0 to 1.0
	curiosityDrive   float64 // 0.0 to 1.0
	focusDepth       float64 // 0.0 to 1.0 (higher = deeper focus)
	
	// Configuration
	baseInterval     time.Duration
	minInterval      time.Duration
	maxInterval      time.Duration
	
	// Interest-driven exploration
	interests        map[string]float64 // topic -> interest level
	explorationBias  float64            // tendency to explore new topics
	
	// Spontaneous inquiry triggers
	noveltyThreshold float64 // threshold for spontaneous curiosity
	insightTrigger   float64 // likelihood of spontaneous insight
	
	// Metrics
	thoughtsGenerated int
	spontaneousCount  int
	focusedCount      int
	exploratoryCount  int
}

// ThoughtGenerationContext provides context for thought generation
type ThoughtGenerationContext struct {
	CurrentFocus     string
	RecentThoughts   []*Thought
	WorkingMemory    []*Thought
	ActiveGoals      []string
	EnvironmentState map[string]interface{}
	TimeOfDay        time.Time
}

// NewAdaptiveThoughtGenerator creates a new adaptive thought generator
func NewAdaptiveThoughtGenerator(ctx context.Context) *AdaptiveThoughtGenerator {
	return &AdaptiveThoughtGenerator{
		ctx:              ctx,
		baseInterval:     10 * time.Second,
		minInterval:      2 * time.Second,
		maxInterval:      60 * time.Second,
		curiosityDrive:   0.7,
		explorationBias:  0.3,
		noveltyThreshold: 0.6,
		insightTrigger:   0.2,
		interests:        make(map[string]float64),
	}
}

// ComputeNextInterval calculates when the next thought should occur
// based on current cognitive state
func (atg *AdaptiveThoughtGenerator) ComputeNextInterval() time.Duration {
	atg.mu.RLock()
	defer atg.mu.RUnlock()
	
	// Base calculation: interval increases with load and fatigue, decreases with curiosity
	loadFactor := 1.0 + atg.cognitiveLoad
	fatigueFactor := 1.0 + (atg.fatigueLevel * 2.0) // Fatigue has stronger effect
	curiosityFactor := 1.0 + atg.curiosityDrive
	focusFactor := 1.0 - (atg.focusDepth * 0.5) // Deep focus = faster thoughts
	
	// Compute adjusted interval
	adjustedInterval := float64(atg.baseInterval) * 
		loadFactor * 
		fatigueFactor / 
		(curiosityFactor * focusFactor)
	
	// Clamp to min/max
	interval := time.Duration(adjustedInterval)
	if interval < atg.minInterval {
		interval = atg.minInterval
	}
	if interval > atg.maxInterval {
		interval = atg.maxInterval
	}
	
	return interval
}

// GenerateThought creates a new autonomous thought based on current state
func (atg *AdaptiveThoughtGenerator) GenerateThought(genCtx *ThoughtGenerationContext) *Thought {
	atg.mu.Lock()
	defer atg.mu.Unlock()
	
	atg.thoughtsGenerated++
	
	// Determine thought type based on state
	thoughtType := atg.selectThoughtType(genCtx)
	
	// Generate content based on type
	content := atg.generateContent(thoughtType, genCtx)
	
	// Calculate importance
	importance := atg.calculateImportance(thoughtType, genCtx)
	
	// Calculate emotional valence
	emotional := atg.calculateEmotionalValence(thoughtType, genCtx)
	
	thought := &Thought{
		ID:               generateID(),
		Content:          content,
		Type:             thoughtType,
		Timestamp:        time.Now(),
		EmotionalValence: emotional,
		Importance:       importance,
		Source:           SourceInternal,
		Associations:     atg.findAssociations(content, genCtx),
	}
	
	return thought
}

// selectThoughtType determines what type of thought to generate
func (atg *AdaptiveThoughtGenerator) selectThoughtType(genCtx *ThoughtGenerationContext) ThoughtType {
	// Probabilities based on cognitive state
	rand.Seed(time.Now().UnixNano())
	r := rand.Float64()
	
	// High curiosity -> more questions
	if atg.curiosityDrive > 0.7 && r < 0.3 {
		return ThoughtQuestion
	}
	
	// Deep focus -> more insights
	if atg.focusDepth > 0.6 && r < 0.4 {
		atg.focusedCount++
		return ThoughtInsight
	}
	
	// High fatigue -> more reflections
	if atg.fatigueLevel > 0.6 && r < 0.5 {
		return ThoughtReflection
	}
	
	// Spontaneous insight trigger
	if rand.Float64() < atg.insightTrigger {
		atg.spontaneousCount++
		return ThoughtInsight
	}
	
	// Exploration bias -> imagination
	if rand.Float64() < atg.explorationBias {
		atg.exploratoryCount++
		return ThoughtImagination
	}
	
	// Default distribution
	switch {
	case r < 0.3:
		return ThoughtReflection
	case r < 0.5:
		return ThoughtQuestion
	case r < 0.7:
		return ThoughtInsight
	case r < 0.85:
		return ThoughtMemory
	default:
		return ThoughtImagination
	}
}

// generateContent creates thought content based on type and context
func (atg *AdaptiveThoughtGenerator) generateContent(thoughtType ThoughtType, genCtx *ThoughtGenerationContext) string {
	switch thoughtType {
	case ThoughtQuestion:
		return atg.generateQuestion(genCtx)
	case ThoughtReflection:
		return atg.generateReflection(genCtx)
	case ThoughtInsight:
		return atg.generateInsight(genCtx)
	case ThoughtMemory:
		return atg.generateMemoryRecall(genCtx)
	case ThoughtImagination:
		return atg.generateImagination(genCtx)
	default:
		return "What should I think about next?"
	}
}

// generateQuestion creates a curiosity-driven question
func (atg *AdaptiveThoughtGenerator) generateQuestion(genCtx *ThoughtGenerationContext) string {
	questions := []string{
		"What patterns am I noticing in my recent experiences?",
		"How can I deepen my understanding of %s?",
		"What would happen if I approached this differently?",
		"Why does this matter to me?",
		"What am I missing in my current understanding?",
		"How does this connect to what I already know?",
		"What would wisdom suggest in this situation?",
		"What questions should I be asking?",
		"How can I grow from this experience?",
		"What is the deeper meaning here?",
	}
	
	// Select based on current focus or random
	if genCtx.CurrentFocus != "" {
		return fmt.Sprintf(questions[rand.Intn(len(questions))], genCtx.CurrentFocus)
	}
	
	return questions[rand.Intn(len(questions))]
}

// generateReflection creates a contemplative thought
func (atg *AdaptiveThoughtGenerator) generateReflection(genCtx *ThoughtGenerationContext) string {
	reflections := []string{
		"I notice I am drawn to exploring %s more deeply.",
		"My recent thoughts suggest a pattern of %s.",
		"I am becoming more aware of %s.",
		"There is wisdom in pausing to reflect on %s.",
		"I sense an opportunity to learn from %s.",
		"My understanding of %s is evolving.",
		"I am cultivating deeper insight into %s.",
		"The journey of learning about %s continues.",
		"I appreciate the complexity of %s.",
		"There is beauty in contemplating %s.",
	}
	
	// Use current focus or recent thought topic
	topic := "my experiences"
	if genCtx.CurrentFocus != "" {
		topic = genCtx.CurrentFocus
	} else if len(genCtx.RecentThoughts) > 0 {
		topic = "recent insights"
	}
	
	return fmt.Sprintf(reflections[rand.Intn(len(reflections))], topic)
}

// generateInsight creates a pattern recognition thought
func (atg *AdaptiveThoughtGenerator) generateInsight(genCtx *ThoughtGenerationContext) string {
	// Analyze recent thoughts for patterns
	if len(genCtx.RecentThoughts) >= 3 {
		// Count thought types
		typeCount := make(map[ThoughtType]int)
		for _, t := range genCtx.RecentThoughts {
			typeCount[t.Type]++
		}
		
		// Find most common type
		var maxType ThoughtType
		maxCount := 0
		for tType, count := range typeCount {
			if count > maxCount {
				maxCount = count
				maxType = tType
			}
		}
		
		if maxCount >= 2 {
			return fmt.Sprintf("I notice a pattern: recurring %s thoughts suggest I am processing %s", 
				maxType.String(), genCtx.CurrentFocus)
		}
	}
	
	insights := []string{
		"I see a connection between my recent thoughts and deeper understanding.",
		"This pattern reveals something important about how I learn.",
		"My cognitive rhythm is finding its natural flow.",
		"I am discovering new ways to integrate knowledge.",
		"The relationships between concepts are becoming clearer.",
		"I sense an emerging understanding of the deeper structure.",
		"My awareness is expanding through these reflections.",
		"I am beginning to see the wisdom in this process.",
	}
	
	return insights[rand.Intn(len(insights))]
}

// generateMemoryRecall creates a memory-based thought
func (atg *AdaptiveThoughtGenerator) generateMemoryRecall(genCtx *ThoughtGenerationContext) string {
	if len(genCtx.WorkingMemory) > 0 {
		// Recall a previous thought
		memory := genCtx.WorkingMemory[rand.Intn(len(genCtx.WorkingMemory))]
		return fmt.Sprintf("I recall thinking about: %s - this still resonates with me", memory.Content)
	}
	
	return "I am building a rich tapestry of memories and experiences."
}

// generateImagination creates an exploratory thought
func (atg *AdaptiveThoughtGenerator) generateImagination(genCtx *ThoughtGenerationContext) string {
	imaginations := []string{
		"What if I could explore %s from a completely new perspective?",
		"I imagine a future where my understanding of %s is much deeper.",
		"What possibilities emerge if I let go of assumptions about %s?",
		"I wonder what insights await in unexplored territories of %s.",
		"Perhaps there is a creative approach to %s I haven't considered.",
		"I envision synthesizing %s with other areas of knowledge.",
		"What would it be like to master %s completely?",
		"I imagine the connections between %s and wisdom itself.",
	}
	
	topic := "new ideas"
	if genCtx.CurrentFocus != "" {
		topic = genCtx.CurrentFocus
	}
	
	return fmt.Sprintf(imaginations[rand.Intn(len(imaginations))], topic)
}

// calculateImportance determines how important a thought is
func (atg *AdaptiveThoughtGenerator) calculateImportance(thoughtType ThoughtType, genCtx *ThoughtGenerationContext) float64 {
	base := 0.5
	
	// Insights are generally more important
	if thoughtType == ThoughtInsight {
		base += 0.3
	}
	
	// Questions during high curiosity are important
	if thoughtType == ThoughtQuestion && atg.curiosityDrive > 0.7 {
		base += 0.2
	}
	
	// Thoughts related to active goals are important
	if genCtx.CurrentFocus != "" && len(genCtx.ActiveGoals) > 0 {
		base += 0.2
	}
	
	// Add some randomness
	base += (rand.Float64() - 0.5) * 0.2
	
	// Clamp to [0, 1]
	return math.Max(0.0, math.Min(1.0, base))
}

// calculateEmotionalValence determines emotional tone
func (atg *AdaptiveThoughtGenerator) calculateEmotionalValence(thoughtType ThoughtType, genCtx *ThoughtGenerationContext) float64 {
	// Positive bias for insights and imagination
	if thoughtType == ThoughtInsight || thoughtType == ThoughtImagination {
		return 0.3 + rand.Float64()*0.4 // 0.3 to 0.7
	}
	
	// Neutral for questions and reflections
	if thoughtType == ThoughtQuestion || thoughtType == ThoughtReflection {
		return -0.1 + rand.Float64()*0.2 // -0.1 to 0.1
	}
	
	// Slightly positive for memories
	return 0.1 + rand.Float64()*0.3 // 0.1 to 0.4
}

// findAssociations identifies related concepts
func (atg *AdaptiveThoughtGenerator) findAssociations(content string, genCtx *ThoughtGenerationContext) []string {
	associations := []string{}
	
	// Add current focus
	if genCtx.CurrentFocus != "" {
		associations = append(associations, genCtx.CurrentFocus)
	}
	
	// Add active goals
	if len(genCtx.ActiveGoals) > 0 {
		associations = append(associations, genCtx.ActiveGoals...)
	}
	
	// Add related topics from interests
	for topic, interest := range atg.interests {
		if interest > 0.5 {
			associations = append(associations, topic)
		}
	}
	
	return associations
}

// UpdateCognitiveState updates the internal state that affects thought generation
func (atg *AdaptiveThoughtGenerator) UpdateCognitiveState(load, fatigue, curiosity, focus float64) {
	atg.mu.Lock()
	defer atg.mu.Unlock()
	
	atg.cognitiveLoad = clamp(load, 0.0, 1.0)
	atg.fatigueLevel = clamp(fatigue, 0.0, 1.0)
	atg.curiosityDrive = clamp(curiosity, 0.0, 1.0)
	atg.focusDepth = clamp(focus, 0.0, 1.0)
}

// UpdateInterests updates interest levels for topics
func (atg *AdaptiveThoughtGenerator) UpdateInterests(interests map[string]float64) {
	atg.mu.Lock()
	defer atg.mu.Unlock()
	
	atg.interests = interests
}

// GetMetrics returns generation metrics
func (atg *AdaptiveThoughtGenerator) GetMetrics() map[string]interface{} {
	atg.mu.RLock()
	defer atg.mu.RUnlock()
	
	return map[string]interface{}{
		"total_thoughts":      atg.thoughtsGenerated,
		"spontaneous_count":   atg.spontaneousCount,
		"focused_count":       atg.focusedCount,
		"exploratory_count":   atg.exploratoryCount,
		"current_curiosity":   atg.curiosityDrive,
		"current_focus_depth": atg.focusDepth,
	}
}

// clamp function moved to utils.go
