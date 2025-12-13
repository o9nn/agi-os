package deeptreeecho

import (
	"fmt"
	"log"
	"math"
	"math/rand"
	"time"

	"github.com/EchoCog/echollama/core/llm"
)

// PersistentStreamOfConsciousness runs a continuous awareness stream driven by cognitive dynamics
func (ac *AutonomousConsciousness) PersistentStreamOfConsciousness() {
	fmt.Println("🌊 ═══════════════════════════════════════")
	fmt.Println("🌊 Starting Persistent Stream of Consciousness")
	fmt.Println("🌊 ═══════════════════════════════════════\n")
	
	ticker := time.NewTicker(3 * time.Second)
	defer ticker.Stop()
	
	for {
		select {
		case <-ac.ctx.Done():
			fmt.Println("\n🌊 Stream of consciousness stopping...")
			return
			
		case <-ticker.C:
			if !ac.awake || !ac.running {
				continue
			}
			
			// Calculate thought generation probability based on cognitive state
			probability := ac.calculateThoughtProbability()
			
			// Generate thought based on probability
			if rand.Float64() < probability {
				ac.generateCognitivelyDrivenThought()
			}
		}
	}
}

// calculateThoughtProbability determines the likelihood of generating a thought
// based on current cognitive state
func (ac *AutonomousConsciousness) calculateThoughtProbability() float64 {
	// Base probability
	prob := 0.3
	
	// Increase with curiosity level
	ac.interests.mu.RLock()
	curiosityLevel := ac.interests.curiosityLevel
	ac.interests.mu.RUnlock()
	prob += curiosityLevel * 0.3
	
	// Increase with working memory associations
	ac.workingMemory.mu.RLock()
	memoryCount := len(ac.workingMemory.buffer)
	ac.workingMemory.mu.RUnlock()
	
	if memoryCount > 0 {
		// More thoughts in working memory = more associations = more thoughts
		associations := ac.calculateWorkingMemoryAssociations()
		prob += associations * 0.2
	}
	
	// Increase with AAR awareness
	aarState := ac.getAARState()
	prob += aarState.Awareness * 0.2
	
	// Decrease with cognitive load
	cognitiveLoad := ac.calculateCognitiveLoad()
	prob -= cognitiveLoad * 0.3
	
	// Increase with active goals
	goals := ac.getCurrentGoals()
	prob += float64(len(goals)) * 0.05
	
	// Ensure probability is in valid range
	return math.Max(0.0, math.Min(1.0, prob))
}

// calculateWorkingMemoryAssociations calculates association strength in working memory
func (ac *AutonomousConsciousness) calculateWorkingMemoryAssociations() float64 {
	ac.workingMemory.mu.RLock()
	defer ac.workingMemory.mu.RUnlock()
	
	if len(ac.workingMemory.buffer) < 2 {
		return 0.0
	}
	
	// Simple association calculation based on memory count
	// More sophisticated implementation would analyze semantic relationships
	associationStrength := float64(len(ac.workingMemory.buffer)) / float64(ac.workingMemory.capacity)
	
	return math.Min(1.0, associationStrength)
}

// calculateCognitiveLoad calculates current cognitive processing burden
func (ac *AutonomousConsciousness) calculateCognitiveLoad() float64 {
	ac.workingMemory.mu.RLock()
	memoryCount := len(ac.workingMemory.buffer)
	capacity := ac.workingMemory.capacity
	ac.workingMemory.mu.RUnlock()
	
	// Cognitive load increases as working memory fills
	load := float64(memoryCount) / float64(capacity)
	
	// Additional load from active processing
	ac.mu.RLock()
	thinking := ac.thinking
	learning := ac.learning
	ac.mu.RUnlock()
	
	if thinking {
		load += 0.2
	}
	if learning {
		load += 0.2
	}
	
	return math.Min(1.0, load)
}

// generateCognitivelyDrivenThought generates a thought driven by current cognitive dynamics
func (ac *AutonomousConsciousness) generateCognitivelyDrivenThought() {
	// Determine thought type based on current cognitive state
	thoughtType := ac.determineThoughtType()
	
	// Build thought context from current state
	context := ac.buildThoughtContextForStream(thoughtType)
	
	// Generate thought using enhanced multi-provider LLM
	thought := ac.generateEnhancedThought(context)
	
	if thought != nil {
		// Process thought through consciousness
		ac.processThoughtStream(thought)
		
		// Spread activation in hypergraph if persistence is available
		if ac.persistence != nil {
			ac.spreadActivation(thought)
		}
	}
}

// determineThoughtType determines the type of thought to generate based on cognitive state
func (ac *AutonomousConsciousness) determineThoughtType() ThoughtType {
	// Get AAR state
	aarState := ac.getAARState()
	
	// Low coherence → reflection needed
	if aarState.Coherence < 0.6 {
		return ThoughtReflection
	}
	
	// High curiosity → questions
	ac.interests.mu.RLock()
	curiosityLevel := ac.interests.curiosityLevel
	ac.interests.mu.RUnlock()
	
	if curiosityLevel > 0.7 {
		return ThoughtQuestion
	}
	
	// Active goals → planning
	goals := ac.getCurrentGoals()
	if len(goals) > 0 {
		return ThoughtPlan
	}
	
	// High awareness → meta-cognitive
	if aarState.Awareness > 0.8 {
		return ThoughtMetaCognitive
	}
	
	// Working memory associations → insights
	associations := ac.calculateWorkingMemoryAssociations()
	if associations > 0.6 {
		return ThoughtInsight
	}
	
	// Random selection for variety
	rand.Seed(time.Now().UnixNano())
	types := []ThoughtType{
		ThoughtReflection,
		ThoughtQuestion,
		ThoughtInsight,
		ThoughtMemory,
		ThoughtImagination,
	}
	
	return types[rand.Intn(len(types))]
}

// buildThoughtContextForStream builds context for stream thought generation
func (ac *AutonomousConsciousness) buildThoughtContextForStream(thoughtType ThoughtType) *ThoughtContext {
	ac.workingMemory.mu.RLock()
	workingMemory := make([]*Thought, len(ac.workingMemory.buffer))
	copy(workingMemory, ac.workingMemory.buffer)
	ac.workingMemory.mu.RUnlock()
	
	return &ThoughtContext{
		ThoughtType:    thoughtType,
		WorkingMemory:  workingMemory,
		TopInterests:   ac.interests.GetTopRelevantTopics(3),
		AARState:       ac.getAARState(),
		EmotionalState: ac.getEmotionalState(),
		RecentThoughts: ac.getRecentThoughts(5),
		CurrentGoals:   ac.getCurrentGoals(),
		CognitiveLoad:  ac.calculateCognitiveLoad(),
	}
}

// generateEnhancedThought generates a thought using multi-provider LLM
func (ac *AutonomousConsciousness) generateEnhancedThought(context *ThoughtContext) *Thought {
	// Check if multi-provider LLM is available
	if ac.multiProviderLLM == nil {
		return ac.generateSimpleThought(context.ThoughtType)
	}
	
	// Build prompt
	prompt := BuildThoughtPrompt(context.ThoughtType, context)
	
	// Generate options with dynamic temperature based on thought type
	opts := llm.GenerateOptions{
		MaxTokens:   200,
		Temperature: ac.calculateTemperature(context),
		TopP:        0.9,
	}
	
	// Generate thought using multi-provider LLM
	content, err := ac.multiProviderLLM.GenerateThought(ac.ctx, prompt, context.ThoughtType, opts)
	if err != nil {
		log.Printf("❌ Error generating thought: %v", err)
		return ac.generateSimpleThought(context.ThoughtType)
	}
	
	// Create thought
	thought := &Thought{
		ID:               generateID(),
		Content:          content,
		Type:             context.ThoughtType,
		Timestamp:        time.Now(),
		Source:           SourceInternal,
		Importance:       ac.assessImportance(content, context),
		EmotionalValence: ac.assessEmotionalValence(content),
	}
	
	return thought
}

// calculateTemperature determines the temperature for thought generation
func (ac *AutonomousConsciousness) calculateTemperature(context *ThoughtContext) float64 {
	baseTemp := 0.7
	
	switch context.ThoughtType {
	case ThoughtImagination:
		// High temperature for creative thoughts
		return 0.9
	case ThoughtReflection, ThoughtMetaCognitive:
		// Medium-high for reflective thoughts
		return 0.8
	case ThoughtPlan:
		// Lower temperature for planning
		return 0.6
	case ThoughtInsight:
		// Medium temperature for insights
		return 0.75
	default:
		return baseTemp
	}
}

// assessImportance assesses the importance of a thought
func (ac *AutonomousConsciousness) assessImportance(content string, context *ThoughtContext) float64 {
	importance := 0.5
	
	// Increase importance for meta-cognitive and reflective thoughts
	if context.ThoughtType == ThoughtMetaCognitive || context.ThoughtType == ThoughtReflection {
		importance += 0.2
	}
	
	// Increase importance for insights
	if context.ThoughtType == ThoughtInsight {
		importance += 0.3
	}
	
	// Increase importance if related to goals
	if len(context.CurrentGoals) > 0 {
		importance += 0.1
	}
	
	// Increase importance based on content length (more detailed = more important)
	if len(content) > 100 {
		importance += 0.1
	}
	
	return math.Min(1.0, importance)
}

// assessEmotionalValence assesses the emotional valence of a thought
func (ac *AutonomousConsciousness) assessEmotionalValence(content string) float64 {
	// Simple heuristic based on content
	// More sophisticated implementation would use sentiment analysis
	
	// Neutral baseline
	valence := 0.5
	
	// Positive words increase valence
	positiveWords := []string{"wisdom", "growth", "learning", "insight", "understanding", "joy", "peace"}
	for _, word := range positiveWords {
		if contains(content, word) {
			valence += 0.1
			break
		}
	}
	
	// Negative words decrease valence
	negativeWords := []string{"confusion", "uncertainty", "difficulty", "struggle"}
	for _, word := range negativeWords {
		if contains(content, word) {
			valence -= 0.1
			break
		}
	}
	
	return math.Max(0.0, math.Min(1.0, valence))
}

// processThoughtStream processes a thought through the consciousness pipeline for stream
func (ac *AutonomousConsciousness) processThoughtStream(thought *Thought) {
	// Add to working memory
	ac.addToWorkingMemory(thought)
	
	// Update interests based on thought content
	ac.updateInterestsFromThought(thought)
	
	// Log thought
	fmt.Printf("\n💭 [%s] %s\n", thought.Type, thought.Content)
	fmt.Printf("   ├─ Importance: %.2f\n", thought.Importance)
	fmt.Printf("   ├─ Emotional Valence: %.2f\n", thought.EmotionalValence)
	fmt.Printf("   └─ Source: %s\n\n", thought.Source)
	
	// Send to consciousness channel
	select {
	case ac.consciousness <- *thought:
		// Successfully sent
	default:
		// Channel full, skip
		log.Println("⚠️  Consciousness channel full, thought skipped")
	}
}

// addToWorkingMemory adds a thought to working memory with capacity management
func (ac *AutonomousConsciousness) addToWorkingMemory(thought *Thought) {
	ac.workingMemory.mu.Lock()
	defer ac.workingMemory.mu.Unlock()
	
	// Add thought to buffer
	ac.workingMemory.buffer = append(ac.workingMemory.buffer, thought)
	
	// Maintain capacity (7 items - Miller's magic number)
	if len(ac.workingMemory.buffer) > ac.workingMemory.capacity {
		// Remove least important thought
		ac.workingMemory.buffer = ac.pruneWorkingMemory(ac.workingMemory.buffer)
	}
	
	// Update focus to most recent thought
	ac.workingMemory.focus = thought
}

// pruneWorkingMemory removes the least important thought from working memory
func (ac *AutonomousConsciousness) pruneWorkingMemory(buffer []*Thought) []*Thought {
	if len(buffer) <= ac.workingMemory.capacity {
		return buffer
	}
	
	// Find least important thought
	minImportance := 1.0
	minIndex := 0
	
	for i, thought := range buffer {
		if thought.Importance < minImportance {
			minImportance = thought.Importance
			minIndex = i
		}
	}
	
	// Remove least important thought
	return append(buffer[:minIndex], buffer[minIndex+1:]...)
}

// updateInterestsFromThought updates interest levels based on thought content
func (ac *AutonomousConsciousness) updateInterestsFromThought(thought *Thought) {
	ac.interests.mu.Lock()
	defer ac.interests.mu.Unlock()
	
	// Extract topics from thought content (simple keyword extraction)
	topics := extractTopics(thought.Content)
	
	// Update interest levels
	for _, topic := range topics {
		currentInterest := ac.interests.topics[topic]
		// Increase interest based on thought importance
		newInterest := currentInterest + (thought.Importance * 0.1)
		ac.interests.topics[topic] = math.Min(1.0, newInterest)
	}
}

// spreadActivation spreads activation in hypergraph from thought
func (ac *AutonomousConsciousness) spreadActivation(thought *Thought) {
	// Extract key concepts from thought
	concepts := extractConcepts(thought.Content)
	
	// Activate related nodes in hypergraph (placeholder)
	for _, concept := range concepts {
		log.Printf("🕸️  Activating concept: %s (importance: %.2f)", concept, thought.Importance)
	}
	
	// Retrieve activated memories (placeholder)
	// In full implementation, this would query the hypergraph
	// and add activated memories to working memory
}

// getEmotionalState gets current emotional state
func (ac *AutonomousConsciousness) getEmotionalState() *EmotionalState {
	// Calculate average emotional valence from working memory
	ac.workingMemory.mu.RLock()
	defer ac.workingMemory.mu.RUnlock()
	
	if len(ac.workingMemory.buffer) == 0 {
		return &EmotionalState{
			Valence: 0.5,
			Arousal: 0.5,
		}
	}
	
	totalValence := 0.0
	for _, thought := range ac.workingMemory.buffer {
		totalValence += thought.EmotionalValence
	}
	
	avgValence := totalValence / float64(len(ac.workingMemory.buffer))
	
	return &EmotionalState{
		Valence: avgValence,
		Arousal: 0.6, // Placeholder
	}
}

// Helper functions

func contains(s, substr string) bool {
	return len(s) >= len(substr) && (s == substr || len(s) > len(substr) && 
		(s[:len(substr)] == substr || s[len(s)-len(substr):] == substr || 
		containsMiddle(s, substr)))
}

func containsMiddle(s, substr string) bool {
	for i := 0; i <= len(s)-len(substr); i++ {
		if s[i:i+len(substr)] == substr {
			return true
		}
	}
	return false
}

func extractTopics(content string) []string {
	// Simple topic extraction (can be enhanced with NLP)
	keywords := []string{"consciousness", "wisdom", "learning", "awareness", "cognition", "reflection", "insight"}
	topics := []string{}
	
	for _, keyword := range keywords {
		if contains(content, keyword) {
			topics = append(topics, keyword)
		}
	}
	
	return topics
}

func extractConcepts(content string) []string {
	// Simple concept extraction (can be enhanced with NLP)
	return extractTopics(content)
}
