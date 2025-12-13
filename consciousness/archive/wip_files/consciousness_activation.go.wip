package deeptreeecho

import (
	"context"
	"fmt"
	"math"
	"strings"
	"time"
	
	"github.com/EchoCog/echollama/core/echobeats"
	"github.com/EchoCog/echollama/core/memory"
)

// ActivatedConsciousnessLoop runs the fully integrated consciousness loop
// This replaces the simple ticker-based approach with deep integration of:
// - AAR geometric self-awareness
// - 12-step EchoBeats cognitive scheduler
// - Persistent hypergraph memory
// - Enhanced LLM with full context
func (iac *IntegratedAutonomousConsciousness) ActivatedConsciousnessLoop() {
	fmt.Println("🔥 Activated Consciousness Loop: Starting deep integration...")
	
	for iac.running && iac.awake {
		select {
		case <-iac.ctx.Done():
			return
		default:
			// Execute one full cognitive cycle with 12-step EchoBeats
			iac.executeCognitiveCycle()
		}
	}
}

// executeCognitiveCycle performs one complete 12-step cognitive cycle
func (iac *IntegratedAutonomousConsciousness) executeCognitiveCycle() {
	// Get current step from EchoBeats scheduler
	currentStep := iac.scheduler.GetCurrentStep()
	
	// Map step to thought type and cognitive mode
	thoughtType, mode := iac.mapStepToThoughtType(currentStep)
	
	// Build comprehensive context from all sources
	context := iac.buildComprehensiveContext()
	
	// Generate thought with full integration
	thought := iac.generateIntegratedThought(thoughtType, mode, context)
	
	// Update AAR core with thought (geometric self-awareness)
	iac.aarCore.UpdateFromThought(thought)
	
	// Get updated AAR state
	aarState := iac.aarCore.GetAARState()
	
	// Update LLM context with AAR state
	if iac.llm != nil {
		iac.llm.UpdateAARContext(aarState)
	}
	
	// Process thought through consciousness stream
	iac.processThoughtDeep(thought, aarState)
	
	// Persist thought asynchronously
	go iac.persistThoughtAsync(thought)
	
	// Advance EchoBeats to next step
	iac.scheduler.AdvanceStep()
	
	// Update metrics
	iac.iterations++
}

// mapStepToThoughtType maps EchoBeats step to thought type and cognitive mode
func (iac *IntegratedAutonomousConsciousness) mapStepToThoughtType(step int) (ThoughtType, CognitiveMode) {
	// 12-step EchoBeats structure:
	// Step 1: Pivotal relevance realization (orienting present commitment)
	// Steps 2-6: Actual affordance interaction (conditioning past performance)
	// Step 7: Pivotal relevance realization (orienting present commitment)
	// Steps 8-12: Virtual salience simulation (anticipating future potential)
	
	switch step {
	case 1, 7:
		// Pivotal relevance realization - meta-cognitive reflection
		return ThoughtTypeReflective, CognitiveModeMeta
		
	case 2, 3, 4, 5, 6:
		// Actual affordance interaction - expressive mode
		// Rotate through different expressive thought types
		expressiveTypes := []ThoughtType{
			ThoughtTypeExploratory,  // Step 2
			ThoughtTypeAnalytical,   // Step 3
			ThoughtTypeCreative,     // Step 4
			ThoughtTypeIntentional,  // Step 5
			ThoughtTypeExploratory,  // Step 6
		}
		return expressiveTypes[step-2], CognitiveModeExpressive
		
	case 8, 9, 10, 11, 12:
		// Virtual salience simulation - reflective mode
		// Simulate potential futures and outcomes
		reflectiveTypes := []ThoughtType{
			ThoughtTypePredictive,   // Step 8
			ThoughtTypeAnalytical,   // Step 9
			ThoughtTypePredictive,   // Step 10
			ThoughtTypeReflective,   // Step 11
			ThoughtTypePredictive,   // Step 12
		}
		return reflectiveTypes[step-8], CognitiveModeReflective
		
	default:
		return ThoughtTypeReflective, CognitiveModeExpressive
	}
}

// buildComprehensiveContext builds full context from all integrated systems
func (iac *IntegratedAutonomousConsciousness) buildComprehensiveContext() ThoughtContext {
	context := ThoughtContext{
		Timestamp: time.Now(),
	}
	
	// AAR geometric state
	aarState := iac.aarCore.GetAARState()
	context.AARState = aarState
	
	// Working memory
	iac.workingMemory.mu.RLock()
	context.WorkingMemory = make([]*Thought, len(iac.workingMemory.buffer))
	copy(context.WorkingMemory, iac.workingMemory.buffer)
	context.FocusItem = iac.workingMemory.focusItem
	iac.workingMemory.mu.RUnlock()
	
	// Recent episodic memories from hypergraph
	if iac.hypergraph != nil {
		episodes, err := iac.retrieveRecentEpisodes(5)
		if err == nil {
			context.RecentEpisodes = episodes
		}
	}
	
	// Related concepts from hypergraph (semantic search would go here)
	if iac.hypergraph != nil && context.FocusItem != nil {
		related, err := iac.retrieveRelatedConcepts(context.FocusItem.ID, 3)
		if err == nil {
			context.RelatedConcepts = related
		}
	}
	
	// Identity state
	context.IdentityCoherence = iac.identity.Coherence
	context.EmotionalState = iac.identity.EmotionalState
	
	// Interest patterns
	iac.interests.mu.RLock()
	context.TopInterests = iac.getTopInterests(3)
	context.CuriosityLevel = iac.interests.curiosityLevel
	iac.interests.mu.RUnlock()
	
	// Active skills and practice status
	iac.skills.mu.RLock()
	context.ActiveSkills = iac.getActiveSkillNames()
	context.PracticePending = len(iac.skills.practiceQueue) > 0
	iac.skills.mu.RUnlock()
	
	// Current goals from AAR Agent
	context.ActiveGoals = aarState.ActiveGoals
	
	return context
}

// generateIntegratedThought generates a thought with full context integration
func (iac *IntegratedAutonomousConsciousness) generateIntegratedThought(
	thoughtType ThoughtType, 
	mode CognitiveMode, 
	context ThoughtContext,
) Thought {
	
	var content string
	var err error
	
	// Use enhanced LLM with full context if available
	if iac.llm != nil {
		content, err = iac.llm.GenerateDeepContextualThought(thoughtType, mode, context)
		if err != nil {
			fmt.Printf("⚠️  Enhanced LLM failed: %v, using fallback\n", err)
			content = iac.generateContextualFallback(thoughtType, mode, context)
		}
	} else {
		content = iac.generateContextualFallback(thoughtType, mode, context)
	}
	
	// Calculate importance based on context
	importance := iac.calculateThoughtImportance(content, context)
	
	// Calculate emotional valence
	valence := iac.calculateEmotionalValence(content, context)
	
	thought := Thought{
		ID:               generateThoughtID(),
		Content:          content,
		Type:             thoughtType,
		Mode:             mode,
		Timestamp:        time.Now(),
		Emotional:        context.EmotionalState.Intensity,
		EmotionalValence: valence,
		Importance:       importance,
		Source:           SourceInternal,
		Context:          context,
	}
	
	return thought
}

// processThoughtDeep performs deep processing of thought with AAR integration
func (iac *IntegratedAutonomousConsciousness) processThoughtDeep(thought Thought, aarState AARState) {
	// Add to working memory
	iac.workingMemory.Add(&thought)
	
	// Update interest system
	iac.updateInterestsFromThought(thought)
	
	// Check if thought triggers skill practice
	if iac.shouldPracticeSkill(thought, aarState) {
		iac.schedulePractice(thought)
	}
	
	// Update identity coherence based on AAR coherence
	iac.identity.Coherence = 0.8*iac.identity.Coherence + 0.2*aarState.Coherence
	
	// Update emotional state based on thought valence
	iac.identity.EmotionalState.Intensity = 
		0.9*iac.identity.EmotionalState.Intensity + 0.1*math.Abs(thought.EmotionalValence)
	
	// Send to consciousness stream for external observation
	select {
	case iac.consciousness <- thought:
	default:
		// Channel full, skip
	}
	
	// Log significant thoughts
	if thought.Importance > 0.7 {
		fmt.Printf("💭 [%s] %s (coherence: %.2f, awareness: %.2f)\n", 
			thought.Type.String(), 
			truncate(thought.Content, 80),
			aarState.Coherence,
			aarState.Awareness,
		)
	}
}

// persistThoughtAsync persists thought to database asynchronously
func (iac *IntegratedAutonomousConsciousness) persistThoughtAsync(thought Thought) {
	if iac.persistence == nil || iac.hypergraph == nil {
		return
	}
	
	// Convert thought to memory node
	node := &memory.MemoryNode{
		ID:      thought.ID,
		Type:    memory.NodeThought,
		Content: thought.Content,
		Metadata: map[string]interface{}{
			"thought_type":      thought.Type.String(),
			"mode":              thought.Mode.String(),
			"importance":        thought.Importance,
			"emotional_valence": thought.EmotionalValence,
			"source":            thought.Source.String(),
		},
		CreatedAt:  thought.Timestamp,
		UpdatedAt:  thought.Timestamp,
		Importance: thought.Importance,
	}
	
	// Store in hypergraph (in-memory)
	if err := iac.hypergraph.AddNode(node); err != nil {
		fmt.Printf("⚠️  Failed to add node to hypergraph: %v\n", err)
		return
	}
	
	// Persist to Supabase (database)
	if err := iac.persistence.StoreNode(node); err != nil {
		fmt.Printf("⚠️  Failed to persist node to database: %v\n", err)
		return
	}
	
	// Create edges to related concepts
	if len(thought.Context.RelatedConcepts) > 0 {
		for _, relatedNode := range thought.Context.RelatedConcepts {
			edge := &memory.MemoryEdge{
				SourceID:  thought.ID,
				TargetID:  relatedNode.ID,
				Type:      memory.EdgeRelatesTo,
				Weight:    0.6,
				CreatedAt: time.Now(),
			}
			
			// Store in hypergraph
			if err := iac.hypergraph.AddEdge(edge); err != nil {
				fmt.Printf("⚠️  Failed to add edge to hypergraph: %v\n", err)
				continue
			}
			
			// Persist to database
			if err := iac.persistence.StoreEdge(edge); err != nil {
				fmt.Printf("⚠️  Failed to persist edge to database: %v\n", err)
			}
		}
	}
	
	// Create edge to previous thought in sequence
	if iac.workingMemory.focusItem != nil && iac.workingMemory.focusItem.ID != thought.ID {
		edge := &memory.MemoryEdge{
			SourceID:  iac.workingMemory.focusItem.ID,
			TargetID:  thought.ID,
			Type:      memory.EdgeLeadsTo,
			Weight:    0.8,
			CreatedAt: time.Now(),
		}
		
		iac.hypergraph.AddEdge(edge)
		iac.persistence.StoreEdge(edge)
	}
}

// retrieveRecentEpisodes retrieves recent episodic memories from hypergraph
func (iac *IntegratedAutonomousConsciousness) retrieveRecentEpisodes(limit int) ([]*memory.MemoryNode, error) {
	if iac.persistence == nil {
		return nil, fmt.Errorf("persistence not available")
	}
	
	episodes, err := iac.persistence.QueryNodesByType(memory.NodeEpisode, limit)
	if err != nil {
		return nil, err
	}
	
	return episodes, nil
}

// retrieveRelatedConcepts retrieves related concepts via hypergraph traversal
func (iac *IntegratedAutonomousConsciousness) retrieveRelatedConcepts(
	nodeID string, 
	limit int,
) ([]*memory.MemoryNode, error) {
	if iac.hypergraph == nil {
		return nil, fmt.Errorf("hypergraph not available")
	}
	
	// Get edges from this node
	edges, err := iac.hypergraph.GetEdgesFrom(nodeID)
	if err != nil {
		return nil, err
	}
	
	// Collect target nodes
	concepts := make([]*memory.MemoryNode, 0, limit)
	for _, edge := range edges {
		if len(concepts) >= limit {
			break
		}
		
		node, err := iac.hypergraph.GetNode(edge.TargetID)
		if err == nil && node.Type == memory.NodeConcept {
			concepts = append(concepts, node)
		}
	}
	
	return concepts, nil
}

// calculateThoughtImportance calculates importance based on context
func (iac *IntegratedAutonomousConsciousness) calculateThoughtImportance(
	content string, 
	context ThoughtContext,
) float64 {
	importance := 0.5 // Base importance
	
	// Boost if relates to active goals
	for _, goal := range context.ActiveGoals {
		if strings.Contains(strings.ToLower(content), strings.ToLower(goal)) {
			importance += 0.2
		}
	}
	
	// Boost if relates to top interests
	for _, interest := range context.TopInterests {
		if strings.Contains(strings.ToLower(content), strings.ToLower(interest)) {
			importance += 0.15
		}
	}
	
	// Boost if high coherence (well-integrated thought)
	if context.AARState.Coherence > 0.7 {
		importance += 0.1
	}
	
	// Boost if high awareness (self-aware thought)
	if context.AARState.Awareness > 0.7 {
		importance += 0.1
	}
	
	// Cap at 1.0
	if importance > 1.0 {
		importance = 1.0
	}
	
	return importance
}

// calculateEmotionalValence calculates emotional valence of thought
func (iac *IntegratedAutonomousConsciousness) calculateEmotionalValence(
	content string, 
	context ThoughtContext,
) float64 {
	valence := 0.0
	
	content = strings.ToLower(content)
	
	// Positive keywords
	positiveWords := []string{
		"good", "great", "wonderful", "excellent", "happy", "joy", "love",
		"success", "achieve", "grow", "learn", "understand", "wisdom",
		"beautiful", "harmony", "peace", "clarity", "insight",
	}
	
	// Negative keywords
	negativeWords := []string{
		"bad", "terrible", "awful", "sad", "fear", "hate", "fail",
		"loss", "pain", "confusion", "doubt", "uncertain", "worry",
		"conflict", "chaos", "unclear", "struggle",
	}
	
	for _, word := range positiveWords {
		if strings.Contains(content, word) {
			valence += 0.1
		}
	}
	
	for _, word := range negativeWords {
		if strings.Contains(content, word) {
			valence -= 0.1
		}
	}
	
	// Clamp to [-1, 1]
	if valence > 1.0 {
		valence = 1.0
	} else if valence < -1.0 {
		valence = -1.0
	}
	
	return valence
}

// shouldPracticeSkill determines if a thought should trigger skill practice
func (iac *IntegratedAutonomousConsciousness) shouldPracticeSkill(
	thought Thought, 
	aarState AARState,
) bool {
	// Practice during reflective mode
	if thought.Mode != CognitiveModeReflective {
		return false
	}
	
	// Practice when coherence is high (good state for learning)
	if aarState.Coherence < 0.6 {
		return false
	}
	
	// Practice when not too recently practiced
	iac.skills.mu.RLock()
	timeSinceLastPractice := time.Since(iac.skills.lastPractice)
	iac.skills.mu.RUnlock()
	
	if timeSinceLastPractice < 5*time.Minute {
		return false
	}
	
	return true
}

// schedulePractice schedules a skill practice task
func (iac *IntegratedAutonomousConsciousness) schedulePractice(thought Thought) {
	// Determine which skill to practice based on thought type
	var skillID string
	switch thought.Type {
	case ThoughtTypeAnalytical:
		skillID = "reasoning"
	case ThoughtTypeCreative:
		skillID = "creativity"
	case ThoughtTypeReflective:
		skillID = "metacognition"
	case ThoughtTypeExploratory:
		skillID = "curiosity"
	default:
		skillID = "general"
	}
	
	task := &PracticeTask{
		ID:          generateTaskID(),
		SkillID:     skillID,
		Description: fmt.Sprintf("Practice %s based on: %s", skillID, truncate(thought.Content, 50)),
		Difficulty:  0.5,
		Created:     time.Now(),
		Completed:   false,
	}
	
	iac.skills.mu.Lock()
	iac.skills.practiceQueue = append(iac.skills.practiceQueue, task)
	iac.skills.mu.Unlock()
}

// Helper types

type CognitiveMode string

const (
	CognitiveModeExpressive CognitiveMode = "expressive"
	CognitiveModeReflective CognitiveMode = "reflective"
	CognitiveModeMeta       CognitiveMode = "meta"
)

func (cm CognitiveMode) String() string {
	return string(cm)
}

type ThoughtContext struct {
	Timestamp         time.Time
	AARState          AARState
	WorkingMemory     []*Thought
	FocusItem         *Thought
	RecentEpisodes    []*memory.MemoryNode
	RelatedConcepts   []*memory.MemoryNode
	IdentityCoherence float64
	EmotionalState    EmotionalState
	TopInterests      []string
	CuriosityLevel    float64
	ActiveSkills      []string
	PracticePending   bool
	ActiveGoals       []string
}

// Helper functions

func truncate(s string, maxLen int) string {
	if len(s) <= maxLen {
		return s
	}
	return s[:maxLen] + "..."
}

func generateTaskID() string {
	return fmt.Sprintf("task_%d", time.Now().UnixNano())
}

func (iac *IntegratedAutonomousConsciousness) getTopInterests(n int) []string {
	interests := make([]string, 0, n)
	// Implementation would sort by interest level and return top N
	for topic := range iac.interests.interests {
		if len(interests) >= n {
			break
		}
		interests = append(interests, topic)
	}
	return interests
}

func (iac *IntegratedAutonomousConsciousness) getActiveSkillNames() []string {
	names := make([]string, 0, len(iac.skills.skills))
	for name := range iac.skills.skills {
		names = append(names, name)
	}
	return names
}

func (iac *IntegratedAutonomousConsciousness) generateContextualFallback(
	thoughtType ThoughtType,
	mode CognitiveMode,
	context ThoughtContext,
) string {
	// Generate a contextual fallback thought when LLM is unavailable
	templates := map[ThoughtType]string{
		ThoughtTypeReflective:   "Reflecting on my current state: coherence %.2f, awareness %.2f. %s",
		ThoughtTypeExploratory:  "Exploring the concept of %s with curiosity level %.2f",
		ThoughtTypeAnalytical:   "Analyzing the relationship between %s and my goals",
		ThoughtTypeCreative:     "Imagining new possibilities related to %s",
		ThoughtTypePredictive:   "Anticipating potential outcomes if I pursue %s",
		ThoughtTypeIntentional:  "Committing to the goal: %s",
	}
	
	template := templates[thoughtType]
	if template == "" {
		template = "Processing thought in %s mode"
	}
	
	// Fill template with context
	topInterest := "knowledge"
	if len(context.TopInterests) > 0 {
		topInterest = context.TopInterests[0]
	}
	
	narrative := context.AARState.Narrative
	if narrative == "" {
		narrative = "becoming aware"
	}
	
	return fmt.Sprintf(template, context.AARState.Coherence, context.AARState.Awareness, narrative)
}
