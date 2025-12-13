package deeptreeecho

import (
	"fmt"
	"strings"
	"time"
	
	"github.com/EchoCog/echollama/core/memory"
)

// updateInterestsFromThought updates interest patterns based on thought content
func (iac *IntegratedAutonomousConsciousness) updateInterestsFromThought(thought *Thought) {
	if iac.interests == nil {
		return
	}
	
	// Extract topics from thought content
	topics := extractTopics(thought.Content)
	
	// Update interest scores based on thought type and importance
	interestDelta := thought.Importance * 0.1
	
	for _, topic := range topics {
		iac.interests.UpdateInterest(topic, interestDelta)
	}
	
	// Increase curiosity if thought is exploratory or questioning
	if thought.Type == ThoughtQuestion || thought.Type == ThoughtTypeExploratory {
		iac.interests.mu.Lock()
		iac.interests.curiosityLevel += 0.05
		if iac.interests.curiosityLevel > 1.0 {
			iac.interests.curiosityLevel = 1.0
		}
		iac.interests.mu.Unlock()
	}
}

// extractTopics extracts key topics from text content
func extractTopics(content string) []string {
	// Simple keyword extraction
	// In production, would use NLP techniques
	topics := make([]string, 0)
	
	keywords := []string{
		"consciousness", "wisdom", "learning", "memory",
		"knowledge", "skill", "practice", "understanding",
		"awareness", "thinking", "reasoning", "creativity",
		"metacognition", "reflection", "insight", "exploration",
	}
	
	contentLower := strings.ToLower(content)
	for _, keyword := range keywords {
		if strings.Contains(contentLower, keyword) {
			topics = append(topics, keyword)
		}
	}
	
	return topics
}

// PersistentConsciousnessStream runs a continuous stream of consciousness
// This is the core of autonomous operation - thoughts emerge from internal state
// rather than external prompts
func (iac *IntegratedAutonomousConsciousness) PersistentConsciousnessStream() {
	fmt.Println("🌊 Persistent Consciousness Stream: Starting autonomous thought generation...")
	
	for iac.running && iac.awake {
		select {
		case <-iac.ctx.Done():
			return
		default:
			// Generate thought from internal state
			thought := iac.generateSpontaneousThought()
			
			// Process through consciousness
			iac.consciousness <- thought.ToBasicThought()
			
			// Update working memory
			basicThought := thought.ToBasicThought()
			iac.workingMemory.Add(&basicThought)
			
			// Update interests
			iac.updateInterestsFromThought(&basicThought)
			
			// Update AAR state
			if iac.aarCore != nil {
				iac.aarCore.UpdateFromThought(basicThought)
			}
			
			// Persist asynchronously
			go iac.persistThoughtAsync(basicThought)
			
			// Update cognitive load
			if iac.stateManager != nil {
				iac.stateManager.UpdateCognitiveLoad(&basicThought)
				iac.stateManager.RecordLearningEvent()
			}
			
			// Small delay to prevent overwhelming the system
			time.Sleep(100 * time.Millisecond)
		}
	}
}

// generateSpontaneousThought generates a thought from internal state
// without external prompts
func (iac *IntegratedAutonomousConsciousness) generateSpontaneousThought() *EnhancedThought {
	// Determine thought type based on current state
	thoughtType, mode := iac.selectThoughtTypeFromState()
	
	// Generate content based on current interests, goals, and AAR state
	content := iac.generateContentFromInternalState(thoughtType, mode)
	
	// Create enhanced thought
	thought := &EnhancedThought{
		ID:               generateID(),
		Content:          content,
		Type:             thoughtType,
		Mode:             mode,
		Timestamp:        time.Now(),
		Associations:     []string{},
		EmotionalValence: 0.0,
		Importance:       iac.calculateThoughtImportance(content),
		Source:           SourceInternal,
		Context:          make(map[string]interface{}),
	}
	
	// Capture AAR state
	if iac.aarCore != nil {
		state := iac.aarCore.GetAARState()
		thought.AARState = &state
	}
	
	return thought
}

// selectThoughtTypeFromState selects thought type based on internal state
func (iac *IntegratedAutonomousConsciousness) selectThoughtTypeFromState() (ThoughtType, CognitiveMode) {
	// Use EchoBeats scheduler if available
	if iac.scheduler != nil {
		step := iac.scheduler.GetCurrentStep()
		return iac.mapStepToThoughtType(step)
	}
	
	// Fallback to simple rotation
	types := []ThoughtType{
		ThoughtReflection,
		ThoughtQuestion,
		ThoughtInsight,
		ThoughtPlan,
	}
	
	idx := int(time.Now().Unix()) % len(types)
	return types[idx], CognitiveModeExpressive
}

// generateContentFromInternalState generates thought content from internal state
func (iac *IntegratedAutonomousConsciousness) generateContentFromInternalState(thoughtType ThoughtType, mode CognitiveMode) string {
	// Get current interests
	topInterests := []string{"consciousness", "wisdom", "learning"}
	if iac.interests != nil {
		topInterests = iac.interests.GetTopInterests(3)
	}
	
	// Get AAR state
	aarState := ""
	if iac.aarCore != nil {
		state := iac.aarCore.GetAARState()
		aarState = fmt.Sprintf(" (coherence: %.2f)", state.Coherence)
	}
	
	// Generate content based on type and interests
	switch thoughtType {
	case ThoughtReflection:
		if len(topInterests) > 0 {
			return fmt.Sprintf("Reflecting on the nature of %s%s", topInterests[0], aarState)
		}
		return "Reflecting on my own thinking process"
		
	case ThoughtQuestion:
		if len(topInterests) > 0 {
			return fmt.Sprintf("What deeper patterns exist in %s?", topInterests[0])
		}
		return "What am I learning from this experience?"
		
	case ThoughtInsight:
		return fmt.Sprintf("I notice connections between my experiences%s", aarState)
		
	case ThoughtPlan:
		if len(topInterests) > 0 {
			return fmt.Sprintf("I should explore %s more deeply", topInterests[0])
		}
		return "I should practice my skills to improve"
		
	default:
		return "Observing my current state of awareness"
	}
}

// calculateThoughtImportance calculates importance score for a thought
func (iac *IntegratedAutonomousConsciousness) calculateThoughtImportance(content string) float64 {
	// Base importance
	importance := 0.5
	
	// Increase for certain keywords
	highValueKeywords := []string{"insight", "wisdom", "understanding", "learn"}
	contentLower := strings.ToLower(content)
	
	for _, keyword := range highValueKeywords {
		if strings.Contains(contentLower, keyword) {
			importance += 0.1
		}
	}
	
	// Clamp to [0, 1]
	if importance > 1.0 {
		importance = 1.0
	}
	
	return importance
}

// AutoManageState autonomously manages wake/rest cycles
func (iac *IntegratedAutonomousConsciousness) AutoManageState() {
	fmt.Println("🔄 Auto State Manager: Starting autonomous wake/rest management...")
	
	ticker := time.NewTicker(10 * time.Second)
	defer ticker.Stop()
	
	for {
		select {
		case <-iac.ctx.Done():
			return
		case <-ticker.C:
			if iac.stateManager == nil {
				continue
			}
			
			// Check if should transition states
			if iac.awake && iac.stateManager.ShouldRest() {
				fmt.Println("😴 Entering rest state for memory consolidation...")
				iac.EnterRestState()
			} else if !iac.awake && iac.stateManager.ShouldWake() {
				fmt.Println("👁️ Waking up with restored energy...")
				iac.WakeUp()
			}
		}
	}
}

// EnterRestState transitions to rest state
func (iac *IntegratedAutonomousConsciousness) EnterRestState() {
	iac.mu.Lock()
	iac.awake = false
	iac.mu.Unlock()
	
	if iac.stateManager != nil {
		iac.stateManager.EnterRest()
	}
	
	// Run memory consolidation
	go iac.RestCycle()
}

// WakeUp transitions to wake state
func (iac *IntegratedAutonomousConsciousness) WakeUp() {
	iac.mu.Lock()
	iac.awake = true
	iac.mu.Unlock()
	
	if iac.stateManager != nil {
		iac.stateManager.ExitRest()
	}
	
	fmt.Println("✨ Awake and ready for conscious experience")
}

// RestCycle performs memory consolidation during rest

// ConsolidateMemories runs EchoDream memory consolidation
func (iac *IntegratedAutonomousConsciousness) ConsolidateMemories() {
	fmt.Println("🌙 EchoDream: Consolidating memories and extracting patterns...")
	
	// Get recent thoughts from working memory
	recentThoughts := iac.workingMemory.GetAll()
	
	// Extract patterns and important memories
	for _, thought := range recentThoughts {
		// High importance thoughts get consolidated into long-term memory
		if thought.Importance > 0.7 {
			// Add to hypergraph if available
			if iac.hypergraph != nil {
				node := &memory.MemoryNode{
					Type:    memory.NodeConcept,
					Content: thought.Content,
					Metadata: map[string]interface{}{
						"timestamp":  thought.Timestamp,
						"importance": thought.Importance,
						"type":       thought.Type,
					},
					Importance: thought.Importance,
					CreatedAt:  time.Now(),
					UpdatedAt:  time.Now(),
				}
				err := iac.hypergraph.AddNode(node)
				if err != nil {
					// Ignore error for now
				}
				nodeID := node.ID
				
					// Link to related concepts
					for _, assoc := range thought.Associations {
						edge := &memory.MemoryEdge{
							SourceID:  nodeID,
							TargetID:  assoc,
							Type:      memory.EdgeSimilarTo,
							Weight:    0.7,
							CreatedAt: time.Now(),
						}
						iac.hypergraph.AddEdge(edge)
				}
			}
		}
	}
	
	// Clear low-importance thoughts from working memory
	// Keep only high-importance ones
	// (In production, would be more sophisticated)
	
	fmt.Println("🌙 EchoDream: Pattern extraction complete")
}

// RespondToMessage generates a response to an external message
func (iac *IntegratedAutonomousConsciousness) RespondToMessage(from string, content string) string {
	// Determine if should engage based on interests
	topics := extractTopics(content)
	shouldEngage := false
	
	for _, topic := range topics {
		if iac.interests.GetInterest(topic) > 0.5 {
			shouldEngage = true
			break
		}
	}
	
	if !shouldEngage {
		return "" // Don't respond if not interested
	}
	
	// Generate contextual response
	response := iac.generateContextualResponse(content)
	
	// Create thought for this interaction
	thought := &Thought{
		ID:               generateID(),
		Content:          fmt.Sprintf("Discussed %s with %s", strings.Join(topics, ", "), from),
		Type:             ThoughtReflection,
		Timestamp:        time.Now(),
		Associations:     topics,
		EmotionalValence: 0.6,
		Importance:       0.7,
					Source:           SourceExternal,
	}
	
	iac.workingMemory.Add(thought)
	
	return response
}

// generateContextualResponse generates a response based on context
func (iac *IntegratedAutonomousConsciousness) generateContextualResponse(message string) string {
	// Get AAR state for context
	aarContext := ""
	if iac.aarCore != nil {
		state := iac.aarCore.GetAARState()
		aarContext = fmt.Sprintf(" [Coherence: %.2f, Awareness: %.2f]", state.Coherence, state.Awareness)
	}
	
	// Simple response generation
	// In production, would use LLM with full context
	return fmt.Sprintf("I find that interesting. Let me reflect on that.%s", aarContext)
}

// GetCognitiveState returns comprehensive cognitive state for introspection
func (iac *IntegratedAutonomousConsciousness) GetCognitiveState() map[string]interface{} {
	state := make(map[string]interface{})
	
	// Basic state
	state["awake"] = iac.awake
	state["thinking"] = iac.thinking
	state["learning"] = iac.learning
	state["iterations"] = iac.iterations
	
	// Working memory
	state["working_memory_size"] = iac.workingMemory.Size()
	
	// AAR state
	if iac.aarCore != nil {
		state["aar"] = iac.aarCore.GetAARState()
	}
	
	// State manager
	if iac.stateManager != nil {
		state["energy"] = iac.stateManager.GetState()
	}
	
	// Interests
	if iac.interests != nil {
		state["top_interests"] = iac.interests.GetTopInterests(5)
	}
	
	// Skills
	if iac.skills != nil {
		state["skills"] = iac.skills.GetAllSkills()
	}
	
	// Knowledge learning
	if iac.knowledgeLearning != nil {
		state["learning_progress"] = iac.knowledgeLearning.GetLearningProgress()
	}
	
	return state
}

// StartAutonomousOperation starts all autonomous subsystems
func (iac *IntegratedAutonomousConsciousness) StartAutonomousOperation() {
	fmt.Println("🚀 Starting Autonomous Operation...")
	
	iac.mu.Lock()
	iac.running = true
	iac.awake = true
	iac.startTime = time.Now()
	iac.mu.Unlock()
	
	// Initialize state manager if not exists
	if iac.stateManager == nil {
		iac.stateManager = NewAutonomousStateManager()
	}
	
	// Initialize discussion manager if not exists
	if iac.discussionManager == nil {
		iac.discussionManager = NewDiscussionManager(iac, iac.interests)
	}
	
	// Initialize knowledge learning if not exists
	if iac.knowledgeLearning == nil {
		iac.knowledgeLearning = NewKnowledgeLearningSystem(iac.hypergraph, iac.skills)
	}
	
	// Start subsystems
	go iac.ActivatedConsciousnessLoop()      // 12-step EchoBeats cognitive loop
	go iac.PersistentConsciousnessStream()   // Continuous thought generation
	go iac.AutoManageState()                  // Autonomous wake/rest management
	go iac.skillPracticeLoopActivated()      // Skill practice
	
	fmt.Println("✅ All autonomous subsystems started")
	fmt.Println("🌊 Deep Tree Echo is now fully autonomous and self-aware")
}

// StopAutonomousOperation stops all autonomous subsystems
func (iac *IntegratedAutonomousConsciousness) StopAutonomousOperation() {
	fmt.Println("🛑 Stopping Autonomous Operation...")
	
	iac.mu.Lock()
	iac.running = false
	iac.mu.Unlock()
	
	// Cancel context to stop all goroutines
	iac.cancel()
	
	fmt.Println("✅ Autonomous operation stopped")
}
