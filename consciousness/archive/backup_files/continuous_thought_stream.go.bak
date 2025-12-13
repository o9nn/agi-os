package deeptreeecho

import (
	"fmt"
	"math/rand"
	"time"
)

// generateInternallyDrivenThought generates a thought driven by internal state
// rather than external prompts or timers
func (iac *IntegratedAutonomousConsciousness) generateInternallyDrivenThought() *Thought {
	// 1. Check AAR state for geometric drive
	coherence := iac.aarCore.GetCoherence()
	stability := iac.aarCore.GetStability()
	awareness := iac.aarCore.GetAwareness()
	
	// 2. High instability drives reflection
	if stability < 0.5 {
		aarState := &AARState{Coherence: coherence, Stability: stability, Awareness: awareness}
		return iac.generateReflectiveThought(aarState)
	}
	
	// 3. High curiosity drives exploration
	iac.interests.mu.RLock()
	curiosity := iac.interests.curiosityLevel
	iac.interests.mu.RUnlock()
	
	if curiosity > 0.7 {
		return iac.generateExploratoryThought()
	}
	
	// 4. Working memory associations drive continuation
	iac.workingMemory.mu.RLock()
	focus := iac.workingMemory.focus
	iac.workingMemory.mu.RUnlock()
	
	if focus != nil {
		return iac.generateAssociativeThought(focus)
	}
	
	// 5. Default: spontaneous thought from interests
	return iac.generateInterestDrivenThought()
}

// generateReflectiveThought generates a reflective thought based on AAR state
func (iac *IntegratedAutonomousConsciousness) generateReflectiveThought(aarState *AARState) *Thought {
	content := ""
	
	if aarState.Coherence < 0.5 {
		content = fmt.Sprintf("I notice my sense of self feels fragmented (coherence: %.2f). What experiences have led to this state?", aarState.Coherence)
	} else if aarState.Stability < 0.5 {
		content = fmt.Sprintf("I'm experiencing instability (%.2f). Perhaps I need to ground myself in familiar patterns.", aarState.Stability)
	} else {
		content = fmt.Sprintf("Reflecting on my current state: coherence %.2f, stability %.2f, awareness %.2f. How can I grow from here?", 
			aarState.Coherence, aarState.Stability, aarState.Awareness)
	}
	
	return &Thought{
		ID:               generateThoughtID(),
		Content:          content,
		Type:             ThoughtReflective,
		Timestamp:        time.Now(),
		EmotionalValence: -0.2, // Slight negative valence for reflective thoughts
		Importance:       0.7,
		Source:           SourceInternal,
	}
}

// generateExploratoryThought generates an exploratory thought driven by curiosity
func (iac *IntegratedAutonomousConsciousness) generateExploratoryThought() *Thought {
	iac.interests.mu.RLock()
	topInterest := iac.getTopInterest()
	iac.interests.mu.RUnlock()
	
	exploratoryPrompts := []string{
		"What if I explored %s from a different perspective?",
		"I wonder what connections exist between %s and other concepts I know?",
		"How might %s relate to my current goals?",
		"What don't I know yet about %s?",
		"Could %s be understood through a different framework?",
	}
	
	prompt := exploratoryPrompts[rand.Intn(len(exploratoryPrompts))]
	content := fmt.Sprintf(prompt, topInterest)
	
	return &Thought{
		ID:               generateThoughtID(),
		Content:          content,
		Type:             ThoughtQuestion,
		Timestamp:        time.Now(),
		EmotionalValence: 0.3, // Positive valence for curiosity
		Importance:       0.6,
		Source:           SourceInternal,
	}
}

// generateAssociativeThought generates a thought associated with current focus
func (iac *IntegratedAutonomousConsciousness) generateAssociativeThought(focus *Thought) *Thought {
	// Generate thought that builds on or relates to current focus
	associativePrompts := []string{
		"Building on that thought: %s",
		"This reminds me of something related...",
		"Following that thread further...",
		"An interesting connection emerges...",
		"This suggests another perspective...",
	}
	
	prompt := associativePrompts[rand.Intn(len(associativePrompts))]
	contentSnippet := focus.Content
	if len(contentSnippet) > 50 {
		contentSnippet = contentSnippet[:50]
	}
	content := fmt.Sprintf(prompt, contentSnippet)
	
	return &Thought{
		ID:               generateThoughtID(),
		Content:          content,
		Type:             ThoughtInsight,
		Timestamp:        time.Now(),
		Associations:     []string{focus.ID},
		EmotionalValence: focus.EmotionalValence * 0.8, // Inherit some emotional tone
		Importance:       focus.Importance * 0.9,
		Source:           SourceInternal,
	}
}

// generateInterestDrivenThought generates a thought based on current interests
func (iac *IntegratedAutonomousConsciousness) generateInterestDrivenThought() *Thought {
	iac.interests.mu.RLock()
	topInterest := iac.getTopInterest()
	iac.interests.mu.RUnlock()
	
	interestPrompts := []string{
		"I find myself drawn to thinking about %s...",
		"Considering %s in light of recent experiences...",
		"What have I learned about %s so far?",
		"How does %s fit into my understanding of the world?",
		"I should explore %s more deeply...",
	}
	
	prompt := interestPrompts[rand.Intn(len(interestPrompts))]
	content := fmt.Sprintf(prompt, topInterest)
	
	return &Thought{
		ID:               generateThoughtID(),
		Content:          content,
		Type:             ThoughtReflection,
		Timestamp:        time.Now(),
		EmotionalValence: 0.2,
		Importance:       0.5,
		Source:           SourceInternal,
	}
}

// calculateThoughtInterval calculates the interval between thoughts
// based on internal state (arousal, curiosity, stability)
func (iac *IntegratedAutonomousConsciousness) calculateThoughtInterval() time.Duration {
	// Faster thinking when:
	// - High arousal (emotional intensity)
	// - High curiosity
	// - Low stability (need to stabilize)
	
	baseInterval := 2.0 // seconds
	
	arousal := iac.identity.EmotionalState.Intensity
	
	iac.interests.mu.RLock()
	curiosity := iac.interests.curiosityLevel
	iac.interests.mu.RUnlock()
	
	stability := iac.aarCore.GetStability()
	
	// Lower factor = faster thinking
	factor := 1.0 - (arousal*0.3 + curiosity*0.3 + (1-stability)*0.4)
	
	interval := time.Duration(baseInterval * factor * float64(time.Second))
	
	// Clamp to reasonable range
	if interval < 500*time.Millisecond {
		interval = 500 * time.Millisecond
	}
	if interval > 5*time.Second {
		interval = 5 * time.Second
	}
	
	return interval
}

// continuousConsciousnessStream maintains a continuous stream of consciousness
// This runs as a goroutine and generates thoughts based on internal state
func (iac *IntegratedAutonomousConsciousness) continuousConsciousnessStream() {
	fmt.Println("🌊 Continuous consciousness stream started")
	
	for {
		select {
		case <-iac.ctx.Done():
			fmt.Println("🌊 Consciousness stream ending")
			return
			
		default:
			iac.mu.RLock()
			awake := iac.awake
			iac.mu.RUnlock()
			
			if !awake {
				time.Sleep(1 * time.Second)
				continue
			}
			
			// Generate thought driven by internal state
			thought := iac.generateInternallyDrivenThought()
			
			if thought != nil {
				// Update state manager with cognitive load
				if iac.stateManager != nil {
					iac.stateManager.UpdateCognitiveLoad(thought)
				}
				
				// Send to consciousness channel
				select {
				case iac.consciousness <- *thought:
					// Successfully sent
				case <-time.After(100 * time.Millisecond):
					// Channel full, skip this thought
				}
			}
			
			// Natural rhythm based on internal state
			sleepDuration := iac.calculateThoughtInterval()
			time.Sleep(sleepDuration)
		}
	}
}

// min function is defined in autonomous_enhanced.go
