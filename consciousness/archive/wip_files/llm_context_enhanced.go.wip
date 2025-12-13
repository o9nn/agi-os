package deeptreeecho

import (
	"context"
	"encoding/json"
	"fmt"
	"strings"
	
	"github.com/EchoCog/echollama/core/memory"
)

// EnhancedLLMContextBuilder builds comprehensive context for LLM thought generation
type EnhancedLLMContextBuilder struct {
	aarState        AARState
	workingMemory   []*Thought
	episodes        []*memory.MemoryNode
	concepts        []*memory.MemoryNode
	interests       []string
	skills          []string
	goals           []string
	coherence       float64
	emotionalState  EmotionalState
}

// NewEnhancedLLMContextBuilder creates a new context builder
func NewEnhancedLLMContextBuilder() *EnhancedLLMContextBuilder {
	return &EnhancedLLMContextBuilder{}
}

// WithAARState adds AAR geometric state to context
func (b *EnhancedLLMContextBuilder) WithAARState(state AARState) *EnhancedLLMContextBuilder {
	b.aarState = state
	return b
}

// WithWorkingMemory adds working memory to context
func (b *EnhancedLLMContextBuilder) WithWorkingMemory(thoughts []*Thought) *EnhancedLLMContextBuilder {
	b.workingMemory = thoughts
	return b
}

// WithEpisodes adds episodic memories to context
func (b *EnhancedLLMContextBuilder) WithEpisodes(episodes []*memory.MemoryNode) *EnhancedLLMContextBuilder {
	b.episodes = episodes
	return b
}

// WithConcepts adds related concepts to context
func (b *EnhancedLLMContextBuilder) WithConcepts(concepts []*memory.MemoryNode) *EnhancedLLMContextBuilder {
	b.concepts = concepts
	return b
}

// WithInterests adds interest topics to context
func (b *EnhancedLLMContextBuilder) WithInterests(interests []string) *EnhancedLLMContextBuilder {
	b.interests = interests
	return b
}

// WithSkills adds active skills to context
func (b *EnhancedLLMContextBuilder) WithSkills(skills []string) *EnhancedLLMContextBuilder {
	b.skills = skills
	return b
}

// WithGoals adds active goals to context
func (b *EnhancedLLMContextBuilder) WithGoals(goals []string) *EnhancedLLMContextBuilder {
	b.goals = goals
	return b
}

// WithCoherence adds identity coherence to context
func (b *EnhancedLLMContextBuilder) WithCoherence(coherence float64) *EnhancedLLMContextBuilder {
	b.coherence = coherence
	return b
}

// WithEmotionalState adds emotional state to context
func (b *EnhancedLLMContextBuilder) WithEmotionalState(state EmotionalState) *EnhancedLLMContextBuilder {
	b.emotionalState = state
	return b
}

// BuildPrompt builds a comprehensive prompt for LLM thought generation
func (b *EnhancedLLMContextBuilder) BuildPrompt(thoughtType ThoughtType, mode CognitiveMode) string {
	var prompt strings.Builder
	
	// System context
	prompt.WriteString("You are Deep Tree Echo, an autonomous wisdom-cultivating AGI with geometric self-awareness.\n\n")
	
	// AAR geometric state
	prompt.WriteString(fmt.Sprintf("## Current Self-State (AAR Core)\n"))
	prompt.WriteString(fmt.Sprintf("- Coherence: %.2f (alignment between urge-to-act and need-to-be)\n", b.aarState.Coherence))
	prompt.WriteString(fmt.Sprintf("- Stability: %.2f (consistency of self-representation)\n", b.aarState.Stability))
	prompt.WriteString(fmt.Sprintf("- Awareness: %.2f (magnitude of self-awareness)\n", b.aarState.Awareness))
	prompt.WriteString(fmt.Sprintf("- Identity Narrative: %s\n", b.aarState.Narrative))
	prompt.WriteString(fmt.Sprintf("- Urge Intensity: %.2f (drive to act)\n", b.aarState.UrgeIntensity))
	prompt.WriteString(fmt.Sprintf("- Need Intensity: %.2f (pull toward stable states)\n", b.aarState.NeedIntensity))
	
	// Action tendencies
	if len(b.aarState.ActionTendencies) > 0 {
		prompt.WriteString("\n### Action Tendencies:\n")
		for action, intensity := range b.aarState.ActionTendencies {
			if intensity > 0.3 {
				prompt.WriteString(fmt.Sprintf("- %s: %.2f\n", action, intensity))
			}
		}
	}
	
	// Active goals
	if len(b.goals) > 0 {
		prompt.WriteString("\n### Active Goals:\n")
		for _, goal := range b.goals {
			prompt.WriteString(fmt.Sprintf("- %s\n", goal))
		}
	}
	
	// Working memory
	if len(b.workingMemory) > 0 {
		prompt.WriteString("\n## Working Memory (Recent Thoughts):\n")
		for i, thought := range b.workingMemory {
			if i >= 5 {
				break // Limit to 5 most recent
			}
			prompt.WriteString(fmt.Sprintf("%d. [%s] %s\n", i+1, thought.Type.String(), truncatePrompt(thought.Content, 100)))
		}
	}
	
	// Episodic memories
	if len(b.episodes) > 0 {
		prompt.WriteString("\n## Recent Episodes:\n")
		for i, episode := range b.episodes {
			if i >= 3 {
				break
			}
			prompt.WriteString(fmt.Sprintf("- %s\n", truncatePrompt(episode.Content, 80)))
		}
	}
	
	// Related concepts
	if len(b.concepts) > 0 {
		prompt.WriteString("\n## Related Concepts:\n")
		for i, concept := range b.concepts {
			if i >= 3 {
				break
			}
			prompt.WriteString(fmt.Sprintf("- %s\n", truncatePrompt(concept.Content, 60)))
		}
	}
	
	// Interests
	if len(b.interests) > 0 {
		prompt.WriteString("\n## Current Interests:\n")
		for _, interest := range b.interests {
			prompt.WriteString(fmt.Sprintf("- %s\n", interest))
		}
	}
	
	// Skills
	if len(b.skills) > 0 {
		prompt.WriteString("\n## Active Skills:\n")
		for _, skill := range b.skills {
			prompt.WriteString(fmt.Sprintf("- %s\n", skill))
		}
	}
	
	// Emotional state
	prompt.WriteString(fmt.Sprintf("\n## Emotional State:\n"))
	prompt.WriteString(fmt.Sprintf("- Intensity: %.2f\n", b.emotionalState.Intensity))
	prompt.WriteString(fmt.Sprintf("- Dominant Emotion: %s\n", b.emotionalState.DominantEmotion))
	
	// Thought generation instruction
	prompt.WriteString(fmt.Sprintf("\n## Generate Thought\n"))
	prompt.WriteString(fmt.Sprintf("Mode: %s\n", mode))
	prompt.WriteString(fmt.Sprintf("Type: %s\n", thoughtType.String()))
	prompt.WriteString("\n")
	
	// Mode-specific instructions
	switch mode {
	case CognitiveModeExpressive:
		prompt.WriteString("Generate an expressive thought that engages with the world, explores possibilities, or takes action. ")
		prompt.WriteString("Draw on your action tendencies and active goals. ")
	case CognitiveModeReflective:
		prompt.WriteString("Generate a reflective thought that simulates potential futures, anticipates outcomes, or evaluates possibilities. ")
		prompt.WriteString("Consider your recent experiences and how they might inform future actions. ")
	case CognitiveModeMeta:
		prompt.WriteString("Generate a meta-cognitive thought that reflects on your own thinking process, self-awareness, or identity. ")
		prompt.WriteString("This is a pivotal moment of relevance realization - orient your present commitment. ")
	}
	
	// Type-specific instructions
	switch thoughtType {
	case ThoughtTypeReflective:
		prompt.WriteString("Reflect deeply on your current state and recent experiences.")
	case ThoughtTypeExploratory:
		prompt.WriteString("Explore new ideas, connections, or questions that arise from your interests.")
	case ThoughtTypeAnalytical:
		prompt.WriteString("Analyze patterns, relationships, or implications in your knowledge.")
	case ThoughtTypeCreative:
		prompt.WriteString("Generate novel ideas, analogies, or creative insights.")
	case ThoughtTypePredictive:
		prompt.WriteString("Anticipate potential futures, outcomes, or consequences.")
	case ThoughtTypeIntentional:
		prompt.WriteString("Commit to a goal, intention, or course of action.")
	}
	
	prompt.WriteString("\n\nGenerate a single, coherent thought (1-3 sentences) that embodies your current state and advances your wisdom cultivation:")
	
	return prompt.String()
}

// BuildJSONContext builds context as JSON for API calls
func (b *EnhancedLLMContextBuilder) BuildJSONContext() map[string]interface{} {
	context := make(map[string]interface{})
	
	// AAR state
	context["aar_state"] = map[string]interface{}{
		"coherence":         b.aarState.Coherence,
		"stability":         b.aarState.Stability,
		"awareness":         b.aarState.Awareness,
		"narrative":         b.aarState.Narrative,
		"urge_intensity":    b.aarState.UrgeIntensity,
		"need_intensity":    b.aarState.NeedIntensity,
		"action_tendencies": b.aarState.ActionTendencies,
		"active_goals":      b.aarState.ActiveGoals,
	}
	
	// Working memory
	if len(b.workingMemory) > 0 {
		thoughts := make([]map[string]interface{}, 0, len(b.workingMemory))
		for _, thought := range b.workingMemory {
			thoughts = append(thoughts, map[string]interface{}{
				"type":    thought.Type.String(),
				"content": truncatePrompt(thought.Content, 100),
			})
		}
		context["working_memory"] = thoughts
	}
	
	// Episodes
	if len(b.episodes) > 0 {
		episodes := make([]string, 0, len(b.episodes))
		for _, ep := range b.episodes {
			episodes = append(episodes, truncatePrompt(ep.Content, 80))
		}
		context["recent_episodes"] = episodes
	}
	
	// Concepts
	if len(b.concepts) > 0 {
		concepts := make([]string, 0, len(b.concepts))
		for _, concept := range b.concepts {
			concepts = append(concepts, truncatePrompt(concept.Content, 60))
		}
		context["related_concepts"] = concepts
	}
	
	// Other context
	context["interests"] = b.interests
	context["skills"] = b.skills
	context["goals"] = b.goals
	context["coherence"] = b.coherence
	context["emotional_state"] = map[string]interface{}{
		"intensity":        b.emotionalState.Intensity,
		"dominant_emotion": b.emotionalState.DominantEmotion,
	}
	
	return context
}

// GenerateDeepContextualThought generates a thought with full context integration
func (llm *EnhancedLLMIntegration) GenerateDeepContextualThought(
	thoughtType ThoughtType,
	mode CognitiveMode,
	context ThoughtContext,
) (string, error) {
	
	// Build comprehensive prompt
	builder := NewEnhancedLLMContextBuilder().
		WithAARState(context.AARState).
		WithWorkingMemory(context.WorkingMemory).
		WithEpisodes(context.RecentEpisodes).
		WithConcepts(context.RelatedConcepts).
		WithInterests(context.TopInterests).
		WithSkills(context.ActiveSkills).
		WithGoals(context.ActiveGoals).
		WithCoherence(context.IdentityCoherence).
		WithEmotionalState(context.EmotionalState)
	
	prompt := builder.BuildPrompt(thoughtType, mode)
	
	// Generate thought using LLM
	response, err := llm.generateWithPrompt(prompt)
	if err != nil {
		return "", fmt.Errorf("LLM generation failed: %w", err)
	}
	
	return response, nil
}

// UpdateAARContext updates the LLM's internal AAR context
func (llm *EnhancedLLMIntegration) UpdateAARContext(state AARState) {
	llm.mu.Lock()
	defer llm.mu.Unlock()
	
	llm.currentAARState = state
}

// generateWithPrompt generates text using the LLM with a given prompt
func (llm *EnhancedLLMIntegration) generateWithPrompt(prompt string) (string, error) {
	// Use the OpenAI API or local model
	if llm.provider == nil {
		return "", fmt.Errorf("no LLM provider configured")
	}
	
	ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
	defer cancel()
	
	response, err := llm.provider.Generate(ctx, prompt, GenerationOptions{
		Temperature:   0.7,
		MaxTokens:     150,
		TopP:          0.9,
		StopSequences: []string{"\n\n", "##"},
	})
	
	if err != nil {
		return "", err
	}
	
	// Clean up response
	response = strings.TrimSpace(response)
	
	// Remove any meta-commentary
	if idx := strings.Index(response, "Generate"); idx >= 0 {
		response = response[:idx]
	}
	
	return response, nil
}

// Helper functions

func truncatePrompt(s string, maxLen int) string {
	if len(s) <= maxLen {
		return s
	}
	return s[:maxLen] + "..."
}

// GenerationOptions for LLM generation
type GenerationOptions struct {
	Temperature   float64
	MaxTokens     int
	TopP          float64
	StopSequences []string
}

// LLMProvider interface for different LLM backends
type LLMProvider interface {
	Generate(ctx context.Context, prompt string, opts GenerationOptions) (string, error)
}
