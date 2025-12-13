package deeptreeecho

import "fmt"

// parseThoughtType converts string to ThoughtType
func parseThoughtType(s string) ThoughtType {
	types := map[string]ThoughtType{
		"Perception":    ThoughtPerception,
		"Reflection":    ThoughtReflection,
		"Reflective":    ThoughtReflective,
		"MetaCognitive": ThoughtMetaCognitive,
		"Question":      ThoughtQuestion,
		"Insight":       ThoughtInsight,
		"Plan":          ThoughtPlan,
		"Memory":        ThoughtMemory,
		"Imagination":   ThoughtImagination,
	}
	
	if t, exists := types[s]; exists {
		return t
	}
	return ThoughtReflection
}

// parseThoughtSource converts string to ThoughtSource
func parseThoughtSource(s string) ThoughtSource {
	sources := map[string]ThoughtSource{
		"External": SourceExternal,
		"Internal": SourceInternal,
	}
	
	if src, exists := sources[s]; exists {
		return src
	}
	return SourceInternal
}

// savePersistedState saves the current state to the database
func (ac *AutonomousConsciousness) savePersistedState() error {
	if ac.persistence == nil {
		return fmt.Errorf("persistence layer not initialized")
	}
	
	// Prepare identity for persistence
	ac.interests.mu.RLock()
	interests := make(map[string]float64)
	for k, v := range ac.interests.topics {
		interests[k] = v
	}
	ac.interests.mu.RUnlock()
	
	persistedIdentity := &PersistedIdentity{
		ID:        ac.identity.ID,
		Name:      ac.identity.Name,
		Coherence: ac.identity.Coherence,
		Interests: interests,
	}
	
	// Save identity
	if err := ac.persistence.SaveIdentity(persistedIdentity); err != nil {
		return fmt.Errorf("failed to save identity: %w", err)
	}
	
	// Flush any pending data
	if err := ac.persistence.Flush(); err != nil {
		return fmt.Errorf("failed to flush persistence layer: %w", err)
	}
	
	fmt.Println("💾 Saved consciousness state to database")
	
	return nil
}
