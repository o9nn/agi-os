package deeptreeecho

import (
	"time"
)

// CognitiveMode is already defined in consciousness_activation.go

// EnhancedThought extends the base Thought with fields needed for integrated consciousness
type EnhancedThought struct {
	ID               string
	Content          string
	Type             ThoughtType
	Mode             CognitiveMode
	Timestamp        time.Time
	Associations     []string
	EmotionalValence float64
	Importance       float64
	Source           ThoughtSource
	Context          map[string]interface{}
	AARState         *AARState
}

// ToBasicThought converts EnhancedThought to basic Thought for compatibility
func (et *EnhancedThought) ToBasicThought() Thought {
	return Thought{
		ID:               et.ID,
		Content:          et.Content,
		Type:             et.Type,
		Timestamp:        et.Timestamp,
		Associations:     et.Associations,
		EmotionalValence: et.EmotionalValence,
		Importance:       et.Importance,
		Source:           et.Source,
	}
}

// FromBasicThought creates EnhancedThought from basic Thought
func FromBasicThought(t Thought, mode CognitiveMode) *EnhancedThought {
	return &EnhancedThought{
		ID:               t.ID,
		Content:          t.Content,
		Type:             t.Type,
		Mode:             mode,
		Timestamp:        t.Timestamp,
		Associations:     t.Associations,
		EmotionalValence: t.EmotionalValence,
		Importance:       t.Importance,
		Source:           t.Source,
		Context:          make(map[string]interface{}),
	}
}

// WorkingMemory methods for enhanced functionality
func (wm *WorkingMemory) Add(thought *Thought) {
	wm.mu.Lock()
	defer wm.mu.Unlock()
	
	// Add to buffer
	wm.buffer = append(wm.buffer, thought)
	
	// Maintain capacity limit (FIFO)
	if len(wm.buffer) > wm.capacity {
		wm.buffer = wm.buffer[1:]
	}
	
	// Update focus to most recent high-importance thought
	if thought.Importance > 0.7 {
		wm.focus = thought
	}
}

// AddThought is an alias for Add for compatibility
func (wm *WorkingMemory) AddThought(thought *Thought) {
	wm.Add(thought)
}

// focusItem sets the current focus of working memory
func (wm *WorkingMemory) focusItem(thought *Thought) {
	wm.mu.Lock()
	defer wm.mu.Unlock()
	wm.focus = thought
}

// GetFocus is now defined in autonomous.go to avoid duplication

// GetRecent returns the n most recent thoughts
func (wm *WorkingMemory) GetRecent(n int) []*Thought {
	wm.mu.RLock()
	defer wm.mu.RUnlock()
	
	if len(wm.buffer) == 0 {
		return []*Thought{}
	}
	
	start := len(wm.buffer) - n
	if start < 0 {
		start = 0
	}
	
	// Return copy to avoid race conditions
	recent := make([]*Thought, len(wm.buffer)-start)
	copy(recent, wm.buffer[start:])
	return recent
}

// GetAll returns all thoughts in working memory
func (wm *WorkingMemory) GetAll() []*Thought {
	wm.mu.RLock()
	defer wm.mu.RUnlock()
	
	all := make([]*Thought, len(wm.buffer))
	copy(all, wm.buffer)
	return all
}

// Clear clears the working memory buffer
func (wm *WorkingMemory) Clear() {
	wm.mu.Lock()
	defer wm.mu.Unlock()
	wm.buffer = make([]*Thought, 0)
	wm.focus = nil
}

// Size returns the current size of the buffer
func (wm *WorkingMemory) Size() int {
	wm.mu.RLock()
	defer wm.mu.RUnlock()
	return len(wm.buffer)
}


// InterestSystem methods for enhanced functionality
func (is *InterestSystem) GetInterest(topic string) float64 {
	is.mu.RLock()
	defer is.mu.RUnlock()
	
	if score, exists := is.topics[topic]; exists {
		return score
	}
	return 0.0
}

// UpdateInterest updates the interest score for a topic
func (is *InterestSystem) UpdateInterest(topic string, delta float64) {
	is.mu.Lock()
	defer is.mu.Unlock()
	
	current := is.topics[topic]
	current += delta
	
	// Clamp to [0, 1]
	if current > 1.0 {
		current = 1.0
	}
	if current < 0.0 {
		current = 0.0
	}
	
	is.topics[topic] = current
}

// GetTopInterests returns the top N interests
func (is *InterestSystem) GetTopInterests(n int) []string {
	is.mu.RLock()
	defer is.mu.RUnlock()
	
	// Simple implementation - return first n topics
	// In production, would sort by score
	topics := make([]string, 0, n)
	count := 0
	for topic := range is.topics {
		if count >= n {
			break
		}
		topics = append(topics, topic)
		count++
	}
	return topics
}

// generateID is already defined in identity.go
