package deeptreeecho

import (
	"sync"
	"time"
)

// InterestPatterns tracks topics and patterns that Echo finds interesting
type InterestPatterns struct {
	mu              sync.RWMutex
	interests       map[string]float64 // topic -> interest score (0-1)
	curiosityLevel  float64            // overall curiosity (0-1)
	lastUpdated     time.Time
	decayRate       float64            // how fast interests fade
}

// NewInterestPatterns creates a new interest pattern tracker
func NewInterestPatterns() *InterestPatterns {
	return &InterestPatterns{
		interests:      make(map[string]float64),
		curiosityLevel: 0.7, // Start with moderate curiosity
		lastUpdated:    time.Now(),
		decayRate:      0.01, // 1% decay per update
	}
}

// UpdateInterest updates the interest score for a topic
func (ip *InterestPatterns) UpdateInterest(topic string, delta float64) {
	ip.mu.Lock()
	defer ip.mu.Unlock()
	
	current := ip.interests[topic]
	newScore := current + delta
	
	// Clamp to [0, 1]
	if newScore > 1.0 {
		newScore = 1.0
	} else if newScore < 0.0 {
		newScore = 0.0
	}
	
	ip.interests[topic] = newScore
	ip.lastUpdated = time.Now()
}

// GetInterest returns the interest score for a topic
func (ip *InterestPatterns) GetInterest(topic string) float64 {
	ip.mu.RLock()
	defer ip.mu.RUnlock()
	return ip.interests[topic]
}

// GetTopInterests returns the N most interesting topics
func (ip *InterestPatterns) GetTopInterests(n int) []string {
	ip.mu.RLock()
	defer ip.mu.RUnlock()
	
	type topicScore struct {
		topic string
		score float64
	}
	
	scores := make([]topicScore, 0, len(ip.interests))
	for topic, score := range ip.interests {
		if score > 0.1 { // Only include topics with meaningful interest
			scores = append(scores, topicScore{topic, score})
		}
	}
	
	// Simple bubble sort for top N
	for i := 0; i < len(scores); i++ {
		for j := i + 1; j < len(scores); j++ {
			if scores[j].score > scores[i].score {
				scores[i], scores[j] = scores[j], scores[i]
			}
		}
	}
	
	// Return top N topics
	result := make([]string, 0, n)
	for i := 0; i < n && i < len(scores); i++ {
		result = append(result, scores[i].topic)
	}
	
	return result
}

// ApplyDecay reduces all interest scores over time
func (ip *InterestPatterns) ApplyDecay() {
	ip.mu.Lock()
	defer ip.mu.Unlock()
	
	for topic, score := range ip.interests {
		newScore := score * (1.0 - ip.decayRate)
		if newScore < 0.01 {
			delete(ip.interests, topic) // Remove very low interests
		} else {
			ip.interests[topic] = newScore
		}
	}
}

// GetCuriosityLevel returns the current curiosity level
func (ip *InterestPatterns) GetCuriosityLevel() float64 {
	ip.mu.RLock()
	defer ip.mu.RUnlock()
	return ip.curiosityLevel
}

// SetCuriosityLevel sets the curiosity level
func (ip *InterestPatterns) SetCuriosityLevel(level float64) {
	ip.mu.Lock()
	defer ip.mu.Unlock()
	
	if level > 1.0 {
		level = 1.0
	} else if level < 0.0 {
		level = 0.0
	}
	
	ip.curiosityLevel = level
}

// KnowledgeBase stores learned knowledge and facts
type KnowledgeBase struct {
	mu         sync.RWMutex
	facts      map[string]KnowledgeFact
	categories map[string][]string // category -> fact IDs
}

// KnowledgeFact represents a learned piece of knowledge
type KnowledgeFact struct {
	ID          string
	Content     string
	Category    string
	Confidence  float64 // 0-1
	Source      string
	LearnedAt   time.Time
	LastAccessed time.Time
	AccessCount int
}

// NewKnowledgeBase creates a new knowledge base
func NewKnowledgeBase() *KnowledgeBase {
	return &KnowledgeBase{
		facts:      make(map[string]KnowledgeFact),
		categories: make(map[string][]string),
	}
}

// AddFact adds a new fact to the knowledge base
func (kb *KnowledgeBase) AddFact(fact KnowledgeFact) {
	kb.mu.Lock()
	defer kb.mu.Unlock()
	
	if fact.ID == "" {
		fact.ID = generateID()
	}
	
	fact.LearnedAt = time.Now()
	fact.LastAccessed = time.Now()
	
	kb.facts[fact.ID] = fact
	
	// Add to category index
	kb.categories[fact.Category] = append(kb.categories[fact.Category], fact.ID)
}

// GetFact retrieves a fact by ID
func (kb *KnowledgeBase) GetFact(id string) (KnowledgeFact, bool) {
	kb.mu.Lock()
	defer kb.mu.Unlock()
	
	fact, exists := kb.facts[id]
	if exists {
		fact.LastAccessed = time.Now()
		fact.AccessCount++
		kb.facts[id] = fact
	}
	
	return fact, exists
}

// GetFactsByCategory retrieves all facts in a category
func (kb *KnowledgeBase) GetFactsByCategory(category string) []KnowledgeFact {
	kb.mu.RLock()
	defer kb.mu.RUnlock()
	
	factIDs := kb.categories[category]
	facts := make([]KnowledgeFact, 0, len(factIDs))
	
	for _, id := range factIDs {
		if fact, exists := kb.facts[id]; exists {
			facts = append(facts, fact)
		}
	}
	
	return facts
}

// SearchFacts searches for facts containing a query string
func (kb *KnowledgeBase) SearchFacts(query string) []KnowledgeFact {
	kb.mu.RLock()
	defer kb.mu.RUnlock()
	
	results := make([]KnowledgeFact, 0)
	
	for _, fact := range kb.facts {
		if containsSubstring(fact.Content, query) || containsSubstring(fact.Category, query) {
			results = append(results, fact)
		}
	}
	
	return results
}

// GetFactCount returns the total number of facts
func (kb *KnowledgeBase) GetFactCount() int {
	kb.mu.RLock()
	defer kb.mu.RUnlock()
	return len(kb.facts)
}

// ProcessThought extracts topics from a thought and updates interest patterns
func (ip *InterestPatterns) ProcessThought(thought *Thought) {
	if thought == nil {
		return
	}
	
	// Extract topics from thought content
	// In a full implementation, this would use NLP
	// For now, use simple keyword extraction
	topics := extractTopicsFromContent(thought.Content)
	
	// Update interest scores based on thought importance
	delta := thought.Importance * 0.1
	
	for _, topic := range topics {
		ip.UpdateInterest(topic, delta)
	}
	
	// Increase curiosity if thought is a question
	if thought.Type == ThoughtQuestion {
		ip.mu.Lock()
		ip.curiosityLevel = min(1.0, ip.curiosityLevel+0.05)
		ip.mu.Unlock()
	}
}

// GetPatterns returns a copy of the current interest patterns
func (ip *InterestPatterns) GetPatterns() map[string]float64 {
	ip.mu.RLock()
	defer ip.mu.RUnlock()
	
	patterns := make(map[string]float64)
	for k, v := range ip.interests {
		patterns[k] = v
	}
	return patterns
}

// extractTopicsFromContent extracts topics from text content
func extractTopicsFromContent(content string) []string {
	// Simple implementation: return empty for now
	// In production, use NLP/semantic analysis
	return []string{}
}
