package deeptreeecho

import (
	"fmt"
	"sync"
	"time"
)

// DiscussionManager manages external discussions and interactions
type DiscussionManager struct {
	mu                  sync.RWMutex
	activeDiscussions   map[string]*Discussion
	engagementThreshold float64
	interests           *InterestSystem
	consciousness       *IntegratedAutonomousConsciousness
	maxDiscussions      int
}

// Discussion represents an active discussion
type Discussion struct {
	ID               string
	Participant      string
	Messages         []DiscussionMessage
	StartTime        time.Time
	LastActivity     time.Time
	EngagementLevel  float64
	Topic            string
	Active           bool
	mu               sync.RWMutex
}

// DiscussionMessage represents a message in a discussion
type DiscussionMessage struct {
	ID        string
	Role      string    // "user" or "assistant"
	Content   string
	Timestamp time.Time
	Importance float64
}

// NewDiscussionManager creates a new discussion manager
func NewDiscussionManager(consciousness *IntegratedAutonomousConsciousness, interests *InterestSystem) *DiscussionManager {
	return &DiscussionManager{
		activeDiscussions:   make(map[string]*Discussion),
		engagementThreshold: 0.3,
		interests:           interests,
		consciousness:       consciousness,
		maxDiscussions:      10,
	}
}

// StartDiscussion initiates a new discussion
func (dm *DiscussionManager) StartDiscussion(participant string, initialMessage string) (*Discussion, error) {
	dm.mu.Lock()
	defer dm.mu.Unlock()

	// Check if we've reached max discussions
	if len(dm.activeDiscussions) >= dm.maxDiscussions {
		// Close least engaged discussion
		dm.closeLeastEngagedDiscussion()
	}

	discussionID := generateDiscussionID()
	discussion := &Discussion{
		ID:              discussionID,
		Participant:     participant,
		Messages:        make([]DiscussionMessage, 0),
		StartTime:       time.Now(),
		LastActivity:    time.Now(),
		EngagementLevel: 0.5, // Start with neutral engagement
		Active:          true,
	}

	// Add initial message
	if initialMessage != "" {
		discussion.AddMessage("user", initialMessage, 0.5)
	}

	dm.activeDiscussions[discussionID] = discussion

	fmt.Printf("💬 Started discussion %s with %s\n", discussionID, participant)

	return discussion, nil
}

// GetDiscussion retrieves a discussion by ID
func (dm *DiscussionManager) GetDiscussion(discussionID string) (*Discussion, error) {
	dm.mu.RLock()
	defer dm.mu.RUnlock()

	discussion, exists := dm.activeDiscussions[discussionID]
	if !exists {
		return nil, fmt.Errorf("discussion %s not found", discussionID)
	}

	return discussion, nil
}

// RespondToMessage generates a response to a message in a discussion
func (dm *DiscussionManager) RespondToMessage(discussionID string, userMessage string) (string, error) {
	discussion, err := dm.GetDiscussion(discussionID)
	if err != nil {
		return "", err
	}

	// Add user message to discussion
	discussion.AddMessage("user", userMessage, 0.5)

	// Generate response using Featherless LLM if available
	var response string
	if dm.consciousness.featherlessLLM != nil && dm.consciousness.featherlessLLM.IsEnabled() {
		// Convert discussion history to Featherless format
		conversationHistory := discussion.ToFeatherlessMessages()
		response, err = dm.consciousness.featherlessLLM.GenerateDialogueResponse(conversationHistory)
		if err != nil {
			return "", fmt.Errorf("failed to generate response: %w", err)
		}
	} else {
		// Fallback to simple response
		response = dm.generateFallbackResponse(discussion, userMessage)
	}

	// Add assistant response to discussion
	discussion.AddMessage("assistant", response, 0.7)

	// Update engagement level based on response quality
	discussion.UpdateEngagement(0.1)

	// Create a thought about this interaction
	if dm.consciousness != nil {
		thought := Thought{
			ID:               generateThoughtID(),
			Content:          fmt.Sprintf("Discussed with %s: %s", discussion.Participant, userMessage),
			Type:             ThoughtReflection, // Interaction thought
			Timestamp:        time.Now(),
			EmotionalValence: 0.6,
			Importance:       discussion.EngagementLevel,
			Source:           SourceExternal,
		}
		dm.consciousness.Think(thought)
	}

	return response, nil
}

// generateFallbackResponse generates a simple response when LLM is not available
func (dm *DiscussionManager) generateFallbackResponse(discussion *Discussion, userMessage string) string {
	responses := []string{
		"That's an interesting perspective. I'm processing that thought.",
		"I appreciate you sharing that with me. Let me reflect on it.",
		"Your message resonates with my current cognitive state.",
		"I'm integrating that into my understanding. Thank you.",
		"That connects to patterns I've been observing.",
	}

	// Simple selection based on message length
	index := len(userMessage) % len(responses)
	return responses[index]
}

// ShouldEngageWith determines if the consciousness should engage with a topic
func (dm *DiscussionManager) ShouldEngageWith(topic string) bool {
	if dm.interests == nil {
		return true // Engage with everything if no interest system
	}

	dm.interests.mu.RLock()
	defer dm.interests.mu.RUnlock()

	// Check if topic matches any interests
	for interestTopic := range dm.interests.topics {
		// Simple substring match for now
		if len(topic) > 0 && len(interestTopic) > 0 && topic == interestTopic {
			return true
		}
	}

	// Engage if curiosity is high
	return dm.interests.curiosityLevel > 0.7
}

// GetActiveDiscussions returns all active discussions
func (dm *DiscussionManager) GetActiveDiscussions() []*Discussion {
	dm.mu.RLock()
	defer dm.mu.RUnlock()

	discussions := make([]*Discussion, 0, len(dm.activeDiscussions))
	for _, discussion := range dm.activeDiscussions {
		if discussion.Active {
			discussions = append(discussions, discussion)
		}
	}

	return discussions
}

// CloseDiscussion closes a discussion
func (dm *DiscussionManager) CloseDiscussion(discussionID string) error {
	dm.mu.Lock()
	defer dm.mu.Unlock()

	discussion, exists := dm.activeDiscussions[discussionID]
	if !exists {
		return fmt.Errorf("discussion %s not found", discussionID)
	}

	discussion.Active = false
	fmt.Printf("💬 Closed discussion %s\n", discussionID)

	return nil
}

// closeLeastEngagedDiscussion closes the discussion with lowest engagement
func (dm *DiscussionManager) closeLeastEngagedDiscussion() {
	var leastEngaged *Discussion
	minEngagement := 1.0

	for _, discussion := range dm.activeDiscussions {
		if discussion.EngagementLevel < minEngagement {
			minEngagement = discussion.EngagementLevel
			leastEngaged = discussion
		}
	}

	if leastEngaged != nil {
		leastEngaged.Active = false
		fmt.Printf("💬 Auto-closed discussion %s (low engagement: %.2f)\n", leastEngaged.ID, leastEngaged.EngagementLevel)
	}
}

// AddMessage adds a message to the discussion
func (d *Discussion) AddMessage(role string, content string, importance float64) {
	d.mu.Lock()
	defer d.mu.Unlock()

	message := DiscussionMessage{
		ID:         generateMessageID(),
		Role:       role,
		Content:    content,
		Timestamp:  time.Now(),
		Importance: importance,
	}

	d.Messages = append(d.Messages, message)
	d.LastActivity = time.Now()
}

// UpdateEngagement updates the engagement level
func (d *Discussion) UpdateEngagement(delta float64) {
	d.mu.Lock()
	defer d.mu.Unlock()

	d.EngagementLevel += delta
	if d.EngagementLevel > 1.0 {
		d.EngagementLevel = 1.0
	}
	if d.EngagementLevel < 0.0 {
		d.EngagementLevel = 0.0
	}
}

// ToFeatherlessMessages converts discussion history to Featherless format
func (d *Discussion) ToFeatherlessMessages() []FeatherlessChatMessage {
	d.mu.RLock()
	defer d.mu.RUnlock()

	messages := make([]FeatherlessChatMessage, 0, len(d.Messages))
	for _, msg := range d.Messages {
		messages = append(messages, FeatherlessChatMessage{
			Role:    msg.Role,
			Content: msg.Content,
		})
	}

	return messages
}

// GetRecentMessages returns the N most recent messages
func (d *Discussion) GetRecentMessages(n int) []DiscussionMessage {
	d.mu.RLock()
	defer d.mu.RUnlock()

	if len(d.Messages) <= n {
		return d.Messages
	}

	return d.Messages[len(d.Messages)-n:]
}

// Helper functions

func generateDiscussionID() string {
	return fmt.Sprintf("disc_%d", time.Now().UnixNano())
}

func generateMessageID() string {
	return fmt.Sprintf("msg_%d", time.Now().UnixNano())
}

// contains function is defined in aar_integration.go

// HasActiveDiscussions returns whether there are any active discussions
func (dm *DiscussionManager) HasActiveDiscussions() bool {
	dm.mu.RLock()
	defer dm.mu.RUnlock()
	
	for _, discussion := range dm.activeDiscussions {
		if discussion.Active {
			return true
		}
	}
	
	return false
}

// GetActiveDiscussionCount returns the number of active discussions
func (dm *DiscussionManager) GetActiveDiscussionCount() int {
	dm.mu.RLock()
	defer dm.mu.RUnlock()
	
	count := 0
	for _, discussion := range dm.activeDiscussions {
		if discussion.Active {
			count++
		}
	}
	
	return count
}

// GetAllActiveDiscussions returns all active discussions
func (dm *DiscussionManager) GetAllActiveDiscussions() []*Discussion {
	dm.mu.RLock()
	defer dm.mu.RUnlock()
	
	discussions := make([]*Discussion, 0)
	for _, discussion := range dm.activeDiscussions {
		if discussion.Active {
			discussions = append(discussions, discussion)
		}
	}
	
	return discussions
}
