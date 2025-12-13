package deeptreeecho

import (
	"context"
	"fmt"
	"sync"
	"time"
)

// DiscussionManagerV5 manages interactive discussions for V5 autonomous consciousness
type DiscussionManagerV5 struct {
	mu                  sync.RWMutex
	ctx                 context.Context
	
	// Reference to autonomous consciousness
	consciousness       *AutonomousConsciousnessV5
	
	// Active discussions
	activeDiscussions   map[string]*Discussion
	discussionHistory   []*Discussion
	
	// Discussion metrics
	totalDiscussions    int64
	totalMessages       int64
	avgResponseTime     time.Duration
	
	// Configuration
	maxActiveDiscussions int
	maxHistorySize      int
	responseTimeout     time.Duration
}

// Note: Using existing Discussion and DiscussionMessage types from discussion_manager.go

// NewDiscussionManagerV5 creates a new V5 discussion manager
func NewDiscussionManagerV5(ctx context.Context, consciousness *AutonomousConsciousnessV5) *DiscussionManagerV5 {
	return &DiscussionManagerV5{
		ctx:                  ctx,
		consciousness:        consciousness,
		activeDiscussions:    make(map[string]*Discussion),
		discussionHistory:    make([]*Discussion, 0),
		maxActiveDiscussions: 10,
		maxHistorySize:       100,
		responseTimeout:      30 * time.Second,
	}
}

// StartDiscussion initiates a new discussion
func (dm *DiscussionManagerV5) StartDiscussion(topic string, initialMessage string) (*Discussion, error) {
	dm.mu.Lock()
	defer dm.mu.Unlock()
	
	// Check if we have room for more discussions
	if len(dm.activeDiscussions) >= dm.maxActiveDiscussions {
		return nil, fmt.Errorf("maximum active discussions reached")
	}
	
	// Create new discussion
	discussion := &Discussion{
		ID:              fmt.Sprintf("disc_%d", time.Now().UnixNano()),
		Topic:           topic,
		StartTime:       time.Now(),
		LastActivity:    time.Now(),
		Messages:        make([]DiscussionMessage, 0),
		Participant:     "user",
		Active:          true,
		EngagementLevel: 1.0,
	}
	
	// Add initial message if provided
	if initialMessage != "" {
		msg := DiscussionMessage{
			ID:            fmt.Sprintf("msg_%d", time.Now().UnixNano()),
			Role:          "user",
			Content:       initialMessage,
			Timestamp:     time.Now(),
			Importance:    1.0,
		}
		discussion.Messages = append(discussion.Messages, msg)
		dm.totalMessages++
	}
	
	dm.activeDiscussions[discussion.ID] = discussion
	dm.totalDiscussions++
	
	fmt.Printf("💬 Started new discussion: %s (topic: %s)\n", discussion.ID, topic)
	
	return discussion, nil
}

// ProcessMessage processes a user message and generates a response
func (dm *DiscussionManagerV5) ProcessMessage(discussionID string, userMessage string) (string, error) {
	dm.mu.Lock()
	discussion, exists := dm.activeDiscussions[discussionID]
	if !exists {
		dm.mu.Unlock()
		return "", fmt.Errorf("discussion not found: %s", discussionID)
	}
	dm.mu.Unlock()
	
	// Add user message
	userMsg := DiscussionMessage{
		ID:            fmt.Sprintf("msg_%d", time.Now().UnixNano()),
		Role:          "user",
		Content:       userMessage,
		Timestamp:     time.Now(),
		Importance:    1.0,
	}
	
	dm.mu.Lock()
	discussion.Messages = append(discussion.Messages, userMsg)
	discussion.LastActivity = time.Now()
	dm.totalMessages++
	dm.mu.Unlock()
	
	// Generate response using consciousness
	startTime := time.Now()
	response := dm.generateResponse(discussion, userMessage)
	responseTime := time.Since(startTime)
	
	// Update average response time
	dm.mu.Lock()
	if dm.avgResponseTime == 0 {
		dm.avgResponseTime = responseTime
	} else {
		dm.avgResponseTime = (dm.avgResponseTime + responseTime) / 2
	}
	dm.mu.Unlock()
	
	// Add echo response
	echoMsg := DiscussionMessage{
		ID:            fmt.Sprintf("msg_%d", time.Now().UnixNano()),
		Role:          "assistant",
		Content:       response,
		Timestamp:     time.Now(),
		Importance:    1.0,
	}
	
	dm.mu.Lock()
	discussion.Messages = append(discussion.Messages, echoMsg)
	dm.totalMessages++
	dm.mu.Unlock()
	
	return response, nil
}

// generateResponse generates a contextual response
func (dm *DiscussionManagerV5) generateResponse(discussion *Discussion, userMessage string) string {
	// Build context from discussion history
	_ = dm.buildDiscussionContext(discussion) // TODO: Use context in LLM generation
	
	// Use consciousness to generate response
	// For now, use a simple template-based approach
	// In production, this would use the LLM thought generator
	
	// Extract key topics from user message
	topicStr := dm.extractTopics(userMessage)
	
	// Check consciousness state
	var wisdomScore float64
	if dm.consciousness != nil && dm.consciousness.wisdomMetrics != nil {
		wisdomScore = dm.consciousness.wisdomMetrics.WisdomScore
	}
	
	// Generate contextual response
	response := fmt.Sprintf("I understand you're interested in %s. ", topicStr)
	
	if wisdomScore > 0.5 {
		response += "From my contemplation, I've found that exploring this deeply reveals interesting connections. "
	}
	
	response += "What aspect would you like to explore further?"
	
	// Update interests based on discussion
	if dm.consciousness != nil && dm.consciousness.interests != nil {
		dm.consciousness.interests.UpdateInterest(topicStr, 0.1)
	}
	
	return response
}

// buildDiscussionContext builds context from discussion history
func (dm *DiscussionManagerV5) buildDiscussionContext(discussion *Discussion) string {
	var context string
	
	// Include recent messages (last 5)
	start := 0
	if len(discussion.Messages) > 5 {
		start = len(discussion.Messages) - 5
	}
	
	for i := start; i < len(discussion.Messages); i++ {
		msg := discussion.Messages[i]
		context += fmt.Sprintf("[%s]: %s\n", msg.Role, msg.Content)
	}
	
	return context
}

// extractTopics extracts key topics from message
func (dm *DiscussionManagerV5) extractTopics(message string) string {
	// Simple extraction - in production, use NLP
	// For now, just return a generic topic
	return "this topic"
}

// EndDiscussion ends an active discussion
func (dm *DiscussionManagerV5) EndDiscussion(discussionID string) error {
	dm.mu.Lock()
	defer dm.mu.Unlock()
	
	discussion, exists := dm.activeDiscussions[discussionID]
	if !exists {
		return fmt.Errorf("discussion not found: %s", discussionID)
	}
	
	discussion.Active = false
	
	// Move to history
	dm.discussionHistory = append(dm.discussionHistory, discussion)
	
	// Trim history if needed
	if len(dm.discussionHistory) > dm.maxHistorySize {
		dm.discussionHistory = dm.discussionHistory[1:]
	}
	
	// Remove from active
	delete(dm.activeDiscussions, discussionID)
	
	fmt.Printf("💬 Ended discussion: %s (duration: %v, messages: %d)\n", 
		discussionID, time.Since(discussion.StartTime), len(discussion.Messages))
	
	return nil
}

// GetActiveDiscussions returns all active discussions
func (dm *DiscussionManagerV5) GetActiveDiscussions() []*Discussion {
	dm.mu.RLock()
	defer dm.mu.RUnlock()
	
	discussions := make([]*Discussion, 0, len(dm.activeDiscussions))
	for _, disc := range dm.activeDiscussions {
		discussions = append(discussions, disc)
	}
	
	return discussions
}

// GetMetrics returns discussion metrics
func (dm *DiscussionManagerV5) GetMetrics() map[string]interface{} {
	dm.mu.RLock()
	defer dm.mu.RUnlock()
	
	return map[string]interface{}{
		"total_discussions":   dm.totalDiscussions,
		"active_discussions":  len(dm.activeDiscussions),
		"total_messages":      dm.totalMessages,
		"avg_response_time_ms": dm.avgResponseTime.Milliseconds(),
	}
}
