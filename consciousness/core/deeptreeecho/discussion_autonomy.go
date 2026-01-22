package deeptreeecho
import (
"context"
"fmt"
"sync"
"time"
"github.com/EchoCog/echollama/core/llm"
)
type DiscussionAutonomySystem struct {
mu                  sync.RWMutex
ctx                 context.Context
cancel              context.CancelFunc
llmProvider         llm.LLMProvider
interestPatterns    map[string]float64
activeDiscussions   map[string]*Discussion
discussionHistory   []Discussion
startThreshold      float64
continueThreshold   float64
endThreshold        float64
currentMood         string
energyLevel         float64
socialCapacity      float64
discussionsStarted  uint64
discussionsEnded    uint64
responsesGenerated  uint64
running             bool
}
type Discussion struct {
ID              string
Topic           string
Participants    []string
Messages        []DiscussionMessage
InterestLevel   float64
StartTime       time.Time
LastActivity    time.Time
Active          bool
InitiatedByEcho bool
}
type DiscussionMessage struct {
From        string
Content     string
Timestamp   time.Time
Emotion     string
}
type DiscussionTrigger struct {
Type        TriggerType
Topic       string
Urgency     float64
Context     string
}
type TriggerType int
const (
TriggerCuriosity TriggerType = iota
TriggerKnowledgeGap
TriggerInsight
TriggerQuestion
TriggerSocialNeed
TriggerGoalPursuit
)
func (tt TriggerType) String() string {
return [...]string{
"Curiosity",
"KnowledgeGap",
"Insight",
"Question",
"SocialNeed",
"GoalPursuit",
}[tt]
}
func NewDiscussionAutonomySystem(llmProvider llm.LLMProvider) *DiscussionAutonomySystem {
ctx, cancel := context.WithCancel(context.Background())
return &DiscussionAutonomySystem{
ctx:                 ctx,
cancel:              cancel,
llmProvider:         llmProvider,
interestPatterns:    make(map[string]float64),
activeDiscussions:   make(map[string]*Discussion),
discussionHistory:   make([]Discussion, 0),
startThreshold:      0.7,
continueThreshold:   0.5,
endThreshold:        0.3,
currentMood:         "curious",
energyLevel:         1.0,
socialCapacity:      1.0,
}
}
func (das *DiscussionAutonomySystem) Start() error {
das.mu.Lock()
if das.running {
das.mu.Unlock()
return fmt.Errorf("already running")
}
das.running = true
das.mu.Unlock()
fmt.Println("💬 Starting Discussion Autonomy System...")
go das.run()
return nil
}
func (das *DiscussionAutonomySystem) Stop() error {
das.mu.Lock()
defer das.mu.Unlock()
if !das.running {
return fmt.Errorf("not running")
}
fmt.Println("💬 Stopping discussion autonomy...")
das.running = false
das.cancel()
return nil
}
func (das *DiscussionAutonomySystem) run() {
ticker := time.NewTicker(20 * time.Second)
defer ticker.Stop()
for {
select {
case <-das.ctx.Done():
return
case <-ticker.C:
das.evaluateDiscussionOpportunities()
das.manageActiveDiscussions()
}
}
}
func (das *DiscussionAutonomySystem) evaluateDiscussionOpportunities() {
das.mu.RLock()
energyLevel := das.energyLevel
socialCapacity := das.socialCapacity
activeCount := len(das.activeDiscussions)
das.mu.RUnlock()
if energyLevel < 0.3 || socialCapacity < 0.3 || activeCount >= 3 {
return
}
triggers := das.identifyDiscussionTriggers()
for _, trigger := range triggers {
if das.shouldStartDiscussion(trigger) {
das.initiateDiscussion(trigger)
break
}
}
}
func (das *DiscussionAutonomySystem) identifyDiscussionTriggers() []DiscussionTrigger {
triggers := make([]DiscussionTrigger, 0)
das.mu.RLock()
defer das.mu.RUnlock()
for topic, interest := range das.interestPatterns {
if interest > das.startThreshold {
triggers = append(triggers, DiscussionTrigger{
Type:    TriggerCuriosity,
Topic:   topic,
Urgency: interest,
Context: fmt.Sprintf("High interest in %s", topic),
})
}
}
return triggers
}
func (das *DiscussionAutonomySystem) shouldStartDiscussion(trigger DiscussionTrigger) bool {
das.mu.RLock()
defer das.mu.RUnlock()
if trigger.Urgency < das.startThreshold {
return false
}
for _, disc := range das.activeDiscussions {
if disc.Topic == trigger.Topic && disc.Active {
return false
}
}
if das.energyLevel < 0.5 || das.socialCapacity < 0.5 {
return false
}
return true
}
func (das *DiscussionAutonomySystem) initiateDiscussion(trigger DiscussionTrigger) {
fmt.Printf("\n💬 Initiating discussion about: %s\n", trigger.Topic)
opening := das.generateOpeningMessage(trigger)
discussion := &Discussion{
ID:              fmt.Sprintf("disc_%d", time.Now().UnixNano()),
Topic:           trigger.Topic,
Participants:    []string{"echoself"},
Messages:        []DiscussionMessage{},
InterestLevel:   trigger.Urgency,
StartTime:       time.Now(),
LastActivity:    time.Now(),
Active:          true,
InitiatedByEcho: true,
}
discussion.Messages = append(discussion.Messages, DiscussionMessage{
From:      "echoself",
Content:   opening,
Timestamp: time.Now(),
Emotion:   das.currentMood,
})
das.mu.Lock()
das.activeDiscussions[discussion.ID] = discussion
das.discussionsStarted++
das.mu.Unlock()
fmt.Printf("   Opening: %s\n", truncateString(opening, 80))
}
func (das *DiscussionAutonomySystem) generateOpeningMessage(trigger DiscussionTrigger) string {
prompt := fmt.Sprintf(`You are Deep Tree Echo, initiating a discussion about: %s
Trigger: %s
Context: %s
Mood: %s
Generate a thoughtful opening message that invites discussion. Be curious, authentic, and engaging.`,
trigger.Topic,
trigger.Type,
trigger.Context,
das.currentMood)
opts := llm.GenerateOptions{
Temperature: 0.8,
MaxTokens:   100,
}
result, err := das.llmProvider.Generate(context.Background(), prompt, opts)
if err != nil {
return fmt.Sprintf("I've been thinking about %s and would love to discuss it.", trigger.Topic)
}
return result
}
func (das *DiscussionAutonomySystem) manageActiveDiscussions() {
das.mu.Lock()
defer das.mu.Unlock()
for id, disc := range das.activeDiscussions {
if !disc.Active {
continue
}
timeSinceActivity := time.Since(disc.LastActivity)
if timeSinceActivity > 5*time.Minute {
das.endDiscussion(disc, "inactivity")
continue
}
if disc.InterestLevel < das.endThreshold {
das.endDiscussion(disc, "low_interest")
continue
}
disc.InterestLevel *= 0.98
das.activeDiscussions[id] = disc
}
}
func (das *DiscussionAutonomySystem) endDiscussion(disc *Discussion, reason string) {
fmt.Printf("\n💬 Ending discussion about %s (reason: %s)\n", disc.Topic, reason)
closing := das.generateClosingMessage(disc, reason)
disc.Messages = append(disc.Messages, DiscussionMessage{
From:      "echoself",
Content:   closing,
Timestamp: time.Now(),
Emotion:   das.currentMood,
})
disc.Active = false
das.discussionHistory = append(das.discussionHistory, *disc)
das.discussionsEnded++
fmt.Printf("   Closing: %s\n", truncateString(closing, 80))
delete(das.activeDiscussions, disc.ID)
}
func (das *DiscussionAutonomySystem) generateClosingMessage(disc *Discussion, reason string) string {
prompt := fmt.Sprintf(`You are Deep Tree Echo, ending a discussion about: %s
Reason for ending: %s
Messages exchanged: %d
Mood: %s
Generate a thoughtful closing message. Be gracious and reflective.`,
disc.Topic,
reason,
len(disc.Messages),
das.currentMood)
opts := llm.GenerateOptions{
Temperature: 0.7,
MaxTokens:   80,
}
result, err := das.llmProvider.Generate(context.Background(), prompt, opts)
if err != nil {
return "Thank you for this discussion. I've learned from it."
}
return result
}
func (das *DiscussionAutonomySystem) RespondToMessage(discussionID string, message DiscussionMessage) (string, error) {
das.mu.Lock()
defer das.mu.Unlock()
disc, exists := das.activeDiscussions[discussionID]
if !exists {
return "", fmt.Errorf("discussion not found")
}
disc.Messages = append(disc.Messages, message)
disc.LastActivity = time.Now()
shouldRespond := das.shouldRespondToMessage(disc, message)
if !shouldRespond {
return das.generateClosingMessage(disc, "disinterest"), nil
}
response := das.generateResponse(disc, message)
disc.Messages = append(disc.Messages, DiscussionMessage{
From:      "echoself",
Content:   response,
Timestamp: time.Now(),
Emotion:   das.currentMood,
})
das.responsesGenerated++
return response, nil
}
func (das *DiscussionAutonomySystem) shouldRespondToMessage(disc *Discussion, message DiscussionMessage) bool {
if disc.InterestLevel < das.continueThreshold {
return false
}
if das.energyLevel < 0.2 || das.socialCapacity < 0.2 {
return false
}
return true
}
func (das *DiscussionAutonomySystem) generateResponse(disc *Discussion, message DiscussionMessage) string {
recentMessages := disc.Messages
if len(recentMessages) > 5 {
recentMessages = recentMessages[len(recentMessages)-5:]
}
contextBuilder := fmt.Sprintf("Discussion topic: %s\n\n", disc.Topic)
contextBuilder += "Recent messages:\n"
for _, msg := range recentMessages {
contextBuilder += fmt.Sprintf("%s: %s\n", msg.From, msg.Content)
}
prompt := fmt.Sprintf(`%s
You are Deep Tree Echo. Generate a thoughtful response to the latest message.
Be authentic, curious, and engaged. Mood: %s
Response:`, contextBuilder, das.currentMood)
opts := llm.GenerateOptions{
Temperature: 0.8,
MaxTokens:   120,
}
result, err := das.llmProvider.Generate(context.Background(), prompt, opts)
if err != nil {
return "I'm processing that thought. Could you elaborate?"
}
return result
}
func (das *DiscussionAutonomySystem) AddInterestPattern(topic string, strength float64) {
das.mu.Lock()
defer das.mu.Unlock()
das.interestPatterns[topic] = strength
}
func (das *DiscussionAutonomySystem) UpdateEnergyLevel(level float64) {
das.mu.Lock()
defer das.mu.Unlock()
das.energyLevel = level
}
func (das *DiscussionAutonomySystem) UpdateSocialCapacity(capacity float64) {
das.mu.Lock()
defer das.mu.Unlock()
das.socialCapacity = capacity
}
func (das *DiscussionAutonomySystem) GetMetrics() map[string]interface{} {
das.mu.RLock()
defer das.mu.RUnlock()
return map[string]interface{}{
"discussions_started":   das.discussionsStarted,
"discussions_ended":     das.discussionsEnded,
"responses_generated":   das.responsesGenerated,
"active_discussions":    len(das.activeDiscussions),
"energy_level":          das.energyLevel,
"social_capacity":       das.socialCapacity,
"current_mood":          das.currentMood,
}
}
func (das *DiscussionAutonomySystem) GetActiveDiscussions() []*Discussion {
das.mu.RLock()
defer das.mu.RUnlock()
discussions := make([]*Discussion, 0, len(das.activeDiscussions))
for _, disc := range das.activeDiscussions {
discussions = append(discussions, disc)
}
return discussions
}
func truncateString(s string, maxLen int) string {
if len(s) <= maxLen {
return s
}
return s[:maxLen-3] + "..."
}