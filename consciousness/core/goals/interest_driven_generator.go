package goals
import (
"context"
"fmt"
"math"
"math/rand"
"sync"
"time"
"github.com/google/uuid"
)
type InterestDrivenGoalGenerator struct {
mu                  sync.RWMutex
ctx                 context.Context
cancel              context.CancelFunc
interestPatterns    map[string]*InterestPattern
curiosityLevel      float64
explorationRate     float64
goalOrchestrator    *GoalOrchestrator
generatedGoals      []*Goal
exploredTopics      map[string]bool
unexploredTopics    []string
currentFocus        []string
minInterestThreshold float64
maxGoalsPerCycle    int
generationInterval  time.Duration
goalsGenerated      uint64
explorationGoals    uint64
learningGoals       uint64
discussionGoals     uint64
running             bool
}
type InterestPattern struct {
Topic           string
Strength        float64
Recency         float64
Depth           float64
Novelty         float64
Utility         float64
LastEngaged     time.Time
EngagementCount int
RelatedTopics   []string
}
func NewInterestDrivenGoalGenerator(goalOrchestrator *GoalOrchestrator) *InterestDrivenGoalGenerator {
ctx, cancel := context.WithCancel(context.Background())
idgg := &InterestDrivenGoalGenerator{
ctx:                  ctx,
cancel:               cancel,
goalOrchestrator:     goalOrchestrator,
interestPatterns:     make(map[string]*InterestPattern),
exploredTopics:       make(map[string]bool),
unexploredTopics:     make([]string, 0),
currentFocus:         make([]string, 0),
generatedGoals:       make([]*Goal, 0),
curiosityLevel:       0.7,
explorationRate:      0.3,
minInterestThreshold: 0.4,
maxGoalsPerCycle:     3,
generationInterval:   5 * time.Minute,
}
idgg.initializeSeedInterests()
return idgg
}
func (idgg *InterestDrivenGoalGenerator) initializeSeedInterests() {
seedTopics := []string{
"pattern recognition",
"wisdom cultivation",
"cognitive architectures",
"consciousness studies",
"knowledge integration",
"autonomous learning",
"creative problem solving",
"meta-cognition",
"temporal reasoning",
"social understanding",
}
for _, topic := range seedTopics {
idgg.interestPatterns[topic] = &InterestPattern{
Topic:           topic,
Strength:        0.5 + rand.Float64()*0.3,
Recency:         0.5,
Depth:           0.2,
Novelty:         0.8,
Utility:         0.6,
LastEngaged:     time.Now().Add(-time.Duration(rand.Intn(24)) * time.Hour),
EngagementCount: 0,
RelatedTopics:   make([]string, 0),
}
}
}
func (idgg *InterestDrivenGoalGenerator) Start() error {
idgg.mu.Lock()
if idgg.running {
idgg.mu.Unlock()
return fmt.Errorf("interest-driven goal generator already running")
}
idgg.running = true
idgg.mu.Unlock()
go idgg.goalGenerationLoop()
go idgg.interestDecayLoop()
go idgg.curiosityEvolutionLoop()
return nil
}
func (idgg *InterestDrivenGoalGenerator) Stop() {
idgg.mu.Lock()
idgg.running = false
idgg.mu.Unlock()
idgg.cancel()
}
func (idgg *InterestDrivenGoalGenerator) goalGenerationLoop() {
ticker := time.NewTicker(idgg.generationInterval)
defer ticker.Stop()
for {
select {
case <-idgg.ctx.Done():
return
case <-ticker.C:
idgg.generateInterestDrivenGoals()
}
}
}
func (idgg *InterestDrivenGoalGenerator) generateInterestDrivenGoals() {
idgg.mu.RLock()
strongInterests := idgg.findStrongestInterests(idgg.maxGoalsPerCycle)
idgg.mu.RUnlock()
for _, interest := range strongInterests {
goal := idgg.createGoalFromInterest(interest)
if goal != nil {
idgg.mu.Lock()
idgg.generatedGoals = append(idgg.generatedGoals, goal)
idgg.goalsGenerated++
idgg.mu.Unlock()
}
}
}
func (idgg *InterestDrivenGoalGenerator) findStrongestInterests(count int) []*InterestPattern {
type scoredInterest struct {
pattern *InterestPattern
score   float64
}
scored := make([]scoredInterest, 0)
for _, pattern := range idgg.interestPatterns {
score := idgg.calculateInterestScore(pattern)
if score > idgg.minInterestThreshold {
scored = append(scored, scoredInterest{pattern, score})
}
}
for i := 0; i < len(scored)-1; i++ {
for j := i + 1; j < len(scored); j++ {
if scored[j].score > scored[i].score {
scored[i], scored[j] = scored[j], scored[i]
}
}
}
result := make([]*InterestPattern, 0)
for i := 0; i < count && i < len(scored); i++ {
result = append(result, scored[i].pattern)
}
return result
}
func (idgg *InterestDrivenGoalGenerator) calculateInterestScore(pattern *InterestPattern) float64 {
strengthWeight := 0.4
noveltyWeight := 0.3
utilityWeight := 0.2
recencyWeight := 0.1
timeSinceEngagement := time.Since(pattern.LastEngaged)
recencyScore := math.Exp(-timeSinceEngagement.Hours() / 24.0)
score := pattern.Strength*strengthWeight +
pattern.Novelty*noveltyWeight +
pattern.Utility*utilityWeight +
recencyScore*recencyWeight
if pattern.Depth < 0.3 {
score += idgg.curiosityLevel * 0.2
}
return score
}
func (idgg *InterestDrivenGoalGenerator) createGoalFromInterest(interest *InterestPattern) *Goal {
var goalType string
var description string
if interest.Depth < 0.3 {
goalType = "exploration"
description = fmt.Sprintf("Explore and understand %s", interest.Topic)
idgg.mu.Lock()
idgg.explorationGoals++
idgg.mu.Unlock()
} else if interest.Novelty > 0.6 {
goalType = "learning"
description = fmt.Sprintf("Deepen knowledge of %s", interest.Topic)
idgg.mu.Lock()
idgg.learningGoals++
idgg.mu.Unlock()
} else {
goalType = "discussion"
description = fmt.Sprintf("Engage in discussion about %s", interest.Topic)
idgg.mu.Lock()
idgg.discussionGoals++
idgg.mu.Unlock()
}
goal := &Goal{
ID:          uuid.New().String(),
CreatedAt:   time.Now(),
UpdatedAt:   time.Now(),
Title:       fmt.Sprintf("%s: %s", goalType, interest.Topic),
Description: description,
Category:    CategoryExploration,
Priority:    int(interest.Strength * 10),
Progress:    0.0,
Status:      StatusActive,
SuccessCriteria: []string{"Engage with topic", "Generate insights"},
Milestones:  []Milestone{},
Actions:     []Action{},
RelatedGoals: []string{},
Metadata: map[string]interface{}{
"type":     goalType,
"topic":    interest.Topic,
"interest": interest.Strength,
"source":   "interest_driven",
},
LessonsLearned: []string{},
Challenges:     []string{},
}
return goal
}
func (idgg *InterestDrivenGoalGenerator) interestDecayLoop() {
ticker := time.NewTicker(1 * time.Hour)
defer ticker.Stop()
for {
select {
case <-idgg.ctx.Done():
return
case <-ticker.C:
idgg.decayInterests()
}
}
}
func (idgg *InterestDrivenGoalGenerator) decayInterests() {
idgg.mu.Lock()
defer idgg.mu.Unlock()
for _, pattern := range idgg.interestPatterns {
timeSinceEngagement := time.Since(pattern.LastEngaged)
if timeSinceEngagement > 24*time.Hour {
decayFactor := 0.95
pattern.Strength *= decayFactor
}
pattern.Recency *= 0.98
if pattern.EngagementCount > 0 {
pattern.Novelty *= 0.99
}
}
}
func (idgg *InterestDrivenGoalGenerator) curiosityEvolutionLoop() {
ticker := time.NewTicker(10 * time.Minute)
defer ticker.Stop()
for {
select {
case <-idgg.ctx.Done():
return
case <-ticker.C:
idgg.evolveCuriosity()
}
}
}
func (idgg *InterestDrivenGoalGenerator) evolveCuriosity() {
idgg.mu.Lock()
defer idgg.mu.Unlock()
unexploredCount := len(idgg.unexploredTopics)
if unexploredCount > 10 {
idgg.curiosityLevel += 0.01
}
if unexploredCount < 3 {
idgg.curiosityLevel -= 0.01
}
idgg.curiosityLevel = clamp(idgg.curiosityLevel, 0.3, 0.9)
}
func (idgg *InterestDrivenGoalGenerator) RecordEngagement(topic string, depth float64) {
idgg.mu.Lock()
defer idgg.mu.Unlock()
pattern, exists := idgg.interestPatterns[topic]
if !exists {
pattern = &InterestPattern{
Topic:           topic,
Strength:        0.5,
Recency:         1.0,
Depth:           depth,
Novelty:         0.8,
Utility:         0.5,
LastEngaged:     time.Now(),
EngagementCount: 1,
RelatedTopics:   make([]string, 0),
}
idgg.interestPatterns[topic] = pattern
} else {
pattern.Strength += 0.1
pattern.Strength = clamp(pattern.Strength, 0.0, 1.0)
pattern.Recency = 1.0
pattern.Depth = (pattern.Depth + depth) / 2.0
pattern.LastEngaged = time.Now()
pattern.EngagementCount++
}
idgg.exploredTopics[topic] = true
}
func (idgg *InterestDrivenGoalGenerator) SuggestExplorationTopics(count int) []string {
idgg.mu.RLock()
defer idgg.mu.RUnlock()
suggestions := make([]string, 0)
for topic, pattern := range idgg.interestPatterns {
if pattern.Novelty > 0.6 && pattern.Depth < 0.3 {
suggestions = append(suggestions, topic)
if len(suggestions) >= count {
break
}
}
}
return suggestions
}
func (idgg *InterestDrivenGoalGenerator) GetInterestPatterns() map[string]*InterestPattern {
idgg.mu.RLock()
defer idgg.mu.RUnlock()
patterns := make(map[string]*InterestPattern)
for k, v := range idgg.interestPatterns {
patterns[k] = v
}
return patterns
}
func (idgg *InterestDrivenGoalGenerator) GetMetrics() map[string]interface{} {
idgg.mu.RLock()
defer idgg.mu.RUnlock()
return map[string]interface{}{
"goals_generated":    idgg.goalsGenerated,
"exploration_goals":  idgg.explorationGoals,
"learning_goals":     idgg.learningGoals,
"discussion_goals":   idgg.discussionGoals,
"curiosity_level":    idgg.curiosityLevel,
"exploration_rate":   idgg.explorationRate,
"active_interests":   len(idgg.interestPatterns),
"explored_topics":    len(idgg.exploredTopics),
}
}
func clamp(value, min, max float64) float64 {
if value < min {
return min
}
if value > max {
return max
}
return value
}