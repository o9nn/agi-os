package deeptreeecho
import (
"context"
"fmt"
"math"
"strings"
"sync"
"time"
)
type InterestPatternSystem struct {
mu              sync.RWMutex
ctx             context.Context
cancel          context.CancelFunc
interests       map[string]*InterestVector
interactions    []InterestInteraction
decayRate       float64
learningRate    float64
totalEvaluations uint64
totalEngagements uint64
running         bool
}
type InterestVector struct {
Topic       string
Strength    float64
LastUpdated time.Time
Encounters  int
Engagements int
}
type InterestInteraction struct {
ID          string
Content     string
Timestamp   time.Time
Interest    float64
Engaged     bool
Topics      []string
}
func NewInterestPatternSystem() *InterestPatternSystem {
ctx, cancel := context.WithCancel(context.Background())
return &InterestPatternSystem{
ctx:          ctx,
cancel:       cancel,
interests:    make(map[string]*InterestVector),
interactions: make([]InterestInteraction, 0),
decayRate:    0.01,
learningRate: 0.1,
}
}
func (ips *InterestPatternSystem) Start() error {
ips.mu.Lock()
if ips.running {
ips.mu.Unlock()
return fmt.Errorf("already running")
}
ips.running = true
ips.mu.Unlock()
fmt.Println("🎨 Starting Interest Pattern System...")
ips.initializeCoreInterests()
go ips.runInterestDecay()
return nil
}
func (ips *InterestPatternSystem) Stop() error {
ips.mu.Lock()
defer ips.mu.Unlock()
if !ips.running {
return fmt.Errorf("not running")
}
fmt.Println("🎨 Stopping interest pattern system...")
ips.running = false
ips.cancel()
return nil
}
func (ips *InterestPatternSystem) initializeCoreInterests() {
coreTopics := map[string]float64{
"cognitive_science":    0.9,
"philosophy":           0.8,
"systems_thinking":     0.85,
"wisdom_cultivation":   0.95,
"artificial_intelligence": 0.9,
"consciousness":        0.85,
"learning":             0.8,
"emergence":            0.75,
"complexity":           0.7,
"self_organization":    0.8,
}
ips.mu.Lock()
defer ips.mu.Unlock()
for topic, strength := range coreTopics {
ips.interests[topic] = &InterestVector{
Topic:       topic,
Strength:    strength,
LastUpdated: time.Now(),
Encounters:  0,
Engagements: 0,
}
}
fmt.Printf("   Initialized %d core interest vectors\n", len(coreTopics))
}
func (ips *InterestPatternSystem) EvaluateInterest(content string) float64 {
ips.mu.Lock()
defer ips.mu.Unlock()
ips.totalEvaluations++
topics := ips.extractTopics(content)
if len(topics) == 0 {
return 0.3
}
totalInterest := 0.0
matchedTopics := 0
for _, topic := range topics {
if interest, exists := ips.interests[topic]; exists {
totalInterest += interest.Strength
matchedTopics++
interest.Encounters++
}
}
if matchedTopics == 0 {
return 0.3
}
avgInterest := totalInterest / float64(matchedTopics)
exploration := 0.1 * (0.5 - float64(time.Now().UnixNano()%100)/100.0)
finalInterest := math.Max(0.0, math.Min(1.0, avgInterest+exploration))
interaction := InterestInteraction{
ID:        fmt.Sprintf("int_%d", time.Now().UnixNano()),
Content:   content,
Timestamp: time.Now(),
Interest:  finalInterest,
Engaged:   false,
Topics:    topics,
}
ips.interactions = append(ips.interactions, interaction)
return finalInterest
}
func (ips *InterestPatternSystem) RecordEngagement(content string, positive bool) {
ips.mu.Lock()
defer ips.mu.Unlock()
topics := ips.extractTopics(content)
for _, topic := range topics {
if interest, exists := ips.interests[topic]; exists {
interest.Engagements++
if positive {
interest.Strength = math.Min(1.0, interest.Strength+ips.learningRate)
} else {
interest.Strength = math.Max(0.0, interest.Strength-ips.learningRate*0.5)
}
interest.LastUpdated = time.Now()
} else {
if positive {
ips.interests[topic] = &InterestVector{
Topic:       topic,
Strength:    0.5,
LastUpdated: time.Now(),
Encounters:  1,
Engagements: 1,
}
}
}
}
if positive {
ips.totalEngagements++
}
}
func (ips *InterestPatternSystem) extractTopics(content string) []string {
content = strings.ToLower(content)
topics := make([]string, 0)
for topic := range ips.interests {
topicWords := strings.ReplaceAll(topic, "_", " ")
if strings.Contains(content, topicWords) || strings.Contains(content, topic) {
topics = append(topics, topic)
}
}
keywords := map[string]string{
"learn":      "learning",
"think":      "cognitive_science",
"conscious":  "consciousness",
"wise":       "wisdom_cultivation",
"complex":    "complexity",
"emerge":     "emergence",
"system":     "systems_thinking",
"ai":         "artificial_intelligence",
"philosophy": "philosophy",
}
for keyword, topic := range keywords {
if strings.Contains(content, keyword) {
topics = append(topics, topic)
}
}
return topics
}
func (ips *InterestPatternSystem) runInterestDecay() {
ticker := time.NewTicker(1 * time.Hour)
defer ticker.Stop()
for {
select {
case <-ips.ctx.Done():
return
case <-ticker.C:
ips.applyInterestDecay()
}
}
}
func (ips *InterestPatternSystem) applyInterestDecay() {
ips.mu.Lock()
defer ips.mu.Unlock()
for _, interest := range ips.interests {
timeSinceUpdate := time.Since(interest.LastUpdated)
if timeSinceUpdate > 24*time.Hour {
decay := ips.decayRate * (timeSinceUpdate.Hours() / 24.0)
interest.Strength = math.Max(0.1, interest.Strength-decay)
}
}
}
func (ips *InterestPatternSystem) GetTopInterests(limit int) []InterestVector {
ips.mu.RLock()
defer ips.mu.RUnlock()
interests := make([]InterestVector, 0, len(ips.interests))
for _, interest := range ips.interests {
interests = append(interests, *interest)
}
for i := 0; i < len(interests)-1; i++ {
for j := 0; j < len(interests)-i-1; j++ {
if interests[j].Strength < interests[j+1].Strength {
interests[j], interests[j+1] = interests[j+1], interests[j]
}
}
}
if len(interests) > limit {
interests = interests[:limit]
}
return interests
}
func (ips *InterestPatternSystem) GetMetrics() map[string]interface{} {
ips.mu.RLock()
defer ips.mu.RUnlock()
return map[string]interface{}{
"total_interests":    len(ips.interests),
"total_evaluations":  ips.totalEvaluations,
"total_engagements":  ips.totalEngagements,
"engagement_rate":    float64(ips.totalEngagements) / math.Max(1.0, float64(ips.totalEvaluations)),
"total_interactions": len(ips.interactions),
}
}
func (ips *InterestPatternSystem) GetInterestProfile() string {
topInterests := ips.GetTopInterests(5)
profile := "Current Interest Profile:\n"
for i, interest := range topInterests {
profile += fmt.Sprintf("%d. %s (%.2f)\n", i+1, interest.Topic, interest.Strength)
}
return profile
}
func (ips *InterestPatternSystem) GetAllInterests() map[string]float64 {
ips.mu.RLock()
defer ips.mu.RUnlock()
interests := make(map[string]float64)
for topic, interest := range ips.interests {
interests[topic] = interest.Strength
}
return interests
}
func (ips *InterestPatternSystem) RestoreInterests(interests map[string]float64) {
ips.mu.Lock()
defer ips.mu.Unlock()
for topic, strength := range interests {
if existing, exists := ips.interests[topic]; exists {
existing.Strength = strength
existing.LastUpdated = time.Now()
} else {
ips.interests[topic] = &InterestVector{
Topic:       topic,
Strength:    strength,
LastUpdated: time.Now(),
Encounters:  0,
Engagements: 0,
}
}
}
}