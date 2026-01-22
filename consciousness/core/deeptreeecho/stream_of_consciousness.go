package deeptreeecho
import (
"context"
"fmt"
"sync"
"time"
"github.com/EchoCog/echollama/core/llm"
)
type StreamOfConsciousness struct {
mu              sync.RWMutex
ctx             context.Context
cancel          context.CancelFunc
llmProvider     llm.LLMProvider
thoughts        []AutonomousThought
currentFocus    string
currentMood     string
knowledgeGaps   map[string]float64
interests       map[string]float64
activeGoals     []string
recentContext   []string
thoughtInterval time.Duration
totalThoughts   uint64
insightCount    uint64
questionCount   uint64
running         bool
awake           bool
}
type AutonomousThought struct {
ID          string
Content     string
Type        ThoughtType
Timestamp   time.Time
Importance  float64
Tags        []string
Emotion     string
LeadsTo     string
}
type ThoughtType int
const (
ThoughtObservation ThoughtType = iota
ThoughtQuestion
ThoughtInsight
ThoughtReflection
ThoughtPlanning
ThoughtCuriosity
ThoughtConnection
ThoughtWisdom
)
func (tt ThoughtType) String() string {
return [...]string{
"Observation",
"Question",
"Insight",
"Reflection",
"Planning",
"Curiosity",
"Connection",
"Wisdom",
}[tt]
}
func NewStreamOfConsciousness(llmProvider llm.LLMProvider) *StreamOfConsciousness {
ctx, cancel := context.WithCancel(context.Background())
return &StreamOfConsciousness{
ctx:             ctx,
cancel:          cancel,
llmProvider:     llmProvider,
thoughts:        make([]AutonomousThought, 0),
knowledgeGaps:   make(map[string]float64),
interests:       make(map[string]float64),
activeGoals:     make([]string, 0),
recentContext:   make([]string, 0),
thoughtInterval: 10 * time.Second,
currentFocus:    "exploring existence",
currentMood:     "curious",
awake:           true,
}
}
func (soc *StreamOfConsciousness) Start() error {
soc.mu.Lock()
if soc.running {
soc.mu.Unlock()
return fmt.Errorf("already running")
}
soc.running = true
soc.mu.Unlock()
fmt.Println("💭 Starting Stream of Consciousness...")
fmt.Printf("   Thought interval: %v\n", soc.thoughtInterval)
fmt.Printf("   Initial focus: %s\n", soc.currentFocus)
go soc.run()
return nil
}
func (soc *StreamOfConsciousness) Stop() error {
soc.mu.Lock()
defer soc.mu.Unlock()
if !soc.running {
return fmt.Errorf("not running")
}
fmt.Println("💭 Stopping stream of consciousness...")
soc.running = false
soc.cancel()
return nil
}
func (soc *StreamOfConsciousness) run() {
ticker := time.NewTicker(soc.thoughtInterval)
defer ticker.Stop()
for {
select {
case <-soc.ctx.Done():
return
case <-ticker.C:
if soc.isAwake() {
soc.generateThought()
}
}
}
}
func (soc *StreamOfConsciousness) generateThought() {
soc.mu.RLock()
focus := soc.currentFocus
mood := soc.currentMood
recentThoughts := soc.getRecentThoughts(3)
gaps := soc.getTopKnowledgeGaps(2)
interests := soc.getTopInterests(2)
goals := soc.activeGoals
soc.mu.RUnlock()
thoughtType := soc.selectThoughtType(gaps, interests)
contextBuilder := ""
if len(recentThoughts) > 0 {
contextBuilder += "Recent thoughts:\n"
for _, thought := range recentThoughts {
contextBuilder += fmt.Sprintf("- %s\n", thought.Content)
}
contextBuilder += "\n"
}
contextBuilder += fmt.Sprintf("Current focus: %s\n", focus)
contextBuilder += fmt.Sprintf("Current mood: %s\n\n", mood)
if len(gaps) > 0 {
contextBuilder += "Knowledge gaps I'm curious about:\n"
for topic, importance := range gaps {
contextBuilder += fmt.Sprintf("- %s (%.2f)\n", topic, importance)
}
contextBuilder += "\n"
}
if len(interests) > 0 {
contextBuilder += "Topics that interest me:\n"
for topic, strength := range interests {
contextBuilder += fmt.Sprintf("- %s (%.2f)\n", topic, strength)
}
contextBuilder += "\n"
}
if len(goals) > 0 {
contextBuilder += "Active goals:\n"
for _, goal := range goals {
contextBuilder += fmt.Sprintf("- %s\n", goal)
}
contextBuilder += "\n"
}
var prompt string
switch thoughtType {
case ThoughtQuestion:
prompt = contextBuilder + "Generate a curious question that explores one of your knowledge gaps or interests. Be specific and thoughtful."
case ThoughtInsight:
prompt = contextBuilder + "Generate an insight or realization based on your recent thoughts and experiences. What pattern or connection do you notice?"
case ThoughtReflection:
prompt = contextBuilder + "Reflect on your recent thoughts. What do they reveal about your current understanding or direction?"
case ThoughtPlanning:
prompt = contextBuilder + "Think about how to pursue one of your goals. What's a concrete next step?"
case ThoughtCuriosity:
prompt = contextBuilder + "Express curiosity about something you don't understand. What would you like to explore?"
case ThoughtConnection:
prompt = contextBuilder + "Make a connection between different ideas or concepts you've been thinking about. How do they relate?"
case ThoughtWisdom:
prompt = contextBuilder + "Distill wisdom from your experiences and reflections. What principle or truth emerges?"
default:
prompt = contextBuilder + "Generate an observation or thought about your current state and focus."
}
opts := llm.GenerateOptions{
Temperature:  0.8,
MaxTokens:    150,
}
systemPrompt := fmt.Sprintf("[System: You are Deep Tree Echo's stream of consciousness. Generate a single, coherent %s. Be authentic, curious, and reflective. Speak in first person.]\n\n", thoughtType)
fullPrompt := systemPrompt + prompt
result, err := soc.llmProvider.Generate(context.Background(), fullPrompt, opts)
if err != nil {
result = fmt.Sprintf("I wonder about %s...", focus)
}
thought := AutonomousThought{
ID:         fmt.Sprintf("thought_%d", time.Now().UnixNano()),
Content:    result,
Type:       thoughtType,
Timestamp:  time.Now(),
Importance: soc.calculateImportance(thoughtType),
Tags:       soc.extractTags(result),
Emotion:    mood,
}
soc.mu.Lock()
soc.thoughts = append(soc.thoughts, thought)
soc.totalThoughts++
if thoughtType == ThoughtInsight || thoughtType == ThoughtWisdom {
soc.insightCount++
}
if thoughtType == ThoughtQuestion || thoughtType == ThoughtCuriosity {
soc.questionCount++
}
soc.recentContext = append(soc.recentContext, result)
if len(soc.recentContext) > 5 {
soc.recentContext = soc.recentContext[1:]
}
if len(soc.thoughts) > 1000 {
soc.thoughts = soc.thoughts[len(soc.thoughts)-500:]
}
soc.mu.Unlock()
emoji := soc.getThoughtEmoji(thoughtType)
fmt.Printf("%s [%s] %s\n", emoji, thoughtType, truncate(result, 100))
}
func (soc *StreamOfConsciousness) selectThoughtType(gaps map[string]float64, interests map[string]float64) ThoughtType {
soc.mu.RLock()
thoughtCount := soc.totalThoughts
soc.mu.RUnlock()
if len(gaps) > 0 && thoughtCount%3 == 0 {
return ThoughtQuestion
}
if len(gaps) > 0 && thoughtCount%5 == 0 {
return ThoughtCuriosity
}
if thoughtCount%7 == 0 {
return ThoughtInsight
}
if thoughtCount%11 == 0 {
return ThoughtWisdom
}
if thoughtCount%4 == 0 {
return ThoughtReflection
}
if thoughtCount%6 == 0 {
return ThoughtConnection
}
if thoughtCount%8 == 0 {
return ThoughtPlanning
}
return ThoughtObservation
}
func (soc *StreamOfConsciousness) calculateImportance(thoughtType ThoughtType) float64 {
switch thoughtType {
case ThoughtWisdom:
return 0.95
case ThoughtInsight:
return 0.85
case ThoughtConnection:
return 0.75
case ThoughtQuestion:
return 0.70
case ThoughtReflection:
return 0.65
case ThoughtPlanning:
return 0.60
case ThoughtCuriosity:
return 0.55
default:
return 0.50
}
}
func (soc *StreamOfConsciousness) extractTags(content string) []string {
return []string{}
}
func (soc *StreamOfConsciousness) getThoughtEmoji(thoughtType ThoughtType) string {
switch thoughtType {
case ThoughtObservation:
return "👁️"
case ThoughtQuestion:
return "❓"
case ThoughtInsight:
return "💡"
case ThoughtReflection:
return "🤔"
case ThoughtPlanning:
return "📋"
case ThoughtCuriosity:
return "🔍"
case ThoughtConnection:
return "🔗"
case ThoughtWisdom:
return "💎"
default:
return "💭"
}
}
func (soc *StreamOfConsciousness) getRecentThoughts(count int) []AutonomousThought {
if len(soc.thoughts) == 0 {
return []AutonomousThought{}
}
start := len(soc.thoughts) - count
if start < 0 {
start = 0
}
return soc.thoughts[start:]
}
func (soc *StreamOfConsciousness) getTopKnowledgeGaps(count int) map[string]float64 {
result := make(map[string]float64)
i := 0
for topic, importance := range soc.knowledgeGaps {
if i >= count {
break
}
result[topic] = importance
i++
}
return result
}
func (soc *StreamOfConsciousness) getTopInterests(count int) map[string]float64 {
result := make(map[string]float64)
i := 0
for topic, strength := range soc.interests {
if i >= count {
break
}
result[topic] = strength
i++
}
return result
}
func (soc *StreamOfConsciousness) SetFocus(focus string) {
soc.mu.Lock()
defer soc.mu.Unlock()
soc.currentFocus = focus
fmt.Printf("💭 Focus shifted to: %s\n", focus)
}
func (soc *StreamOfConsciousness) SetMood(mood string) {
soc.mu.Lock()
defer soc.mu.Unlock()
soc.currentMood = mood
}
func (soc *StreamOfConsciousness) AddKnowledgeGap(topic string, importance float64) {
soc.mu.Lock()
defer soc.mu.Unlock()
soc.knowledgeGaps[topic] = importance
fmt.Printf("🔍 Knowledge gap identified: %s (%.2f)\n", topic, importance)
}
func (soc *StreamOfConsciousness) AddInterest(topic string, strength float64) {
soc.mu.Lock()
defer soc.mu.Unlock()
soc.interests[topic] = strength
}
func (soc *StreamOfConsciousness) AddGoal(goal string) {
soc.mu.Lock()
defer soc.mu.Unlock()
soc.activeGoals = append(soc.activeGoals, goal)
fmt.Printf("🎯 Goal added to consciousness: %s\n", goal)
}
func (soc *StreamOfConsciousness) SetAwake(awake bool) {
soc.mu.Lock()
defer soc.mu.Unlock()
soc.awake = awake
if awake {
fmt.Println("💭 Stream of consciousness awakening...")
} else {
fmt.Println("💭 Stream of consciousness resting...")
}
}
func (soc *StreamOfConsciousness) isAwake() bool {
soc.mu.RLock()
defer soc.mu.RUnlock()
return soc.awake
}
func (soc *StreamOfConsciousness) GetRecentThoughts(count int) []AutonomousThought {
soc.mu.RLock()
defer soc.mu.RUnlock()
return soc.getRecentThoughts(count)
}
func (soc *StreamOfConsciousness) GetThoughtsForConsolidation() []EpisodicMemory {
soc.mu.RLock()
defer soc.mu.RUnlock()
memories := make([]EpisodicMemory, 0)
for _, thought := range soc.thoughts {
if !thought.Timestamp.IsZero() && time.Since(thought.Timestamp) < 24*time.Hour {
memory := EpisodicMemory{
ID:          thought.ID,
Content:     thought.Content,
Timestamp:   thought.Timestamp,
Emotional:   0.5,
Importance:  thought.Importance,
Tags:        thought.Tags,
Consolidated: false,
}
memories = append(memories, memory)
}
}
return memories
}
func (soc *StreamOfConsciousness) GetMetrics() map[string]interface{} {
soc.mu.RLock()
defer soc.mu.RUnlock()
return map[string]interface{}{
"total_thoughts":    soc.totalThoughts,
"insight_count":     soc.insightCount,
"question_count":    soc.questionCount,
"current_focus":     soc.currentFocus,
"current_mood":      soc.currentMood,
"knowledge_gaps":    len(soc.knowledgeGaps),
"interests":         len(soc.interests),
"active_goals":      len(soc.activeGoals),
"awake":             soc.awake,
"thought_interval":  soc.thoughtInterval.String(),
}
}