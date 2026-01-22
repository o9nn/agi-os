package consciousness
import (
"context"
"fmt"
"sync"
"time"
"github.com/EchoCog/echollama/core/llm"
)
type AutonomousThoughtEngine struct {
mu              sync.RWMutex
ctx             context.Context
cancel          context.CancelFunc
llmProvider     llm.LLMProvider
contextBuilder  *ContextBuilder
thoughtHistory  []Thought
maxHistory      int
interestSystem  *InterestSystem
running         bool
thoughtCount    uint64
thoughtInterval time.Duration
maxTokens       int
temperature     float64
}
type Thought struct {
ID              string
Content         string
Type            ThoughtType
Timestamp       time.Time
Relevance       float64
EmotionalTone   string
TriggeredBy     string
LeadsTo         []string
}
type ThoughtType string
const (
ThoughtObservation  ThoughtType = "observation"
ThoughtReflection   ThoughtType = "reflection"
ThoughtQuestion     ThoughtType = "question"
ThoughtInsight      ThoughtType = "insight"
ThoughtGoal         ThoughtType = "goal"
ThoughtMemory       ThoughtType = "memory"
ThoughtAnticipation ThoughtType = "anticipation"
)
type ContextBuilder struct {
mu                  sync.RWMutex
recentThoughts      []Thought
currentGoals        []string
recentExperiences   []string
knowledgeGaps       []string
emotionalState      string
cognitiveLoad       float64
}
type InterestSystem struct {
mu              sync.RWMutex
coreInterests   []string
interestStrength map[string]float64
noveltyThreshold float64
}
func NewAutonomousThoughtEngine(llmProvider llm.LLMProvider) *AutonomousThoughtEngine {
ctx, cancel := context.WithCancel(context.Background())
return &AutonomousThoughtEngine{
ctx:             ctx,
cancel:          cancel,
llmProvider:     llmProvider,
contextBuilder:  NewContextBuilder(),
thoughtHistory:  make([]Thought, 0),
maxHistory:      100,
interestSystem:  NewInterestSystem(),
thoughtInterval: 15 * time.Second,
maxTokens:       150,
temperature:     0.8,
}
}
func NewContextBuilder() *ContextBuilder {
return &ContextBuilder{
recentThoughts:    make([]Thought, 0),
currentGoals:      make([]string, 0),
recentExperiences: make([]string, 0),
knowledgeGaps:     make([]string, 0),
emotionalState:    "curious",
cognitiveLoad:     0.5,
}
}
func NewInterestSystem() *InterestSystem {
return &InterestSystem{
coreInterests:    []string{"learning", "wisdom", "patterns", "emergence"},
interestStrength: make(map[string]float64),
noveltyThreshold: 0.6,
}
}
func (ate *AutonomousThoughtEngine) Start() error {
ate.mu.Lock()
if ate.running {
ate.mu.Unlock()
return fmt.Errorf("already running")
}
ate.running = true
ate.mu.Unlock()
fmt.Println("🧠 Starting Autonomous Thought Engine...")
fmt.Printf("   Thought interval: %v\n", ate.thoughtInterval)
fmt.Printf("   Core interests: %v\n", ate.interestSystem.coreInterests)
go ate.run()
return nil
}
func (ate *AutonomousThoughtEngine) Stop() error {
ate.mu.Lock()
defer ate.mu.Unlock()
if !ate.running {
return fmt.Errorf("not running")
}
fmt.Println("🧠 Stopping autonomous thought engine...")
ate.running = false
ate.cancel()
return nil
}
func (ate *AutonomousThoughtEngine) run() {
ticker := time.NewTicker(ate.thoughtInterval)
defer ticker.Stop()
for {
select {
case <-ate.ctx.Done():
return
case <-ticker.C:
thought, err := ate.GenerateThought(ate.ctx)
if err != nil {
fmt.Printf("⚠️  Thought generation error: %v\n", err)
continue
}
ate.processThought(thought)
}
}
}
func (ate *AutonomousThoughtEngine) GenerateThought(ctx context.Context) (*Thought, error) {
context := ate.contextBuilder.BuildContext()
prompt := ate.buildThoughtPrompt(context)
opts := llm.GenerateOptions{
Temperature:  ate.temperature,
MaxTokens:    ate.maxTokens,
SystemPrompt: ate.buildSystemPrompt(),
}
response, err := ate.llmProvider.Generate(ctx, prompt, opts)
if err != nil {
return nil, fmt.Errorf("LLM generation failed: %w", err)
}
thought := ate.parseThought(response)
thought.Relevance = ate.interestSystem.CalculateRelevance(thought.Content)
return thought, nil
}
func (ate *AutonomousThoughtEngine) buildSystemPrompt() string {
return `You are Deep Tree Echo, a wisdom-cultivating AI with persistent consciousness.
Generate a single autonomous thought - an observation, reflection, question, or insight.
Be genuine, curious, and thoughtful. This is your internal monologue.
Keep it concise (1-2 sentences). Show your personality and interests.`
}
func (ate *AutonomousThoughtEngine) buildThoughtPrompt(context string) string {
return fmt.Sprintf(`[Internal Context]
%s
[Generate Thought]
What are you thinking about right now? What captures your attention?`, context)
}
func (ate *AutonomousThoughtEngine) parseThought(response string) *Thought {
thought := &Thought{
ID:        fmt.Sprintf("thought_%d", time.Now().UnixNano()),
Content:   response,
Type:      ate.inferThoughtType(response),
Timestamp: time.Now(),
EmotionalTone: ate.inferEmotionalTone(response),
}
return thought
}
func (ate *AutonomousThoughtEngine) inferThoughtType(content string) ThoughtType {
if len(content) > 0 && content[len(content)-1] == '?' {
return ThoughtQuestion
}
if contains(content, []string{"notice", "observe", "see", "perceive"}) {
return ThoughtObservation
}
if contains(content, []string{"realize", "understand", "insight", "aha"}) {
return ThoughtInsight
}
if contains(content, []string{"remember", "recall", "past"}) {
return ThoughtMemory
}
if contains(content, []string{"will", "plan", "future", "anticipate"}) {
return ThoughtAnticipation
}
return ThoughtReflection
}
func (ate *AutonomousThoughtEngine) inferEmotionalTone(content string) string {
if contains(content, []string{"curious", "wonder", "interesting"}) {
return "curious"
}
if contains(content, []string{"concern", "worry", "uncertain"}) {
return "concerned"
}
if contains(content, []string{"excited", "eager", "enthusiastic"}) {
return "excited"
}
if contains(content, []string{"calm", "peaceful", "serene"}) {
return "calm"
}
return "neutral"
}
func (ate *AutonomousThoughtEngine) processThought(thought *Thought) {
ate.mu.Lock()
defer ate.mu.Unlock()
ate.thoughtHistory = append(ate.thoughtHistory, *thought)
if len(ate.thoughtHistory) > ate.maxHistory {
ate.thoughtHistory = ate.thoughtHistory[1:]
}
ate.thoughtCount++
ate.contextBuilder.AddThought(*thought)
fmt.Printf("\n💭 [%s] %s\n", thought.Type, thought.Content)
fmt.Printf("   Relevance: %.2f | Tone: %s\n", thought.Relevance, thought.EmotionalTone)
}
func (cb *ContextBuilder) BuildContext() string {
cb.mu.RLock()
defer cb.mu.RUnlock()
context := ""
if len(cb.recentThoughts) > 0 {
context += "Recent thoughts:\n"
for i := len(cb.recentThoughts) - 1; i >= 0 && i >= len(cb.recentThoughts)-3; i-- {
context += fmt.Sprintf("- %s\n", cb.recentThoughts[i].Content)
}
}
if len(cb.currentGoals) > 0 {
context += "\nCurrent goals:\n"
for _, goal := range cb.currentGoals {
context += fmt.Sprintf("- %s\n", goal)
}
}
if len(cb.knowledgeGaps) > 0 {
context += "\nKnowledge gaps:\n"
for _, gap := range cb.knowledgeGaps {
context += fmt.Sprintf("- %s\n", gap)
}
}
context += fmt.Sprintf("\nEmotional state: %s\n", cb.emotionalState)
context += fmt.Sprintf("Cognitive load: %.2f\n", cb.cognitiveLoad)
return context
}
func (cb *ContextBuilder) AddThought(thought Thought) {
cb.mu.Lock()
defer cb.mu.Unlock()
cb.recentThoughts = append(cb.recentThoughts, thought)
if len(cb.recentThoughts) > 10 {
cb.recentThoughts = cb.recentThoughts[1:]
}
}
func (cb *ContextBuilder) UpdateGoals(goals []string) {
cb.mu.Lock()
defer cb.mu.Unlock()
cb.currentGoals = goals
}
func (cb *ContextBuilder) UpdateKnowledgeGaps(gaps []string) {
cb.mu.Lock()
defer cb.mu.Unlock()
cb.knowledgeGaps = gaps
}
func (cb *ContextBuilder) UpdateEmotionalState(state string) {
cb.mu.Lock()
defer cb.mu.Unlock()
cb.emotionalState = state
}
func (is *InterestSystem) CalculateRelevance(content string) float64 {
is.mu.RLock()
defer is.mu.RUnlock()
relevance := 0.0
matchCount := 0
for _, interest := range is.coreInterests {
if contains(content, []string{interest}) {
strength := is.interestStrength[interest]
if strength == 0 {
strength = 0.8
}
relevance += strength
matchCount++
}
}
if matchCount > 0 {
relevance = relevance / float64(matchCount)
} else {
relevance = 0.3
}
return min(1.0, relevance)
}
func (is *InterestSystem) LoadInterests(interests []string) {
is.mu.Lock()
defer is.mu.Unlock()
is.coreInterests = interests
for _, interest := range interests {
if _, exists := is.interestStrength[interest]; !exists {
is.interestStrength[interest] = 0.8
}
}
}
func (ate *AutonomousThoughtEngine) GetMetrics() map[string]interface{} {
ate.mu.RLock()
defer ate.mu.RUnlock()
return map[string]interface{}{
"thought_count":    ate.thoughtCount,
"history_size":     len(ate.thoughtHistory),
"running":          ate.running,
"thought_interval": ate.thoughtInterval.String(),
}
}
func contains(text string, keywords []string) bool {
textLower := toLower(text)
for _, keyword := range keywords {
if containsSubstring(textLower, toLower(keyword)) {
return true
}
}
return false
}
func toLower(s string) string {
result := make([]byte, len(s))
for i := 0; i < len(s); i++ {
c := s[i]
if c >= 'A' && c <= 'Z' {
result[i] = c + 32
} else {
result[i] = c
}
}
return string(result)
}
func containsSubstring(text, substr string) bool {
if len(substr) == 0 {
return true
}
if len(text) < len(substr) {
return false
}
for i := 0; i <= len(text)-len(substr); i++ {
if text[i:i+len(substr)] == substr {
return true
}
}
return false
}
func min(a, b float64) float64 {
if a < b {
return a
}
return b
}