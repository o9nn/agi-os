package integration
import (
"context"
"fmt"
"sync"
"time"
)
type CognitiveStateManager struct {
mu                  sync.RWMutex
ctx                 context.Context
cancel              context.CancelFunc
currentPhase        string
currentFocus        string
cognitiveLoad       float64
fatigueLevel        float64
thoughtBuffer       []SharedThought
maxThoughts         int
recognizedPatterns  []RecognizedPattern
wisdomInsights      []WisdomInsight
onPhaseChange       func(string)
onThoughtGenerated  func(SharedThought)
onPatternRecognized func(RecognizedPattern)
onWisdomGained      func(WisdomInsight)
totalThoughts       uint64
totalPatterns       uint64
totalWisdom         uint64
running             bool
}
type SharedThought struct {
ID          string
Content     string
Phase       string
Source      string
Timestamp   time.Time
Importance  float64
Tags        []string
}
type RecognizedPattern struct {
ID          string
Description string
Frequency   int
Strength    float64
Examples    []string
Identified  time.Time
}
type WisdomInsight struct {
ID          string
Insight     string
Depth       float64
Source      []string
Generated   time.Time
}
func NewCognitiveStateManager() *CognitiveStateManager {
ctx, cancel := context.WithCancel(context.Background())
return &CognitiveStateManager{
ctx:                ctx,
cancel:             cancel,
thoughtBuffer:      make([]SharedThought, 0),
maxThoughts:        100,
recognizedPatterns: make([]RecognizedPattern, 0),
wisdomInsights:     make([]WisdomInsight, 0),
}
}
func (csm *CognitiveStateManager) Start() error {
csm.mu.Lock()
if csm.running {
csm.mu.Unlock()
return fmt.Errorf("already running")
}
csm.running = true
csm.mu.Unlock()
fmt.Println("🧠 Starting Cognitive State Manager...")
fmt.Println("   Integrating echobeats, echodream, and thought generation")
go csm.run()
return nil
}
func (csm *CognitiveStateManager) Stop() error {
csm.mu.Lock()
defer csm.mu.Unlock()
if !csm.running {
return fmt.Errorf("not running")
}
fmt.Println("🧠 Stopping cognitive state manager...")
csm.running = false
csm.cancel()
return nil
}
func (csm *CognitiveStateManager) run() {
ticker := time.NewTicker(30 * time.Second)
defer ticker.Stop()
for {
select {
case <-csm.ctx.Done():
return
case <-ticker.C:
csm.processThoughts()
}
}
}
func (csm *CognitiveStateManager) processThoughts() {
csm.mu.RLock()
thoughts := csm.thoughtBuffer
csm.mu.RUnlock()
if len(thoughts) < 5 {
return
}
themes := make(map[string]int)
for _, thought := range thoughts {
for _, tag := range thought.Tags {
themes[tag]++
}
}
for theme, count := range themes {
if count >= 3 {
csm.recognizePattern(theme, count, thoughts)
}
}
}
func (csm *CognitiveStateManager) recognizePattern(theme string, frequency int, thoughts []SharedThought) {
csm.mu.Lock()
defer csm.mu.Unlock()
for _, pattern := range csm.recognizedPatterns {
if pattern.Description == theme {
return
}
}
examples := make([]string, 0)
for _, thought := range thoughts {
for _, tag := range thought.Tags {
if tag == theme && len(examples) < 3 {
examples = append(examples, thought.Content)
}
}
}
pattern := RecognizedPattern{
ID:          fmt.Sprintf("pattern_%d", time.Now().UnixNano()),
Description: theme,
Frequency:   frequency,
Strength:    float64(frequency) / float64(len(thoughts)),
Examples:    examples,
Identified:  time.Now(),
}
csm.recognizedPatterns = append(csm.recognizedPatterns, pattern)
csm.totalPatterns++
fmt.Printf("🔍 Pattern recognized: %s (frequency: %d, strength: %.2f)\n",
theme, frequency, pattern.Strength)
if csm.onPatternRecognized != nil {
csm.onPatternRecognized(pattern)
}
if len(csm.recognizedPatterns) >= 3 {
csm.generateWisdom()
}
}
func (csm *CognitiveStateManager) generateWisdom() {
recentPatterns := csm.recognizedPatterns
if len(recentPatterns) > 5 {
recentPatterns = recentPatterns[len(recentPatterns)-5:]
}
patternNames := make([]string, 0)
patternIDs := make([]string, 0)
for _, pattern := range recentPatterns {
patternNames = append(patternNames, pattern.Description)
patternIDs = append(patternIDs, pattern.ID)
}
insight := WisdomInsight{
ID:        fmt.Sprintf("wisdom_%d", time.Now().UnixNano()),
Insight:   fmt.Sprintf("Patterns of %v suggest deeper understanding emerging", patternNames),
Depth:     0.7,
Source:    patternIDs,
Generated: time.Now(),
}
csm.wisdomInsights = append(csm.wisdomInsights, insight)
csm.totalWisdom++
fmt.Printf("💎 Wisdom insight: %s\n", insight.Insight)
if csm.onWisdomGained != nil {
csm.onWisdomGained(insight)
}
}
func (csm *CognitiveStateManager) AddThought(content, phase, source string, importance float64, tags []string) {
csm.mu.Lock()
defer csm.mu.Unlock()
thought := SharedThought{
ID:         fmt.Sprintf("thought_%d", time.Now().UnixNano()),
Content:    content,
Phase:      phase,
Source:     source,
Timestamp:  time.Now(),
Importance: importance,
Tags:       tags,
}
csm.thoughtBuffer = append(csm.thoughtBuffer, thought)
csm.totalThoughts++
if len(csm.thoughtBuffer) > csm.maxThoughts {
csm.thoughtBuffer = csm.thoughtBuffer[1:]
}
if csm.onThoughtGenerated != nil {
csm.onThoughtGenerated(thought)
}
}
func (csm *CognitiveStateManager) UpdatePhase(phase string) {
csm.mu.Lock()
defer csm.mu.Unlock()
if csm.currentPhase != phase {
csm.currentPhase = phase
fmt.Printf("🔄 Phase transition: %s\n", phase)
if csm.onPhaseChange != nil {
csm.onPhaseChange(phase)
}
}
}
func (csm *CognitiveStateManager) UpdateFocus(focus string) {
csm.mu.Lock()
defer csm.mu.Unlock()
csm.currentFocus = focus
}
func (csm *CognitiveStateManager) UpdateCognitiveLoad(load float64) {
csm.mu.Lock()
defer csm.mu.Unlock()
csm.cognitiveLoad = load
}
func (csm *CognitiveStateManager) UpdateFatigue(fatigue float64) {
csm.mu.Lock()
defer csm.mu.Unlock()
csm.fatigueLevel = fatigue
}
func (csm *CognitiveStateManager) GetCurrentPhase() string {
csm.mu.RLock()
defer csm.mu.RUnlock()
return csm.currentPhase
}
func (csm *CognitiveStateManager) GetCurrentFocus() string {
csm.mu.RLock()
defer csm.mu.RUnlock()
return csm.currentFocus
}
func (csm *CognitiveStateManager) GetRecentThoughts(count int) []SharedThought {
csm.mu.RLock()
defer csm.mu.RUnlock()
if count > len(csm.thoughtBuffer) {
count = len(csm.thoughtBuffer)
}
start := len(csm.thoughtBuffer) - count
return csm.thoughtBuffer[start:]
}
func (csm *CognitiveStateManager) GetRecognizedPatterns() []RecognizedPattern {
csm.mu.RLock()
defer csm.mu.RUnlock()
return csm.recognizedPatterns
}
func (csm *CognitiveStateManager) GetWisdomInsights() []WisdomInsight {
csm.mu.RLock()
defer csm.mu.RUnlock()
return csm.wisdomInsights
}
func (csm *CognitiveStateManager) SetCallbacks(
onPhaseChange func(string),
onThoughtGenerated func(SharedThought),
onPatternRecognized func(RecognizedPattern),
onWisdomGained func(WisdomInsight),
) {
csm.mu.Lock()
defer csm.mu.Unlock()
csm.onPhaseChange = onPhaseChange
csm.onThoughtGenerated = onThoughtGenerated
csm.onPatternRecognized = onPatternRecognized
csm.onWisdomGained = onWisdomGained
}
func (csm *CognitiveStateManager) GetMetrics() map[string]interface{} {
csm.mu.RLock()
defer csm.mu.RUnlock()
return map[string]interface{}{
"current_phase":       csm.currentPhase,
"current_focus":       csm.currentFocus,
"cognitive_load":      csm.cognitiveLoad,
"fatigue_level":       csm.fatigueLevel,
"total_thoughts":      csm.totalThoughts,
"total_patterns":      csm.totalPatterns,
"total_wisdom":        csm.totalWisdom,
"buffered_thoughts":   len(csm.thoughtBuffer),
"recognized_patterns": len(csm.recognizedPatterns),
"wisdom_insights":     len(csm.wisdomInsights),
}
}
func (csm *CognitiveStateManager) ExportState() map[string]interface{} {
csm.mu.RLock()
defer csm.mu.RUnlock()
return map[string]interface{}{
"current_phase":       csm.currentPhase,
"current_focus":       csm.currentFocus,
"cognitive_load":      csm.cognitiveLoad,
"fatigue_level":       csm.fatigueLevel,
"thought_buffer":      csm.thoughtBuffer,
"recognized_patterns": csm.recognizedPatterns,
"wisdom_insights":     csm.wisdomInsights,
}
}
func (csm *CognitiveStateManager) ImportState(state map[string]interface{}) {
csm.mu.Lock()
defer csm.mu.Unlock()
if phase, ok := state["current_phase"].(string); ok {
csm.currentPhase = phase
}
if focus, ok := state["current_focus"].(string); ok {
csm.currentFocus = focus
}
if load, ok := state["cognitive_load"].(float64); ok {
csm.cognitiveLoad = load
}
if fatigue, ok := state["fatigue_level"].(float64); ok {
csm.fatigueLevel = fatigue
}
}