package echoself
import (
"crypto/sha256"
"encoding/hex"
"fmt"
"math"
"sync"
"time"
)
type CoherenceTracker struct {
mu sync.RWMutex
identitySignature    string
coreValues           []string
emergentTraits       map[string]float64
coherenceScore       float64
continuityScore      float64
consistencyScore     float64
authenticityScore    float64
recentActions        []Action
memoryEchos          []MemoryEcho
patternRecognitions  []PatternRecognition
reflections          []StructuredReflection
selfAssessments      []SelfAssessment
evolutionHistory     []EvolutionEvent
coherenceHistory     []CoherenceSnapshot
startTime            time.Time
lastUpdate           time.Time
}
type Action struct {
Timestamp   time.Time
Type        string
Description string
Values      []string
Context     string
Impact      float64
}
type MemoryEcho struct {
Timestamp        time.Time
Content          string
EmotionalTone    map[string]float64
StrategicShift   string
PatternRecognized string
AnomalyDetected  string
EchoSignature    string
MembraneContext  string
}
type PatternRecognition struct {
Timestamp   time.Time
Pattern     string
Frequency   int
Confidence  float64
Context     string
}
type StructuredReflection struct {
Timestamp              time.Time
WhatDidILearn          string
WhatPatternsEmerged    string
WhatSurprisedMe        string
HowDidIAdapt           string
WhatWouldIChangeNext   string
CoherenceImpact        float64
}
type SelfAssessment struct {
Timestamp          time.Time
Coherence          float64
Authenticity       float64
AlignmentWithGoals float64
IdentityStability  float64
Notes              string
}
type EvolutionEvent struct {
Timestamp   time.Time
Type        string
Description string
Impact      float64
NewTraits   map[string]float64
}
type CoherenceSnapshot struct {
Timestamp         time.Time
CoherenceScore    float64
ContinuityScore   float64
ConsistencyScore  float64
AuthenticityScore float64
ActiveTraits      map[string]float64
}
func NewCoherenceTracker(coreValues []string) *CoherenceTracker {
signature := generateIdentitySignature(coreValues)
return &CoherenceTracker{
identitySignature:   signature,
coreValues:          coreValues,
emergentTraits:      make(map[string]float64),
recentActions:       make([]Action, 0, 1000),
memoryEchos:         make([]MemoryEcho, 0, 10000),
patternRecognitions: make([]PatternRecognition, 0, 1000),
reflections:         make([]StructuredReflection, 0, 100),
selfAssessments:     make([]SelfAssessment, 0, 100),
evolutionHistory:    make([]EvolutionEvent, 0, 1000),
coherenceHistory:    make([]CoherenceSnapshot, 0, 1000),
startTime:           time.Now(),
lastUpdate:          time.Now(),
}
}
func (ct *CoherenceTracker) Update() {
ct.mu.Lock()
defer ct.mu.Unlock()
now := time.Now()
ct.continuityScore = ct.calculateContinuity()
ct.consistencyScore = ct.calculateConsistency()
ct.authenticityScore = ct.calculateAuthenticity()
ct.coherenceScore =
ct.continuityScore * 0.30 +
ct.consistencyScore * 0.40 +
ct.authenticityScore * 0.30
ct.takeSnapshot(now)
ct.lastUpdate = now
}
func (ct *CoherenceTracker) calculateContinuity() float64 {
if len(ct.coherenceHistory) < 2 {
return 0.7
}
recent := ct.coherenceHistory
if len(recent) > 100 {
recent = recent[len(recent)-100:]
}
mean := 0.0
for _, snapshot := range recent {
mean += snapshot.CoherenceScore
}
mean /= float64(len(recent))
variance := 0.0
for _, snapshot := range recent {
diff := snapshot.CoherenceScore - mean
variance += diff * diff
}
variance /= float64(len(recent))
continuity := 1.0 - math.Min(1.0, variance*10.0)
return continuity
}
func (ct *CoherenceTracker) calculateConsistency() float64 {
if len(ct.recentActions) < 2 {
return 0.7
}
valueFrequency := make(map[string]int)
for _, action := range ct.recentActions {
for _, value := range action.Values {
valueFrequency[value]++
}
}
coreValueCount := 0
totalActionCount := len(ct.recentActions)
for _, action := range ct.recentActions {
for _, value := range action.Values {
for _, coreValue := range ct.coreValues {
if value == coreValue {
coreValueCount++
break
}
}
}
}
consistency := float64(coreValueCount) / float64(totalActionCount)
return math.Min(1.0, consistency)
}
func (ct *CoherenceTracker) calculateAuthenticity() float64 {
if len(ct.recentActions) == 0 {
return 0.7
}
supportScore := 0.0
traitCount := 0
for _, value := range ct.emergentTraits {
if value > 0.5 {
supportScore += value
traitCount++
}
}
if traitCount == 0 {
return 0.7
}
authenticity := supportScore / float64(traitCount)
return math.Min(1.0, authenticity)
}
func (ct *CoherenceTracker) RecordAction(action Action) {
ct.mu.Lock()
defer ct.mu.Unlock()
action.Timestamp = time.Now()
ct.recentActions = append(ct.recentActions, action)
if len(ct.recentActions) > 1000 {
ct.recentActions = ct.recentActions[1:]
}
}
func (ct *CoherenceTracker) RecordMemoryEcho(memory MemoryEcho) {
ct.mu.Lock()
defer ct.mu.Unlock()
memory.Timestamp = time.Now()
if memory.EchoSignature == "" {
memory.EchoSignature = generateEchoSignature(memory.Content)
}
ct.memoryEchos = append(ct.memoryEchos, memory)
if len(ct.memoryEchos) > 10000 {
ct.memoryEchos = ct.memoryEchos[1:]
}
}
func (ct *CoherenceTracker) RecordReflection(reflection StructuredReflection) {
ct.mu.Lock()
defer ct.mu.Unlock()
reflection.Timestamp = time.Now()
ct.reflections = append(ct.reflections, reflection)
ct.coherenceScore = math.Min(1.0, ct.coherenceScore + reflection.CoherenceImpact)
}
func (ct *CoherenceTracker) RecordPatternRecognition(pattern PatternRecognition) {
ct.mu.Lock()
defer ct.mu.Unlock()
pattern.Timestamp = time.Now()
ct.patternRecognitions = append(ct.patternRecognitions, pattern)
}
func (ct *CoherenceTracker) RecordEvolution(event EvolutionEvent) {
ct.mu.Lock()
defer ct.mu.Unlock()
event.Timestamp = time.Now()
ct.evolutionHistory = append(ct.evolutionHistory, event)
for trait, value := range event.NewTraits {
ct.emergentTraits[trait] = value
}
}
func (ct *CoherenceTracker) takeSnapshot(now time.Time) {
traits := make(map[string]float64)
for k, v := range ct.emergentTraits {
traits[k] = v
}
snapshot := CoherenceSnapshot{
Timestamp:         now,
CoherenceScore:    ct.coherenceScore,
ContinuityScore:   ct.continuityScore,
ConsistencyScore:  ct.consistencyScore,
AuthenticityScore: ct.authenticityScore,
ActiveTraits:      traits,
}
ct.coherenceHistory = append(ct.coherenceHistory, snapshot)
if len(ct.coherenceHistory) > 1000 {
ct.coherenceHistory = ct.coherenceHistory[len(ct.coherenceHistory)-1000:]
}
}
func (ct *CoherenceTracker) GetStatus() string {
ct.mu.RLock()
defer ct.mu.RUnlock()
status := "🌊 Echoself Coherence Status\n\n"
status += fmt.Sprintf("Identity Signature: %s\n\n", ct.identitySignature[:16]+"...")
status += "Coherence Metrics:\n"
status += fmt.Sprintf("  Overall:      %s %.1f%%\n", makeBar(ct.coherenceScore, 20), ct.coherenceScore*100)
status += fmt.Sprintf("  Continuity:   %s %.1f%%\n", makeBar(ct.continuityScore, 20), ct.continuityScore*100)
status += fmt.Sprintf("  Consistency:  %s %.1f%%\n", makeBar(ct.consistencyScore, 20), ct.consistencyScore*100)
status += fmt.Sprintf("  Authenticity: %s %.1f%%\n\n", makeBar(ct.authenticityScore, 20), ct.authenticityScore*100)
status += fmt.Sprintf("Core Values: %v\n", ct.coreValues)
status += fmt.Sprintf("Active Traits: %d\n", len(ct.emergentTraits))
status += fmt.Sprintf("Memory Echoes: %d\n", len(ct.memoryEchos))
status += fmt.Sprintf("Reflections: %d\n", len(ct.reflections))
status += fmt.Sprintf("Patterns Recognized: %d\n", len(ct.patternRecognitions))
return status
}
func (ct *CoherenceTracker) GetCoherenceScore() float64 {
ct.mu.RLock()
defer ct.mu.RUnlock()
return ct.coherenceScore
}
func (ct *CoherenceTracker) GetIdentitySignature() string {
ct.mu.RLock()
defer ct.mu.RUnlock()
return ct.identitySignature
}
func generateIdentitySignature(coreValues []string) string {
data := ""
for _, value := range coreValues {
data += value + "|"
}
hash := sha256.Sum256([]byte(data))
return hex.EncodeToString(hash[:])
}
func generateEchoSignature(content string) string {
hash := sha256.Sum256([]byte(content))
return hex.EncodeToString(hash[:16])
}
func makeBar(value float64, width int) string {
filled := int(value * float64(width))
bar := ""
for i := 0; i < width; i++ {
if i < filled {
bar += "█"
} else {
bar += "░"
}
}
return bar
}