package integration
import (
"context"
"fmt"
"sync"
"time"
"github.com/EchoCog/echollama/core/consciousness"
"github.com/EchoCog/echollama/core/echobeats"
"github.com/EchoCog/echollama/core/echodream"
"github.com/EchoCog/echollama/core/goals"
"github.com/google/uuid"
)
type CognitiveEventLoopOrchestrator struct {
mu                sync.RWMutex
ctx               context.Context
cancel            context.CancelFunc
consciousness     *consciousness.StreamOfConsciousnessLLM
scheduler         *echobeats.EchoBeats
dreamSystem       *echodream.EchoDream
goalOrchestrator  *goals.GoalOrchestrator
thoughtToEvent    map[consciousness.ThoughtType]echobeats.EventType
eventToThought    map[echobeats.EventType]consciousness.ThoughtType
currentFocus      string
cognitiveLoad     float64
fatigueLevel      float64
awarenessLevel    float64
eventsTriggered   uint64
thoughtsTriggered uint64
goalsGenerated    uint64
cyclesCompleted   uint64
running           bool
}
func NewCognitiveEventLoopOrchestrator(
consciousness *consciousness.StreamOfConsciousnessLLM,
scheduler *echobeats.EchoBeats,
dreamSystem *echodream.EchoDream,
goalOrchestrator *goals.GoalOrchestrator,
) *CognitiveEventLoopOrchestrator {
ctx, cancel := context.WithCancel(context.Background())
orchestrator := &CognitiveEventLoopOrchestrator{
ctx:              ctx,
cancel:           cancel,
consciousness:    consciousness,
scheduler:        scheduler,
dreamSystem:      dreamSystem,
goalOrchestrator: goalOrchestrator,
cognitiveLoad:    0.3,
fatigueLevel:     0.0,
awarenessLevel:   0.7,
}
orchestrator.initializeMappings()
return orchestrator
}
func (celo *CognitiveEventLoopOrchestrator) initializeMappings() {
celo.thoughtToEvent = map[consciousness.ThoughtType]echobeats.EventType{
consciousness.ThoughtTypeReflection:    echobeats.EventIntrospection,
consciousness.ThoughtTypeQuestion:      echobeats.EventLearning,
consciousness.ThoughtTypeInsight:       echobeats.EventMemoryConsolidation,
consciousness.ThoughtTypePlanning:      echobeats.EventGoalPursuit,
consciousness.ThoughtTypeMetaCognition: echobeats.EventIntrospection,
consciousness.ThoughtTypePerception:    echobeats.EventPerception,
}
celo.eventToThought = map[echobeats.EventType]consciousness.ThoughtType{
echobeats.EventIntrospection:        consciousness.ThoughtTypeReflection,
echobeats.EventLearning:             consciousness.ThoughtTypeQuestion,
echobeats.EventMemoryConsolidation:  consciousness.ThoughtTypeInsight,
echobeats.EventGoalPursuit:          consciousness.ThoughtTypePlanning,
echobeats.EventPerception:           consciousness.ThoughtTypePerception,
}
}
func (celo *CognitiveEventLoopOrchestrator) Start() error {
celo.mu.Lock()
if celo.running {
celo.mu.Unlock()
return fmt.Errorf("cognitive event loop orchestrator already running")
}
celo.running = true
celo.mu.Unlock()
go celo.thoughtToEventLoop()
go celo.eventToThoughtLoop()
go celo.cognitiveStateLoop()
go celo.goalDrivenEventLoop()
go celo.autonomousCycleLoop()
return nil
}
func (celo *CognitiveEventLoopOrchestrator) Stop() {
celo.mu.Lock()
celo.running = false
celo.mu.Unlock()
celo.cancel()
}
func (celo *CognitiveEventLoopOrchestrator) thoughtToEventLoop() {
ticker := time.NewTicker(2 * time.Second)
defer ticker.Stop()
for {
select {
case <-celo.ctx.Done():
return
case <-ticker.C:
celo.translateThoughtsToEvents()
}
}
}
func (celo *CognitiveEventLoopOrchestrator) translateThoughtsToEvents() {
thoughts := celo.consciousness.GetRecentThoughts(3)
for _, thought := range thoughts {
if celo.shouldTriggerEvent(thought) {
event := celo.createEventFromThought(thought)
if event != nil {
celo.scheduler.ScheduleEvent(event)
celo.mu.Lock()
celo.eventsTriggered++
celo.mu.Unlock()
}
}
}
}
func (celo *CognitiveEventLoopOrchestrator) shouldTriggerEvent(thought interface{}) bool {
return true
}
func (celo *CognitiveEventLoopOrchestrator) createEventFromThought(thought interface{}) *echobeats.CognitiveEvent {
event := &echobeats.CognitiveEvent{
ID:          uuid.New().String(),
Type:        echobeats.EventThought,
Priority:    5,
Timestamp:   time.Now(),
ScheduledAt: time.Now().Add(1 * time.Second),
Payload:     thought,
Context: map[string]interface{}{
"source": "consciousness",
},
Recurring: false,
}
return event
}
func (celo *CognitiveEventLoopOrchestrator) eventToThoughtLoop() {
ticker := time.NewTicker(3 * time.Second)
defer ticker.Stop()
for {
select {
case <-celo.ctx.Done():
return
case <-ticker.C:
celo.translateEventsToThoughts()
}
}
}
func (celo *CognitiveEventLoopOrchestrator) translateEventsToThoughts() {
goals := celo.goalOrchestrator.GetActiveGoals()
if len(goals) > 0 {
goal := goals[0]
thoughtContent := fmt.Sprintf("Pursuing goal: %s", goal.Title)
celo.consciousness.AddExternalThought(thoughtContent)
celo.mu.Lock()
celo.thoughtsTriggered++
celo.mu.Unlock()
}
}
func (celo *CognitiveEventLoopOrchestrator) cognitiveStateLoop() {
ticker := time.NewTicker(5 * time.Second)
defer ticker.Stop()
for {
select {
case <-celo.ctx.Done():
return
case <-ticker.C:
celo.updateCognitiveState()
}
}
}
func (celo *CognitiveEventLoopOrchestrator) updateCognitiveState() {
celo.mu.Lock()
defer celo.mu.Unlock()
celo.fatigueLevel += celo.cognitiveLoad * 0.01
if celo.fatigueLevel > 0.7 {
celo.awarenessLevel -= 0.01
}
celo.fatigueLevel = clamp(celo.fatigueLevel, 0.0, 1.0)
celo.awarenessLevel = clamp(celo.awarenessLevel, 0.0, 1.0)
if celo.fatigueLevel > 0.8 && celo.awarenessLevel < 0.5 {
celo.triggerRestCycle()
}
}
func (celo *CognitiveEventLoopOrchestrator) triggerRestCycle() {
event := &echobeats.CognitiveEvent{
ID:          uuid.New().String(),
Type:        echobeats.EventRest,
Priority:    10,
Timestamp:   time.Now(),
ScheduledAt: time.Now(),
Context: map[string]interface{}{
"fatigue_level": celo.fatigueLevel,
"reason":        "autonomous_fatigue_management",
},
}
celo.scheduler.ScheduleEvent(event)
celo.fatigueLevel = 0.0
celo.awarenessLevel = 0.7
}
func (celo *CognitiveEventLoopOrchestrator) goalDrivenEventLoop() {
ticker := time.NewTicker(10 * time.Second)
defer ticker.Stop()
for {
select {
case <-celo.ctx.Done():
return
case <-ticker.C:
celo.generateGoalDrivenEvents()
}
}
}
func (celo *CognitiveEventLoopOrchestrator) generateGoalDrivenEvents() {
goals := celo.goalOrchestrator.GetActiveGoals()
for _, goal := range goals {
event := &echobeats.CognitiveEvent{
ID:          uuid.New().String(),
Type:        echobeats.EventGoalPursuit,
Priority:    goal.Priority,
Timestamp:   time.Now(),
ScheduledAt: time.Now().Add(5 * time.Second),
Payload:     goal,
Context: map[string]interface{}{
"goal_id":   goal.ID,
"goal_name": goal.Title,
},
}
celo.scheduler.ScheduleEvent(event)
}
}
func (celo *CognitiveEventLoopOrchestrator) autonomousCycleLoop() {
ticker := time.NewTicker(30 * time.Second)
defer ticker.Stop()
for {
select {
case <-celo.ctx.Done():
return
case <-ticker.C:
celo.executeAutonomousCycle()
}
}
}
func (celo *CognitiveEventLoopOrchestrator) executeAutonomousCycle() {
celo.orientingPhase()
celo.conditioningPhase()
celo.anticipatingPhase()
celo.mu.Lock()
celo.cyclesCompleted++
celo.mu.Unlock()
}
func (celo *CognitiveEventLoopOrchestrator) orientingPhase() {
celo.consciousness.AddExternalThought("What is most relevant to my current goals?")
time.Sleep(1 * time.Second)
}
func (celo *CognitiveEventLoopOrchestrator) conditioningPhase() {
celo.consciousness.AddExternalThought("What have I learned that applies here?")
time.Sleep(1 * time.Second)
}
func (celo *CognitiveEventLoopOrchestrator) anticipatingPhase() {
celo.consciousness.AddExternalThought("What possibilities should I explore?")
time.Sleep(1 * time.Second)
}
func (celo *CognitiveEventLoopOrchestrator) GetMetrics() map[string]interface{} {
celo.mu.RLock()
defer celo.mu.RUnlock()
return map[string]interface{}{
"events_triggered":   celo.eventsTriggered,
"thoughts_triggered": celo.thoughtsTriggered,
"goals_generated":    celo.goalsGenerated,
"cycles_completed":   celo.cyclesCompleted,
"cognitive_load":     celo.cognitiveLoad,
"fatigue_level":      celo.fatigueLevel,
"awareness_level":    celo.awarenessLevel,
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