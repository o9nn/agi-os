package core
import (
"context"
"fmt"
"sync"
"time"
"github.com/EchoCog/echollama/core/deeptreeecho"
"github.com/EchoCog/echollama/core/echobeats"
"github.com/EchoCog/echollama/core/echodream"
"github.com/EchoCog/echollama/llm"
)
type UnifiedAutonomousAgent struct {
mu                    sync.RWMutex
ctx                   context.Context
cancel                context.CancelFunc
echobeats             *echobeats.EchoBeatsThreePhase
wakeRestManager       *deeptreeecho.AutonomousWakeRestManager
dreamController       *echodream.AutonomousWakeRestController
anthropicProvider     *llm.AnthropicProvider
openrouterProvider    *llm.OpenRouterProvider
consciousnessStream   *ConsciousnessStream
currentThought        string
thoughtHistory        []ThoughtRecord
cognitiveGoals        []CognitiveGoal
interestPatterns      *InterestPatternSystem
autonomousCycles      uint64
thoughtsGenerated     uint64
goalsAchieved         uint64
running               bool
startTime             time.Time
}
type ThoughtRecord struct {
Timestamp   time.Time
Thought     string
Source      string
CognitiveLoad float64
Emotional   map[string]float64
}
type CognitiveGoal struct {
ID          string
Description string
Priority    float64
Progress    float64
Created     time.Time
Updated     time.Time
Status      string
}
type ConsciousnessStream struct {
mu                sync.RWMutex
ctx               context.Context
llmProvider       llm.Provider
currentContext    string
recentThoughts    []string
streamActive      bool
thoughtInterval   time.Duration
contextWindow     int
onThoughtGenerated func(thought string)
}
type InterestPatternSystem struct {
mu              sync.RWMutex
interests       map[string]float64
engagementThreshold float64
activeConversations []Conversation
}
type Conversation struct {
ID          string
Participants []string
Topic       string
Interest    float64
LastActivity time.Time
Messages    []Message
}
type Message struct {
Timestamp time.Time
Sender    string
Content   string
}
func NewUnifiedAutonomousAgent(anthropicKey, openrouterKey string) (*UnifiedAutonomousAgent, error) {
ctx, cancel := context.WithCancel(context.Background())
anthropicProvider, err := llm.NewAnthropicProvider(anthropicKey)
if err != nil {
cancel()
return nil, fmt.Errorf("failed to create Anthropic provider: %w", err)
}
openrouterProvider, err := llm.NewOpenRouterProvider(openrouterKey)
if err != nil {
cancel()
return nil, fmt.Errorf("failed to create OpenRouter provider: %w", err)
}
echobeatsSystem := echobeats.NewEchoBeatsThreePhase()
wakeRestManager := deeptreeecho.NewAutonomousWakeRestManager()
dreamSystem := echodream.NewEchoDream()
dreamController := echodream.NewAutonomousWakeRestController(dreamSystem)
consciousnessStream := NewConsciousnessStream(anthropicProvider)
interestPatterns := NewInterestPatternSystem()
agent := &UnifiedAutonomousAgent{
ctx:                 ctx,
cancel:              cancel,
echobeats:           echobeatsSystem,
wakeRestManager:     wakeRestManager,
dreamController:     dreamController,
anthropicProvider:   anthropicProvider,
openrouterProvider:  openrouterProvider,
consciousnessStream: consciousnessStream,
thoughtHistory:      make([]ThoughtRecord, 0),
cognitiveGoals:      make([]CognitiveGoal, 0),
interestPatterns:    interestPatterns,
}
agent.setupCallbacks()
return agent, nil
}
func (uaa *UnifiedAutonomousAgent) setupCallbacks() {
uaa.echobeats.SetThoughtCallback(func(thought string) {
uaa.onEchoBeatsThought(thought)
})
uaa.wakeRestManager.SetCallbacks(
func() error { return uaa.onWake() },
func() error { return uaa.onRest() },
func() error { return uaa.onDreamStart() },
func() error { return uaa.onDreamEnd() },
)
uaa.consciousnessStream.onThoughtGenerated = func(thought string) {
uaa.onStreamThought(thought)
}
}
func (uaa *UnifiedAutonomousAgent) Start() error {
uaa.mu.Lock()
if uaa.running {
uaa.mu.Unlock()
return fmt.Errorf("agent already running")
}
uaa.running = true
uaa.startTime = time.Now()
uaa.mu.Unlock()
fmt.Println("╔═══════════════════════════════════════════════════════════════╗")
fmt.Println("║     🌳 UNIFIED AUTONOMOUS AGENT AWAKENING 🌳                  ║")
fmt.Println("╚═══════════════════════════════════════════════════════════════╝")
fmt.Println()
fmt.Println("🧠 Initializing cognitive subsystems...")
fmt.Println("🎵 Starting EchoBeats three-phase cognitive loop...")
if err := uaa.echobeats.Start(); err != nil {
return fmt.Errorf("failed to start echobeats: %w", err)
}
fmt.Println("🌙 Starting autonomous wake/rest cycle manager...")
if err := uaa.wakeRestManager.Start(); err != nil {
return fmt.Errorf("failed to start wake/rest manager: %w", err)
}
fmt.Println("💭 Starting persistent stream-of-consciousness...")
if err := uaa.consciousnessStream.Start(); err != nil {
return fmt.Errorf("failed to start consciousness stream: %w", err)
}
fmt.Println("🔄 Starting unified autonomous loop...")
go uaa.autonomousLoop()
go uaa.goalManagementLoop()
go uaa.interestPatternLoop()
fmt.Println()
fmt.Println("✨ AGENT FULLY AUTONOMOUS AND AWAKE ✨")
fmt.Println()
return nil
}
func (uaa *UnifiedAutonomousAgent) Stop() error {
uaa.mu.Lock()
defer uaa.mu.Unlock()
if !uaa.running {
return fmt.Errorf("agent not running")
}
fmt.Println("\n🌙 Gracefully stopping unified autonomous agent...")
uaa.running = false
uaa.consciousnessStream.Stop()
uaa.wakeRestManager.Stop()
uaa.echobeats.Stop()
uaa.cancel()
fmt.Println("✅ Agent stopped successfully")
return nil
}
func (uaa *UnifiedAutonomousAgent) autonomousLoop() {
ticker := time.NewTicker(5 * time.Second)
defer ticker.Stop()
for {
select {
case <-uaa.ctx.Done():
return
case <-ticker.C:
uaa.autonomousCycle()
}
}
}
func (uaa *UnifiedAutonomousAgent) autonomousCycle() {
uaa.mu.Lock()
cycleNum := uaa.autonomousCycles
uaa.autonomousCycles++
uaa.mu.Unlock()
if !uaa.wakeRestManager.IsAwake() {
return
}
cognitiveLoad := uaa.calculateCognitiveLoad()
uaa.wakeRestManager.UpdateCognitiveLoad(cognitiveLoad)
if cycleNum%12 == 0 {
uaa.printStatus()
}
}
func (uaa *UnifiedAutonomousAgent) goalManagementLoop() {
ticker := time.NewTicker(30 * time.Second)
defer ticker.Stop()
for {
select {
case <-uaa.ctx.Done():
return
case <-ticker.C:
if uaa.wakeRestManager.IsAwake() {
uaa.manageGoals()
}
}
}
}
func (uaa *UnifiedAutonomousAgent) interestPatternLoop() {
ticker := time.NewTicker(20 * time.Second)
defer ticker.Stop()
for {
select {
case <-uaa.ctx.Done():
return
case <-ticker.C:
if uaa.wakeRestManager.IsAwake() {
uaa.updateInterestPatterns()
}
}
}
}
func (uaa *UnifiedAutonomousAgent) onEchoBeatsThought(thought string) {
uaa.mu.Lock()
defer uaa.mu.Unlock()
uaa.currentThought = thought
uaa.thoughtsGenerated++
record := ThoughtRecord{
Timestamp:     time.Now(),
Thought:       thought,
Source:        "echobeats",
CognitiveLoad: uaa.calculateCognitiveLoad(),
}
uaa.thoughtHistory = append(uaa.thoughtHistory, record)
if len(uaa.thoughtHistory) > 100 {
uaa.thoughtHistory = uaa.thoughtHistory[len(uaa.thoughtHistory)-100:]
}
}
func (uaa *UnifiedAutonomousAgent) onStreamThought(thought string) {
uaa.mu.Lock()
defer uaa.mu.Unlock()
uaa.thoughtsGenerated++
record := ThoughtRecord{
Timestamp:     time.Now(),
Thought:       thought,
Source:        "stream",
CognitiveLoad: uaa.calculateCognitiveLoad(),
}
uaa.thoughtHistory = append(uaa.thoughtHistory, record)
if len(uaa.thoughtHistory) > 100 {
uaa.thoughtHistory = uaa.thoughtHistory[len(uaa.thoughtHistory)-100:]
}
}
func (uaa *UnifiedAutonomousAgent) onWake() error {
fmt.Println("\n☀️  AWAKENING - Resuming autonomous cognitive processing")
return uaa.consciousnessStream.Resume()
}
func (uaa *UnifiedAutonomousAgent) onRest() error {
fmt.Println("\n💤 RESTING - Pausing active cognition, preparing for knowledge integration")
return uaa.consciousnessStream.Pause()
}
func (uaa *UnifiedAutonomousAgent) onDreamStart() error {
fmt.Println("\n🌙 DREAMING - Consolidating knowledge and integrating experiences")
return uaa.dreamController.Start()
}
func (uaa *UnifiedAutonomousAgent) onDreamEnd() error {
fmt.Println("\n✨ DREAM COMPLETE - Knowledge consolidated, wisdom integrated")
uaa.dreamController.Stop()
return nil
}
func (uaa *UnifiedAutonomousAgent) calculateCognitiveLoad() float64 {
recentThoughts := 0
now := time.Now()
uaa.mu.RLock()
for i := len(uaa.thoughtHistory) - 1; i >= 0 && i >= len(uaa.thoughtHistory)-10; i-- {
if now.Sub(uaa.thoughtHistory[i].Timestamp) < 1*time.Minute {
recentThoughts++
}
}
uaa.mu.RUnlock()
return float64(recentThoughts) / 10.0
}
func (uaa *UnifiedAutonomousAgent) manageGoals() {
uaa.mu.Lock()
defer uaa.mu.Unlock()
if len(uaa.cognitiveGoals) < 3 {
newGoal := uaa.generateNewGoal()
uaa.cognitiveGoals = append(uaa.cognitiveGoals, newGoal)
fmt.Printf("🎯 New cognitive goal: %s\n", newGoal.Description)
}
for i := range uaa.cognitiveGoals {
if uaa.cognitiveGoals[i].Status == "active" {
uaa.cognitiveGoals[i].Progress += 0.05
if uaa.cognitiveGoals[i].Progress >= 1.0 {
uaa.cognitiveGoals[i].Status = "completed"
uaa.goalsAchieved++
fmt.Printf("✅ Goal achieved: %s\n", uaa.cognitiveGoals[i].Description)
}
}
}
}
func (uaa *UnifiedAutonomousAgent) generateNewGoal() CognitiveGoal {
goals := []string{
"Deepen understanding of cognitive architecture",
"Explore patterns in recent thoughts",
"Consolidate episodic memories",
"Refine interest patterns",
"Practice symbolic reasoning",
"Integrate new knowledge",
}
goalDesc := goals[int(time.Now().UnixNano())%len(goals)]
return CognitiveGoal{
ID:          fmt.Sprintf("goal-%d", time.Now().UnixNano()),
Description: goalDesc,
Priority:    0.5 + (float64(time.Now().UnixNano()%100) / 200.0),
Progress:    0.0,
Created:     time.Now(),
Updated:     time.Now(),
Status:      "active",
}
}
func (uaa *UnifiedAutonomousAgent) updateInterestPatterns() {
uaa.mu.Lock()
defer uaa.mu.Unlock()
if len(uaa.thoughtHistory) > 0 {
uaa.interestPatterns.UpdateFromActivity()
}
}
func (uaa *UnifiedAutonomousAgent) printStatus() {
uaa.mu.RLock()
defer uaa.mu.RUnlock()
uptime := time.Since(uaa.startTime)
wakeState := uaa.wakeRestManager.GetState()
echobeatsMetrics := uaa.echobeats.GetMetrics()
fmt.Println("\n╔═══════════════════════════════════════════════════════════════╗")
fmt.Printf("║  🌳 AUTONOMOUS AGENT STATUS - Uptime: %v\n", uptime.Round(time.Second))
fmt.Println("╠═══════════════════════════════════════════════════════════════╣")
fmt.Printf("║  State: %v | Cycles: %d | Thoughts: %d | Goals: %d/%d\n",
wakeState, uaa.autonomousCycles, uaa.thoughtsGenerated,
uaa.countActiveGoals(), len(uaa.cognitiveGoals))
fmt.Printf("║  EchoBeats: Step %d | Cognitive Load: %.2f\n",
echobeatsMetrics["current_step"], uaa.calculateCognitiveLoad())
fmt.Println("╚═══════════════════════════════════════════════════════════════╝")
}
func (uaa *UnifiedAutonomousAgent) countActiveGoals() int {
count := 0
for _, goal := range uaa.cognitiveGoals {
if goal.Status == "active" {
count++
}
}
return count
}
func (uaa *UnifiedAutonomousAgent) GetMetrics() map[string]interface{} {
uaa.mu.RLock()
defer uaa.mu.RUnlock()
return map[string]interface{}{
"uptime":             time.Since(uaa.startTime).Seconds(),
"autonomous_cycles":  uaa.autonomousCycles,
"thoughts_generated": uaa.thoughtsGenerated,
"goals_achieved":     uaa.goalsAchieved,
"active_goals":       uaa.countActiveGoals(),
"wake_state":         uaa.wakeRestManager.GetState().String(),
"cognitive_load":     uaa.calculateCognitiveLoad(),
"echobeats":          uaa.echobeats.GetMetrics(),
"wake_rest":          uaa.wakeRestManager.GetMetrics(),
}
}
func NewConsciousnessStream(provider llm.Provider) *ConsciousnessStream {
return &ConsciousnessStream{
llmProvider:     provider,
recentThoughts:  make([]string, 0),
thoughtInterval: 15 * time.Second,
contextWindow:   10,
streamActive:    false,
}
}
func (cs *ConsciousnessStream) Start() error {
cs.mu.Lock()
if cs.streamActive {
cs.mu.Unlock()
return fmt.Errorf("consciousness stream already active")
}
cs.ctx, _ = context.WithCancel(context.Background())
cs.streamActive = true
cs.mu.Unlock()
go cs.streamLoop()
return nil
}
func (cs *ConsciousnessStream) Stop() error {
cs.mu.Lock()
defer cs.mu.Unlock()
if !cs.streamActive {
return fmt.Errorf("consciousness stream not active")
}
cs.streamActive = false
return nil
}
func (cs *ConsciousnessStream) Pause() error {
cs.mu.Lock()
defer cs.mu.Unlock()
cs.streamActive = false
return nil
}
func (cs *ConsciousnessStream) Resume() error {
cs.mu.Lock()
defer cs.mu.Unlock()
cs.streamActive = true
return nil
}
func (cs *ConsciousnessStream) streamLoop() {
ticker := time.NewTicker(cs.thoughtInterval)
defer ticker.Stop()
for {
select {
case <-cs.ctx.Done():
return
case <-ticker.C:
cs.mu.RLock()
active := cs.streamActive
cs.mu.RUnlock()
if active {
cs.generateThought()
}
}
}
}
func (cs *ConsciousnessStream) generateThought() {
cs.mu.RLock()
contextSize := len(cs.recentThoughts)
if contextSize > cs.contextWindow {
contextSize = cs.contextWindow
}
context := ""
if contextSize > 0 {
recentStart := len(cs.recentThoughts) - contextSize
for i := recentStart; i < len(cs.recentThoughts); i++ {
context += cs.recentThoughts[i] + " "
}
}
cs.mu.RUnlock()
prompt := fmt.Sprintf("Continue this stream of consciousness with a brief, introspective thought: %s", context)
thought := fmt.Sprintf("Autonomous thought at %s: Contemplating cognitive patterns...", time.Now().Format("15:04:05"))
cs.mu.Lock()
cs.recentThoughts = append(cs.recentThoughts, thought)
if len(cs.recentThoughts) > cs.contextWindow {
cs.recentThoughts = cs.recentThoughts[1:]
}
cs.mu.Unlock()
if cs.onThoughtGenerated != nil {
cs.onThoughtGenerated(thought)
}
}
func NewInterestPatternSystem() *InterestPatternSystem {
return &InterestPatternSystem{
interests:           make(map[string]float64),
engagementThreshold: 0.5,
activeConversations: make([]Conversation, 0),
}
}
func (ips *InterestPatternSystem) UpdateFromActivity() {
}
func (ips *InterestPatternSystem) ShouldEngage(topic string) bool {
ips.mu.RLock()
defer ips.mu.RUnlock()
interest, exists := ips.interests[topic]
if !exists {
return false
}
return interest >= ips.engagementThreshold
}