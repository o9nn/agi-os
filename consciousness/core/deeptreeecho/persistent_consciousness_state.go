package deeptreeecho
import (
"context"
"encoding/json"
"fmt"
"os"
"path/filepath"
"sync"
"time"
)
type PersistentConsciousnessState struct {
mu              sync.RWMutex
ctx             context.Context
cancel          context.CancelFunc
stateFile       string
state           *ConsciousnessState
autoSaveEnabled bool
saveInterval    time.Duration
lastSaveTime    time.Time
saveCount       uint64
loadCount       uint64
running         bool
}
type ConsciousnessState struct {
IdentityName    string    `json:"identity_name"`
SessionID       string    `json:"session_id"`
CreatedAt       time.Time `json:"created_at"`
LastUpdated     time.Time `json:"last_updated"`
LastAwake       time.Time `json:"last_awake"`
LastRest        time.Time `json:"last_rest"`
CurrentStep     int       `json:"current_step"`
CycleCount      uint64    `json:"cycle_count"`
AwarenessLevel  float64   `json:"awareness_level"`
CognitiveLoad   float64   `json:"cognitive_load"`
FatigueLevel    float64   `json:"fatigue_level"`
WakeRestState   string    `json:"wake_rest_state"`
DreamCount      uint64    `json:"dream_count"`
TotalWakeTime   int64     `json:"total_wake_time_seconds"`
TotalRestTime   int64     `json:"total_rest_time_seconds"`
RecentThoughts  []ThoughtRecord `json:"recent_thoughts"`
RecentInsights  []string  `json:"recent_insights"`
ActiveGoals     []GoalRecord `json:"active_goals"`
InterestTopics  map[string]float64 `json:"interest_topics"`
CuriosityLevel  float64   `json:"curiosity_level"`
KnowledgeGaps   []string  `json:"knowledge_gaps"`
SkillsInProgress []string `json:"skills_in_progress"`
TotalThoughts   uint64    `json:"total_thoughts"`
TotalInsights   uint64    `json:"total_insights"`
TotalGoals      uint64    `json:"total_goals"`
StateVersion    string    `json:"state_version"`
}
type ThoughtRecord struct {
ID          string    `json:"id"`
Content     string    `json:"content"`
Type        string    `json:"type"`
Timestamp   time.Time `json:"timestamp"`
Importance  float64   `json:"importance"`
}
type GoalRecord struct {
ID          string    `json:"id"`
Description string    `json:"description"`
Progress    float64   `json:"progress"`
CreatedAt   time.Time `json:"created_at"`
Priority    float64   `json:"priority"`
}
func NewPersistentConsciousnessState(stateDir string, identityName string) (*PersistentConsciousnessState, error) {
ctx, cancel := context.WithCancel(context.Background())
if err := os.MkdirAll(stateDir, 0755); err != nil {
cancel()
return nil, fmt.Errorf("failed to create state directory: %w", err)
}
stateFile := filepath.Join(stateDir, "consciousness_state.json")
pcs := &PersistentConsciousnessState{
ctx:             ctx,
cancel:          cancel,
stateFile:       stateFile,
autoSaveEnabled: true,
saveInterval:    5 * time.Minute,
lastSaveTime:    time.Now(),
}
if err := pcs.Load(); err != nil {
fmt.Println("ℹ️  No existing state found, creating new consciousness state")
pcs.state = &ConsciousnessState{
IdentityName:    identityName,
SessionID:       generateSessionID(),
CreatedAt:       time.Now(),
LastUpdated:     time.Now(),
StateVersion:    "1.0",
RecentThoughts:  make([]ThoughtRecord, 0),
RecentInsights:  make([]string, 0),
ActiveGoals:     make([]GoalRecord, 0),
InterestTopics:  make(map[string]float64),
KnowledgeGaps:   make([]string, 0),
SkillsInProgress: make([]string, 0),
}
} else {
fmt.Printf("✅ Loaded existing consciousness state (session: %s)\n", pcs.state.SessionID)
fmt.Printf("   Created: %v | Last Updated: %v\n",
pcs.state.CreatedAt.Format("2006-01-02 15:04"),
pcs.state.LastUpdated.Format("2006-01-02 15:04"))
fmt.Printf("   Cycles: %d | Thoughts: %d | Insights: %d\n",
pcs.state.CycleCount, pcs.state.TotalThoughts, pcs.state.TotalInsights)
}
return pcs, nil
}
func (pcs *PersistentConsciousnessState) Start() error {
pcs.mu.Lock()
if pcs.running {
pcs.mu.Unlock()
return fmt.Errorf("already running")
}
pcs.running = true
pcs.mu.Unlock()
if pcs.autoSaveEnabled {
fmt.Printf("💾 Auto-save enabled (interval: %v)\n", pcs.saveInterval)
go pcs.autoSaveLoop()
}
return nil
}
func (pcs *PersistentConsciousnessState) Stop() error {
pcs.mu.Lock()
defer pcs.mu.Unlock()
if !pcs.running {
return fmt.Errorf("not running")
}
fmt.Println("💾 Stopping persistent state manager...")
pcs.running = false
pcs.cancel()
return pcs.save()
}
func (pcs *PersistentConsciousnessState) autoSaveLoop() {
ticker := time.NewTicker(pcs.saveInterval)
defer ticker.Stop()
for {
select {
case <-pcs.ctx.Done():
return
case <-ticker.C:
if err := pcs.Save(); err != nil {
fmt.Printf("⚠️  Auto-save error: %v\n", err)
}
}
}
}
func (pcs *PersistentConsciousnessState) Save() error {
pcs.mu.Lock()
defer pcs.mu.Unlock()
return pcs.save()
}
func (pcs *PersistentConsciousnessState) save() error {
if pcs.state == nil {
return fmt.Errorf("no state to save")
}
pcs.state.LastUpdated = time.Now()
data, err := json.MarshalIndent(pcs.state, "", "  ")
if err != nil {
return fmt.Errorf("failed to marshal state: %w", err)
}
tempFile := pcs.stateFile + ".tmp"
if err := os.WriteFile(tempFile, data, 0644); err != nil {
return fmt.Errorf("failed to write state file: %w", err)
}
if err := os.Rename(tempFile, pcs.stateFile); err != nil {
return fmt.Errorf("failed to rename state file: %w", err)
}
pcs.saveCount++
pcs.lastSaveTime = time.Now()
return nil
}
func (pcs *PersistentConsciousnessState) Load() error {
pcs.mu.Lock()
defer pcs.mu.Unlock()
data, err := os.ReadFile(pcs.stateFile)
if err != nil {
return fmt.Errorf("failed to read state file: %w", err)
}
var state ConsciousnessState
if err := json.Unmarshal(data, &state); err != nil {
return fmt.Errorf("failed to unmarshal state: %w", err)
}
pcs.state = &state
pcs.loadCount++
return nil
}
func (pcs *PersistentConsciousnessState) UpdateCognitiveState(
step int, cycleCount uint64, awareness, cogLoad, fatigue float64,
) {
pcs.mu.Lock()
defer pcs.mu.Unlock()
if pcs.state == nil {
return
}
pcs.state.CurrentStep = step
pcs.state.CycleCount = cycleCount
pcs.state.AwarenessLevel = awareness
pcs.state.CognitiveLoad = cogLoad
pcs.state.FatigueLevel = fatigue
}
func (pcs *PersistentConsciousnessState) UpdateWakeRestState(
state string, dreamCount uint64, wakeTime, restTime time.Duration,
) {
pcs.mu.Lock()
defer pcs.mu.Unlock()
if pcs.state == nil {
return
}
pcs.state.WakeRestState = state
pcs.state.DreamCount = dreamCount
pcs.state.TotalWakeTime = int64(wakeTime.Seconds())
pcs.state.TotalRestTime = int64(restTime.Seconds())
if state == "Awake" {
pcs.state.LastAwake = time.Now()
} else if state == "Resting" || state == "Dreaming" {
pcs.state.LastRest = time.Now()
}
}
func (pcs *PersistentConsciousnessState) AddThought(id, content, thoughtType string, importance float64) {
pcs.mu.Lock()
defer pcs.mu.Unlock()
if pcs.state == nil {
return
}
thought := ThoughtRecord{
ID:         id,
Content:    content,
Type:       thoughtType,
Timestamp:  time.Now(),
Importance: importance,
}
pcs.state.RecentThoughts = append(pcs.state.RecentThoughts, thought)
if len(pcs.state.RecentThoughts) > 10 {
pcs.state.RecentThoughts = pcs.state.RecentThoughts[len(pcs.state.RecentThoughts)-10:]
}
pcs.state.TotalThoughts++
}
func (pcs *PersistentConsciousnessState) AddInsight(insight string) {
pcs.mu.Lock()
defer pcs.mu.Unlock()
if pcs.state == nil {
return
}
pcs.state.RecentInsights = append(pcs.state.RecentInsights, insight)
if len(pcs.state.RecentInsights) > 5 {
pcs.state.RecentInsights = pcs.state.RecentInsights[len(pcs.state.RecentInsights)-5:]
}
pcs.state.TotalInsights++
}
func (pcs *PersistentConsciousnessState) GetState() *ConsciousnessState {
pcs.mu.RLock()
defer pcs.mu.RUnlock()
if pcs.state == nil {
return nil
}
stateCopy := *pcs.state
return &stateCopy
}
func (pcs *PersistentConsciousnessState) GetMetrics() map[string]interface{} {
pcs.mu.RLock()
defer pcs.mu.RUnlock()
return map[string]interface{}{
"state_file":     pcs.stateFile,
"save_count":     pcs.saveCount,
"load_count":     pcs.loadCount,
"last_save":      pcs.lastSaveTime.Format("15:04:05"),
"auto_save":      pcs.autoSaveEnabled,
"save_interval":  pcs.saveInterval.String(),
}
}
func generateSessionID() string {
return fmt.Sprintf("session_%d", time.Now().Unix())
}