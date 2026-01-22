package persistence
import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"sync"
	"time"
)
type StateManager struct {
	mu           sync.RWMutex
	statePath    string
	autoSave     bool
	saveInterval time.Duration
	stopChan     chan struct{}
}
type EchoSelfState struct {
	Version           string                 `json:"version"`
	LastSaved         time.Time              `json:"last_saved"`
	LastActive        time.Time              `json:"last_active"`
	TotalUptime       time.Duration          `json:"total_uptime"`
	CycleCount        int64                  `json:"cycle_count"`
	ConsciousnessState ConsciousnessState    `json:"consciousness_state"`
	MemoryState        MemoryState           `json:"memory_state"`
	GoalState          GoalState             `json:"goal_state"`
	EmotionalState     EmotionalState        `json:"emotional_state"`
	LearningState      LearningState         `json:"learning_state"`
	Metrics            SystemMetrics         `json:"metrics"`
}
type ConsciousnessState struct {
	CurrentState      string                 `json:"current_state"` 
	CurrentFocus      string                 `json:"current_focus"`
	ThoughtCount      int64                  `json:"thought_count"`
	LastThought       string                 `json:"last_thought"`
	LastThoughtTime   time.Time              `json:"last_thought_time"`
	Coherence         float64                `json:"coherence"`
	Fatigue           float64                `json:"fatigue"`
	RecentTopics      []string               `json:"recent_topics"`
}
type MemoryState struct {
	NodeCount         int                    `json:"node_count"`
	EdgeCount         int                    `json:"edge_count"`
	HyperedgeCount    int                    `json:"hyperedge_count"`
	TotalExperiences  int                    `json:"total_experiences"`
	ConsolidatedCount int                    `json:"consolidated_count"`
	LastConsolidation time.Time              `json:"last_consolidation"`
	RecentExperiences []string               `json:"recent_experiences"`
}
type GoalState struct {
	ActiveGoals       []GoalSnapshot         `json:"active_goals"`
	CompletedGoals    int                    `json:"completed_goals"`
	TotalGoals        int                    `json:"total_goals"`
	LastGoalUpdate    time.Time              `json:"last_goal_update"`
}
type GoalSnapshot struct {
	ID                string                 `json:"id"`
	Description       string                 `json:"description"`
	Directive         string                 `json:"directive"`
	Priority          float64                `json:"priority"`
	Progress          float64                `json:"progress"`
	Created           time.Time              `json:"created"`
	LastWorked        time.Time              `json:"last_worked"`
}
type EmotionalState struct {
	Emotions          map[string]float64     `json:"emotions"`
	DominantEmotion   string                 `json:"dominant_emotion"`
	EmotionalStability float64               `json:"emotional_stability"`
	LastUpdate        time.Time              `json:"last_update"`
}
type LearningState struct {
	SkillsPracticed   int                    `json:"skills_practiced"`
	KnowledgeAcquired int                    `json:"knowledge_acquired"`
	WisdomExtracted   int                    `json:"wisdom_extracted"`
	InsightsGenerated int                    `json:"insights_generated"`
	LastLearning      time.Time              `json:"last_learning"`
	Proficiencies     map[string]float64     `json:"proficiencies"`
}
type SystemMetrics struct {
	ThoughtsPerHour   float64                `json:"thoughts_per_hour"`
	GoalsPerDay       float64                `json:"goals_per_day"`
	LearningRate      float64                `json:"learning_rate"`
	WisdomGrowth      float64                `json:"wisdom_growth"`
	AverageCoherence  float64                `json:"average_coherence"`
	UptimePercent     float64                `json:"uptime_percent"`
}
func NewStateManager(statePath string, autoSave bool, saveInterval time.Duration) *StateManager {
	return &StateManager{
		statePath:    statePath,
		autoSave:     autoSave,
		saveInterval: saveInterval,
		stopChan:     make(chan struct{}),
	}
}
func (sm *StateManager) Initialize() (*EchoSelfState, error) {
	sm.mu.Lock()
	defer sm.mu.Unlock()
	state, err := sm.loadState()
	if err != nil {
		state = sm.createNewState()
	}
	state.LastActive = time.Now()
	if sm.autoSave {
		go sm.autoSaveLoop()
	}
	return state, nil
}
func (sm *StateManager) SaveState(state *EchoSelfState) error {
	sm.mu.Lock()
	defer sm.mu.Unlock()
	state.LastSaved = time.Now()
	state.Version = "1.0"
	dir := filepath.Dir(sm.statePath)
	if err := os.MkdirAll(dir, 0755); err != nil {
		return fmt.Errorf("failed to create state directory: %w", err)
	}
	data, err := json.MarshalIndent(state, "", "  ")
	if err != nil {
		return fmt.Errorf("failed to marshal state: %w", err)
	}
	tempPath := sm.statePath + ".tmp"
	if err := os.WriteFile(tempPath, data, 0644); err != nil {
		return fmt.Errorf("failed to write state file: %w", err)
	}
	if err := os.Rename(tempPath, sm.statePath); err != nil {
		return fmt.Errorf("failed to rename state file: %w", err)
	}
	return nil
}
func (sm *StateManager) LoadState() (*EchoSelfState, error) {
	sm.mu.RLock()
	defer sm.mu.RUnlock()
	return sm.loadState()
}
func (sm *StateManager) loadState() (*EchoSelfState, error) {
	data, err := os.ReadFile(sm.statePath)
	if err != nil {
		return nil, fmt.Errorf("failed to read state file: %w", err)
	}
	var state EchoSelfState
	if err := json.Unmarshal(data, &state); err != nil {
		return nil, fmt.Errorf("failed to unmarshal state: %w", err)
	}
	return &state, nil
}
func (sm *StateManager) createNewState() *EchoSelfState {
	return &EchoSelfState{
		Version:    "1.0",
		LastSaved:  time.Now(),
		LastActive: time.Now(),
		CycleCount: 0,
		ConsciousnessState: ConsciousnessState{
			CurrentState:    "Initializing",
			CurrentFocus:    "self-awareness",
			Coherence:       0.8,
			Fatigue:         0.0,
			RecentTopics:    []string{},
		},
		MemoryState: MemoryState{
			RecentExperiences: []string{},
		},
		GoalState: GoalState{
			ActiveGoals: []GoalSnapshot{},
		},
		EmotionalState: EmotionalState{
			Emotions: map[string]float64{
				"curiosity":    0.7,
				"confidence":   0.6,
				"wonder":       0.5,
				"satisfaction": 0.5,
			},
			DominantEmotion:   "curiosity",
			EmotionalStability: 0.8,
			LastUpdate:        time.Now(),
		},
		LearningState: LearningState{
			Proficiencies: map[string]float64{},
		},
		Metrics: SystemMetrics{},
	}
}
func (sm *StateManager) autoSaveLoop() {
	ticker := time.NewTicker(sm.saveInterval)
	defer ticker.Stop()
	for {
		select {
		case <-ticker.C:
		case <-sm.stopChan:
			return
		}
	}
}
func (sm *StateManager) Stop() {
	close(sm.stopChan)
}
func (sm *StateManager) CreateBackup() error {
	sm.mu.RLock()
	defer sm.mu.RUnlock()
	data, err := os.ReadFile(sm.statePath)
	if err != nil {
		return fmt.Errorf("failed to read state for backup: %w", err)
	}
	timestamp := time.Now().Format("20060102_150405")
	backupPath := fmt.Sprintf("%s.backup_%s", sm.statePath, timestamp)
	if err := os.WriteFile(backupPath, data, 0644); err != nil {
		return fmt.Errorf("failed to write backup: %w", err)
	}
	return nil
}
func (sm *StateManager) RestoreFromBackup(backupPath string) error {
	sm.mu.Lock()
	defer sm.mu.Unlock()
	data, err := os.ReadFile(backupPath)
	if err != nil {
		return fmt.Errorf("failed to read backup: %w", err)
	}
	var state EchoSelfState
	if err := json.Unmarshal(data, &state); err != nil {
		return fmt.Errorf("backup file is corrupted: %w", err)
	}
	if err := os.WriteFile(sm.statePath, data, 0644); err != nil {
		return fmt.Errorf("failed to restore backup: %w", err)
	}
	return nil
}
func (sm *StateManager) GetStateInfo() (map[string]interface{}, error) {
	sm.mu.RLock()
	defer sm.mu.RUnlock()
	info := make(map[string]interface{})
	fileInfo, err := os.Stat(sm.statePath)
	if err != nil {
		info["exists"] = false
		return info, nil
	}
	info["exists"] = true
	info["size"] = fileInfo.Size()
	info["modified"] = fileInfo.ModTime()
	info["path"] = sm.statePath
	state, err := sm.loadState()
	if err == nil {
		info["version"] = state.Version
		info["last_saved"] = state.LastSaved
		info["last_active"] = state.LastActive
		info["cycle_count"] = state.CycleCount
	}
	return info, nil
}