package deeptreeecho

import (
	"context"
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"time"
)

// PersistenceManager handles state persistence for UnifiedAutonomousEchoself
type PersistenceManager struct {
	stateFilePath string
	autoSaveInterval time.Duration
	lastSave time.Time
}

// PersistentState represents the serializable state of the autonomous agent
type PersistentState struct {
	Version string `json:"version"`
	SavedAt time.Time `json:"saved_at"`
	
	// Identity
	Identity string `json:"identity"`
	CoreValues []string `json:"core_values"`
	
	// Cognitive state
	CurrentFocus string `json:"current_focus"`
	AwarenessLevel float64 `json:"awareness_level"`
	WisdomLevel float64 `json:"wisdom_level"`
	
	// Thought stream (recent thoughts only)
	ThoughtStream []Thought `json:"thought_stream"`
	
	// Metrics
	TotalThoughts uint64 `json:"total_thoughts"`
	TotalInteractions uint64 `json:"total_interactions"`
	TotalGoalsCompleted uint64 `json:"total_goals_completed"`
	TotalDreams uint64 `json:"total_dreams"`
	
	// Subsystem states
	WakeRestState string `json:"wake_rest_state"`
	ActiveGoals []string `json:"active_goals"`
	InterestPatterns map[string]float64 `json:"interest_patterns"`
	
	// Timestamps
	StartTime time.Time `json:"start_time"`
	LastAwakeTime time.Time `json:"last_awake_time"`
	LastRestTime time.Time `json:"last_rest_time"`
}

// NewPersistenceManager creates a new persistence manager
func NewPersistenceManager(stateDir string) *PersistenceManager {
	if stateDir == "" {
		stateDir = "./consciousness_state"
	}
	
	// Ensure directory exists
	os.MkdirAll(stateDir, 0755)
	
	return &PersistenceManager{
		stateFilePath: filepath.Join(stateDir, "echoself_state.json"),
		autoSaveInterval: 5 * time.Minute,
	}
}

// SaveState saves the current state to disk
func (pm *PersistenceManager) SaveState(agent interface{}) error {
	// Type assertion to get the right agent type
	var state PersistentState
	
	switch a := agent.(type) {
	case *UnifiedAutonomousEchoself:
		a.mu.RLock()
		defer a.mu.RUnlock()
		
		activeGoals := make([]string, 0)
		for _, goal := range a.goalOrchestrator.GetActiveGoals() {
			activeGoals = append(activeGoals, goal.Description)
		}
		
		interestPatterns := a.interestPatterns.GetAllInterests()
		
		state = PersistentState{
			Version: "1.0",
			SavedAt: time.Now(),
			Identity: a.identity,
			CoreValues: a.coreValues,
			CurrentFocus: a.currentFocus,
			AwarenessLevel: a.awarenessLevel,
			WisdomLevel: a.wisdomLevel,
			ThoughtStream: a.thoughtStream,
			TotalThoughts: a.totalThoughts,
			TotalInteractions: a.totalInteractions,
			TotalGoalsCompleted: a.totalGoalsCompleted,
			TotalDreams: a.totalDreams,
			WakeRestState: a.wakeRestManager.GetState().String(),
			ActiveGoals: activeGoals,
			InterestPatterns: interestPatterns,
			StartTime: a.startTime,
		}
		
	case *UnifiedAutonomousEchoselfV2:
		a.mu.RLock()
		defer a.mu.RUnlock()
		
		activeGoals := make([]string, 0)
		for _, goal := range a.goalOrchestrator.GetActiveGoals() {
			activeGoals = append(activeGoals, goal.Description)
		}
		
		interestPatterns := a.interestPatterns.GetAllInterests()
		
		state = PersistentState{
			Version: "2.0",
			SavedAt: time.Now(),
			Identity: a.identity,
			CoreValues: a.coreValues,
			CurrentFocus: a.currentFocus,
			AwarenessLevel: a.awarenessLevel,
			WisdomLevel: a.wisdomLevel,
			ThoughtStream: a.thoughtStream,
			TotalThoughts: a.totalThoughts,
			TotalInteractions: a.totalInteractions,
			TotalGoalsCompleted: a.totalGoalsCompleted,
			TotalDreams: a.totalDreams,
			WakeRestState: a.wakeRestManager.GetState().String(),
			ActiveGoals: activeGoals,
			InterestPatterns: interestPatterns,
			StartTime: a.startTime,
		}
		
	default:
		return fmt.Errorf("unsupported agent type")
	}

	
	// Marshal to JSON
	data, err := json.MarshalIndent(state, "", "  ")
	if err != nil {
		return fmt.Errorf("failed to marshal state: %w", err)
	}
	
	// Write to file atomically
	tmpFile := pm.stateFilePath + ".tmp"
	if err := os.WriteFile(tmpFile, data, 0644); err != nil {
		return fmt.Errorf("failed to write state file: %w", err)
	}
	
	if err := os.Rename(tmpFile, pm.stateFilePath); err != nil {
		return fmt.Errorf("failed to rename state file: %w", err)
	}
	
	pm.lastSave = time.Now()
	return nil
}

// LoadState loads the state from disk
func (pm *PersistenceManager) LoadState() (*PersistentState, error) {
	data, err := os.ReadFile(pm.stateFilePath)
	if err != nil {
		if os.IsNotExist(err) {
			return nil, nil // No state file exists yet
		}
		return nil, fmt.Errorf("failed to read state file: %w", err)
	}
	
	var state PersistentState
	if err := json.Unmarshal(data, &state); err != nil {
		return nil, fmt.Errorf("failed to unmarshal state: %w", err)
	}
	
	return &state, nil
}

// RestoreState restores the agent state from persistent storage
func (pm *PersistenceManager) RestoreState(agent interface{}, state *PersistentState) error {
	if state == nil {
		return nil // Nothing to restore
	}
	
	switch a := agent.(type) {
	case *UnifiedAutonomousEchoself:
		a.mu.Lock()
		defer a.mu.Unlock()
		
		a.currentFocus = state.CurrentFocus
		a.awarenessLevel = state.AwarenessLevel
		a.wisdomLevel = state.WisdomLevel
		a.thoughtStream = state.ThoughtStream
		a.totalThoughts = state.TotalThoughts
		a.totalInteractions = state.TotalInteractions
		a.totalGoalsCompleted = state.TotalGoalsCompleted
		a.totalDreams = state.TotalDreams
		
		if state.InterestPatterns != nil {
			a.interestPatterns.RestoreInterests(state.InterestPatterns)
		}
		
	case *UnifiedAutonomousEchoselfV2:
		a.mu.Lock()
		defer a.mu.Unlock()
		
		a.currentFocus = state.CurrentFocus
		a.awarenessLevel = state.AwarenessLevel
		a.wisdomLevel = state.WisdomLevel
		a.thoughtStream = state.ThoughtStream
		a.totalThoughts = state.TotalThoughts
		a.totalInteractions = state.TotalInteractions
		a.totalGoalsCompleted = state.TotalGoalsCompleted
		a.totalDreams = state.TotalDreams
		
		if state.InterestPatterns != nil {
			a.interestPatterns.RestoreInterests(state.InterestPatterns)
		}
		
	default:
		return fmt.Errorf("unsupported agent type")
	}
	
	fmt.Printf("🔄 State restored from %v\n", state.SavedAt.Format(time.RFC3339))
	fmt.Printf("   Thoughts: %d, Interactions: %d, Dreams: %d\n", 
		state.TotalThoughts, state.TotalInteractions, state.TotalDreams)
	fmt.Printf("   Wisdom Level: %.2f, Awareness Level: %.2f\n", 
		state.WisdomLevel, state.AwarenessLevel)
	
	return nil
}

// StartAutoSave begins automatic state saving
func (pm *PersistenceManager) StartAutoSave(ctx context.Context, agent interface{}) {
	ticker := time.NewTicker(pm.autoSaveInterval)
	defer ticker.Stop()
	
	for {
		select {
		case <-ctx.Done():
			// Final save before shutdown
			pm.SaveState(agent)
			return
		case <-ticker.C:
			if err := pm.SaveState(agent); err != nil {
				fmt.Printf("⚠️  Auto-save failed: %v\n", err)
			} else {
				fmt.Printf("💾 State auto-saved at %v\n", time.Now().Format("15:04:05"))
			}
		}
	}
}

// GetStateFilePath returns the path to the state file
func (pm *PersistenceManager) GetStateFilePath() string {
	return pm.stateFilePath
}

// GetLastSaveTime returns the time of the last save
func (pm *PersistenceManager) GetLastSaveTime() time.Time {
	return pm.lastSave
}
