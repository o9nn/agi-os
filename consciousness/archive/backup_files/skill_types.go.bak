package deeptreeecho

import (
	"fmt"
	"sync"
	"time"
)

// PracticeSession is defined in skill_practice_enhanced.go
// Using that definition for consistency across the skill practice system

// SkillRegistryEnhanced manages skills with practice history tracking
type SkillRegistryEnhanced struct {
	mu              sync.RWMutex
	skills          map[string]*Skill
	practiceHistory []*PracticeSession
	totalPracticeTime time.Duration
}

// NewSkillRegistryEnhanced creates a new enhanced skill registry
func NewSkillRegistryEnhanced() *SkillRegistryEnhanced {
	return &SkillRegistryEnhanced{
		skills:          make(map[string]*Skill),
		practiceHistory: make([]*PracticeSession, 0),
		totalPracticeTime: 0,
	}
}

// RegisterSkill adds a skill to the registry
func (sr *SkillRegistryEnhanced) RegisterSkill(skill *Skill) {
	sr.mu.Lock()
	defer sr.mu.Unlock()
	sr.skills[skill.ID] = skill
}

// GetSkill retrieves a skill by ID
func (sr *SkillRegistryEnhanced) GetSkill(id string) (*Skill, bool) {
	sr.mu.RLock()
	defer sr.mu.RUnlock()
	skill, exists := sr.skills[id]
	return skill, exists
}

// RecordPracticeSession records a new practice session
func (sr *SkillRegistryEnhanced) RecordPracticeSession(session *PracticeSession) {
	sr.mu.Lock()
	defer sr.mu.Unlock()
	
	if session.ID == "" {
		session.ID = generateID()
	}
	
	sr.practiceHistory = append(sr.practiceHistory, session)
	sessionDuration := session.EndTime.Sub(session.StartTime)
	sr.totalPracticeTime += sessionDuration
	
	// Update skill proficiency based on practice
	if skill, exists := sr.skills[session.SkillID]; exists {
		// Improve proficiency based on practice performance
		improvement := session.Performance * 0.01
		skill.Proficiency += improvement
		if skill.Proficiency > 1.0 {
			skill.Proficiency = 1.0
		}
		skill.LastPracticed = session.EndTime
		skill.PracticeCount++
	}
}

// GetPracticeHistory returns all practice sessions for a skill
func (sr *SkillRegistryEnhanced) GetPracticeHistory(skillID string) []*PracticeSession {
	sr.mu.RLock()
	defer sr.mu.RUnlock()
	
	sessions := make([]*PracticeSession, 0)
	for _, session := range sr.practiceHistory {
		if session.SkillID == skillID {
			sessions = append(sessions, session)
		}
	}
	
	return sessions
}

// GetRecentPracticeSessions returns the N most recent practice sessions
func (sr *SkillRegistryEnhanced) GetRecentPracticeSessions(n int) []*PracticeSession {
	sr.mu.RLock()
	defer sr.mu.RUnlock()
	
	if n > len(sr.practiceHistory) {
		n = len(sr.practiceHistory)
	}
	
	// Return last N sessions
	start := len(sr.practiceHistory) - n
	return sr.practiceHistory[start:]
}

// GetTotalPracticeTime returns the total time spent practicing
func (sr *SkillRegistryEnhanced) GetTotalPracticeTime() time.Duration {
	sr.mu.RLock()
	defer sr.mu.RUnlock()
	return sr.totalPracticeTime
}

// GetSkillsNeedingPractice returns skills that haven't been practiced recently
func (sr *SkillRegistryEnhanced) GetSkillsNeedingPractice(threshold time.Duration) []*Skill {
	sr.mu.RLock()
	defer sr.mu.RUnlock()
	
	now := time.Now()
	needsPractice := make([]*Skill, 0)
	
	for _, skill := range sr.skills {
		timeSincePractice := now.Sub(skill.LastPracticed)
		if timeSincePractice > threshold {
			needsPractice = append(needsPractice, skill)
		}
	}
	
	return needsPractice
}

// GetAllSkills returns all registered skills
func (sr *SkillRegistryEnhanced) GetAllSkills() []*Skill {
	sr.mu.RLock()
	defer sr.mu.RUnlock()
	
	skills := make([]*Skill, 0, len(sr.skills))
	for _, skill := range sr.skills {
		skills = append(skills, skill)
	}
	
	return skills
}

// GetSkillCount returns the number of registered skills
func (sr *SkillRegistryEnhanced) GetSkillCount() int {
	sr.mu.RLock()
	defer sr.mu.RUnlock()
	return len(sr.skills)
}

// GetPracticeSessionCount returns the number of practice sessions
func (sr *SkillRegistryEnhanced) GetPracticeSessionCount() int {
	sr.mu.RLock()
	defer sr.mu.RUnlock()
	return len(sr.practiceHistory)
}

// IsCurrentlyPracticing returns whether a practice session is currently active
func (sr *SkillRegistryEnhanced) IsCurrentlyPracticing() bool {
	sr.mu.RLock()
	defer sr.mu.RUnlock()
	
	// Check if there's an active practice session (one with no end time)
	for _, session := range sr.practiceHistory {
		if session.EndTime.IsZero() {
			return true
		}
	}
	
	return false
}

// SchedulePractice schedules a practice session for a skill
func (sr *SkillRegistryEnhanced) SchedulePractice(skillID string, duration time.Duration) error {
	sr.mu.Lock()
	defer sr.mu.Unlock()
	
	_, exists := sr.skills[skillID]
	if !exists {
		return fmt.Errorf("skill not found: %s", skillID)
	}
	
	// Create a new practice session
	session := &PracticeSession{
		ID:          generateID(),
		SkillID:     skillID,
		StartTime:   time.Now(),
		EndTime:     time.Now().Add(duration),
		Success:     false, // Will be assessed after practice
		Performance: 0.0,
		Improvement: 0.0,
	}
	
	sr.practiceHistory = append(sr.practiceHistory, session)
	
	return nil
}
