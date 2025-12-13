package deeptreeecho

import (
	"fmt"
	"math"
	"sync"
	"time"

	"github.com/google/uuid"
)

// SkillPracticeSystem manages deliberate practice during rest cycles
type SkillPracticeSystem struct {
	mu              sync.RWMutex
	registry        *SkillRegistry
	practiceHistory []PracticeSession
	
	// Spaced repetition parameters
	spacingMultiplier float64
	difficultyAdjustment float64
	
	// Metrics
	totalSessions   uint64
	totalImprovement float64
}

// PracticeSession represents a single practice session
type PracticeSession struct {
	ID          string
	SkillID     string
	ExerciseID  string
	StartTime   time.Time
	EndTime     time.Time
	Success     bool
	Performance float64
	Improvement float64
}

// NewSkillPracticeSystem creates a new skill practice system
func NewSkillPracticeSystem(registry *SkillRegistry) *SkillPracticeSystem {
	return &SkillPracticeSystem{
		registry:          registry,
		practiceHistory:   make([]PracticeSession, 0),
		spacingMultiplier: 2.0,
		difficultyAdjustment: 0.1,
	}
}

// PracticeDuringRest selects and practices skills during rest cycle
func (sps *SkillPracticeSystem) PracticeDuringRest(duration time.Duration) {
	sps.mu.Lock()
	defer sps.mu.Unlock()
	
	fmt.Println("🎯 Beginning skill practice session...")
	
	// Select skills that need practice
	skillsToPractice := sps.selectSkillsForPractice(3)
	
	if len(skillsToPractice) == 0 {
		fmt.Println("  ℹ️  No skills need practice at this time")
		return
	}
	
	// Practice each selected skill
	for _, skill := range skillsToPractice {
		session := sps.practiceSkill(skill)
		sps.practiceHistory = append(sps.practiceHistory, session)
		sps.totalSessions++
		
		if session.Improvement > 0 {
			sps.totalImprovement += session.Improvement
		}
		
		fmt.Printf("  ✅ Practiced: %s (proficiency: %.2f → %.2f)\n", 
			skill.Name, 
			skill.Proficiency - session.Improvement,
			skill.Proficiency)
	}
	
	fmt.Printf("✅ Practice session complete (%d skills practiced)\n", len(skillsToPractice))
}

// selectSkillsForPractice selects skills using spaced repetition
func (sps *SkillPracticeSystem) selectSkillsForPractice(count int) []*Skill {
	allSkills := sps.registry.GetAllSkills()
	
	// Score each skill for practice priority
	type skillScore struct {
		skill *Skill
		score float64
	}
	
	scores := make([]skillScore, 0, len(allSkills))
	
	for _, skill := range allSkills {
		score := sps.calculatePracticeScore(skill)
		scores = append(scores, skillScore{skill: skill, score: score})
	}
	
	// Sort by score (highest first)
	// Simplified: just take first N
	selected := make([]*Skill, 0, count)
	for i := 0; i < count && i < len(scores); i++ {
		if scores[i].score > 0.3 {
			selected = append(selected, scores[i].skill)
		}
	}
	
	return selected
}

// calculatePracticeScore determines priority for practicing a skill
func (sps *SkillPracticeSystem) calculatePracticeScore(skill *Skill) float64 {
	// Factors:
	// 1. Time since last practice (spaced repetition)
	// 2. Current proficiency (lower = higher priority)
	// 3. Skill category importance
	
	timeSinceLastPractice := time.Since(skill.LastPracticed)
	daysSince := timeSinceLastPractice.Hours() / 24.0
	
	// Spaced repetition: optimal interval increases with proficiency
	optimalInterval := math.Pow(sps.spacingMultiplier, skill.Proficiency * 10)
	timeScore := math.Min(daysSince / optimalInterval, 1.0)
	
	// Proficiency score: lower proficiency = higher priority
	proficiencyScore := 1.0 - skill.Proficiency
	
	// Category importance
	categoryScore := sps.getCategoryImportance(skill.Category)
	
	// Combined score
	return (timeScore * 0.4) + (proficiencyScore * 0.4) + (categoryScore * 0.2)
}

// getCategoryImportance returns importance weight for skill category
func (sps *SkillPracticeSystem) getCategoryImportance(category SkillCategory) float64 {
	importance := map[SkillCategory]float64{
		SkillMetaCognition: 1.0,  // Highest priority
		SkillSynthesis:     0.9,
		SkillAnalysis:      0.8,
		SkillReasoning:     0.8,
		SkillCreativity:    0.7,
		SkillCommunication: 0.6,
	}
	
	if val, ok := importance[category]; ok {
		return val
	}
	return 0.5
}

// practiceSkill performs a practice session for a skill
func (sps *SkillPracticeSystem) practiceSkill(skill *Skill) PracticeSession {
	session := PracticeSession{
		ID:        uuid.New().String(),
		SkillID:   skill.ID,
		StartTime: time.Now(),
	}
	
	// Select appropriate exercise
	exercise := sps.selectExercise(skill)
	if exercise != nil {
		session.ExerciseID = exercise.ID
	}
	
	// Simulate practice (in full implementation, this would be actual cognitive work)
	performance := sps.simulatePractice(skill, exercise)
	session.Performance = performance
	
	// Update skill proficiency based on performance
	improvement := sps.calculateImprovement(skill, performance)
	skill.Proficiency = math.Min(skill.Proficiency + improvement, 1.0)
	skill.LastPracticed = time.Now()
	skill.PracticeCount++
	
	session.Improvement = improvement
	session.Success = performance > 0.6
	session.EndTime = time.Now()
	
	// Adjust exercise difficulty if needed
	if exercise != nil {
		sps.adjustExerciseDifficulty(exercise, performance)
	}
	
	return session
}

// selectExercise selects an appropriate exercise for the skill
func (sps *SkillPracticeSystem) selectExercise(skill *Skill) *Exercise {
	if len(skill.Exercises) == 0 {
		return nil
	}
	
	// Select exercise closest to current proficiency level
	targetDifficulty := skill.Proficiency + 0.1 // Slightly above current level
	
	var bestExercise *Exercise
	minDiff := 1.0
	
	for i := range skill.Exercises {
		diff := math.Abs(skill.Exercises[i].Difficulty - targetDifficulty)
		if diff < minDiff {
			minDiff = diff
			bestExercise = &skill.Exercises[i]
		}
	}
	
	return bestExercise
}

// simulatePractice simulates practice performance
func (sps *SkillPracticeSystem) simulatePractice(skill *Skill, exercise *Exercise) float64 {
	// Simplified simulation
	// In full implementation, this would involve actual cognitive tasks
	
	baseProbability := skill.Proficiency
	
	if exercise != nil {
		// Adjust based on exercise difficulty
		difficultyFactor := 1.0 - math.Abs(exercise.Difficulty - skill.Proficiency)
		baseProbability *= difficultyFactor
	}
	
	// Add some randomness
	randomFactor := 0.8 + (0.4 * 0.5) // Simplified random
	
	return math.Min(baseProbability * randomFactor, 1.0)
}

// calculateImprovement determines proficiency improvement from practice
func (sps *SkillPracticeSystem) calculateImprovement(skill *Skill, performance float64) float64 {
	// Learning rate decreases as proficiency increases
	learningRate := 0.1 * (1.0 - skill.Proficiency)
	
	// Improvement based on performance
	if performance > 0.8 {
		// Excellent performance
		return learningRate * 1.5
	} else if performance > 0.6 {
		// Good performance
		return learningRate
	} else if performance > 0.4 {
		// Moderate performance
		return learningRate * 0.5
	} else {
		// Poor performance
		return learningRate * 0.2
	}
}

// adjustExerciseDifficulty adjusts exercise difficulty based on performance
func (sps *SkillPracticeSystem) adjustExerciseDifficulty(exercise *Exercise, performance float64) {
	if performance > 0.9 {
		// Too easy, increase difficulty
		exercise.Difficulty = math.Min(exercise.Difficulty + sps.difficultyAdjustment, 1.0)
	} else if performance < 0.4 {
		// Too hard, decrease difficulty
		exercise.Difficulty = math.Max(exercise.Difficulty - sps.difficultyAdjustment, 0.1)
	}
	
	// Update success rate
	if exercise.SuccessRate == 0 {
		exercise.SuccessRate = performance
	} else {
		// Exponential moving average
		exercise.SuccessRate = (exercise.SuccessRate * 0.7) + (performance * 0.3)
	}
	
	exercise.LastAttempt = time.Now()
}

// GetPracticeMetrics returns practice system metrics
func (sps *SkillPracticeSystem) GetPracticeMetrics() map[string]interface{} {
	sps.mu.RLock()
	defer sps.mu.RUnlock()
	
	avgImprovement := 0.0
	if sps.totalSessions > 0 {
		avgImprovement = sps.totalImprovement / float64(sps.totalSessions)
	}
	
	return map[string]interface{}{
		"total_sessions":     sps.totalSessions,
		"total_improvement":  sps.totalImprovement,
		"average_improvement": avgImprovement,
		"recent_sessions":    len(sps.practiceHistory),
	}
}

// GetSkillProgress returns progress for all skills
func (sps *SkillPracticeSystem) GetSkillProgress() map[string]float64 {
	sps.mu.RLock()
	defer sps.mu.RUnlock()
	
	allSkills := sps.registry.GetAllSkills()
	progress := make(map[string]float64)
	
	for _, skill := range allSkills {
		progress[skill.Name] = skill.Proficiency
	}
	
	return progress
}

// GetAllSkills returns all skills from registry
func (sr *SkillRegistry) GetAllSkills() []*Skill {
	sr.mu.RLock()
	defer sr.mu.RUnlock()
	
	skills := make([]*Skill, 0, len(sr.skills))
	for _, skill := range sr.skills {
		skills = append(skills, skill)
	}
	
	return skills
}
