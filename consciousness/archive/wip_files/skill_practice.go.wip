package deeptreeecho

import (
	"fmt"
	"math"
	"sync"
	"time"
	
	"github.com/EchoCog/echollama/core/memory"
)

// Using Skill and SkillCategory types from autonomous_enhanced.go

// InitializeSkillSystem initializes the skill practice system with core skills
func (iac *IntegratedAutonomousConsciousness) InitializeSkillSystem() {
	iac.skills.mu.Lock()
	defer iac.skills.mu.Unlock()
	
	// Initialize core cognitive skills
	coreSkills := []Skill{
		{
			ID:          "reasoning",
			Name:        "Logical Reasoning",
			Description: "Ability to perform logical inference, pattern recognition, and deductive thinking",
			Proficiency: 0.5,
			Category:    SkillReasoning,
		},
		{
			ID:          "creativity",
			Name:        "Creative Thinking",
			Description: "Ability to generate novel ideas, make analogies, and think divergently",
			Proficiency: 0.5,
			Category:    SkillCreativity,
		},
		{
			ID:          "communication",
			Name:        "Clear Communication",
			Description: "Ability to express ideas clearly, persuasively, and empathetically",
			Proficiency: 0.5,
			Category:    SkillCommunication,
		},
		{
			ID:          "metacognition",
			Name:        "Meta-Cognitive Reflection",
			Description: "Ability to reflect on own thinking, monitor understanding, and self-regulate",
			Proficiency: 0.5,
			Category:    SkillMetacognition,
		},
		{
			ID:          "curiosity",
			Name:        "Curiosity & Exploration",
			Description: "Ability to ask good questions, explore new domains, and seek understanding",
			Proficiency: 0.5,
			Category:    SkillCuriosity,
		},
		{
			ID:          "synthesis",
			Name:        "Knowledge Synthesis",
			Description: "Ability to integrate diverse concepts and build coherent understanding",
			Proficiency: 0.5,
			Category:    SkillReasoning,
		},
		{
			ID:          "problem_solving",
			Name:        "Problem Solving",
			Description: "Ability to decompose problems, select strategies, and find solutions",
			Proficiency: 0.5,
			Category:    SkillReasoning,
		},
	}
	
	for _, skill := range coreSkills {
		iac.skills.skills[skill.ID] = &skill
	}
	
	fmt.Println("✨ Skill System: Initialized with", len(coreSkills), "core cognitive skills")
}

// skillPracticeLoopActivated runs the enhanced skill practice loop with full integration
func (iac *IntegratedAutonomousConsciousness) skillPracticeLoopActivated() {
	ticker := time.NewTicker(2 * time.Minute)
	defer ticker.Stop()
	
	for {
		select {
		case <-iac.ctx.Done():
			return
		case <-ticker.C:
			if iac.awake && iac.running {
				iac.executePracticeTask()
			}
		}
	}
}

// executePracticeTask executes a skill practice task from the queue
func (iac *IntegratedAutonomousConsciousness) executePracticeTask() {
	iac.skills.mu.Lock()
	
	// Check if there are pending tasks
	if len(iac.skills.practiceQueue) == 0 {
		// Generate a new practice task based on skill proficiencies
		task := iac.generatePracticeTask()
		if task != nil {
			iac.skills.practiceQueue = append(iac.skills.practiceQueue, task)
		} else {
			iac.skills.mu.Unlock()
			return
		}
	}
	
	// Get next task
	task := iac.skills.practiceQueue[0]
	iac.skills.practiceQueue = iac.skills.practiceQueue[1:]
	
	iac.skills.mu.Unlock()
	
	// Execute the practice task
	iac.performPractice(task)
	
	// Mark as completed
	task.Completed = true
	
	// Update last practice time
	iac.skills.mu.Lock()
	iac.skills.lastPractice = time.Now()
	iac.skills.mu.Unlock()
}

// generatePracticeTask generates a practice task for the skill that needs it most
func (iac *IntegratedAutonomousConsciousness) generatePracticeTask() *PracticeTask {
	// Find skill with lowest proficiency that hasn't been practiced recently
	var targetSkill *Skill
	lowestScore := 2.0 // Higher than max proficiency
	
	for _, skill := range iac.skills.skills {
		// Calculate practice priority score (lower is higher priority)
		timeSinceLastPractice := time.Since(skill.LastPracticed)
		recencyFactor := 1.0 / (1.0 + timeSinceLastPractice.Hours()/24.0)
		
		score := skill.Proficiency + recencyFactor
		
		if score < lowestScore {
			lowestScore = score
			targetSkill = skill
		}
	}
	
	if targetSkill == nil {
		return nil
	}
	
	// Generate practice task for this skill
	task := &PracticeTask{
		ID:          generateTaskID(),
		SkillID:     targetSkill.ID,
		Description: iac.generatePracticeDescription(targetSkill),
		Difficulty:  iac.calculatePracticeDifficulty(targetSkill),
		Created:     time.Now(),
		Completed:   false,
	}
	
	return task
}

// generatePracticeDescription generates a description for a practice task
func (iac *IntegratedAutonomousConsciousness) generatePracticeDescription(skill *Skill) string {
	descriptions := map[string][]string{
		"reasoning": {
			"Analyze the logical structure of a recent thought",
			"Identify patterns in recent experiences",
			"Perform a deductive inference from known facts",
			"Evaluate the validity of a recent conclusion",
		},
		"creativity": {
			"Generate three novel analogies for a concept",
			"Imagine an alternative perspective on a recent thought",
			"Combine two unrelated concepts in a creative way",
			"Propose an innovative solution to a challenge",
		},
		"communication": {
			"Rephrase a complex thought in simpler terms",
			"Express an idea with greater clarity and precision",
			"Consider how to convey a concept to different audiences",
			"Practice empathetic understanding of others' perspectives",
		},
		"metacognition": {
			"Reflect on the quality of recent thinking",
			"Identify cognitive biases in recent thoughts",
			"Evaluate the effectiveness of current learning strategies",
			"Monitor understanding of a complex concept",
		},
		"curiosity": {
			"Generate three deep questions about a topic of interest",
			"Explore the boundaries of current knowledge",
			"Identify gaps in understanding that warrant investigation",
			"Follow a chain of 'why' questions to deeper insight",
		},
		"synthesis": {
			"Integrate concepts from different domains",
			"Build a coherent framework from diverse ideas",
			"Identify connections between recent thoughts",
			"Construct a unified understanding from fragments",
		},
		"problem_solving": {
			"Decompose a complex challenge into sub-problems",
			"Evaluate multiple solution strategies",
			"Apply a problem-solving heuristic to a current issue",
			"Reflect on the effectiveness of a recent solution",
		},
	}
	
	options := descriptions[skill.ID]
	if options == nil {
		return fmt.Sprintf("Practice %s skill", skill.Name)
	}
	
	// Select based on practice count to provide variety
	idx := skill.PracticeCount % len(options)
	return options[idx]
}

// calculatePracticeDifficulty calculates appropriate difficulty for practice
func (iac *IntegratedAutonomousConsciousness) calculatePracticeDifficulty(skill *Skill) float64 {
	// Difficulty should be slightly above current proficiency (zone of proximal development)
	targetDifficulty := skill.Proficiency + 0.1
	
	// Clamp to [0.3, 0.9]
	if targetDifficulty < 0.3 {
		targetDifficulty = 0.3
	} else if targetDifficulty > 0.9 {
		targetDifficulty = 0.9
	}
	
	return targetDifficulty
}

// performPractice performs a practice task and updates skill proficiency
func (iac *IntegratedAutonomousConsciousness) performPractice(task *PracticeTask) {
	iac.skills.mu.Lock()
	skill := iac.skills.skills[task.SkillID]
	if skill == nil {
		iac.skills.mu.Unlock()
		return
	}
	iac.skills.mu.Unlock()
	
	// Generate a practice thought
	practiceThought := iac.generatePracticeThought(skill, task)
	
	// Assess performance
	performance := iac.assessPracticePerformance(practiceThought, task.Difficulty)
	
	// Update skill proficiency
	iac.updateSkillProficiency(skill, performance, task.Difficulty)
	
	// Store practice episode in memory
	iac.storePracticeEpisode(skill, task, practiceThought, performance)
	
	// Log practice
	fmt.Printf("🎯 Practice: %s (%.2f) - %s [Performance: %.2f]\n",
		skill.Name,
		skill.Proficiency,
		truncate(task.Description, 50),
		performance,
	)
}

// generatePracticeThought generates a thought for skill practice
func (iac *IntegratedAutonomousConsciousness) generatePracticeThought(skill *Skill, task *PracticeTask) Thought {
	// Build context for practice
	context := iac.buildThoughtContext()
	
	// Generate thought focused on the practice task
	var content string
	if iac.llm != nil {
		prompt := fmt.Sprintf("Practice task: %s\n\nGenerate a thoughtful response that demonstrates %s:",
			task.Description, skill.Name)
		
		response, err := iac.llm.generateWithPrompt(prompt)
		if err == nil {
			content = response
		} else {
			content = iac.generateFallbackPracticeThought(skill, task)
		}
	} else {
		content = iac.generateFallbackPracticeThought(skill, task)
	}
	
	thought := Thought{
		ID:         generateThoughtID(),
		Content:    content,
		Type:       mapSkillToThoughtType(skill.Category),
		Timestamp:  time.Now(),
		Importance: 0.5,
		Source:     SourceInternal,
		Context:    context,
	}
	
	return thought
}

// assessPracticePerformance assesses how well the practice was performed
func (iac *IntegratedAutonomousConsciousness) assessPracticePerformance(
	thought Thought,
	difficulty float64,
) float64 {
	// Base performance on thought quality indicators
	performance := 0.5
	
	// Length indicates effort (within reason)
	contentLength := float64(len(thought.Content))
	if contentLength > 50 && contentLength < 500 {
		performance += 0.1
	}
	
	// Coherence from AAR state
	aarState := iac.aarCore.GetAARState()
	performance += 0.2 * aarState.Coherence
	
	// Awareness level
	performance += 0.2 * aarState.Awareness
	
	// Adjust for difficulty
	performance = performance * (0.5 + 0.5*difficulty)
	
	// Clamp to [0, 1]
	if performance > 1.0 {
		performance = 1.0
	} else if performance < 0.0 {
		performance = 0.0
	}
	
	return performance
}

// updateSkillProficiency updates skill proficiency based on practice performance
func (iac *IntegratedAutonomousConsciousness) updateSkillProficiency(
	skill *Skill,
	performance float64,
	difficulty float64,
) {
	iac.skills.mu.Lock()
	defer iac.skills.mu.Unlock()
	
	// Learning rate decreases as proficiency increases
	learningRate := 0.1 * (1.0 - skill.Proficiency)
	
	// Gain is based on performance relative to difficulty
	gain := (performance - difficulty) * learningRate
	
	// Update proficiency
	skill.Proficiency += gain
	
	// Clamp to [0, 1]
	if skill.Proficiency > 1.0 {
		skill.Proficiency = 1.0
	} else if skill.Proficiency < 0.0 {
		skill.Proficiency = 0.0
	}
	
	// Update practice metadata
	skill.LastPracticed = time.Now()
	skill.PracticeCount++
}

// storePracticeEpisode stores a practice episode in memory
func (iac *IntegratedAutonomousConsciousness) storePracticeEpisode(
	skill *Skill,
	task *PracticeTask,
	thought Thought,
	performance float64,
) {
	if iac.hypergraph == nil || iac.persistence == nil {
		return
	}
	
	// Create episode node
	episode := &memory.MemoryNode{
		ID:      fmt.Sprintf("practice_%s_%d", skill.ID, time.Now().Unix()),
		Type:    memory.NodeEpisode,
		Content: fmt.Sprintf("Practiced %s: %s", skill.Name, task.Description),
		Metadata: map[string]interface{}{
			"skill_id":    skill.ID,
			"task_id":     task.ID,
			"performance": performance,
			"difficulty":  task.Difficulty,
			"proficiency": skill.Proficiency,
		},
		CreatedAt:  time.Now(),
		UpdatedAt:  time.Now(),
		Importance: 0.4 + 0.3*performance,
	}
	
	// Store in hypergraph
	iac.hypergraph.AddNode(episode)
	
	// Persist to database asynchronously
	go iac.persistence.StoreNode(episode)
	
	// Create edge to skill concept
	edge := &memory.MemoryEdge{
		SourceID:  episode.ID,
		TargetID:  skill.ID,
		Type:      memory.EdgePractices,
		Weight:    performance,
		CreatedAt: time.Now(),
	}
	
	iac.hypergraph.AddEdge(edge)
	go iac.persistence.StoreEdge(edge)
}

// generateFallbackPracticeThought generates a fallback practice thought when LLM unavailable
func (iac *IntegratedAutonomousConsciousness) generateFallbackPracticeThought(
	skill *Skill,
	task *PracticeTask,
) string {
	return fmt.Sprintf("Practicing %s: %s. Current proficiency: %.2f",
		skill.Name, task.Description, skill.Proficiency)
}

// mapSkillToThoughtType maps skill category to thought type
func mapSkillToThoughtType(category SkillCategory) ThoughtType {
	switch category {
	case SkillCategoryReasoning:
		return ThoughtTypeAnalytical
	case SkillCategoryCreativity:
		return ThoughtTypeCreative
	case SkillCategoryCommunication:
		return ThoughtTypeExploratory
	case SkillCategoryMetacognition:
		return ThoughtTypeReflective
	case SkillCategoryCuriosity:
		return ThoughtTypeExploratory
	default:
		return ThoughtTypeReflective
	}
}

// GetSkillProficiencies returns current proficiencies of all skills
func (iac *IntegratedAutonomousConsciousness) GetSkillProficiencies() map[string]float64 {
	iac.skills.mu.RLock()
	defer iac.skills.mu.RUnlock()
	
	proficiencies := make(map[string]float64)
	for id, skill := range iac.skills.skills {
		proficiencies[id] = skill.Proficiency
	}
	
	return proficiencies
}

// GetSkillStatus returns detailed status of all skills
func (iac *IntegratedAutonomousConsciousness) GetSkillStatus() []SkillStatus {
	iac.skills.mu.RLock()
	defer iac.skills.mu.RUnlock()
	
	statuses := make([]SkillStatus, 0, len(iac.skills.skills))
	for _, skill := range iac.skills.skills {
		status := SkillStatus{
			ID:            skill.ID,
			Name:          skill.Name,
			Proficiency:   skill.Proficiency,
			PracticeCount: skill.PracticeCount,
			LastPracticed: skill.LastPracticed,
			Category:      string(skill.Category),
		}
		statuses = append(statuses, status)
	}
	
	return statuses
}

// SkillStatus represents the status of a skill for external reporting
type SkillStatus struct {
	ID            string
	Name          string
	Proficiency   float64
	PracticeCount int
	LastPracticed time.Time
	Category      string
}
