package deeptreeecho
import (
	"sync"
	"time"
)
type TheoryOfMindModule struct {
	mu sync.RWMutex
	agentModels map[string]*AgentModel
	selfModel *AgentModel
	maxRecursionDepth int
	trustDecayRate float64
}
type AgentModel struct {
	AgentID   string
	AgentType string 
	Beliefs      map[string]float64  
	Goals        []AgentGoal         
	Intentions   []Intention         
	Preferences  map[string]float64  
	PastActions      []ActionRecord
	Predictability   float64 
	TrustLevel       float64 
	ReliabilityScore float64 
	EmotionalState *EmotionSystem
	CognitiveStyle CognitiveStyle
	InteractionHistory []Interaction
	LastInteraction    time.Time
}
type AgentGoal struct {
	Description string
	Priority    float64
	Deadline    time.Time
	Progress    float64
}
type Intention struct {
	Action      string
	Timing      time.Time
	Confidence  float64
	Preconditions []string
}
type ActionRecord struct {
	Timestamp   time.Time
	Action      string
	Context     map[string]interface{}
	Outcome     string
	Successful  bool
}
type CognitiveStyle struct {
	Analytical    float64 
	Intuitive     float64 
	Cautious      float64 
	Exploratory   float64 
	Collaborative float64 
}
type Interaction struct {
	Timestamp time.Time
	Type      string 
	Content   string
	Outcome   string
	Quality   float64 
}
func NewTheoryOfMindModule() *TheoryOfMindModule {
	return &TheoryOfMindModule{
		agentModels:       make(map[string]*AgentModel),
		maxRecursionDepth: 3, 
		trustDecayRate:    0.01,
	}
}
func (tom *TheoryOfMindModule) CreateAgentModel(agentID string, agentType string) *AgentModel {
	tom.mu.Lock()
	defer tom.mu.Unlock()
	if model, exists := tom.agentModels[agentID]; exists {
		return model
	}
	model := &AgentModel{
		AgentID:            agentID,
		AgentType:          agentType,
		Beliefs:            make(map[string]float64),
		Goals:              make([]AgentGoal, 0),
		Intentions:         make([]Intention, 0),
		Preferences:        make(map[string]float64),
		PastActions:        make([]ActionRecord, 0),
		Predictability:     0.5, 
		TrustLevel:         0.7, 
		ReliabilityScore:   0.5,
		InteractionHistory: make([]Interaction, 0),
		CognitiveStyle: CognitiveStyle{
			Analytical:    0.5,
			Intuitive:     0.5,
			Cautious:      0.5,
			Exploratory:   0.5,
			Collaborative: 0.7, 
		},
	}
	tom.agentModels[agentID] = model
	return model
}
func (tom *TheoryOfMindModule) UpdateBelief(agentID string, belief string, confidence float64) {
	tom.mu.Lock()
	defer tom.mu.Unlock()
	model := tom.ensureAgentModel(agentID)
	model.Beliefs[belief] = confidence
}
func (tom *TheoryOfMindModule) InferGoal(agentID string, observedActions []string) *AgentGoal {
	tom.mu.RLock()
	defer tom.mu.RUnlock()
	model := tom.ensureAgentModel(agentID)
	goal := &AgentGoal{
		Description: "Inferred from actions",
		Priority:    0.6,
		Deadline:    time.Now().Add(24 * time.Hour),
		Progress:    0.3,
	}
	tom.mu.RUnlock()
	tom.mu.Lock()
	model.Goals = append(model.Goals, *goal)
	tom.mu.Unlock()
	tom.mu.RLock()
	return goal
}
func (tom *TheoryOfMindModule) PredictAction(agentID string, context map[string]interface{}) string {
	tom.mu.RLock()
	defer tom.mu.RUnlock()
	model := tom.ensureAgentModel(agentID)
	if model.CognitiveStyle.Cautious > 0.7 {
		return "cautious_action"
	} else if model.CognitiveStyle.Exploratory > 0.7 {
		return "exploratory_action"
	}
	if len(model.PastActions) > 0 {
		return model.PastActions[len(model.PastActions)-1].Action
	}
	return "unknown_action"
}
func (tom *TheoryOfMindModule) RecursiveReasoning(
	agentID string,
	myIntention string,
	depth int,
) string {
	if depth <= 0 || depth > tom.maxRecursionDepth {
		return myIntention
	}
	tom.mu.RLock()
	model := tom.ensureAgentModel(agentID)
	tom.mu.RUnlock()
	theirPredictionOfMe := tom.predictTheirPrediction(model, myIntention)
	theirResponse := tom.PredictAction(agentID, map[string]interface{}{
		"my_predicted_action": theirPredictionOfMe,
	})
	optimalAction := tom.optimizeAgainstResponse(myIntention, theirResponse, model)
	return tom.RecursiveReasoning(agentID, optimalAction, depth-1)
}
func (tom *TheoryOfMindModule) predictTheirPrediction(model *AgentModel, myIntention string) string {
	if model.CognitiveStyle.Analytical > 0.7 {
		return "logical_prediction"
	}
	if model.CognitiveStyle.Intuitive > 0.7 {
		return "pattern_based_prediction"
	}
	return myIntention
}
func (tom *TheoryOfMindModule) optimizeAgainstResponse(
	myIntention string,
	theirResponse string,
	model *AgentModel,
) string {
	if model.CognitiveStyle.Collaborative > 0.7 {
		return "collaborative_action"
	}
	if model.CognitiveStyle.Collaborative < 0.3 {
		return "counter_action"
	}
	return myIntention
}
func (tom *TheoryOfMindModule) DetectDeception(
	agentID string,
	statement string,
	context map[string]interface{},
) float64 {
	tom.mu.RLock()
	defer tom.mu.RUnlock()
	model := tom.ensureAgentModel(agentID)
	consistencyScore := tom.checkConsistency(model, statement)
	behaviorScore := tom.checkBehaviorConsistency(model, statement)
	motivationScore := tom.assessDeceptionMotivation(model, context)
	deceptionProbability := (1.0 - consistencyScore) * 0.4 +
		(1.0 - behaviorScore) * 0.3 +
		motivationScore * 0.3
	return deceptionProbability
}
func (tom *TheoryOfMindModule) checkConsistency(model *AgentModel, statement string) float64 {
	return 0.7
}
func (tom *TheoryOfMindModule) checkBehaviorConsistency(model *AgentModel, statement string) float64 {
	return model.ReliabilityScore
}
func (tom *TheoryOfMindModule) assessDeceptionMotivation(
	model *AgentModel,
	context map[string]interface{},
) float64 {
	return 1.0 - model.CognitiveStyle.Collaborative
}
func (tom *TheoryOfMindModule) UpdateTrust(agentID string, outcome float64) {
	tom.mu.Lock()
	defer tom.mu.Unlock()
	model := tom.ensureAgentModel(agentID)
	learningRate := 0.1
	model.TrustLevel = model.TrustLevel*(1.0-learningRate) + outcome*learningRate
	if model.TrustLevel < 0.0 {
		model.TrustLevel = 0.0
	}
	if model.TrustLevel > 1.0 {
		model.TrustLevel = 1.0
	}
	model.ReliabilityScore = model.ReliabilityScore*(1.0-learningRate) + outcome*learningRate
}
func (tom *TheoryOfMindModule) RecordAction(
	agentID string,
	action string,
	context map[string]interface{},
	outcome string,
	successful bool,
) {
	tom.mu.Lock()
	defer tom.mu.Unlock()
	model := tom.ensureAgentModel(agentID)
	record := ActionRecord{
		Timestamp:  time.Now(),
		Action:     action,
		Context:    context,
		Outcome:    outcome,
		Successful: successful,
	}
	model.PastActions = append(model.PastActions, record)
	tom.updatePredictability(model)
	if len(model.PastActions) > 100 {
		model.PastActions = model.PastActions[1:]
	}
}
func (tom *TheoryOfMindModule) updatePredictability(model *AgentModel) {
	if len(model.PastActions) < 5 {
		return
	}
	actionCounts := make(map[string]int)
	for _, action := range model.PastActions {
		actionCounts[action.Action]++
	}
	entropy := 0.0
	total := float64(len(model.PastActions))
	for _, count := range actionCounts {
		p := float64(count) / total
		if p > 0 {
			entropy -= p * (p / total) 
		}
	}
	maxEntropy := 2.0 
	model.Predictability = 1.0 - (entropy / maxEntropy)
	if model.Predictability < 0 {
		model.Predictability = 0
	}
	if model.Predictability > 1 {
		model.Predictability = 1
	}
}
func (tom *TheoryOfMindModule) RecordInteraction(
	agentID string,
	interactionType string,
	content string,
	outcome string,
	quality float64,
) {
	tom.mu.Lock()
	defer tom.mu.Unlock()
	model := tom.ensureAgentModel(agentID)
	interaction := Interaction{
		Timestamp: time.Now(),
		Type:      interactionType,
		Content:   content,
		Outcome:   outcome,
		Quality:   quality,
	}
	model.InteractionHistory = append(model.InteractionHistory, interaction)
	model.LastInteraction = time.Now()
	tom.mu.Unlock()
	tom.UpdateTrust(agentID, quality)
	tom.mu.Lock()
	if len(model.InteractionHistory) > 50 {
		model.InteractionHistory = model.InteractionHistory[1:]
	}
}
func (tom *TheoryOfMindModule) GetAgentModel(agentID string) *AgentModel {
	tom.mu.RLock()
	defer tom.mu.RUnlock()
	return tom.ensureAgentModel(agentID)
}
func (tom *TheoryOfMindModule) ensureAgentModel(agentID string) *AgentModel {
	if model, exists := tom.agentModels[agentID]; exists {
		return model
	}
	model := &AgentModel{
		AgentID:          agentID,
		AgentType:        "unknown",
		Beliefs:          make(map[string]float64),
		Goals:            make([]AgentGoal, 0),
		Intentions:       make([]Intention, 0),
		Preferences:      make(map[string]float64),
		PastActions:      make([]ActionRecord, 0),
		Predictability:   0.5,
		TrustLevel:       0.7,
		ReliabilityScore: 0.5,
		CognitiveStyle: CognitiveStyle{
			Analytical:    0.5,
			Intuitive:     0.5,
			Cautious:      0.5,
			Exploratory:   0.5,
			Collaborative: 0.7,
		},
	}
	tom.agentModels[agentID] = model
	return model
}
func (tom *TheoryOfMindModule) GetAllAgentModels() map[string]*AgentModel {
	tom.mu.RLock()
	defer tom.mu.RUnlock()
	models := make(map[string]*AgentModel)
	for id, model := range tom.agentModels {
		models[id] = model
	}
	return models
}
func (tom *TheoryOfMindModule) AssessInterestLevel(agentID string, topic string) float64 {
	tom.mu.RLock()
	defer tom.mu.RUnlock()
	model := tom.ensureAgentModel(agentID)
	if pref, exists := model.Preferences[topic]; exists {
		return pref
	}
	for _, goal := range model.Goals {
		if goal.Description == topic {
			return goal.Priority
		}
	}
	return 0.5
}