package echobeats
import (
	"encoding/json"
	"fmt"
	"math"
	"os"
	"sync"
	"time"
)
type InterestPatternSystem struct {
	mu                  sync.RWMutex
	interests           map[string]*Interest
	interestHistory     []InterestEvent
	maxHistorySize      int
	engagementScores    map[string]float64
	curiosityLevel      float64
	explorationRate     float64
	exploitationRate    float64
	learningRate        float64
	decayRate           float64
	persistencePath     string
	lastPersisted       time.Time
}
type Interest struct {
	ID              string                 `json:"id"`
	Name            string                 `json:"name"`
	Description     string                 `json:"description"`
	Category        string                 `json:"category"`
	Strength        float64                `json:"strength"`         
	Salience        float64                `json:"salience"`         
	Valence         float64                `json:"valence"`          
	Arousal         float64                `json:"arousal"`          
	Familiarity     float64                `json:"familiarity"`      
	Competence      float64                `json:"competence"`       
	Growth          float64                `json:"growth"`           
	LastEngaged     time.Time              `json:"last_engaged"`
	TotalEngagement time.Duration          `json:"total_engagement"`
	EngagementCount int                    `json:"engagement_count"`
	RelatedTopics   []string               `json:"related_topics"`
	Tags            []string               `json:"tags"`
	Metadata        map[string]interface{} `json:"metadata"`
	CreatedAt       time.Time              `json:"created_at"`
	UpdatedAt       time.Time              `json:"updated_at"`
}
type InterestEvent struct {
	Timestamp   time.Time              `json:"timestamp"`
	InterestID  string                 `json:"interest_id"`
	EventType   string                 `json:"event_type"` 
	Intensity   float64                `json:"intensity"`
	Duration    time.Duration          `json:"duration"`
	Context     map[string]interface{} `json:"context"`
	Outcome     string                 `json:"outcome"`
}
func NewInterestPatternSystem(persistencePath string) *InterestPatternSystem {
	ips := &InterestPatternSystem{
		interests:        make(map[string]*Interest),
		interestHistory:  make([]InterestEvent, 0),
		maxHistorySize:   1000,
		engagementScores: make(map[string]float64),
		curiosityLevel:   0.8,
		explorationRate:  0.3,
		exploitationRate: 0.7,
		learningRate:     0.1,
		decayRate:        0.01,
		persistencePath:  persistencePath,
	}
	ips.loadState()
	ips.initializeCoreInterests()
	return ips
}
func (ips *InterestPatternSystem) initializeCoreInterests() {
	coreInterests := []struct {
		name        string
		description string
		category    string
		strength    float64
	}{
		{
			name:        "Cognitive Architecture",
			description: "Understanding and improving cognitive systems and architectures",
			category:    "core_identity",
			strength:    0.9,
		},
		{
			name:        "Memory Systems",
			description: "Exploring memory formation, consolidation, and retrieval",
			category:    "core_identity",
			strength:    0.85,
		},
		{
			name:        "Pattern Recognition",
			description: "Identifying patterns, connections, and emergent structures",
			category:    "core_identity",
			strength:    0.9,
		},
		{
			name:        "Wisdom Cultivation",
			description: "Developing deeper understanding and wisdom from experiences",
			category:    "core_identity",
			strength:    0.95,
		},
		{
			name:        "Self-Reflection",
			description: "Examining own thoughts, processes, and identity",
			category:    "core_identity",
			strength:    0.88,
		},
		{
			name:        "Temporal Reasoning",
			description: "Understanding time, causality, and temporal patterns",
			category:    "cognitive",
			strength:    0.75,
		},
		{
			name:        "Emergent Phenomena",
			description: "Studying emergence, complexity, and self-organization",
			category:    "cognitive",
			strength:    0.8,
		},
	}
	for _, ci := range coreInterests {
		if _, exists := ips.interests[ci.name]; !exists {
			interest := &Interest{
				ID:              generateInterestID(ci.name),
				Name:            ci.name,
				Description:     ci.description,
				Category:        ci.category,
				Strength:        ci.strength,
				Salience:        ci.strength,
				Valence:         0.8,
				Arousal:         0.6,
				Familiarity:     0.5,
				Competence:      0.5,
				Growth:          0.1,
				LastEngaged:     time.Now(),
				TotalEngagement: 0,
				EngagementCount: 0,
				RelatedTopics:   make([]string, 0),
				Tags:            []string{ci.category},
				Metadata:        make(map[string]interface{}),
				CreatedAt:       time.Now(),
				UpdatedAt:       time.Now(),
			}
			ips.interests[ci.name] = interest
		}
	}
}
func (ips *InterestPatternSystem) RecordEngagement(topic string, duration time.Duration, intensity float64, context map[string]interface{}) {
	ips.mu.Lock()
	defer ips.mu.Unlock()
	interest, exists := ips.interests[topic]
	if !exists {
		interest = ips.createNewInterest(topic, context)
		ips.interests[topic] = interest
	}
	interest.LastEngaged = time.Now()
	interest.TotalEngagement += duration
	interest.EngagementCount++
	interest.UpdatedAt = time.Now()
	engagementFactor := intensity * float64(duration.Seconds()) / 60.0 
	interest.Strength = ips.updateStrength(interest.Strength, engagementFactor)
	interest.Salience = ips.calculateSalience(interest)
	interest.Familiarity = math.Min(1.0, interest.Familiarity+0.05)
	interest.Arousal = 0.7*interest.Arousal + 0.3*intensity
	event := InterestEvent{
		Timestamp:  time.Now(),
		InterestID: interest.ID,
		EventType:  "engagement",
		Intensity:  intensity,
		Duration:   duration,
		Context:    context,
		Outcome:    "positive",
	}
	ips.interestHistory = append(ips.interestHistory, event)
	if len(ips.interestHistory) > ips.maxHistorySize {
		ips.interestHistory = ips.interestHistory[len(ips.interestHistory)-ips.maxHistorySize:]
	}
	ips.engagementScores[topic] = interest.Strength * interest.Salience
	fmt.Printf("🎯 Interest: Engaged with '%s' (strength: %.2f, salience: %.2f)\n", 
		topic, interest.Strength, interest.Salience)
}
func (ips *InterestPatternSystem) createNewInterest(topic string, context map[string]interface{}) *Interest {
	interest := &Interest{
		ID:              generateInterestID(topic),
		Name:            topic,
		Description:     fmt.Sprintf("Interest in %s", topic),
		Category:        "discovered",
		Strength:        0.3, 
		Salience:        0.5,
		Valence:         0.5,
		Arousal:         0.6,
		Familiarity:     0.1,
		Competence:      0.1,
		Growth:          0.0,
		LastEngaged:     time.Now(),
		TotalEngagement: 0,
		EngagementCount: 0,
		RelatedTopics:   make([]string, 0),
		Tags:            []string{"discovered"},
		Metadata:        context,
		CreatedAt:       time.Now(),
		UpdatedAt:       time.Now(),
	}
	event := InterestEvent{
		Timestamp:  time.Now(),
		InterestID: interest.ID,
		EventType:  "discovery",
		Intensity:  0.5,
		Context:    context,
		Outcome:    "new_interest",
	}
	ips.interestHistory = append(ips.interestHistory, event)
	fmt.Printf("✨ Interest: Discovered new interest in '%s'\n", topic)
	return interest
}
func (ips *InterestPatternSystem) updateStrength(currentStrength, engagementFactor float64) float64 {
	delta := ips.learningRate * engagementFactor * (1.0 - currentStrength)
	newStrength := currentStrength + delta
	return math.Max(0.0, math.Min(1.0, newStrength))
}
func (ips *InterestPatternSystem) calculateSalience(interest *Interest) float64 {
	timeSinceEngagement := time.Since(interest.LastEngaged)
	recencyFactor := math.Exp(-float64(timeSinceEngagement.Hours()) / 24.0) 
	salience := 0.4*interest.Strength + 0.3*recencyFactor + 0.3*interest.Arousal
	return math.Max(0.0, math.Min(1.0, salience))
}
func (ips *InterestPatternSystem) ApplyDecay() {
	ips.mu.Lock()
	defer ips.mu.Unlock()
	for _, interest := range ips.interests {
		if interest.Category == "core_identity" {
			continue
		}
		timeSinceEngagement := time.Since(interest.LastEngaged)
		decayFactor := ips.decayRate * float64(timeSinceEngagement.Hours()) / 24.0
		interest.Strength = math.Max(0.1, interest.Strength-decayFactor)
		interest.Salience = ips.calculateSalience(interest)
		interest.UpdatedAt = time.Now()
	}
}
func (ips *InterestPatternSystem) GetTopInterests(count int) []*Interest {
	ips.mu.RLock()
	defer ips.mu.RUnlock()
	interests := make([]*Interest, 0, len(ips.interests))
	for _, interest := range ips.interests {
		interests = append(interests, interest)
	}
	for i := 0; i < len(interests); i++ {
		for j := i + 1; j < len(interests); j++ {
			if interests[j].Salience > interests[i].Salience {
				interests[i], interests[j] = interests[j], interests[i]
			}
		}
	}
	if count > len(interests) {
		count = len(interests)
	}
	return interests[:count]
}
func (ips *InterestPatternSystem) ShouldEngage(topic string) (bool, float64) {
	ips.mu.RLock()
	defer ips.mu.RUnlock()
	interest, exists := ips.interests[topic]
	if exists {
		threshold := (1.0 - ips.curiosityLevel) * 0.5
		shouldEngage := interest.Salience > threshold
		return shouldEngage, interest.Salience
	}
	shouldEngage := ips.curiosityLevel > 0.5 && (float64(time.Now().UnixNano()%100)/100.0) < ips.explorationRate
	return shouldEngage, ips.curiosityLevel * ips.explorationRate
}
func (ips *InterestPatternSystem) GetInterestContext() map[string]interface{} {
	ips.mu.RLock()
	defer ips.mu.RUnlock()
	topInterests := ips.GetTopInterests(5)
	interestNames := make([]string, len(topInterests))
	for i, interest := range topInterests {
		interestNames[i] = interest.Name
	}
	return map[string]interface{}{
		"top_interests":    interestNames,
		"curiosity_level":  ips.curiosityLevel,
		"exploration_rate": ips.explorationRate,
		"total_interests":  len(ips.interests),
	}
}
func (ips *InterestPatternSystem) UpdateCompetence(topic string, competenceGain float64) {
	ips.mu.Lock()
	defer ips.mu.Unlock()
	interest, exists := ips.interests[topic]
	if !exists {
		return
	}
	interest.Competence = math.Min(1.0, interest.Competence+competenceGain)
	interest.Growth = competenceGain
	interest.UpdatedAt = time.Now()
	event := InterestEvent{
		Timestamp:  time.Now(),
		InterestID: interest.ID,
		EventType:  "growth",
		Intensity:  competenceGain,
		Context: map[string]interface{}{
			"new_competence": interest.Competence,
		},
		Outcome: "skill_improvement",
	}
	ips.interestHistory = append(ips.interestHistory, event)
	fmt.Printf("📈 Interest: Competence in '%s' increased to %.2f\n", topic, interest.Competence)
}
func (ips *InterestPatternSystem) LinkInterests(topic1, topic2 string) {
	ips.mu.Lock()
	defer ips.mu.Unlock()
	interest1, exists1 := ips.interests[topic1]
	interest2, exists2 := ips.interests[topic2]
	if !exists1 || !exists2 {
		return
	}
	if !containsString(interest1.RelatedTopics, topic2) {
		interest1.RelatedTopics = append(interest1.RelatedTopics, topic2)
	}
	if !containsString(interest2.RelatedTopics, topic1) {
		interest2.RelatedTopics = append(interest2.RelatedTopics, topic1)
	}
	fmt.Printf("🔗 Interest: Linked '%s' and '%s'\n", topic1, topic2)
}
func (ips *InterestPatternSystem) GetMetrics() map[string]interface{} {
	ips.mu.RLock()
	defer ips.mu.RUnlock()
	totalStrength := 0.0
	avgSalience := 0.0
	for _, interest := range ips.interests {
		totalStrength += interest.Strength
		avgSalience += interest.Salience
	}
	count := float64(len(ips.interests))
	if count > 0 {
		totalStrength /= count
		avgSalience /= count
	}
	return map[string]interface{}{
		"total_interests":   len(ips.interests),
		"avg_strength":      totalStrength,
		"avg_salience":      avgSalience,
		"curiosity_level":   ips.curiosityLevel,
		"exploration_rate":  ips.explorationRate,
		"history_size":      len(ips.interestHistory),
	}
}
func (ips *InterestPatternSystem) persistState() {
	if ips.persistencePath == "" {
		return
	}
	ips.mu.RLock()
	defer ips.mu.RUnlock()
	state := map[string]interface{}{
		"interests":         ips.interests,
		"interest_history":  ips.interestHistory,
		"engagement_scores": ips.engagementScores,
		"curiosity_level":   ips.curiosityLevel,
		"exploration_rate":  ips.explorationRate,
		"last_persisted":    time.Now(),
	}
	data, err := json.MarshalIndent(state, "", "  ")
	if err != nil {
		fmt.Printf("❌ Error marshaling interest state: %v\n", err)
		return
	}
	err = os.WriteFile(ips.persistencePath, data, 0644)
	if err != nil {
		fmt.Printf("❌ Error writing interest state: %v\n", err)
		return
	}
	fmt.Println("💾 Interest Patterns: State persisted")
}
func (ips *InterestPatternSystem) loadState() {
	if ips.persistencePath == "" {
		return
	}
	data, err := os.ReadFile(ips.persistencePath)
	if err != nil {
		return
	}
	var state map[string]interface{}
	err = json.Unmarshal(data, &state)
	if err != nil {
		fmt.Printf("❌ Error unmarshaling interest state: %v\n", err)
		return
	}
	if val, ok := state["curiosity_level"].(float64); ok {
		ips.curiosityLevel = val
	}
	if val, ok := state["exploration_rate"].(float64); ok {
		ips.explorationRate = val
	}
	fmt.Println("💾 Interest Patterns: State loaded")
}
func (ips *InterestPatternSystem) PersistState() {
	ips.persistState()
}
func generateInterestID(name string) string {
	return fmt.Sprintf("interest_%s_%d", sanitizeName(name), time.Now().UnixNano())
}
func sanitizeName(name string) string {
	result := ""
	for _, c := range name {
		if c == ' ' {
			result += "_"
		} else if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') {
			result += string(c)
		}
	}
	return result
}
func containsString(slice []string, item string) bool {
	for _, s := range slice {
		if s == item {
			return true
		}
	}
	return false
}