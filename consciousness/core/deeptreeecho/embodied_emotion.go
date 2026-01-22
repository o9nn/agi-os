package deeptreeecho
import (
	"math"
	"sync"
	"time"
)
type EmotionType int
const (
	EmotionInterest EmotionType = iota
	EmotionJoy
	EmotionSurprise
	EmotionSadness
	EmotionAnger
	EmotionDisgust
	EmotionContempt
	EmotionFear
	EmotionShame
	EmotionGuilt
)
func (et EmotionType) String() string {
	names := []string{
		"Interest", "Joy", "Surprise", "Sadness", "Anger",
		"Disgust", "Contempt", "Fear", "Shame", "Guilt",
	}
	if int(et) < len(names) {
		return names[et]
	}
	return "Unknown"
}
type Emotion struct {
	Type      EmotionType
	Intensity float64       
	Duration  time.Duration
	OnsetTime time.Time
	AttentionScope    float64 
	ProcessingDepth   float64 
	ApproachAvoidance float64 
	MemoryStrength    float64 
	ExplorationBias   float64 
}
type EmotionSystem struct {
	mu sync.RWMutex
	emotions        map[EmotionType]*Emotion
	dominantEmotion EmotionType
	emotionBlend    map[EmotionType]float64
	arousal float64 
	valence float64 
	emotionHistory []EmotionEvent
	decayRate      float64 
	blendingFactor float64 
}
type EmotionEvent struct {
	Timestamp time.Time
	Emotion   EmotionType
	Intensity float64
	Trigger   string
	Context   map[string]interface{}
}
func NewEmotionSystem() *EmotionSystem {
	es := &EmotionSystem{
		emotions:       make(map[EmotionType]*Emotion),
		emotionBlend:   make(map[EmotionType]float64),
		emotionHistory: make([]EmotionEvent, 0),
		decayRate:      0.1,
		blendingFactor: 0.3,
		arousal:        0.5,
		valence:        0.5,
	}
	es.initializeEmotions()
	return es
}
func (es *EmotionSystem) initializeEmotions() {
	emotionTypes := []EmotionType{
		EmotionInterest, EmotionJoy, EmotionSurprise, EmotionSadness,
		EmotionAnger, EmotionDisgust, EmotionContempt, EmotionFear,
		EmotionShame, EmotionGuilt,
	}
	for _, et := range emotionTypes {
		es.emotions[et] = es.createEmotion(et, 0.1) 
	}
	es.emotions[EmotionInterest].Intensity = 0.4
	es.dominantEmotion = EmotionInterest
}
func (es *EmotionSystem) createEmotion(emotionType EmotionType, intensity float64) *Emotion {
	emotion := &Emotion{
		Type:      emotionType,
		Intensity: intensity,
		OnsetTime: time.Now(),
	}
	switch emotionType {
	case EmotionInterest:
		emotion.AttentionScope = 1.2
		emotion.ProcessingDepth = 1.3
		emotion.ApproachAvoidance = 0.8
		emotion.MemoryStrength = 1.2
		emotion.ExplorationBias = 0.6
	case EmotionJoy:
		emotion.AttentionScope = 1.5 
		emotion.ProcessingDepth = 1.0
		emotion.ApproachAvoidance = 0.9
		emotion.MemoryStrength = 1.3
		emotion.ExplorationBias = 0.8
	case EmotionSurprise:
		emotion.AttentionScope = 1.8 
		emotion.ProcessingDepth = 0.7 
		emotion.ApproachAvoidance = 0.0 
		emotion.MemoryStrength = 1.5 
		emotion.ExplorationBias = 0.5
	case EmotionFear:
		emotion.AttentionScope = 0.5 
		emotion.ProcessingDepth = 0.6 
		emotion.ApproachAvoidance = -0.9 
		emotion.MemoryStrength = 1.8 
		emotion.ExplorationBias = -0.7 
	case EmotionAnger:
		emotion.AttentionScope = 0.7
		emotion.ProcessingDepth = 0.8
		emotion.ApproachAvoidance = 0.7 
		emotion.MemoryStrength = 1.4
		emotion.ExplorationBias = 0.3
	case EmotionSadness:
		emotion.AttentionScope = 0.8
		emotion.ProcessingDepth = 1.4 
		emotion.ApproachAvoidance = -0.5
		emotion.MemoryStrength = 1.3
		emotion.ExplorationBias = -0.4
	case EmotionDisgust:
		emotion.AttentionScope = 0.6
		emotion.ProcessingDepth = 0.5
		emotion.ApproachAvoidance = -0.8
		emotion.MemoryStrength = 1.2
		emotion.ExplorationBias = -0.6
	default:
		emotion.AttentionScope = 1.0
		emotion.ProcessingDepth = 1.0
		emotion.ApproachAvoidance = 0.0
		emotion.MemoryStrength = 1.0
		emotion.ExplorationBias = 0.0
	}
	return emotion
}
func (es *EmotionSystem) TriggerEmotion(emotionType EmotionType, intensity float64, trigger string) {
	es.mu.Lock()
	defer es.mu.Unlock()
	intensity = math.Max(0.0, math.Min(1.0, intensity))
	if emotion, exists := es.emotions[emotionType]; exists {
		emotion.Intensity = intensity
		emotion.OnsetTime = time.Now()
	} else {
		es.emotions[emotionType] = es.createEmotion(emotionType, intensity)
	}
	es.recordEmotionEvent(emotionType, intensity, trigger)
	es.updateDimensionalAffect()
	es.updateDominantEmotion()
}
func (es *EmotionSystem) UpdateEmotions(deltaTime time.Duration) {
	es.mu.Lock()
	defer es.mu.Unlock()
	dt := deltaTime.Seconds()
	for _, emotion := range es.emotions {
		emotion.Intensity *= math.Exp(-es.decayRate * dt)
		if emotion.Intensity < 0.1 {
			emotion.Intensity = 0.1
		}
	}
	es.updateDimensionalAffect()
	es.updateDominantEmotion()
}
func (es *EmotionSystem) updateDimensionalAffect() {
	arousalSum := 0.0
	arousalSum += es.emotions[EmotionJoy].Intensity * 0.8
	arousalSum += es.emotions[EmotionFear].Intensity * 0.9
	arousalSum += es.emotions[EmotionAnger].Intensity * 0.9
	arousalSum += es.emotions[EmotionSurprise].Intensity * 1.0
	arousalSum += es.emotions[EmotionInterest].Intensity * 0.6
	arousalSum -= es.emotions[EmotionSadness].Intensity * 0.3
	es.arousal = math.Max(0.0, math.Min(1.0, arousalSum/5.0))
	valenceSum := 0.0
	valenceSum += es.emotions[EmotionJoy].Intensity * 1.0
	valenceSum += es.emotions[EmotionInterest].Intensity * 0.6
	valenceSum -= es.emotions[EmotionSadness].Intensity * 0.8
	valenceSum -= es.emotions[EmotionFear].Intensity * 0.9
	valenceSum -= es.emotions[EmotionAnger].Intensity * 0.7
	valenceSum -= es.emotions[EmotionDisgust].Intensity * 0.8
	es.valence = math.Max(-1.0, math.Min(1.0, valenceSum))
}
func (es *EmotionSystem) updateDominantEmotion() {
	maxIntensity := 0.0
	dominant := EmotionInterest 
	for emotionType, emotion := range es.emotions {
		if emotion.Intensity > maxIntensity {
			maxIntensity = emotion.Intensity
			dominant = emotionType
		}
	}
	es.dominantEmotion = dominant
	es.emotionBlend = make(map[EmotionType]float64)
	for emotionType, emotion := range es.emotions {
		if emotion.Intensity > 0.2 { 
			es.emotionBlend[emotionType] = emotion.Intensity
		}
	}
}
func (es *EmotionSystem) GetCognitiveEffects() CognitiveEffects {
	es.mu.RLock()
	defer es.mu.RUnlock()
	effects := CognitiveEffects{
		AttentionScope:    1.0,
		ProcessingDepth:   1.0,
		ApproachAvoidance: 0.0,
		MemoryStrength:    1.0,
		ExplorationBias:   0.0,
	}
	totalWeight := 0.0
	for emotionType, weight := range es.emotionBlend {
		if emotion, exists := es.emotions[emotionType]; exists {
			totalWeight += weight
			effects.AttentionScope += emotion.AttentionScope * weight
			effects.ProcessingDepth += emotion.ProcessingDepth * weight
			effects.ApproachAvoidance += emotion.ApproachAvoidance * weight
			effects.MemoryStrength += emotion.MemoryStrength * weight
			effects.ExplorationBias += emotion.ExplorationBias * weight
		}
	}
	if totalWeight > 0 {
		effects.AttentionScope /= (totalWeight + 1.0)
		effects.ProcessingDepth /= (totalWeight + 1.0)
		effects.ApproachAvoidance /= totalWeight
		effects.MemoryStrength /= (totalWeight + 1.0)
		effects.ExplorationBias /= totalWeight
	}
	return effects
}
type CognitiveEffects struct {
	AttentionScope    float64 
	ProcessingDepth   float64 
	ApproachAvoidance float64 
	MemoryStrength    float64 
	ExplorationBias   float64 
}
func (es *EmotionSystem) recordEmotionEvent(emotionType EmotionType, intensity float64, trigger string) {
	event := EmotionEvent{
		Timestamp: time.Now(),
		Emotion:   emotionType,
		Intensity: intensity,
		Trigger:   trigger,
	}
	es.emotionHistory = append(es.emotionHistory, event)
	if len(es.emotionHistory) > 100 {
		es.emotionHistory = es.emotionHistory[1:]
	}
}
func (es *EmotionSystem) GetEmotionalState() map[string]interface{} {
	es.mu.RLock()
	defer es.mu.RUnlock()
	return map[string]interface{}{
		"dominant_emotion": es.dominantEmotion.String(),
		"arousal":          es.arousal,
		"valence":          es.valence,
		"emotion_blend":    es.emotionBlend,
		"history_size":     len(es.emotionHistory),
	}
}
func (es *EmotionSystem) GetDominantEmotion() EmotionType {
	es.mu.RLock()
	defer es.mu.RUnlock()
	return es.dominantEmotion
}
func (es *EmotionSystem) GetArousal() float64 {
	es.mu.RLock()
	defer es.mu.RUnlock()
	return es.arousal
}
func (es *EmotionSystem) GetValence() float64 {
	es.mu.RLock()
	defer es.mu.RUnlock()
	return es.valence
}