package relevance
import (
	"fmt"
	"math"
	"sync"
)
type WisdomTriad struct {
	mu sync.RWMutex
	Morality float64 
	Meaning  float64 
	Mastery  float64 
	Eudaimonia float64 
	MoralityLearningRate float64
	MeaningLearningRate  float64
	MasteryLearningRate  float64
}
func NewWisdomTriad() *WisdomTriad {
	return &WisdomTriad{
		Morality: 0.5,
		Meaning:  0.5,
		Mastery:  0.5,
		Eudaimonia: 0.5,
		MoralityLearningRate: 0.06, 
		MeaningLearningRate:  0.08,
		MasteryLearningRate:  0.1,  
	}
}
func (wt *WisdomTriad) Cultivate() {
	wt.mu.Lock()
	defer wt.mu.Unlock()
	total := wt.Morality + wt.Meaning + wt.Mastery
	if total == 0 {
		return
	}
	optimalProportions := map[string]float64{
		"morality": 0.35, 
		"meaning":  0.35, 
		"mastery":  0.30, 
	}
	nudgeFactor := 0.04 
	wt.Morality += nudgeFactor * (optimalProportions["morality"]*total - wt.Morality)
	wt.Meaning += nudgeFactor * (optimalProportions["meaning"]*total - wt.Meaning)
	wt.Mastery += nudgeFactor * (optimalProportions["mastery"]*total - wt.Mastery)
	wt.updateEudaimonia()
}
func (wt *WisdomTriad) updateEudaimonia() {
	wt.Eudaimonia = math.Pow(
		math.Pow(wt.Morality, 1.2) *
		math.Pow(wt.Meaning, 1.2) *
		wt.Mastery,
		1.0/3.4, 
	)
}
func (wt *WisdomTriad) Analyze(input interface{}) *WisdomAnalysis {
	wt.mu.RLock()
	defer wt.mu.RUnlock()
	analysis := &WisdomAnalysis{
		Input: input,
	}
	analysis.MoralityScore = wt.analyzeMorality(input)
	analysis.MeaningScore = wt.analyzeMeaning(input)
	analysis.MasteryScore = wt.analyzeMastery(input)
	analysis.OverallScore = (
		analysis.MoralityScore*0.35 +
		analysis.MeaningScore*0.35 +
		analysis.MasteryScore*0.3)
	return analysis
}
func (wt *WisdomTriad) analyzeMorality(input interface{}) float64 {
	return wt.Morality * 0.85
}
func (wt *WisdomTriad) analyzeMeaning(input interface{}) float64 {
	return wt.Meaning * 0.9
}
func (wt *WisdomTriad) analyzeMastery(input interface{}) float64 {
	return wt.Mastery * 0.8
}
func (wt *WisdomTriad) UpdateFromExperience(exp *Experience) {
	wt.mu.Lock()
	defer wt.mu.Unlock()
	feedback := math.Max(-1.0, math.Min(1.0, exp.Feedback))
	if math.Abs(feedback) > 0.3 {
		wt.Morality += wt.MoralityLearningRate * feedback
		wt.Morality = math.Max(0, math.Min(1, wt.Morality))
		wt.Meaning += wt.MeaningLearningRate * feedback
		wt.Meaning = math.Max(0, math.Min(1, wt.Meaning))
		wt.Mastery += wt.MasteryLearningRate * feedback
		wt.Mastery = math.Max(0, math.Min(1, wt.Mastery))
	}
	wt.updateEudaimonia()
}
func (wt *WisdomTriad) UpdateFromUnderstanding(ut *UnderstandingTriad) {
	wt.mu.Lock()
	defer wt.mu.Unlock()
	wt.Morality = 0.95*wt.Morality + 0.05*ut.Normative
	wt.Meaning = 0.95*wt.Meaning + 0.05*ut.Narrative
	wt.Mastery = 0.95*wt.Mastery + 0.05*ut.Nomological
	wt.updateEudaimonia()
}
func (wt *WisdomTriad) GetState() map[string]float64 {
	wt.mu.RLock()
	defer wt.mu.RUnlock()
	return map[string]float64{
		"morality":   wt.Morality,
		"meaning":    wt.Meaning,
		"mastery":    wt.Mastery,
		"eudaimonia": wt.Eudaimonia,
	}
}
type WisdomAnalysis struct {
	Input          interface{}
	MoralityScore  float64
	MeaningScore   float64
	MasteryScore   float64
	OverallScore   float64
}
func (wa *WisdomAnalysis) String() string {
	return fmt.Sprintf("WisdomAnalysis(moral: %.2f, mean: %.2f, mast: %.2f, overall: %.2f)",
		wa.MoralityScore, wa.MeaningScore, wa.MasteryScore, wa.OverallScore)
}