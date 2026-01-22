package relevance
import (
"fmt"
"math"
"sync"
)
type UnderstandingTriad struct {
mu sync.RWMutex
Nomological float64
Normative   float64
Narrative   float64
MeaningIntegration float64
NomologicalLearningRate float64
NormativeLearningRate   float64
NarrativeLearningRate   float64
}
func NewUnderstandingTriad() *UnderstandingTriad {
return &UnderstandingTriad{
Nomological: 0.5,
Normative:   0.5,
Narrative:   0.5,
MeaningIntegration: 0.5,
NomologicalLearningRate: 0.1,
NormativeLearningRate:   0.08,
NarrativeLearningRate:   0.12,
}
}
func (ut *UnderstandingTriad) Integrate() {
ut.mu.Lock()
defer ut.mu.Unlock()
total := ut.Nomological + ut.Normative + ut.Narrative
if total == 0 {
return
}
optimalProportions := map[string]float64{
"nomological": 0.30,
"normative":   0.35,
"narrative":   0.35,
}
nudgeFactor := 0.05
ut.Nomological += nudgeFactor * (optimalProportions["nomological"]*total - ut.Nomological)
ut.Normative += nudgeFactor * (optimalProportions["normative"]*total - ut.Normative)
ut.Narrative += nudgeFactor * (optimalProportions["narrative"]*total - ut.Narrative)
ut.updateMeaningIntegration()
}
func (ut *UnderstandingTriad) updateMeaningIntegration() {
ut.MeaningIntegration = math.Pow(
ut.Nomological*ut.Normative*ut.Narrative,
1.0/3.0,
)
}
func (ut *UnderstandingTriad) Analyze(input interface{}) *UnderstandingAnalysis {
ut.mu.RLock()
defer ut.mu.RUnlock()
analysis := &UnderstandingAnalysis{
Input: input,
}
analysis.NomologicalScore = ut.analyzeNomological(input)
analysis.NormativeScore = ut.analyzeNormative(input)
analysis.NarrativeScore = ut.analyzeNarrative(input)
analysis.OverallScore = (
analysis.NomologicalScore*0.3 +
analysis.NormativeScore*0.35 +
analysis.NarrativeScore*0.35)
return analysis
}
func (ut *UnderstandingTriad) analyzeNomological(input interface{}) float64 {
return ut.Nomological * 0.85
}
func (ut *UnderstandingTriad) analyzeNormative(input interface{}) float64 {
return ut.Normative * 0.9
}
func (ut *UnderstandingTriad) analyzeNarrative(input interface{}) float64 {
return ut.Narrative * 0.8
}
func (ut *UnderstandingTriad) UpdateFromExperience(exp *Experience) {
ut.mu.Lock()
defer ut.mu.Unlock()
feedback := math.Max(-1.0, math.Min(1.0, exp.Feedback))
ut.Nomological += ut.NomologicalLearningRate * feedback
ut.Nomological = math.Max(0, math.Min(1, ut.Nomological))
ut.Normative += ut.NormativeLearningRate * feedback
ut.Normative = math.Max(0, math.Min(1, ut.Normative))
ut.Narrative += ut.NarrativeLearningRate * feedback
ut.Narrative = math.Max(0, math.Min(1, ut.Narrative))
ut.updateMeaningIntegration()
}
func (ut *UnderstandingTriad) UpdateFromKnowing(kt *KnowingTriad) {
ut.mu.Lock()
defer ut.mu.Unlock()
ut.Nomological = 0.95*ut.Nomological + 0.05*kt.Propositional
ut.Normative = 0.95*ut.Normative + 0.05*kt.Perspectival
ut.Narrative = 0.95*ut.Narrative + 0.05*kt.Participatory
ut.updateMeaningIntegration()
}
func (ut *UnderstandingTriad) GetState() map[string]float64 {
ut.mu.RLock()
defer ut.mu.RUnlock()
return map[string]float64{
"nomological":         ut.Nomological,
"normative":           ut.Normative,
"narrative":           ut.Narrative,
"meaning_integration": ut.MeaningIntegration,
}
}
type UnderstandingAnalysis struct {
Input             interface{}
NomologicalScore  float64
NormativeScore    float64
NarrativeScore    float64
OverallScore      float64
}
func (ua *UnderstandingAnalysis) String() string {
return fmt.Sprintf("UnderstandingAnalysis(nomo: %.2f, norm: %.2f, narr: %.2f, overall: %.2f)",
ua.NomologicalScore, ua.NormativeScore, ua.NarrativeScore, ua.OverallScore)
}