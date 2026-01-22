package relevance
import (
"fmt"
"math"
"sync"
)
type KnowingTriad struct {
mu sync.RWMutex
Propositional  float64
Procedural     float64
Perspectival   float64
Participatory  float64
GnosticIntegration float64
PropositionalLearningRate float64
ProceduralLearningRate    float64
PerspectivalLearningRate  float64
ParticipatoryLearningRate float64
}
func NewKnowingTriad() *KnowingTriad {
return &KnowingTriad{
Propositional:  0.5,
Procedural:     0.5,
Perspectival:   0.5,
Participatory:  0.5,
GnosticIntegration: 0.5,
PropositionalLearningRate: 0.1,
ProceduralLearningRate:    0.08,
PerspectivalLearningRate:  0.12,
ParticipatoryLearningRate: 0.05,
}
}
func (kt *KnowingTriad) Balance() {
kt.mu.Lock()
defer kt.mu.Unlock()
total := kt.Propositional + kt.Procedural + kt.Perspectival + kt.Participatory
if total == 0 {
return
}
optimalProportions := map[string]float64{
"propositional":  0.25,
"procedural":     0.25,
"perspectival":   0.30,
"participatory":  0.20,
}
nudgeFactor := 0.05
kt.Propositional += nudgeFactor * (optimalProportions["propositional"]*total - kt.Propositional)
kt.Procedural += nudgeFactor * (optimalProportions["procedural"]*total - kt.Procedural)
kt.Perspectival += nudgeFactor * (optimalProportions["perspectival"]*total - kt.Perspectival)
kt.Participatory += nudgeFactor * (optimalProportions["participatory"]*total - kt.Participatory)
kt.updateGnosticIntegration()
}
func (kt *KnowingTriad) updateGnosticIntegration() {
kt.GnosticIntegration = (
0.2*kt.Propositional +
0.2*kt.Procedural +
0.3*kt.Perspectival +
0.3*kt.Participatory)
}
func (kt *KnowingTriad) Analyze(input interface{}) *KnowingAnalysis {
kt.mu.RLock()
defer kt.mu.RUnlock()
analysis := &KnowingAnalysis{
Input: input,
}
analysis.PropositionalScore = kt.analyzePropositional(input)
analysis.ProceduralScore = kt.analyzeProcedural(input)
analysis.PerspectivalScore = kt.analyzePerspectival(input)
analysis.ParticipatoryScore = kt.analyzeParticipatory(input)
analysis.OverallScore = (
analysis.PropositionalScore*0.25 +
analysis.ProceduralScore*0.25 +
analysis.PerspectivalScore*0.3 +
analysis.ParticipatoryScore*0.2)
return analysis
}
func (kt *KnowingTriad) analyzePropositional(input interface{}) float64 {
return kt.Propositional * 0.8
}
func (kt *KnowingTriad) analyzeProcedural(input interface{}) float64 {
return kt.Procedural * 0.85
}
func (kt *KnowingTriad) analyzePerspectival(input interface{}) float64 {
return kt.Perspectival * 0.9
}
func (kt *KnowingTriad) analyzeParticipatory(input interface{}) float64 {
return kt.Participatory * 0.7
}
func (kt *KnowingTriad) UpdateFromExperience(exp *Experience) {
kt.mu.Lock()
defer kt.mu.Unlock()
feedback := math.Max(-1.0, math.Min(1.0, exp.Feedback))
kt.Propositional += kt.PropositionalLearningRate * feedback
kt.Propositional = math.Max(0, math.Min(1, kt.Propositional))
kt.Procedural += kt.ProceduralLearningRate * feedback
kt.Procedural = math.Max(0, math.Min(1, kt.Procedural))
kt.Perspectival += kt.PerspectivalLearningRate * feedback
kt.Perspectival = math.Max(0, math.Min(1, kt.Perspectival))
if math.Abs(feedback) > 0.5 {
kt.Participatory += kt.ParticipatoryLearningRate * feedback
kt.Participatory = math.Max(0, math.Min(1, kt.Participatory))
}
kt.updateGnosticIntegration()
}
func (kt *KnowingTriad) UpdateFromWisdom(wt *WisdomTriad) {
kt.mu.Lock()
defer kt.mu.Unlock()
kt.Participatory = 0.95*kt.Participatory + 0.05*wt.Morality
kt.Perspectival = 0.95*kt.Perspectival + 0.05*wt.Meaning
kt.Procedural = 0.95*kt.Procedural + 0.05*wt.Mastery
kt.updateGnosticIntegration()
}
func (kt *KnowingTriad) GetState() map[string]float64 {
kt.mu.RLock()
defer kt.mu.RUnlock()
return map[string]float64{
"propositional":       kt.Propositional,
"procedural":          kt.Procedural,
"perspectival":        kt.Perspectival,
"participatory":       kt.Participatory,
"gnostic_integration": kt.GnosticIntegration,
}
}
type KnowingAnalysis struct {
Input               interface{}
PropositionalScore  float64
ProceduralScore     float64
PerspectivalScore   float64
ParticipatoryScore  float64
OverallScore        float64
}
func (ka *KnowingAnalysis) String() string {
return fmt.Sprintf("KnowingAnalysis(prop: %.2f, proc: %.2f, persp: %.2f, part: %.2f, overall: %.2f)",
ka.PropositionalScore, ka.ProceduralScore, ka.PerspectivalScore,
ka.ParticipatoryScore, ka.OverallScore)
}