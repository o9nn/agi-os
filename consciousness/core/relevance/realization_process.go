package relevance
import (
	"math"
	"sync"
)
type RealizationProcess struct {
	mu sync.RWMutex
	contextWeights map[string]float64
	salienceLandscape map[string]float64
	relevanceHistory []float64
	maxHistorySize   int
}
func NewRealizationProcess() *RealizationProcess {
	return &RealizationProcess{
		contextWeights: map[string]float64{
			"knowing":       0.33,
			"understanding": 0.33,
			"wisdom":        0.34,
		},
		salienceLandscape: make(map[string]float64),
		relevanceHistory:  make([]float64, 0, 100),
		maxHistorySize:    100,
	}
}
func (rp *RealizationProcess) CalculateRelevance(
	ka *KnowingAnalysis,
	ua *UnderstandingAnalysis,
	wa *WisdomAnalysis,
) float64 {
	rp.mu.RLock()
	defer rp.mu.RUnlock()
	relevance := (
		ka.OverallScore*rp.contextWeights["knowing"] +
		ua.OverallScore*rp.contextWeights["understanding"] +
		wa.OverallScore*rp.contextWeights["wisdom"])
	relevance = rp.modulateWithSalience(relevance)
	rp.storeRelevance(relevance)
	return relevance
}
func (rp *RealizationProcess) modulateWithSalience(baseRelevance float64) float64 {
	avgSalience := 0.0
	count := 0
	for _, s := range rp.salienceLandscape {
		avgSalience += s
		count++
	}
	if count > 0 {
		avgSalience /= float64(count)
		return baseRelevance * (0.7 + 0.3*avgSalience)
	}
	return baseRelevance
}
func (rp *RealizationProcess) storeRelevance(relevance float64) {
	if len(rp.relevanceHistory) >= rp.maxHistorySize {
		rp.relevanceHistory = rp.relevanceHistory[1:]
	}
	rp.relevanceHistory = append(rp.relevanceHistory, relevance)
}
func (rp *RealizationProcess) OptimizeWithWeights(
	weights map[string]float64,
	state *EnneadState,
) {
	rp.mu.Lock()
	defer rp.mu.Unlock()
	for k, v := range weights {
		rp.contextWeights[k] = v
	}
	rp.updateSalienceLandscape(state)
	rp.applySophrosyne(state)
}
func (rp *RealizationProcess) updateSalienceLandscape(state *EnneadState) {
	state.mu.RLock()
	defer state.mu.RUnlock()
	rp.salienceLandscape["propositional"] = state.PropositionalKnowledge
	rp.salienceLandscape["procedural"] = state.ProceduralKnowledge
	rp.salienceLandscape["perspectival"] = state.PerspectivalKnowledge
	rp.salienceLandscape["participatory"] = state.ParticipatoryKnowledge
	rp.salienceLandscape["nomological"] = state.NomologicalUnderstanding
	rp.salienceLandscape["normative"] = state.NormativeUnderstanding
	rp.salienceLandscape["narrative"] = state.NarrativeUnderstanding
	rp.salienceLandscape["morality"] = state.MoralDevelopment
	rp.salienceLandscape["meaning"] = state.MeaningRealization
	rp.salienceLandscape["mastery"] = state.MasteryAchievement
	for dim, value := range rp.salienceLandscape {
		if value < 0.4 {
			rp.salienceLandscape[dim] = value + (0.4 - value) * 0.5
		}
	}
}
func (rp *RealizationProcess) applySophrosyne(state *EnneadState) {
	state.mu.RLock()
	defer state.mu.RUnlock()
	values := []float64{
		state.PropositionalKnowledge,
		state.ProceduralKnowledge,
		state.PerspectivalKnowledge,
		state.ParticipatoryKnowledge,
		state.NomologicalUnderstanding,
		state.NormativeUnderstanding,
		state.NarrativeUnderstanding,
		state.MoralDevelopment,
		state.MeaningRealization,
		state.MasteryAchievement,
	}
	mean := 0.0
	for _, v := range values {
		mean += v
	}
	mean /= float64(len(values))
	variance := 0.0
	for _, v := range values {
		variance += math.Pow(v-mean, 2)
	}
	variance /= float64(len(values))
	if variance > 0.1 {
		rp.contextWeights["knowing"] = 0.34
		rp.contextWeights["understanding"] = 0.33
		rp.contextWeights["wisdom"] = 0.33
	} else if variance < 0.02 {
		knowingAvg := (state.PropositionalKnowledge + state.ProceduralKnowledge +
			state.PerspectivalKnowledge + state.ParticipatoryKnowledge) / 4.0
		understandingAvg := (state.NomologicalUnderstanding + state.NormativeUnderstanding +
			state.NarrativeUnderstanding) / 3.0
		wisdomAvg := (state.MoralDevelopment + state.MeaningRealization +
			state.MasteryAchievement) / 3.0
		if knowingAvg < understandingAvg && knowingAvg < wisdomAvg {
			rp.contextWeights["knowing"] = 0.40
			rp.contextWeights["understanding"] = 0.30
			rp.contextWeights["wisdom"] = 0.30
		} else if understandingAvg < wisdomAvg {
			rp.contextWeights["knowing"] = 0.30
			rp.contextWeights["understanding"] = 0.40
			rp.contextWeights["wisdom"] = 0.30
		} else {
			rp.contextWeights["knowing"] = 0.30
			rp.contextWeights["understanding"] = 0.30
			rp.contextWeights["wisdom"] = 0.40
		}
	}
}
func (rp *RealizationProcess) GetRelevanceHistory(n int) []float64 {
	rp.mu.RLock()
	defer rp.mu.RUnlock()
	histLen := len(rp.relevanceHistory)
	if n > histLen {
		n = histLen
	}
	if n == 0 {
		return []float64{}
	}
	history := make([]float64, n)
	copy(history, rp.relevanceHistory[histLen-n:])
	return history
}
func (rp *RealizationProcess) GetSalienceLandscape() map[string]float64 {
	rp.mu.RLock()
	defer rp.mu.RUnlock()
	landscape := make(map[string]float64)
	for k, v := range rp.salienceLandscape {
		landscape[k] = v
	}
	return landscape
}
func (rp *RealizationProcess) GetContextWeights() map[string]float64 {
	rp.mu.RLock()
	defer rp.mu.RUnlock()
	weights := make(map[string]float64)
	for k, v := range rp.contextWeights {
		weights[k] = v
	}
	return weights
}