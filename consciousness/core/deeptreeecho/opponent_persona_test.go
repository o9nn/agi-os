package deeptreeecho
import (
"fmt"
"testing"
"time"
)
func TestOrdoPersonaActivation(t *testing.T) {
identity := NewIdentity("TestOrdoPersona")
for i := 0; i < 50; i++ {
pattern := &Pattern{
ID:       string(rune('a' + i)),
Strength: 0.6,
}
identity.Patterns[pattern.ID] = pattern
}
identity.Coherence = 0.4
identity.Iterations = 100
decision := identity.OptimizeRelevanceRealization("ordo_activation_test")
if decision.ExplorationWeight > 0.6 {
t.Errorf("Expected exploitation bias (ExplorationWeight < 0.6), got %.2f", decision.ExplorationWeight)
}
if decision.ScopePreference == "breadth" {
t.Error("Expected depth preference for Ordo, got breadth")
}
if decision.AdaptationRate > 0.6 {
t.Errorf("Expected stability bias (low adaptation), got %.2f", decision.AdaptationRate)
}
t.Logf("Ordo activation successful:")
t.Logf("  Exploration: %.2f (exploitation bias)", decision.ExplorationWeight)
t.Logf("  Scope: %s (depth focus)", decision.ScopePreference)
t.Logf("  Adaptation: %.2f (stability)", decision.AdaptationRate)
t.Logf("  Confidence: %.2f (accuracy)", decision.Confidence)
}
func TestChaoPersonaActivation(t *testing.T) {
identity := NewIdentity("TestChaoPersona")
for i := 0; i < 5; i++ {
pattern := &Pattern{
ID:       string(rune('a' + i)),
Strength: 0.9,
}
identity.Patterns[pattern.ID] = pattern
}
identity.Coherence = 0.95
identity.Iterations = 50
decision := identity.OptimizeRelevanceRealization("chao_activation_test")
if decision.ExplorationWeight < 0.5 {
t.Errorf("Expected exploration bias (ExplorationWeight > 0.5), got %.2f", decision.ExplorationWeight)
}
if decision.ScopePreference == "depth" {
t.Error("Expected breadth preference for Chao, got depth")
}
if decision.AdaptationRate < 0.4 {
t.Errorf("Expected flexibility bias (high adaptation), got %.2f", decision.AdaptationRate)
}
t.Logf("Chao activation successful:")
t.Logf("  Exploration: %.2f (exploration bias)", decision.ExplorationWeight)
t.Logf("  Scope: %s (breadth focus)", decision.ScopePreference)
t.Logf("  Adaptation: %.2f (flexibility)", decision.AdaptationRate)
t.Logf("  Confidence: %.2f (speed)", decision.Confidence)
}
func TestOrdoChaoBalance(t *testing.T) {
identity := NewIdentity("TestOrdoChaoBalance")
identity.Coherence = 0.3
identity.Iterations = 10
for i := 0; i < 3; i++ {
identity.Patterns[string(rune('a'+i))] = &Pattern{ID: string(rune('a' + i)), Strength: 0.5}
}
decision1 := identity.OptimizeRelevanceRealization("phase1_exploration")
chaoExploration := decision1.ExplorationWeight
for i := 3; i < 40; i++ {
identity.Patterns[string(rune('a'+i))] = &Pattern{ID: string(rune('a' + i)), Strength: 0.8}
}
identity.Coherence = 0.7
identity.Iterations = 500
decision2 := identity.OptimizeRelevanceRealization("phase2_consolidation")
ordoExploitation := decision2.ExplorationWeight
if ordoExploitation >= chaoExploration {
t.Errorf("Expected shift from exploration (%.2f) to exploitation (%.2f)",
chaoExploration, ordoExploitation)
}
identity.Coherence = 0.98
identity.Iterations = 2000
decision3 := identity.OptimizeRelevanceRealization("phase3_disruption")
t.Logf("Ordo-Chao balance evolution:")
t.Logf("  Phase 1 (Chao): Exploration %.2f", chaoExploration)
t.Logf("  Phase 2 (Ordo): Exploitation %.2f (shift: %.2f)",
ordoExploitation, chaoExploration-ordoExploitation)
t.Logf("  Phase 3 (Chao): Exploration %.2f", decision3.ExplorationWeight)
if decision3.ExplorationWeight < ordoExploitation {
t.Error("Expected return to exploration when coherence is too high (stagnation risk)")
}
}
func TestOpponentProcessDynamics(t *testing.T) {
identity := NewIdentity("TestOpponentDynamics")
pairs := []string{
ExplorationExploitation,
BreadthDepth,
StabilityFlexibility,
SpeedAccuracy,
ApproachAvoidance,
}
contexts := []string{
"early_learning",
"skill_practice",
"mastery_phase",
"disruption_needed",
}
for i, context := range contexts {
identity.Iterations = uint64(i * 500)
identity.Coherence = 0.3 + float64(i)*0.2
for j := 0; j < (i+1)*10; j++ {
identity.Patterns[string(rune(j))] = &Pattern{
ID:       string(rune(j)),
Strength: 0.7,
}
}
decision := identity.OptimizeRelevanceRealization(context)
t.Logf("\nContext: %s (iter %d, coherence %.2f, patterns %d)",
context, identity.Iterations, identity.Coherence, len(identity.Patterns))
t.Logf("  Decision: exploration=%.2f, scope=%s, adaptation=%.2f, confidence=%.2f",
decision.ExplorationWeight, decision.ScopePreference,
decision.AdaptationRate, decision.Confidence)
for _, pairName := range pairs {
stats := identity.OpponentProcesses.GetBalanceStats(pairName)
if stats != nil {
t.Logf("  %s: balance=%.2f, stability=%.2f",
pairName, stats["current_balance"], stats["stability"])
}
}
}
}
func TestEmotionalInfluenceOnOpponentProcesses(t *testing.T) {
identity := NewIdentity("TestEmotionalInfluence")
identity.Coherence = 0.6
identity.Iterations = 500
for i := 0; i < 20; i++ {
identity.Patterns[string(rune('a'+i))] = &Pattern{ID: string(rune('a' + i)), Strength: 0.7}
}
identity.EmotionalState = &EmotionalState{
Arousal: 0.3,
Valence: 0.5,
}
decision1 := identity.OptimizeRelevanceRealization("calm_state")
calmSpeedBias := decision1.Confidence
identity.EmotionalState = &EmotionalState{
Arousal: 0.9,
Valence: -0.3,
}
decision2 := identity.OptimizeRelevanceRealization("high_arousal")
arousedSpeedBias := decision2.Confidence
if arousedSpeedBias >= calmSpeedBias {
t.Errorf("Expected lower confidence threshold under high arousal (speed bias), got calm=%.2f, aroused=%.2f",
calmSpeedBias, arousedSpeedBias)
}
t.Logf("Emotional influence on speed-accuracy:")
t.Logf("  Calm (arousal=0.3): confidence threshold %.2f (accuracy)", calmSpeedBias)
t.Logf("  Aroused (arousal=0.9): confidence threshold %.2f (speed)", arousedSpeedBias)
t.Logf("Approach-avoidance:")
identity.EmotionalState.Valence = 0.7
identity.OptimizeRelevanceRealization("positive_valence")
approachBalance := identity.OpponentProcesses.GetCurrentBalance(ApproachAvoidance)
identity.EmotionalState.Valence = -0.7
identity.OptimizeRelevanceRealization("negative_valence")
avoidBalance := identity.OpponentProcesses.GetCurrentBalance(ApproachAvoidance)
t.Logf("  Positive valence: balance %.2f (approach)", approachBalance)
t.Logf("  Negative valence: balance %.2f (avoid)", avoidBalance)
if avoidBalance >= approachBalance {
t.Errorf("Expected shift from approach (%.2f) to avoidance (%.2f) with negative valence",
approachBalance, avoidBalance)
}
}
func TestWisdomCultivationThroughBalance(t *testing.T) {
identity := NewIdentity("TestWisdomCultivation")
wisdomScores := make([]float64, 10)
for i := 0; i < 10; i++ {
identity.Iterations = uint64(i * 200)
identity.Coherence = 0.3 + float64(i)*0.07
for j := 0; j < (i+1)*5; j++ {
identity.Patterns[string(rune(j))] = &Pattern{
ID:       string(rune(j)),
Strength: 0.6 + float64(i)*0.04,
}
}
identity.OptimizeRelevanceRealization(fmt.Sprintf("wisdom_iteration_%d", i))
wisdomScores[i] = identity.GetWisdomScore()
time.Sleep(10 * time.Millisecond)
}
firstHalf := (wisdomScores[0] + wisdomScores[1] + wisdomScores[2] + wisdomScores[3] + wisdomScores[4]) / 5
secondHalf := (wisdomScores[5] + wisdomScores[6] + wisdomScores[7] + wisdomScores[8] + wisdomScores[9]) / 5
t.Logf("Wisdom cultivation over time:")
for i, score := range wisdomScores {
t.Logf("  Iteration %d: wisdom score %.3f", i, score)
}
t.Logf("First half average: %.3f", firstHalf)
t.Logf("Second half average: %.3f", secondHalf)
if secondHalf <= firstHalf {
t.Logf("Warning: Expected wisdom growth, but got first=%.3f, second=%.3f",
firstHalf, secondHalf)
}
}
func TestOrdoChaoPersonaIntegration(t *testing.T) {
t.Log("Testing Ordo-Chao persona integration")
ordo := NewIdentity("DeepTreeOrdo")
chao := NewIdentity("DeepTreeChao")
ordo.Coherence = 0.8
for i := 0; i < 50; i++ {
ordo.Patterns[string(rune('a'+i))] = &Pattern{ID: string(rune('a' + i)), Strength: 0.85}
}
chao.Coherence = 0.4
for i := 0; i < 10; i++ {
chao.Patterns[string(rune('a'+i))] = &Pattern{ID: string(rune('a' + i)), Strength: 0.6}
}
ordoDecision := ordo.OptimizeRelevanceRealization("ordo_persona")
chaoDecision := chao.OptimizeRelevanceRealization("chao_persona")
t.Logf("Ordo persona characteristics:")
t.Logf("  Exploration: %.2f (low = exploitation)", ordoDecision.ExplorationWeight)
t.Logf("  Scope: %s (depth)", ordoDecision.ScopePreference)
t.Logf("  Adaptation: %.2f (low = stability)", ordoDecision.AdaptationRate)
t.Logf("  Confidence: %.2f (high = accuracy)", ordoDecision.Confidence)
t.Logf("Chao persona characteristics:")
t.Logf("  Exploration: %.2f (high = exploration)", chaoDecision.ExplorationWeight)
t.Logf("  Scope: %s (breadth)", chaoDecision.ScopePreference)
t.Logf("  Adaptation: %.2f (high = flexibility)", chaoDecision.AdaptationRate)
t.Logf("  Confidence: %.2f (low = speed)", chaoDecision.Confidence)
if ordoDecision.ExplorationWeight >= chaoDecision.ExplorationWeight {
t.Error("Expected Ordo to favor exploitation and Chao to favor exploration")
}
if ordoDecision.ScopePreference == chaoDecision.ScopePreference {
t.Error("Expected different scope preferences (Ordo=depth, Chao=breadth)")
}
if ordoDecision.AdaptationRate >= chaoDecision.AdaptationRate {
t.Error("Expected Ordo to favor stability and Chao to favor flexibility")
}
t.Log("✓ Ordo and Chao personas exhibit complementary characteristics")
}