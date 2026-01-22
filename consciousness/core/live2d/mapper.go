package live2d
import (
"fmt"
"math"
"sync"
"time"
)
type DefaultParameterMapper struct {
mu              sync.RWMutex
smoothingFactor float64
previousState   *AvatarState
}
func NewDefaultParameterMapper() *DefaultParameterMapper {
return &DefaultParameterMapper{
smoothingFactor: 0.3,
previousState:   nil,
}
}
func (m *DefaultParameterMapper) MapEmotionalState(state EmotionalState) []ModelParameter {
params := []ModelParameter{}
smileIntensity := math.Max(0, state.Valence)
params = append(params, ModelParameter{
ID:    StandardParameterNames.MouthSmile,
Value: smileIntensity,
Min:   0.0,
Max:   1.0,
})
params = append(params, ModelParameter{
ID:    StandardParameterNames.EyeSmileLeft,
Value: smileIntensity * 0.8,
Min:   0.0,
Max:   1.0,
})
params = append(params, ModelParameter{
ID:    StandardParameterNames.EyeSmileRight,
Value: smileIntensity * 0.8,
Min:   0.0,
Max:   1.0,
})
eyeOpenness := 0.5 + (state.Arousal * 0.5)
params = append(params, ModelParameter{
ID:    StandardParameterNames.EyeOpenLeft,
Value: eyeOpenness,
Min:   0.0,
Max:   1.0,
})
params = append(params, ModelParameter{
ID:    StandardParameterNames.EyeOpenRight,
Value: eyeOpenness,
Min:   0.0,
Max:   1.0,
})
if state.Curiosity > 0.6 {
params = append(params, ModelParameter{
ID:    StandardParameterNames.AngleX,
Value: (state.Curiosity - 0.6) * 20,
Min:   -30.0,
Max:   30.0,
})
}
bodyAngle := (state.Confidence - 0.5) * 10
params = append(params, ModelParameter{
ID:    StandardParameterNames.BodyAngleX,
Value: bodyAngle,
Min:   -30.0,
Max:   30.0,
})
return params
}
func (m *DefaultParameterMapper) MapCognitiveState(state CognitiveState) []ModelParameter {
params := []ModelParameter{}
breathingRate := 0.5 + (state.EnergyLevel * 0.5)
params = append(params, ModelParameter{
ID:    StandardParameterNames.Breathing,
Value: breathingRate,
Min:   0.0,
Max:   1.0,
})
blinkIntensity := 1.0 - (state.CognitiveLoad * 0.3)
params = append(params, ModelParameter{
ID:    StandardParameterNames.EyeOpenLeft,
Value: blinkIntensity,
Min:   0.0,
Max:   1.0,
})
params = append(params, ModelParameter{
ID:    StandardParameterNames.EyeOpenRight,
Value: blinkIntensity,
Min:   0.0,
Max:   1.0,
})
gazeDirectness := state.Awareness
params = append(params, ModelParameter{
ID:    StandardParameterNames.EyeBallX,
Value: (1.0 - gazeDirectness) * 5,
Min:   -30.0,
Max:   30.0,
})
var headAngleY float64
switch state.ProcessingMode {
case "contemplative":
headAngleY = -5.0
case "dynamic":
headAngleY = 5.0
case "cautious":
headAngleY = 0.0
case "creative":
headAngleY = 3.0
default:
headAngleY = 0.0
}
params = append(params, ModelParameter{
ID:    StandardParameterNames.AngleY,
Value: headAngleY,
Min:   -30.0,
Max:   30.0,
})
return params
}
func (m *DefaultParameterMapper) MapCombinedState(state AvatarState) []ModelParameter {
m.mu.Lock()
defer m.mu.Unlock()
emotionalParams := m.MapEmotionalState(state.Emotional)
cognitiveParams := m.MapCognitiveState(state.Cognitive)
paramMap := make(map[string]ModelParameter)
for _, param := range emotionalParams {
paramMap[param.ID] = param
}
for _, param := range cognitiveParams {
if existing, ok := paramMap[param.ID]; ok {
blended := (existing.Value + param.Value) / 2.0
param.Value = blended
}
paramMap[param.ID] = param
}
if m.previousState != nil && m.smoothingFactor > 0 {
for id, param := range paramMap {
smoothed := param
smoothed.Value = param.Value * (1.0 - m.smoothingFactor)
paramMap[id] = smoothed
}
}
stateCopy := state
m.previousState = &stateCopy
result := make([]ModelParameter, 0, len(paramMap))
for _, param := range paramMap {
result = append(result, param)
}
return result
}
func (m *DefaultParameterMapper) SetSmoothingFactor(factor float64) {
m.mu.Lock()
defer m.mu.Unlock()
if factor < 0.0 {
factor = 0.0
} else if factor > 1.0 {
factor = 1.0
}
m.smoothingFactor = factor
}
func clamp(value, min, max float64) float64 {
if value < min {
return min
}
if value > max {
return max
}
return value
}
func NewLive2DModel(name, modelPath string) *Live2DModel {
return &Live2DModel{
Name:       name,
ModelPath:  modelPath,
Parameters: make(map[string]*ModelParameter),
CurrentState: AvatarState{
Emotional: EmotionPresets["neutral"],
Cognitive: CognitiveState{
Awareness:      0.5,
Attention:      0.5,
CognitiveLoad:  0.3,
Coherence:      0.7,
EnergyLevel:    0.7,
ProcessingMode: "contemplative",
},
Timestamp: time.Now(),
},
UpdateRate: 16 * time.Millisecond,
}
}
func (m *Live2DModel) UpdateState(state AvatarState) error {
m.mu.Lock()
defer m.mu.Unlock()
state.Timestamp = time.Now()
m.CurrentState = state
return nil
}
func (m *Live2DModel) GetCurrentParameters(mapper ParameterMapper) []ModelParameter {
m.mu.RLock()
defer m.mu.RUnlock()
return mapper.MapCombinedState(m.CurrentState)
}
func (m *Live2DModel) GetCurrentState() AvatarState {
m.mu.RLock()
defer m.mu.RUnlock()
return m.CurrentState
}
func (m *Live2DModel) String() string {
return fmt.Sprintf("Live2DModel{Name: %s, Path: %s}", m.Name, m.ModelPath)
}