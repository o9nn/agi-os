package live2d
import (
"sync"
"time"
)
type ModelParameter struct {
ID    string  `json:"id"`
Value float64 `json:"value"`
Min   float64 `json:"min"`
Max   float64 `json:"max"`
}
type ParameterUpdate struct {
Timestamp  time.Time        `json:"timestamp"`
Parameters []ModelParameter `json:"parameters"`
}
type EmotionalState struct {
Valence    float64 `json:"valence"`
Arousal    float64 `json:"arousal"`
Dominance  float64 `json:"dominance"`
Curiosity  float64 `json:"curiosity"`
Confidence float64 `json:"confidence"`
}
type CognitiveState struct {
Awareness      float64 `json:"awareness"`
Attention      float64 `json:"attention"`
CognitiveLoad  float64 `json:"cognitive_load"`
Coherence      float64 `json:"coherence"`
EnergyLevel    float64 `json:"energy_level"`
ProcessingMode string  `json:"processing_mode"`
}
type AvatarState struct {
Emotional EmotionalState `json:"emotional"`
Cognitive CognitiveState `json:"cognitive"`
Timestamp time.Time      `json:"timestamp"`
}
type Live2DModel struct {
mu          sync.RWMutex
ModelPath   string                    `json:"model_path"`
Name        string                    `json:"name"`
Parameters  map[string]*ModelParameter `json:"parameters"`
CurrentState AvatarState              `json:"current_state"`
UpdateRate  time.Duration             `json:"update_rate"`
}
type ParameterMapper interface {
MapEmotionalState(state EmotionalState) []ModelParameter
MapCognitiveState(state CognitiveState) []ModelParameter
MapCombinedState(state AvatarState) []ModelParameter
}
var StandardParameterNames = struct {
EyeOpenLeft      string
EyeOpenRight     string
EyeSmileLeft     string
EyeSmileRight    string
MouthOpenY       string
MouthForm        string
MouthSmile       string
EyeBallX         string
EyeBallY         string
AngleX           string
AngleY           string
AngleZ           string
BodyAngleX       string
BodyAngleY       string
BodyAngleZ       string
Breathing        string
}{
EyeOpenLeft:   "ParamEyeLOpen",
EyeOpenRight:  "ParamEyeROpen",
EyeSmileLeft:  "ParamEyeLSmile",
EyeSmileRight: "ParamEyeRSmile",
MouthOpenY:    "ParamMouthOpenY",
MouthForm:     "ParamMouthForm",
MouthSmile:    "ParamMouthSmile",
EyeBallX:      "ParamEyeBallX",
EyeBallY:      "ParamEyeBallY",
AngleX:        "ParamAngleX",
AngleY:        "ParamAngleY",
AngleZ:        "ParamAngleZ",
BodyAngleX:    "ParamBodyAngleX",
BodyAngleY:    "ParamBodyAngleY",
BodyAngleZ:    "ParamBodyAngleZ",
Breathing:     "ParamBreath",
}
var EmotionPresets = map[string]EmotionalState{
"neutral": {
Valence:    0.0,
Arousal:    0.3,
Dominance:  0.5,
Curiosity:  0.3,
Confidence: 0.5,
},
"happy": {
Valence:    0.8,
Arousal:    0.6,
Dominance:  0.6,
Curiosity:  0.4,
Confidence: 0.7,
},
"sad": {
Valence:    -0.6,
Arousal:    0.2,
Dominance:  0.3,
Curiosity:  0.2,
Confidence: 0.3,
},
"curious": {
Valence:    0.3,
Arousal:    0.5,
Dominance:  0.4,
Curiosity:  0.9,
Confidence: 0.5,
},
"confident": {
Valence:    0.5,
Arousal:    0.5,
Dominance:  0.8,
Curiosity:  0.4,
Confidence: 0.9,
},
"contemplative": {
Valence:    0.2,
Arousal:    0.3,
Dominance:  0.5,
Curiosity:  0.7,
Confidence: 0.6,
},
"excited": {
Valence:    0.7,
Arousal:    0.9,
Dominance:  0.7,
Curiosity:  0.6,
Confidence: 0.7,
},
}