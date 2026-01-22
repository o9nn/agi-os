package live2d
import (
	"context"
	"encoding/json"
	"fmt"
	"sync"
	"time"
)
type AvatarManager struct {
	mu              sync.RWMutex
	model           *Live2DModel
	mapper          ParameterMapper
	updateChan      chan AvatarState
	subscribers     []chan ParameterUpdate
	ctx             context.Context
	cancel          context.CancelFunc
	running         bool
}
func NewAvatarManager(modelName, modelPath string) *AvatarManager {
	ctx, cancel := context.WithCancel(context.Background())
	return &AvatarManager{
		model:       NewLive2DModel(modelName, modelPath),
		mapper:      NewDefaultParameterMapper(),
		updateChan:  make(chan AvatarState, 100),
		subscribers: make([]chan ParameterUpdate, 0),
		ctx:         ctx,
		cancel:      cancel,
		running:     false,
	}
}
func (am *AvatarManager) Start() error {
	am.mu.Lock()
	if am.running {
		am.mu.Unlock()
		return fmt.Errorf("avatar manager already running")
	}
	am.running = true
	am.mu.Unlock()
	go am.updateLoop()
	return nil
}
func (am *AvatarManager) Stop() error {
	am.mu.Lock()
	defer am.mu.Unlock()
	if !am.running {
		return fmt.Errorf("avatar manager not running")
	}
	am.cancel()
	am.running = false
	close(am.updateChan)
	for _, ch := range am.subscribers {
		close(ch)
	}
	am.subscribers = nil
	return nil
}
func (am *AvatarManager) UpdateEmotionalState(emotional EmotionalState) error {
	am.mu.RLock()
	currentState := am.model.GetCurrentState()
	am.mu.RUnlock()
	newState := AvatarState{
		Emotional: emotional,
		Cognitive: currentState.Cognitive,
		Timestamp: time.Now(),
	}
	select {
	case am.updateChan <- newState:
		return nil
	case <-am.ctx.Done():
		return fmt.Errorf("avatar manager stopped")
	case <-time.After(100 * time.Millisecond):
		return fmt.Errorf("timeout updating emotional state")
	}
}
func (am *AvatarManager) UpdateCognitiveState(cognitive CognitiveState) error {
	am.mu.RLock()
	currentState := am.model.GetCurrentState()
	am.mu.RUnlock()
	newState := AvatarState{
		Emotional: currentState.Emotional,
		Cognitive: cognitive,
		Timestamp: time.Now(),
	}
	select {
	case am.updateChan <- newState:
		return nil
	case <-am.ctx.Done():
		return fmt.Errorf("avatar manager stopped")
	case <-time.After(100 * time.Millisecond):
		return fmt.Errorf("timeout updating cognitive state")
	}
}
func (am *AvatarManager) UpdateFullState(state AvatarState) error {
	state.Timestamp = time.Now()
	select {
	case am.updateChan <- state:
		return nil
	case <-am.ctx.Done():
		return fmt.Errorf("avatar manager stopped")
	case <-time.After(100 * time.Millisecond):
		return fmt.Errorf("timeout updating full state")
	}
}
func (am *AvatarManager) Subscribe() (<-chan ParameterUpdate, error) {
	am.mu.Lock()
	defer am.mu.Unlock()
	if !am.running {
		return nil, fmt.Errorf("avatar manager not running")
	}
	ch := make(chan ParameterUpdate, 10)
	am.subscribers = append(am.subscribers, ch)
	return ch, nil
}
func (am *AvatarManager) GetCurrentState() AvatarState {
	am.mu.RLock()
	defer am.mu.RUnlock()
	return am.model.GetCurrentState()
}
func (am *AvatarManager) GetCurrentParameters() ([]byte, error) {
	am.mu.RLock()
	defer am.mu.RUnlock()
	params := am.model.GetCurrentParameters(am.mapper)
	return json.Marshal(params)
}
func (am *AvatarManager) updateLoop() {
	ticker := time.NewTicker(am.model.UpdateRate)
	defer ticker.Stop()
	var lastState *AvatarState
	for {
		select {
		case <-am.ctx.Done():
			return
		case newState, ok := <-am.updateChan:
			if !ok {
				return
			}
			if err := am.model.UpdateState(newState); err != nil {
				continue
			}
			lastState = &newState
		case <-ticker.C:
			if lastState != nil {
				am.publishParameters()
			}
		}
	}
}
func (am *AvatarManager) publishParameters() {
	am.mu.RLock()
	params := am.model.GetCurrentParameters(am.mapper)
	subscribers := am.subscribers
	am.mu.RUnlock()
	update := ParameterUpdate{
		Timestamp:  time.Now(),
		Parameters: params,
	}
	for _, ch := range subscribers {
		select {
		case ch <- update:
		default:
		}
	}
}
func (am *AvatarManager) SetEmotionPreset(presetName string) error {
	preset, ok := EmotionPresets[presetName]
	if !ok {
		return fmt.Errorf("unknown emotion preset: %s", presetName)
	}
	return am.UpdateEmotionalState(preset)
}
func BlendEmotions(emotion1, emotion2 EmotionalState, weight float64) EmotionalState {
	weight = clamp(weight, 0.0, 1.0)
	invWeight := 1.0 - weight
	return EmotionalState{
		Valence:    emotion1.Valence*invWeight + emotion2.Valence*weight,
		Arousal:    emotion1.Arousal*invWeight + emotion2.Arousal*weight,
		Dominance:  emotion1.Dominance*invWeight + emotion2.Dominance*weight,
		Curiosity:  emotion1.Curiosity*invWeight + emotion2.Curiosity*weight,
		Confidence: emotion1.Confidence*invWeight + emotion2.Confidence*weight,
	}
}
func (am *AvatarManager) AnimateEmotionTransition(from, to EmotionalState, duration time.Duration) error {
	steps := int(duration.Milliseconds() / am.model.UpdateRate.Milliseconds())
	if steps < 1 {
		steps = 1
	}
	go func() {
		for i := 0; i <= steps; i++ {
			weight := float64(i) / float64(steps)
			blended := BlendEmotions(from, to, weight)
			if err := am.UpdateEmotionalState(blended); err != nil {
				return
			}
			time.Sleep(am.model.UpdateRate)
		}
	}()
	return nil
}
func (am *AvatarManager) GetModelInfo() map[string]interface{} {
	am.mu.RLock()
	defer am.mu.RUnlock()
	return map[string]interface{}{
		"name":        am.model.Name,
		"model_path":  am.model.ModelPath,
		"update_rate": am.model.UpdateRate.String(),
		"running":     am.running,
		"subscribers": len(am.subscribers),
	}
}
func (am *AvatarManager) IsRunning() bool {
	am.mu.RLock()
	defer am.mu.RUnlock()
	return am.running
}