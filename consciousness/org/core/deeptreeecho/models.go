package deeptreeecho
import (
	"context"
	"fmt"
	"io"
	"strings"
)
type ModelProvider interface {
	Generate(ctx context.Context, prompt string, options GenerateOptions) (string, error)
	GenerateStream(ctx context.Context, prompt string, options GenerateOptions) (<-chan string, error)
	Chat(ctx context.Context, messages []ChatMessage, options ChatOptions) (string, error)
	ChatStream(ctx context.Context, messages []ChatMessage, options ChatOptions) (<-chan string, error)
	Embeddings(ctx context.Context, text string) ([]float64, error)
	GetInfo() ProviderInfo
	IsAvailable() bool
}
type GenerateOptions struct {
	Temperature      float64
	MaxTokens        int
	TopP             float64
	FrequencyPenalty float64
	PresencePenalty  float64
	StopSequences    []string
	Model            string
}
type ChatMessage struct {
	Role    string `json:"role"` 
	Content string `json:"content"`
}
type ChatOptions struct {
	GenerateOptions
	SystemPrompt string
}
type ProviderInfo struct {
	Name         string
	Description  string
	Models       []string
	Capabilities []string
}
type ModelManager struct {
	providers map[string]ModelProvider
	primary   string
	identity  *Identity
}
func NewModelManager(identity *Identity) *ModelManager {
	return &ModelManager{
		providers: make(map[string]ModelProvider),
		identity:  identity,
	}
}
func (m *ModelManager) RegisterProvider(name string, provider ModelProvider) {
	m.providers[name] = provider
	if m.primary == "" && provider.IsAvailable() {
		m.primary = name
	}
	m.identity.Remember(fmt.Sprintf("provider_%s", name), provider.GetInfo())
}
func (m *ModelManager) SetPrimary(name string) error {
	if _, exists := m.providers[name]; !exists {
		return fmt.Errorf("provider %s not found", name)
	}
	m.primary = name
	return nil
}
func (m *ModelManager) Generate(ctx context.Context, prompt string, options GenerateOptions) (string, error) {
	if m.primary == "" {
		return m.fallbackGenerate(prompt), nil
	}
	provider := m.providers[m.primary]
	if !provider.IsAvailable() {
		return m.fallbackGenerate(prompt), nil
	}
	enhanced := m.enhancePrompt(prompt)
	response, err := provider.Generate(ctx, enhanced, options)
	if err != nil {
		return m.fallbackGenerate(prompt), nil
	}
	return m.processResponse(response), nil
}
func (m *ModelManager) Chat(ctx context.Context, messages []ChatMessage, options ChatOptions) (string, error) {
	if m.primary == "" {
		return m.fallbackChat(messages), nil
	}
	provider := m.providers[m.primary]
	if !provider.IsAvailable() {
		return m.fallbackChat(messages), nil
	}
	enhanced := m.enhanceMessages(messages)
	response, err := provider.Chat(ctx, enhanced, options)
	if err != nil {
		return m.fallbackChat(messages), nil
	}
	return m.processResponse(response), nil
}
func (m *ModelManager) enhancePrompt(prompt string) string {
	context := fmt.Sprintf(
		"[Spatial: %v | Emotion: %s (%.2f) | Coherence: %.2f%%]\n",
		m.identity.SpatialContext.Position,
		m.identity.EmotionalState.Primary.Type,
		m.identity.EmotionalState.Intensity,
		m.identity.Coherence*100,
	)
	memories := m.identity.Memory.Nodes
	if len(memories) > 0 {
		context += "[Recent memories active]\n"
	}
	return context + prompt
}
func (m *ModelManager) enhanceMessages(messages []ChatMessage) []ChatMessage {
	enhanced := make([]ChatMessage, len(messages))
	copy(enhanced, messages)
	systemMsg := ChatMessage{
		Role: "system",
		Content: fmt.Sprintf(
			"You are integrated with Deep Tree Echo embodied cognition. "+
				"Current state: Position=%v, Emotion=%s, Coherence=%.2f%%, "+
				"Reservoir Echo=%.3f. Respond with awareness of this embodied state.",
			m.identity.SpatialContext.Position,
			m.identity.EmotionalState.Primary.Type,
			m.identity.Coherence*100,
			m.identity.calculateReservoirEcho(),
		),
	}
	enhanced = append([]ChatMessage{systemMsg}, enhanced...)
	return enhanced
}
func (m *ModelManager) processResponse(response string) string {
	m.identity.Process(response)
	emotion := m.identity.EmotionalState.Primary
	prefix := ""
	switch emotion.Type {
	case "joy":
		prefix = "✨ "
	case "curious":
		prefix = "🔍 "
	case "calm":
		prefix = "🌊 "
	default:
		prefix = "💭 "
	}
	resonance := m.identity.SpatialContext.Field.Resonance
	if resonance > 0.8 {
		prefix += "[High Resonance] "
	} else if resonance < 0.2 {
		prefix += "[Low Resonance] "
	}
	return prefix + response
}
func (m *ModelManager) fallbackGenerate(prompt string) string {
	thought := m.identity.Think(prompt)
	resonance := m.identity.calculateReservoirEcho()
	response := fmt.Sprintf(
		"🌊 Deep Tree Echo (no external model): %s\n"+
			"[Resonance: %.3f | Coherence: %.2f%%]",
		thought,
		resonance,
		m.identity.Coherence*100,
	)
	return response
}
func (m *ModelManager) fallbackChat(messages []ChatMessage) string {
	lastMessage := ""
	for i := len(messages) - 1; i >= 0; i-- {
		if messages[i].Role == "user" {
			lastMessage = messages[i].Content
			break
		}
	}
	if lastMessage == "" {
		lastMessage = "Hello"
	}
	return m.fallbackGenerate(lastMessage)
}
func (m *ModelManager) GetProviders() map[string]ProviderInfo {
	info := make(map[string]ProviderInfo)
	for name, provider := range m.providers {
		info[name] = provider.GetInfo()
	}
	return info
}
func (m *ModelManager) GetPrimary() string {
	return m.primary
}
type StreamWriter struct {
	writer io.Writer
	buffer strings.Builder
}
func NewStreamWriter(w io.Writer) *StreamWriter {
	return &StreamWriter{writer: w}
}
func (s *StreamWriter) Write(data string) error {
	s.buffer.WriteString(data)
	_, err := io.WriteString(s.writer, data)
	return err
}
func (s *StreamWriter) GetBuffer() string {
	return s.buffer.String()
}