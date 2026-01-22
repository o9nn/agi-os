package consciousness
import (
	"context"
	"fmt"
	"sync"
	"time"
)
type LayerMessage struct {
	ID          string
	Timestamp   time.Time
	FromLayer   LayerIdentifier
	ToLayer     LayerIdentifier
	MessageType MessageType
	Content     string
	Priority    float64
	Context     map[string]interface{}
}
type LayerIdentifier string
const (
	LayerBasic      LayerIdentifier = "basic"
	LayerReflective LayerIdentifier = "reflective"
	LayerMetaCog    LayerIdentifier = "meta_cognitive"
)
type MessageType string
const (
	MessagePerception   MessageType = "perception"    
	MessagePattern      MessageType = "pattern"       
	MessageAnomaly      MessageType = "anomaly"       
	MessageReflection   MessageType = "reflection"    
	MessageQuestion     MessageType = "question"      
	MessageGoal         MessageType = "goal"          
	MessageAttention    MessageType = "attention"     
	MessageStrategy     MessageType = "strategy"      
	MessageInhibition   MessageType = "inhibition"    
	MessageFeedback     MessageType = "feedback"      
	MessageEmergence    MessageType = "emergence"     
)
type LayerCommunicationHub struct {
	mu              sync.RWMutex
	ctx             context.Context
	cancel          context.CancelFunc
	basicChannel      chan *LayerMessage
	reflectiveChannel chan *LayerMessage
	metaCogChannel    chan *LayerMessage
	messageHistory    []*LayerMessage
	maxHistorySize    int
	basicHandler      LayerHandler
	reflectiveHandler LayerHandler
	metaCogHandler    LayerHandler
	messagesProcessed uint64
	emergenceDetected uint64
	running           bool
}
type LayerHandler interface {
	ProcessMessage(msg *LayerMessage) ([]*LayerMessage, error)
	GetLayerState() map[string]interface{}
}
func NewLayerCommunicationHub() *LayerCommunicationHub {
	ctx, cancel := context.WithCancel(context.Background())
	return &LayerCommunicationHub{
		ctx:               ctx,
		cancel:            cancel,
		basicChannel:      make(chan *LayerMessage, 100),
		reflectiveChannel: make(chan *LayerMessage, 100),
		metaCogChannel:    make(chan *LayerMessage, 100),
		messageHistory:    make([]*LayerMessage, 0),
		maxHistorySize:    1000,
	}
}
func (hub *LayerCommunicationHub) RegisterHandler(layer LayerIdentifier, handler LayerHandler) {
	hub.mu.Lock()
	defer hub.mu.Unlock()
	switch layer {
	case LayerBasic:
		hub.basicHandler = handler
	case LayerReflective:
		hub.reflectiveHandler = handler
	case LayerMetaCog:
		hub.metaCogHandler = handler
	}
}
func (hub *LayerCommunicationHub) Start() error {
	hub.mu.Lock()
	if hub.running {
		hub.mu.Unlock()
		return fmt.Errorf("communication hub already running")
	}
	hub.running = true
	hub.mu.Unlock()
	go hub.processBasicLayer()
	go hub.processReflectiveLayer()
	go hub.processMetaCogLayer()
	go hub.detectEmergence()
	return nil
}
func (hub *LayerCommunicationHub) Stop() {
	hub.mu.Lock()
	if !hub.running {
		hub.mu.Unlock()
		return
	}
	hub.running = false
	hub.mu.Unlock()
	hub.cancel()
}
func (hub *LayerCommunicationHub) SendMessage(msg *LayerMessage) error {
	hub.mu.RLock()
	if !hub.running {
		hub.mu.RUnlock()
		return fmt.Errorf("communication hub not running")
	}
	hub.mu.RUnlock()
	hub.mu.Lock()
	hub.messageHistory = append(hub.messageHistory, msg)
	if len(hub.messageHistory) > hub.maxHistorySize {
		hub.messageHistory = hub.messageHistory[1:]
	}
	hub.mu.Unlock()
	switch msg.ToLayer {
	case LayerBasic:
		select {
		case hub.basicChannel <- msg:
		default:
			return fmt.Errorf("basic layer channel full")
		}
	case LayerReflective:
		select {
		case hub.reflectiveChannel <- msg:
		default:
			return fmt.Errorf("reflective layer channel full")
		}
	case LayerMetaCog:
		select {
		case hub.metaCogChannel <- msg:
		default:
			return fmt.Errorf("meta-cognitive layer channel full")
		}
	default:
		return fmt.Errorf("unknown layer: %s", msg.ToLayer)
	}
	return nil
}
func (hub *LayerCommunicationHub) processBasicLayer() {
	for {
		select {
		case <-hub.ctx.Done():
			return
		case msg := <-hub.basicChannel:
			hub.processLayerMessage(LayerBasic, msg)
		}
	}
}
func (hub *LayerCommunicationHub) processReflectiveLayer() {
	for {
		select {
		case <-hub.ctx.Done():
			return
		case msg := <-hub.reflectiveChannel:
			hub.processLayerMessage(LayerReflective, msg)
		}
	}
}
func (hub *LayerCommunicationHub) processMetaCogLayer() {
	for {
		select {
		case <-hub.ctx.Done():
			return
		case msg := <-hub.metaCogChannel:
			hub.processLayerMessage(LayerMetaCog, msg)
		}
	}
}
func (hub *LayerCommunicationHub) processLayerMessage(layer LayerIdentifier, msg *LayerMessage) {
	hub.mu.RLock()
	var handler LayerHandler
	switch layer {
	case LayerBasic:
		handler = hub.basicHandler
	case LayerReflective:
		handler = hub.reflectiveHandler
	case LayerMetaCog:
		handler = hub.metaCogHandler
	}
	hub.mu.RUnlock()
	if handler == nil {
		return 
	}
	responses, err := handler.ProcessMessage(msg)
	if err != nil {
		return
	}
	for _, response := range responses {
		hub.SendMessage(response)
	}
	hub.mu.Lock()
	hub.messagesProcessed++
	hub.mu.Unlock()
}
func (hub *LayerCommunicationHub) detectEmergence() {
	ticker := time.NewTicker(30 * time.Second)
	defer ticker.Stop()
	for {
		select {
		case <-hub.ctx.Done():
			return
		case <-ticker.C:
			hub.analyzeEmergence()
		}
	}
}
func (hub *LayerCommunicationHub) analyzeEmergence() {
	hub.mu.RLock()
	defer hub.mu.RUnlock()
	if len(hub.messageHistory) < 10 {
		return
	}
	recentMessages := hub.messageHistory[len(hub.messageHistory)-20:]
	typeCount := make(map[MessageType]int)
	for _, msg := range recentMessages {
		typeCount[msg.MessageType]++
	}
	if typeCount[MessageReflection] > 5 && typeCount[MessageQuestion] > 3 {
		hub.emergenceDetected++
		fmt.Println("🌟 Emergence detected: Reflective inquiry cascade")
	}
	if typeCount[MessagePattern] > 3 && typeCount[MessageAttention] > 2 {
		hub.emergenceDetected++
		fmt.Println("🌟 Emergence detected: Pattern-driven attention shift")
	}
}
func (hub *LayerCommunicationHub) GetMetrics() map[string]interface{} {
	hub.mu.RLock()
	defer hub.mu.RUnlock()
	return map[string]interface{}{
		"messages_processed":  hub.messagesProcessed,
		"emergence_detected":  hub.emergenceDetected,
		"message_history_size": len(hub.messageHistory),
		"basic_queue":         len(hub.basicChannel),
		"reflective_queue":    len(hub.reflectiveChannel),
		"meta_cog_queue":      len(hub.metaCogChannel),
	}
}
func (hub *LayerCommunicationHub) GetRecentMessages(n int) []*LayerMessage {
	hub.mu.RLock()
	defer hub.mu.RUnlock()
	if len(hub.messageHistory) == 0 {
		return []*LayerMessage{}
	}
	start := len(hub.messageHistory) - n
	if start < 0 {
		start = 0
	}
	messages := make([]*LayerMessage, len(hub.messageHistory)-start)
	copy(messages, hub.messageHistory[start:])
	return messages
}
func CreateMessage(from, to LayerIdentifier, msgType MessageType, content string, priority float64) *LayerMessage {
	return &LayerMessage{
		ID:          fmt.Sprintf("msg-%d", time.Now().UnixNano()),
		Timestamp:   time.Now(),
		FromLayer:   from,
		ToLayer:     to,
		MessageType: msgType,
		Content:     content,
		Priority:    priority,
		Context:     make(map[string]interface{}),
	}
}