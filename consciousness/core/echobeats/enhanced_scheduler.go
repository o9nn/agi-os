package echobeats
import (
	"context"
	"fmt"
	"sync"
	"time"
)
type EnhancedScheduler struct {
	mu              sync.RWMutex
	ctx             context.Context
	cancel          context.CancelFunc
	echoBeats       *EchoBeats
	engines         []*InferenceEngine
	masterLoop      *CognitiveLoop
	wakeRestManager   interface{} 
	goalOrchestrator  interface{} 
	streamOfConsc     interface{} 
	dreamCycle        interface{} 
	loopCycles      uint64
	engineTasks     uint64
	running         bool
}
func NewEnhancedScheduler() *EnhancedScheduler {
	ctx, cancel := context.WithCancel(context.Background())
	es := &EnhancedScheduler{
		ctx:       ctx,
		cancel:    cancel,
		echoBeats: NewEchoBeats(),
		engines:   make([]*InferenceEngine, 0, 3),
	}
	es.engines = append(es.engines, NewInferenceEngine(1, SpecializationPerception))
	es.engines = append(es.engines, NewInferenceEngine(2, SpecializationCognition))
	es.engines = append(es.engines, NewInferenceEngine(3, SpecializationAction))
	es.masterLoop = NewCognitiveLoop()
	es.setupCallbacks()
	return es
}
func (es *EnhancedScheduler) setupCallbacks() {
	es.masterLoop.SetCallbacks(
		func(step int, result *StepResult) {
			es.onCognitiveStepComplete(step, result)
		},
		func(cycle uint64) {
			es.mu.Lock()
			es.loopCycles++
			es.mu.Unlock()
			fmt.Printf("🔄 Enhanced Scheduler: Cognitive cycle %d complete\n", cycle)
		},
	)
	es.registerEnhancedHandlers()
}
func (es *EnhancedScheduler) registerEnhancedHandlers() {
	es.echoBeats.RegisterHandler(EventThought, func(event *CognitiveEvent) error {
		task := &InferenceTask{
			ID:       event.ID,
			Type:     "thought_generation",
			Input:    event.Payload,
			Priority: float64(event.Priority) / 100.0,
			Context:  event.Context,
		}
		return es.engines[0].SubmitTask(task)
	})
	es.echoBeats.RegisterHandler(EventGoalPursuit, func(event *CognitiveEvent) error {
		task := &InferenceTask{
			ID:       event.ID,
			Type:     "goal_pursuit",
			Input:    event.Payload,
			Priority: float64(event.Priority) / 100.0,
			Context:  event.Context,
		}
		return es.engines[2].SubmitTask(task)
	})
	es.echoBeats.RegisterHandler(EventIntrospection, func(event *CognitiveEvent) error {
		task := &InferenceTask{
			ID:       event.ID,
			Type:     "introspection",
			Input:    event.Payload,
			Priority: float64(event.Priority) / 100.0,
			Context:  event.Context,
		}
		return es.engines[1].SubmitTask(task)
	})
	es.echoBeats.RegisterHandler(EventLearning, func(event *CognitiveEvent) error {
		task := &InferenceTask{
			ID:       event.ID,
			Type:     "learning",
			Input:    event.Payload,
			Priority: float64(event.Priority) / 100.0,
			Context:  event.Context,
		}
		return es.engines[1].SubmitTask(task)
	})
}
func (es *EnhancedScheduler) onCognitiveStepComplete(step int, result *StepResult) {
	if result == nil || !result.Success {
		return
	}
	if len(result.Insights) > 0 {
		for _, insight := range result.Insights {
			es.echoBeats.ScheduleEvent(&CognitiveEvent{
				ID:          fmt.Sprintf("insight_%d_%d", step, time.Now().UnixNano()),
				Type:        EventIntrospection,
				Priority:    70,
				ScheduledAt: time.Now().Add(1 * time.Second),
				Payload:     insight,
				Context: map[string]interface{}{
					"source_step": step,
					"from_loop":   true,
				},
			})
		}
	}
	if result.CognitiveLoad > 0.8 {
		es.echoBeats.ScheduleEvent(&CognitiveEvent{
			ID:          fmt.Sprintf("rest_trigger_%d", time.Now().UnixNano()),
			Type:        EventRest,
			Priority:    85,
			ScheduledAt: time.Now().Add(5 * time.Second),
			Payload:     "High cognitive load detected",
		})
	}
}
func (es *EnhancedScheduler) Start() error {
	es.mu.Lock()
	if es.running {
		es.mu.Unlock()
		return fmt.Errorf("enhanced scheduler already running")
	}
	es.running = true
	es.mu.Unlock()
	fmt.Println("🎵 Enhanced EchoBeats Scheduler: Starting...")
	fmt.Println("   Components:")
	fmt.Println("   • Original EchoBeats event scheduler")
	fmt.Println("   • 3 concurrent inference engines")
	fmt.Println("   • 12-step cognitive loop")
	if err := es.echoBeats.Start(); err != nil {
		return fmt.Errorf("failed to start EchoBeats: %w", err)
	}
	for _, engine := range es.engines {
		if err := engine.Start(); err != nil {
			return fmt.Errorf("failed to start inference engine: %w", err)
		}
	}
	if err := es.masterLoop.Start(); err != nil {
		return fmt.Errorf("failed to start cognitive loop: %w", err)
	}
	fmt.Println("🎵 Enhanced EchoBeats Scheduler: All systems operational!")
	return nil
}
func (es *EnhancedScheduler) Stop() error {
	es.mu.Lock()
	defer es.mu.Unlock()
	if !es.running {
		return fmt.Errorf("enhanced scheduler not running")
	}
	fmt.Println("🎵 Enhanced EchoBeats Scheduler: Stopping...")
	es.running = false
	es.cancel()
	if err := es.masterLoop.Stop(); err != nil {
		fmt.Printf("⚠️  Error stopping cognitive loop: %v\n", err)
	}
	for _, engine := range es.engines {
		if err := engine.Stop(); err != nil {
			fmt.Printf("⚠️  Error stopping inference engine: %v\n", err)
		}
	}
	if err := es.echoBeats.Stop(); err != nil {
		fmt.Printf("⚠️  Error stopping EchoBeats: %v\n", err)
	}
	return nil
}
func (es *EnhancedScheduler) ScheduleEvent(event *CognitiveEvent) {
	es.echoBeats.ScheduleEvent(event)
}
func (es *EnhancedScheduler) SetWakeRestManager(manager interface{}) {
	es.mu.Lock()
	defer es.mu.Unlock()
	es.wakeRestManager = manager
}
func (es *EnhancedScheduler) SetGoalOrchestrator(orchestrator interface{}) {
	es.mu.Lock()
	defer es.mu.Unlock()
	es.goalOrchestrator = orchestrator
}
func (es *EnhancedScheduler) SetStreamOfConsciousness(soc interface{}) {
	es.mu.Lock()
	defer es.mu.Unlock()
	es.streamOfConsc = soc
}
func (es *EnhancedScheduler) SetDreamCycle(dc interface{}) {
	es.mu.Lock()
	defer es.mu.Unlock()
	es.dreamCycle = dc
}
func (es *EnhancedScheduler) GetStatus() map[string]interface{} {
	es.mu.RLock()
	defer es.mu.RUnlock()
	echoBeatsStatus := es.echoBeats.GetStatus()
	loopMetrics := es.masterLoop.GetMetrics()
	engineMetrics := make([]map[string]interface{}, len(es.engines))
	for i, engine := range es.engines {
		engineMetrics[i] = engine.GetMetrics()
	}
	return map[string]interface{}{
		"running":          es.running,
		"loop_cycles":      es.loopCycles,
		"engine_tasks":     es.engineTasks,
		"echobeats":        echoBeatsStatus,
		"cognitive_loop":   loopMetrics,
		"inference_engines": engineMetrics,
	}
}
func (es *EnhancedScheduler) GetCognitiveState() *CognitiveState {
	return es.masterLoop.GetCurrentState()
}
func (es *EnhancedScheduler) GetEchoBeats() *EchoBeats {
	return es.echoBeats
}