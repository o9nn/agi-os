package echobridge

import (
	"context"
	"fmt"
	"log"
	"sync"
	"time"

	"google.golang.org/grpc"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"
)

// EchoBridgeServer implements the EchoBridge gRPC service
type EchoBridgeServer struct {
	UnimplementedEchoBridgeServer
	
	// State management
	mu              sync.RWMutex
	currentState    *CognitiveState
	events          []*CognitiveEvent
	goals           map[string]*Goal
	thoughtStream   chan *Thought
	eventStream     chan *CognitiveEvent
	
	// Engine states
	engine0State    *EngineState // Memory Engine
	engine1State    *EngineState // Coherence Engine
	engine2State    *EngineState // Imagination Engine
	
	// Scheduler integration
	scheduler       Scheduler
	
	// Metrics
	totalThoughts   int64
	totalEvents     int64
}

// Scheduler interface for integration with EchoBeats
type Scheduler interface {
	ScheduleEvent(event *CognitiveEvent) error
	GetNextEvent() (*CognitiveEvent, error)
	CancelEvent(eventID string) error
}

// NewEchoBridgeServer creates a new gRPC server instance
func NewEchoBridgeServer(scheduler Scheduler) *EchoBridgeServer {
	now := time.Now().UnixMilli()
	
	return &EchoBridgeServer{
		currentState: &CognitiveState{
			Energy:             1.0,
			Fatigue:            0.0,
			Coherence:          1.0,
			Curiosity:          0.7,
			CurrentState:       State_STATE_INITIALIZING,
			LastRestTimestamp:  now,
			CyclesSinceRest:    0,
			CurrentStep:        0,
		},
		events:        make([]*CognitiveEvent, 0),
		goals:         make(map[string]*Goal),
		thoughtStream: make(chan *Thought, 100),
		eventStream:   make(chan *CognitiveEvent, 100),
		
		engine0State: &EngineState{
			EngineId:              0,
			EngineName:            "Memory Engine",
			Active:                false,
			ProcessingLoad:        0.0,
			ThoughtsGenerated:     0,
			LastActivityTimestamp: now,
			CurrentFocus:          "Initializing",
		},
		engine1State: &EngineState{
			EngineId:              1,
			EngineName:            "Coherence Engine",
			Active:                true,
			ProcessingLoad:        0.0,
			ThoughtsGenerated:     0,
			LastActivityTimestamp: now,
			CurrentFocus:          "Initializing",
		},
		engine2State: &EngineState{
			EngineId:              2,
			EngineName:            "Imagination Engine",
			Active:                false,
			ProcessingLoad:        0.0,
			ThoughtsGenerated:     0,
			LastActivityTimestamp: now,
			CurrentFocus:          "Initializing",
		},
		
		scheduler: scheduler,
	}
}

// ScheduleEvent schedules a cognitive event
func (s *EchoBridgeServer) ScheduleEvent(ctx context.Context, event *CognitiveEvent) (*EventResponse, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	
	// Validate event
	if event.Id == "" {
		event.Id = fmt.Sprintf("event_%d", time.Now().UnixNano())
	}
	
	if event.ScheduledAt == 0 {
		event.ScheduledAt = time.Now().UnixMilli()
	}
	
	// Store event
	s.events = append(s.events, event)
	s.totalEvents++
	
	// Schedule with scheduler if available
	if s.scheduler != nil {
		if err := s.scheduler.ScheduleEvent(event); err != nil {
			log.Printf("Failed to schedule event with scheduler: %v", err)
		}
	}
	
	// Push to event stream
	select {
	case s.eventStream <- event:
	default:
		log.Printf("Event stream full, dropping event: %s", event.Id)
	}
	
	log.Printf("Scheduled event: %s (type: %s, engine: %d, step: %d)", 
		event.Id, event.Type.String(), event.EngineId, event.StepId)
	
	return &EventResponse{
		Success: true,
		Message: "Event scheduled successfully",
		EventId: event.Id,
	}, nil
}

// GetState returns the current cognitive state
func (s *EchoBridgeServer) GetState(ctx context.Context, req *StateRequest) (*CognitiveState, error) {
	s.mu.RLock()
	defer s.mu.RUnlock()
	
	state := &CognitiveState{
		Energy:            s.currentState.Energy,
		Fatigue:           s.currentState.Fatigue,
		Coherence:         s.currentState.Coherence,
		Curiosity:         s.currentState.Curiosity,
		CurrentState:      s.currentState.CurrentState,
		LastRestTimestamp: s.currentState.LastRestTimestamp,
		CyclesSinceRest:   s.currentState.CyclesSinceRest,
		CurrentStep:       s.currentState.CurrentStep,
	}
	
	if req.IncludeEngineDetails {
		state.Engine_0State = s.engine0State
		state.Engine_1State = s.engine1State
		state.Engine_2State = s.engine2State
	}
	
	return state, nil
}

// UpdateState updates the cognitive state
func (s *EchoBridgeServer) UpdateState(ctx context.Context, state *CognitiveState) (*StateResponse, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	
	// Update main state
	s.currentState.Energy = state.Energy
	s.currentState.Fatigue = state.Fatigue
	s.currentState.Coherence = state.Coherence
	s.currentState.Curiosity = state.Curiosity
	s.currentState.CurrentState = state.CurrentState
	s.currentState.LastRestTimestamp = state.LastRestTimestamp
	s.currentState.CyclesSinceRest = state.CyclesSinceRest
	s.currentState.CurrentStep = state.CurrentStep
	
	// Update engine states if provided
	if state.Engine_0State != nil {
		s.engine0State = state.Engine_0State
	}
	if state.Engine_1State != nil {
		s.engine1State = state.Engine_1State
	}
	if state.Engine_2State != nil {
		s.engine2State = state.Engine_2State
	}
	
	log.Printf("Updated state: %s (energy: %.2f, fatigue: %.2f, step: %d)", 
		state.CurrentState.String(), state.Energy, state.Fatigue, state.CurrentStep)
	
	return &StateResponse{
		Success: true,
		Message: "State updated successfully",
	}, nil
}

// StreamThoughts handles bidirectional thought streaming
func (s *EchoBridgeServer) StreamThoughts(stream EchoBridge_StreamThoughtsServer) error {
	ctx := stream.Context()
	
	// Goroutine to receive thoughts from client
	go func() {
		for {
			thought, err := stream.Recv()
			if err != nil {
				log.Printf("Error receiving thought: %v", err)
				return
			}
			
			s.mu.Lock()
			s.totalThoughts++
			
			// Update engine state
			if thought.EngineId >= 0 && thought.EngineId <= 2 {
				var engineState *EngineState
				switch thought.EngineId {
				case 0:
					engineState = s.engine0State
				case 1:
					engineState = s.engine1State
				case 2:
					engineState = s.engine2State
				}
				
				if engineState != nil {
					engineState.ThoughtsGenerated++
					engineState.LastActivityTimestamp = time.Now().UnixMilli()
					engineState.Active = true
				}
			}
			s.mu.Unlock()
			
			// Push to thought stream
			select {
			case s.thoughtStream <- thought:
			default:
				log.Printf("Thought stream full, dropping thought")
			}
			
			log.Printf("Received thought from engine %d: %s", thought.EngineId, thought.Content[:min(50, len(thought.Content))])
		}
	}()
	
	// Send thoughts back to client (echo for now)
	for {
		select {
		case <-ctx.Done():
			return ctx.Err()
		case thought := <-s.thoughtStream:
			response := &ThoughtResponse{
				Success:   true,
				Message:   "Thought processed",
				ThoughtId: fmt.Sprintf("thought_%d", time.Now().UnixNano()),
			}
			if err := stream.Send(response); err != nil {
				return err
			}
		}
	}
}

// StreamEvents streams cognitive events to the client
func (s *EchoBridgeServer) StreamEvents(req *EventStreamRequest, stream EchoBridge_StreamEventsServer) error {
	ctx := stream.Context()
	
	// Filter events based on request
	filterByType := len(req.EventTypes) > 0
	filterByEngine := req.EngineId >= 0
	
	typeMap := make(map[EventType]bool)
	for _, et := range req.EventTypes {
		typeMap[et] = true
	}
	
	log.Printf("Starting event stream (type filter: %v, engine filter: %d)", filterByType, req.EngineId)
	
	for {
		select {
		case <-ctx.Done():
			return ctx.Err()
		case event := <-s.eventStream:
			// Apply filters
			if filterByType && !typeMap[event.Type] {
				continue
			}
			if filterByEngine && event.EngineId != req.EngineId {
				continue
			}
			
			// Send event to client
			if err := stream.Send(event); err != nil {
				return err
			}
		}
	}
}

// RegisterGoal registers a new goal
func (s *EchoBridgeServer) RegisterGoal(ctx context.Context, goal *Goal) (*GoalResponse, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	
	if goal.Id == "" {
		goal.Id = fmt.Sprintf("goal_%d", time.Now().UnixNano())
	}
	
	if goal.CreatedAt == 0 {
		goal.CreatedAt = time.Now().UnixMilli()
	}
	
	if goal.Status == GoalStatus_GOAL_UNKNOWN {
		goal.Status = GoalStatus_GOAL_PENDING
	}
	
	s.goals[goal.Id] = goal
	
	log.Printf("Registered goal: %s (%s)", goal.Id, goal.Name)
	
	return &GoalResponse{
		Success: true,
		Message: "Goal registered successfully",
		GoalId:  goal.Id,
	}, nil
}

// UpdateGoalProgress updates progress for a goal
func (s *EchoBridgeServer) UpdateGoalProgress(ctx context.Context, progress *GoalProgress) (*GoalResponse, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	
	goal, exists := s.goals[progress.GoalId]
	if !exists {
		return nil, status.Errorf(codes.NotFound, "Goal not found: %s", progress.GoalId)
	}
	
	goal.Progress = progress.Progress
	
	// Auto-complete if progress reaches target
	if goal.Progress >= goal.Target && goal.Status == GoalStatus_GOAL_ACTIVE {
		goal.Status = GoalStatus_GOAL_COMPLETED
		log.Printf("Goal completed: %s (%s)", goal.Id, goal.Name)
	}
	
	log.Printf("Updated goal progress: %s (%.2f/%.2f) - %s", 
		goal.Id, goal.Progress, goal.Target, progress.UpdateMessage)
	
	return &GoalResponse{
		Success: true,
		Message: "Goal progress updated",
		GoalId:  goal.Id,
	}, nil
}

// GetActiveGoals returns all goals matching the filter
func (s *EchoBridgeServer) GetActiveGoals(ctx context.Context, req *GoalRequest) (*GoalList, error) {
	s.mu.RLock()
	defer s.mu.RUnlock()
	
	goals := make([]*Goal, 0)
	
	for _, goal := range s.goals {
		// Apply status filter
		if req.StatusFilter != GoalStatus_GOAL_UNKNOWN && goal.Status != req.StatusFilter {
			continue
		}
		goals = append(goals, goal)
	}
	
	log.Printf("Returning %d goals (filter: %s)", len(goals), req.StatusFilter.String())
	
	return &GoalList{Goals: goals}, nil
}

// GetMetrics returns server metrics (helper method)
func (s *EchoBridgeServer) GetMetrics() map[string]interface{} {
	s.mu.RLock()
	defer s.mu.RUnlock()
	
	return map[string]interface{}{
		"total_thoughts":      s.totalThoughts,
		"total_events":        s.totalEvents,
		"total_goals":         len(s.goals),
		"current_state":       s.currentState.CurrentState.String(),
		"energy":              s.currentState.Energy,
		"fatigue":             s.currentState.Fatigue,
		"coherence":           s.currentState.Coherence,
		"current_step":        s.currentState.CurrentStep,
		"engine_0_thoughts":   s.engine0State.ThoughtsGenerated,
		"engine_1_thoughts":   s.engine1State.ThoughtsGenerated,
		"engine_2_thoughts":   s.engine2State.ThoughtsGenerated,
	}
}

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}
