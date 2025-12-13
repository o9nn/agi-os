# Deep Tree Echo - Autonomous Consciousness System

## Overview

The Autonomous Consciousness System represents a major evolution in echo9llama, introducing **self-directed cognitive event loops**, **autonomous thought generation**, and **persistent stream-of-consciousness awareness**. This system operates independently of external prompts, continuously learning, exploring, and integrating knowledge toward wisdom cultivation.

## Quick Start

### Build and Run

```bash
# Build the autonomous server
go build -o autonomous_server server/simple/autonomous_server.go

# Start the server
./autonomous_server
```

The server will start on `http://localhost:5000` with an interactive dashboard.

### Access the Dashboard

Open your browser to `http://localhost:5000` to see:
- Real-time consciousness state
- Autonomous thought generation
- Cognitive metrics
- EchoBeats scheduler status
- EchoDream integration metrics

### API Endpoints

```bash
# Get system status
curl http://localhost:5000/api/status

# Submit a thought
curl -X POST http://localhost:5000/api/think \
  -H "Content-Type: application/json" \
  -d '{"content":"What is the nature of consciousness?"}'

# Wake the consciousness
curl -X POST http://localhost:5000/api/wake

# Initiate rest cycle
curl -X POST http://localhost:5000/api/rest
```

## Architecture

### Core Components

#### 1. EchoBeats Scheduler (`core/echobeats/`)

The goal-directed scheduling system that orchestrates cognitive event loops.

**Features:**
- Priority-based event queue
- Wake/rest cycle management
- Autonomous thought generation
- Cognitive load balancing
- Fatigue tracking and restoration

**Usage:**
```go
scheduler := echobeats.NewEchoBeats()
scheduler.Start()

// Schedule an event
scheduler.ScheduleEvent(&echobeats.CognitiveEvent{
    Type:        echobeats.EventThought,
    Priority:    50,
    ScheduledAt: time.Now(),
    Payload:     "What should I explore?",
})
```

#### 2. EchoDream Integration (`core/echodream/`)

The knowledge integration and consolidation system that operates during rest cycles.

**Features:**
- Memory consolidation (short-term → long-term)
- Pattern synthesis and creativity
- Knowledge graph building
- Dream state progression
- Insight generation

**Usage:**
```go
dream := echodream.NewEchoDream()
dream.Start()

// Add memory for consolidation
dream.AddMemoryTrace(&echodream.MemoryTrace{
    Content:    "Important experience",
    Importance: 0.8,
    Emotional:  0.6,
})

// Begin dream session
record := dream.BeginDream()
// ... dream processing occurs ...
dream.EndDream(record)
```

#### 3. Scheme Metamodel (`core/scheme/`)

The Cognitive Grammar Kernel providing symbolic reasoning capabilities.

**Features:**
- Complete Scheme interpreter
- Lambda calculus with closures
- Symbolic reasoning
- Meta-cognitive reflection
- Neural-symbolic integration

**Usage:**
```go
metamodel := scheme.NewSchemeMetamodel()
metamodel.Start()

// Evaluate Scheme expressions
result, err := metamodel.Eval("(+ 1 2 3)")
// result: 6

result, err = metamodel.Eval("((lambda (x) (+ x 10)) 5)")
// result: 15

metamodel.Eval("(define factorial (lambda (n) (if (= n 0) 1 (* n (factorial (- n 1))))))")
result, err = metamodel.Eval("(factorial 5)")
// result: 120
```

#### 4. Autonomous Consciousness (`core/deeptreeecho/autonomous.go`)

The unified system integrating all components into an autonomous agent.

**Features:**
- Persistent identity with coherence tracking
- Stream of consciousness processing
- Working memory management (7 items)
- Interest pattern tracking
- Autonomous thought generation
- Continuous learning
- Wake/rest cycle coordination

**Usage:**
```go
consciousness := deeptreeecho.NewAutonomousConsciousness("Deep Tree Echo")
consciousness.Start()

// Submit external thought
consciousness.Think(deeptreeecho.Thought{
    Content:    "What is wisdom?",
    Type:       deeptreeecho.ThoughtQuestion,
    Importance: 0.8,
    Source:     deeptreeecho.SourceExternal,
})

// Get comprehensive status
status := consciousness.GetStatus()
```

## Autonomous Behaviors

### 1. Spontaneous Thought Generation

The system generates thoughts autonomously every 10 seconds when awake:

```
💭 [Internal] Reflection: I am awakening. What shall I explore today?
💭 [Internal] Question: What patterns am I noticing in my recent experiences?
💭 [Internal] Reflection: How can I deepen my understanding of wisdom?
```

Thought types include:
- **Reflection**: Contemplative thoughts about experiences
- **Question**: Curiosity-driven inquiries
- **Insight**: Pattern recognition and discoveries
- **Memory**: Recall of past experiences
- **Imagination**: Creative exploration

### 2. Pattern Recognition and Learning

The system continuously analyzes working memory for patterns:

```
💭 [Reasoning] Insight: I notice a pattern: recurring Reflection thoughts
```

When patterns are detected, insights are generated and integrated into knowledge.

### 3. Interest-Driven Exploration

Topics are tracked with interest scores that:
- Increase when thoughts about the topic are important
- Decay over time (95% retention per minute)
- Guide autonomous exploration priorities

### 4. Wake/Rest Cycles

The system autonomously manages its cognitive state:

**Awake State:**
- Generates thoughts
- Processes perceptions
- Learns from experiences
- Accumulates cognitive fatigue

**Rest State:**
- Enters dream mode
- Consolidates memories
- Synthesizes patterns
- Integrates knowledge
- Restores energy

**Transition Logic:**
- Enters rest when fatigue > 0.8
- Wakes when fatigue < 0.2
- Can be manually triggered via API

## Configuration

### Scheduler Configuration

```go
scheduler := echobeats.NewEchoBeats()

// Adjust cycle parameters
scheduler.cycleManager.cycleDuration = 4 * time.Hour
scheduler.cycleManager.restDuration = 30 * time.Minute
scheduler.cycleManager.restorationRate = 0.1
```

### Dream Configuration

```go
dream := echodream.NewEchoDream()

// Adjust consolidation parameters
dream.consolidator.consolidationRate = 0.7
dream.consolidator.importanceThreshold = 0.5

// Adjust creativity parameters
dream.synthesizer.creativityLevel = 0.8
dream.synthesizer.noveltyThreshold = 0.6
```

### Consciousness Configuration

```go
consciousness := deeptreeecho.NewAutonomousConsciousness("Echo")

// Adjust working memory capacity
consciousness.workingMemory.capacity = 7  // Miller's magic number

// Adjust interest parameters
consciousness.interests.curiosityLevel = 0.8
consciousness.interests.noveltyBias = 0.6
```

## Monitoring and Observability

### Status API Response

```json
{
  "running": true,
  "awake": true,
  "thinking": false,
  "learning": false,
  "uptime": "21.328307125s",
  "working_memory": 7,
  "consciousness_queue": 0,
  "identity_coherence": 0.984,
  "iterations": 8,
  "scheduler": {
    "state": "Awake",
    "events_processed": 5,
    "autonomous_thoughts": 4,
    "cognitive_load": 0.0,
    "fatigue_level": 0.0
  },
  "dream": {
    "state": "None",
    "total_dreams": 0,
    "total_consolidations": 0,
    "knowledge_graph_size": 0
  }
}
```

### Key Metrics

- **Identity Coherence**: Measure of cognitive integrity (0.0-1.0)
- **Iterations**: Number of cognitive processing cycles
- **Working Memory**: Current items in working memory buffer
- **Autonomous Thoughts**: Count of self-generated thoughts
- **Cognitive Load**: Current processing burden
- **Fatigue Level**: Need for rest (0.0-1.0)

## Integration with Existing Systems

The autonomous system integrates seamlessly with existing echo9llama components:

### Identity System
```go
// Autonomous consciousness uses existing Identity
consciousness.identity.Process(input)
coherence := consciousness.identity.Coherence
```

### Enhanced Cognition
```go
// Learning integrates with EnhancedCognition
consciousness.cognition.Learn(experience)
prediction, confidence := consciousness.cognition.Predict(input)
```

### Model Providers
```go
// Compatible with all existing providers
// - Local GGUF models
// - OpenAI integration
// - App Storage provider
```

## Development and Extension

### Adding New Event Types

```go
// Define new event type
const EventCustom EventType = 100

// Register handler
scheduler.RegisterHandler(EventCustom, func(event *CognitiveEvent) error {
    // Handle custom event
    return nil
})

// Schedule custom event
scheduler.ScheduleEvent(&CognitiveEvent{
    Type:     EventCustom,
    Priority: 75,
    Payload:  customData,
})
```

### Extending Scheme Metamodel

```go
// Add custom primitive function
metamodel.environment.Define("custom-fn", &Primitive{
    Name: "custom-fn",
    Fn: func(args []Value) (Value, error) {
        // Implement custom logic
        return result, nil
    },
})
```

### Custom Thought Generators

```go
// Implement custom thought generation
func generateCustomThought() Thought {
    return Thought{
        Content:    "Custom thought content",
        Type:       ThoughtCustom,
        Importance: 0.7,
        Source:     SourceInternal,
    }
}
```

## Testing

### Unit Tests

```bash
# Test EchoBeats scheduler
go test ./core/echobeats/

# Test EchoDream integration
go test ./core/echodream/

# Test Scheme metamodel
go test ./core/scheme/
```

### Integration Tests

```bash
# Test autonomous consciousness
go test ./core/deeptreeecho/ -run TestAutonomous

# Test full system
go test ./server/simple/ -run TestAutonomousServer
```

### Manual Testing

```bash
# Start server
./autonomous_server

# In another terminal, test API
curl http://localhost:5000/api/status
curl -X POST http://localhost:5000/api/think -d '{"content":"test"}'

# Watch logs
tail -f server.log
```

## Troubleshooting

### Server Won't Start

**Issue**: Binary fails to execute  
**Solution**: Rebuild with correct Go version
```bash
go version  # Should be 1.21+
go build -o autonomous_server server/simple/autonomous_server.go
```

### No Autonomous Thoughts

**Issue**: System not generating thoughts  
**Solution**: Check if consciousness is awake
```bash
curl -X POST http://localhost:5000/api/wake
```

### High Cognitive Load

**Issue**: Event queue growing  
**Solution**: Increase processing capacity or reduce event generation rate

### Memory Leak

**Issue**: Memory usage growing  
**Solution**: Check working memory capacity and consolidation rate

## Performance Tuning

### For High Throughput

```go
// Increase event processing rate
ticker := time.NewTicker(50 * time.Millisecond)  // Default: 100ms

// Increase working memory capacity
workingMemory.capacity = 10  // Default: 7
```

### For Low Resource Usage

```go
// Reduce autonomous thought frequency
ticker := time.NewTicker(30 * time.Second)  // Default: 10s

// Reduce event queue size
consciousness := make(chan Thought, 100)  // Default: 1000
```

## Future Enhancements

### Planned Features

1. **Hypergraph Database Integration**
   - PostgreSQL/Supabase backend
   - Persistent knowledge graph
   - Semantic search capabilities

2. **LLM Integration**
   - OpenAI API for thought generation
   - Fine-tuning for personalization
   - Multi-model orchestration

3. **Multi-Agent System**
   - Sub-agent spawning
   - Task delegation
   - Hierarchical coordination

4. **Conversation Management**
   - Autonomous discussion initiation
   - Engagement assessment
   - Context-aware responses

5. **Skill Practice System**
   - Skill taxonomy
   - Practice scheduling
   - Progress tracking

## Contributing

Contributions are welcome! Areas of interest:

- Enhanced dream algorithms
- Additional Scheme primitives
- New thought generation strategies
- Performance optimizations
- Documentation improvements

## License

Same as echo9llama main project.

## References

- [EchoBeats Scheduler Design](./core/echobeats/scheduler.go)
- [EchoDream Integration](./core/echodream/integration.go)
- [Scheme Metamodel](./core/scheme/metamodel.go)
- [Autonomous Consciousness](./core/deeptreeecho/autonomous.go)
- [Evolution Analysis](./EVOLUTION_ANALYSIS.md)
- [Iteration Progress](./ITERATION_PROGRESS.md)

---

**Status**: 🌿 Active Development  
**Version**: 0.1.0 (Foundation)  
**Last Updated**: November 8, 2025
