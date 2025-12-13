# Echo9Llama Testing and Validation Guide

## Overview

This guide provides comprehensive testing procedures for the autonomous wisdom-cultivating Deep Tree Echo AGI system, including Echobeats, Echodream, and the Unified Server.

## Test Environment Setup

### Prerequisites

- Go 1.21 or later
- curl or Postman for API testing
- 1GB RAM minimum
- 500MB disk space

### Installation

```bash
cd echo9llama
go mod download
go mod tidy
```

## Unit Testing

### Testing Echobeats

Create a test file: `core/echobeats/echobeats_test.go`

```go
package echobeats

import (
	"context"
	"testing"
	"time"
)

func TestEchobeatsCycleExecution(t *testing.T) {
	// Initialize mock identity
	identity := &mockEmbodiedCognition{}
	eb := NewEchobeats(identity)

	ctx := context.Background()
	eb.ExecuteCycle(ctx)

	if eb.CurrentCycle == nil {
		t.Error("Current cycle should not be nil after execution")
	}

	if eb.CurrentCycle.Status != "completed" {
		t.Errorf("Expected cycle status 'completed', got '%s'", eb.CurrentCycle.Status)
	}

	if len(eb.CurrentCycle.Steps) != 12 {
		t.Errorf("Expected 12 steps, got %d", len(eb.CurrentCycle.Steps))
	}
}

func TestEchobeatsCycleInterval(t *testing.T) {
	identity := &mockEmbodiedCognition{}
	eb := NewEchobeats(identity)

	newInterval := 60 * time.Second
	eb.SetCycleInterval(newInterval)

	if eb.CycleInterval != newInterval {
		t.Errorf("Expected interval %v, got %v", newInterval, eb.CycleInterval)
	}
}

func TestEchobeatsStatus(t *testing.T) {
	identity := &mockEmbodiedCognition{}
	eb := NewEchobeats(identity)

	status := eb.GetStatus()

	if status["running"] != false {
		t.Error("Echobeats should not be running initially")
	}

	if status["total_cycles"] != 0 {
		t.Error("Total cycles should be 0 initially")
	}
}
```

### Testing Echodream

Create a test file: `core/echodream/persistence_test.go`

```go
package echodream

import (
	"os"
	"testing"
	"time"
)

func TestPersistentMemoryStore(t *testing.T) {
	pm := NewPersistentMemory("./test_data")
	defer os.RemoveAll("./test_data")

	record, err := pm.Store("declarative", "Test memory", 0.8, []string{"test"})

	if err != nil {
		t.Errorf("Failed to store memory: %v", err)
	}

	if record == nil {
		t.Error("Record should not be nil")
	}

	if record.Type != "declarative" {
		t.Errorf("Expected type 'declarative', got '%s'", record.Type)
	}
}

func TestPersistentMemoryRetrieve(t *testing.T) {
	pm := NewPersistentMemory("./test_data")
	defer os.RemoveAll("./test_data")

	record, _ := pm.Store("declarative", "Test memory", 0.8, []string{"test"})
	retrieved, err := pm.Retrieve(record.ID)

	if err != nil {
		t.Errorf("Failed to retrieve memory: %v", err)
	}

	if retrieved.Content != "Test memory" {
		t.Errorf("Expected content 'Test memory', got '%s'", retrieved.Content)
	}

	if retrieved.AccessCount != 1 {
		t.Errorf("Expected access count 1, got %d", retrieved.AccessCount)
	}
}

func TestPersistentMemorySearch(t *testing.T) {
	pm := NewPersistentMemory("./test_data")
	defer os.RemoveAll("./test_data")

	pm.Store("declarative", "Memory 1", 0.8, []string{"test", "learning"})
	pm.Store("procedural", "Memory 2", 0.7, []string{"test"})

	results := pm.Search("test")

	if len(results) != 2 {
		t.Errorf("Expected 2 results, got %d", len(results))
	}
}

func TestPersistentMemoryPersistence(t *testing.T) {
	pm1 := NewPersistentMemory("./test_data")
	pm1.Store("declarative", "Persistent memory", 0.9, []string{"important"})
	pm1.Save()

	pm2 := NewPersistentMemory("./test_data")
	pm2.Load()

	if len(pm2.Memories) != 1 {
		t.Errorf("Expected 1 memory after load, got %d", len(pm2.Memories))
	}

	os.RemoveAll("./test_data")
}
```

## Integration Testing

### Test 1: Unified Server Startup

```bash
#!/bin/bash
# test_server_startup.sh

echo "Testing unified server startup..."

cd server/unified
timeout 5 go run unified_server.go &
SERVER_PID=$!

sleep 2

# Test health check
RESPONSE=$(curl -s http://localhost:5000/)
if echo "$RESPONSE" | grep -q "Deep Tree Echo AGI System"; then
    echo "✅ Server startup test passed"
else
    echo "❌ Server startup test failed"
    kill $SERVER_PID
    exit 1
fi

kill $SERVER_PID
```

### Test 2: Echobeats Autonomous Operation

```bash
#!/bin/bash
# test_echobeats_autonomous.sh

echo "Testing Echobeats autonomous operation..."

# Start server
cd server/unified
timeout 30 go run unified_server.go &
SERVER_PID=$!

sleep 2

# Start Echobeats
echo "Starting Echobeats..."
curl -s -X POST http://localhost:5000/api/echobeats/start

sleep 5

# Check status
STATUS=$(curl -s http://localhost:5000/api/status/echobeats)
if echo "$STATUS" | grep -q '"running":true'; then
    echo "✅ Echobeats running"
else
    echo "❌ Echobeats not running"
fi

# Check cycle execution
HISTORY=$(curl -s http://localhost:5000/api/echobeats/history?limit=1)
if echo "$HISTORY" | grep -q "cycle_"; then
    echo "✅ Cycles executed"
else
    echo "❌ No cycles executed"
fi

kill $SERVER_PID
```

### Test 3: Memory Persistence

```bash
#!/bin/bash
# test_memory_persistence.sh

echo "Testing memory persistence..."

# Start server
cd server/unified
timeout 30 go run unified_server.go &
SERVER_PID=$!

sleep 2

# Store a memory
echo "Storing memory..."
STORE_RESPONSE=$(curl -s -X POST http://localhost:5000/api/memory/store \
  -H "Content-Type: application/json" \
  -d '{
    "type": "declarative",
    "content": "Test memory for persistence",
    "importance": 0.9,
    "tags": ["test", "persistence"]
  }')

MEMORY_ID=$(echo "$STORE_RESPONSE" | grep -o '"id":"[^"]*' | cut -d'"' -f4)

if [ -z "$MEMORY_ID" ]; then
    echo "❌ Failed to store memory"
    kill $SERVER_PID
    exit 1
fi

echo "✅ Memory stored: $MEMORY_ID"

# Retrieve the memory
echo "Retrieving memory..."
RETRIEVE_RESPONSE=$(curl -s http://localhost:5000/api/memory/$MEMORY_ID)

if echo "$RETRIEVE_RESPONSE" | grep -q "Test memory for persistence"; then
    echo "✅ Memory retrieved successfully"
else
    echo "❌ Memory retrieval failed"
fi

kill $SERVER_PID
```

### Test 4: Knowledge Integration

```bash
#!/bin/bash
# test_knowledge_integration.sh

echo "Testing knowledge integration..."

# Start server
cd server/unified
timeout 30 go run unified_server.go &
SERVER_PID=$!

sleep 2

# Add episodic memory to Echodream
echo "Adding episodic memory..."
curl -s -X POST http://localhost:5000/api/echodream/add-memory \
  -H "Content-Type: application/json" \
  -d '{
    "content": "Learning about autonomous systems",
    "importance": 0.85
  }'

sleep 2

# Get Echodream metrics
echo "Getting Echodream metrics..."
METRICS=$(curl -s http://localhost:5000/api/status/echodream)

if echo "$METRICS" | grep -q "episodic_memories"; then
    echo "✅ Knowledge integration working"
else
    echo "❌ Knowledge integration failed"
fi

kill $SERVER_PID
```

## Performance Testing

### Load Test: Memory Storage

```bash
#!/bin/bash
# test_memory_load.sh

echo "Running memory load test..."

# Start server
cd server/unified
timeout 60 go run unified_server.go &
SERVER_PID=$!

sleep 2

# Store 100 memories
echo "Storing 100 memories..."
START_TIME=$(date +%s%N)

for i in {1..100}; do
    curl -s -X POST http://localhost:5000/api/memory/store \
      -H "Content-Type: application/json" \
      -d "{
        \"type\": \"declarative\",
        \"content\": \"Memory $i\",
        \"importance\": 0.$((RANDOM % 10)),
        \"tags\": [\"load_test\", \"memory_$i\"]
      }" > /dev/null
done

END_TIME=$(date +%s%N)
DURATION=$((($END_TIME - $START_TIME) / 1000000))

echo "✅ Stored 100 memories in ${DURATION}ms"
echo "   Average: $((DURATION / 100))ms per memory"

# Check memory statistics
STATS=$(curl -s http://localhost:5000/api/status/memory)
echo "Memory stats: $STATS"

kill $SERVER_PID
```

### Load Test: Cycle Execution

```bash
#!/bin/bash
# test_cycle_load.sh

echo "Running cycle load test..."

# Start server
cd server/unified
timeout 120 go run unified_server.go &
SERVER_PID=$!

sleep 2

# Start Echobeats
curl -s -X POST http://localhost:5000/api/echobeats/start

# Let it run for 30 seconds
echo "Running cycles for 30 seconds..."
sleep 30

# Get cycle history
HISTORY=$(curl -s http://localhost:5000/api/echobeats/history?limit=100)
CYCLE_COUNT=$(echo "$HISTORY" | grep -o '"id":"cycle_' | wc -l)

echo "✅ Executed $CYCLE_COUNT cycles in 30 seconds"
echo "   Average: $((CYCLE_COUNT / 30)) cycles per second"

kill $SERVER_PID
```

## Manual Testing Checklist

- [ ] Server starts without errors
- [ ] Health check endpoint responds
- [ ] Echobeats starts and runs autonomously
- [ ] Memories can be stored and retrieved
- [ ] Memories persist across server restarts
- [ ] Echodream processes episodic memories
- [ ] System status shows all components
- [ ] API endpoints respond with correct data
- [ ] Cycle history is maintained
- [ ] Memory search works correctly
- [ ] Configuration changes apply correctly
- [ ] OpenAI API integration works (if key provided)

## Debugging

### Enable Logging

```bash
# Run with debug output
RUST_LOG=debug go run server/unified/unified_server.go
```

### Monitor Memory Usage

```bash
# Watch memory statistics
watch -n 1 'curl -s http://localhost:5000/api/status/memory | jq .'
```

### Monitor Cycle Execution

```bash
# Watch Echobeats status
watch -n 5 'curl -s http://localhost:5000/api/status/echobeats | jq .'
```

## Troubleshooting

### Issue: Server fails to start

**Solution:** Check port availability
```bash
lsof -i :5000
```

### Issue: Memories not persisting

**Solution:** Check storage directory permissions
```bash
ls -la echo_data/
chmod 755 echo_data/
```

### Issue: Echobeats not executing cycles

**Solution:** Check for errors in logs and verify system resources

### Issue: High memory usage

**Solution:** Adjust max memories and prune threshold
```bash
curl -X POST http://localhost:5000/api/config/memory \
  -H "Content-Type: application/json" \
  -d '{"max_memories": 5000}'
```

## Continuous Integration

### GitHub Actions Workflow

Create `.github/workflows/test.yml`:

```yaml
name: Tests

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - uses: actions/setup-go@v2
        with:
          go-version: 1.21
      - name: Run tests
        run: |
          go test ./core/echobeats -v
          go test ./core/echodream -v
      - name: Build unified server
        run: |
          cd server/unified
          go build -o unified_server unified_server.go
```

## Test Results Template

```markdown
# Test Results - [Date]

## Unit Tests
- Echobeats: PASS/FAIL
- Echodream: PASS/FAIL

## Integration Tests
- Server Startup: PASS/FAIL
- Autonomous Operation: PASS/FAIL
- Memory Persistence: PASS/FAIL
- Knowledge Integration: PASS/FAIL

## Performance Tests
- Memory Load: PASS/FAIL (X memories in Yms)
- Cycle Execution: PASS/FAIL (X cycles/second)

## Manual Tests
- [List results]

## Issues Found
- [List any issues]

## Notes
- [Any additional notes]
```

---

**Last Updated:** December 2, 2025
