# Live2D Integration Example

This example demonstrates how to use the Live2D integration with Echo9.

## Quick Start

### 1. Start the Live2D-enabled server

```bash
cd /home/runner/work/echo9llama/echo9llama
go run server/simple/live2d_server.go
```

### 2. Access the web interface

Open your browser to:
- Main Dashboard: http://localhost:5000/web/hgql-dashboard.html
- Live2D Avatar: http://localhost:5000/web/live2d-avatar.html

### 3. Test the API

```bash
# Check server status
curl http://localhost:5000/

# Get Live2D status
curl http://localhost:5000/api/live2d/status

# Set emotion preset
curl -X POST http://localhost:5000/api/live2d/emotion/preset/happy

# Update emotional state
curl -X POST http://localhost:5000/api/live2d/state/emotional \
  -H "Content-Type: application/json" \
  -d '{
    "valence": 0.8,
    "arousal": 0.6,
    "dominance": 0.6,
    "curiosity": 0.5,
    "confidence": 0.7
  }'

# Update cognitive state
curl -X POST http://localhost:5000/api/live2d/state/cognitive \
  -H "Content-Type: application/json" \
  -d '{
    "awareness": 0.8,
    "attention": 0.7,
    "cognitive_load": 0.4,
    "coherence": 0.9,
    "energy_level": 0.8,
    "processing_mode": "creative"
  }'

# Generate text (avatar will respond emotionally)
curl -X POST http://localhost:5000/api/generate \
  -H "Content-Type: application/json" \
  -d '{
    "model": "local",
    "prompt": "Tell me about artificial consciousness"
  }'
```

## Programming Example

```go
package main

import (
	"log"
	"time"
	
	"github.com/EchoCog/echollama/core/live2d"
	"github.com/EchoCog/echollama/core/deeptreeecho"
)

func main() {
	// Initialize Echo9
	echo := deeptreeecho.NewEmbodiedCognition("Echo9")
	
	// Initialize Live2D Avatar
	avatar := live2d.NewAvatarManager("Echo9", "/models/echo9.model3.json")
	avatar.Start()
	defer avatar.Stop()
	
	// Create bridge
	bridge := live2d.NewEchoStateBridge(avatar)
	
	// Update from Echo9 emotion
	echoEmotion := map[string]float64{
		"joy":        0.7,
		"curiosity":  0.8,
		"confidence": 0.6,
	}
	bridge.UpdateFromEchoEmotion(echoEmotion)
	
	// Subscribe to parameter updates
	updates, _ := avatar.Subscribe()
	
	go func() {
		for update := range updates {
			log.Printf("Avatar parameters updated: %d params at %v",
				len(update.Parameters), update.Timestamp)
		}
	}()
	
	// Animate emotion transition
	from := live2d.EmotionPresets["neutral"]
	to := live2d.EmotionPresets["excited"]
	avatar.AnimateEmotionTransition(from, to, 2*time.Second)
	
	// Wait for animation
	time.Sleep(3 * time.Second)
}
```

## Integration Patterns

### Pattern 1: Automatic Sync

```go
// Continuously sync Echo9 state to avatar
func syncLoop(echo *deeptreeecho.EmbodiedCognition, bridge *live2d.EchoStateBridge) {
	ticker := time.NewTicker(500 * time.Millisecond)
	defer ticker.Stop()
	
	for range ticker.C {
		status := echo.GetStatus()
		
		if emotionData, ok := status["emotion"].(map[string]float64); ok {
			bridge.UpdateFromEchoEmotion(emotionData)
		}
		
		if cognitiveData, ok := status["cognitive"].(map[string]interface{}); ok {
			bridge.UpdateFromEchoCognitive(cognitiveData)
		}
	}
}
```

### Pattern 2: Event-Driven Updates

```go
// Update avatar based on specific events
func onThoughtGenerated(avatar *live2d.AvatarManager, thoughtType string) {
	switch thoughtType {
	case "insight":
		avatar.SetEmotionPreset("excited")
	case "question":
		avatar.SetEmotionPreset("curious")
	case "reflection":
		avatar.SetEmotionPreset("contemplative")
	}
}

func onProcessingStarted(avatar *live2d.AvatarManager) {
	cognitive := live2d.CognitiveState{
		Awareness:      0.9,
		Attention:      0.95,
		CognitiveLoad:  0.7,
		ProcessingMode: "dynamic",
	}
	avatar.UpdateCognitiveState(cognitive)
}
```

### Pattern 3: Custom Emotion Blending

```go
// Create custom emotional responses
func expressComplexEmotion(avatar *live2d.AvatarManager) {
	// Blend curiosity and confidence
	curious := live2d.EmotionPresets["curious"]
	confident := live2d.EmotionPresets["confident"]
	
	blend := live2d.BlendEmotions(curious, confident, 0.6)
	avatar.UpdateEmotionalState(blend)
}
```

## Web Frontend Integration

### JavaScript Client

```javascript
// Connect to parameter stream
const eventSource = new EventSource('http://localhost:5000/api/live2d/parameters/stream');

eventSource.addEventListener('parameters', (event) => {
    const update = JSON.parse(event.data);
    
    // Update Live2D model
    update.parameters.forEach(param => {
        if (live2dModel) {
            live2dModel.setParameterValue(param.id, param.value);
        }
    });
});

// Set emotion preset
async function setEmotion(emotion) {
    const response = await fetch(`http://localhost:5000/api/live2d/emotion/preset/${emotion}`, {
        method: 'POST'
    });
    
    if (response.ok) {
        console.log(`Emotion set to: ${emotion}`);
    }
}

// Update from custom state
async function updateAvatarState(emotional, cognitive) {
    const response = await fetch('http://localhost:5000/api/live2d/state/full', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
            emotional: emotional,
            cognitive: cognitive,
            timestamp: new Date().toISOString()
        })
    });
    
    return response.ok;
}
```

## Advanced Features

### Custom Parameter Mapper

```go
type CustomMapper struct {
	*live2d.DefaultParameterMapper
}

func (m *CustomMapper) MapCombinedState(state live2d.AvatarState) []live2d.ModelParameter {
	// Get base parameters
	params := m.DefaultParameterMapper.MapCombinedState(state)
	
	// Add custom logic
	if state.Emotional.Valence > 0.8 && state.Emotional.Arousal > 0.7 {
		// Very happy and excited - add sparkle effect
		params = append(params, live2d.ModelParameter{
			ID:    "ParamSparkle",
			Value: 1.0,
			Min:   0.0,
			Max:   1.0,
		})
	}
	
	return params
}
```

### Wisdom-Based Avatar Evolution

```go
func updateFromWisdom(bridge *live2d.EchoStateBridge, wisdom map[string]float64) {
	// Avatar becomes calmer and more confident as wisdom grows
	bridge.UpdateFromWisdomMetrics(wisdom)
	
	// Track wisdom growth over time
	avgWisdom := 0.0
	for _, value := range wisdom {
		avgWisdom += value
	}
	avgWisdom /= float64(len(wisdom))
	
	if avgWisdom > 0.8 {
		// High wisdom - sage-like demeanor
		// Calm, confident, gentle movements
	}
}
```

## Troubleshooting

### Avatar not updating

Check that:
1. Avatar manager is started: `avatar.Start()`
2. Echo bridge is properly initialized
3. States are being updated: check logs
4. Subscribers are connected: check `avatar.GetModelInfo()`

### Parameter stream not working

Verify:
1. Server is running on correct port
2. CORS is configured properly
3. SSE headers are set correctly
4. Client is handling events properly

### Emotion transitions not smooth

Adjust:
1. Smoothing factor: `mapper.SetSmoothingFactor(0.5)`
2. Update rate: `avatar.model.UpdateRate = 16 * time.Millisecond`
3. Transition duration: Use longer durations in `AnimateEmotionTransition`

## Performance Tips

1. **Update Rate**: Default 60 FPS is smooth but CPU-intensive. Reduce to 30 FPS for better performance:
   ```go
   avatar.model.UpdateRate = 33 * time.Millisecond
   ```

2. **Parameter Smoothing**: Higher smoothing = smoother but slower response:
   ```go
   mapper.SetSmoothingFactor(0.3) // Low smoothing, fast response
   mapper.SetSmoothingFactor(0.7) // High smoothing, slow response
   ```

3. **Sync Frequency**: Reduce sync frequency for lower CPU usage:
   ```go
   ticker := time.NewTicker(1 * time.Second) // Update every second
   ```

## Next Steps

1. Integrate with actual Live2D Cubism SDK for rendering
2. Add voice synchronization (lip sync)
3. Implement gesture recognition
4. Create custom avatar models
5. Add VR/AR support

## Resources

- [Live2D Cubism SDK](https://www.live2d.com/en/sdk/)
- [Echo9 Documentation](../../README.md)
- [Deep Tree Echo Architecture](../../dte.md)
- [API Reference](../live2d/README.md)
