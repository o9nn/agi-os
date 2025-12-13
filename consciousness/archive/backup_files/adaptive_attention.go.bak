package deeptreeecho

import (
	"math"
	"sync"
	"time"
)

// AdaptiveAttentionSystem manages dynamic attention allocation
// Based on NANECHO formula: threshold = 0.5 + (cognitive_load × 0.3) - (recent_activity × 0.2)
type AdaptiveAttentionSystem struct {
	mu              sync.RWMutex
	
	// Current state
	cognitiveLoad   float64
	recentActivity  float64
	baseThreshold   float64
	
	// Parameters (from NANECHO)
	loadWeight      float64
	activityWeight  float64
	
	// History
	thresholdHistory []ThresholdRecord
	
	// Adaptive parameters
	adaptationRate  float64
	minThreshold    float64
	maxThreshold    float64
}

// ThresholdRecord tracks attention threshold over time
type ThresholdRecord struct {
	Timestamp   time.Time
	Threshold   float64
	Load        float64
	Activity    float64
	Context     string
}

// NewAdaptiveAttentionSystem creates adaptive attention manager
func NewAdaptiveAttentionSystem() *AdaptiveAttentionSystem {
	return &AdaptiveAttentionSystem{
		baseThreshold:    0.5,
		loadWeight:       0.3,
		activityWeight:   0.2,
		adaptationRate:   0.1,
		minThreshold:     0.1,
		maxThreshold:     0.9,
		thresholdHistory: make([]ThresholdRecord, 0),
		cognitiveLoad:    0.5,
		recentActivity:   0.5,
	}
}

// CalculateThreshold computes current attention threshold
// Formula from NANECHO: threshold = 0.5 + (cognitive_load × 0.3) - (recent_activity × 0.2)
func (aas *AdaptiveAttentionSystem) CalculateThreshold() float64 {
	aas.mu.RLock()
	defer aas.mu.RUnlock()
	
	threshold := aas.baseThreshold +
		(aas.cognitiveLoad * aas.loadWeight) -
		(aas.recentActivity * aas.activityWeight)
	
	// Clamp to [minThreshold, maxThreshold]
	threshold = math.Max(aas.minThreshold, math.Min(aas.maxThreshold, threshold))
	
	return threshold
}

// UpdateCognitiveLoad updates current cognitive load
func (aas *AdaptiveAttentionSystem) UpdateCognitiveLoad(load float64) {
	aas.mu.Lock()
	defer aas.mu.Unlock()
	
	// Smooth update using exponential moving average
	aas.cognitiveLoad = (aas.cognitiveLoad * (1 - aas.adaptationRate)) +
		(load * aas.adaptationRate)
	
	// Clamp to [0, 1]
	aas.cognitiveLoad = math.Max(0.0, math.Min(1.0, aas.cognitiveLoad))
}

// UpdateRecentActivity updates recent activity level
func (aas *AdaptiveAttentionSystem) UpdateRecentActivity(activity float64) {
	aas.mu.Lock()
	defer aas.mu.Unlock()
	
	// Smooth update using exponential moving average
	aas.recentActivity = (aas.recentActivity * (1 - aas.adaptationRate)) +
		(activity * aas.adaptationRate)
	
	// Clamp to [0, 1]
	aas.recentActivity = math.Max(0.0, math.Min(1.0, aas.recentActivity))
}

// RecordThreshold adds current threshold to history with context
func (aas *AdaptiveAttentionSystem) RecordThreshold(context string) {
	aas.mu.Lock()
	defer aas.mu.Unlock()
	
	threshold := aas.baseThreshold +
		(aas.cognitiveLoad * aas.loadWeight) -
		(aas.recentActivity * aas.activityWeight)
	
	threshold = math.Max(aas.minThreshold, math.Min(aas.maxThreshold, threshold))
	
	record := ThresholdRecord{
		Timestamp: time.Now(),
		Threshold: threshold,
		Load:      aas.cognitiveLoad,
		Activity:  aas.recentActivity,
		Context:   context,
	}
	
	aas.thresholdHistory = append(aas.thresholdHistory, record)
	
	// Keep last 1000 records
	if len(aas.thresholdHistory) > 1000 {
		aas.thresholdHistory = aas.thresholdHistory[1:]
	}
}

// ShouldAttendTo determines if a stimulus should receive attention
func (aas *AdaptiveAttentionSystem) ShouldAttendTo(salience float64) bool {
	threshold := aas.CalculateThreshold()
	return salience >= threshold
}

// AllocateAttention allocates attention based on salience and context
func (aas *AdaptiveAttentionSystem) AllocateAttention(
	stimuli map[string]float64,
) map[string]float64 {
	threshold := aas.CalculateThreshold()
	
	allocation := make(map[string]float64)
	totalSalience := 0.0
	
	// Filter stimuli above threshold
	for stimulus, salience := range stimuli {
		if salience >= threshold {
			allocation[stimulus] = salience
			totalSalience += salience
		}
	}
	
	// Normalize to sum to 1.0
	if totalSalience > 0 {
		for stimulus := range allocation {
			allocation[stimulus] /= totalSalience
		}
	}
	
	return allocation
}

// AdjustParameters adapts weights based on performance
func (aas *AdaptiveAttentionSystem) AdjustParameters(performance float64) {
	aas.mu.Lock()
	defer aas.mu.Unlock()
	
	// If performance is low, increase sensitivity to cognitive load
	if performance < 0.5 {
		aas.loadWeight = math.Min(aas.loadWeight+0.05, 0.5)
	} else {
		// If performance is high, can reduce sensitivity
		aas.loadWeight = math.Max(aas.loadWeight-0.02, 0.1)
	}
	
	// Activity weight inversely related to load weight
	aas.activityWeight = 0.5 - aas.loadWeight
}

// GetMetrics returns attention system metrics
func (aas *AdaptiveAttentionSystem) GetMetrics() map[string]interface{} {
	aas.mu.RLock()
	defer aas.mu.RUnlock()
	
	currentThreshold := aas.baseThreshold +
		(aas.cognitiveLoad * aas.loadWeight) -
		(aas.recentActivity * aas.activityWeight)
	
	currentThreshold = math.Max(aas.minThreshold, math.Min(aas.maxThreshold, currentThreshold))
	
	return map[string]interface{}{
		"current_threshold":  currentThreshold,
		"cognitive_load":     aas.cognitiveLoad,
		"recent_activity":    aas.recentActivity,
		"load_weight":        aas.loadWeight,
		"activity_weight":    aas.activityWeight,
		"history_size":       len(aas.thresholdHistory),
	}
}

// GetThresholdTrend analyzes threshold trend over time
func (aas *AdaptiveAttentionSystem) GetThresholdTrend(duration time.Duration) map[string]float64 {
	aas.mu.RLock()
	defer aas.mu.RUnlock()
	
	if len(aas.thresholdHistory) == 0 {
		return map[string]float64{
			"average":   0.5,
			"min":       0.5,
			"max":       0.5,
			"variance":  0.0,
		}
	}
	
	cutoff := time.Now().Add(-duration)
	
	var sum, min, max float64
	min = 1.0
	max = 0.0
	count := 0
	
	for _, record := range aas.thresholdHistory {
		if record.Timestamp.After(cutoff) {
			sum += record.Threshold
			count++
			
			if record.Threshold < min {
				min = record.Threshold
			}
			if record.Threshold > max {
				max = record.Threshold
			}
		}
	}
	
	if count == 0 {
		return map[string]float64{
			"average":   0.5,
			"min":       0.5,
			"max":       0.5,
			"variance":  0.0,
		}
	}
	
	average := sum / float64(count)
	
	// Calculate variance
	varianceSum := 0.0
	for _, record := range aas.thresholdHistory {
		if record.Timestamp.After(cutoff) {
			diff := record.Threshold - average
			varianceSum += diff * diff
		}
	}
	variance := varianceSum / float64(count)
	
	return map[string]float64{
		"average":   average,
		"min":       min,
		"max":       max,
		"variance":  variance,
		"std_dev":   math.Sqrt(variance),
	}
}

// MonitorAndAdjust continuously monitors and adjusts attention parameters
func (aas *AdaptiveAttentionSystem) MonitorAndAdjust(
	ac *AutonomousConsciousnessV13,
	interval time.Duration,
) {
	ticker := time.NewTicker(interval)
	defer ticker.Stop()
	
	for {
		select {
		case <-ticker.C:
			// Calculate cognitive load based on system state
			status := ac.GetStatus()
			
			// Cognitive load factors
			load := 0.5 // Base load
			
			if coherence, ok := status["temporal_coherence"].(float64); ok {
				// Higher coherence = lower load
				load -= (coherence - 0.5) * 0.3
			}
			
			if integration, ok := status["integration_level"].(float64); ok {
				// Higher integration = lower load
				load -= (integration - 0.5) * 0.2
			}
			
			// Update cognitive load
			aas.UpdateCognitiveLoad(load)
			
			// Activity based on recent thoughts
			// (would need access to thought stream)
			activity := 0.5 // Placeholder
			aas.UpdateRecentActivity(activity)
			
			// Record threshold
			aas.RecordThreshold("scheduled_monitoring")
			
		case <-ac.ctx.Done():
			return
		}
	}
}
