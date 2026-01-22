package wisdom
import (
	"sync"
	"time"
)
type WisdomMetrics struct {
	mu sync.RWMutex
	WisdomDepthScore      float64
	CoherenceStability    float64
	LearningVelocity      float64
	InsightFrequency      float64
	WisdomHistory         []WisdomSnapshot
	InsightCount          int64
	SkillsAcquired        int64
	PatternsRecognized    int64
	StartTime             time.Time
	LastUpdate            time.Time
}
type WisdomSnapshot struct {
	Timestamp         time.Time
	WisdomScore       float64
	Coherence         float64
	Stability         float64
	Awareness         float64
	ActiveSkills      int
	MemoryNodes       int
}
func NewWisdomMetrics() *WisdomMetrics {
	return &WisdomMetrics{
		WisdomHistory: make([]WisdomSnapshot, 0),
		StartTime:     time.Now(),
		LastUpdate:    time.Now(),
	}
}
func (wm *WisdomMetrics) Update(coherence, stability, awareness float64, activeSkills, memoryNodes int) {
	wm.mu.Lock()
	defer wm.mu.Unlock()
	wm.WisdomDepthScore = (awareness + coherence + stability) / 3.0
	wm.CoherenceStability = wm.calculateCoherenceStability()
	wm.LearningVelocity = wm.calculateLearningVelocity()
	wm.InsightFrequency = wm.calculateInsightFrequency()
	snapshot := WisdomSnapshot{
		Timestamp:    time.Now(),
		WisdomScore:  wm.WisdomDepthScore,
		Coherence:    coherence,
		Stability:    stability,
		Awareness:    awareness,
		ActiveSkills: activeSkills,
		MemoryNodes:  memoryNodes,
	}
	wm.WisdomHistory = append(wm.WisdomHistory, snapshot)
	if len(wm.WisdomHistory) > 1000 {
		wm.WisdomHistory = wm.WisdomHistory[len(wm.WisdomHistory)-1000:]
	}
	wm.LastUpdate = time.Now()
}
func (wm *WisdomMetrics) RecordInsight() {
	wm.mu.Lock()
	defer wm.mu.Unlock()
	wm.InsightCount++
}
func (wm *WisdomMetrics) RecordSkillAcquisition() {
	wm.mu.Lock()
	defer wm.mu.Unlock()
	wm.SkillsAcquired++
}
func (wm *WisdomMetrics) RecordPatternRecognition() {
	wm.mu.Lock()
	defer wm.mu.Unlock()
	wm.PatternsRecognized++
}
func (wm *WisdomMetrics) GetMetrics() WisdomMetricsSnapshot {
	wm.mu.RLock()
	defer wm.mu.RUnlock()
	return WisdomMetricsSnapshot{
		WisdomDepthScore:   wm.WisdomDepthScore,
		CoherenceStability: wm.CoherenceStability,
		LearningVelocity:   wm.LearningVelocity,
		InsightFrequency:   wm.InsightFrequency,
		InsightCount:       wm.InsightCount,
		SkillsAcquired:     wm.SkillsAcquired,
		PatternsRecognized: wm.PatternsRecognized,
		Uptime:             time.Since(wm.StartTime),
	}
}
type WisdomMetricsSnapshot struct {
	WisdomDepthScore   float64
	CoherenceStability float64
	LearningVelocity   float64
	InsightFrequency   float64
	InsightCount       int64
	SkillsAcquired     int64
	PatternsRecognized int64
	Uptime             time.Duration
}
func (wm *WisdomMetrics) calculateCoherenceStability() float64 {
	if len(wm.WisdomHistory) < 2 {
		return 1.0 
	}
	recentWindow := 10
	if len(wm.WisdomHistory) < recentWindow {
		recentWindow = len(wm.WisdomHistory)
	}
	recent := wm.WisdomHistory[len(wm.WisdomHistory)-recentWindow:]
	var sum float64
	for _, snapshot := range recent {
		sum += snapshot.Coherence
	}
	mean := sum / float64(len(recent))
	var variance float64
	for _, snapshot := range recent {
		diff := snapshot.Coherence - mean
		variance += diff * diff
	}
	variance /= float64(len(recent))
	stability := 1.0 / (1.0 + variance)
	return stability
}
func (wm *WisdomMetrics) calculateLearningVelocity() float64 {
	hoursElapsed := time.Since(wm.StartTime).Hours()
	if hoursElapsed < 0.01 {
		return 0.0
	}
	velocity := float64(wm.SkillsAcquired) / hoursElapsed
	return velocity
}
func (wm *WisdomMetrics) calculateInsightFrequency() float64 {
	hoursElapsed := time.Since(wm.StartTime).Hours()
	if hoursElapsed < 0.01 {
		return 0.0
	}
	frequency := float64(wm.InsightCount) / hoursElapsed
	return frequency
}
func (wm *WisdomMetrics) GetWisdomGrowthRate() float64 {
	wm.mu.RLock()
	defer wm.mu.RUnlock()
	if len(wm.WisdomHistory) < 2 {
		return 0.0
	}
	compareWindow := 10
	if len(wm.WisdomHistory) < compareWindow {
		compareWindow = len(wm.WisdomHistory)
	}
	current := wm.WisdomHistory[len(wm.WisdomHistory)-1]
	past := wm.WisdomHistory[len(wm.WisdomHistory)-compareWindow]
	timeDiff := current.Timestamp.Sub(past.Timestamp).Hours()
	if timeDiff < 0.01 {
		return 0.0
	}
	wisdomDiff := current.WisdomScore - past.WisdomScore
	growthRate := wisdomDiff / timeDiff
	return growthRate
}