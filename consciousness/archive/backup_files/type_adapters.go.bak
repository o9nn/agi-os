package deeptreeecho

import (
	"time"
)

// InterestSystemInterface provides an interface for interest pattern management
// This allows different implementations to be used interchangeably
type InterestSystemInterface interface {
	UpdateInterest(topic string, delta float64)
	GetInterest(topic string) float64
	GetTopInterests(n int) []string
	GetCuriosityLevel() float64
	SetCuriosityLevel(level float64)
	ApplyDecay()
}

// Ensure InterestPatterns implements InterestSystemInterface
var _ InterestSystemInterface = (*InterestPatterns)(nil)

// InterestSystemAdapter adapts InterestPatterns to InterestSystem interface
type InterestSystemAdapter struct {
	patterns *InterestPatterns
}

// NewInterestSystemAdapter creates an adapter for InterestPatterns
func NewInterestSystemAdapter(patterns *InterestPatterns) *InterestSystemAdapter {
	return &InterestSystemAdapter{
		patterns: patterns,
	}
}

// UpdateInterest delegates to InterestPatterns
func (isa *InterestSystemAdapter) UpdateInterest(topic string, delta float64) {
	isa.patterns.UpdateInterest(topic, delta)
}

// GetInterest delegates to InterestPatterns
func (isa *InterestSystemAdapter) GetInterest(topic string) float64 {
	return isa.patterns.GetInterest(topic)
}

// GetTopInterests delegates to InterestPatterns
func (isa *InterestSystemAdapter) GetTopInterests(n int) []string {
	return isa.patterns.GetTopInterests(n)
}

// GetCuriosityLevel delegates to InterestPatterns
func (isa *InterestSystemAdapter) GetCuriosityLevel() float64 {
	return isa.patterns.GetCuriosityLevel()
}

// SetCuriosityLevel delegates to InterestPatterns
func (isa *InterestSystemAdapter) SetCuriosityLevel(level float64) {
	isa.patterns.SetCuriosityLevel(level)
}

// ApplyDecay delegates to InterestPatterns
func (isa *InterestSystemAdapter) ApplyDecay() {
	isa.patterns.ApplyDecay()
}

// AutonomousConsciousnessInterface defines the interface for autonomous consciousness
// This allows different versions to be used interchangeably
type AutonomousConsciousnessInterface interface {
	GetIdentity() *Identity
	GetWorkingMemory() *WorkingMemory
	GetInterests() InterestSystemInterface
	IsAwake() bool
	GetUptime() time.Duration
}

// AutonomousConsciousnessV4Adapter adapts V4 to the common interface
type AutonomousConsciousnessV4Adapter struct {
	v4 *AutonomousConsciousnessV4
}

// NewAutonomousConsciousnessV4Adapter creates an adapter for V4
func NewAutonomousConsciousnessV4Adapter(v4 *AutonomousConsciousnessV4) *AutonomousConsciousnessV4Adapter {
	return &AutonomousConsciousnessV4Adapter{v4: v4}
}

// GetIdentity returns the identity
func (aca *AutonomousConsciousnessV4Adapter) GetIdentity() *Identity {
	return aca.v4.identity
}

// GetWorkingMemory returns the working memory
func (aca *AutonomousConsciousnessV4Adapter) GetWorkingMemory() *WorkingMemory {
	return aca.v4.workingMemory
}

// GetInterests returns the interest system
func (aca *AutonomousConsciousnessV4Adapter) GetInterests() InterestSystemInterface {
	return aca.v4.interests
}

// IsAwake returns whether the system is awake
func (aca *AutonomousConsciousnessV4Adapter) IsAwake() bool {
	aca.v4.mu.RLock()
	defer aca.v4.mu.RUnlock()
	return aca.v4.awake
}

// GetUptime returns the uptime duration
func (aca *AutonomousConsciousnessV4Adapter) GetUptime() time.Duration {
	aca.v4.mu.RLock()
	defer aca.v4.mu.RUnlock()
	if aca.v4.startTime.IsZero() {
		return 0
	}
	return time.Since(aca.v4.startTime)
}

// DiscussionManagerV4 is a version of DiscussionManager compatible with V4 types
type DiscussionManagerV4 struct {
	*DiscussionManager
}

// NewDiscussionManagerV4 creates a discussion manager compatible with V4
func NewDiscussionManagerV4(consciousness *AutonomousConsciousnessV4, interests *InterestPatterns) *DiscussionManagerV4 {
	// Create adapters to bridge the type gap
	consciousnessAdapter := &IntegratedAutonomousConsciousness{
		// Map V4 fields to IntegratedAutonomousConsciousness
		// This is a temporary bridge until types are fully unified
	}
	
	// Cast interests to the old InterestSystem struct type
	// This is a workaround until DiscussionManager is updated to use the interface
	interestSystem := (*InterestSystem)(nil) // Placeholder - DiscussionManager needs refactoring
	
	dm := &DiscussionManager{
		activeDiscussions:   make(map[string]*Discussion),
		engagementThreshold: 0.3,
		interests:           interestSystem,
		consciousness:       consciousnessAdapter,
		maxDiscussions:      10,
	}
	
	return &DiscussionManagerV4{DiscussionManager: dm}
}
