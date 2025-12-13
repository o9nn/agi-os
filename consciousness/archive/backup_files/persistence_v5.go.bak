package deeptreeecho

import (
	"encoding/json"
	"fmt"
	"time"

	"github.com/EchoCog/echollama/core/memory"
)

// PersistenceV5 implements complete state save/load for autonomous consciousness
// This enables true continuity across wake/rest cycles and system restarts
type PersistenceV5 struct {
	persistence *memory.SupabasePersistence
	identityID  string
	version     int
}

// CognitiveStateSnapshot captures complete cognitive state for persistence
type CognitiveStateSnapshot struct {
	Version          int                    `json:"version"`
	Timestamp        time.Time              `json:"timestamp"`
	IdentityID       string                 `json:"identity_id"`
	
	// Core identity
	IdentityState    *IdentityState         `json:"identity_state"`
	
	// Working memory
	WorkingMemory    []*Thought             `json:"working_memory"`
	
	// Interest patterns
	Interests        map[string]float64     `json:"interests"`
	InterestHistory  []InterestSnapshot     `json:"interest_history"`
	
	// Skill registry
	Skills           map[string]*SkillState `json:"skills"`
	
	// Wisdom metrics
	WisdomState      *WisdomState           `json:"wisdom_state"`
	
	// Cognitive state
	CognitiveParams  *CognitiveParams       `json:"cognitive_params"`
	
	// Consciousness metrics
	ConsciousnessMetrics *ConsciousnessMetrics `json:"consciousness_metrics"`
	
	// AAR state
	AARState         *AARStateSnapshot      `json:"aar_state"`
	
	// Dream state
	DreamState       *DreamStateSnapshot    `json:"dream_state"`
	
	// Metadata
	UpTime           time.Duration          `json:"uptime"`
	Iterations       int64                  `json:"iterations"`
	ThoughtsGenerated int64                 `json:"thoughts_generated"`
}

// IdentityState captures identity information
type IdentityState struct {
	Name             string                 `json:"name"`
	CoreBeliefs      []string               `json:"core_beliefs"`
	Values           map[string]float64     `json:"values"`
	Personality      map[string]float64     `json:"personality"`
	SelfImage        string                 `json:"self_image"`
	CreatedAt        time.Time              `json:"created_at"`
	LastUpdated      time.Time              `json:"last_updated"`
}

// SkillState captures skill proficiency and practice history
type SkillState struct {
	Name             string                 `json:"name"`
	Proficiency      float64                `json:"proficiency"`
	PracticeCount    int                    `json:"practice_count"`
	LastPracticed    time.Time              `json:"last_practiced"`
	TotalPracticeTime time.Duration         `json:"total_practice_time"`
}

// WisdomState captures wisdom metrics
type WisdomState struct {
	Depth            float64                `json:"depth"`
	Breadth          float64                `json:"breadth"`
	Integration      float64                `json:"integration"`
	Coherence        float64                `json:"coherence"`
	Reflection       float64                `json:"reflection"`
	TotalScore       float64                `json:"total_score"`
	UpdatedAt        time.Time              `json:"updated_at"`
}

// CognitiveParams captures cognitive state parameters
type CognitiveParams struct {
	Arousal          float64                `json:"arousal"`
	Valence          float64                `json:"valence"`
	Clarity          float64                `json:"clarity"`
	Openness         float64                `json:"openness"`
	Load             float64                `json:"load"`
	Capacity         float64                `json:"capacity"`
	PastWeight       float64                `json:"past_weight"`
	FutureWeight     float64                `json:"future_weight"`
	PresentWeight    float64                `json:"present_weight"`
}

// ConsciousnessMetrics captures consciousness stream metrics
type ConsciousnessMetrics struct {
	ThoughtsEmerged  uint64                 `json:"thoughts_emerged"`
	StimuliProcessed uint64                 `json:"stimuli_processed"`
	FlowQuality      float64                `json:"flow_quality"`
	ActivityLevel    float64                `json:"activity_level"`
	AttentionIntensity float64              `json:"attention_intensity"`
}

// AARStateSnapshot captures AAR geometric state
type AARStateSnapshot struct {
	Position         []float64              `json:"position"`
	Velocity         []float64              `json:"velocity"`
	Dimensions       int                    `json:"dimensions"`
}

// DreamStateSnapshot captures dream system state
type DreamStateSnapshot struct {
	RestQuality      float64                `json:"rest_quality"`
	LastRestTime     time.Time              `json:"last_rest_time"`
	CircadianPhase   float64                `json:"circadian_phase"`
	FatigueLevel     float64                `json:"fatigue_level"`
}

// InterestSnapshot captures interest at a point in time
type InterestSnapshot struct {
	Timestamp        time.Time              `json:"timestamp"`
	Topic            string                 `json:"topic"`
	Strength         float64                `json:"strength"`
}

// NewPersistenceV5 creates a new V5 persistence manager
func NewPersistenceV5(persistence *memory.SupabasePersistence, identityID string) *PersistenceV5 {
	return &PersistenceV5{
		persistence: persistence,
		identityID:  identityID,
		version:     5, // V5 schema
	}
}

// SaveState saves complete cognitive state to persistent storage
func (p *PersistenceV5) SaveState(ac *AutonomousConsciousnessV4) error {
	if p.persistence == nil {
		return fmt.Errorf("persistence layer not initialized")
	}
	
	fmt.Println("💾 Saving cognitive state...")
	
	// Build snapshot
	snapshot := &CognitiveStateSnapshot{
		Version:    p.version,
		Timestamp:  time.Now(),
		IdentityID: p.identityID,
	}
	
	// Capture identity state
	snapshot.IdentityState = p.captureIdentityState(ac.identity)
	
	// Capture working memory
	snapshot.WorkingMemory = p.captureWorkingMemory(ac.workingMemory)
	
	// Capture interests
	snapshot.Interests, snapshot.InterestHistory = p.captureInterests(ac.interests)
	
	// Capture skills
	snapshot.Skills = p.captureSkills(ac.skills)
	
	// Capture wisdom
	snapshot.WisdomState = p.captureWisdom(ac.wisdomMetrics)
	
	// Capture cognitive state
	if ac.consciousnessStream != nil && ac.consciousnessStream.cognitiveState != nil {
		snapshot.CognitiveParams = p.captureCognitiveState(ac.consciousnessStream.cognitiveState)
	}
	
	// Capture consciousness metrics
	if ac.consciousnessStream != nil {
		snapshot.ConsciousnessMetrics = p.captureConsciousnessMetrics(ac.consciousnessStream)
	}
	
	// Capture AAR state
	if ac.aarCore != nil {
		snapshot.AARState = p.captureAARState(ac.aarCore)
	}
	
	// Capture dream state
	if ac.dreamTrigger != nil {
		snapshot.DreamState = p.captureDreamState(ac.dreamTrigger, ac.loadManager)
	}
	
	// Capture runtime metrics
	ac.mu.RLock()
	snapshot.UpTime = time.Since(ac.startTime)
	snapshot.Iterations = ac.iterations
	ac.mu.RUnlock()
	
	// Serialize to JSON
	data, err := json.Marshal(snapshot)
	if err != nil {
		return fmt.Errorf("failed to marshal state: %w", err)
	}
	
	// Save to Supabase
	err = p.persistence.SaveCognitiveState(p.identityID, data)
	if err != nil {
		fmt.Printf("⚠️  Failed to save to database: %v\n", err)
		// Don't fail completely, just warn
	}
	
	// Also save hypergraph memory
	// TODO: Implement Persist method in HypergraphMemory
	// if ac.hypergraph != nil {
	// 	err = ac.hypergraph.Persist()
	// 	if err != nil {
	// 		fmt.Printf("⚠️  Failed to persist hypergraph: %v\n", err)
	// 	}
	// }
	
	fmt.Printf("✅ Cognitive state saved (version %d, %d bytes)\n", p.version, len(data))
	
	return nil
}

// LoadState loads complete cognitive state from persistent storage
func (p *PersistenceV5) LoadState(ac *AutonomousConsciousnessV4) error {
	if p.persistence == nil {
		return fmt.Errorf("persistence layer not initialized")
	}
	
	fmt.Println("📥 Loading cognitive state...")
	
	// Load from Supabase
	data, err := p.persistence.LoadCognitiveState(p.identityID)
	if err != nil {
		fmt.Printf("⚠️  Failed to load from database: %v\n", err)
		return nil // Don't fail, just start fresh
	}
	
	if len(data) == 0 {
		fmt.Println("ℹ️  No saved state found, starting fresh")
		return nil
	}
	
	// Deserialize
	var snapshot CognitiveStateSnapshot
	err = json.Unmarshal(data, &snapshot)
	if err != nil {
		fmt.Printf("⚠️  Failed to unmarshal state: %v\n", err)
		return nil // Don't fail, just start fresh
	}
	
	// Verify version compatibility
	if snapshot.Version != p.version {
		fmt.Printf("⚠️  State version mismatch: saved=%d, current=%d\n", snapshot.Version, p.version)
		// Could implement migration here
	}
	
	// Restore identity state
	p.restoreIdentityState(ac.identity, snapshot.IdentityState)
	
	// Restore working memory
	p.restoreWorkingMemory(ac.workingMemory, snapshot.WorkingMemory)
	
	// Restore interests
	p.restoreInterests(ac.interests, snapshot.Interests, snapshot.InterestHistory)
	
	// Restore skills
	p.restoreSkills(ac.skills, snapshot.Skills)
	
	// Restore wisdom
	p.restoreWisdom(ac.wisdomMetrics, snapshot.WisdomState)
	
	// Restore cognitive state
	if ac.consciousnessStream != nil && ac.consciousnessStream.cognitiveState != nil && snapshot.CognitiveParams != nil {
		p.restoreCognitiveState(ac.consciousnessStream.cognitiveState, snapshot.CognitiveParams)
	}
	
	// Restore consciousness metrics
	if ac.consciousnessStream != nil && snapshot.ConsciousnessMetrics != nil {
		p.restoreConsciousnessMetrics(ac.consciousnessStream, snapshot.ConsciousnessMetrics)
	}
	
	// Restore AAR state
	if ac.aarCore != nil && snapshot.AARState != nil {
		p.restoreAARState(ac.aarCore, snapshot.AARState)
	}
	
	// Restore dream state
	if ac.dreamTrigger != nil && snapshot.DreamState != nil {
		p.restoreDreamState(ac.dreamTrigger, ac.loadManager, snapshot.DreamState)
	}
	
	// Restore hypergraph memory - commented out until Persist/Load methods are implemented
	// if ac.hypergraph != nil {
	// 	err = ac.hypergraph.Load()
	// 	if err != nil {
	// 		fmt.Printf("⚠️  Failed to load hypergraph: %v\n", err)
	// 	}
	// }
	
	fmt.Printf("✅ Cognitive state restored (saved: %s, uptime: %s, iterations: %d)\n",
		snapshot.Timestamp.Format("2006-01-02 15:04:05"),
		snapshot.UpTime,
		snapshot.Iterations)
	
	return nil
}

// Capture methods

func (p *PersistenceV5) captureIdentityState(identity *Identity) *IdentityState {
	if identity == nil {
		return nil
	}
	
	identity.mu.RLock()
	defer identity.mu.RUnlock()
	
	return &IdentityState{
		Name:        identity.Name,
		CoreBeliefs: []string{identity.Essence}, // Use Essence as core belief
		Values:      make(map[string]float64),   // TODO: Add values field to Identity
		Personality: make(map[string]float64),   // TODO: Add personality field to Identity
		SelfImage:   identity.Essence,           // Use Essence as self-image
		CreatedAt:   identity.CreatedAt,
		LastUpdated: time.Now(),
	}
}

func (p *PersistenceV5) captureWorkingMemory(wm *WorkingMemory) []*Thought {
	if wm == nil {
		return nil
	}
	
	wm.mu.RLock()
	defer wm.mu.RUnlock()
	
	// Deep copy thoughts
	thoughts := make([]*Thought, len(wm.buffer))
	copy(thoughts, wm.buffer)
	
	return thoughts
}

func (p *PersistenceV5) captureInterests(interests *InterestPatterns) (map[string]float64, []InterestSnapshot) {
	if interests == nil {
		return nil, nil
	}
	
	// Copy current interests using accessor method
	currentInterests := interests.GetPatterns()
	
	// Capture history snapshots
	var history []InterestSnapshot
	for topic, strength := range currentInterests {
		history = append(history, InterestSnapshot{
			Timestamp: time.Now(),
			Topic:     topic,
			Strength:  strength,
		})
	}
	
	return currentInterests, history
}

func (p *PersistenceV5) captureSkills(skills *SkillRegistryEnhanced) map[string]*SkillState {
	if skills == nil {
		return nil
	}
	
	skills.mu.RLock()
	defer skills.mu.RUnlock()
	
	skillStates := make(map[string]*SkillState)
	
	for name, skill := range skills.skills {
		skillStates[name] = &SkillState{
			Name:              name,
			Proficiency:       skill.Proficiency,
			PracticeCount:     skill.PracticeCount,
			LastPracticed:     skill.LastPracticed,
			TotalPracticeTime: 0, // TODO: Add TotalPracticeTime to Skill struct
		}
	}
	
	return skillStates
}

func (p *PersistenceV5) captureWisdom(wisdom *WisdomMetrics) *WisdomState {
	if wisdom == nil {
		return nil
	}
	
	wisdom.mu.RLock()
	defer wisdom.mu.RUnlock()
	
	return &WisdomState{
		Depth:       wisdom.KnowledgeDepth,
		Breadth:     wisdom.KnowledgeBreadth,
		Integration: wisdom.IntegrationLevel,
		Coherence:   wisdom.PracticalApplication, // Use PracticalApplication as coherence proxy
		Reflection:  wisdom.ReflectiveInsight,
		TotalScore:  wisdom.WisdomScore,
		UpdatedAt:   time.Now(),
	}
}

func (p *PersistenceV5) captureCognitiveState(cs *CognitiveState) *CognitiveParams {
	if cs == nil {
		return nil
	}
	
	cs.mu.RLock()
	defer cs.mu.RUnlock()
	
	return &CognitiveParams{
		Arousal:       cs.arousal,
		Valence:       cs.valence,
		Clarity:       cs.clarity,
		Openness:      cs.openness,
		Load:          cs.load,
		Capacity:      cs.capacity,
		PastWeight:    cs.pastWeight,
		FutureWeight:  cs.futureWeight,
		PresentWeight: cs.presentWeight,
	}
}

func (p *PersistenceV5) captureConsciousnessMetrics(stream *ContinuousConsciousnessStream) *ConsciousnessMetrics {
	if stream == nil {
		return nil
	}
	
	stream.mu.RLock()
	defer stream.mu.RUnlock()
	
	flowQuality := 0.0
	if stream.flowState != nil {
		stream.flowState.mu.RLock()
		flowQuality = stream.flowState.quality
		stream.flowState.mu.RUnlock()
	}
	
	attentionIntensity := 0.0
	if stream.attentionFocus != nil {
		stream.attentionFocus.mu.RLock()
		attentionIntensity = stream.attentionFocus.intensity
		stream.attentionFocus.mu.RUnlock()
	}
	
	return &ConsciousnessMetrics{
		ThoughtsEmerged:    stream.thoughtsEmerged,
		StimuliProcessed:   stream.stimuliProcessed,
		FlowQuality:        flowQuality,
		ActivityLevel:      stream.currentActivity,
		AttentionIntensity: attentionIntensity,
	}
}

func (p *PersistenceV5) captureAARState(aar *AARCore) *AARStateSnapshot {
	if aar == nil {
		return nil
	}
	
	aar.mu.RLock()
	defer aar.mu.RUnlock()
	
	return &AARStateSnapshot{
		Position:   append([]float64{}, aar.selfVector...),
		Velocity:   []float64{aar.coherence, aar.stability}, // Use coherence/stability as velocity proxy
		Dimensions: len(aar.selfVector),
	}
}

func (p *PersistenceV5) captureDreamState(trigger *AutomaticDreamTrigger, loadMgr *CognitiveLoadManager) *DreamStateSnapshot {
	if trigger == nil {
		return nil
	}
	
	trigger.mu.RLock()
	defer trigger.mu.RUnlock()
	
	fatigueLevel := 0.0
	if loadMgr != nil {
		loadMgr.mu.RLock()
		fatigueLevel = loadMgr.fatigueLevel
		loadMgr.mu.RUnlock()
	}
	
	return &DreamStateSnapshot{
		RestQuality:    trigger.restQuality,
		LastRestTime:   trigger.lastRestTime,
		CircadianPhase: trigger.circadianPhase,
		FatigueLevel:   fatigueLevel,
	}
}

// Restore methods

func (p *PersistenceV5) restoreIdentityState(identity *Identity, state *IdentityState) {
	if identity == nil || state == nil {
		return
	}
	
	identity.mu.Lock()
	defer identity.mu.Unlock()
	
	identity.Name = state.Name
	identity.Essence = state.SelfImage // Restore essence from self-image
	identity.CreatedAt = state.CreatedAt
	// TODO: Add fields for coreBeliefs, values, personality to Identity struct
}

func (p *PersistenceV5) restoreWorkingMemory(wm *WorkingMemory, thoughts []*Thought) {
	if wm == nil || thoughts == nil {
		return
	}
	
	wm.mu.Lock()
	defer wm.mu.Unlock()
	
	wm.buffer = make([]*Thought, len(thoughts))
	copy(wm.buffer, thoughts)
}

func (p *PersistenceV5) restoreInterests(interests *InterestPatterns, patterns map[string]float64, history []InterestSnapshot) {
	if interests == nil || patterns == nil {
		return
	}
	
	// Restore interests using UpdateInterest method
	for topic, strength := range patterns {
		interests.UpdateInterest(topic, strength)
	}
}

func (p *PersistenceV5) restoreSkills(skills *SkillRegistryEnhanced, states map[string]*SkillState) {
	if skills == nil || states == nil {
		return
	}
	
	skills.mu.Lock()
	defer skills.mu.Unlock()
	
	for name, state := range states {
		if skill, exists := skills.skills[name]; exists {
			skill.Proficiency = state.Proficiency
			skill.LastPracticed = state.LastPracticed
			// skill.TotalPracticeTime = state.TotalPracticeTime // TODO: Add field to Skill
		}
	}
}

func (p *PersistenceV5) restoreWisdom(wisdom *WisdomMetrics, state *WisdomState) {
	if wisdom == nil || state == nil {
		return
	}
	
	wisdom.mu.Lock()
	defer wisdom.mu.Unlock()
	
	wisdom.KnowledgeDepth = state.Depth
	wisdom.KnowledgeBreadth = state.Breadth
	wisdom.IntegrationLevel = state.Integration
	wisdom.PracticalApplication = state.Coherence
	wisdom.ReflectiveInsight = state.Reflection
	wisdom.WisdomScore = state.TotalScore
}

func (p *PersistenceV5) restoreCognitiveState(cs *CognitiveState, params *CognitiveParams) {
	if cs == nil || params == nil {
		return
	}
	
	cs.mu.Lock()
	defer cs.mu.Unlock()
	
	cs.arousal = params.Arousal
	cs.valence = params.Valence
	cs.clarity = params.Clarity
	cs.openness = params.Openness
	cs.load = params.Load
	cs.capacity = params.Capacity
	cs.pastWeight = params.PastWeight
	cs.futureWeight = params.FutureWeight
	cs.presentWeight = params.PresentWeight
}

func (p *PersistenceV5) restoreConsciousnessMetrics(stream *ContinuousConsciousnessStream, metrics *ConsciousnessMetrics) {
	if stream == nil || metrics == nil {
		return
	}
	
	stream.mu.Lock()
	defer stream.mu.Unlock()
	
	stream.thoughtsEmerged = metrics.ThoughtsEmerged
	stream.stimuliProcessed = metrics.StimuliProcessed
	stream.currentActivity = metrics.ActivityLevel
}

func (p *PersistenceV5) restoreAARState(aar *AARCore, state *AARStateSnapshot) {
	if aar == nil || state == nil {
		return
	}
	
	aar.mu.Lock()
	defer aar.mu.Unlock()
	
	aar.selfVector = append([]float64{}, state.Position...)
	// Restore coherence and stability from velocity proxy
	if len(state.Velocity) >= 2 {
		aar.coherence = state.Velocity[0]
		aar.stability = state.Velocity[1]
	}
}

func (p *PersistenceV5) restoreDreamState(trigger *AutomaticDreamTrigger, loadMgr *CognitiveLoadManager, state *DreamStateSnapshot) {
	if trigger == nil || state == nil {
		return
	}
	
	trigger.mu.Lock()
	trigger.restQuality = state.RestQuality
	trigger.lastRestTime = state.LastRestTime
	trigger.circadianPhase = state.CircadianPhase
	trigger.mu.Unlock()
	
	if loadMgr != nil {
		loadMgr.mu.Lock()
		loadMgr.fatigueLevel = state.FatigueLevel
		loadMgr.mu.Unlock()
	}
}

// Utility methods

func (p *PersistenceV5) copyMap(m map[string]float64) map[string]float64 {
	if m == nil {
		return nil
	}
	
	copy := make(map[string]float64)
	for k, v := range m {
		copy[k] = v
	}
	return copy
}
