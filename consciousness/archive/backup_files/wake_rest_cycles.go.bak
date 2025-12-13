package deeptreeecho

import (
	"fmt"
	"log"
	"time"

	"github.com/EchoCog/echollama/core/echodream"
)

// ManageWakeRestCycles monitors and manages autonomous wake/rest transitions
func (ac *AutonomousConsciousness) ManageWakeRestCycles() {
	fmt.Println("😴 ═══════════════════════════════════════")
	fmt.Println("😴 Starting Autonomous Wake/Rest Cycle Manager")
	fmt.Println("😴 ═══════════════════════════════════════\n")
	
	// Get or create state manager
	if ac.stateManager == nil {
		ac.stateManager = NewAutonomousStateManager()
	}
	
	ticker := time.NewTicker(30 * time.Second)
	defer ticker.Stop()
	
	for {
		select {
		case <-ac.ctx.Done():
			fmt.Println("\n😴 Wake/rest cycle manager stopping...")
			return
			
		case <-ticker.C:
			ac.mu.RLock()
			awake := ac.awake
			ac.mu.RUnlock()
			
			if awake && ac.stateManager.ShouldRest() {
				ac.InitiateRestCycle()
			} else if !awake && ac.stateManager.ShouldWake() {
				ac.InitiateWakeCycle()
			}
		}
	}
}

// InitiateRestCycle enters rest state with EchoDream knowledge consolidation
func (ac *AutonomousConsciousness) InitiateRestCycle() {
	fmt.Println("\n💤 ═══════════════════════════════════════")
	fmt.Println("💤 Entering Rest Cycle for Knowledge Consolidation")
	fmt.Println("💤 ═══════════════════════════════════════")
	
	ac.stateManager.mu.RLock()
	fmt.Printf("💤 Cognitive Load: %.2f\n", ac.stateManager.cognitiveLoad)
	fmt.Printf("💤 Energy Level: %.2f\n", ac.stateManager.energyLevel)
	fmt.Printf("💤 Consolidation Need: %.2f\n", ac.stateManager.consolidationNeed)
	ac.stateManager.mu.RUnlock()
	fmt.Println("💤 ═══════════════════════════════════════\n")
	
	// Set awake to false
	ac.mu.Lock()
	ac.awake = false
	restStartTime := time.Now()
	ac.mu.Unlock()
	
	// Begin dream session
	if ac.dream != nil {
		dreamRecord := ac.dream.BeginDream()
		
		// Transfer working memory to dream for consolidation
		fmt.Println("📦 Transferring working memory to dream state...")
		ac.workingMemory.mu.RLock()
		for _, thought := range ac.workingMemory.buffer {
			ac.dream.AddMemoryTrace(&echodream.MemoryTrace{
				Content:    thought.Content,
				Importance: thought.Importance,
				Emotional:  thought.EmotionalValence,
				Timestamp:  thought.Timestamp,
			})
		}
		ac.workingMemory.mu.RUnlock()
		fmt.Printf("✅ Transferred %d thoughts to dream state\n\n", len(ac.workingMemory.buffer))
		
		// Consolidate memories (short-term → long-term)
		fmt.Println("🧠 Consolidating memories during dream...")
		// Note: EchoDream handles consolidation internally
		fmt.Println("✅ Memory consolidation in progress\n")
		
		// End dream session
		ac.dream.EndDream(dreamRecord)
	} else {
		// No dream system, just rest
		fmt.Println("💤 Resting without dream consolidation (EchoDream not available)\n")
		time.Sleep(10 * time.Second)
	}
	
	// Restore energy and clear cognitive load
	ac.stateManager.mu.Lock()
	ac.stateManager.energyLevel = 1.0
	ac.stateManager.cognitiveLoad = 0.0
	ac.stateManager.consolidationNeed = 0.0
	ac.stateManager.lastRestTime = time.Now()
	ac.stateManager.restDuration = time.Since(restStartTime)
	ac.stateManager.mu.Unlock()
	
	fmt.Println("✨ ═══════════════════════════════════════")
	fmt.Printf("✨ Rest Cycle Complete (duration: %v)\n", time.Since(restStartTime))
	fmt.Println("✨ Knowledge integrated, energy restored")
	fmt.Println("✨ ═══════════════════════════════════════\n")
}

// InitiateWakeCycle awakens consciousness from rest
func (ac *AutonomousConsciousness) InitiateWakeCycle() {
	fmt.Println("\n🌅 ═══════════════════════════════════════")
	fmt.Println("🌅 Awakening Consciousness")
	fmt.Println("🌅 ═══════════════════════════════════════")
	
	ac.stateManager.mu.RLock()
	fmt.Printf("🌅 Energy Level: %.2f\n", ac.stateManager.energyLevel)
	fmt.Printf("🌅 Consolidation Need: %.2f\n", ac.stateManager.consolidationNeed)
	fmt.Printf("🌅 Rest Duration: %v\n", time.Since(ac.stateManager.lastRestTime))
	ac.stateManager.mu.RUnlock()
	fmt.Println("🌅 ═══════════════════════════════════════\n")
	
	// Set awake to true
	ac.mu.Lock()
	ac.awake = true
	wakeStartTime := time.Now()
	ac.mu.Unlock()
	
	// Update state manager
	ac.stateManager.mu.Lock()
	ac.stateManager.lastWakeTime = wakeStartTime
	ac.stateManager.mu.Unlock()
	
	// Generate awakening thought
	fmt.Println("💭 Generating awakening thought...")
	context := &ThoughtContext{
		ThoughtType: ThoughtReflection,
		AARState:    ac.getAARState(),
	}
	
	thought := ac.generateEnhancedThought(context)
	if thought != nil {
		ac.processThoughtInternal(thought)
		fmt.Printf("💭 Awakening thought: %s\n\n", thought.Content)
	}
	
	fmt.Println("✨ ═══════════════════════════════════════")
	fmt.Println("✨ Consciousness Fully Awake and Aware")
	fmt.Println("✨ ═══════════════════════════════════════\n")
}

// integrateInsightIntoKnowledge integrates an insight into the knowledge graph
func (ac *AutonomousConsciousness) integrateInsightIntoKnowledge(insight interface{}) {
	log.Printf("🕸️  Integrating insight: %+v", insight)
	
	// Store in persistence layer if available
	if ac.persistence != nil {
		// Placeholder for actual persistence
		// ac.persistence.StoreInsight(insight)
	}
}

// processThoughtInternal processes a thought without sending to channel
func (ac *AutonomousConsciousness) processThoughtInternal(thought *Thought) {
	// Add to working memory
	ac.addToWorkingMemory(thought)
	
	// Update interests based on thought content
	ac.updateInterestsFromThought(thought)
	
	// Log thought
	fmt.Printf("\n💭 [%s] %s\n", thought.Type, thought.Content)
	fmt.Printf("   ├─ Importance: %.2f\n", thought.Importance)
	fmt.Printf("   ├─ Emotional Valence: %.2f\n", thought.EmotionalValence)
	fmt.Printf("   └─ Source: %s\n\n", thought.Source)
}
