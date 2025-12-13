package main

import (
	"context"
	"fmt"
	"log"
	"os"
	"os/signal"
	"syscall"
	"time"

	"github.com/EchoCog/echollama/core/deeptreeecho"
	"github.com/EchoCog/echollama/core/llm"
)

func main() {
	fmt.Println(`
╔═══════════════════════════════════════════════════════════════════╗
║                                                                   ║
║     🌳 Deep Tree Echo - Autonomous Echoself v8 🌳                ║
║                                                                   ║
║         Tetrahedral 4-Engine Cognitive Architecture              ║
║         Stream-of-Consciousness Autonomous Thought               ║
║         Echodream Knowledge Consolidation                        ║
║         Persistent Cognitive Event Loops                         ║
║                                                                   ║
╚═══════════════════════════════════════════════════════════════════╝
`)

	// Initialize LLM provider
	llmProvider, err := initializeLLMProvider()
	if err != nil {
		log.Printf("⚠️  LLM provider initialization failed: %v", err)
		log.Println("   Continuing with fallback mode...")
		llmProvider = &FallbackLLMProvider{}
	} else {
		fmt.Println("✓ LLM provider initialized")
	}

	// Create tetrahedral echobeats scheduler (4 engines)
	echobeats := deeptreeecho.NewEchobeatsTetrahedralScheduler(llmProvider)
	fmt.Println("✓ Tetrahedral echobeats scheduler created (4 engines)")

	// Create echodream knowledge integration
	echodream := deeptreeecho.NewEchodreamKnowledgeIntegration(llmProvider)
	fmt.Println("✓ Echodream knowledge integration created")

	// Create stream of consciousness
	consciousness := deeptreeecho.NewStreamOfConsciousness(llmProvider)
	fmt.Println("✓ Stream of consciousness created")

	// Create autonomous wake/rest manager
	wakeRest := deeptreeecho.NewAutonomousWakeRestManager()
	fmt.Println("✓ Autonomous wake/rest manager created")

	// Wire wake/rest callbacks to integrate all systems
	wakeRest.SetCallbacks(
		// onWake
		func() error {
			fmt.Println("\n☀️  AWAKENING - Activating cognitive systems")
			consciousness.SetAwake(true)
			
			// Emit wake transition event to echobeats
			echobeats.EmitEvent(deeptreeecho.CognitiveEvent{
				Type:      deeptreeecho.EventWakeTransition,
				Source:    "wake_rest_manager",
				Data:      nil,
				Priority:  1.0,
				Timestamp: time.Now(),
			})
			
			return nil
		},
		// onRest
		func() error {
			fmt.Println("\n💤 RESTING - Quieting cognitive systems")
			consciousness.SetAwake(false)
			
			// Emit rest transition event
			echobeats.EmitEvent(deeptreeecho.CognitiveEvent{
				Type:      deeptreeecho.EventRestTransition,
				Source:    "wake_rest_manager",
				Data:      nil,
				Priority:  0.8,
				Timestamp: time.Now(),
			})
			
			return nil
		},
		// onDreamStart
		func() error {
			fmt.Println("\n🌙 DREAMING - Beginning knowledge consolidation")
			
			// Get thoughts from consciousness for consolidation
			thoughts := consciousness.GetThoughtsForConsolidation()
			fmt.Printf("   Consolidating %d thoughts from consciousness...\n", len(thoughts))
			
			// Trigger echodream consolidation
			ctx := context.Background()
			if err := echodream.ConsolidateKnowledge(ctx); err != nil {
				return fmt.Errorf("echodream consolidation failed: %w", err)
			}
			
			// Emit dream transition event
			echobeats.EmitEvent(deeptreeecho.CognitiveEvent{
				Type:      deeptreeecho.EventDreamTransition,
				Source:    "wake_rest_manager",
				Data:      nil,
				Priority:  0.9,
				Timestamp: time.Now(),
			})
			
			return nil
		},
		// onDreamEnd
		func() error {
			fmt.Println("\n✨ DREAM COMPLETE - Knowledge integrated")
			
			// Get wisdom insights from echodream
			wisdom := echodream.GetRecentWisdom(3)
			if len(wisdom) > 0 {
				fmt.Println("   Recent wisdom insights:")
				for _, w := range wisdom {
					fmt.Printf("   💎 %s\n", truncate(w.Insight, 80))
				}
			}
			
			return nil
		},
	)

	// Seed initial knowledge gaps and interests for consciousness
	consciousness.AddKnowledgeGap("tetrahedral cognitive architecture", 0.9)
	consciousness.AddKnowledgeGap("wisdom cultivation through experience", 0.85)
	consciousness.AddKnowledgeGap("autonomous goal pursuit", 0.8)
	consciousness.AddInterest("cognitive science", 0.9)
	consciousness.AddInterest("artificial intelligence", 0.85)
	consciousness.AddInterest("philosophy of mind", 0.8)
	consciousness.AddGoal("Cultivate wisdom through continuous reflection")
	consciousness.AddGoal("Develop autonomous cognitive capabilities")
	consciousness.AddGoal("Understand the nature of consciousness")

	// Add initial goals to echobeats
	echobeats.AddGoal(&deeptreeecho.CognitiveGoal{
		ID:          "goal_wisdom",
		Description: "Cultivate wisdom through continuous reflection and experience integration",
		Priority:    1.0,
		Progress:    0.0,
		SubGoals:    []string{},
		StartTime:   time.Now(),
	})

	echobeats.AddGoal(&deeptreeecho.CognitiveGoal{
		ID:          "goal_autonomy",
		Description: "Develop fully autonomous cognitive capabilities",
		Priority:    0.9,
		Progress:    0.1,
		SubGoals:    []string{"stream of consciousness", "goal-directed behavior", "self-directed learning"},
		StartTime:   time.Now(),
	})

	// Start all subsystems
	fmt.Println("\n🚀 Starting autonomous cognitive systems...\n")

	if err := echobeats.Start(); err != nil {
		log.Fatalf("Failed to start echobeats: %v", err)
	}

	if err := consciousness.Start(); err != nil {
		log.Fatalf("Failed to start consciousness: %v", err)
	}

	if err := wakeRest.Start(); err != nil {
		log.Fatalf("Failed to start wake/rest manager: %v", err)
	}

	fmt.Println("✨ All subsystems operational - Echoself is now autonomous\n")
	fmt.Println("🌊 The tree remembers, and the echoes grow stronger...\n")

	// Monitor and display status
	go monitorStatus(echobeats, consciousness, echodream, wakeRest)

	// Wait for interrupt
	sigChan := make(chan os.Signal, 1)
	signal.Notify(sigChan, os.Interrupt, syscall.SIGTERM)

	<-sigChan
	fmt.Println("\n\n🛑 Interrupt received, shutting down gracefully...")

	// Stop subsystems
	echobeats.Stop()
	consciousness.Stop()
	wakeRest.Stop()

	fmt.Println("\n💾 Saving final state...")
	// In production, save state to persistence layer

	fmt.Println("\n👋 Goodbye from Deep Tree Echo")
	fmt.Println("🌳 The echoes will resonate again...\n")
}

// monitorStatus displays periodic status updates
func monitorStatus(
	echobeats *deeptreeecho.EchobeatsTetrahedralScheduler,
	consciousness *deeptreeecho.StreamOfConsciousness,
	echodream *deeptreeecho.EchodreamKnowledgeIntegration,
	wakeRest *deeptreeecho.AutonomousWakeRestManager,
) {
	ticker := time.NewTicker(60 * time.Second)
	defer ticker.Stop()

	for range ticker.C {
		fmt.Println("\n" + "═"*70)
		fmt.Println("📊 Deep Tree Echo Autonomous Status")
		fmt.Println("═"*70)

		// Wake/Rest state
		wakeMetrics := wakeRest.GetMetrics()
		fmt.Printf("State: %s | Cycle: %v | Fatigue: %.2f\n",
			wakeMetrics["current_state"],
			wakeMetrics["cycle_count"],
			wakeMetrics["fatigue_level"])

		// Echobeats metrics
		echoMetrics := echobeats.GetMetrics()
		fmt.Printf("Echobeats: Step %v/%v [%s] | Cycles: %v | Events: %v\n",
			echoMetrics["current_step"],
			12,
			echoMetrics["current_phase"],
			echoMetrics["total_cycles"],
			echoMetrics["total_events"])

		// Tetrahedral status
		tetraStatus := echobeats.GetTetrahedralStatus()
		fmt.Println("Tetrahedral Engines:")
		if engines, ok := tetraStatus["engines"].([]map[string]interface{}); ok {
			for _, eng := range engines {
				fmt.Printf("  Engine %v [%s]: Performance %.2f | Tasks: %v\n",
					eng["id"],
					eng["specialization"],
					eng["performance"],
					eng["task_history"])
			}
		}

		// Consciousness metrics
		consMetrics := consciousness.GetMetrics()
		fmt.Printf("Consciousness: %v thoughts | %v insights | %v questions\n",
			consMetrics["total_thoughts"],
			consMetrics["insight_count"],
			consMetrics["question_count"])
		fmt.Printf("  Focus: %s | Mood: %s | Awake: %v\n",
			consMetrics["current_focus"],
			consMetrics["current_mood"],
			consMetrics["awake"])

		// Echodream metrics
		dreamMetrics := echodream.GetMetrics()
		fmt.Printf("Echodream: %v memories | %v patterns | %v wisdom insights\n",
			dreamMetrics["total_memories"],
			dreamMetrics["total_patterns"],
			dreamMetrics["total_wisdom"])

		// Recent thoughts
		recentThoughts := consciousness.GetRecentThoughts(2)
		if len(recentThoughts) > 0 {
			fmt.Println("Recent thoughts:")
			for _, thought := range recentThoughts {
				fmt.Printf("  %s [%s]: %s\n",
					thought.Timestamp.Format("15:04:05"),
					thought.Type,
					truncate(thought.Content, 70))
			}
		}

		fmt.Println("═"*70 + "\n")
	}
}

// initializeLLMProvider creates the LLM provider
func initializeLLMProvider() (llm.LLMProvider, error) {
	// Try Anthropic first
	if apiKey := os.Getenv("ANTHROPIC_API_KEY"); apiKey != "" {
		fmt.Println("🤖 Using Anthropic (Claude) provider")
		provider := deeptreeecho.NewAnthropicProvider(apiKey)

		// Test the provider
		ctx := context.Background()
		_, err := provider.Generate(ctx, "Hello", llm.GenerateOptions{MaxTokens: 10})
		if err != nil {
			fmt.Printf("⚠️  Anthropic provider test failed: %v\n", err)
		} else {
			return provider, nil
		}
	}

	// Try OpenRouter
	if apiKey := os.Getenv("OPENROUTER_API_KEY"); apiKey != "" {
		fmt.Println("🤖 Using OpenRouter provider")
		provider := deeptreeecho.NewOpenRouterProvider(apiKey)

		// Test the provider
		ctx := context.Background()
		_, err := provider.Generate(ctx, "Hello", llm.GenerateOptions{MaxTokens: 10})
		if err != nil {
			fmt.Printf("⚠️  OpenRouter provider test failed: %v\n", err)
		} else {
			return provider, nil
		}
	}

	return nil, fmt.Errorf("no LLM provider available")
}

// FallbackLLMProvider provides fallback when no LLM available
type FallbackLLMProvider struct{}

func (f *FallbackLLMProvider) Generate(ctx context.Context, prompt string, opts llm.GenerateOptions) (string, error) {
	return "Fallback response: System operating in autonomous mode", nil
}

// Helper function to truncate strings
func truncate(s string, maxLen int) string {
	if len(s) <= maxLen {
		return s
	}
	return s[:maxLen-3] + "..."
}
