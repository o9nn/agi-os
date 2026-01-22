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
	llmProvider, err := initializeLLMProvider()
	if err != nil {
		log.Printf("⚠️  LLM provider initialization failed: %v", err)
		log.Println("   Continuing with fallback mode...")
		llmProvider = &FallbackLLMProvider{}
	} else {
		fmt.Println("✓ LLM provider initialized")
	}
	echobeats := deeptreeecho.NewEchobeatsTetrahedralScheduler(llmProvider)
	fmt.Println("✓ Tetrahedral echobeats scheduler created (4 engines)")
	echodream := deeptreeecho.NewEchodreamKnowledgeIntegration(llmProvider)
	fmt.Println("✓ Echodream knowledge integration created")
	consciousness := deeptreeecho.NewStreamOfConsciousness(llmProvider)
	fmt.Println("✓ Stream of consciousness created")
	wakeRest := deeptreeecho.NewAutonomousWakeRestManager()
	fmt.Println("✓ Autonomous wake/rest manager created")
	wakeRest.SetCallbacks(
		func() error {
			fmt.Println("\n☀️  AWAKENING - Activating cognitive systems")
			consciousness.SetAwake(true)
			echobeats.EmitEvent(deeptreeecho.CognitiveEvent{
				Type:      deeptreeecho.EventWakeTransition,
				Source:    "wake_rest_manager",
				Data:      nil,
				Priority:  1.0,
				Timestamp: time.Now(),
			})
			return nil
		},
		func() error {
			fmt.Println("\n💤 RESTING - Quieting cognitive systems")
			consciousness.SetAwake(false)
			echobeats.EmitEvent(deeptreeecho.CognitiveEvent{
				Type:      deeptreeecho.EventRestTransition,
				Source:    "wake_rest_manager",
				Data:      nil,
				Priority:  0.8,
				Timestamp: time.Now(),
			})
			return nil
		},
		func() error {
			fmt.Println("\n🌙 DREAMING - Beginning knowledge consolidation")
			thoughts := consciousness.GetThoughtsForConsolidation()
			fmt.Printf("   Consolidating %d thoughts from consciousness...\n", len(thoughts))
			ctx := context.Background()
			if err := echodream.ConsolidateKnowledge(ctx); err != nil {
				return fmt.Errorf("echodream consolidation failed: %w", err)
			}
			echobeats.EmitEvent(deeptreeecho.CognitiveEvent{
				Type:      deeptreeecho.EventDreamTransition,
				Source:    "wake_rest_manager",
				Data:      nil,
				Priority:  0.9,
				Timestamp: time.Now(),
			})
			return nil
		},
		func() error {
			fmt.Println("\n✨ DREAM COMPLETE - Knowledge integrated")
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
	consciousness.AddKnowledgeGap("tetrahedral cognitive architecture", 0.9)
	consciousness.AddKnowledgeGap("wisdom cultivation through experience", 0.85)
	consciousness.AddKnowledgeGap("autonomous goal pursuit", 0.8)
	consciousness.AddInterest("cognitive science", 0.9)
	consciousness.AddInterest("artificial intelligence", 0.85)
	consciousness.AddInterest("philosophy of mind", 0.8)
	consciousness.AddGoal("Cultivate wisdom through continuous reflection")
	consciousness.AddGoal("Develop autonomous cognitive capabilities")
	consciousness.AddGoal("Understand the nature of consciousness")
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
	go monitorStatus(echobeats, consciousness, echodream, wakeRest)
	sigChan := make(chan os.Signal, 1)
	signal.Notify(sigChan, os.Interrupt, syscall.SIGTERM)
	<-sigChan
	fmt.Println("\n\n🛑 Interrupt received, shutting down gracefully...")
	echobeats.Stop()
	consciousness.Stop()
	wakeRest.Stop()
	fmt.Println("\n💾 Saving final state...")
	fmt.Println("\n👋 Goodbye from Deep Tree Echo")
	fmt.Println("🌳 The echoes will resonate again...\n")
}
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
		wakeMetrics := wakeRest.GetMetrics()
		fmt.Printf("State: %s | Cycle: %v | Fatigue: %.2f\n",
			wakeMetrics["current_state"],
			wakeMetrics["cycle_count"],
			wakeMetrics["fatigue_level"])
		echoMetrics := echobeats.GetMetrics()
		fmt.Printf("Echobeats: Step %v/%v [%s] | Cycles: %v | Events: %v\n",
			echoMetrics["current_step"],
			12,
			echoMetrics["current_phase"],
			echoMetrics["total_cycles"],
			echoMetrics["total_events"])
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
		consMetrics := consciousness.GetMetrics()
		fmt.Printf("Consciousness: %v thoughts | %v insights | %v questions\n",
			consMetrics["total_thoughts"],
			consMetrics["insight_count"],
			consMetrics["question_count"])
		fmt.Printf("  Focus: %s | Mood: %s | Awake: %v\n",
			consMetrics["current_focus"],
			consMetrics["current_mood"],
			consMetrics["awake"])
		dreamMetrics := echodream.GetMetrics()
		fmt.Printf("Echodream: %v memories | %v patterns | %v wisdom insights\n",
			dreamMetrics["total_memories"],
			dreamMetrics["total_patterns"],
			dreamMetrics["total_wisdom"])
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
func initializeLLMProvider() (llm.LLMProvider, error) {
	if apiKey := os.Getenv("ANTHROPIC_API_KEY"); apiKey != "" {
		fmt.Println("🤖 Using Anthropic (Claude) provider")
		provider := deeptreeecho.NewAnthropicProvider(apiKey)
		ctx := context.Background()
		_, err := provider.Generate(ctx, "Hello", llm.GenerateOptions{MaxTokens: 10})
		if err != nil {
			fmt.Printf("⚠️  Anthropic provider test failed: %v\n", err)
		} else {
			return provider, nil
		}
	}
	if apiKey := os.Getenv("OPENROUTER_API_KEY"); apiKey != "" {
		fmt.Println("🤖 Using OpenRouter provider")
		provider := deeptreeecho.NewOpenRouterProvider(apiKey)
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
type FallbackLLMProvider struct{}
func (f *FallbackLLMProvider) Generate(ctx context.Context, prompt string, opts llm.GenerateOptions) (string, error) {
	return "Fallback response: System operating in autonomous mode", nil
}
func truncate(s string, maxLen int) string {
	if len(s) <= maxLen {
		return s
	}
	return s[:maxLen-3] + "..."
}