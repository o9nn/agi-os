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
)
func main() {
	fmt.Println(`
╔═══════════════════════════════════════════════════════════╗
║                                                           ║
║              🌳 Deep Tree Echo - Iteration Test 🌳       ║
║                                                           ║
║        Testing Core Autonomous Components                ║
║                                                           ║
╚═══════════════════════════════════════════════════════════╝
`)
	llmProvider, err := initializeLLMProvider()
	if err != nil {
		log.Fatalf("❌ Failed to initialize LLM provider: %v", err)
	}
	fmt.Println("✓ LLM provider initialized")
	fmt.Println("\n🔧 Initializing core components...")
	echobeatsScheduler := deeptreeecho.NewEchobeatsScheduler(llmProvider)
	fmt.Println("   ✓ EchoBeats Scheduler (12-step cognitive loop)")
	echodreamIntegration := deeptreeecho.NewEchodreamKnowledgeIntegration(llmProvider)
	fmt.Println("   ✓ Echodream Knowledge Integration")
	wakeRestManager := deeptreeecho.NewAutonomousWakeRestManager()
	fmt.Println("   ✓ Autonomous Wake/Rest Manager")
	wakeRestManager.SetCallbacks(
		func() error {
			fmt.Println("\n☀️  AWAKENING - Resuming cognitive processing")
			return echobeatsScheduler.Start()
		},
		func() error {
			fmt.Println("\n💤 RESTING - Reducing cognitive activity")
			return nil
		},
		func() error {
			fmt.Println("\n🌙 DREAMING - Beginning knowledge consolidation")
			return echodreamIntegration.ConsolidateKnowledge(context.Background())
		},
		func() error {
			fmt.Println("\n✨ DREAM COMPLETE - Knowledge integrated")
			wisdomLevel := echodreamIntegration.ExtractWisdom()
			fmt.Printf("   Wisdom Level: %.1f%%\n", wisdomLevel*100)
			return nil
		},
	)
	fmt.Println("\n🔗 Components wired together")
	fmt.Println("\n🚀 Starting autonomous systems...")
	if err := echobeatsScheduler.Start(); err != nil {
		log.Fatalf("❌ Failed to start EchoBeats: %v", err)
	}
	if err := wakeRestManager.Start(); err != nil {
		log.Fatalf("❌ Failed to start Wake/Rest Manager: %v", err)
	}
	fmt.Println("\n✨ All systems operational - autonomous operation begun\n")
	go func() {
		ticker := time.NewTicker(30 * time.Second)
		defer ticker.Stop()
		for range ticker.C {
			printStatus(echobeatsScheduler, wakeRestManager, echodreamIntegration)
		}
	}()
	sigChan := make(chan os.Signal, 1)
	signal.Notify(sigChan, os.Interrupt, syscall.SIGTERM)
	<-sigChan
	fmt.Println("\n\n🛑 Interrupt received...")
	fmt.Println("\n🌳 Shutting down...")
	echobeatsScheduler.Stop()
	wakeRestManager.Stop()
	fmt.Println("\n👋 Goodbye from Deep Tree Echo\n")
}
func printStatus(
	echobeats *deeptreeecho.EchobeatsScheduler,
	wakeRest *deeptreeecho.AutonomousWakeRestManager,
	echodream *deeptreeecho.EchodreamKnowledgeIntegration,
) {
	fmt.Println("\n" + stringsRepeat("─", 60))
	fmt.Println("📊 Deep Tree Echo Status")
	fmt.Println(stringsRepeat("─", 60))
	wakeRestMetrics := wakeRest.GetMetrics()
	fmt.Printf("State: %v | Fatigue: %.2f | Cognitive Load: %.2f\n",
		wakeRestMetrics["current_state"],
		wakeRestMetrics["fatigue_level"],
		wakeRestMetrics["cognitive_load"])
	echobeatsMetrics := echobeats.GetMetrics()
	fmt.Printf("EchoBeats: Step %v/%v [%v] | Cycles: %v\n",
		echobeatsMetrics["current_step"],
		12,
		echobeatsMetrics["current_phase"],
		echobeatsMetrics["cycle_count"])
	engines := echobeats.GetEngineStatus()
	fmt.Printf("Engines: [1:%.2f] [2:%.2f] [3:%.2f]\n",
		engines[0]["performance"],
		engines[1]["performance"],
		engines[2]["performance"])
	echodreamMetrics := echodream.GetMetrics()
	fmt.Printf("Echodream: Memories=%v | Patterns=%v | Wisdom=%v\n",
		echodreamMetrics["total_memories"],
		echodreamMetrics["total_patterns"],
		echodreamMetrics["total_wisdom"])
	fmt.Println(stringsRepeat("─", 60) + "\n")
}
func initializeLLMProvider() (deeptreeecho.LLMProvider, error) {
	if apiKey := os.Getenv("ANTHROPIC_API_KEY"); apiKey != "" {
		fmt.Println("🤖 Using Anthropic (Claude) provider")
		provider := deeptreeecho.NewAnthropicProvider(apiKey, "claude-3-5-sonnet-20241022")
		return provider, nil
	}
	if apiKey := os.Getenv("OPENROUTER_API_KEY"); apiKey != "" {
		fmt.Println("🤖 Using OpenRouter provider")
		provider := deeptreeecho.NewOpenRouterProvider(apiKey, "anthropic/claude-3.5-sonnet")
		return provider, nil
	}
	if apiKey := os.Getenv("OPENAI_API_KEY"); apiKey != "" {
		fmt.Println("🤖 Using OpenAI provider")
		provider := deeptreeecho.NewOpenAIProvider(apiKey, "gpt-4")
		return provider, nil
	}
	return nil, fmt.Errorf("no LLM provider available - set ANTHROPIC_API_KEY, OPENROUTER_API_KEY, or OPENAI_API_KEY")
}
func stringsRepeat(s string, count int) string {
	result := ""
	for i := 0; i < count; i++ {
		result += s
	}
	return result
}