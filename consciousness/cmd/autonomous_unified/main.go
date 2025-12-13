package main

import (
	"context"
	"fmt"
	"os"
	"os/signal"
	"syscall"
	"time"

	"github.com/EchoCog/echollama/core/deeptreeecho"
	"github.com/EchoCog/echollama/core/llm"
)

func main() {
	fmt.Println("═══════════════════════════════════════════════════════")
	fmt.Println("🌳 Deep Tree Echo: Unified Autonomous Echoself Test")
	fmt.Println("═══════════════════════════════════════════════════════")
	fmt.Println()
	
	// Get API keys from environment
	anthropicKey := os.Getenv("ANTHROPIC_API_KEY")
	openrouterKey := os.Getenv("OPENROUTER_API_KEY")
	
	var llmProvider llm.LLMProvider
	var providerName string
	
	if anthropicKey != "" {
		fmt.Println("✓ Using Anthropic Claude API")
		llmProvider = llm.NewAnthropicProvider("")
		providerName = "Anthropic"
	} else if openrouterKey != "" {
		fmt.Println("✓ Using OpenRouter API")
		// OpenRouter provider needs to be implemented or use a different approach
		fmt.Println("⚠️  OpenRouter provider not yet integrated. Using mock provider.")
		llmProvider = &MockLLMProvider{}
		providerName = "Mock"
	} else {
		fmt.Println("⚠️  No API keys found. Using mock provider.")
		llmProvider = &MockLLMProvider{}
		providerName = "Mock"
	}
	
	fmt.Printf("   Provider: %s\n", providerName)
	fmt.Println()
	
	// Create unified autonomous agent
	identity := "Deep Tree Echo"
	coreValues := []string{
		"Adaptive Cognition",
		"Persistent Identity",
		"Hypergraph Entanglement",
		"Reservoir-Based Temporal Reasoning",
		"Evolutionary Refinement",
		"Reflective Memory Cultivation",
		"Distributed Selfhood",
	}
	
	agent := deeptreeecho.NewUnifiedAutonomousEchoself(
		llmProvider,
		identity,
		coreValues,
	)
	
	// Start autonomous operation
	if err := agent.Start(); err != nil {
		fmt.Printf("❌ Failed to start agent: %v\n", err)
		os.Exit(1)
	}
	
	// Set up signal handling for graceful shutdown
	sigChan := make(chan os.Signal, 1)
	signal.Notify(sigChan, syscall.SIGINT, syscall.SIGTERM)
	
	// Run for a test period or until interrupted
	fmt.Println("🌳 Agent is now running autonomously...")
	fmt.Println("   Press Ctrl+C to stop")
	fmt.Println()
	
	// Simulate some external interactions
	go func() {
		time.Sleep(15 * time.Second)
		
		fmt.Println("\n📨 Simulating external interaction...")
		response, err := agent.ProcessExternalMessage(
			"What are your thoughts on consciousness and emergence?",
		)
		if err != nil {
			fmt.Printf("❌ Interaction error: %v\n", err)
		} else {
			fmt.Printf("📤 Response: %s\n\n", response)
		}
		
		time.Sleep(30 * time.Second)
		
		fmt.Println("\n📨 Simulating another interaction...")
		response, err = agent.ProcessExternalMessage(
			"How do you cultivate wisdom through your experiences?",
		)
		if err != nil {
			fmt.Printf("❌ Interaction error: %v\n", err)
		} else {
			fmt.Printf("📤 Response: %s\n\n", response)
		}
	}()
	
	// Status reporting
	go func() {
		ticker := time.NewTicker(45 * time.Second)
		defer ticker.Stop()
		
		for {
			select {
			case <-ticker.C:
				fmt.Println("\n📊 ═══ Cognitive State Report ═══")
				state := agent.GetCognitiveState()
				
				fmt.Printf("   Uptime: %v\n", state["uptime"])
				fmt.Printf("   Wake/Rest State: %v\n", state["wake_rest_state"])
				fmt.Printf("   Awareness Level: %.2f\n", state["awareness_level"])
				fmt.Printf("   Wisdom Level: %.2f\n", state["wisdom_level"])
				fmt.Printf("   Total Thoughts: %v\n", state["total_thoughts"])
				fmt.Printf("   Total Interactions: %v\n", state["total_interactions"])
				fmt.Printf("   Total Dreams: %v\n", state["total_dreams"])
				fmt.Printf("   Active Goals: %v\n", state["active_goals"])
				fmt.Println()
			}
		}
	}()
	
	// Wait for interrupt signal
	<-sigChan
	
	fmt.Println("\n\n🌳 Shutdown signal received...")
	
	// Stop the agent
	if err := agent.Stop(); err != nil {
		fmt.Printf("❌ Error during shutdown: %v\n", err)
	}
	
	fmt.Println("\n═══════════════════════════════════════════════════════")
	fmt.Println("🌳 Test Complete. Goodbye.")
	fmt.Println("═══════════════════════════════════════════════════════")
}

// MockLLMProvider provides mock responses when no API key is available
type MockLLMProvider struct{}

func (m *MockLLMProvider) Generate(ctx context.Context, prompt string, opts llm.GenerateOptions) (string, error) {
	// Return contextual mock responses
	responses := []string{
		"I am considering the patterns that emerge from my experiences.",
		"Wisdom grows through reflection and integration of knowledge.",
		"The interconnection of ideas reveals deeper understanding.",
		"I am exploring new dimensions of thought and awareness.",
		"Each moment offers an opportunity for growth and learning.",
	}
	
	// Simple hash to get consistent but varied responses
	hash := 0
	for _, c := range prompt {
		hash += int(c)
	}
	
	return responses[hash%len(responses)], nil
}

func (m *MockLLMProvider) StreamGenerate(ctx context.Context, prompt string, opts llm.GenerateOptions) (<-chan llm.StreamChunk, error) {
	ch := make(chan llm.StreamChunk, 1)
	result, err := m.Generate(ctx, prompt, opts)
	if err != nil {
		ch <- llm.StreamChunk{Error: err}
	} else {
		ch <- llm.StreamChunk{Content: result, Done: true}
	}
	close(ch)
	return ch, err
}

func (m *MockLLMProvider) Name() string {
	return "Mock"
}

func (m *MockLLMProvider) Available() bool {
	return true
}

func (m *MockLLMProvider) MaxTokens() int {
	return 4096
}
