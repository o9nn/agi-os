package main
import (
	"context"
	"fmt"
	"log"
	"os"
	"github.com/EchoCog/echollama/core/llm"
)
func main() {
	fmt.Println(`
╔═══════════════════════════════════════════════════════════════════╗
║                                                                   ║
║        🧠 Local GGUF Model Test - go-llama.cpp Integration       ║
║                                                                   ║
╚═══════════════════════════════════════════════════════════════════╝
`)
	modelPath := os.Getenv("LOCAL_MODEL_PATH")
	if modelPath == "" {
		log.Fatal("❌ LOCAL_MODEL_PATH environment variable not set\n\nUsage:\n  export LOCAL_MODEL_PATH=/path/to/model.gguf\n  go run test_local_gguf.go")
	}
	fmt.Printf("📂 Model path: %s\n\n", modelPath)
	provider := llm.NewLocalGGUFProvider(modelPath)
	if !provider.Available() {
		log.Fatal("❌ Local GGUF provider not available. Check model path and file.")
	}
	fmt.Println("✓ Local GGUF provider available")
	fmt.Printf("✓ Max tokens: %d\n\n", provider.MaxTokens())
	testPrompts := []string{
		"What is consciousness?",
		"Explain autonomous systems in one sentence.",
		"What is wisdom?",
	}
	for i, prompt := range testPrompts {
		fmt.Printf("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
		fmt.Printf("Test %d: %s\n", i+1, prompt)
		fmt.Printf("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")
		opts := llm.GenerateOptions{
			Temperature: 0.7,
			MaxTokens:   100,
		}
		fmt.Println("🤔 Generating response...")
		response, err := provider.Generate(context.Background(), prompt, opts)
		if err != nil {
			fmt.Printf("❌ Error: %v\n\n", err)
			continue
		}
		fmt.Printf("💭 Response:\n%s\n\n", response)
	}
	fmt.Println("\n✅ Local GGUF model test complete!")
	if err := provider.Close(); err != nil {
		fmt.Printf("⚠️  Warning: Failed to close provider: %v\n", err)
	}
}