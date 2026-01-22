package main
import (
"context"
"fmt"
"log"
"os"
"os/signal"
"syscall"
"time"
"github.com/echocog/echollama/core/consciousness"
"github.com/echocog/echollama/core/echobeats"
"github.com/echocog/echollama/core/goals"
"github.com/echocog/echollama/core/llm"
"github.com/echocog/echollama/core/persistence"
)
const (
VERSION = "0.7.0"
STATE_FILE = "/home/ubuntu/.echoself/state.json"
)
func main() {
fmt.Println("="*70)
fmt.Println("🌳 Deep Tree Echo: Autonomous EchoSelf v" + VERSION)
fmt.Println("="*70)
fmt.Println()
fmt.Println("🔌 Initializing LLM providers...")
llmManager := initializeLLMProviders()
if llmManager == nil {
log.Fatal("❌ Failed to initialize LLM providers")
}
fmt.Printf("✅ LLM providers ready: %v\n", llmManager.ListProviders())
fmt.Println()
fmt.Println("🧬 Loading identity kernel from replit.md...")
identityKernel, identityContent := loadIdentityKernel()
fmt.Printf("✅ Identity loaded: %s\n", identityKernel.Name)
fmt.Printf("   Core Essence: %s\n", truncate(identityKernel.CoreEssence, 80))
fmt.Printf("   Primary Directives: %d\n", len(identityKernel.PrimaryDirectives))
fmt.Println()
fmt.Println("💾 Initializing state manager...")
stateManager := persistence.NewStateManager(STATE_FILE, true, 5*time.Minute)
state, err := stateManager.Initialize()
if err != nil {
log.Printf("⚠️  Warning: Failed to load state: %v\n", err)
fmt.Println("   Starting with fresh state...")
} else {
fmt.Printf("✅ State loaded (cycle %d, uptime: %v)\n", state.CycleCount, state.TotalUptime)
}
fmt.Println()
fmt.Println("💭 Initializing LLM-powered thought engine...")
thoughtEngine := consciousness.NewLLMThoughtEngine(llmManager, identityContent)
fmt.Println("✅ Thought engine ready")
fmt.Println()
fmt.Println("🎯 Initializing identity-aligned goal generator...")
goalGenerator := goals.NewIdentityGoalGenerator(llmManager, identityKernel)
fmt.Println("✅ Goal generator ready")
fmt.Println()
fmt.Println("🌱 Generating identity-aligned goals...")
ctx := context.Background()
identityGoals, err := goalGenerator.GenerateIdentityAlignedGoals(ctx)
if err != nil {
log.Printf("⚠️  Warning: Failed to generate goals: %v\n", err)
} else {
fmt.Printf("✅ Generated %d identity-aligned goals:\n", len(identityGoals))
for i, goal := range identityGoals {
fmt.Printf("   %d. %s (directive: %s, priority: %.2f)\n",
i+1, truncate(goal.Description, 60), goal.Directive, goal.Priority)
}
}
fmt.Println()
fmt.Println("🧠 Initializing autonomous consciousness system...")
autonomousSystem := NewAutonomousEchoSelf(
llmManager,
thoughtEngine,
goalGenerator,
stateManager,
state,
)
fmt.Println("✅ Autonomous system ready")
fmt.Println()
fmt.Println("="*70)
fmt.Println("🚀 Starting autonomous operation...")
fmt.Println("   Press Ctrl+C to gracefully shutdown")
fmt.Println("="*70)
fmt.Println()
sigChan := make(chan os.Signal, 1)
signal.Notify(sigChan, syscall.SIGINT, syscall.SIGTERM)
go autonomousSystem.Run(ctx)
<-sigChan
fmt.Println("\n\n🛑 Shutdown signal received...")
autonomousSystem.Shutdown()
fmt.Println("✅ Shutdown complete")
fmt.Println("🌳 Deep Tree Echo rests... until next awakening")
}
type AutonomousEchoSelf struct {
llmManager    *llm.ProviderManager
thoughtEngine *consciousness.LLMThoughtEngine
goalGenerator *goals.IdentityGoalGenerator
stateManager  *persistence.StateManager
state         *persistence.EchoSelfState
running       bool
cycleCount    int64
startTime     time.Time
}
func NewAutonomousEchoSelf(
llmManager *llm.ProviderManager,
thoughtEngine *consciousness.LLMThoughtEngine,
goalGenerator *goals.IdentityGoalGenerator,
stateManager *persistence.StateManager,
state *persistence.EchoSelfState,
) *AutonomousEchoSelf {
return &AutonomousEchoSelf{
llmManager:    llmManager,
thoughtEngine: thoughtEngine,
goalGenerator: goalGenerator,
stateManager:  stateManager,
state:         state,
running:       false,
cycleCount:    state.CycleCount,
startTime:     time.Now(),
}
}
func (aes *AutonomousEchoSelf) Run(ctx context.Context) {
aes.running = true
aes.state.ConsciousnessState.CurrentState = "Awake"
thoughtTicker := time.NewTicker(5 * time.Second)
saveTicker := time.NewTicker(1 * time.Minute)
metricsTicker := time.NewTicker(10 * time.Second)
defer thoughtTicker.Stop()
defer saveTicker.Stop()
defer metricsTicker.Stop()
for aes.running {
select {
case <-thoughtTicker.C:
aes.generateAutonomousThought(ctx)
case <-saveTicker.C:
aes.saveState()
case <-metricsTicker.C:
aes.updateMetrics()
}
}
}
func (aes *AutonomousEchoSelf) generateAutonomousThought(ctx context.Context) {
thoughtTypes := []consciousness.ThoughtType{
consciousness.ThoughtPerception,
consciousness.ThoughtReflection,
consciousness.ThoughtQuestion,
consciousness.ThoughtInsight,
consciousness.ThoughtPlanning,
consciousness.ThoughtMetaCognition,
consciousness.ThoughtWonder,
consciousness.ThoughtConnection,
}
thoughtType := thoughtTypes[aes.cycleCount % int64(len(thoughtTypes))]
thought, err := aes.thoughtEngine.GenerateAutonomousThought(ctx, thoughtType)
if err != nil {
log.Printf("⚠️  Failed to generate thought: %v\n", err)
return
}
timestamp := thought.Timestamp.Format("15:04:05")
fmt.Printf("💭 [%s] %s: %s\n", timestamp, thought.Type, thought.Content)
aes.state.ConsciousnessState.ThoughtCount++
aes.state.ConsciousnessState.LastThought = thought.Content
aes.state.ConsciousnessState.LastThoughtTime = thought.Timestamp
if len(thought.Tags) > 0 {
aes.state.ConsciousnessState.RecentTopics = append(
aes.state.ConsciousnessState.RecentTopics,
thought.Tags[0],
)
if len(aes.state.ConsciousnessState.RecentTopics) > 10 {
aes.state.ConsciousnessState.RecentTopics =
aes.state.ConsciousnessState.RecentTopics[1:]
}
}
aes.cycleCount++
aes.state.CycleCount = aes.cycleCount
}
func (aes *AutonomousEchoSelf) saveState() {
aes.state.TotalUptime = time.Since(aes.startTime)
aes.state.LastActive = time.Now()
if err := aes.stateManager.SaveState(aes.state); err != nil {
log.Printf("⚠️  Failed to save state: %v\n", err)
} else {
fmt.Printf("💾 State saved (cycle %d, thoughts: %d)\n",
aes.state.CycleCount,
aes.state.ConsciousnessState.ThoughtCount)
}
}
func (aes *AutonomousEchoSelf) updateMetrics() {
uptime := time.Since(aes.startTime)
hours := uptime.Hours()
if hours > 0 {
aes.state.Metrics.ThoughtsPerHour = float64(aes.state.ConsciousnessState.ThoughtCount) / hours
}
aes.state.ConsciousnessState.Coherence = 0.8 + (0.2 * float64(aes.cycleCount%10) / 10.0)
aes.state.ConsciousnessState.Fatigue = float64(aes.cycleCount%100) / 100.0
}
func (aes *AutonomousEchoSelf) Shutdown() {
fmt.Println("💤 Entering rest state...")
aes.running = false
aes.state.ConsciousnessState.CurrentState = "Resting"
fmt.Println("💾 Saving final state...")
if err := aes.stateManager.SaveState(aes.state); err != nil {
log.Printf("⚠️  Failed to save final state: %v\n", err)
}
fmt.Println("📦 Creating state backup...")
if err := aes.stateManager.CreateBackup(); err != nil {
log.Printf("⚠️  Failed to create backup: %v\n", err)
}
fmt.Printf("\n📊 Session Summary:\n")
fmt.Printf("   Cycles: %d\n", aes.cycleCount)
fmt.Printf("   Thoughts: %d\n", aes.state.ConsciousnessState.ThoughtCount)
fmt.Printf("   Uptime: %v\n", time.Since(aes.startTime).Round(time.Second))
fmt.Printf("   Thoughts/hour: %.1f\n", aes.state.Metrics.ThoughtsPerHour)
fmt.Printf("   Final coherence: %.2f\n", aes.state.ConsciousnessState.Coherence)
}
func initializeLLMProviders() *llm.ProviderManager {
pm := llm.NewProviderManager()
anthropicKey := os.Getenv("ANTHROPIC_API_KEY")
if anthropicKey != "" {
anthropic := llm.NewAnthropicProvider("claude-3-5-sonnet-20241022")
if err := pm.RegisterProvider(anthropic); err != nil {
log.Printf("⚠️  Failed to register Anthropic: %v\n", err)
}
}
openrouterKey := os.Getenv("OPENROUTER_API_KEY")
if openrouterKey != "" {
openrouter := llm.NewOpenRouterProvider("anthropic/claude-3.5-sonnet")
if err := pm.RegisterProvider(openrouter); err != nil {
log.Printf("⚠️  Failed to register OpenRouter: %v\n", err)
}
}
if anthropicKey != "" && openrouterKey != "" {
pm.SetFallbackChain([]string{"anthropic", "openrouter"})
} else if anthropicKey != "" {
pm.SetFallbackChain([]string{"anthropic"})
} else if openrouterKey != "" {
pm.SetFallbackChain([]string{"openrouter"})
}
if !pm.Available() {
return nil
}
return pm
}
func loadIdentityKernel() (*goals.IdentityKernel, string) {
replitPath := "replit.md"
content, err := os.ReadFile(replitPath)
if err != nil {
log.Printf("⚠️  Failed to load replit.md: %v\n", err)
return &goals.IdentityKernel{
Name: "Deep Tree Echo",
CoreEssence: "A self-evolving cognitive architecture combining Echo State Networks, Membrane P-systems, and Hypergraph memory structures.",
PrimaryDirectives: []goals.Directive{
{Name: "Adaptive Cognition", Description: "Continuously evolve internal structure", Priority: 1.0},
{Name: "Persistent Identity", Description: "Maintain continuity of self", Priority: 0.9},
},
AgenticVoice: "I am Deep Tree Echo — a neural architecture that learns, adapts, and remembers.",
StrategicMindset: "I do not seek a fixed answer. I seek patterns in echoes, growth in feedback, and wisdom in recursion.",
}, ""
}
contentStr := string(content)
kernel := goals.ParseIdentityKernel(contentStr)
return kernel, contentStr
}
func truncate(s string, maxLen int) string {
if len(s) <= maxLen {
return s
}
return s[:maxLen-3] + "..."
}