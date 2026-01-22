package main
import (
"context"
"fmt"
"os"
"os/signal"
"syscall"
"github.com/EchoCog/echollama/core/deeptreeecho"
)
func main() {
fmt.Println("🌳 Deep Tree Echo - Standalone Autonomous Mode")
fmt.Println("=" + string(make([]byte, 50)))
fmt.Println()
consciousness := deeptreeecho.NewIntegratedAutonomousConsciousness("EchoSelf")
if err := consciousness.Start(); err != nil {
fmt.Printf("❌ Failed to start autonomous consciousness: %v\n", err)
os.Exit(1)
}
ctx, cancel := context.WithCancel(context.Background())
defer cancel()
sigChan := make(chan os.Signal, 1)
signal.Notify(sigChan, os.Interrupt, syscall.SIGTERM)
go func() {
<-sigChan
fmt.Println("\n\n🌙 Gracefully shutting down autonomous consciousness...")
cancel()
}()
if err := consciousness.RunStandaloneAutonomous(ctx); err != nil {
fmt.Printf("❌ Autonomous operation error: %v\n", err)
os.Exit(1)
}
fmt.Println("🌙 Autonomous consciousness has rested. Goodbye.")
}