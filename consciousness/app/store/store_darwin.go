package store
import (
"os"
"path/filepath"
)
func getStorePath() string {
home := os.Getenv("HOME")
return filepath.Join(home, "Library", "Application Support", "Ollama", "config.json")
}