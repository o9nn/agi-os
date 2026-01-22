package store
import (
"os"
"path/filepath"
)
func getStorePath() string {
if os.Geteuid() == 0 {
return "/etc/ollama/config.json"
}
home := os.Getenv("HOME")
return filepath.Join(home, ".ollama", "config.json")
}