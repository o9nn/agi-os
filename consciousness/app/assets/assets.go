package assets
import (
	"embed"
	"io/fs"
)
var icons embed.FS
func ListIcons() ([]string, error) {
	return fs.Glob(icons, "*")
}
func GetIcon(filename string) ([]byte, error) {
	return icons.ReadFile(filename)
}