package echoself
import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"sync"
)
type RepositoryIntrospector struct {
	mu                sync.RWMutex
	rootPath          string
	attentionThreshold float64
	fileNodes         map[string]*FileNode
	totalFiles        int
	scannedFiles      int
}
type FileNode struct {
	Path            string
	Type            string
	Content         string
	SalienceScore   float64
	LastModified    int64
	Size            int64
	Links           []string
	Metadata        map[string]interface{}
}
func NewRepositoryIntrospector(rootPath string, attentionThreshold float64) *RepositoryIntrospector {
	return &RepositoryIntrospector{
		rootPath:           rootPath,
		attentionThreshold: attentionThreshold,
		fileNodes:          make(map[string]*FileNode),
	}
}
func (ri *RepositoryIntrospector) Scan() error {
	ri.mu.Lock()
	defer ri.mu.Unlock()
	ri.fileNodes = make(map[string]*FileNode)
	ri.totalFiles = 0
	ri.scannedFiles = 0
	err := filepath.Walk(ri.rootPath, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return nil 
		}
		if info.IsDir() || strings.HasPrefix(info.Name(), ".") {
			return nil
		}
		if !ri.isCodeFile(path) {
			return nil
		}
		ri.totalFiles++
		salience := ri.calculateSalience(path, info)
		if salience < ri.attentionThreshold {
			return nil
		}
		node := &FileNode{
			Path:          path,
			Type:          ri.getFileType(path),
			SalienceScore: salience,
			LastModified:  info.ModTime().Unix(),
			Size:          info.Size(),
			Links:         make([]string, 0),
			Metadata:      make(map[string]interface{}),
		}
		if info.Size() < 100000 { 
			content, err := os.ReadFile(path)
			if err == nil {
				node.Content = string(content)
			}
		} else {
			node.Content = "[File too large - content omitted]"
		}
		ri.fileNodes[path] = node
		ri.scannedFiles++
		return nil
	})
	return err
}
func (ri *RepositoryIntrospector) calculateSalience(path string, info os.FileInfo) float64 {
	score := 0.5 
	if strings.Contains(path, "/core/") {
		score += 0.3
	}
	if strings.Contains(path, "/autonomous") {
		score += 0.2
	}
	if strings.Contains(path, "/echoself") {
		score += 0.2
	}
	if strings.Contains(path, "/deeptreeecho") {
		score += 0.2
	}
	if strings.Contains(path, "/consciousness") {
		score += 0.2
	}
	if strings.Contains(path, "/echobeats") {
		score += 0.15
	}
	if strings.Contains(path, "/echodream") {
		score += 0.15
	}
	if strings.Contains(path, "/goals") {
		score += 0.15
	}
	if strings.Contains(path, "README") {
		score += 0.3
	}
	if strings.Contains(path, "autonomous_echoself") {
		score += 0.4
	}
	if strings.Contains(path, "types.go") {
		score += 0.1
	}
	if strings.HasSuffix(path, ".bak") || strings.HasSuffix(path, ".wip") || strings.HasSuffix(path, ".backup") {
		score -= 0.5
	}
	if strings.Contains(path, "_test.go") || strings.HasPrefix(filepath.Base(path), "test_") {
		score -= 0.2
	}
	if score > 1.0 {
		score = 1.0
	}
	return score
}
func (ri *RepositoryIntrospector) isCodeFile(path string) bool {
	ext := strings.ToLower(filepath.Ext(path))
	codeExtensions := []string{".go", ".md", ".py", ".js", ".ts", ".scm", ".lisp"}
	for _, codeExt := range codeExtensions {
		if ext == codeExt {
			return true
		}
	}
	return false
}
func (ri *RepositoryIntrospector) getFileType(path string) string {
	ext := strings.ToLower(filepath.Ext(path))
	typeMap := map[string]string{
		".go":   "code",
		".py":   "code",
		".js":   "code",
		".ts":   "code",
		".scm":  "scheme",
		".lisp": "lisp",
		".md":   "documentation",
	}
	if fileType, ok := typeMap[ext]; ok {
		return fileType
	}
	return "unknown"
}
func (ri *RepositoryIntrospector) GetHighSalienceFiles() []*FileNode {
	ri.mu.RLock()
	defer ri.mu.RUnlock()
	files := make([]*FileNode, 0, len(ri.fileNodes))
	for _, node := range ri.fileNodes {
		files = append(files, node)
	}
	return files
}
func (ri *RepositoryIntrospector) GetFileNode(path string) (*FileNode, bool) {
	ri.mu.RLock()
	defer ri.mu.RUnlock()
	node, ok := ri.fileNodes[path]
	return node, ok
}
func (ri *RepositoryIntrospector) GetStats() map[string]interface{} {
	ri.mu.RLock()
	defer ri.mu.RUnlock()
	return map[string]interface{}{
		"total_files":   ri.totalFiles,
		"scanned_files": ri.scannedFiles,
		"attention_threshold": ri.attentionThreshold,
		"root_path": ri.rootPath,
	}
}
func (ri *RepositoryIntrospector) GenerateHypergraphSummary() string {
	ri.mu.RLock()
	defer ri.mu.RUnlock()
	var sb strings.Builder
	sb.WriteString("Repository Hypergraph Summary\n")
	sb.WriteString(strings.Repeat("=", 50))
	sb.WriteString("\n\n")
	sb.WriteString(fmt.Sprintf("Total Files: %d\n", ri.totalFiles))
	sb.WriteString(fmt.Sprintf("High-Salience Files: %d\n", ri.scannedFiles))
	sb.WriteString(fmt.Sprintf("Attention Threshold: %.2f\n\n", ri.attentionThreshold))
	typeGroups := make(map[string][]*FileNode)
	for _, node := range ri.fileNodes {
		typeGroups[node.Type] = append(typeGroups[node.Type], node)
	}
	for fileType, nodes := range typeGroups {
		sb.WriteString(fmt.Sprintf("%s Files (%d):\n", strings.Title(fileType), len(nodes)))
		for _, node := range nodes {
			relPath := strings.TrimPrefix(node.Path, ri.rootPath+"/")
			sb.WriteString(fmt.Sprintf("  - %s (salience: %.2f)\n", relPath, node.SalienceScore))
		}
		sb.WriteString("\n")
	}
	return sb.String()
}
func (ri *RepositoryIntrospector) AdaptiveAttentionAllocation(cognitiveLoad float64, recentActivity float64) float64 {
	threshold := 0.5 + (cognitiveLoad * 0.3) - (recentActivity * 0.2)
	if threshold < 0.3 {
		threshold = 0.3
	}
	if threshold > 0.9 {
		threshold = 0.9
	}
	return threshold
}
func (ri *RepositoryIntrospector) UpdateAttentionThreshold(newThreshold float64) {
	ri.mu.Lock()
	defer ri.mu.Unlock()
	ri.attentionThreshold = newThreshold
}