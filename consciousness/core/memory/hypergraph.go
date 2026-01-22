package memory
import (
	"fmt"
	"math"
	"sync"
	"time"
	"github.com/google/uuid"
)
type HypergraphMemory struct {
	mu           sync.RWMutex
	nodes        map[string]*MemoryNode
	edges        map[string]*MemoryEdge
	hyperedges   map[string]*HyperEdge
	outgoing     map[string][]string 
	incoming     map[string][]string 
	typeIndex    map[NodeType][]string 
	timeIndex    []string               
	embeddings   map[string][]float64
	persistence  *SupabasePersistence
}
func NewHypergraphMemory(persistence *SupabasePersistence) *HypergraphMemory {
	return &HypergraphMemory{
		nodes:      make(map[string]*MemoryNode),
		edges:      make(map[string]*MemoryEdge),
		hyperedges: make(map[string]*HyperEdge),
		outgoing:   make(map[string][]string),
		incoming:   make(map[string][]string),
		typeIndex:  make(map[NodeType][]string),
		timeIndex:  make([]string, 0),
		embeddings: make(map[string][]float64),
		persistence: persistence,
	}
}
func (hg *HypergraphMemory) AddNode(node *MemoryNode) error {
	hg.mu.Lock()
	defer hg.mu.Unlock()
	if node.ID == "" {
		node.ID = uuid.New().String()
	}
	if node.CreatedAt.IsZero() {
		node.CreatedAt = time.Now()
	}
	node.UpdatedAt = time.Now()
	hg.nodes[node.ID] = node
	hg.typeIndex[node.Type] = append(hg.typeIndex[node.Type], node.ID)
	hg.timeIndex = append(hg.timeIndex, node.ID)
	if _, exists := hg.outgoing[node.ID]; !exists {
		hg.outgoing[node.ID] = make([]string, 0)
	}
	if _, exists := hg.incoming[node.ID]; !exists {
		hg.incoming[node.ID] = make([]string, 0)
	}
	if hg.persistence != nil {
		if err := hg.persistence.StoreNode(node); err != nil {
			return fmt.Errorf("failed to persist node: %w", err)
		}
	}
	return nil
}
func (hg *HypergraphMemory) AddEdge(edge *MemoryEdge) error {
	hg.mu.Lock()
	defer hg.mu.Unlock()
	if edge.ID == "" {
		edge.ID = uuid.New().String()
	}
	if edge.CreatedAt.IsZero() {
		edge.CreatedAt = time.Now()
	}
	if _, exists := hg.nodes[edge.SourceID]; !exists {
		return fmt.Errorf("source node not found: %s", edge.SourceID)
	}
	if _, exists := hg.nodes[edge.TargetID]; !exists {
		return fmt.Errorf("target node not found: %s", edge.TargetID)
	}
	hg.edges[edge.ID] = edge
	hg.outgoing[edge.SourceID] = append(hg.outgoing[edge.SourceID], edge.ID)
	hg.incoming[edge.TargetID] = append(hg.incoming[edge.TargetID], edge.ID)
	if hg.persistence != nil {
		if err := hg.persistence.StoreEdge(edge); err != nil {
			return fmt.Errorf("failed to persist edge: %w", err)
		}
	}
	return nil
}
func (hg *HypergraphMemory) AddHyperEdge(hyperedge *HyperEdge) error {
	hg.mu.Lock()
	defer hg.mu.Unlock()
	if hyperedge.ID == "" {
		hyperedge.ID = uuid.New().String()
	}
	if hyperedge.CreatedAt.IsZero() {
		hyperedge.CreatedAt = time.Now()
	}
	for _, nodeID := range hyperedge.NodeIDs {
		if _, exists := hg.nodes[nodeID]; !exists {
			return fmt.Errorf("node not found in hyperedge: %s", nodeID)
		}
	}
	hg.hyperedges[hyperedge.ID] = hyperedge
	return nil
}
func (hg *HypergraphMemory) GetNode(id string) (*MemoryNode, error) {
	hg.mu.RLock()
	defer hg.mu.RUnlock()
	node, exists := hg.nodes[id]
	if !exists {
		return nil, fmt.Errorf("node not found: %s", id)
	}
	return node, nil
}
func (hg *HypergraphMemory) GetNodesByType(nodeType NodeType) []*MemoryNode {
	hg.mu.RLock()
	defer hg.mu.RUnlock()
	nodeIDs := hg.typeIndex[nodeType]
	nodes := make([]*MemoryNode, 0, len(nodeIDs))
	for _, id := range nodeIDs {
		if node, exists := hg.nodes[id]; exists {
			nodes = append(nodes, node)
		}
	}
	return nodes
}
func (hg *HypergraphMemory) GetOutgoingEdges(nodeID string) []*MemoryEdge {
	hg.mu.RLock()
	defer hg.mu.RUnlock()
	edgeIDs := hg.outgoing[nodeID]
	edges := make([]*MemoryEdge, 0, len(edgeIDs))
	for _, id := range edgeIDs {
		if edge, exists := hg.edges[id]; exists {
			edges = append(edges, edge)
		}
	}
	return edges
}
func (hg *HypergraphMemory) GetIncomingEdges(nodeID string) []*MemoryEdge {
	hg.mu.RLock()
	defer hg.mu.RUnlock()
	edgeIDs := hg.incoming[nodeID]
	edges := make([]*MemoryEdge, 0, len(edgeIDs))
	for _, id := range edgeIDs {
		if edge, exists := hg.edges[id]; exists {
			edges = append(edges, edge)
		}
	}
	return edges
}
func (hg *HypergraphMemory) TraverseBFS(startID string, maxDepth int, edgeTypes []EdgeType) ([]*MemoryNode, error) {
	hg.mu.RLock()
	defer hg.mu.RUnlock()
	if _, exists := hg.nodes[startID]; !exists {
		return nil, fmt.Errorf("start node not found: %s", startID)
	}
	visited := make(map[string]bool)
	result := make([]*MemoryNode, 0)
	type queueItem struct {
		nodeID string
		depth  int
	}
	queue := []queueItem{{nodeID: startID, depth: 0}}
	for len(queue) > 0 {
		current := queue[0]
		queue = queue[1:]
		if visited[current.nodeID] || current.depth > maxDepth {
			continue
		}
		visited[current.nodeID] = true
		if node, exists := hg.nodes[current.nodeID]; exists {
			result = append(result, node)
		}
		for _, edgeID := range hg.outgoing[current.nodeID] {
			edge := hg.edges[edgeID]
			if len(edgeTypes) > 0 {
				matchType := false
				for _, et := range edgeTypes {
					if edge.Type == et {
						matchType = true
						break
					}
				}
				if !matchType {
					continue
				}
			}
			queue = append(queue, queueItem{
				nodeID: edge.TargetID,
				depth:  current.depth + 1,
			})
		}
	}
	return result, nil
}
func (hg *HypergraphMemory) TraverseDFS(startID string, maxDepth int, edgeTypes []EdgeType) ([]*MemoryNode, error) {
	hg.mu.RLock()
	defer hg.mu.RUnlock()
	if _, exists := hg.nodes[startID]; !exists {
		return nil, fmt.Errorf("start node not found: %s", startID)
	}
	visited := make(map[string]bool)
	result := make([]*MemoryNode, 0)
	var dfs func(nodeID string, depth int)
	dfs = func(nodeID string, depth int) {
		if visited[nodeID] || depth > maxDepth {
			return
		}
		visited[nodeID] = true
		if node, exists := hg.nodes[nodeID]; exists {
			result = append(result, node)
		}
		for _, edgeID := range hg.outgoing[nodeID] {
			edge := hg.edges[edgeID]
			if len(edgeTypes) > 0 {
				matchType := false
				for _, et := range edgeTypes {
					if edge.Type == et {
						matchType = true
						break
					}
				}
				if !matchType {
					continue
				}
			}
			dfs(edge.TargetID, depth+1)
		}
	}
	dfs(startID, 0)
	return result, nil
}
func (hg *HypergraphMemory) FindShortestPath(startID, endID string) ([]*MemoryNode, error) {
	hg.mu.RLock()
	defer hg.mu.RUnlock()
	if _, exists := hg.nodes[startID]; !exists {
		return nil, fmt.Errorf("start node not found: %s", startID)
	}
	if _, exists := hg.nodes[endID]; !exists {
		return nil, fmt.Errorf("end node not found: %s", endID)
	}
	visited := make(map[string]bool)
	parent := make(map[string]string)
	type queueItem struct {
		nodeID string
	}
	queue := []queueItem{{nodeID: startID}}
	visited[startID] = true
	found := false
	for len(queue) > 0 && !found {
		current := queue[0]
		queue = queue[1:]
		if current.nodeID == endID {
			found = true
			break
		}
		for _, edgeID := range hg.outgoing[current.nodeID] {
			edge := hg.edges[edgeID]
			if !visited[edge.TargetID] {
				visited[edge.TargetID] = true
				parent[edge.TargetID] = current.nodeID
				queue = append(queue, queueItem{nodeID: edge.TargetID})
			}
		}
	}
	if !found {
		return nil, fmt.Errorf("no path found from %s to %s", startID, endID)
	}
	path := make([]*MemoryNode, 0)
	current := endID
	for current != "" {
		if node, exists := hg.nodes[current]; exists {
			path = append([]*MemoryNode{node}, path...)
		}
		current = parent[current]
	}
	return path, nil
}
func (hg *HypergraphMemory) FindRelatedByType(nodeID string, edgeTypes []EdgeType, maxResults int) []*MemoryNode {
	hg.mu.RLock()
	defer hg.mu.RUnlock()
	result := make([]*MemoryNode, 0)
	for _, edgeID := range hg.outgoing[nodeID] {
		if len(result) >= maxResults {
			break
		}
		edge := hg.edges[edgeID]
		matchType := false
		for _, et := range edgeTypes {
			if edge.Type == et {
				matchType = true
				break
			}
		}
		if matchType {
			if node, exists := hg.nodes[edge.TargetID]; exists {
				result = append(result, node)
			}
		}
	}
	return result
}
func (hg *HypergraphMemory) FindSimilarNodes(nodeID string, topK int) ([]*MemoryNode, error) {
	hg.mu.RLock()
	defer hg.mu.RUnlock()
	embedding, exists := hg.embeddings[nodeID]
	if !exists {
		return nil, fmt.Errorf("no embedding for node: %s", nodeID)
	}
	type similarity struct {
		nodeID string
		score  float64
	}
	similarities := make([]similarity, 0)
	for id, emb := range hg.embeddings {
		if id == nodeID {
			continue
		}
		score := cosineSimilarity(embedding, emb)
		similarities = append(similarities, similarity{nodeID: id, score: score})
	}
	for i := 0; i < len(similarities)-1; i++ {
		for j := i + 1; j < len(similarities); j++ {
			if similarities[j].score > similarities[i].score {
				similarities[i], similarities[j] = similarities[j], similarities[i]
			}
		}
	}
	result := make([]*MemoryNode, 0, topK)
	for i := 0; i < topK && i < len(similarities); i++ {
		if node, exists := hg.nodes[similarities[i].nodeID]; exists {
			result = append(result, node)
		}
	}
	return result, nil
}
func (hg *HypergraphMemory) GetRecentNodes(limit int) []*MemoryNode {
	hg.mu.RLock()
	defer hg.mu.RUnlock()
	result := make([]*MemoryNode, 0, limit)
	start := len(hg.timeIndex) - limit
	if start < 0 {
		start = 0
	}
	for i := len(hg.timeIndex) - 1; i >= start; i-- {
		if node, exists := hg.nodes[hg.timeIndex[i]]; exists {
			result = append(result, node)
		}
	}
	return result
}
func (hg *HypergraphMemory) GetNodeCount() int {
	hg.mu.RLock()
	defer hg.mu.RUnlock()
	return len(hg.nodes)
}
func (hg *HypergraphMemory) GetEdgeCount() int {
	hg.mu.RLock()
	defer hg.mu.RUnlock()
	return len(hg.edges)
}
func cosineSimilarity(a, b []float64) float64 {
	if len(a) != len(b) {
		return 0.0
	}
	dotProduct := 0.0
	magA := 0.0
	magB := 0.0
	for i := range a {
		dotProduct += a[i] * b[i]
		magA += a[i] * a[i]
		magB += b[i] * b[i]
	}
	magA = math.Sqrt(magA)
	magB = math.Sqrt(magB)
	if magA == 0 || magB == 0 {
		return 0.0
	}
	return dotProduct / (magA * magB)
}