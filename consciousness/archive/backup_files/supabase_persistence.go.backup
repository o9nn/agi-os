package memory

import (
	"context"
	"encoding/json"
	"fmt"
	"time"

	"github.com/google/uuid"
)

// SupabasePersistence implements full persistence operations for Deep Tree Echo
type SupabasePersistence struct {
	client *SupabaseClient
	ctx    context.Context
}

// NewSupabasePersistence creates a new Supabase persistence layer
func NewSupabasePersistence(ctx context.Context, url, key string) (*SupabasePersistence, error) {
	client := NewSupabaseClient(url, key)
	
	sp := &SupabasePersistence{
		client: client,
		ctx:    ctx,
	}
	
	// Initialize schema
	if err := sp.initializeSchema(); err != nil {
		return nil, fmt.Errorf("failed to initialize schema: %w", err)
	}
	
	return sp, nil
}

// initializeSchema ensures all required tables exist
func (sp *SupabasePersistence) initializeSchema() error {
	// Tables are created via Supabase migrations
	// This function validates connectivity
	_, err := sp.client.Query("memory_nodes", map[string]interface{}{}, 1)
	if err != nil {
		return fmt.Errorf("schema validation failed: %w", err)
	}
	return nil
}

// StoreNode stores a memory node in the hypergraph
func (sp *SupabasePersistence) StoreNode(node *MemoryNode) error {
	if node.ID == "" {
		node.ID = uuid.New().String()
	}
	if node.CreatedAt.IsZero() {
		node.CreatedAt = time.Now()
	}
	node.UpdatedAt = time.Now()
	
	return sp.client.Insert("memory_nodes", node)
}

// RetrieveNode retrieves a memory node by ID
func (sp *SupabasePersistence) RetrieveNode(id string) (*MemoryNode, error) {
	results, err := sp.client.Query("memory_nodes", map[string]interface{}{
		"id": id,
	}, 1)
	if err != nil {
		return nil, err
	}
	
	if len(results) == 0 {
		return nil, fmt.Errorf("node not found: %s", id)
	}
	
	var node MemoryNode
	data, err := json.Marshal(results[0])
	if err != nil {
		return nil, err
	}
	
	if err := json.Unmarshal(data, &node); err != nil {
		return nil, err
	}
	
	return &node, nil
}

// QueryNodesByType retrieves nodes of a specific type
func (sp *SupabasePersistence) QueryNodesByType(nodeType NodeType, limit int) ([]*MemoryNode, error) {
	results, err := sp.client.Query("memory_nodes", map[string]interface{}{
		"type": string(nodeType),
	}, limit)
	if err != nil {
		return nil, err
	}
	
	nodes := make([]*MemoryNode, 0, len(results))
	for _, result := range results {
		var node MemoryNode
		data, err := json.Marshal(result)
		if err != nil {
			continue
		}
		if err := json.Unmarshal(data, &node); err != nil {
			continue
		}
		nodes = append(nodes, &node)
	}
	
	return nodes, nil
}

// StoreEdge stores a memory edge in the hypergraph
func (sp *SupabasePersistence) StoreEdge(edge *MemoryEdge) error {
	if edge.ID == "" {
		edge.ID = uuid.New().String()
	}
	if edge.CreatedAt.IsZero() {
		edge.CreatedAt = time.Now()
	}
	
	return sp.client.Insert("memory_edges", edge)
}

// QueryEdgesFromNode retrieves all edges originating from a node
func (sp *SupabasePersistence) QueryEdgesFromNode(nodeID string) ([]*MemoryEdge, error) {
	results, err := sp.client.Query("memory_edges", map[string]interface{}{
		"source_id": nodeID,
	}, 100)
	if err != nil {
		return nil, err
	}
	
	edges := make([]*MemoryEdge, 0, len(results))
	for _, result := range results {
		var edge MemoryEdge
		data, err := json.Marshal(result)
		if err != nil {
			continue
		}
		if err := json.Unmarshal(data, &edge); err != nil {
			continue
		}
		edges = append(edges, &edge)
	}
	
	return edges, nil
}

// QueryEdgesToNode retrieves all edges pointing to a node
func (sp *SupabasePersistence) QueryEdgesToNode(nodeID string) ([]*MemoryEdge, error) {
	results, err := sp.client.Query("memory_edges", map[string]interface{}{
		"target_id": nodeID,
	}, 100)
	if err != nil {
		return nil, err
	}
	
	edges := make([]*MemoryEdge, 0, len(results))
	for _, result := range results {
		var edge MemoryEdge
		data, err := json.Marshal(result)
		if err != nil {
			continue
		}
		if err := json.Unmarshal(data, &edge); err != nil {
			continue
		}
		edges = append(edges, &edge)
	}
	
	return edges, nil
}

// StoreEpisode stores an episodic memory
func (sp *SupabasePersistence) StoreEpisode(episode *Episode) error {
	if episode.ID == "" {
		episode.ID = uuid.New().String()
	}
	if episode.Timestamp.IsZero() {
		episode.Timestamp = time.Now()
	}
	
	return sp.client.Insert("episodes", episode)
}

// QueryRecentEpisodes retrieves recent episodic memories
func (sp *SupabasePersistence) QueryRecentEpisodes(limit int) ([]*Episode, error) {
	// Note: This uses basic query - in production would use order by timestamp desc
	results, err := sp.client.Query("episodes", map[string]interface{}{}, limit)
	if err != nil {
		return nil, err
	}
	
	episodes := make([]*Episode, 0, len(results))
	for _, result := range results {
		var episode Episode
		data, err := json.Marshal(result)
		if err != nil {
			continue
		}
		if err := json.Unmarshal(data, &episode); err != nil {
			continue
		}
		episodes = append(episodes, &episode)
	}
	
	return episodes, nil
}

// StoreIdentitySnapshot stores a snapshot of identity state
func (sp *SupabasePersistence) StoreIdentitySnapshot(snapshot *IdentitySnapshot) error {
	if snapshot.ID == "" {
		snapshot.ID = uuid.New().String()
	}
	if snapshot.Timestamp.IsZero() {
		snapshot.Timestamp = time.Now()
	}
	
	return sp.client.Insert("identity_snapshots", snapshot)
}

// RetrieveLatestIdentitySnapshot retrieves the most recent identity snapshot
func (sp *SupabasePersistence) RetrieveLatestIdentitySnapshot() (*IdentitySnapshot, error) {
	// Note: This uses basic query - in production would use order by timestamp desc
	results, err := sp.client.Query("identity_snapshots", map[string]interface{}{}, 1)
	if err != nil {
		return nil, err
	}
	
	if len(results) == 0 {
		return nil, fmt.Errorf("no identity snapshots found")
	}
	
	var snapshot IdentitySnapshot
	data, err := json.Marshal(results[0])
	if err != nil {
		return nil, err
	}
	
	if err := json.Unmarshal(data, &snapshot); err != nil {
		return nil, err
	}
	
	return &snapshot, nil
}

// StoreDreamJournal stores a dream session record
func (sp *SupabasePersistence) StoreDreamJournal(journal *DreamJournal) error {
	if journal.ID == "" {
		journal.ID = uuid.New().String()
	}
	if journal.Timestamp.IsZero() {
		journal.Timestamp = time.Now()
	}
	
	return sp.client.Insert("dream_journals", journal)
}

// TraverseGraph performs a breadth-first traversal from a starting node
func (sp *SupabasePersistence) TraverseGraph(startNodeID string, maxDepth int, edgeTypes []EdgeType) ([]*MemoryNode, error) {
	visited := make(map[string]bool)
	nodes := make([]*MemoryNode, 0)
	
	// BFS queue
	type queueItem struct {
		nodeID string
		depth  int
	}
	queue := []queueItem{{nodeID: startNodeID, depth: 0}}
	
	for len(queue) > 0 {
		current := queue[0]
		queue = queue[1:]
		
		if visited[current.nodeID] || current.depth > maxDepth {
			continue
		}
		visited[current.nodeID] = true
		
		// Retrieve node
		node, err := sp.RetrieveNode(current.nodeID)
		if err != nil {
			continue
		}
		nodes = append(nodes, node)
		
		// Get outgoing edges
		edges, err := sp.QueryEdgesFromNode(current.nodeID)
		if err != nil {
			continue
		}
		
		// Filter by edge type if specified
		for _, edge := range edges {
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
	
	return nodes, nil
}

// FindRelatedNodes finds nodes related to a given node through specific edge types
func (sp *SupabasePersistence) FindRelatedNodes(nodeID string, edgeTypes []EdgeType, limit int) ([]*MemoryNode, error) {
	edges, err := sp.QueryEdgesFromNode(nodeID)
	if err != nil {
		return nil, err
	}
	
	nodes := make([]*MemoryNode, 0)
	for _, edge := range edges {
		if len(nodes) >= limit {
			break
		}
		
		// Check edge type
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
		
		// Retrieve target node
		node, err := sp.RetrieveNode(edge.TargetID)
		if err != nil {
			continue
		}
		nodes = append(nodes, node)
	}
	
	return nodes, nil
}

// GetMemoryContext retrieves contextual memories for thought generation
func (sp *SupabasePersistence) GetMemoryContext(thoughtType string, limit int) (map[string]interface{}, error) {
	context := make(map[string]interface{})
	
	// Get recent episodes
	episodes, err := sp.QueryRecentEpisodes(5)
	if err == nil && len(episodes) > 0 {
		context["recent_episodes"] = episodes
	}
	
	// Get relevant concepts
	concepts, err := sp.QueryNodesByType(NodeConcept, 10)
	if err == nil && len(concepts) > 0 {
		context["concepts"] = concepts
	}
	
	// Get recent thoughts
	thoughts, err := sp.QueryNodesByType(NodeThought, 10)
	if err == nil && len(thoughts) > 0 {
		context["recent_thoughts"] = thoughts
	}
	
	// Get active goals
	goals, err := sp.QueryNodesByType(NodeGoal, 5)
	if err == nil && len(goals) > 0 {
		context["active_goals"] = goals
	}
	
	return context, nil
}
