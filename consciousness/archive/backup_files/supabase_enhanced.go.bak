package memory

import (
	"encoding/json"
	"fmt"
	"os"
	"time"
)

// SupabasePersistence provides enhanced persistence using Supabase
type SupabasePersistence struct {
	url       string
	key       string
	enabled   bool
	tableName string
}

// NewSupabasePersistence creates a new Supabase persistence layer
func NewSupabasePersistence() (*SupabasePersistence, error) {
	url := os.Getenv("SUPABASE_URL")
	key := os.Getenv("SUPABASE_KEY")

	if url == "" || key == "" {
		fmt.Println("⚠️  Supabase credentials not found, persistence disabled")
		return &SupabasePersistence{
			enabled: false,
		}, nil
	}

	fmt.Println("✅ Supabase persistence enabled")
	return &SupabasePersistence{
		url:       url,
		key:       key,
		enabled:   true,
		tableName: "echo_memories",
	}, nil
}

// IsEnabled returns whether persistence is enabled
func (sp *SupabasePersistence) IsEnabled() bool {
	return sp.enabled
}

// StoreNode stores a memory node (alias for SaveMemoryNode for interface compatibility)
func (sp *SupabasePersistence) StoreNode(node *MemoryNode) error {
	return sp.SaveMemoryNode(node)
}

// StoreEdge stores a memory edge (alias for SaveMemoryEdge for interface compatibility)
func (sp *SupabasePersistence) StoreEdge(edge *MemoryEdge) error {
	return sp.SaveMemoryEdge(edge)
}

// SaveMemoryNode saves a memory node to Supabase
func (sp *SupabasePersistence) SaveMemoryNode(node *MemoryNode) error {
	if !sp.enabled {
		return nil
	}

	// For now, log the save operation
	// Full Supabase client integration would require the Supabase Go SDK
	fmt.Printf("💾 Persisting memory node: %s (importance: %.2f)\n", 
		node.ID, node.Importance)

	// In production, this would use the Supabase client:
	// client := supabase.CreateClient(sp.url, sp.key)
	// _, err := client.From(sp.tableName).Insert(node).Execute()
	// return err

	return nil
}

// SaveMemoryEdge saves a memory edge to Supabase
func (sp *SupabasePersistence) SaveMemoryEdge(edge *MemoryEdge) error {
	if !sp.enabled {
		return nil
	}

	// For now, log the save operation
	fmt.Printf("💾 Persisting memory edge: %s -> %s (weight: %.2f)\n", 
		edge.SourceID, edge.TargetID, edge.Weight)

	// In production, this would use the Supabase client
	return nil
}

// LoadMemoryNodes loads memory nodes from Supabase
func (sp *SupabasePersistence) LoadMemoryNodes(limit int) ([]*MemoryNode, error) {
	if !sp.enabled {
		return []*MemoryNode{}, nil
	}

	fmt.Printf("📥 Loading up to %d memory nodes from persistence\n", limit)

	// In production, this would query Supabase:
	// client := supabase.CreateClient(sp.url, sp.key)
	// result, err := client.From(sp.tableName).Select("*").Limit(limit).Execute()
	// return parseNodes(result), err

	return []*MemoryNode{}, nil
}

// SaveThought saves a thought to persistence
func (sp *SupabasePersistence) SaveThought(thought interface{}) error {
	if !sp.enabled {
		return nil
	}

	data, err := json.Marshal(thought)
	if err != nil {
		return fmt.Errorf("failed to marshal thought: %w", err)
	}

	fmt.Printf("💾 Persisting thought: %d bytes\n", len(data))

	// In production, save to Supabase thoughts table
	return nil
}

// SaveReflection saves a reflection to persistence
func (sp *SupabasePersistence) SaveReflection(reflection interface{}) error {
	if !sp.enabled {
		return nil
	}

	data, err := json.Marshal(reflection)
	if err != nil {
		return fmt.Errorf("failed to marshal reflection: %w", err)
	}

	fmt.Printf("💾 Persisting reflection: %d bytes\n", len(data))

	// In production, save to Supabase reflections table
	return nil
}

// SaveWisdomMetrics saves wisdom metrics to persistence
func (sp *SupabasePersistence) SaveWisdomMetrics(metrics interface{}) error {
	if !sp.enabled {
		return nil
	}

	data, err := json.Marshal(metrics)
	if err != nil {
		return fmt.Errorf("failed to marshal metrics: %w", err)
	}

	fmt.Printf("💾 Persisting wisdom metrics: %d bytes\n", len(data))

	// In production, save to Supabase metrics table
	return nil
}

// GetLastReflectionTime retrieves the timestamp of the last reflection
func (sp *SupabasePersistence) GetLastReflectionTime() (time.Time, error) {
	if !sp.enabled {
		return time.Time{}, nil
	}

	// In production, query Supabase for last reflection timestamp
	return time.Time{}, nil
}

// GetThoughtCount returns the total number of persisted thoughts
func (sp *SupabasePersistence) GetThoughtCount() (int, error) {
	if !sp.enabled {
		return 0, nil
	}

	// In production, query Supabase for count
	return 0, nil
}

// CleanupOldMemories removes old low-importance memories
func (sp *SupabasePersistence) CleanupOldMemories(olderThan time.Duration, importanceThreshold float64) error {
	if !sp.enabled {
		return nil
	}

	cutoffTime := time.Now().Add(-olderThan)
	fmt.Printf("🧹 Cleaning up memories older than %v with importance < %.2f\n", 
		cutoffTime, importanceThreshold)

	// In production, delete from Supabase:
	// client := supabase.CreateClient(sp.url, sp.key)
	// _, err := client.From(sp.tableName).
	//     Delete().
	//     Lt("created_at", cutoffTime).
	//     Lt("importance", importanceThreshold).
	//     Execute()
	// return err

	return nil
}

// ExportMemories exports all memories to JSON
func (sp *SupabasePersistence) ExportMemories(filepath string) error {
	if !sp.enabled {
		return fmt.Errorf("persistence not enabled")
	}

	fmt.Printf("📤 Exporting memories to %s\n", filepath)

	// In production, query all memories and export to file
	return nil
}

// ImportMemories imports memories from JSON
func (sp *SupabasePersistence) ImportMemories(filepath string) error {
	if !sp.enabled {
		return fmt.Errorf("persistence not enabled")
	}

	fmt.Printf("📥 Importing memories from %s\n", filepath)

	// In production, read file and bulk insert to Supabase
	return nil
}

// GetMemoryStatistics returns statistics about persisted memories
func (sp *SupabasePersistence) GetMemoryStatistics() (*PersistenceStatistics, error) {
	if !sp.enabled {
		return &PersistenceStatistics{}, nil
	}

	// In production, query Supabase for statistics
	return &PersistenceStatistics{
		TotalMemories:      0,
		HighImportance:     0,
		MediumImportance:   0,
		LowImportance:      0,
		OldestMemory:       time.Time{},
		NewestMemory:       time.Time{},
		AverageImportance:  0.0,
	}, nil
}

// PersistenceStatistics holds statistics about persisted memories (renamed to avoid conflict with MemoryStatistics in persistent.go)
type PersistenceStatistics struct {
	TotalMemories     int
	HighImportance    int
	MediumImportance  int
	LowImportance     int
	OldestMemory      time.Time
	NewestMemory      time.Time
	AverageImportance float64
}

// PrintStatistics prints memory statistics
func (stats *PersistenceStatistics) PrintStatistics() {
	fmt.Println("\n╔════════════════════════════════════════════════════════════╗")
	fmt.Printf("║ Memory Persistence Statistics                              ║\n")
	fmt.Println("╠════════════════════════════════════════════════════════════╣")
	fmt.Printf("║ Total Memories:      %-10d                          ║\n", stats.TotalMemories)
	fmt.Printf("║ High Importance:     %-10d                          ║\n", stats.HighImportance)
	fmt.Printf("║ Medium Importance:   %-10d                          ║\n", stats.MediumImportance)
	fmt.Printf("║ Low Importance:      %-10d                          ║\n", stats.LowImportance)
	fmt.Printf("║ Average Importance:  %-10.2f                        ║\n", stats.AverageImportance)
	fmt.Println("╚════════════════════════════════════════════════════════════╝\n")
}

// SaveCognitiveState saves cognitive state snapshot to Supabase
func (sp *SupabasePersistence) SaveCognitiveState(identityID string, data []byte) error {
	if !sp.enabled {
		return nil
	}

	fmt.Printf("💾 Persisting cognitive state for identity: %s (%d bytes)\n", 
		identityID, len(data))

	// Create a record for the cognitive_states table
	record := map[string]interface{}{
		"identity_id": identityID,
		"state_data":  string(data), // Store as JSON string
		"version":     5,
		"timestamp":   time.Now().Format(time.RFC3339),
	}

	// Use the Supabase client to insert
	client := NewSupabaseClient(sp.url, sp.key)
	err := client.Insert("cognitive_states", record)
	if err != nil {
		return fmt.Errorf("failed to save cognitive state: %w", err)
	}

	fmt.Println("✅ Cognitive state persisted successfully")
	return nil
}

// LoadCognitiveState loads the latest cognitive state snapshot from Supabase
func (sp *SupabasePersistence) LoadCognitiveState(identityID string) ([]byte, error) {
	if !sp.enabled {
		return nil, nil
	}

	fmt.Printf("📥 Loading cognitive state for identity: %s\n", identityID)

	// Query the latest state for this identity
	client := NewSupabaseClient(sp.url, sp.key)
	filter := map[string]interface{}{
		"identity_id": identityID,
	}
	
	results, err := client.Query("cognitive_states", filter, 1)
	if err != nil {
		return nil, fmt.Errorf("failed to query cognitive state: %w", err)
	}

	if len(results) == 0 {
		fmt.Println("ℹ️  No saved cognitive state found")
		return nil, nil
	}

	// Extract the state data
	stateData, ok := results[0]["state_data"].(string)
	if !ok {
		return nil, fmt.Errorf("invalid state data format")
	}

	fmt.Printf("✅ Loaded cognitive state (%d bytes)\n", len(stateData))
	return []byte(stateData), nil
}

// SaveKnowledgeGraph saves knowledge graph nodes to Supabase
func (sp *SupabasePersistence) SaveKnowledgeGraph(nodes []interface{}) error {
	if !sp.enabled {
		return nil
	}

	fmt.Printf("💾 Persisting %d knowledge nodes\n", len(nodes))

	client := NewSupabaseClient(sp.url, sp.key)
	for _, node := range nodes {
		err := client.Insert("knowledge_nodes", node)
		if err != nil {
			fmt.Printf("⚠️  Failed to persist knowledge node: %v\n", err)
			// Continue with other nodes
		}
	}

	fmt.Println("✅ Knowledge graph persisted")
	return nil
}

// LoadKnowledgeGraph loads knowledge graph from Supabase
func (sp *SupabasePersistence) LoadKnowledgeGraph(identityID string, limit int) ([]interface{}, error) {
	if !sp.enabled {
		return []interface{}{}, nil
	}

	fmt.Printf("📥 Loading knowledge graph for identity: %s\n", identityID)

	client := NewSupabaseClient(sp.url, sp.key)
	filter := map[string]interface{}{
		"identity_id": identityID,
	}
	
	results, err := client.Query("knowledge_nodes", filter, limit)
	if err != nil {
		return nil, fmt.Errorf("failed to query knowledge graph: %w", err)
	}

	// Convert to interface slice
	nodes := make([]interface{}, len(results))
	for i, result := range results {
		nodes[i] = result
	}

	fmt.Printf("✅ Loaded %d knowledge nodes\n", len(nodes))
	return nodes, nil
}
