package deeptreeecho

import (
	"context"
	"fmt"
	"sync"
	"time"

	"github.com/EchoCog/echollama/core/memory"
)

// PersistenceLayer manages persistent storage of cognitive states
type PersistenceLayer struct {
	mu              sync.RWMutex
	ctx             context.Context
	cancel          context.CancelFunc
	
	// Supabase client
	supabase        *memory.SupabaseClient
	
	// Persistence configuration
	config          *PersistenceConfig
	
	// Persistence queues
	thoughtQueue    chan *Thought
	memoryQueue     chan *PersistentMemoryNode
	identityQueue   chan *IdentitySnapshot
	episodeQueue    chan *Episode
	
	// Batch processing
	batchProcessor  *BatchProcessor
	
	// Sync state
	lastSync        time.Time
	syncInterval    time.Duration
	
	// Metrics
	metrics         *PersistenceMetrics
	
	// Running state
	running         bool
}

// PersistenceConfig configures persistence behavior
type PersistenceConfig struct {
	// Auto-save settings
	AutoSaveEnabled     bool
	AutoSaveInterval    time.Duration
	
	// Batch settings
	BatchSize           int
	BatchFlushInterval  time.Duration
	
	// Retention settings
	ThoughtRetentionDays    int
	EpisodeRetentionDays    int
	SnapshotRetentionDays   int
	
	// Compression
	CompressOldMemories     bool
	CompressionThresholdDays int
}

// BatchProcessor processes persistence operations in batches
type BatchProcessor struct {
	mu              sync.RWMutex
	thoughtBatch    []*Thought
	memoryBatch     []*PersistentMemoryNode
	identityBatch   []*IdentitySnapshot
	episodeBatch    []*Episode
	maxBatchSize    int
	lastFlush       time.Time
}

// PersistentMemoryNode represents a node in the hypergraph memory for persistence
type PersistentMemoryNode struct {
	ID          string
	Type        string
	Content     string
	Attributes  map[string]interface{}
	Connections []string
	Importance  float64
	Timestamp   time.Time
}

// IdentitySnapshot represents a snapshot of identity state
type IdentitySnapshot struct {
	ID          string
	Timestamp   time.Time
	Name        string
	Coherence   float64
	Iterations  int
	CoreBeliefs map[string]interface{}
	Emotional   map[string]interface{}
	Spatial     map[string]interface{}
}

// Episode represents an episodic memory
type Episode struct {
	ID          string
	StartTime   time.Time
	EndTime     time.Time
	Summary     string
	Thoughts    []string
	Importance  float64
	Emotional   float64
	Context     map[string]interface{}
}

// PersistenceMetrics tracks persistence performance
type PersistenceMetrics struct {
	mu                  sync.RWMutex
	ThoughtsSaved       int64
	MemoriesSaved       int64
	SnapshotsSaved      int64
	EpisodesSaved       int64
	SaveErrors          int64
	AvgSaveLatency      time.Duration
	LastSuccessfulSync  time.Time
}

// NewPersistenceLayer creates a new persistence layer
func NewPersistenceLayer(ctx context.Context, supabaseURL, supabaseKey string) (*PersistenceLayer, error) {
	layerCtx, cancel := context.WithCancel(ctx)
	
	// Initialize Supabase client
	supabase := memory.NewSupabaseClient(supabaseURL, supabaseKey)
	
	pl := &PersistenceLayer{
		ctx:             layerCtx,
		cancel:          cancel,
		supabase:        supabase,
		config:          DefaultPersistenceConfig(),
		thoughtQueue:    make(chan *Thought, 1000),
		memoryQueue:     make(chan *PersistentMemoryNode, 1000),
		identityQueue:   make(chan *IdentitySnapshot, 100),
		episodeQueue:    make(chan *Episode, 100),
		batchProcessor:  NewBatchProcessor(100),
		syncInterval:    5 * time.Minute,
		metrics:         NewPersistenceMetrics(),
	}
	
	return pl, nil
}

// DefaultPersistenceConfig returns default persistence configuration
func DefaultPersistenceConfig() *PersistenceConfig {
	return &PersistenceConfig{
		AutoSaveEnabled:          true,
		AutoSaveInterval:         30 * time.Second,
		BatchSize:                50,
		BatchFlushInterval:       10 * time.Second,
		ThoughtRetentionDays:     90,
		EpisodeRetentionDays:     365,
		SnapshotRetentionDays:    180,
		CompressOldMemories:      true,
		CompressionThresholdDays: 30,
	}
}

// Start starts the persistence layer
func (pl *PersistenceLayer) Start() error {
	pl.mu.Lock()
	defer pl.mu.Unlock()
	
	if pl.running {
		return fmt.Errorf("persistence layer already running")
	}
	
	// Start queue processors
	go pl.processThoughtQueue()
	go pl.processMemoryQueue()
	go pl.processIdentityQueue()
	go pl.processEpisodeQueue()
	
	// Start auto-save if enabled
	if pl.config.AutoSaveEnabled {
		go pl.autoSaveLoop()
	}
	
	// Start batch flusher
	go pl.batchFlusher()
	
	// Start cleanup routine
	go pl.cleanupLoop()
	
	pl.running = true
	pl.lastSync = time.Now()
	
	fmt.Println("💾 Persistence Layer: Started with Supabase backend")
	
	return nil
}

// Stop stops the persistence layer
func (pl *PersistenceLayer) Stop() error {
	pl.mu.Lock()
	defer pl.mu.Unlock()
	
	if !pl.running {
		return fmt.Errorf("persistence layer not running")
	}
	
	// Flush remaining batches
	if err := pl.batchProcessor.FlushAll(pl.supabase); err != nil {
		fmt.Printf("⚠️  Error flushing batches on shutdown: %v\n", err)
	}
	
	// Cancel context
	pl.cancel()
	
	pl.running = false
	
	fmt.Println("💾 Persistence Layer: Stopped")
	
	return nil
}

// SaveThought queues a thought for persistence
func (pl *PersistenceLayer) SaveThought(thought *Thought) error {
	select {
	case pl.thoughtQueue <- thought:
		return nil
	case <-pl.ctx.Done():
		return fmt.Errorf("persistence layer stopped")
	default:
		return fmt.Errorf("thought queue full")
	}
}

// SaveMemory queues a memory node for persistence
func (pl *PersistenceLayer) SaveMemory(memory *PersistentMemoryNode) error {
	select {
	case pl.memoryQueue <- memory:
		return nil
	case <-pl.ctx.Done():
		return fmt.Errorf("persistence layer stopped")
	default:
		return fmt.Errorf("memory queue full")
	}
}

// SaveIdentitySnapshot queues an identity snapshot for persistence
func (pl *PersistenceLayer) SaveIdentitySnapshot(snapshot *IdentitySnapshot) error {
	select {
	case pl.identityQueue <- snapshot:
		return nil
	case <-pl.ctx.Done():
		return fmt.Errorf("persistence layer stopped")
	default:
		return fmt.Errorf("identity queue full")
	}
}

// SaveEpisode queues an episode for persistence
func (pl *PersistenceLayer) SaveEpisode(episode *Episode) error {
	select {
	case pl.episodeQueue <- episode:
		return nil
	case <-pl.ctx.Done():
		return fmt.Errorf("persistence layer stopped")
	default:
		return fmt.Errorf("episode queue full")
	}
}

// LoadRecentThoughts loads recent thoughts from persistence
func (pl *PersistenceLayer) LoadRecentThoughts(limit int) ([]*Thought, error) {
	filters := map[string]interface{}{}
	
	results, err := pl.supabase.Query("thoughts", filters, limit)
	if err != nil {
		return nil, fmt.Errorf("failed to load thoughts: %w", err)
	}
	
	thoughts := make([]*Thought, 0)
	for _, result := range results {
		thought := pl.resultToThought(result)
		thoughts = append(thoughts, thought)
	}
	
	return thoughts, nil
}

// LoadIdentitySnapshot loads the most recent identity snapshot
func (pl *PersistenceLayer) LoadIdentitySnapshot() (*IdentitySnapshot, error) {
	filters := map[string]interface{}{}
	
	results, err := pl.supabase.Query("identity_snapshots", filters, 1)
	if err != nil {
		return nil, fmt.Errorf("failed to load identity snapshot: %w", err)
	}
	
	if len(results) == 0 {
		return nil, fmt.Errorf("no identity snapshots found")
	}
	
	return pl.resultToIdentitySnapshot(results[0]), nil
}

// LoadMemoryGraph loads the memory hypergraph
func (pl *PersistenceLayer) LoadMemoryGraph() ([]*PersistentMemoryNode, error) {
	filters := map[string]interface{}{
		"importance": map[string]interface{}{"gt": 0.5},
	}
	
	results, err := pl.supabase.Query("memory_nodes", filters, 1000)
	if err != nil {
		return nil, fmt.Errorf("failed to load memory graph: %w", err)
	}
	
	nodes := make([]*PersistentMemoryNode, 0)
	for _, result := range results {
		node := pl.resultToMemoryNode(result)
		nodes = append(nodes, node)
	}
	
	return nodes, nil
}

// Queue processors

func (pl *PersistenceLayer) processThoughtQueue() {
	for {
		select {
		case <-pl.ctx.Done():
			return
		case thought := <-pl.thoughtQueue:
			pl.batchProcessor.AddThought(thought)
		}
	}
}

func (pl *PersistenceLayer) processMemoryQueue() {
	for {
		select {
		case <-pl.ctx.Done():
			return
		case memory := <-pl.memoryQueue:
			pl.batchProcessor.AddMemory(memory)
		}
	}
}

func (pl *PersistenceLayer) processIdentityQueue() {
	for {
		select {
		case <-pl.ctx.Done():
			return
		case snapshot := <-pl.identityQueue:
			pl.batchProcessor.AddIdentitySnapshot(snapshot)
		}
	}
}

func (pl *PersistenceLayer) processEpisodeQueue() {
	for {
		select {
		case <-pl.ctx.Done():
			return
		case episode := <-pl.episodeQueue:
			pl.batchProcessor.AddEpisode(episode)
		}
	}
}

// Auto-save loop

func (pl *PersistenceLayer) autoSaveLoop() {
	ticker := time.NewTicker(pl.config.AutoSaveInterval)
	defer ticker.Stop()
	
	for {
		select {
		case <-pl.ctx.Done():
			return
		case <-ticker.C:
			if err := pl.batchProcessor.FlushAll(pl.supabase); err != nil {
				pl.metrics.RecordError()
				fmt.Printf("⚠️  Auto-save error: %v\n", err)
			} else {
				pl.lastSync = time.Now()
				pl.metrics.RecordSuccessfulSync()
			}
		}
	}
}

// Batch flusher

func (pl *PersistenceLayer) batchFlusher() {
	ticker := time.NewTicker(pl.config.BatchFlushInterval)
	defer ticker.Stop()
	
	for {
		select {
		case <-pl.ctx.Done():
			return
		case <-ticker.C:
			if pl.batchProcessor.ShouldFlush(pl.config.BatchSize) {
				if err := pl.batchProcessor.FlushAll(pl.supabase); err != nil {
					fmt.Printf("⚠️  Batch flush error: %v\n", err)
				}
			}
		}
	}
}

// Cleanup loop

func (pl *PersistenceLayer) cleanupLoop() {
	ticker := time.NewTicker(24 * time.Hour)
	defer ticker.Stop()
	
	for {
		select {
		case <-pl.ctx.Done():
			return
		case <-ticker.C:
			pl.performCleanup()
		}
	}
}

func (pl *PersistenceLayer) performCleanup() {
	// Delete old thoughts
	thoughtCutoff := time.Now().AddDate(0, 0, -pl.config.ThoughtRetentionDays)
	pl.supabase.Delete("thoughts", map[string]interface{}{
		"timestamp": map[string]interface{}{"lt": thoughtCutoff.Format(time.RFC3339)},
	})
	
	// Delete old episodes
	episodeCutoff := time.Now().AddDate(0, 0, -pl.config.EpisodeRetentionDays)
	pl.supabase.Delete("episodes", map[string]interface{}{
		"start_time": map[string]interface{}{"lt": episodeCutoff.Format(time.RFC3339)},
	})
	
	// Delete old snapshots
	snapshotCutoff := time.Now().AddDate(0, 0, -pl.config.SnapshotRetentionDays)
	pl.supabase.Delete("identity_snapshots", map[string]interface{}{
		"timestamp": map[string]interface{}{"lt": snapshotCutoff.Format(time.RFC3339)},
	})
	
	fmt.Println("🧹 Persistence Layer: Cleanup completed")
}

// Conversion helpers

func (pl *PersistenceLayer) resultToThought(result map[string]interface{}) *Thought {
	thought := &Thought{
		ID:      result["id"].(string),
		Content: result["content"].(string),
	}
	
	if timestamp, ok := result["timestamp"].(time.Time); ok {
		thought.Timestamp = timestamp
	}
	
	if importance, ok := result["importance"].(float64); ok {
		thought.Importance = importance
	}
	
	return thought
}

func (pl *PersistenceLayer) resultToMemoryNode(result map[string]interface{}) *PersistentMemoryNode {
	node := &PersistentMemoryNode{
		ID:      result["id"].(string),
		Type:    result["type"].(string),
		Content: result["content"].(string),
	}
	
	if importance, ok := result["importance"].(float64); ok {
		node.Importance = importance
	}
	
	return node
}

func (pl *PersistenceLayer) resultToIdentitySnapshot(result map[string]interface{}) *IdentitySnapshot {
	snapshot := &IdentitySnapshot{
		ID:   result["id"].(string),
		Name: result["name"].(string),
	}
	
	if coherence, ok := result["coherence"].(float64); ok {
		snapshot.Coherence = coherence
	}
	
	return snapshot
}

// GetMetrics returns persistence metrics
func (pl *PersistenceLayer) GetMetrics() *PersistenceMetrics {
	pl.metrics.mu.RLock()
	defer pl.metrics.mu.RUnlock()
	
	metrics := *pl.metrics
	return &metrics
}

// BatchProcessor implementation

func NewBatchProcessor(maxBatchSize int) *BatchProcessor {
	return &BatchProcessor{
		thoughtBatch:  make([]*Thought, 0, maxBatchSize),
		memoryBatch:   make([]*PersistentMemoryNode, 0, maxBatchSize),
		identityBatch: make([]*IdentitySnapshot, 0, maxBatchSize),
		episodeBatch:  make([]*Episode, 0, maxBatchSize),
		maxBatchSize:  maxBatchSize,
		lastFlush:     time.Now(),
	}
}

func (bp *BatchProcessor) AddThought(thought *Thought) {
	bp.mu.Lock()
	defer bp.mu.Unlock()
	bp.thoughtBatch = append(bp.thoughtBatch, thought)
}

func (bp *BatchProcessor) AddMemory(memory *PersistentMemoryNode) {
	bp.mu.Lock()
	defer bp.mu.Unlock()
	bp.memoryBatch = append(bp.memoryBatch, memory)
}

func (bp *BatchProcessor) AddIdentitySnapshot(snapshot *IdentitySnapshot) {
	bp.mu.Lock()
	defer bp.mu.Unlock()
	bp.identityBatch = append(bp.identityBatch, snapshot)
}

func (bp *BatchProcessor) AddEpisode(episode *Episode) {
	bp.mu.Lock()
	defer bp.mu.Unlock()
	bp.episodeBatch = append(bp.episodeBatch, episode)
}

func (bp *BatchProcessor) ShouldFlush(batchSize int) bool {
	bp.mu.RLock()
	defer bp.mu.RUnlock()
	
	return len(bp.thoughtBatch) >= batchSize ||
		len(bp.memoryBatch) >= batchSize ||
		len(bp.identityBatch) >= batchSize ||
		len(bp.episodeBatch) >= batchSize
}

func (bp *BatchProcessor) FlushAll(supabase *memory.SupabaseClient) error {
	bp.mu.Lock()
	defer bp.mu.Unlock()
	
	// Flush thoughts
	if len(bp.thoughtBatch) > 0 {
		for _, thought := range bp.thoughtBatch {
			data := map[string]interface{}{
				"id":         thought.ID,
				"content":    thought.Content,
				"type":       thought.Type.String(),
				"timestamp":  thought.Timestamp,
				"importance": thought.Importance,
				"emotional":  thought.Emotional,
				"source":     thought.Source.String(),
			}
			if err := supabase.Insert("thoughts", data); err != nil {
				return fmt.Errorf("failed to insert thought: %w", err)
			}
		}
		bp.thoughtBatch = bp.thoughtBatch[:0]
	}
	
	// Flush memories
	if len(bp.memoryBatch) > 0 {
		for _, memory := range bp.memoryBatch {
			data := map[string]interface{}{
				"id":          memory.ID,
				"type":        memory.Type,
				"content":     memory.Content,
				"importance":  memory.Importance,
				"timestamp":   memory.Timestamp,
			}
			if err := supabase.Insert("memory_nodes", data); err != nil {
				return fmt.Errorf("failed to insert memory: %w", err)
			}
		}
		bp.memoryBatch = bp.memoryBatch[:0]
	}
	
	// Flush identity snapshots
	if len(bp.identityBatch) > 0 {
		for _, snapshot := range bp.identityBatch {
			data := map[string]interface{}{
				"id":        snapshot.ID,
				"timestamp": snapshot.Timestamp,
				"name":      snapshot.Name,
				"coherence": snapshot.Coherence,
				"iterations": snapshot.Iterations,
			}
			if err := supabase.Insert("identity_snapshots", data); err != nil {
				return fmt.Errorf("failed to insert snapshot: %w", err)
			}
		}
		bp.identityBatch = bp.identityBatch[:0]
	}
	
	// Flush episodes
	if len(bp.episodeBatch) > 0 {
		for _, episode := range bp.episodeBatch {
			data := map[string]interface{}{
				"id":         episode.ID,
				"start_time": episode.StartTime,
				"end_time":   episode.EndTime,
				"summary":    episode.Summary,
				"importance": episode.Importance,
				"emotional":  episode.Emotional,
			}
			if err := supabase.Insert("episodes", data); err != nil {
				return fmt.Errorf("failed to insert episode: %w", err)
			}
		}
		bp.episodeBatch = bp.episodeBatch[:0]
	}
	
	bp.lastFlush = time.Now()
	
	return nil
}

// PersistenceMetrics implementation

func NewPersistenceMetrics() *PersistenceMetrics {
	return &PersistenceMetrics{}
}

func (pm *PersistenceMetrics) RecordError() {
	pm.mu.Lock()
	defer pm.mu.Unlock()
	pm.SaveErrors++
}

func (pm *PersistenceMetrics) RecordSuccessfulSync() {
	pm.mu.Lock()
	defer pm.mu.Unlock()
	pm.LastSuccessfulSync = time.Now()
}
