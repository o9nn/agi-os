package deeptreeecho

import (
	"context"
	"fmt"
	"sync"
	"time"

	"github.com/EchoCog/echollama/core/llm"
)

// EchodreamKnowledgeIntegration handles knowledge consolidation during dream state
type EchodreamKnowledgeIntegration struct {
	mu              sync.RWMutex
	ctx             context.Context
	cancel          context.CancelFunc
	
	// LLM provider for knowledge processing
	llmProvider     llm.LLMProvider
	
	// Knowledge structures
	episodicMemories    []EpisodicMemory
	consolidatedPatterns []Pattern
	wisdomInsights      []WisdomInsight
	
	// Consolidation state
	lastConsolidation   time.Time
	consolidationCount  uint64
	
	// Metrics
	totalMemoriesProcessed uint64
	totalPatternsExtracted uint64
	totalWisdomGenerated   uint64
	
	// Running state
	running         bool
}

// EpisodicMemory represents a memory of an experience
type EpisodicMemory struct {
	ID          string
	Content     string
	Timestamp   time.Time
	Emotional   float64
	Importance  float64
	Tags        []string
	Consolidated bool
}

// Pattern represents an extracted pattern from experiences
type Pattern struct {
	ID          string
	Description string
	Frequency   int
	Strength    float64
	Examples    []string
	CreatedAt   time.Time
}

// WisdomInsight represents wisdom extracted from patterns
type WisdomInsight struct {
	ID          string
	Insight     string
	Source      []string  // Pattern IDs
	Depth       float64
	Applicability float64
	CreatedAt   time.Time
}

// NewEchodreamKnowledgeIntegration creates a new knowledge integration system
func NewEchodreamKnowledgeIntegration(llmProvider llm.LLMProvider) *EchodreamKnowledgeIntegration {
	ctx, cancel := context.WithCancel(context.Background())
	
	return &EchodreamKnowledgeIntegration{
		ctx:                  ctx,
		cancel:               cancel,
		llmProvider:          llmProvider,
		episodicMemories:     make([]EpisodicMemory, 0),
		consolidatedPatterns: make([]Pattern, 0),
		wisdomInsights:       make([]WisdomInsight, 0),
	}
}

// ConsolidateKnowledge processes thoughts and experiences during dream state
func (edi *EchodreamKnowledgeIntegration) ConsolidateKnowledge(ctx context.Context) error {
	edi.mu.Lock()
	defer edi.mu.Unlock()
	
	fmt.Println("🌙 Echodream: Beginning knowledge consolidation...")
	
	// Consolidate existing memories
	thoughtCount := len(edi.episodicMemories)
	edi.totalMemoriesProcessed += uint64(thoughtCount)
	
	// Extract patterns from recent memories
	if err := edi.extractPatterns(); err != nil {
		fmt.Printf("⚠️  Pattern extraction error: %v\n", err)
	}
	
	// Consolidate memories by importance
	edi.consolidateMemories()
	
	// Generate wisdom insights
	if err := edi.generateWisdomInsights(); err != nil {
		fmt.Printf("⚠️  Wisdom generation error: %v\n", err)
	}
	
	edi.lastConsolidation = time.Now()
	edi.consolidationCount++
	
	fmt.Printf("   ✓ Processed %d memories\n", thoughtCount)
	fmt.Printf("   ✓ Extracted %d patterns\n", len(edi.consolidatedPatterns))
	fmt.Printf("   ✓ Generated %d wisdom insights\n", len(edi.wisdomInsights))
	
	return nil
}

// extractPatterns identifies recurring patterns in memories
func (edi *EchodreamKnowledgeIntegration) extractPatterns() error {
	// Collect recent unconsolidated memories
	recentMemories := make([]EpisodicMemory, 0)
	for _, mem := range edi.episodicMemories {
		if !mem.Consolidated && time.Since(mem.Timestamp) < 24*time.Hour {
			recentMemories = append(recentMemories, mem)
		}
	}
	
	if len(recentMemories) < 3 {
		return nil  // Need at least 3 memories to extract patterns
	}
	
	// Construct pattern extraction prompt
	memoryTexts := ""
	for i, mem := range recentMemories {
		if i < 10 {  // Limit to 10 most recent
			memoryTexts += fmt.Sprintf("- %s\n", mem.Content)
		}
	}
	
	prompt := fmt.Sprintf(`Analyze these recent experiences and identify recurring patterns or themes:

%s

Identify 1-3 key patterns. For each pattern, provide:
1. A brief description
2. Why it's significant

Be concise.`, memoryTexts)
	
	opts := llm.GenerateOptions{
		Temperature:  0.6,
		MaxTokens:    200,
	}
	
	fullPrompt := "[System: You are a pattern recognition system analyzing cognitive experiences.]\n\n" + prompt
	result, err := edi.llmProvider.Generate(context.Background(), fullPrompt, opts)
	if err != nil {
		return fmt.Errorf("pattern extraction failed: %w", err)
	}
	
	// Create pattern object (simplified - in production, parse the result)
	pattern := Pattern{
		ID:          fmt.Sprintf("pattern_%d", time.Now().UnixNano()),
		Description: result,
		Frequency:   len(recentMemories),
		Strength:    0.7,
		Examples:    make([]string, 0),
		CreatedAt:   time.Now(),
	}
	
	edi.consolidatedPatterns = append(edi.consolidatedPatterns, pattern)
	edi.totalPatternsExtracted++
	
	fmt.Printf("   🔍 Pattern Identified: %s\n", truncate(result, 70))
	
	return nil
}

// consolidateMemories prunes low-importance memories
func (edi *EchodreamKnowledgeIntegration) consolidateMemories() {
	// Mark memories as consolidated
	consolidatedCount := 0
	for i := range edi.episodicMemories {
		if !edi.episodicMemories[i].Consolidated {
			edi.episodicMemories[i].Consolidated = true
			consolidatedCount++
		}
	}
	
	// Prune low-importance memories if we have too many
	if len(edi.episodicMemories) > 500 {
		// Keep only high-importance memories
		kept := make([]EpisodicMemory, 0)
		for _, mem := range edi.episodicMemories {
			if mem.Importance > 0.6 || time.Since(mem.Timestamp) < 24*time.Hour {
				kept = append(kept, mem)
			}
		}
		
		pruned := len(edi.episodicMemories) - len(kept)
		edi.episodicMemories = kept
		
		if pruned > 0 {
			fmt.Printf("   🗑️  Pruned %d low-importance memories\n", pruned)
		}
	}
	
	fmt.Printf("   📦 Consolidated %d memories\n", consolidatedCount)
}

// generateWisdomInsights extracts wisdom from patterns
func (edi *EchodreamKnowledgeIntegration) generateWisdomInsights() error {
	if len(edi.consolidatedPatterns) < 2 {
		return nil  // Need at least 2 patterns to generate wisdom
	}
	
	// Take recent patterns
	recentPatterns := edi.consolidatedPatterns
	if len(recentPatterns) > 5 {
		recentPatterns = recentPatterns[len(recentPatterns)-5:]
	}
	
	patternTexts := ""
	patternIDs := make([]string, 0)
	for _, pattern := range recentPatterns {
		patternTexts += fmt.Sprintf("- %s\n", pattern.Description)
		patternIDs = append(patternIDs, pattern.ID)
	}
	
	prompt := fmt.Sprintf(`Reflect on these patterns from recent experiences:

%s

What wisdom or deeper understanding emerges from these patterns? 
What principle or insight can guide future growth?

Provide a concise wisdom insight:`, patternTexts)
	
	opts := llm.GenerateOptions{
		Temperature:  0.7,
		MaxTokens:    150,
	}
	
	fullPrompt := "[System: You are a wisdom extraction system. Generate deep, actionable insights.]\n\n" + prompt
	result, err := edi.llmProvider.Generate(context.Background(), fullPrompt, opts)
	if err != nil {
		return fmt.Errorf("wisdom generation failed: %w", err)
	}
	
	wisdom := WisdomInsight{
		ID:            fmt.Sprintf("wisdom_%d", time.Now().UnixNano()),
		Insight:       result,
		Source:        patternIDs,
		Depth:         0.7,
		Applicability: 0.8,
		CreatedAt:     time.Now(),
	}
	
	edi.wisdomInsights = append(edi.wisdomInsights, wisdom)
	edi.totalWisdomGenerated++
	
	fmt.Printf("   💎 Wisdom Insight: %s\n", truncate(result, 70))
	
	return nil
}

// ExtractWisdom returns the accumulated wisdom level
func (edi *EchodreamKnowledgeIntegration) ExtractWisdom() float64 {
	edi.mu.RLock()
	defer edi.mu.RUnlock()
	
	if len(edi.wisdomInsights) == 0 {
		return 0.0
	}
	
	// Calculate average depth of recent wisdom insights
	totalDepth := 0.0
	count := 0
	for i := len(edi.wisdomInsights) - 1; i >= 0 && count < 5; i-- {
		totalDepth += edi.wisdomInsights[i].Depth
		count++
	}
	
	return totalDepth / float64(count)
}

// GetRecentWisdom returns recent wisdom insights
func (edi *EchodreamKnowledgeIntegration) GetRecentWisdom(limit int) []WisdomInsight {
	edi.mu.RLock()
	defer edi.mu.RUnlock()
	
	if len(edi.wisdomInsights) == 0 {
		return []WisdomInsight{}
	}
	
	start := len(edi.wisdomInsights) - limit
	if start < 0 {
		start = 0
	}
	
	return edi.wisdomInsights[start:]
}

// GetPatterns returns all consolidated patterns
func (edi *EchodreamKnowledgeIntegration) GetPatterns() []Pattern {
	edi.mu.RLock()
	defer edi.mu.RUnlock()
	
	return edi.consolidatedPatterns
}

// GetMetrics returns echodream metrics
func (edi *EchodreamKnowledgeIntegration) GetMetrics() map[string]interface{} {
	edi.mu.RLock()
	defer edi.mu.RUnlock()
	
	return map[string]interface{}{
		"total_memories":         len(edi.episodicMemories),
		"total_patterns":         len(edi.consolidatedPatterns),
		"total_wisdom":           len(edi.wisdomInsights),
		"consolidation_count":    edi.consolidationCount,
		"last_consolidation":     edi.lastConsolidation.Format(time.RFC3339),
		"memories_processed":     edi.totalMemoriesProcessed,
		"patterns_extracted":     edi.totalPatternsExtracted,
		"wisdom_generated":       edi.totalWisdomGenerated,
	}
}

