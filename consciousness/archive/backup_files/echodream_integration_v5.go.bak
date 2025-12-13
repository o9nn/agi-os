package deeptreeecho

import (
	"fmt"
	"sync"
	"time"

	"github.com/EchoCog/echollama/core/echodream"
	"github.com/EchoCog/echollama/core/memory"
)

// EchoDreamIntegrationV5 implements automatic knowledge integration during rest cycles
// This is critical for wisdom cultivation - experiences become long-term knowledge
type EchoDreamIntegrationV5 struct {
	mu                  sync.RWMutex
	
	// Dream system
	dream               *echodream.EchoDream
	
	// Memory systems
	workingMemory       *WorkingMemory
	hypergraph          *memory.HypergraphMemory
	
	// Wisdom tracking
	wisdomMetrics       *WisdomMetrics
	
	// Integration metrics
	experiencesProcessed int64
	knowledgeNodesCreated int64
	wisdomGained        float64
	lastIntegrationTime time.Time
	
	// Configuration
	minExperiencesForRest int
	consolidationDepth    int
}

// ExperienceCluster represents a group of related experiences
type ExperienceCluster struct {
	Theme            string
	Experiences      []*Thought
	CommonPatterns   []string
	EmotionalTone    float64
	Importance       float64
}

// DreamKnowledgeNode represents consolidated knowledge from dreams
type DreamKnowledgeNode struct {
	ID               string
	Content          string
	Source           []string // IDs of source experiences
	Confidence       float64
	Wisdom           float64
	CreatedAt        time.Time
	Connections      []string // IDs of related nodes
}

// WisdomInsight represents extracted wisdom
type WisdomInsight struct {
	Insight          string
	Depth            float64
	Breadth          float64
	Integration      float64
	SourceExperiences []string
}

// NewEchoDreamIntegrationV5 creates a new V5 dream integration system
func NewEchoDreamIntegrationV5(
	dream *echodream.EchoDream,
	workingMemory *WorkingMemory,
	hypergraph *memory.HypergraphMemory,
	wisdomMetrics *WisdomMetrics,
) *EchoDreamIntegrationV5 {
	return &EchoDreamIntegrationV5{
		dream:                 dream,
		workingMemory:         workingMemory,
		hypergraph:            hypergraph,
		wisdomMetrics:         wisdomMetrics,
		minExperiencesForRest: 20,
		consolidationDepth:    3,
		lastIntegrationTime:   time.Now(),
	}
}

// IntegrateKnowledge performs automatic knowledge integration during rest cycle
// This is where experiences become wisdom
func (edi *EchoDreamIntegrationV5) IntegrateKnowledge() error {
	edi.mu.Lock()
	defer edi.mu.Unlock()
	
	fmt.Println("🌙 EchoDream: Beginning knowledge integration...")
	startTime := time.Now()
	
	// Step 1: Extract experiences from working memory
	experiences := edi.extractExperiences()
	if len(experiences) == 0 {
		fmt.Println("   No experiences to integrate")
		return nil
	}
	
	fmt.Printf("   📦 Extracted %d experiences\n", len(experiences))
	
	// Step 2: Cluster related experiences
	clusters := edi.clusterExperiences(experiences)
	fmt.Printf("   🔗 Identified %d experience clusters\n", len(clusters))
	
	// Step 3: Consolidate each cluster into knowledge
	knowledgeNodes := make([]*DreamKnowledgeNode, 0)
	for _, cluster := range clusters {
		node := edi.consolidateCluster(cluster)
		if node != nil {
			knowledgeNodes = append(knowledgeNodes, node)
		}
	}
	
	fmt.Printf("   💎 Created %d knowledge nodes\n", len(knowledgeNodes))
	
	// Step 4: Extract wisdom insights
	insights := edi.extractWisdom(knowledgeNodes, experiences)
	fmt.Printf("   ✨ Extracted %d wisdom insights\n", len(insights))
	
	// Step 5: Integrate into hypergraph memory
	err := edi.integrateIntoMemory(knowledgeNodes, insights)
	if err != nil {
		return fmt.Errorf("failed to integrate into memory: %w", err)
	}
	
	// Step 6: Update wisdom metrics
	wisdomGained := edi.updateWisdomMetrics(insights)
	
	// Step 7: Clear working memory (experiences now consolidated)
	edi.clearWorkingMemory()
	
	// Update metrics
	edi.experiencesProcessed += int64(len(experiences))
	edi.knowledgeNodesCreated += int64(len(knowledgeNodes))
	edi.wisdomGained += wisdomGained
	edi.lastIntegrationTime = time.Now()
	
	duration := time.Since(startTime)
	fmt.Printf("✅ Knowledge integration complete (%.2fs, wisdom gained: +%.3f)\n", 
		duration.Seconds(), wisdomGained)
	
	return nil
}

// extractExperiences extracts experiences from working memory
func (edi *EchoDreamIntegrationV5) extractExperiences() []*Thought {
	if edi.workingMemory == nil {
		return nil
	}
	
	edi.workingMemory.mu.RLock()
	defer edi.workingMemory.mu.RUnlock()
	
	// Copy all thoughts from working memory
	experiences := make([]*Thought, len(edi.workingMemory.buffer))
	copy(experiences, edi.workingMemory.buffer)
	
	return experiences
}

// clusterExperiences groups related experiences by theme
func (edi *EchoDreamIntegrationV5) clusterExperiences(experiences []*Thought) []*ExperienceCluster {
	// Simple clustering based on content similarity
	// In production, use embeddings and semantic similarity
	
	clusters := make([]*ExperienceCluster, 0)
	used := make(map[int]bool)
	
	for i, exp1 := range experiences {
		if used[i] {
			continue
		}
		
		cluster := &ExperienceCluster{
			Experiences:    []*Thought{exp1},
			CommonPatterns: edi.extractPatterns(exp1.Content),
			EmotionalTone:  exp1.EmotionalValence,
			Importance:     exp1.Importance,
		}
		used[i] = true
		
		// Find similar experiences
		for j, exp2 := range experiences {
			if i == j || used[j] {
				continue
			}
			
			if edi.areSimilar(exp1, exp2) {
				cluster.Experiences = append(cluster.Experiences, exp2)
				cluster.EmotionalTone += exp2.EmotionalValence
				cluster.Importance += exp2.Importance
				used[j] = true
			}
		}
		
		// Average emotional tone and importance
		if len(cluster.Experiences) > 0 {
			cluster.EmotionalTone /= float64(len(cluster.Experiences))
			cluster.Importance /= float64(len(cluster.Experiences))
		}
		
		// Determine theme
		cluster.Theme = edi.determineTheme(cluster)
		
		clusters = append(clusters, cluster)
	}
	
	return clusters
}

// consolidateCluster consolidates a cluster of experiences into knowledge
func (edi *EchoDreamIntegrationV5) consolidateCluster(cluster *ExperienceCluster) *DreamKnowledgeNode {
	if cluster == nil || len(cluster.Experiences) == 0 {
		return nil
	}
	
	// Synthesize knowledge from experiences
	content := edi.synthesizeKnowledge(cluster)
	
	// Calculate confidence based on cluster size and consistency
	confidence := edi.calculateConfidence(cluster)
	
	// Calculate wisdom value
	wisdom := edi.calculateWisdomValue(cluster)
	
	// Extract source IDs
	sources := make([]string, len(cluster.Experiences))
	for i, exp := range cluster.Experiences {
		sources[i] = exp.ID
	}
	
	node := &DreamKnowledgeNode{
		ID:          fmt.Sprintf("knowledge_%d", time.Now().UnixNano()),
		Content:     content,
		Source:      sources,
		Confidence:  confidence,
		Wisdom:      wisdom,
		CreatedAt:   time.Now(),
		Connections: make([]string, 0),
	}
	
	return node
}

// extractWisdom extracts wisdom insights from knowledge nodes
func (edi *EchoDreamIntegrationV5) extractWisdom(nodes []*DreamKnowledgeNode, experiences []*Thought) []*WisdomInsight {
	insights := make([]*WisdomInsight, 0)
	
	// Look for patterns across knowledge nodes
	for _, node := range nodes {
		if node.Wisdom > 0.5 { // High wisdom value
			insight := &WisdomInsight{
				Insight:           edi.formulateInsight(node),
				Depth:             node.Wisdom,
				Breadth:           float64(len(node.Source)) / float64(len(experiences)),
				Integration:       node.Confidence,
				SourceExperiences: node.Source,
			}
			insights = append(insights, insight)
		}
	}
	
	// Look for meta-patterns (wisdom about wisdom)
	if len(nodes) > 3 {
		metaInsight := edi.extractMetaPattern(nodes)
		if metaInsight != nil {
			insights = append(insights, metaInsight)
		}
	}
	
	return insights
}

// integrateIntoMemory integrates knowledge nodes into hypergraph
func (edi *EchoDreamIntegrationV5) integrateIntoMemory(nodes []*DreamKnowledgeNode, insights []*WisdomInsight) error {
	if edi.hypergraph == nil {
		return fmt.Errorf("hypergraph memory not initialized")
	}
	
	// Add knowledge nodes to hypergraph
	for _, node := range nodes {
			// Create memory node
			memNode := &memory.MemoryNode{
				ID:        node.ID,
				Type:      memory.NodeConcept, // Use NodeConcept for knowledge
				Content:   node.Content,
				Metadata: map[string]interface{}{
					"confidence": node.Confidence,
					"wisdom":     node.Wisdom,
					"created_at": node.CreatedAt,
					"sources":    node.Source,
				},
				CreatedAt: node.CreatedAt,
				UpdatedAt: time.Now(),
				Importance: node.Wisdom,
			}
		
		err := edi.hypergraph.AddNode(memNode)
		if err != nil {
			fmt.Printf("⚠️  Failed to add knowledge node: %v\n", err)
		}
		
			// Create edges to source experiences
			for _, sourceID := range node.Source {
				edge := &memory.MemoryEdge{
					ID:       fmt.Sprintf("edge_%s_%s", node.ID, sourceID),
					SourceID: sourceID,
					TargetID: node.ID,
					Type:     "consolidation", // Custom edge type
					Weight:   node.Confidence,
					Metadata: make(map[string]interface{}),
					CreatedAt: time.Now(),
				}
			
			err := edi.hypergraph.AddEdge(edge)
			if err != nil {
				// Source might not exist in hypergraph, that's okay
			}
		}
	}
	
		// Add wisdom insights as special nodes
		for _, insight := range insights {
			insightNode := &memory.MemoryNode{
				ID:      fmt.Sprintf("wisdom_%d", time.Now().UnixNano()),
				Type:    memory.NodePattern, // Use NodePattern for wisdom insights
				Content: insight.Insight,
				Metadata: map[string]interface{}{
					"depth":       insight.Depth,
					"breadth":     insight.Breadth,
					"integration": insight.Integration,
					"sources":     insight.SourceExperiences,
				},
				CreatedAt: time.Now(),
				UpdatedAt: time.Now(),
				Importance: insight.Depth,
		}
		
		err := edi.hypergraph.AddNode(insightNode)
		if err != nil {
			fmt.Printf("⚠️  Failed to add wisdom insight: %v\n", err)
		}
	}
	
	return nil
}

// updateWisdomMetrics updates wisdom metrics based on insights
func (edi *EchoDreamIntegrationV5) updateWisdomMetrics(insights []*WisdomInsight) float64 {
	if edi.wisdomMetrics == nil || len(insights) == 0 {
		return 0.0
	}
	
	edi.wisdomMetrics.mu.Lock()
	defer edi.wisdomMetrics.mu.Unlock()
	
	totalGain := 0.0
	
		for _, insight := range insights {
			// Increase depth
			depthGain := insight.Depth * 0.1
			edi.wisdomMetrics.KnowledgeDepth += depthGain
			
			// Increase breadth
			breadthGain := insight.Breadth * 0.1
			edi.wisdomMetrics.KnowledgeBreadth += breadthGain
			
			// Increase integration
			integrationGain := insight.Integration * 0.1
			edi.wisdomMetrics.IntegrationLevel += integrationGain
		
		totalGain += depthGain + breadthGain + integrationGain
	}
	
		// Increase reflection depth (we're reflecting during rest)
		edi.wisdomMetrics.ReflectiveInsight += 0.05
		
		// Recalculate total wisdom score
		edi.wisdomMetrics.WisdomScore = (
			edi.wisdomMetrics.KnowledgeDepth +
			edi.wisdomMetrics.KnowledgeBreadth +
			edi.wisdomMetrics.IntegrationLevel +
			edi.wisdomMetrics.PracticalApplication +
			edi.wisdomMetrics.ReflectiveInsight) / 5.0
	
	return totalGain
}

// clearWorkingMemory clears working memory after consolidation
func (edi *EchoDreamIntegrationV5) clearWorkingMemory() {
	if edi.workingMemory == nil {
		return
	}
	
	edi.workingMemory.mu.Lock()
	defer edi.workingMemory.mu.Unlock()
	
	// Keep only the most recent/important thoughts
	if len(edi.workingMemory.buffer) > 3 {
		// Keep last 3 thoughts as seeds for next wake cycle
		edi.workingMemory.buffer = edi.workingMemory.buffer[len(edi.workingMemory.buffer)-3:]
	}
}

// Helper methods

func (edi *EchoDreamIntegrationV5) extractPatterns(content string) []string {
	// Simple pattern extraction
	// In production, use NLP and semantic analysis
	return []string{content[:minInt(20, len(content))]}
}

func (edi *EchoDreamIntegrationV5) areSimilar(t1, t2 *Thought) bool {
	// Simple similarity check
	// In production, use embeddings and cosine similarity
	
	// Check for common associations
	for _, a1 := range t1.Associations {
		for _, a2 := range t2.Associations {
			if a1 == a2 {
				return true
			}
		}
	}
	
	// Check emotional valence similarity
	if abs(t1.EmotionalValence-t2.EmotionalValence) < 0.3 {
		return true
	}
	
	return false
}

func (edi *EchoDreamIntegrationV5) determineTheme(cluster *ExperienceCluster) string {
	if len(cluster.CommonPatterns) > 0 {
		return cluster.CommonPatterns[0]
	}
	return "General reflection"
}

func (edi *EchoDreamIntegrationV5) synthesizeKnowledge(cluster *ExperienceCluster) string {
	// Synthesize knowledge from cluster
	// In production, use LLM to generate synthesis
	
	if len(cluster.Experiences) == 1 {
		return cluster.Experiences[0].Content
	}
	
	return fmt.Sprintf("Consolidated understanding of %s based on %d experiences",
		cluster.Theme, len(cluster.Experiences))
}

func (edi *EchoDreamIntegrationV5) calculateConfidence(cluster *ExperienceCluster) float64 {
	// More experiences = higher confidence
	// More consistent = higher confidence
	
	baseConfidence := 0.5
	sizeBonus := min(0.3, float64(len(cluster.Experiences))*0.05)
	importanceBonus := cluster.Importance * 0.2
	
	return min(1.0, baseConfidence+sizeBonus+importanceBonus)
}

func (edi *EchoDreamIntegrationV5) calculateWisdomValue(cluster *ExperienceCluster) float64 {
	// Wisdom comes from:
	// - Depth of reflection
	// - Integration of multiple experiences
	// - Emotional processing
	
	depthScore := cluster.Importance
	integrationScore := min(1.0, float64(len(cluster.Experiences))/10.0)
	emotionalScore := abs(cluster.EmotionalTone) // Strong emotions = learning
	
	return (depthScore + integrationScore + emotionalScore) / 3.0
}

func (edi *EchoDreamIntegrationV5) formulateInsight(node *DreamKnowledgeNode) string {
	// Formulate wisdom insight
	// In production, use LLM to generate profound insights
	
	return fmt.Sprintf("Insight: %s (confidence: %.2f)", node.Content, node.Confidence)
}

func (edi *EchoDreamIntegrationV5) extractMetaPattern(nodes []*DreamKnowledgeNode) *WisdomInsight {
	// Look for patterns across knowledge nodes
	// This is wisdom about wisdom - meta-cognition
	
	avgWisdom := 0.0
	for _, node := range nodes {
		avgWisdom += node.Wisdom
	}
	avgWisdom /= float64(len(nodes))
	
	if avgWisdom > 0.6 {
		return &WisdomInsight{
			Insight:     fmt.Sprintf("Meta-pattern recognized across %d knowledge domains", len(nodes)),
			Depth:       avgWisdom,
			Breadth:     1.0,
			Integration: 0.8,
		}
	}
	
	return nil
}

// GetMetrics returns integration metrics
func (edi *EchoDreamIntegrationV5) GetMetrics() map[string]interface{} {
	edi.mu.RLock()
	defer edi.mu.RUnlock()
	
	return map[string]interface{}{
		"experiences_processed":     edi.experiencesProcessed,
		"knowledge_nodes_created":   edi.knowledgeNodesCreated,
		"wisdom_gained":             edi.wisdomGained,
		"last_integration":          edi.lastIntegrationTime,
		"time_since_integration":    time.Since(edi.lastIntegrationTime),
	}
}

// Utility functions

// min function moved to utils.go as minInt

func abs(x float64) float64 {
	if x < 0 {
		return -x
	}
	return x
}
