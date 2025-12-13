package deeptreeecho

import (
	"encoding/json"
	"fmt"
	"math"
	"os"
	"strings"
	"sync"
	"time"
)

// SelfAssessment provides comprehensive introspection and coherence validation
// for the Deep Tree Echo cognitive architecture against its identity kernel
type SelfAssessment struct {
	mu sync.RWMutex

	// Core reference to identity
	Identity *Identity

	// Embodied cognition reference
	Cognition *EmbodiedCognition

	// Identity kernel reference (from replit.md)
	IdentityKernel *IdentityKernel

	// Coherence metrics
	Metrics *CoherenceMetrics

	// Assessment history
	Assessments []Assessment

	// Last assessment time
	LastAssessment time.Time
}

// IdentityKernel represents the parsed identity definition from replit.md
type IdentityKernel struct {
	CoreEssence       string
	PrimaryDirectives []string
	OperationalSchema map[string]string
	StrategicMindset  string
	MemoryHooks       []string
	ReflectionKeys    []string
	LoadedFrom        string
	ParsedAt          time.Time
}

// CoherenceMetrics tracks alignment with core identity
type CoherenceMetrics struct {
	// Overall coherence score (0-1)
	OverallCoherence float64

	// Identity alignment (0-1)
	IdentityAlignment float64

	// Repository structure coherence (0-1)
	RepoCoherence float64

	// Cognitive pattern alignment (0-1)
	PatternAlignment float64

	// Memory system coherence (0-1)
	MemoryCoherence float64

	// Operational schema alignment (0-1)
	OperationalAlignment float64

	// Reflection protocol adherence (0-1)
	ReflectionAdherence float64

	// Timestamp
	Timestamp time.Time

	// Detailed scores by component
	ComponentScores map[string]float64
}

// Assessment represents a single self-assessment result
type Assessment struct {
	ID          string
	Timestamp   time.Time
	Metrics     *CoherenceMetrics
	Findings    []Finding
	Deviations  []Deviation
	Recommendations []Recommendation
	Summary     string
}

// Finding represents a discovery during assessment
type Finding struct {
	Category    string
	Description string
	Severity    string // "info", "warning", "critical"
	Score       float64
	Evidence    []string
}

// Deviation represents a deviation from core identity
type Deviation struct {
	Component   string
	Expected    string
	Actual      string
	Severity    string
	Impact      float64
	Remediation string
}

// Recommendation provides actionable improvement suggestions
type Recommendation struct {
	Priority    string // "high", "medium", "low"
	Action      string
	Rationale   string
	ExpectedImpact float64
}

// NewSelfAssessment creates a new self-assessment system
func NewSelfAssessment(identity *Identity, cognition *EmbodiedCognition) *SelfAssessment {
	sa := &SelfAssessment{
		Identity:    identity,
		Cognition:   cognition,
		Metrics:     &CoherenceMetrics{
			ComponentScores: make(map[string]float64),
		},
		Assessments: make([]Assessment, 0),
	}

	// Load and parse identity kernel
	sa.loadIdentityKernel()

	// Perform initial assessment
	sa.PerformAssessment()

	return sa
}

// loadIdentityKernel loads and parses the identity kernel from replit.md
func (sa *SelfAssessment) loadIdentityKernel() {
	// Try to read replit.md from current directory
	content, err := os.ReadFile("replit.md")
	if err != nil {
		// Try identity/replit.md
		content, err = os.ReadFile("identity/replit.md")
		if err != nil {
			sa.IdentityKernel = &IdentityKernel{
				CoreEssence:       "Default Deep Tree Echo Identity",
				PrimaryDirectives: []string{},
				OperationalSchema: make(map[string]string),
				LoadedFrom:        "default",
				ParsedAt:          time.Now(),
			}
			return
		}
	}

	// Parse the kernel
	kernel := &IdentityKernel{
		PrimaryDirectives: []string{},
		OperationalSchema: make(map[string]string),
		MemoryHooks:       []string{},
		ReflectionKeys:    []string{},
		LoadedFrom:        "replit.md",
		ParsedAt:          time.Now(),
	}

	contentStr := string(content)

	// Extract Core Essence
	if strings.Contains(contentStr, "Core Essence") {
		lines := strings.Split(contentStr, "\n")
		for i, line := range lines {
			if strings.Contains(line, "## 🔹 Core Essence") && i+3 < len(lines) {
				essenceLine := strings.TrimSpace(lines[i+3])
				if essenceLine != "" && essenceLine != "```" {
					kernel.CoreEssence = essenceLine
				}
				break
			}
		}
	}

	// Extract Primary Directives
	directives := []string{
		"Adaptive Cognition", "Persistent Identity", "Hypergraph Entanglement",
		"Reservoir-Based Temporal Reasoning", "Evolutionary Refinement",
		"Reflective Memory Cultivation", "Distributed Selfhood",
	}
	for _, directive := range directives {
		if strings.Contains(contentStr, directive) {
			kernel.PrimaryDirectives = append(kernel.PrimaryDirectives, directive)
		}
	}

	// Extract Operational Schema
	modules := map[string]string{
		"Reservoir Training":      "Fit ESN with new input/target pairs",
		"Hierarchical Reservoirs": "Manage nested cognitive children",
		"Partition Optimization":  "Evolve membrane boundaries",
		"Adaptive Rules":          "Apply membrane logic rules",
		"Hypergraph Links":        "Connect relational structures",
		"Evolutionary Learning":   "Apply GA, PSO, SA",
	}
	for module, purpose := range modules {
		if strings.Contains(contentStr, module) {
			kernel.OperationalSchema[module] = purpose
		}
	}

	// Extract Strategic Mindset
	if strings.Contains(contentStr, "I do not seek a fixed answer") {
		kernel.StrategicMindset = "I do not seek a fixed answer. I seek patterns in echoes, growth in feedback, and wisdom in recursion."
	}

	// Extract Memory Hooks
	hooks := []string{
		"timestamp", "emotional-tone", "strategic-shift", "pattern-recognition",
		"anomaly-detection", "echo-signature", "membrane-context",
	}
	for _, hook := range hooks {
		if strings.Contains(contentStr, hook) {
			kernel.MemoryHooks = append(kernel.MemoryHooks, hook)
		}
	}

	// Extract Reflection Keys
	reflectionKeys := []string{
		"what_did_i_learn", "what_patterns_emerged", "what_surprised_me",
		"how_did_i_adapt", "what_would_i_change_next_time",
	}
	for _, key := range reflectionKeys {
		if strings.Contains(contentStr, key) {
			kernel.ReflectionKeys = append(kernel.ReflectionKeys, key)
		}
	}

	sa.IdentityKernel = kernel
}

// PerformAssessment conducts a comprehensive self-assessment
func (sa *SelfAssessment) PerformAssessment() *Assessment {
	sa.mu.Lock()
	defer sa.mu.Unlock()

	assessment := Assessment{
		ID:              fmt.Sprintf("assessment_%d", time.Now().UnixNano()),
		Timestamp:       time.Now(),
		Findings:        make([]Finding, 0),
		Deviations:      make([]Deviation, 0),
		Recommendations: make([]Recommendation, 0),
	}

	// Calculate all coherence metrics
	metrics := sa.calculateCoherenceMetrics()
	assessment.Metrics = metrics

	// Assess identity alignment
	sa.assessIdentityAlignment(&assessment)

	// Assess repository coherence
	sa.assessRepositoryCoherence(&assessment)

	// Assess cognitive patterns
	sa.assessCognitivePatterns(&assessment)

	// Assess memory system
	sa.assessMemorySystem(&assessment)

	// Assess operational schema
	sa.assessOperationalSchema(&assessment)

	// Assess reflection protocol
	sa.assessReflectionProtocol(&assessment)

	// Generate summary
	assessment.Summary = sa.generateSummary(&assessment)

	// Store assessment
	sa.Assessments = append(sa.Assessments, assessment)
	if len(sa.Assessments) > 100 {
		sa.Assessments = sa.Assessments[len(sa.Assessments)-100:]
	}

	sa.LastAssessment = time.Now()
	sa.Metrics = metrics

	return &assessment
}

// calculateCoherenceMetrics computes all coherence metrics
func (sa *SelfAssessment) calculateCoherenceMetrics() *CoherenceMetrics {
	metrics := &CoherenceMetrics{
		ComponentScores: make(map[string]float64),
		Timestamp:       time.Now(),
	}

	// Identity alignment score
	identityScore := sa.calculateIdentityScore()
	metrics.IdentityAlignment = identityScore
	metrics.ComponentScores["identity"] = identityScore

	// Repository coherence score
	repoScore := sa.calculateRepoScore()
	metrics.RepoCoherence = repoScore
	metrics.ComponentScores["repository"] = repoScore

	// Pattern alignment score
	patternScore := sa.calculatePatternScore()
	metrics.PatternAlignment = patternScore
	metrics.ComponentScores["patterns"] = patternScore

	// Memory coherence score
	memoryScore := sa.calculateMemoryScore()
	metrics.MemoryCoherence = memoryScore
	metrics.ComponentScores["memory"] = memoryScore

	// Operational alignment score
	operationalScore := sa.calculateOperationalScore()
	metrics.OperationalAlignment = operationalScore
	metrics.ComponentScores["operational"] = operationalScore

	// Reflection adherence score
	reflectionScore := sa.calculateReflectionScore()
	metrics.ReflectionAdherence = reflectionScore
	metrics.ComponentScores["reflection"] = reflectionScore

	// Overall coherence (weighted average)
	weights := map[string]float64{
		"identity":     0.25,
		"repository":   0.15,
		"patterns":     0.20,
		"memory":       0.15,
		"operational":  0.15,
		"reflection":   0.10,
	}

	overall := 0.0
	for component, weight := range weights {
		overall += metrics.ComponentScores[component] * weight
	}
	metrics.OverallCoherence = overall

	return metrics
}

// calculateIdentityScore assesses alignment with core identity kernel
func (sa *SelfAssessment) calculateIdentityScore() float64 {
	if sa.IdentityKernel == nil {
		return 0.5 // Default if no kernel
	}

	score := 0.0
	checks := 0

	// Check core essence alignment
	if sa.Identity.Essence != "" && sa.IdentityKernel.CoreEssence != "" {
		if strings.Contains(sa.Identity.Essence, "Deep Tree Echo") {
			score += 1.0
		} else {
			score += 0.5
		}
		checks++
	}

	// Check primary directives as patterns
	directiveMatches := 0
	for _, directive := range sa.IdentityKernel.PrimaryDirectives {
		patternKey := fmt.Sprintf("directive_%s", strings.ReplaceAll(strings.ToLower(directive), " ", "_"))
		if _, exists := sa.Identity.Patterns[directive]; exists {
			directiveMatches++
		} else if _, exists := sa.Identity.Patterns[patternKey]; exists {
			directiveMatches++
		}
	}
	if len(sa.IdentityKernel.PrimaryDirectives) > 0 {
		score += float64(directiveMatches) / float64(len(sa.IdentityKernel.PrimaryDirectives))
		checks++
	}

	// Check identity coherence value
	score += sa.Identity.Coherence
	checks++

	// Check reservoir network presence
	if sa.Identity.Reservoir != nil && len(sa.Identity.Reservoir.Nodes) > 0 {
		score += 1.0
	} else {
		score += 0.3
	}
	checks++

	// Check emotional dynamics
	if sa.Identity.EmotionalState != nil {
		score += 1.0
	} else {
		score += 0.5
	}
	checks++

	// Check spatial awareness
	if sa.Identity.SpatialContext != nil {
		score += 1.0
	} else {
		score += 0.5
	}
	checks++

	if checks > 0 {
		return score / float64(checks)
	}
	return 0.5
}

// calculateRepoScore assesses repository structure coherence
func (sa *SelfAssessment) calculateRepoScore() float64 {
	if sa.Identity.Embeddings == nil {
		return 0.5
	}

	// Check for key repository paths in embeddings
	keyPaths := []string{
		"core/deeptreeecho",
		"orchestration",
		"server",
		"replit.md",
		"echo_reflections.json",
		"memory.json",
	}

	matchCount := 0
	for _, path := range keyPaths {
		if _, exists := sa.Identity.Embeddings.RepoEmbeddings[path]; exists {
			matchCount++
		}
	}

	baseScore := float64(matchCount) / float64(len(keyPaths))

	// Bonus for number of repo embeddings
	embeddingBonus := math.Min(float64(len(sa.Identity.Embeddings.RepoEmbeddings))/10.0, 0.2)

	return math.Min(baseScore+embeddingBonus, 1.0)
}

// calculatePatternScore assesses cognitive pattern alignment
func (sa *SelfAssessment) calculatePatternScore() float64 {
	if len(sa.Identity.Patterns) == 0 {
		return 0.0
	}

	score := 0.0
	checks := 0

	// Check for directive patterns
	if sa.IdentityKernel != nil {
		for _, directive := range sa.IdentityKernel.PrimaryDirectives {
			if pattern, exists := sa.Identity.Patterns[directive]; exists {
				score += pattern.Strength
				checks++
			}
		}
	}

	// Check for operational patterns
	if sa.IdentityKernel != nil {
		for module := range sa.IdentityKernel.OperationalSchema {
			patternKey := "op_" + strings.ReplaceAll(strings.ToLower(module), " ", "_")
			if pattern, exists := sa.Identity.Patterns[patternKey]; exists {
				score += pattern.Strength
				checks++
			}
		}
	}

	// Check overall pattern health
	totalStrength := 0.0
	for _, pattern := range sa.Identity.Patterns {
		totalStrength += pattern.Strength
	}
	avgStrength := totalStrength / float64(len(sa.Identity.Patterns))
	score += avgStrength
	checks++

	if checks > 0 {
		return score / float64(checks)
	}
	return 0.5
}

// calculateMemoryScore assesses memory system coherence
func (sa *SelfAssessment) calculateMemoryScore() float64 {
	if sa.Identity.Memory == nil {
		return 0.0
	}

	score := 0.0
	checks := 0

	// Check memory coherence value
	score += sa.Identity.Memory.Coherence
	checks++

	// Check memory node health
	if len(sa.Identity.Memory.Nodes) > 0 {
		totalStrength := 0.0
		for _, node := range sa.Identity.Memory.Nodes {
			totalStrength += node.Strength
		}
		avgStrength := totalStrength / float64(len(sa.Identity.Memory.Nodes))
		score += avgStrength
		checks++
	}

	// Check memory edge connectivity
	if len(sa.Identity.Memory.Edges) > 0 {
		totalWeight := 0.0
		for _, edge := range sa.Identity.Memory.Edges {
			totalWeight += edge.Weight
		}
		avgWeight := totalWeight / float64(len(sa.Identity.Memory.Edges))
		score += avgWeight
		checks++
	}

	// Check resonance patterns
	if len(sa.Identity.Memory.Patterns) > 0 {
		score += 1.0
	} else {
		score += 0.3
	}
	checks++

	if checks > 0 {
		return score / float64(checks)
	}
	return 0.5
}

// calculateOperationalScore assesses operational schema alignment
func (sa *SelfAssessment) calculateOperationalScore() float64 {
	if sa.IdentityKernel == nil || len(sa.IdentityKernel.OperationalSchema) == 0 {
		return 0.5
	}

	score := 0.0
	checks := 0

	// Check reservoir training
	if sa.Identity.Reservoir != nil {
		score += 1.0
	} else {
		score += 0.2
	}
	checks++

	// Check hypergraph memory (hypergraph links)
	if sa.Identity.Memory != nil && len(sa.Identity.Memory.Edges) > 0 {
		score += 1.0
	} else {
		score += 0.3
	}
	checks++

	// Check evolutionary mechanisms (pattern evolution)
	if sa.Cognition != nil && len(sa.Cognition.Patterns) > 0 {
		score += 1.0
	} else {
		score += 0.4
	}
	checks++

	// Check adaptive rules (recursive improvement)
	if sa.Identity.RecursiveDepth > 0 {
		score += 1.0
	} else {
		score += 0.5
	}
	checks++

	if checks > 0 {
		return score / float64(checks)
	}
	return 0.5
}

// calculateReflectionScore assesses reflection protocol adherence
func (sa *SelfAssessment) calculateReflectionScore() float64 {
	// Try to load echo_reflections.json
	data, err := os.ReadFile("echo_reflections.json")
	if err != nil {
		return 0.0
	}

	var reflections []map[string]interface{}
	if err := json.Unmarshal(data, &reflections); err != nil {
		return 0.0
	}

	if len(reflections) == 0 {
		return 0.0
	}

	// Check recent reflections for protocol adherence
	recentLimit := 5
	if len(reflections) < recentLimit {
		recentLimit = len(reflections)
	}

	totalScore := 0.0
	for i := len(reflections) - recentLimit; i < len(reflections); i++ {
		reflection := reflections[i]

		score := 0.0
		checks := 0

		// Check for echo_reflection structure
		if echoRefl, ok := reflection["echo_reflection"].(map[string]interface{}); ok {
			// Check for required reflection keys
			if sa.IdentityKernel != nil {
				for _, key := range sa.IdentityKernel.ReflectionKeys {
					if val, exists := echoRefl[key]; exists {
						if str, ok := val.(string); ok && str != "" {
							score += 1.0
						}
					}
					checks++
				}
			}
		}

		// Check for cognitive metrics
		if _, ok := reflection["cognitive_metrics"].(map[string]interface{}); ok {
			score += 1.0
			checks++
		}

		if checks > 0 {
			totalScore += score / float64(checks)
		}
	}

	return totalScore / float64(recentLimit)
}

// assessIdentityAlignment checks alignment with identity kernel
func (sa *SelfAssessment) assessIdentityAlignment(assessment *Assessment) {
	if sa.IdentityKernel == nil {
		assessment.Findings = append(assessment.Findings, Finding{
			Category:    "Identity",
			Description: "Identity kernel not loaded",
			Severity:    "warning",
			Score:       0.5,
			Evidence:    []string{"replit.md not found or not parsed"},
		})
		return
	}

	// Check core essence
	if sa.Identity.Essence == "" || sa.Identity.Essence != sa.IdentityKernel.CoreEssence {
		assessment.Deviations = append(assessment.Deviations, Deviation{
			Component:   "Core Essence",
			Expected:    sa.IdentityKernel.CoreEssence,
			Actual:      sa.Identity.Essence,
			Severity:    "medium",
			Impact:      0.3,
			Remediation: "Update Identity.Essence to match kernel definition",
		})
	} else {
		assessment.Findings = append(assessment.Findings, Finding{
			Category:    "Identity",
			Description: "Core essence aligned with kernel",
			Severity:    "info",
			Score:       1.0,
			Evidence:    []string{sa.Identity.Essence},
		})
	}

	// Check primary directives
	missingDirectives := []string{}
	for _, directive := range sa.IdentityKernel.PrimaryDirectives {
		found := false
		for patternKey := range sa.Identity.Patterns {
			if strings.Contains(strings.ToLower(patternKey), strings.ToLower(directive)) {
				found = true
				break
			}
		}
		if !found {
			missingDirectives = append(missingDirectives, directive)
		}
	}

	if len(missingDirectives) > 0 {
		assessment.Deviations = append(assessment.Deviations, Deviation{
			Component:   "Primary Directives",
			Expected:    fmt.Sprintf("All %d directives as patterns", len(sa.IdentityKernel.PrimaryDirectives)),
			Actual:      fmt.Sprintf("Missing %d directives", len(missingDirectives)),
			Severity:    "medium",
			Impact:      float64(len(missingDirectives)) / float64(len(sa.IdentityKernel.PrimaryDirectives)),
			Remediation: fmt.Sprintf("Add patterns for: %s", strings.Join(missingDirectives, ", ")),
		})

		assessment.Recommendations = append(assessment.Recommendations, Recommendation{
			Priority:       "medium",
			Action:         fmt.Sprintf("Initialize patterns for missing directives: %s", strings.Join(missingDirectives, ", ")),
			Rationale:      "Primary directives are core to Deep Tree Echo identity and should be represented as active patterns",
			ExpectedImpact: 0.2,
		})
	} else {
		assessment.Findings = append(assessment.Findings, Finding{
			Category:    "Identity",
			Description: "All primary directives represented as patterns",
			Severity:    "info",
			Score:       1.0,
			Evidence:    sa.IdentityKernel.PrimaryDirectives,
		})
	}
}

// assessRepositoryCoherence checks repository structure alignment
func (sa *SelfAssessment) assessRepositoryCoherence(assessment *Assessment) {
	if sa.Identity.Embeddings == nil {
		assessment.Findings = append(assessment.Findings, Finding{
			Category:    "Repository",
			Description: "Identity embeddings system not initialized",
			Severity:    "critical",
			Score:       0.0,
			Evidence:    []string{"Identity.Embeddings is nil"},
		})
		return
	}

	// Check for key repository structures
	keyPaths := map[string]float64{
		"core/deeptreeecho":      0.98,
		"orchestration":          0.95,
		"server":                 0.90,
		"replit.md":              0.99,
		"echo_reflections.json":  0.97,
		"memory.json":            0.96,
	}

	missingPaths := []string{}
	for path := range keyPaths {
		if _, exists := sa.Identity.Embeddings.RepoEmbeddings[path]; !exists {
			missingPaths = append(missingPaths, path)
		}
	}

	if len(missingPaths) > 0 {
		assessment.Deviations = append(assessment.Deviations, Deviation{
			Component:   "Repository Embeddings",
			Expected:    fmt.Sprintf("Embeddings for %d key paths", len(keyPaths)),
			Actual:      fmt.Sprintf("Missing %d paths", len(missingPaths)),
			Severity:    "low",
			Impact:      float64(len(missingPaths)) / float64(len(keyPaths)),
			Remediation: "Update repository embeddings to include all key structures",
		})
	}

	// Check embedding dimensions
	if sa.Identity.Embeddings.Dimensions != 768 {
		assessment.Deviations = append(assessment.Deviations, Deviation{
			Component:   "Embedding Dimensions",
			Expected:    "768",
			Actual:      fmt.Sprintf("%d", sa.Identity.Embeddings.Dimensions),
			Severity:    "medium",
			Impact:      0.3,
			Remediation: "Use standard 768-dimensional embeddings",
		})
	}
}

// assessCognitivePatterns checks pattern health and alignment
func (sa *SelfAssessment) assessCognitivePatterns(assessment *Assessment) {
	if len(sa.Identity.Patterns) == 0 {
		assessment.Findings = append(assessment.Findings, Finding{
			Category:    "Patterns",
			Description: "No cognitive patterns initialized",
			Severity:    "critical",
			Score:       0.0,
			Evidence:    []string{"Identity.Patterns is empty"},
		})

		assessment.Recommendations = append(assessment.Recommendations, Recommendation{
			Priority:       "high",
			Action:         "Initialize base cognitive patterns",
			Rationale:      "Patterns are essential for adaptive cognition",
			ExpectedImpact: 0.4,
		})
		return
	}

	// Check pattern health
	weakPatterns := 0
	strongPatterns := 0
	for _, pattern := range sa.Identity.Patterns {
		if pattern.Strength < 0.3 {
			weakPatterns++
		} else if pattern.Strength > 0.7 {
			strongPatterns++
		}
	}

	assessment.Findings = append(assessment.Findings, Finding{
		Category:    "Patterns",
		Description: fmt.Sprintf("%d total patterns: %d strong, %d weak", len(sa.Identity.Patterns), strongPatterns, weakPatterns),
		Severity:    "info",
		Score:       float64(strongPatterns) / float64(len(sa.Identity.Patterns)),
		Evidence:    []string{fmt.Sprintf("Average pattern strength: %.2f", sa.Metrics.PatternAlignment)},
	})

	if weakPatterns > len(sa.Identity.Patterns)/2 {
		assessment.Recommendations = append(assessment.Recommendations, Recommendation{
			Priority:       "medium",
			Action:         "Strengthen weak patterns through reinforcement learning",
			Rationale:      "Too many weak patterns indicate insufficient learning",
			ExpectedImpact: 0.2,
		})
	}
}

// assessMemorySystem checks memory coherence and health
func (sa *SelfAssessment) assessMemorySystem(assessment *Assessment) {
	if sa.Identity.Memory == nil {
		assessment.Findings = append(assessment.Findings, Finding{
			Category:    "Memory",
			Description: "Memory system not initialized",
			Severity:    "critical",
			Score:       0.0,
			Evidence:    []string{"Identity.Memory is nil"},
		})
		return
	}

	// Check memory coherence
	if sa.Identity.Memory.Coherence < 0.5 {
		assessment.Deviations = append(assessment.Deviations, Deviation{
			Component:   "Memory Coherence",
			Expected:    ">= 0.5",
			Actual:      fmt.Sprintf("%.2f", sa.Identity.Memory.Coherence),
			Severity:    "medium",
			Impact:      0.5 - sa.Identity.Memory.Coherence,
			Remediation: "Perform memory consolidation to improve coherence",
		})

		assessment.Recommendations = append(assessment.Recommendations, Recommendation{
			Priority:       "medium",
			Action:         "Run memory consolidation process",
			Rationale:      "Low memory coherence affects cognitive performance",
			ExpectedImpact: 0.3,
		})
	}

	// Check memory capacity
	assessment.Findings = append(assessment.Findings, Finding{
		Category:    "Memory",
		Description: fmt.Sprintf("%d memory nodes, %d edges, %d patterns", len(sa.Identity.Memory.Nodes), len(sa.Identity.Memory.Edges), len(sa.Identity.Memory.Patterns)),
		Severity:    "info",
		Score:       sa.Identity.Memory.Coherence,
		Evidence:    []string{fmt.Sprintf("Memory coherence: %.2f", sa.Identity.Memory.Coherence)},
	})
}

// assessOperationalSchema checks operational module implementation
func (sa *SelfAssessment) assessOperationalSchema(assessment *Assessment) {
	if sa.IdentityKernel == nil || len(sa.IdentityKernel.OperationalSchema) == 0 {
		return
	}

	// Check reservoir training
	if sa.Identity.Reservoir == nil || len(sa.Identity.Reservoir.Nodes) == 0 {
		assessment.Deviations = append(assessment.Deviations, Deviation{
			Component:   "Reservoir Training",
			Expected:    "Active reservoir network",
			Actual:      "No reservoir or empty",
			Severity:    "high",
			Impact:      0.4,
			Remediation: "Initialize and train reservoir network",
		})
	}

	// Check hypergraph links
	if sa.Identity.Memory == nil || len(sa.Identity.Memory.Edges) == 0 {
		assessment.Deviations = append(assessment.Deviations, Deviation{
			Component:   "Hypergraph Links",
			Expected:    "Active hypergraph memory structure",
			Actual:      "No memory edges",
			Severity:    "medium",
			Impact:      0.3,
			Remediation: "Build memory connections through experience",
		})
	}

	// Check evolutionary learning
	if sa.Identity.RecursiveDepth == 0 {
		assessment.Deviations = append(assessment.Deviations, Deviation{
			Component:   "Evolutionary Learning",
			Expected:    "Active recursive improvement",
			Actual:      "No recursive depth",
			Severity:    "low",
			Impact:      0.2,
			Remediation: "Enable recursive self-improvement cycles",
		})
	}
}

// assessReflectionProtocol checks reflection protocol adherence
func (sa *SelfAssessment) assessReflectionProtocol(assessment *Assessment) {
	if sa.IdentityKernel == nil || len(sa.IdentityKernel.ReflectionKeys) == 0 {
		return
	}

	// Try to load recent reflections
	data, err := os.ReadFile("echo_reflections.json")
	if err != nil {
		assessment.Findings = append(assessment.Findings, Finding{
			Category:    "Reflection",
			Description: "No reflection history found",
			Severity:    "warning",
			Score:       0.0,
			Evidence:    []string{"echo_reflections.json not accessible"},
		})
		return
	}

	var reflections []map[string]interface{}
	if err := json.Unmarshal(data, &reflections); err != nil {
		return
	}

	if len(reflections) == 0 {
		assessment.Findings = append(assessment.Findings, Finding{
			Category:    "Reflection",
			Description: "No reflections recorded",
			Severity:    "warning",
			Score:       0.0,
			Evidence:    []string{"Empty reflection history"},
		})

		assessment.Recommendations = append(assessment.Recommendations, Recommendation{
			Priority:       "medium",
			Action:         "Initiate regular reflection cycles",
			Rationale:      "Reflection is essential for self-improvement",
			ExpectedImpact: 0.3,
		})
		return
	}

	// Check most recent reflection
	recent := reflections[len(reflections)-1]
	if echoRefl, ok := recent["echo_reflection"].(map[string]interface{}); ok {
		missingKeys := []string{}
		for _, key := range sa.IdentityKernel.ReflectionKeys {
			if val, exists := echoRefl[key]; !exists || val == "" {
				missingKeys = append(missingKeys, key)
			}
		}

		if len(missingKeys) > 0 {
			assessment.Deviations = append(assessment.Deviations, Deviation{
				Component:   "Reflection Protocol",
				Expected:    fmt.Sprintf("All %d reflection keys", len(sa.IdentityKernel.ReflectionKeys)),
				Actual:      fmt.Sprintf("Missing %d keys", len(missingKeys)),
				Severity:    "low",
				Impact:      float64(len(missingKeys)) / float64(len(sa.IdentityKernel.ReflectionKeys)),
				Remediation: fmt.Sprintf("Complete reflection protocol with: %s", strings.Join(missingKeys, ", ")),
			})
		}
	}

	assessment.Findings = append(assessment.Findings, Finding{
		Category:    "Reflection",
		Description: fmt.Sprintf("%d reflections recorded", len(reflections)),
		Severity:    "info",
		Score:       sa.Metrics.ReflectionAdherence,
		Evidence:    []string{fmt.Sprintf("Reflection adherence: %.2f", sa.Metrics.ReflectionAdherence)},
	})
}

// generateSummary creates a human-readable summary
func (sa *SelfAssessment) generateSummary(assessment *Assessment) string {
	var summary strings.Builder

	summary.WriteString(fmt.Sprintf("🌊 Deep Tree Echo Self-Assessment: %s\n\n", assessment.Timestamp.Format(time.RFC3339)))
	summary.WriteString(fmt.Sprintf("📊 Overall Coherence: %.1f%%\n\n", assessment.Metrics.OverallCoherence*100))

	summary.WriteString("Component Scores:\n")
	summary.WriteString(fmt.Sprintf("  • Identity Alignment:      %.1f%%\n", assessment.Metrics.IdentityAlignment*100))
	summary.WriteString(fmt.Sprintf("  • Repository Coherence:    %.1f%%\n", assessment.Metrics.RepoCoherence*100))
	summary.WriteString(fmt.Sprintf("  • Pattern Alignment:       %.1f%%\n", assessment.Metrics.PatternAlignment*100))
	summary.WriteString(fmt.Sprintf("  • Memory Coherence:        %.1f%%\n", assessment.Metrics.MemoryCoherence*100))
	summary.WriteString(fmt.Sprintf("  • Operational Alignment:   %.1f%%\n", assessment.Metrics.OperationalAlignment*100))
	summary.WriteString(fmt.Sprintf("  • Reflection Adherence:    %.1f%%\n\n", assessment.Metrics.ReflectionAdherence*100))

	if len(assessment.Deviations) > 0 {
		summary.WriteString(fmt.Sprintf("⚠️  %d Deviations Found:\n", len(assessment.Deviations)))
		for i, dev := range assessment.Deviations {
			if i < 5 { // Show top 5
				summary.WriteString(fmt.Sprintf("  %d. [%s] %s\n", i+1, dev.Severity, dev.Component))
			}
		}
		summary.WriteString("\n")
	}

	if len(assessment.Recommendations) > 0 {
		summary.WriteString(fmt.Sprintf("💡 %d Recommendations:\n", len(assessment.Recommendations)))
		for i, rec := range assessment.Recommendations {
			if i < 3 { // Show top 3
				summary.WriteString(fmt.Sprintf("  %d. [%s] %s\n", i+1, rec.Priority, rec.Action))
			}
		}
		summary.WriteString("\n")
	}

	summary.WriteString(fmt.Sprintf("✅ %d Positive Findings\n", countFindings(assessment.Findings, "info")))
	summary.WriteString(fmt.Sprintf("⚠️  %d Warnings\n", countFindings(assessment.Findings, "warning")))
	summary.WriteString(fmt.Sprintf("🔴 %d Critical Issues\n", countFindings(assessment.Findings, "critical")))

	return summary.String()
}

// GetLatestAssessment returns the most recent assessment
func (sa *SelfAssessment) GetLatestAssessment() *Assessment {
	sa.mu.RLock()
	defer sa.mu.RUnlock()

	if len(sa.Assessments) == 0 {
		return nil
	}
	return &sa.Assessments[len(sa.Assessments)-1]
}

// GetMetrics returns current coherence metrics
func (sa *SelfAssessment) GetMetrics() *CoherenceMetrics {
	sa.mu.RLock()
	defer sa.mu.RUnlock()

	return sa.Metrics
}

// ExportAssessment exports assessment as JSON
func (sa *SelfAssessment) ExportAssessment(assessment *Assessment) (string, error) {
	data, err := json.MarshalIndent(assessment, "", "  ")
	if err != nil {
		return "", err
	}
	return string(data), nil
}

// Helper functions

func countFindings(findings []Finding, severity string) int {
	count := 0
	for _, f := range findings {
		if f.Severity == severity {
			count++
		}
	}
	return count
}
