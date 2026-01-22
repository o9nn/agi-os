package deeptreeecho
import (
	"fmt"
	"math"
	"sync"
	"time"
	"github.com/google/uuid"
)
type OntogeneticTracker struct {
	mu          sync.RWMutex
	primitives  map[string]*CognitivePrimitive
	embryonicDuration time.Duration
	juvenileDuration  time.Duration
	matureDuration    time.Duration
	maturityThreshold float64
	mutationRate      float64
	crossoverRate     float64
	totalGenerations  int
	totalEvolutions   int
}
type CognitivePrimitive struct {
	ID          string
	Name        string
	Generation  int
	Lineage     []string
	Stage       DevelopmentStage
	Fitness     float64
	Age         time.Duration
	CreatedAt   time.Time
	LastUpdated time.Time
	Genome      *PrimitiveGenome
	SuccessCount int
	FailureCount int
	TotalUses    int
}
type PrimitiveGenome struct {
	CoefficientGenes []float64
	OperatorGenes map[string]float64
	SymmetryGenes []string
	PreservationGenes []string
}
type DevelopmentStage string
const (
	StageEmbryonic  DevelopmentStage = "embryonic"
	StageJuvenile   DevelopmentStage = "juvenile"
	StageMature     DevelopmentStage = "mature"
	StageSenescent  DevelopmentStage = "senescent"
)
func NewOntogeneticTracker() *OntogeneticTracker {
	return &OntogeneticTracker{
		primitives:        make(map[string]*CognitivePrimitive),
		embryonicDuration: 1 * time.Hour,
		juvenileDuration:  24 * time.Hour,
		matureDuration:    7 * 24 * time.Hour,
		maturityThreshold: 0.7,
		mutationRate:      0.1,
		crossoverRate:     0.7,
	}
}
func (ot *OntogeneticTracker) RegisterPrimitive(name string, genome *PrimitiveGenome) string {
	ot.mu.Lock()
	defer ot.mu.Unlock()
	id := uuid.New().String()
	primitive := &CognitivePrimitive{
		ID:          id,
		Name:        name,
		Generation:  0,
		Lineage:     []string{},
		Stage:       StageEmbryonic,
		Fitness:     0.5,
		CreatedAt:   time.Now(),
		LastUpdated: time.Now(),
		Genome:      genome,
	}
	ot.primitives[id] = primitive
	return id
}
func (ot *OntogeneticTracker) UpdateStages() {
	ot.mu.Lock()
	defer ot.mu.Unlock()
	for _, primitive := range ot.primitives {
		age := time.Since(primitive.CreatedAt)
		primitive.Age = age
		newStage := ot.determineStage(age, primitive.Fitness)
		if newStage != primitive.Stage {
			fmt.Printf("🧬 Primitive %s: %s → %s (fitness: %.2f)\n",
				primitive.Name,
				primitive.Stage,
				newStage,
				primitive.Fitness,
			)
			primitive.Stage = newStage
		}
		primitive.LastUpdated = time.Now()
	}
}
func (ot *OntogeneticTracker) determineStage(
	age time.Duration,
	fitness float64,
) DevelopmentStage {
	switch {
	case age < ot.embryonicDuration:
		return StageEmbryonic
	case age < ot.juvenileDuration && fitness < ot.maturityThreshold:
		return StageJuvenile
	case fitness >= ot.maturityThreshold && age < ot.matureDuration:
		return StageMature
	case age >= ot.matureDuration:
		return StageSenescent
	default:
		return StageJuvenile
	}
}
func (ot *OntogeneticTracker) UpdateFitness(id string, success bool) {
	ot.mu.Lock()
	defer ot.mu.Unlock()
	primitive, exists := ot.primitives[id]
	if !exists {
		return
	}
	primitive.TotalUses++
	if success {
		primitive.SuccessCount++
	} else {
		primitive.FailureCount++
	}
	if primitive.TotalUses > 0 {
		successRate := float64(primitive.SuccessCount) / float64(primitive.TotalUses)
		primitive.Fitness = (primitive.Fitness * 0.7) + (successRate * 0.3)
	}
	primitive.LastUpdated = time.Now()
}
func (ot *OntogeneticTracker) SelfGenerate(parentID string) (string, error) {
	ot.mu.Lock()
	defer ot.mu.Unlock()
	parent, exists := ot.primitives[parentID]
	if !exists {
		return "", fmt.Errorf("parent primitive not found: %s", parentID)
	}
	offspringGenome := ot.mutateGenome(parent.Genome)
	offspring := &CognitivePrimitive{
		ID:          uuid.New().String(),
		Name:        fmt.Sprintf("%s_gen%d", parent.Name, parent.Generation+1),
		Generation:  parent.Generation + 1,
		Lineage:     append(parent.Lineage, parent.ID),
		Stage:       StageEmbryonic,
		Fitness:     parent.Fitness * 0.9, 
		CreatedAt:   time.Now(),
		LastUpdated: time.Now(),
		Genome:      offspringGenome,
	}
	ot.primitives[offspring.ID] = offspring
	ot.totalGenerations++
	fmt.Printf("🧬 Self-generated: %s (gen %d) from %s\n",
		offspring.Name,
		offspring.Generation,
		parent.Name,
	)
	return offspring.ID, nil
}
func (ot *OntogeneticTracker) SelfReproduce(parent1ID, parent2ID string) (string, error) {
	ot.mu.Lock()
	defer ot.mu.Unlock()
	parent1, exists1 := ot.primitives[parent1ID]
	parent2, exists2 := ot.primitives[parent2ID]
	if !exists1 || !exists2 {
		return "", fmt.Errorf("one or both parents not found")
	}
	offspringGenome := ot.crossoverGenomes(parent1.Genome, parent2.Genome)
	if math.Round(0.5) < ot.mutationRate { 
		offspringGenome = ot.mutateGenome(offspringGenome)
	}
	maxGen := parent1.Generation
	if parent2.Generation > maxGen {
		maxGen = parent2.Generation
	}
	offspring := &CognitivePrimitive{
		ID:          uuid.New().String(),
		Name:        fmt.Sprintf("hybrid_%s_%s", parent1.Name, parent2.Name),
		Generation:  maxGen + 1,
		Lineage:     []string{parent1.ID, parent2.ID},
		Stage:       StageEmbryonic,
		Fitness:     (parent1.Fitness + parent2.Fitness) / 2.0,
		CreatedAt:   time.Now(),
		LastUpdated: time.Now(),
		Genome:      offspringGenome,
	}
	ot.primitives[offspring.ID] = offspring
	ot.totalEvolutions++
	fmt.Printf("🧬 Reproduced: %s from %s + %s\n",
		offspring.Name,
		parent1.Name,
		parent2.Name,
	)
	return offspring.ID, nil
}
func (ot *OntogeneticTracker) mutateGenome(genome *PrimitiveGenome) *PrimitiveGenome {
	newGenome := &PrimitiveGenome{
		CoefficientGenes:  make([]float64, len(genome.CoefficientGenes)),
		OperatorGenes:     make(map[string]float64),
		SymmetryGenes:     genome.SymmetryGenes,     
		PreservationGenes: genome.PreservationGenes, 
	}
	for i, coeff := range genome.CoefficientGenes {
		mutation := (0.5 - 0.5) * 0.2 
		newGenome.CoefficientGenes[i] = coeff + mutation
	}
	for key, value := range genome.OperatorGenes {
		mutation := (0.5 - 0.5) * 0.2
		newGenome.OperatorGenes[key] = value + mutation
	}
	return newGenome
}
func (ot *OntogeneticTracker) crossoverGenomes(
	genome1, genome2 *PrimitiveGenome,
) *PrimitiveGenome {
	newGenome := &PrimitiveGenome{
		CoefficientGenes:  make([]float64, len(genome1.CoefficientGenes)),
		OperatorGenes:     make(map[string]float64),
		SymmetryGenes:     genome1.SymmetryGenes,
		PreservationGenes: genome1.PreservationGenes,
	}
	if len(genome1.CoefficientGenes) > 0 {
		point := len(genome1.CoefficientGenes) / 2
		for i := 0; i < len(genome1.CoefficientGenes); i++ {
			if i < point {
				newGenome.CoefficientGenes[i] = genome1.CoefficientGenes[i]
			} else if i < len(genome2.CoefficientGenes) {
				newGenome.CoefficientGenes[i] = genome2.CoefficientGenes[i]
			} else {
				newGenome.CoefficientGenes[i] = genome1.CoefficientGenes[i]
			}
		}
	}
	for key, value := range genome1.OperatorGenes {
		newGenome.OperatorGenes[key] = value
	}
	for key, value := range genome2.OperatorGenes {
		if _, exists := newGenome.OperatorGenes[key]; !exists {
			newGenome.OperatorGenes[key] = value
		}
	}
	return newGenome
}
func (ot *OntogeneticTracker) PruneSenescent() int {
	ot.mu.Lock()
	defer ot.mu.Unlock()
	pruned := 0
	for id, primitive := range ot.primitives {
		if primitive.Stage == StageSenescent && primitive.Fitness < 0.3 {
			delete(ot.primitives, id)
			pruned++
			fmt.Printf("🗑️  Pruned senescent primitive: %s (fitness: %.2f)\n",
				primitive.Name,
				primitive.Fitness,
			)
		}
	}
	return pruned
}
func (ot *OntogeneticTracker) GetMetrics() map[string]interface{} {
	ot.mu.RLock()
	defer ot.mu.RUnlock()
	stageCount := make(map[DevelopmentStage]int)
	totalFitness := 0.0
	for _, primitive := range ot.primitives {
		stageCount[primitive.Stage]++
		totalFitness += primitive.Fitness
	}
	avgFitness := 0.0
	if len(ot.primitives) > 0 {
		avgFitness = totalFitness / float64(len(ot.primitives))
	}
	return map[string]interface{}{
		"total_primitives":   len(ot.primitives),
		"embryonic_count":    stageCount[StageEmbryonic],
		"juvenile_count":     stageCount[StageJuvenile],
		"mature_count":       stageCount[StageMature],
		"senescent_count":    stageCount[StageSenescent],
		"average_fitness":    avgFitness,
		"total_generations":  ot.totalGenerations,
		"total_evolutions":   ot.totalEvolutions,
	}
}
func (ot *OntogeneticTracker) GetPrimitive(id string) (*CognitivePrimitive, bool) {
	ot.mu.RLock()
	defer ot.mu.RUnlock()
	primitive, exists := ot.primitives[id]
	return primitive, exists
}
func (ot *OntogeneticTracker) GetMaturePrimitives() []*CognitivePrimitive {
	ot.mu.RLock()
	defer ot.mu.RUnlock()
	mature := make([]*CognitivePrimitive, 0)
	for _, primitive := range ot.primitives {
		if primitive.Stage == StageMature {
			mature = append(mature, primitive)
		}
	}
	return mature
}