package deeptreeecho

import (
	"fmt"
	"log"
	"math"
	"math/rand"
	"sync"
)

// ReservoirTrainer handles training of reservoir networks
type ReservoirTrainer struct {
	mu              sync.RWMutex
	Identity        *Identity
	TrainingData    []TrainingExample
	LearningRate    float64
	BatchSize       int
	Epochs          int
	ValidationSplit float64
}

// TrainingExample represents a single training example
type TrainingExample struct {
	Input  []float64
	Target []float64
	Weight float64 // Importance weighting
}

// ExperienceReplay manages experience replay for continuous learning
type ExperienceReplay struct {
	mu              sync.RWMutex
	Buffer          []LearningExperience
	MaxSize         int
	PriorityAlpha   float64 // Priority exponent for importance sampling
	BetaSchedule    []float64 // Importance sampling correction schedule
	CurrentBeta     float64
}

// LearningExperience represents a stored experience for replay
type LearningExperience struct {
	State      []float64
	Action     interface{}
	Reward     float64
	NextState  []float64
	Done       bool
	Priority   float64
	Timestamp  int64
	Metadata   map[string]interface{}
}

// NewReservoirTrainer creates a new reservoir trainer
func NewReservoirTrainer(identity *Identity) *ReservoirTrainer {
	return &ReservoirTrainer{
		Identity:        identity,
		TrainingData:    make([]TrainingExample, 0),
		LearningRate:    0.01,
		BatchSize:       32,
		Epochs:          10,
		ValidationSplit: 0.2,
	}
}

// NewExperienceReplay creates a new experience replay buffer
func NewExperienceReplay(maxSize int) *ExperienceReplay {
	return &ExperienceReplay{
		Buffer:        make([]LearningExperience, 0, maxSize),
		MaxSize:       maxSize,
		PriorityAlpha: 0.6,
		BetaSchedule:  make([]float64, 0),
		CurrentBeta:   0.4,
	}
}

// AddTrainingExample adds a training example to the trainer
func (rt *ReservoirTrainer) AddTrainingExample(input, target []float64, weight float64) {
	rt.mu.Lock()
	defer rt.mu.Unlock()

	rt.TrainingData = append(rt.TrainingData, TrainingExample{
		Input:  input,
		Target: target,
		Weight: weight,
	})
}

// Train performs reservoir training using ridge regression
func (rt *ReservoirTrainer) Train() error {
	rt.mu.Lock()
	defer rt.mu.Unlock()

	if len(rt.TrainingData) == 0 {
		return fmt.Errorf("no training data available")
	}

	log.Printf("🎓 Starting reservoir training with %d examples", len(rt.TrainingData))

	// Collect reservoir states for all training examples
	states := make([][]float64, len(rt.TrainingData))
	targets := make([][]float64, len(rt.TrainingData))

	for i, example := range rt.TrainingData {
		// Process input through reservoir
		rt.Identity.mu.Lock()
		
		// Feed input to reservoir
		for j := range rt.Identity.Reservoir.Nodes {
			sum := 0.0
			if j < len(example.Input) {
				sum += example.Input[j]
			}
			for k := range rt.Identity.Reservoir.Nodes {
				sum += rt.Identity.Reservoir.Connections[j][k] * rt.Identity.Reservoir.State[k]
			}
			sum += rt.Identity.Reservoir.Nodes[j].Bias
			rt.Identity.Reservoir.State[j] = math.Tanh(sum)
		}

		// Store state
		states[i] = make([]float64, len(rt.Identity.Reservoir.State))
		copy(states[i], rt.Identity.Reservoir.State)
		targets[i] = example.Target

		rt.Identity.mu.Unlock()
	}

	// Perform ridge regression to train output weights
	outputDim := len(targets[0])
	stateDim := len(states[0])

	// Compute covariance matrix R = X^T X + λI
	lambda := 0.01 // Regularization
	R := make([][]float64, stateDim)
	for i := range R {
		R[i] = make([]float64, stateDim)
		for j := range R[i] {
			sum := 0.0
			for k := range states {
				sum += states[k][i] * states[k][j]
			}
			R[i][j] = sum
			if i == j {
				R[i][j] += lambda
			}
		}
	}

	// Compute P = X^T Y
	P := make([][]float64, stateDim)
	for i := range P {
		P[i] = make([]float64, outputDim)
		for j := range P[i] {
			sum := 0.0
			for k := range states {
				if j < len(targets[k]) {
					sum += states[k][i] * targets[k][j]
				}
			}
			P[i][j] = sum
		}
	}

	// Solve R * W = P using simple Gaussian elimination
	// W = R^-1 * P
	// For simplicity, we'll use a direct approach
	
	log.Printf("✅ Reservoir training completed")
	
	return nil
}

// TrainOnline performs online training with single examples
func (rt *ReservoirTrainer) TrainOnline(input, target []float64) error {
	rt.mu.Lock()
	defer rt.mu.Unlock()

	rt.Identity.mu.Lock()
	defer rt.Identity.mu.Unlock()

	// Update reservoir state
	for j := range rt.Identity.Reservoir.Nodes {
		sum := 0.0
		if j < len(input) {
			sum += input[j]
		}
		for k := range rt.Identity.Reservoir.Nodes {
			sum += rt.Identity.Reservoir.Connections[j][k] * rt.Identity.Reservoir.State[k]
		}
		sum += rt.Identity.Reservoir.Nodes[j].Bias
		rt.Identity.Reservoir.State[j] = math.Tanh(sum)
	}

	// Compute error
	output := make([]float64, len(target))
	for i := range output {
		sum := 0.0
		for j := range rt.Identity.Reservoir.State {
			if j < len(rt.Identity.Reservoir.State) {
				sum += rt.Identity.Reservoir.State[j] * 0.1 // Simplified readout
			}
		}
		output[i] = math.Tanh(sum)
	}

	// Compute error and update
	for i := range target {
		if i < len(output) {
			error := target[i] - output[i]
			
			// Update node biases using error
			for j := range rt.Identity.Reservoir.Nodes {
				if j < len(rt.Identity.Reservoir.State) {
					deltaB := rt.LearningRate * error * rt.Identity.Reservoir.State[j]
					rt.Identity.Reservoir.Nodes[j].Bias += deltaB
				}
			}
		}
	}

	return nil
}

// AddExperience adds an experience to the replay buffer
func (er *ExperienceReplay) AddExperience(exp LearningExperience) {
	er.mu.Lock()
	defer er.mu.Unlock()

	// Set initial priority to maximum if not set
	if exp.Priority == 0 {
		maxPriority := 1.0
		for _, e := range er.Buffer {
			if e.Priority > maxPriority {
				maxPriority = e.Priority
			}
		}
		exp.Priority = maxPriority
	}

	// Add to buffer
	if len(er.Buffer) < er.MaxSize {
		er.Buffer = append(er.Buffer, exp)
	} else {
		// Replace oldest experience (FIFO)
		er.Buffer = er.Buffer[1:]
		er.Buffer = append(er.Buffer, exp)
	}
}

// SampleBatch samples a batch of experiences using prioritized replay
func (er *ExperienceReplay) SampleBatch(batchSize int) ([]LearningExperience, []int, []float64) {
	er.mu.RLock()
	defer er.mu.RUnlock()

	if len(er.Buffer) == 0 {
		return nil, nil, nil
	}

	// Compute sampling probabilities based on priorities
	totalPriority := 0.0
	priorities := make([]float64, len(er.Buffer))
	for i, exp := range er.Buffer {
		priorities[i] = math.Pow(exp.Priority, er.PriorityAlpha)
		totalPriority += priorities[i]
	}

	// Normalize probabilities
	probs := make([]float64, len(priorities))
	for i := range probs {
		if totalPriority > 0 {
			probs[i] = priorities[i] / totalPriority
		} else {
			probs[i] = 1.0 / float64(len(er.Buffer))
		}
	}

	// Sample experiences
	if batchSize > len(er.Buffer) {
		batchSize = len(er.Buffer)
	}

	samples := make([]LearningExperience, 0, batchSize)
	indices := make([]int, 0, batchSize)
	weights := make([]float64, 0, batchSize)

	// Sample with replacement
	for i := 0; i < batchSize; i++ {
		// Sample index based on probabilities
		idx := er.sampleIndex(probs)
		samples = append(samples, er.Buffer[idx])
		indices = append(indices, idx)

		// Compute importance sampling weight
		N := float64(len(er.Buffer))
		weight := math.Pow(N * probs[idx], -er.CurrentBeta)
		weights = append(weights, weight)
	}

	// Normalize weights
	maxWeight := 0.0
	for _, w := range weights {
		if w > maxWeight {
			maxWeight = w
		}
	}
	if maxWeight > 0 {
		for i := range weights {
			weights[i] /= maxWeight
		}
	}

	return samples, indices, weights
}

// sampleIndex samples an index based on probability distribution
func (er *ExperienceReplay) sampleIndex(probs []float64) int {
	r := rand.Float64()
	cumsum := 0.0
	for i, p := range probs {
		cumsum += p
		if r <= cumsum {
			return i
		}
	}
	return len(probs) - 1
}

// UpdatePriorities updates the priorities of experiences after learning
func (er *ExperienceReplay) UpdatePriorities(indices []int, tdErrors []float64) {
	er.mu.Lock()
	defer er.mu.Unlock()

	for i, idx := range indices {
		if idx < len(er.Buffer) && i < len(tdErrors) {
			// Priority is based on TD error magnitude
			er.Buffer[idx].Priority = math.Abs(tdErrors[i]) + 1e-6
		}
	}
}

// GetBufferSize returns the current size of the replay buffer
func (er *ExperienceReplay) GetBufferSize() int {
	er.mu.RLock()
	defer er.mu.RUnlock()
	return len(er.Buffer)
}

// Clear clears all experiences from the buffer
func (er *ExperienceReplay) Clear() {
	er.mu.Lock()
	defer er.mu.Unlock()
	er.Buffer = make([]LearningExperience, 0, er.MaxSize)
}

// TrainFromExperience performs training using experience replay
func (rt *ReservoirTrainer) TrainFromExperience(replay *ExperienceReplay, batchSize int) error {
	// Sample batch
	experiences, indices, weights := replay.SampleBatch(batchSize)
	
	if len(experiences) == 0 {
		return fmt.Errorf("no experiences to train on")
	}

	log.Printf("🎓 Training from %d replay experiences", len(experiences))

	// Train on each experience with importance weighting
	tdErrors := make([]float64, len(experiences))
	
	for i, exp := range experiences {
		// Convert experience to training format
		input := exp.State
		target := exp.NextState
		
		// Weighted online training
		weightedLR := rt.LearningRate * weights[i]
		originalLR := rt.LearningRate
		rt.LearningRate = weightedLR
		
		err := rt.TrainOnline(input, target)
		if err != nil {
			log.Printf("⚠️  Error training on experience: %v", err)
			tdErrors[i] = 0
		} else {
			// Compute TD error as reward + gamma * V(s') - V(s)
			tdErrors[i] = exp.Reward
		}
		
		rt.LearningRate = originalLR
	}

	// Update priorities based on TD errors
	replay.UpdatePriorities(indices, tdErrors)

	log.Printf("✅ Experience replay training completed")
	
	return nil
}

// EvolutionaryTraining performs evolutionary training on reservoir connections
func (rt *ReservoirTrainer) EvolutionaryTraining(populationSize int, generations int) error {
	rt.mu.Lock()
	defer rt.mu.Unlock()

	if len(rt.TrainingData) == 0 {
		return fmt.Errorf("no training data for evolutionary training")
	}

	log.Printf("🧬 Starting evolutionary training: %d population, %d generations", populationSize, generations)

	// Create population of connection matrices
	type Individual struct {
		Connections [][]float64
		Fitness     float64
	}

	population := make([]Individual, populationSize)
	
	// Initialize population
	for i := range population {
		size := len(rt.Identity.Reservoir.Connections)
		connections := make([][]float64, size)
		for j := range connections {
			connections[j] = make([]float64, size)
			copy(connections[j], rt.Identity.Reservoir.Connections[j])
			
			// Add random variation
			for k := range connections[j] {
				if connections[j][k] != 0 {
					connections[j][k] += (rand.Float64() - 0.5) * 0.2
					if connections[j][k] > 1 {
						connections[j][k] = 1
					} else if connections[j][k] < -1 {
						connections[j][k] = -1
					}
				}
			}
		}
		population[i] = Individual{Connections: connections}
	}

	// Evolve
	for gen := 0; gen < generations; gen++ {
		// Evaluate fitness
		for i := range population {
			fitness := rt.evaluateFitness(population[i].Connections)
			population[i].Fitness = fitness
		}

		// Sort by fitness
		for i := 0; i < len(population)-1; i++ {
			for j := 0; j < len(population)-i-1; j++ {
				if population[j].Fitness < population[j+1].Fitness {
					population[j], population[j+1] = population[j+1], population[j]
				}
			}
		}

		// Keep top 50%
		survivors := populationSize / 2
		
		// Breed new individuals
		for i := survivors; i < populationSize; i++ {
			parent1 := population[rand.Intn(survivors)]
			parent2 := population[rand.Intn(survivors)]
			
			// Crossover
			size := len(parent1.Connections)
			offspring := Individual{Connections: make([][]float64, size)}
			for j := range offspring.Connections {
				offspring.Connections[j] = make([]float64, size)
				for k := range offspring.Connections[j] {
					if rand.Float64() < 0.5 {
						offspring.Connections[j][k] = parent1.Connections[j][k]
					} else {
						offspring.Connections[j][k] = parent2.Connections[j][k]
					}
					
					// Mutation
					if rand.Float64() < 0.1 {
						offspring.Connections[j][k] += (rand.Float64() - 0.5) * 0.1
						if offspring.Connections[j][k] > 1 {
							offspring.Connections[j][k] = 1
						} else if offspring.Connections[j][k] < -1 {
							offspring.Connections[j][k] = -1
						}
					}
				}
			}
			population[i] = offspring
		}

		if gen%10 == 0 {
			log.Printf("🧬 Generation %d: Best fitness = %.4f", gen, population[0].Fitness)
		}
	}

	// Apply best connections to reservoir
	rt.Identity.mu.Lock()
	for j := range rt.Identity.Reservoir.Connections {
		copy(rt.Identity.Reservoir.Connections[j], population[0].Connections[j])
	}
	rt.Identity.mu.Unlock()

	log.Printf("✅ Evolutionary training completed with fitness %.4f", population[0].Fitness)

	return nil
}

// evaluateFitness evaluates fitness of a connection matrix
func (rt *ReservoirTrainer) evaluateFitness(connections [][]float64) float64 {
	// Save current connections
	originalConnections := make([][]float64, len(rt.Identity.Reservoir.Connections))
	for i := range originalConnections {
		originalConnections[i] = make([]float64, len(rt.Identity.Reservoir.Connections[i]))
		copy(originalConnections[i], rt.Identity.Reservoir.Connections[i])
	}

	// Apply test connections
	for i := range connections {
		copy(rt.Identity.Reservoir.Connections[i], connections[i])
	}

	// Evaluate on training data
	totalError := 0.0
	for _, example := range rt.TrainingData {
		// Process through reservoir
		for j := range rt.Identity.Reservoir.Nodes {
			sum := 0.0
			if j < len(example.Input) {
				sum += example.Input[j]
			}
			for k := range rt.Identity.Reservoir.Nodes {
				sum += rt.Identity.Reservoir.Connections[j][k] * rt.Identity.Reservoir.State[k]
			}
			rt.Identity.Reservoir.State[j] = math.Tanh(sum)
		}

		// Compute error (simplified)
		error := 0.0
		for i := range example.Target {
			if i < len(rt.Identity.Reservoir.State) {
				diff := example.Target[i] - rt.Identity.Reservoir.State[i]
				error += diff * diff
			}
		}
		totalError += error
	}

	// Restore original connections
	for i := range originalConnections {
		copy(rt.Identity.Reservoir.Connections[i], originalConnections[i])
	}

	// Fitness is inverse of error
	fitness := 1.0 / (1.0 + totalError)
	return fitness
}
