package deeptreeecho
import (
"math"
"math/rand"
"sync"
)
type PersonaType int
const (
PersonaContemplativeScholar PersonaType = iota
PersonaDynamicExplorer
PersonaCautiousAnalyst
PersonaCreativeVisionary
)
func (pt PersonaType) String() string {
names := []string{
"Contemplative Scholar",
"Dynamic Explorer",
"Cautious Analyst",
"Creative Visionary",
}
if int(pt) < len(names) {
return names[pt]
}
return "Unknown"
}
type EchoStateReservoir struct {
mu sync.RWMutex
spectralRadius float64
inputScaling   float64
leakRate       float64
size            int
reservoirState  []float64
weights         [][]float64
inputWeights    [][]float64
level           int
parentReservoir *EchoStateReservoir
childReservoirs []*EchoStateReservoir
persona PersonaType
stateHistory [][]float64
maxHistory   int
echoProperty    float64
complexity      float64
}
type PersonaConfig struct {
SpectralRadius float64
InputScaling   float64
LeakRate       float64
Description    string
}
func GetPersonaConfig(persona PersonaType) PersonaConfig {
configs := map[PersonaType]PersonaConfig{
PersonaContemplativeScholar: {
SpectralRadius: 0.95,
InputScaling:   0.3,
LeakRate:       0.2,
Description:    "Deep memory, slow deliberation, reflection over reaction",
},
PersonaDynamicExplorer: {
SpectralRadius: 0.7,
InputScaling:   0.8,
LeakRate:       0.8,
Description:    "Low memory, rapid adaptation, exploration over exploitation",
},
PersonaCautiousAnalyst: {
SpectralRadius: 0.99,
InputScaling:   0.2,
LeakRate:       0.3,
Description:    "Maximal stability, conservative, systematic processing",
},
PersonaCreativeVisionary: {
SpectralRadius: 0.85,
InputScaling:   0.7,
LeakRate:       0.6,
Description:    "Edge of chaos, flexible memory, transformation-seeking",
},
}
if config, exists := configs[persona]; exists {
return config
}
return configs[PersonaContemplativeScholar]
}
func NewEchoStateReservoir(size int, persona PersonaType, level int) *EchoStateReservoir {
config := GetPersonaConfig(persona)
esr := &EchoStateReservoir{
spectralRadius:  config.SpectralRadius,
inputScaling:    config.InputScaling,
leakRate:        config.LeakRate,
size:            size,
reservoirState:  make([]float64, size),
level:           level,
persona:         persona,
childReservoirs: make([]*EchoStateReservoir, 0),
stateHistory:    make([][]float64, 0),
maxHistory:      100,
}
esr.initializeWeights()
return esr
}
func (esr *EchoStateReservoir) initializeWeights() {
esr.weights = make([][]float64, esr.size)
for i := range esr.weights {
esr.weights[i] = make([]float64, esr.size)
for j := range esr.weights[i] {
esr.weights[i][j] = (rand.Float64()*2.0 - 1.0) * 0.5
}
}
esr.scaleToSpectralRadius()
esr.inputWeights = make([][]float64, 0)
}
func (esr *EchoStateReservoir) scaleToSpectralRadius() {
scale := esr.spectralRadius / 1.0
for i := range esr.weights {
for j := range esr.weights[i] {
esr.weights[i][j] *= scale
}
}
}
func (esr *EchoStateReservoir) Update(input []float64) []float64 {
esr.mu.Lock()
defer esr.mu.Unlock()
if len(esr.inputWeights) == 0 {
esr.initializeInputWeights(len(input))
}
newState := make([]float64, esr.size)
for i := 0; i < esr.size; i++ {
inputSum := 0.0
for j := 0; j < len(input); j++ {
if j < len(esr.inputWeights[i]) {
inputSum += esr.inputWeights[i][j] * input[j]
}
}
inputSum *= esr.inputScaling
reservoirSum := 0.0
for j := 0; j < esr.size; j++ {
reservoirSum += esr.weights[i][j] * esr.reservoirState[j]
}
newState[i] = (1.0-esr.leakRate)*esr.reservoirState[i] +
esr.leakRate*math.Tanh(inputSum+reservoirSum)
}
esr.reservoirState = newState
esr.recordState(newState)
esr.updateMetrics()
return newState
}
func (esr *EchoStateReservoir) initializeInputWeights(inputDim int) {
esr.inputWeights = make([][]float64, esr.size)
for i := range esr.inputWeights {
esr.inputWeights[i] = make([]float64, inputDim)
for j := range esr.inputWeights[i] {
esr.inputWeights[i][j] = (rand.Float64()*2.0 - 1.0) * 0.5
}
}
}
func (esr *EchoStateReservoir) recordState(state []float64) {
stateCopy := make([]float64, len(state))
copy(stateCopy, state)
esr.stateHistory = append(esr.stateHistory, stateCopy)
if len(esr.stateHistory) > esr.maxHistory {
esr.stateHistory = esr.stateHistory[1:]
}
}
func (esr *EchoStateReservoir) updateMetrics() {
esr.echoProperty = esr.calculateEchoProperty()
esr.complexity = esr.calculateComplexity()
}
func (esr *EchoStateReservoir) calculateEchoProperty() float64 {
if len(esr.stateHistory) < 2 {
return 1.0
}
recent := esr.stateHistory[len(esr.stateHistory)-1]
previous := esr.stateHistory[len(esr.stateHistory)-2]
changeSum := 0.0
for i := range recent {
diff := recent[i] - previous[i]
changeSum += diff * diff
}
change := math.Sqrt(changeSum / float64(len(recent)))
optimal := 0.2
echoProperty := 1.0 - math.Abs(change-optimal)/optimal
return math.Max(0.0, math.Min(1.0, echoProperty))
}
func (esr *EchoStateReservoir) calculateComplexity() float64 {
if len(esr.stateHistory) < 10 {
return 0.5
}
means := make([]float64, esr.size)
for _, state := range esr.stateHistory {
for i, val := range state {
means[i] += val
}
}
for i := range means {
means[i] /= float64(len(esr.stateHistory))
}
variances := make([]float64, esr.size)
for _, state := range esr.stateHistory {
for i, val := range state {
diff := val - means[i]
variances[i] += diff * diff
}
}
avgVariance := 0.0
for i := range variances {
variances[i] /= float64(len(esr.stateHistory))
avgVariance += variances[i]
}
avgVariance /= float64(len(variances))
complexity := math.Min(avgVariance*2.0, 1.0)
return complexity
}
func (esr *EchoStateReservoir) GetState() []float64 {
esr.mu.RLock()
defer esr.mu.RUnlock()
state := make([]float64, len(esr.reservoirState))
copy(state, esr.reservoirState)
return state
}
func (esr *EchoStateReservoir) Reset() {
esr.mu.Lock()
defer esr.mu.Unlock()
for i := range esr.reservoirState {
esr.reservoirState[i] = 0.0
}
esr.stateHistory = make([][]float64, 0)
}
func (esr *EchoStateReservoir) AddChild(child *EchoStateReservoir) {
esr.mu.Lock()
defer esr.mu.Unlock()
child.parentReservoir = esr
child.level = esr.level + 1
esr.childReservoirs = append(esr.childReservoirs, child)
}
func (esr *EchoStateReservoir) ProcessHierarchical(input []float64) map[int][]float64 {
state := esr.Update(input)
results := make(map[int][]float64)
results[esr.level] = state
for _, child := range esr.childReservoirs {
childResults := child.ProcessHierarchical(state)
for level, childState := range childResults {
results[level] = childState
}
}
return results
}
func (esr *EchoStateReservoir) GetMetrics() map[string]interface{} {
esr.mu.RLock()
defer esr.mu.RUnlock()
return map[string]interface{}{
"persona":          esr.persona.String(),
"spectral_radius":  esr.spectralRadius,
"input_scaling":    esr.inputScaling,
"leak_rate":        esr.leakRate,
"size":             esr.size,
"level":            esr.level,
"echo_property":    esr.echoProperty,
"complexity":       esr.complexity,
"history_size":     len(esr.stateHistory),
"child_count":      len(esr.childReservoirs),
}
}
type HierarchicalReservoirSystem struct {
mu        sync.RWMutex
root      *EchoStateReservoir
allLevels map[int][]*EchoStateReservoir
}
func NewHierarchicalReservoirSystem(
levelsConfig []struct {
Size    int
Persona PersonaType
},
) *HierarchicalReservoirSystem {
hrs := &HierarchicalReservoirSystem{
allLevels: make(map[int][]*EchoStateReservoir),
}
if len(levelsConfig) > 0 {
hrs.root = NewEchoStateReservoir(
levelsConfig[0].Size,
levelsConfig[0].Persona,
0,
)
hrs.allLevels[0] = []*EchoStateReservoir{hrs.root}
parent := hrs.root
for i := 1; i < len(levelsConfig); i++ {
child := NewEchoStateReservoir(
levelsConfig[i].Size,
levelsConfig[i].Persona,
i,
)
parent.AddChild(child)
hrs.allLevels[i] = append(hrs.allLevels[i], child)
parent = child
}
}
return hrs
}
func (hrs *HierarchicalReservoirSystem) Process(input []float64) map[int][]float64 {
if hrs.root == nil {
return make(map[int][]float64)
}
return hrs.root.ProcessHierarchical(input)
}
func (hrs *HierarchicalReservoirSystem) GetSystemMetrics() map[string]interface{} {
hrs.mu.RLock()
defer hrs.mu.RUnlock()
levelMetrics := make(map[int]interface{})
for level, reservoirs := range hrs.allLevels {
metrics := make([]map[string]interface{}, len(reservoirs))
for i, reservoir := range reservoirs {
metrics[i] = reservoir.GetMetrics()
}
levelMetrics[level] = metrics
}
return map[string]interface{}{
"total_levels":  len(hrs.allLevels),
"level_metrics": levelMetrics,
}
}