package echodream
func (dci *DreamCycleIntegration) SetWisdomCallback(callback func(wisdom Wisdom)) {
dci.mu.Lock()
defer dci.mu.Unlock()
dci.onWisdomExtracted = callback
}
func (dci *DreamCycleIntegration) SetDreamCompleteCallback(callback func(dream *Dream)) {
dci.mu.Lock()
defer dci.mu.Unlock()
dci.onDreamComplete = callback
}