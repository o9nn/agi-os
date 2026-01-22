package opencog
import (
	"context"
	"fmt"
	"testing"
	"time"
)
func TestAtomSpace(t *testing.T) {
	as := NewAtomSpace()
	atom1, err := as.AddAtom(ConceptNode, "cat", &TruthValue{
		Strength:   0.9,
		Confidence: 0.8,
		Count:      1.0,
	})
	if err != nil {
		t.Fatalf("Failed to add atom: %v", err)
	}
	atom2, err := as.AddAtom(ConceptNode, "animal", nil)
	if err != nil {
		t.Fatalf("Failed to add atom: %v", err)
	}
	link, err := as.AddLink(InheritanceLink, []string{atom1.ID, atom2.ID}, &TruthValue{
		Strength:   0.95,
		Confidence: 0.9,
		Count:      1.0,
	})
	if err != nil {
		t.Fatalf("Failed to add link: %v", err)
	}
	if link.Type != InheritanceLink {
		t.Errorf("Expected InheritanceLink, got %v", link.Type)
	}
	incoming := as.GetIncoming(atom1.ID)
	if len(incoming) != 1 {
		t.Errorf("Expected 1 incoming link, got %d", len(incoming))
	}
	t.Logf("AtomSpace test passed: %d atoms, %d links", as.AtomCount, as.LinkCount)
}
func TestHypercyclicReactor(t *testing.T) {
	as := NewAtomSpace()
	reactor := NewHypercyclicReactor(as, 4)
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	if err := reactor.Start(ctx); err != nil {
		t.Fatalf("Failed to start reactor: %v", err)
	}
	defer reactor.Stop()
	atom1, _ := as.AddAtom(ConceptNode, "test1", nil)
	atom2, _ := as.AddAtom(ConceptNode, "test2", nil)
	cycle, err := reactor.AddReactionCycle(
		[]string{atom1.ID},
		[]string{atom2.ID},
		[]string{},
		0.8,
	)
	if err != nil {
		t.Fatalf("Failed to add reaction cycle: %v", err)
	}
	catalyst, err := reactor.AddCatalyst(MetabolicCatalyst, 0.5)
	if err != nil {
		t.Fatalf("Failed to add catalyst: %v", err)
	}
	time.Sleep(2 * time.Second)
	metrics := reactor.GetMetrics()
	t.Logf("Reactor metrics: %+v", metrics)
	if cycle.Iterations == 0 {
		t.Errorf("Expected some iterations, got 0")
	}
	if catalyst.State != ActiveState {
		t.Errorf("Expected catalyst to be active")
	}
}
func TestDTESN(t *testing.T) {
	dtesn := NewDTESN(10, 100, 10)
	input := make([]float64, 10)
	for i := range input {
		input[i] = float64(i) * 0.1
	}
	err := dtesn.Update(input)
	if err != nil {
		t.Fatalf("Failed to update DTESN: %v", err)
	}
	state := dtesn.GetState()
	if len(state) != 100 {
		t.Errorf("Expected state size 100, got %d", len(state))
	}
	output := dtesn.Predict()
	if len(output) != 10 {
		t.Errorf("Expected output size 10, got %d", len(output))
	}
	status := dtesn.GetStatus()
	t.Logf("DTESN status: %+v", status)
	if dtesn.Iterations < 1 {
		t.Errorf("Expected at least 1 iteration")
	}
}
func TestPaunPSystem(t *testing.T) {
	pps := NewPaunPSystem()
	rootMembrane := pps.Membranes[pps.RootMembrane]
	rootMembrane.Objects["energy"] = 15
	rootMembrane.Objects["pattern"] = 5
	for i := 0; i < 10; i++ {
		pps.Evolve()
	}
	if len(pps.Membranes) < 1 {
		t.Errorf("Expected at least 1 membrane")
	}
	t.Logf("P-System: %d membranes", len(pps.Membranes))
}
func TestRicciFlow(t *testing.T) {
	rfe := NewRicciFlowEngine(10)
	for i := 0; i < 10; i++ {
		rfe.Flow(0.01)
	}
	if rfe.FlowTime < 0.09 {
		t.Errorf("Expected flow time >= 0.09, got %f", rfe.FlowTime)
	}
	t.Logf("Ricci flow: time=%f, curvature=%f", rfe.FlowTime, rfe.ScalarCurvature)
}
func TestEchoCogSystem(t *testing.T) {
	system := NewEchoCogSystem("TestSystem", 4)
	ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
	defer cancel()
	if err := system.Start(ctx); err != nil {
		t.Fatalf("Failed to start system: %v", err)
	}
	defer system.Stop()
	time.Sleep(1 * time.Second)
	atom1, _ := system.AtomSpace.AddAtom(ConceptNode, "test1", nil)
	atom2, _ := system.AtomSpace.AddAtom(ConceptNode, "test2", nil)
	catalyst, _ := system.HypercyclicReactor.AddCatalyst(MetabolicCatalyst, 0.5)
	system.HypercyclicReactor.AddReactionCycle(
		[]string{atom1.ID},
		[]string{atom2.ID},
		[]string{catalyst.ID},
		0.8,
	)
	time.Sleep(500 * time.Millisecond)
	response, err := system.ProcessInput(ctx, "What is the nature of consciousness?")
	if err != nil {
		t.Fatalf("Failed to process input: %v", err)
	}
	t.Logf("Response: %s", response)
	status := system.GetStatus()
	t.Logf("System status: %+v", status)
	if !status["running"].(bool) {
		t.Errorf("Expected system to be running")
	}
	throughputGain := system.GetThroughputGain()
	t.Logf("Throughput gain: %.2fx", throughputGain)
	sixMonths := 6 * 30 * 24 * time.Hour
	compressed := system.EstimateTimeCompression(sixMonths)
	t.Logf("6 months compressed to: %v", compressed)
	if throughputGain > 1.0 && compressed >= sixMonths {
		t.Errorf("Expected compressed time < 6 months when throughput gain > 1.0")
	}
}
func TestInference(t *testing.T) {
	as := NewAtomSpace()
	reactor := NewHypercyclicReactor(as, 4)
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	reactor.Start(ctx)
	defer reactor.Stop()
	catAtom, _ := as.AddAtom(ConceptNode, "cat", nil)
	animalAtom, _ := as.AddAtom(ConceptNode, "animal", nil)
	as.AddLink(InheritanceLink, []string{catAtom.ID, animalAtom.ID}, &TruthValue{
		Strength:   0.9,
		Confidence: 0.8,
		Count:      1.0,
	})
	dogAtom, _ := as.AddAtom(ConceptNode, "dog", nil)
	as.AddLink(InheritanceLink, []string{dogAtom.ID, animalAtom.ID}, &TruthValue{
		Strength:   0.9,
		Confidence: 0.8,
		Count:      1.0,
	})
	task := &InferenceTask{
		ID:         "test_inference",
		Type:       ForwardInference,
		Input:      []string{catAtom.ID},
		Goal:       animalAtom.ID,
		Priority:   1,
		Deadline:   time.Now().Add(2 * time.Second),
		ResultChan: make(chan *InferenceResult, 1),
	}
	err := reactor.SubmitInference(task)
	if err != nil {
		t.Fatalf("Failed to submit inference: %v", err)
	}
	select {
	case result := <-task.ResultChan:
		t.Logf("Inference result: success=%v, output=%v", result.Success, result.Output)
		if result.Error != nil {
			t.Logf("Inference error: %v", result.Error)
		}
	case <-time.After(3 * time.Second):
		t.Error("Inference timeout")
	}
}
func BenchmarkReactor(b *testing.B) {
	as := NewAtomSpace()
	reactor := NewHypercyclicReactor(as, 8)
	ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
	defer cancel()
	reactor.Start(ctx)
	defer reactor.Stop()
	atoms := make([]*Atom, 100)
	for i := 0; i < 100; i++ {
		atom, _ := as.AddAtom(ConceptNode, fmt.Sprintf("atom_%d", i), nil)
		atoms[i] = atom
	}
	for i := 0; i < 50; i++ {
		reactor.AddReactionCycle(
			[]string{atoms[i].ID},
			[]string{atoms[i+50].ID},
			[]string{},
			0.7,
		)
	}
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		time.Sleep(10 * time.Millisecond)
	}
	metrics := reactor.GetMetrics()
	b.Logf("Reactions per second: %.2f", metrics["reactions_per_second"].(float64))
	b.Logf("Throughput gain: %.2fx", metrics["throughput_gain"].(float64))
}
func BenchmarkDTESN(b *testing.B) {
	dtesn := NewDTESN(128, 1024, 128)
	input := make([]float64, 128)
	for i := range input {
		input[i] = float64(i%10) * 0.1
	}
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		dtesn.Update(input)
	}
}
func BenchmarkConcurrentExecution(b *testing.B) {
	executor := NewConcurrentExecutor(8)
	ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
	defer cancel()
	executor.Start(ctx)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		task := &ExecutionTask{
			ID:       fmt.Sprintf("task_%d", i),
			Type:     ComputeTask,
			Priority: 1,
			Function: func() (interface{}, error) {
				time.Sleep(1 * time.Millisecond)
				return "done", nil
			},
			ResultChan: make(chan *ExecutionResult, 1),
		}
		executor.SubmitTask(task)
		<-task.ResultChan
	}
	b.Logf("Tasks completed: %d", executor.TasksCompleted)
	b.Logf("Average latency: %.2fms", executor.AverageLatency)
}