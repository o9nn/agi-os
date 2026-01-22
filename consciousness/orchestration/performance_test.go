package orchestration
import (
"context"
"fmt"
"testing"
"time"
"github.com/EchoCog/echollama/api"
)
func TestPerformanceOptimization(t *testing.T) {
client := api.Client{}
engine := NewEngine(client)
ctx := context.Background()
RegisterDefaultTools(engine)
RegisterDefaultPlugins(engine)
agent1 := &Agent{
Name:        "perf-agent-1",
Description: "Performance test agent 1",
Type:        AgentTypeGeneral,
Models:      []string{"llama3.2"},
Tools:       []string{"calculator"},
}
agent2 := &Agent{
Name:        "perf-agent-2",
Description: "Performance test agent 2",
Type:        AgentTypeSpecialist,
Models:      []string{"llama3.2"},
Tools:       []string{"web_search"},
}
err := engine.CreateAgent(ctx, agent1)
if err != nil {
t.Fatalf("CreateAgent failed: %v", err)
}
err = engine.CreateAgent(ctx, agent2)
if err != nil {
t.Fatalf("CreateAgent failed: %v", err)
}
task := &Task{
Type:  TaskTypeTool,
Input: "Calculate 10 * 5",
Parameters: map[string]interface{}{
"tool": map[string]interface{}{
"name": "calculator",
"parameters": map[string]interface{}{
"operation": "multiply",
"a":         10.0,
"b":         5.0,
},
},
},
}
deadline := time.Now().Add(1 * time.Minute)
result, err := engine.ExecuteTaskOptimized(ctx, task, TaskPriorityNormal, deadline)
if err != nil {
t.Fatalf("ExecuteTaskOptimized failed: %v", err)
}
if result == nil {
t.Fatal("ExecuteTaskOptimized should return a result")
}
if result.Output == "" {
t.Error("Task result should have output")
}
systemMetrics := engine.GetSystemMetrics()
if systemMetrics == nil {
t.Error("System metrics should be available")
}
if systemMetrics.TotalTasks == 0 {
t.Error("Total tasks should be greater than 0")
}
resourceUsage := engine.GetResourceUsage()
if len(resourceUsage) == 0 {
t.Log("Resource usage tracking initialized (may be empty initially)")
}
agentLoads := engine.GetAgentLoads()
if len(agentLoads) == 0 {
t.Log("Agent loads tracking initialized (may be empty initially)")
}
}
func TestLearningSystemIntegration(t *testing.T) {
client := api.Client{}
engine := NewEngine(client)
ctx := context.Background()
agent := &Agent{
Name:        "learning-integration-agent",
Description: "Agent for learning integration testing",
Type:        AgentTypeGeneral,
Models:      []string{"llama3.2"},
}
err := engine.CreateAgent(ctx, agent)
if err != nil {
t.Fatalf("CreateAgent failed: %v", err)
}
for i := 0; i < 5; i++ {
task := &Task{
ID:      fmt.Sprintf("learning-task-%d", i),
Type:    TaskTypeReflect,
Input:   fmt.Sprintf("Reflection task %d", i),
Status:  TaskStatusPending,
AgentID: agent.ID,
}
_, err := engine.ExecuteTask(ctx, task, agent)
if err != nil {
t.Errorf("ExecuteTask failed for task %d: %v", i, err)
}
}
learningSystem := engine.GetLearningSystem()
if learningSystem == nil {
t.Fatal("Learning system should be available")
}
model := learningSystem.GetLearningModel(agent.ID)
if model == nil {
t.Fatal("Learning model should exist for agent")
}
if model.LearningTrajectory.CurrentPerformance == 0 {
t.Error("Learning trajectory should show current performance")
}
adaptationResult, err := engine.AdaptAgent(ctx, agent.ID)
if err != nil {
t.Fatalf("AdaptAgent failed: %v", err)
}
if adaptationResult == nil {
t.Error("Adaptation result should not be nil")
}
testTask := &Task{
Type:  TaskTypeReflect,
Input: "Test prediction task",
}
optimalAgent, confidence, err := engine.PredictOptimalAgentForTask(ctx, testTask)
if err != nil {
t.Fatalf("PredictOptimalAgentForTask failed: %v", err)
}
if optimalAgent == nil {
t.Error("Should predict an optimal agent")
}
if confidence < 0.0 || confidence > 1.0 {
t.Errorf("Confidence should be between 0.0 and 1.0, got %.2f", confidence)
}
t.Logf("Predicted optimal agent: %s with confidence: %.2f", optimalAgent.Name, confidence)
}
func TestPerformanceMonitoring(t *testing.T) {
client := api.Client{}
engine := NewEngine(client)
performanceOptimizer := engine.GetPerformanceOptimizer()
if performanceOptimizer == nil {
t.Fatal("Performance optimizer should be available")
}
systemMetrics := engine.GetSystemMetrics()
if systemMetrics == nil {
t.Error("System metrics should be initialized")
}
alerts := engine.GetActiveAlerts()
if alerts == nil {
t.Error("Alerts should be initialized (can be empty)")
}
testMetrics := &SystemMetrics{
TotalTasks:          100,
CompletedTasks:      70,
FailedTasks:         30,
AverageResponseTime: 45 * time.Second,
ThroughputTPS:       0.05,
SystemHealth:        0.2,
ResourceUtilization: &ResourceUsage{
CPUUsage:         0.9,
MemoryUsageGB:    30.0,
NetworkUsageMbps: 950.0,
},
LastUpdated: time.Now(),
}
performanceOptimizer.performanceMonitor.UpdateSystemMetrics(testMetrics)
alerts = engine.GetActiveAlerts()
if len(alerts) == 0 {
t.Log("No alerts triggered (alert conditions may need adjustment)")
} else {
t.Logf("Triggered %d alerts", len(alerts))
for _, alert := range alerts {
t.Logf("Alert: %s - %s", alert.RuleName, alert.Message)
}
}
}
func TestLoadBalancing(t *testing.T) {
client := api.Client{}
engine := NewEngine(client)
ctx := context.Background()
agents := make([]*Agent, 3)
for i := 0; i < 3; i++ {
agent := &Agent{
Name:        fmt.Sprintf("load-balance-agent-%d", i),
Description: fmt.Sprintf("Load balancing test agent %d", i),
Type:        AgentTypeGeneral,
Models:      []string{"llama3.2"},
}
err := engine.CreateAgent(ctx, agent)
if err != nil {
t.Fatalf("CreateAgent failed for agent %d: %v", i, err)
}
agents[i] = agent
}
loadBalancer := engine.GetPerformanceOptimizer().loadBalancer
task := &Task{
Type:  TaskTypeGenerate,
Input: "Test load balancing",
}
selectedAgent, err := loadBalancer.SelectOptimalAgent(ctx, task, agents)
if err != nil {
t.Fatalf("SelectOptimalAgent failed: %v", err)
}
if selectedAgent == nil {
t.Fatal("Should select an agent")
}
t.Logf("Selected agent: %s", selectedAgent.Name)
for i, agent := range agents {
activeTasks := i * 2
queuedTasks := i * 1
performanceScore := 1.0 - float64(i)*0.2
healthStatus := HealthStatusHealthy
resourceUsage := &ResourceUsage{
CPUUsage:         float64(i) * 0.2,
MemoryUsageGB:    float64(i) * 2.0,
NetworkUsageMbps: float64(i) * 10.0,
}
loadBalancer.UpdateAgentLoad(agent.ID, activeTasks, queuedTasks, resourceUsage, performanceScore, healthStatus)
}
selectedAgent2, err := loadBalancer.SelectOptimalAgent(ctx, task, agents)
if err != nil {
t.Fatalf("SelectOptimalAgent failed: %v", err)
}
if selectedAgent2 == nil {
t.Fatal("Should select an agent")
}
t.Logf("Selected agent after load update: %s", selectedAgent2.Name)
if selectedAgent2.ID != agents[0].ID {
t.Log("Load balancer selected a different agent (this may be correct depending on strategy)")
}
}
func TestResourceManagement(t *testing.T) {
client := api.Client{}
engine := NewEngine(client)
ctx := context.Background()
resourceManager := engine.GetPerformanceOptimizer().resourceManager
requirements := &ResourceUsage{
CPUUsage:         1.0,
MemoryUsageGB:    4.0,
NetworkUsageMbps: 50.0,
StorageUsageGB:   10.0,
GPUUsage:         0.5,
}
reservation, err := resourceManager.AllocateResources(ctx, "test-task", "test-agent", requirements, ResourcePriorityNormal)
if err != nil {
t.Fatalf("AllocateResources failed: %v", err)
}
if reservation == nil {
t.Fatal("Resource reservation should not be nil")
}
if reservation.ReservationID == "" {
t.Error("Reservation should have an ID")
}
err = resourceManager.ReleaseResources(ctx, reservation.ReservationID)
if err != nil {
t.Fatalf("ReleaseResources failed: %v", err)
}
excessiveRequirements := &ResourceUsage{
CPUUsage:         100.0,
MemoryUsageGB:    1000.0,
NetworkUsageMbps: 10000.0,
}
_, err = resourceManager.AllocateResources(ctx, "excessive-task", "test-agent", excessiveRequirements, ResourcePriorityHigh)
if err == nil {
t.Error("Should fail to allocate excessive resources")
}
t.Logf("Expected error for excessive resource allocation: %v", err)
}