package echobeats
func (eb *EchoBeats) GetQueueSize() int {
eb.mu.RLock()
defer eb.mu.RUnlock()
return eb.eventQueue.Len()
}
func (eb *EchoBeats) GetState() SchedulerState {
eb.mu.RLock()
defer eb.mu.RUnlock()
return eb.state
}
func (eb *EchoBeats) GetMetrics() map[string]interface{} {
eb.metrics.mu.RLock()
defer eb.metrics.mu.RUnlock()
return map[string]interface{}{
"events_processed":    eb.metrics.EventsProcessed,
"events_scheduled":    eb.metrics.EventsScheduled,
"average_latency":     eb.metrics.AverageLatency.String(),
"cycles_completed":    eb.metrics.CyclesCompleted,
"current_load":        eb.metrics.CurrentLoad,
"autonomous_thoughts": eb.metrics.AutonomousThoughts,
"last_heartbeat":      eb.metrics.LastHeartbeat.Format("15:04:05"),
}
}
func (eb *EchoBeats) SetState(state SchedulerState) {
eb.mu.Lock()
defer eb.mu.Unlock()
eb.state = state
}