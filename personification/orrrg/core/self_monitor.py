"""Self-monitoring layer for observing system states and behaviors."""

from typing import List, Dict, Any
from datetime import datetime, timedelta
import statistics
from .models import SystemObservation, ComponentState, BehavioralPattern


class SelfMonitor:
    """Monitors system states and detects patterns."""
    
    def __init__(self):
        self.observation_history: List[SystemObservation] = []
        self.pattern_history: List[BehavioralPattern] = []
        self.max_history_size = 1000
        
    async def observe_system(self, soc) -> SystemObservation:
        """Observe current system state."""
        # Collect component states
        component_states = self._collect_component_states(soc)
        
        # Measure resource utilization
        resource_utilization = self._measure_resources(soc)
        
        # Check event queue status
        event_queue_status = self._check_event_queue(soc)
        
        # Gather performance metrics
        performance_metrics = self._gather_performance_metrics(soc)
        
        observation = SystemObservation(
            component_states=component_states,
            resource_utilization=resource_utilization,
            event_queue_status=event_queue_status,
            performance_metrics=performance_metrics,
        )
        
        # Store observation
        self.observation_history.append(observation)
        if len(self.observation_history) > self.max_history_size:
            self.observation_history.pop(0)
            
        return observation
    
    def _collect_component_states(self, soc) -> List[ComponentState]:
        """Collect states of all system components."""
        states = []
        
        # Core system component
        states.append(ComponentState(
            component_id="core",
            status="active" if hasattr(soc, '_initialized') and soc._initialized else "idle",
            metrics={"uptime_seconds": getattr(soc, '_uptime', 0)},
        ))
        
        # Autognosis component
        if hasattr(soc, 'autognosis'):
            states.append(ComponentState(
                component_id="autognosis",
                status="running" if getattr(soc.autognosis, '_running', False) else "idle",
                metrics={"cycles_completed": getattr(soc.autognosis, '_cycle_count', 0)},
            ))
        
        return states
    
    def _measure_resources(self, soc) -> Dict[str, float]:
        """Measure resource utilization."""
        # Simplified resource measurement
        return {
            "memory_mb": 0.0,  # Would measure actual memory in production
            "cpu_percent": 0.0,
            "capacity_utilization": 0.45,  # Mock value
        }
    
    def _check_event_queue(self, soc) -> Dict[str, Any]:
        """Check event queue status."""
        return {
            "pending_events": 0,
            "processing_rate": 0.0,
            "queue_depth": 0,
        }
    
    def _gather_performance_metrics(self, soc) -> Dict[str, float]:
        """Gather performance metrics."""
        return {
            "response_time_ms": 0.0,
            "throughput": 0.0,
            "error_rate": 0.0,
        }
    
    def detect_patterns(self) -> List[BehavioralPattern]:
        """Detect behavioral patterns from observation history."""
        patterns = []
        
        if len(self.observation_history) < 2:
            return patterns
        
        # Pattern 1: Behavioral stability
        recent_obs = self.observation_history[-10:]
        if len(recent_obs) >= 5:
            cpu_values = [obs.resource_utilization.get("cpu_percent", 0) for obs in recent_obs]
            if cpu_values and statistics.stdev(cpu_values) < 5.0:
                patterns.append(BehavioralPattern(
                    pattern_type="behavioral_stability",
                    description="System showing stable behavioral patterns",
                    confidence=0.85,
                    observations=[f"CPU variance: {statistics.stdev(cpu_values):.2f}%"],
                ))
        
        # Pattern 2: Resource utilization trends
        if len(recent_obs) >= 3:
            capacities = [obs.resource_utilization.get("capacity_utilization", 0) for obs in recent_obs]
            avg_capacity = statistics.mean(capacities) if capacities else 0
            if avg_capacity < 0.6:
                patterns.append(BehavioralPattern(
                    pattern_type="resource_underutilization",
                    description=f"Components underutilized ({avg_capacity*100:.0f}% capacity)",
                    confidence=0.90,
                    observations=[f"Average capacity: {avg_capacity*100:.0f}%"],
                ))
        
        # Pattern 3: Consistent autognosis operation
        if len(self.observation_history) >= 5:
            patterns.append(BehavioralPattern(
                pattern_type="autognosis_active",
                description="Autognosis system maintaining consistent self-monitoring",
                confidence=0.95,
                observations=[f"{len(self.observation_history)} observations recorded"],
            ))
        
        # Store patterns
        self.pattern_history.extend(patterns)
        if len(self.pattern_history) > self.max_history_size:
            self.pattern_history = self.pattern_history[-self.max_history_size:]
        
        return patterns
    
    def detect_anomalies(self) -> List[BehavioralPattern]:
        """Detect anomalies through statistical analysis."""
        anomalies = []
        
        if len(self.observation_history) < 10:
            return anomalies
        
        # Check for sudden resource spikes
        recent = self.observation_history[-10:]
        cpu_values = [obs.resource_utilization.get("cpu_percent", 0) for obs in recent]
        
        if cpu_values:
            mean_cpu = statistics.mean(cpu_values)
            latest_cpu = cpu_values[-1]
            
            if latest_cpu > mean_cpu * 2 and latest_cpu > 50:
                anomalies.append(BehavioralPattern(
                    pattern_type="resource_anomaly",
                    description=f"CPU spike detected: {latest_cpu:.1f}% (avg: {mean_cpu:.1f}%)",
                    confidence=0.75,
                ))
        
        return anomalies
