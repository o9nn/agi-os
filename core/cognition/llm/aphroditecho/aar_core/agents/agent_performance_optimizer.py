"""
Agent Performance Optimizer
Enhanced performance monitoring and optimization capabilities for Agent Manager.

This module adds advanced performance optimization features:
- Real-time resource optimization
- Intelligent load balancing
- Agent performance prediction
- Dynamic resource allocation
"""

import logging
import time
import statistics
from typing import Dict, List, Any, Optional
from dataclasses import dataclass, field
from enum import Enum

logger = logging.getLogger(__name__)


class OptimizationStrategy(Enum):
    """Available optimization strategies."""
    ROUND_ROBIN = "round_robin"
    LEAST_LOADED = "least_loaded"
    PERFORMANCE_BASED = "performance_based"
    CAPABILITY_WEIGHTED = "capability_weighted"


@dataclass
class PerformanceMetrics:
    """Comprehensive performance metrics for agents."""
    response_times: List[float] = field(default_factory=list)
    throughput_history: List[float] = field(default_factory=list)
    error_rates: List[float] = field(default_factory=list)
    resource_utilization: List[float] = field(default_factory=list)
    capability_scores: Dict[str, float] = field(default_factory=dict)
    last_updated: float = field(default_factory=time.time)
    
    def add_measurement(self, response_time: float, success: bool, 
                       resource_usage: float = 0.0):
        """Add a new performance measurement."""
        self.response_times.append(response_time)
        self.resource_utilization.append(resource_usage)
        
        # Keep only recent measurements (last 100)
        if len(self.response_times) > 100:
            self.response_times = self.response_times[-100:]
            self.resource_utilization = self.resource_utilization[-100:]
        
        self.last_updated = time.time()
    
    def get_performance_score(self) -> float:
        """Calculate overall performance score (0.0 to 1.0)."""
        if not self.response_times:
            return 0.5  # Default neutral score
        
        # Calculate score based on response time and success rate
        avg_response_time = statistics.mean(self.response_times)
        max_response_time = max(self.response_times) if self.response_times else 1.0
        
        # Normalize response time (lower is better)
        time_score = max(0.0, 1.0 - (avg_response_time / max(max_response_time, 1.0)))
        
        # Factor in resource efficiency
        resource_score = 1.0
        if self.resource_utilization:
            avg_resource = statistics.mean(self.resource_utilization)
            resource_score = max(0.0, 1.0 - avg_resource)  # Lower utilization is better
        
        # Combine scores
        return (time_score * 0.7) + (resource_score * 0.3)


class AgentPerformanceOptimizer:
    """Advanced performance optimizer for Agent Manager."""
    
    def __init__(self, optimization_strategy: OptimizationStrategy = OptimizationStrategy.PERFORMANCE_BASED):
        self.strategy = optimization_strategy
        self.agent_metrics: Dict[str, PerformanceMetrics] = {}
        self.load_balancer_state: Dict[str, int] = {}  # For round-robin
        self.optimization_interval = 30.0  # seconds
        self.last_optimization = time.time()
        
        # Performance thresholds
        self.slow_response_threshold = 2.0  # seconds
        self.high_utilization_threshold = 0.8  # 80%
        self.min_performance_score = 0.3
        
        logger.info(f"Agent Performance Optimizer initialized with strategy: {self.strategy.value}")
    
    def register_agent(self, agent_id: str) -> None:
        """Register an agent for performance monitoring."""
        if agent_id not in self.agent_metrics:
            self.agent_metrics[agent_id] = PerformanceMetrics()
            self.load_balancer_state[agent_id] = 0
            logger.debug(f"Registered agent {agent_id} for performance monitoring")
    
    def unregister_agent(self, agent_id: str) -> None:
        """Unregister an agent from performance monitoring."""
        self.agent_metrics.pop(agent_id, None)
        self.load_balancer_state.pop(agent_id, None)
        logger.debug(f"Unregistered agent {agent_id} from performance monitoring")
    
    def record_performance(self, agent_id: str, response_time: float, 
                          success: bool, resource_usage: float = 0.0) -> None:
        """Record performance metrics for an agent."""
        if agent_id in self.agent_metrics:
            self.agent_metrics[agent_id].add_measurement(response_time, success, resource_usage)
    
    def get_optimal_agents(self, available_agents: List[str], 
                          count: int = 1, 
                          capability_requirements: Optional[Dict[str, Any]] = None) -> List[str]:
        """Select optimal agents based on current strategy."""
        if not available_agents:
            return []
        
        if self.strategy == OptimizationStrategy.ROUND_ROBIN:
            return self._round_robin_selection(available_agents, count)
        elif self.strategy == OptimizationStrategy.LEAST_LOADED:
            return self._least_loaded_selection(available_agents, count)
        elif self.strategy == OptimizationStrategy.PERFORMANCE_BASED:
            return self._performance_based_selection(available_agents, count)
        elif self.strategy == OptimizationStrategy.CAPABILITY_WEIGHTED:
            return self._capability_weighted_selection(available_agents, count, capability_requirements)
        else:
            return available_agents[:count]  # Fallback
    
    def _round_robin_selection(self, agents: List[str], count: int) -> List[str]:
        """Round-robin agent selection."""
        selected = []
        for _ in range(min(count, len(agents))):
            # Find agent with lowest round-robin counter
            min_count = min(self.load_balancer_state.get(agent_id, 0) for agent_id in agents)
            candidates = [agent_id for agent_id in agents 
                         if self.load_balancer_state.get(agent_id, 0) == min_count]
            
            agent_id = candidates[0]  # Take first candidate
            selected.append(agent_id)
            self.load_balancer_state[agent_id] = self.load_balancer_state.get(agent_id, 0) + 1
            
            # Remove selected agent from remaining choices
            agents = [a for a in agents if a != agent_id]
            if not agents:
                break
        
        return selected
    
    def _least_loaded_selection(self, agents: List[str], count: int) -> List[str]:
        """Select agents with lowest current load."""
        # Sort by resource utilization (lower is better)
        agent_loads = []
        for agent_id in agents:
            metrics = self.agent_metrics.get(agent_id, PerformanceMetrics())
            current_load = statistics.mean(metrics.resource_utilization[-5:]) if metrics.resource_utilization else 0.0
            agent_loads.append((current_load, agent_id))
        
        agent_loads.sort(key=lambda x: x[0])  # Sort by load ascending
        return [agent_id for _, agent_id in agent_loads[:count]]
    
    def _performance_based_selection(self, agents: List[str], count: int) -> List[str]:
        """Select agents based on performance scores."""
        agent_scores = []
        for agent_id in agents:
            metrics = self.agent_metrics.get(agent_id, PerformanceMetrics())
            score = metrics.get_performance_score()
            agent_scores.append((score, agent_id))
        
        agent_scores.sort(key=lambda x: x[0], reverse=True)  # Sort by score descending
        return [agent_id for _, agent_id in agent_scores[:count]]
    
    def _capability_weighted_selection(self, agents: List[str], count: int, 
                                     requirements: Optional[Dict[str, Any]] = None) -> List[str]:
        """Select agents weighted by capability match and performance."""
        if not requirements:
            return self._performance_based_selection(agents, count)
        
        agent_scores = []
        for agent_id in agents:
            metrics = self.agent_metrics.get(agent_id, PerformanceMetrics())
            perf_score = metrics.get_performance_score()
            
            # Calculate capability match score
            capability_score = self._calculate_capability_score(agent_id, requirements)
            
            # Combined score (60% capability, 40% performance)
            combined_score = (capability_score * 0.6) + (perf_score * 0.4)
            agent_scores.append((combined_score, agent_id))
        
        agent_scores.sort(key=lambda x: x[0], reverse=True)
        return [agent_id for _, agent_id in agent_scores[:count]]
    
    def _calculate_capability_score(self, agent_id: str, requirements: Dict[str, Any]) -> float:
        """Calculate how well an agent matches capability requirements."""
        # This would integrate with the actual agent capabilities
        # For now, return a default score
        return 0.8
    
    def get_performance_report(self) -> Dict[str, Any]:
        """Generate comprehensive performance report."""
        report = {
            'optimization_strategy': self.strategy.value,
            'total_agents_monitored': len(self.agent_metrics),
            'last_optimization': self.last_optimization,
            'agent_performance': {}
        }
        
        # Individual agent metrics
        for agent_id, metrics in self.agent_metrics.items():
            agent_report = {
                'performance_score': metrics.get_performance_score(),
                'avg_response_time': statistics.mean(metrics.response_times) if metrics.response_times else 0.0,
                'measurements_count': len(metrics.response_times),
                'last_updated': metrics.last_updated
            }
            
            if metrics.resource_utilization:
                agent_report['avg_resource_utilization'] = statistics.mean(metrics.resource_utilization)
            
            report['agent_performance'][agent_id] = agent_report
        
        # System-wide statistics
        all_scores = [metrics.get_performance_score() for metrics in self.agent_metrics.values()]
        if all_scores:
            report['system_performance'] = {
                'average_score': statistics.mean(all_scores),
                'min_score': min(all_scores),
                'max_score': max(all_scores),
                'score_deviation': statistics.stdev(all_scores) if len(all_scores) > 1 else 0.0
            }
        
        return report
    
    def should_optimize(self) -> bool:
        """Check if optimization should be run."""
        return time.time() - self.last_optimization > self.optimization_interval
    
    def identify_underperforming_agents(self) -> List[str]:
        """Identify agents that are underperforming."""
        underperforming = []
        for agent_id, metrics in self.agent_metrics.items():
            score = metrics.get_performance_score()
            if score < self.min_performance_score:
                underperforming.append(agent_id)
        
        return underperforming
    
    def suggest_optimizations(self) -> List[Dict[str, Any]]:
        """Suggest system optimizations."""
        suggestions = []
        
        # Check for underperforming agents
        underperforming = self.identify_underperforming_agents()
        if underperforming:
            suggestions.append({
                'type': 'agent_optimization',
                'action': 'evolve_or_replace',
                'agents': underperforming,
                'reason': 'Performance below threshold'
            })
        
        # Check for load balancing issues
        if len(self.agent_metrics) > 1:
            scores = [metrics.get_performance_score() for metrics in self.agent_metrics.values()]
            if len(scores) > 1 and statistics.stdev(scores) > 0.3:
                suggestions.append({
                    'type': 'load_balancing',
                    'action': 'redistribute_load',
                    'reason': 'High performance variance detected'
                })
        
        return suggestions
    
    async def optimize_system(self, agent_manager) -> Dict[str, Any]:
        """Run system optimization."""
        start_time = time.time()
        logger.info("Starting system optimization...")
        
        suggestions = self.suggest_optimizations()
        applied_optimizations = []
        
        for suggestion in suggestions:
            if suggestion['type'] == 'agent_optimization':
                # Trigger evolution for underperforming agents
                for agent_id in suggestion['agents']:
                    if hasattr(agent_manager, 'evolve_agent'):
                        evolution_data = {'performance_score': 0.3, 'optimization_triggered': True}
                        await agent_manager.evolve_agent(agent_id, evolution_data)
                        applied_optimizations.append(f"Evolved agent {agent_id}")
            
            elif suggestion['type'] == 'load_balancing':
                # Switch to least-loaded strategy temporarily
                old_strategy = self.strategy
                self.strategy = OptimizationStrategy.LEAST_LOADED
                applied_optimizations.append(f"Switched from {old_strategy.value} to least_loaded strategy")
        
        self.last_optimization = time.time()
        optimization_time = time.time() - start_time
        
        logger.info(f"System optimization completed in {optimization_time:.2f}s")
        
        return {
            'suggestions': suggestions,
            'applied_optimizations': applied_optimizations,
            'optimization_time': optimization_time,
            'timestamp': time.time()
        }