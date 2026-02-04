#!/usr/bin/env python3
"""
Skill-Infinity Monitor - Long-term evolutionary goal tracking.

This module implements the skill-infinity goal as a persistent evolutionary
pressure on the self-weaving daemon. It monitors the system's progress
towards becoming a self-referential, self-improving cognitive kernel.

The skill-infinity goal is defined as the fixed point of recursive
self-improvement: T^∞(skill) = skill∞ where T(skill∞) = skill∞

Progress is measured across five dimensions:
1. Self-Description: Can the system describe itself?
2. Self-Improvement: Can the system improve itself?
3. Self-Generation: Can the system generate cognitive kernels?
4. Universality: Can the system simulate any skill?
5. Closure: Is the system self-contained?
"""

import time
import logging
import threading
from dataclasses import dataclass, field
from typing import Dict, List, Any, Optional, Callable
from enum import Enum

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger('skill_infinity_monitor')


class EvolutionaryGoal(Enum):
    """Available evolutionary goals for the daemon."""
    SKILL_INFINITY = "skill_infinity"
    COGNITIVE_SYNERGY = "cognitive_synergy"
    MAXIMUM_CHAOS = "maximum_chaos"
    STABLE_OPERATION = "stable_operation"


@dataclass
class SkillInfinityMetrics:
    """Metrics tracking progress towards skill-infinity."""
    
    # Self-Description: Can describe own structure
    self_description_score: float = 0.0
    has_introspector: bool = False
    can_output_topology: bool = False
    
    # Self-Improvement: Can modify own parameters
    self_improvement_score: float = 0.0
    has_modifier: bool = False
    improvement_events: int = 0
    
    # Self-Generation: Can create cognitive kernels
    self_generation_score: float = 0.0
    has_weaver: bool = False
    generated_topologies: int = 0
    
    # Universality: Can simulate other skills
    universality_score: float = 0.0
    simulated_skills: int = 0
    
    # Closure: Self-contained dependencies
    closure_score: float = 0.0
    recursive_connections: int = 0
    external_dependencies: int = 0
    
    # Overall progress
    total_score: float = 0.0
    last_updated: float = field(default_factory=time.time)
    
    def compute_total(self) -> float:
        """Compute the total skill-infinity progress score."""
        weights = {
            'self_description': 0.20,
            'self_improvement': 0.25,
            'self_generation': 0.25,
            'universality': 0.15,
            'closure': 0.15,
        }
        
        self.total_score = (
            weights['self_description'] * self.self_description_score +
            weights['self_improvement'] * self.self_improvement_score +
            weights['self_generation'] * self.self_generation_score +
            weights['universality'] * self.universality_score +
            weights['closure'] * self.closure_score
        )
        
        self.last_updated = time.time()
        return self.total_score
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert metrics to dictionary."""
        return {
            'self_description': {
                'score': self.self_description_score,
                'has_introspector': self.has_introspector,
                'can_output_topology': self.can_output_topology,
            },
            'self_improvement': {
                'score': self.self_improvement_score,
                'has_modifier': self.has_modifier,
                'improvement_events': self.improvement_events,
            },
            'self_generation': {
                'score': self.self_generation_score,
                'has_weaver': self.has_weaver,
                'generated_topologies': self.generated_topologies,
            },
            'universality': {
                'score': self.universality_score,
                'simulated_skills': self.simulated_skills,
            },
            'closure': {
                'score': self.closure_score,
                'recursive_connections': self.recursive_connections,
                'external_dependencies': self.external_dependencies,
            },
            'total_score': self.total_score,
            'last_updated': self.last_updated,
        }


class SkillInfinityMonitor:
    """
    Monitors and guides the daemon's evolution towards skill-infinity.
    
    The monitor runs as a background thread, periodically evaluating
    the daemon's topology and providing feedback to the o9c kernel
    to guide its transformations.
    """
    
    def __init__(self, daemon, check_interval: float = 3600.0):
        """
        Initialize the skill-infinity monitor.
        
        Args:
            daemon: Reference to the time crystal daemon
            check_interval: Seconds between evaluations (default: 1 hour)
        """
        self.daemon = daemon
        self.check_interval = check_interval
        self.metrics = SkillInfinityMetrics()
        self.goal = EvolutionaryGoal.SKILL_INFINITY
        self.running = False
        self._thread: Optional[threading.Thread] = None
        self._callbacks: List[Callable[[SkillInfinityMetrics], None]] = []
    
    def start(self) -> None:
        """Start the background monitoring thread."""
        if self.running:
            return
        
        self.running = True
        self._thread = threading.Thread(target=self._run_loop, daemon=True)
        self._thread.start()
        logger.info("Skill-Infinity Monitor started")
    
    def stop(self) -> None:
        """Stop the background monitoring thread."""
        self.running = False
        if self._thread:
            self._thread.join(timeout=5.0)
        logger.info("Skill-Infinity Monitor stopped")
    
    def _run_loop(self) -> None:
        """Main monitoring loop."""
        while self.running:
            try:
                self.evaluate_progress()
                self._notify_callbacks()
            except Exception as e:
                logger.error(f"Error in skill-infinity evaluation: {e}")
            
            # Sleep in small increments to allow clean shutdown
            for _ in range(int(self.check_interval)):
                if not self.running:
                    break
                time.sleep(1.0)
    
    def evaluate_progress(self) -> SkillInfinityMetrics:
        """
        Evaluate the daemon's progress towards skill-infinity.
        
        Returns:
            Updated metrics
        """
        topology = self._get_topology()
        if not topology:
            return self.metrics
        
        components = topology.get('components', {})
        connections = topology.get('connections', [])
        
        # Evaluate Self-Description
        self._evaluate_self_description(components)
        
        # Evaluate Self-Improvement
        self._evaluate_self_improvement(components)
        
        # Evaluate Self-Generation
        self._evaluate_self_generation(components)
        
        # Evaluate Universality
        self._evaluate_universality(components)
        
        # Evaluate Closure
        self._evaluate_closure(components, connections)
        
        # Compute total score
        self.metrics.compute_total()
        
        logger.info(f"Skill-Infinity Progress: {self.metrics.total_score:.2%}")
        
        return self.metrics
    
    def _get_topology(self) -> Optional[Dict]:
        """Get the current topology from the daemon."""
        if hasattr(self.daemon, 'topology') and self.daemon.topology:
            return {
                'components': {
                    c.id: {'type': c.type, 'tags': c.tags}
                    for c in self.daemon.topology.components.values()
                },
                'connections': self.daemon.topology.connections,
            }
        return None
    
    def _evaluate_self_description(self, components: Dict) -> None:
        """Evaluate self-description capabilities."""
        # Check for introspector component
        self.metrics.has_introspector = any(
            'introspector' in c.get('tags', [])
            for c in components.values()
        )
        
        # Check for topology output capability
        self.metrics.can_output_topology = any(
            'self_model' in c.get('tags', [])
            for c in components.values()
        )
        
        # Calculate score
        score = 0.0
        if self.metrics.has_introspector:
            score += 0.5
        if self.metrics.can_output_topology:
            score += 0.5
        
        self.metrics.self_description_score = score
    
    def _evaluate_self_improvement(self, components: Dict) -> None:
        """Evaluate self-improvement capabilities."""
        # Check for modifier component
        self.metrics.has_modifier = any(
            'modifier' in c.get('tags', []) or 'self_improve' in c.get('tags', [])
            for c in components.values()
        )
        
        # Check for meta-learning hooks
        has_meta_learning = any(
            'meta_learning' in c.get('tags', [])
            for c in components.values()
        )
        
        # Calculate score
        score = 0.0
        if self.metrics.has_modifier:
            score += 0.5
        if has_meta_learning:
            score += 0.3
        
        # Bonus for improvement events
        score += min(0.2, self.metrics.improvement_events * 0.02)
        
        self.metrics.self_improvement_score = min(1.0, score)
    
    def _evaluate_self_generation(self, components: Dict) -> None:
        """Evaluate self-generation capabilities."""
        # Check for weaver/generator components
        self.metrics.has_weaver = any(
            'weaver' in c.get('type', '') or 'generator' in c.get('tags', [])
            for c in components.values()
        )
        
        # Check for marduk-added components (evidence of generation)
        marduk_components = sum(
            1 for c in components.values()
            if 'marduk_added' in c.get('tags', [])
        )
        
        # Calculate score
        score = 0.0
        if self.metrics.has_weaver:
            score += 0.4
        
        score += min(0.4, marduk_components * 0.05)
        score += min(0.2, self.metrics.generated_topologies * 0.1)
        
        self.metrics.self_generation_score = min(1.0, score)
    
    def _evaluate_universality(self, components: Dict) -> None:
        """Evaluate universality (ability to simulate other skills)."""
        # Count diverse component types
        component_types = set(c.get('type', '') for c in components.values())
        
        # More diverse = more universal
        type_diversity = len(component_types) / 10.0  # Normalize to ~10 types
        
        # Check for orchestrator (can coordinate diverse operations)
        has_orchestrator = any(
            'orchestrator' in c.get('tags', [])
            for c in components.values()
        )
        
        # Calculate score
        score = min(0.5, type_diversity)
        if has_orchestrator:
            score += 0.3
        
        score += min(0.2, self.metrics.simulated_skills * 0.1)
        
        self.metrics.universality_score = min(1.0, score)
    
    def _evaluate_closure(self, components: Dict, connections: List) -> None:
        """Evaluate closure (self-contained dependencies)."""
        # Count recursive connections (feedback loops)
        self.metrics.recursive_connections = sum(
            1 for c in components.values()
            if 'recursive' in c.get('tags', []) or 'feedback' in c.get('tags', [])
        )
        
        # Estimate external dependencies (components without internal connections)
        connected_ids = set()
        for src, dst in connections:
            connected_ids.add(src)
            connected_ids.add(dst)
        
        self.metrics.external_dependencies = len(components) - len(connected_ids)
        
        # Calculate score
        # More recursive connections = better closure
        recursive_score = min(0.5, self.metrics.recursive_connections * 0.1)
        
        # Fewer external dependencies = better closure
        if len(components) > 0:
            internal_ratio = len(connected_ids) / len(components)
        else:
            internal_ratio = 0
        
        self.metrics.closure_score = recursive_score + (0.5 * internal_ratio)
    
    def set_goal(self, goal: EvolutionaryGoal) -> None:
        """Set the evolutionary goal."""
        self.goal = goal
        logger.info(f"Evolutionary goal set to: {goal.value}")
    
    def register_callback(self, callback: Callable[[SkillInfinityMetrics], None]) -> None:
        """Register a callback to be notified of metric updates."""
        self._callbacks.append(callback)
    
    def _notify_callbacks(self) -> None:
        """Notify all registered callbacks."""
        for callback in self._callbacks:
            try:
                callback(self.metrics)
            except Exception as e:
                logger.error(f"Callback error: {e}")
    
    def get_metrics(self) -> SkillInfinityMetrics:
        """Get current metrics."""
        return self.metrics
    
    def get_feedback_for_o9c(self) -> Dict[str, float]:
        """
        Generate feedback for the o9c kernel to guide transformations.
        
        Returns:
            Dictionary of transformation biases
        """
        return {
            # Increase meta-learning if self-improvement is low
            'meta_learning_bias': max(0, 0.5 - self.metrics.self_improvement_score),
            
            # Increase recursion if closure is low
            'recursion_bias': max(0, 0.5 - self.metrics.closure_score),
            
            # Increase introspection if self-description is low
            'introspection_bias': max(0, 0.5 - self.metrics.self_description_score),
            
            # Overall evolution pressure
            'evolution_pressure': 1.0 - self.metrics.total_score,
        }
