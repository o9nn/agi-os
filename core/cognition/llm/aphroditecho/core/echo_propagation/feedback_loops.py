"""
Feedback Loops - Echo Propagation Recursive Dynamics
Part of Deep Tree Echo Core

This module implements feedback loop mechanisms for the hypergraph.
Feedback loops enable recursive self-modification and adaptive behavior.
"""

import numpy as np
from typing import Dict, List, Set, Tuple, Optional, Callable
from dataclasses import dataclass
from enum import Enum
import json


class FeedbackType(Enum):
    """Types of feedback loops"""
    POSITIVE = "positive"      # Amplifying feedback
    NEGATIVE = "negative"      # Dampening feedback
    HOMEOSTATIC = "homeostatic"  # Stabilizing feedback
    RECURSIVE = "recursive"    # Self-referential feedback


@dataclass
class FeedbackLoop:
    """Represents a feedback loop in the hypergraph"""
    loop_id: str
    node_sequence: List[str]
    feedback_type: FeedbackType
    strength: float
    gain: float
    metadata: Dict
    
    def to_dict(self) -> Dict:
        return {
            "loop_id": self.loop_id,
            "node_sequence": self.node_sequence,
            "feedback_type": self.feedback_type.value,
            "strength": self.strength,
            "gain": self.gain,
            "metadata": self.metadata
        }


class FeedbackLoopManager:
    """
    Manages feedback loops in the Deep Tree Echo hypergraph.
    
    Feedback loops are critical for:
    - Self-regulation and homeostasis
    - Adaptive learning and evolution
    - Recursive self-modification
    - Emergent behavior
    """
    
    def __init__(
        self,
        positive_gain: float = 1.2,
        negative_gain: float = 0.8,
        homeostatic_target: float = 0.5
    ):
        """
        Initialize the feedback loop manager.
        
        Args:
            positive_gain: Amplification factor for positive feedback
            negative_gain: Dampening factor for negative feedback
            homeostatic_target: Target level for homeostatic feedback
        """
        self.positive_gain = positive_gain
        self.negative_gain = negative_gain
        self.homeostatic_target = homeostatic_target
        
        self.feedback_loops: Dict[str, FeedbackLoop] = {}
        self.loop_history: List[Dict] = []
    
    def detect_feedback_loops(
        self,
        hypergraph: Dict,
        min_loop_length: int = 2,
        max_loop_length: int = 10
    ) -> List[FeedbackLoop]:
        """
        Detect feedback loops in the hypergraph.
        
        Args:
            hypergraph: Hypergraph data structure
            min_loop_length: Minimum loop length
            max_loop_length: Maximum loop length
        
        Returns:
            List of detected feedback loops
        """
        loops = []
        
        # Build directed graph from hyperedges
        graph = self._build_directed_graph(hypergraph)
        
        # Find cycles using DFS
        visited = set()
        rec_stack = set()
        path = []
        
        def dfs(node):
            visited.add(node)
            rec_stack.add(node)
            path.append(node)
            
            for neighbor in graph.get(node, []):
                if neighbor not in visited:
                    dfs(neighbor)
                elif neighbor in rec_stack:
                    # Found a cycle
                    cycle_start = path.index(neighbor)
                    cycle = path[cycle_start:]
                    
                    if min_loop_length <= len(cycle) <= max_loop_length:
                        loop = self._create_feedback_loop(cycle, hypergraph)
                        loops.append(loop)
            
            path.pop()
            rec_stack.remove(node)
        
        for node in hypergraph['hypernodes'].keys():
            if node not in visited:
                dfs(node)
        
        # Store detected loops
        for loop in loops:
            self.feedback_loops[loop.loop_id] = loop
        
        return loops
    
    def apply_feedback(
        self,
        activations: Dict[str, float],
        hypergraph: Dict,
        feedback_loops: Optional[List[FeedbackLoop]] = None
    ) -> Dict[str, float]:
        """
        Apply feedback loop modulation to activations.
        
        Args:
            activations: Current activation levels
            hypergraph: Hypergraph data structure
            feedback_loops: Specific loops to apply (None = all)
        
        Returns:
            Modulated activation levels
        """
        if feedback_loops is None:
            feedback_loops = list(self.feedback_loops.values())
        
        modulated = activations.copy()
        
        for loop in feedback_loops:
            # Calculate loop activation
            loop_activation = self._calculate_loop_activation(loop, activations)
            
            # Apply feedback based on type
            if loop.feedback_type == FeedbackType.POSITIVE:
                modulated = self._apply_positive_feedback(
                    modulated, loop, loop_activation
                )
            elif loop.feedback_type == FeedbackType.NEGATIVE:
                modulated = self._apply_negative_feedback(
                    modulated, loop, loop_activation
                )
            elif loop.feedback_type == FeedbackType.HOMEOSTATIC:
                modulated = self._apply_homeostatic_feedback(
                    modulated, loop, loop_activation
                )
            elif loop.feedback_type == FeedbackType.RECURSIVE:
                modulated = self._apply_recursive_feedback(
                    modulated, loop, loop_activation
                )
        
        # Record feedback application
        self._record_feedback_state(activations, modulated, feedback_loops)
        
        return modulated
    
    def _build_directed_graph(self, hypergraph: Dict) -> Dict[str, List[str]]:
        """Build directed graph from hypergraph"""
        graph = {}
        
        for edge_data in hypergraph['hyperedges'].values():
            sources = edge_data['source_node_ids']
            targets = edge_data['target_node_ids']
            
            for source in sources:
                if source not in graph:
                    graph[source] = []
                graph[source].extend(targets)
        
        return graph
    
    def _create_feedback_loop(
        self,
        cycle: List[str],
        hypergraph: Dict
    ) -> FeedbackLoop:
        """Create a feedback loop from a cycle"""
        # Determine feedback type based on edge types in the cycle
        edge_types = []
        for i in range(len(cycle)):
            source = cycle[i]
            target = cycle[(i + 1) % len(cycle)]
            
            # Find edge connecting source to target
            for edge_data in hypergraph['hyperedges'].values():
                if (source in edge_data['source_node_ids'] and
                    target in edge_data['target_node_ids']):
                    edge_types.append(edge_data['edge_type'])
                    break
        
        # Classify feedback type
        if 'feedback' in edge_types:
            feedback_type = FeedbackType.RECURSIVE
        elif edge_types.count('causal') > len(edge_types) / 2:
            feedback_type = FeedbackType.POSITIVE
        elif edge_types.count('entropy') > 0:
            feedback_type = FeedbackType.HOMEOSTATIC
        else:
            feedback_type = FeedbackType.NEGATIVE
        
        # Calculate loop strength (average edge weight)
        total_weight = 0.0
        count = 0
        for i in range(len(cycle)):
            source = cycle[i]
            target = cycle[(i + 1) % len(cycle)]
            
            for edge_data in hypergraph['hyperedges'].values():
                if (source in edge_data['source_node_ids'] and
                    target in edge_data['target_node_ids']):
                    total_weight += edge_data['weight']
                    count += 1
                    break
        
        strength = total_weight / count if count > 0 else 0.5
        
        return FeedbackLoop(
            loop_id=f"loop_{'_'.join(cycle[:3])}",
            node_sequence=cycle,
            feedback_type=feedback_type,
            strength=strength,
            gain=self._get_gain_for_type(feedback_type),
            metadata={"edge_types": edge_types, "length": len(cycle)}
        )
    
    def _get_gain_for_type(self, feedback_type: FeedbackType) -> float:
        """Get gain factor for feedback type"""
        if feedback_type == FeedbackType.POSITIVE:
            return self.positive_gain
        elif feedback_type == FeedbackType.NEGATIVE:
            return self.negative_gain
        elif feedback_type == FeedbackType.HOMEOSTATIC:
            return 1.0
        else:  # RECURSIVE
            return 1.1
    
    def _calculate_loop_activation(
        self,
        loop: FeedbackLoop,
        activations: Dict[str, float]
    ) -> float:
        """Calculate total activation in a feedback loop"""
        total = sum(activations.get(node_id, 0.0) for node_id in loop.node_sequence)
        return total / len(loop.node_sequence)
    
    def _apply_positive_feedback(
        self,
        activations: Dict[str, float],
        loop: FeedbackLoop,
        loop_activation: float
    ) -> Dict[str, float]:
        """Apply positive (amplifying) feedback"""
        modulated = activations.copy()
        
        for node_id in loop.node_sequence:
            current = modulated.get(node_id, 0.0)
            # Amplify based on loop activation
            modulated[node_id] = min(current * loop.gain * (1 + loop_activation), 1.0)
        
        return modulated
    
    def _apply_negative_feedback(
        self,
        activations: Dict[str, float],
        loop: FeedbackLoop,
        loop_activation: float
    ) -> Dict[str, float]:
        """Apply negative (dampening) feedback"""
        modulated = activations.copy()
        
        for node_id in loop.node_sequence:
            current = modulated.get(node_id, 0.0)
            # Dampen based on loop activation
            modulated[node_id] = max(current * loop.gain * (1 - loop_activation * 0.5), 0.0)
        
        return modulated
    
    def _apply_homeostatic_feedback(
        self,
        activations: Dict[str, float],
        loop: FeedbackLoop,
        loop_activation: float
    ) -> Dict[str, float]:
        """Apply homeostatic (stabilizing) feedback"""
        modulated = activations.copy()
        
        # Push toward target level
        for node_id in loop.node_sequence:
            current = modulated.get(node_id, 0.0)
            error = self.homeostatic_target - current
            modulated[node_id] = current + (error * loop.strength * 0.5)
        
        return modulated
    
    def _apply_recursive_feedback(
        self,
        activations: Dict[str, float],
        loop: FeedbackLoop,
        loop_activation: float
    ) -> Dict[str, float]:
        """Apply recursive (self-referential) feedback"""
        modulated = activations.copy()
        
        # Each node's activation affects the next in sequence
        for i, node_id in enumerate(loop.node_sequence):
            prev_node = loop.node_sequence[i - 1]
            prev_activation = activations.get(prev_node, 0.0)
            current = modulated.get(node_id, 0.0)
            
            # Recursive modulation
            modulated[node_id] = (current + prev_activation * loop.strength) / 2.0
        
        return modulated
    
    def _record_feedback_state(
        self,
        original: Dict[str, float],
        modulated: Dict[str, float],
        loops: List[FeedbackLoop]
    ):
        """Record feedback application state"""
        import time
        
        state = {
            "timestamp": time.time(),
            "loops_applied": [loop.loop_id for loop in loops],
            "original_activation": sum(original.values()),
            "modulated_activation": sum(modulated.values()),
            "delta": sum(modulated.values()) - sum(original.values())
        }
        
        self.loop_history.append(state)
    
    def analyze_loop_stability(
        self,
        loop: FeedbackLoop,
        initial_activations: Dict[str, float],
        iterations: int = 100
    ) -> Dict:
        """
        Analyze the stability of a feedback loop.
        
        Args:
            loop: Feedback loop to analyze
            initial_activations: Initial activation levels
            iterations: Number of iterations to simulate
        
        Returns:
            Stability analysis results
        """
        activations = initial_activations.copy()
        history = [sum(activations.values())]
        
        for _ in range(iterations):
            activations = self.apply_feedback(activations, {}, [loop])
            history.append(sum(activations.values()))
        
        # Analyze convergence
        final_values = history[-10:]
        variance = np.var(final_values)
        is_stable = variance < 0.01
        
        # Analyze oscillation
        diffs = [history[i+1] - history[i] for i in range(len(history)-1)]
        sign_changes = sum(1 for i in range(len(diffs)-1) if diffs[i] * diffs[i+1] < 0)
        is_oscillating = sign_changes > len(diffs) * 0.3
        
        return {
            "loop_id": loop.loop_id,
            "is_stable": is_stable,
            "is_oscillating": is_oscillating,
            "final_variance": variance,
            "total_activation": history[-1],
            "convergence_rate": abs(history[-1] - history[0]) / iterations
        }
    
    def export_loops(self, filepath: str):
        """Export feedback loops to JSON file"""
        loops_data = [loop.to_dict() for loop in self.feedback_loops.values()]
        
        with open(filepath, 'w') as f:
            json.dump(loops_data, f, indent=2)
    
    def get_loop_statistics(self) -> Dict:
        """Get statistics about feedback loops"""
        type_counts = {ft.value: 0 for ft in FeedbackType}
        total_strength = 0.0
        
        for loop in self.feedback_loops.values():
            type_counts[loop.feedback_type.value] += 1
            total_strength += loop.strength
        
        return {
            "total_loops": len(self.feedback_loops),
            "loop_types": type_counts,
            "average_strength": total_strength / len(self.feedback_loops) if self.feedback_loops else 0.0,
            "history_length": len(self.loop_history)
        }


if __name__ == "__main__":
    # Test feedback loop manager
    manager = FeedbackLoopManager()
    
    # Sample hypergraph with feedback loops
    hypergraph = {
        "hypernodes": {
            "A": {},
            "B": {},
            "C": {},
        },
        "hyperedges": {
            "e1": {
                "source_node_ids": ["A"],
                "target_node_ids": ["B"],
                "edge_type": "causal",
                "weight": 0.8
            },
            "e2": {
                "source_node_ids": ["B"],
                "target_node_ids": ["C"],
                "edge_type": "causal",
                "weight": 0.7
            },
            "e3": {
                "source_node_ids": ["C"],
                "target_node_ids": ["A"],
                "edge_type": "feedback",
                "weight": 0.9
            }
        }
    }
    
    # Detect loops
    loops = manager.detect_feedback_loops(hypergraph)
    print(f"Detected {len(loops)} feedback loops")
    
    # Analyze stability
    for loop in loops:
        initial = {"A": 0.5, "B": 0.3, "C": 0.2}
        analysis = manager.analyze_loop_stability(loop, initial)
        print(f"\nLoop {loop.loop_id}:")
        print(f"  Type: {loop.feedback_type.value}")
        print(f"  Stable: {analysis['is_stable']}")
        print(f"  Oscillating: {analysis['is_oscillating']}")
    
    print(f"\nLoop Statistics: {manager.get_loop_statistics()}")
