"""
Echo Propagation Engine - Activation Spreading Module
Part of Deep Tree Echo Core

This module implements the activation spreading algorithm for the hypergraph memory system.
Activation propagates through hypernodes and hyperedges based on weights and connection types.
"""

import numpy as np
from typing import Dict, List, Set, Tuple, Optional
from dataclasses import dataclass
from enum import Enum
import json


class ActivationMode(Enum):
    """Activation propagation modes"""
    SPREADING = "spreading"          # Standard spreading activation
    RESONANCE = "resonance"          # Resonance-based activation
    ECHO = "echo"                    # Echo state dynamics
    FEEDBACK = "feedback"            # Feedback loop activation


@dataclass
class ActivationState:
    """Represents the activation state of a hypernode"""
    node_id: str
    activation_level: float
    timestamp: float
    mode: ActivationMode
    source_nodes: List[str]
    
    def to_dict(self) -> Dict:
        return {
            "node_id": self.node_id,
            "activation_level": self.activation_level,
            "timestamp": self.timestamp,
            "mode": self.mode.value,
            "source_nodes": self.source_nodes
        }


class ActivationEngine:
    """
    Core activation spreading engine for Deep Tree Echo hypergraph.
    
    Implements multiple activation propagation algorithms:
    - Spreading activation (classic)
    - Resonance activation (synchronized)
    - Echo state activation (reservoir computing)
    - Feedback loop activation (recursive)
    """
    
    def __init__(
        self,
        decay_rate: float = 0.1,
        threshold: float = 0.01,
        max_iterations: int = 10,
        learning_rate: float = 0.01
    ):
        """
        Initialize the activation engine.
        
        Args:
            decay_rate: Rate at which activation decays (0-1)
            threshold: Minimum activation level to propagate
            max_iterations: Maximum propagation iterations
            learning_rate: Learning rate for weight updates
        """
        self.decay_rate = decay_rate
        self.threshold = threshold
        self.max_iterations = max_iterations
        self.learning_rate = learning_rate
        
        # Activation history
        self.activation_history: List[Dict[str, ActivationState]] = []
        
        # Edge weights (learned)
        self.edge_weights: Dict[str, float] = {}
    
    def propagate_activation(
        self,
        hypergraph: Dict,
        initial_activations: Dict[str, float],
        mode: ActivationMode = ActivationMode.SPREADING
    ) -> Dict[str, float]:
        """
        Propagate activation through the hypergraph.
        
        Args:
            hypergraph: Hypergraph data structure
            initial_activations: Initial activation levels {node_id: activation}
            mode: Activation propagation mode
        
        Returns:
            Final activation levels for all nodes
        """
        # Initialize activation levels
        activations = {node_id: 0.0 for node_id in hypergraph['hypernodes'].keys()}
        activations.update(initial_activations)
        
        # Propagation loop
        for iteration in range(self.max_iterations):
            new_activations = activations.copy()
            
            # Propagate through each hyperedge
            for edge_id, edge_data in hypergraph['hyperedges'].items():
                source_ids = edge_data['source_node_ids']
                target_ids = edge_data['target_node_ids']
                edge_type = edge_data['edge_type']
                weight = edge_data['weight']
                
                # Calculate source activation
                source_activation = sum(activations[sid] for sid in source_ids) / len(source_ids)
                
                # Apply mode-specific propagation
                if mode == ActivationMode.SPREADING:
                    propagated = self._spreading_activation(source_activation, weight)
                elif mode == ActivationMode.RESONANCE:
                    propagated = self._resonance_activation(source_activation, weight, activations, target_ids)
                elif mode == ActivationMode.ECHO:
                    propagated = self._echo_activation(source_activation, weight, iteration)
                elif mode == ActivationMode.FEEDBACK:
                    propagated = self._feedback_activation(source_activation, weight, activations, source_ids, target_ids)
                else:
                    propagated = source_activation * weight
                
                # Update target activations
                for tid in target_ids:
                    new_activations[tid] += propagated / len(target_ids)
            
            # Apply decay
            for node_id in new_activations:
                if node_id not in initial_activations:
                    new_activations[node_id] *= (1 - self.decay_rate)
            
            # Normalize to [0, 1]
            max_activation = max(new_activations.values()) if new_activations else 1.0
            if max_activation > 0:
                new_activations = {k: min(v / max_activation, 1.0) for k, v in new_activations.items()}
            
            # Check for convergence
            if self._has_converged(activations, new_activations):
                break
            
            activations = new_activations
        
        # Record activation state
        self._record_activation_state(activations, mode)
        
        return activations
    
    def _spreading_activation(self, source_activation: float, weight: float) -> float:
        """Standard spreading activation"""
        return source_activation * weight
    
    def _resonance_activation(
        self,
        source_activation: float,
        weight: float,
        current_activations: Dict[str, float],
        target_ids: List[str]
    ) -> float:
        """Resonance-based activation (synchronized)"""
        # Calculate resonance factor based on target synchronization
        target_activations = [current_activations[tid] for tid in target_ids]
        resonance = np.std(target_activations) if target_activations else 0.0
        resonance_factor = 1.0 / (1.0 + resonance)  # Higher sync = higher resonance
        
        return source_activation * weight * resonance_factor
    
    def _echo_activation(self, source_activation: float, weight: float, iteration: int) -> float:
        """Echo state activation (reservoir computing)"""
        # Time-dependent echo decay
        echo_decay = np.exp(-iteration * 0.1)
        return source_activation * weight * echo_decay
    
    def _feedback_activation(
        self,
        source_activation: float,
        weight: float,
        current_activations: Dict[str, float],
        source_ids: List[str],
        target_ids: List[str]
    ) -> float:
        """Feedback loop activation (recursive)"""
        # Calculate feedback from targets back to sources
        target_activations = [current_activations[tid] for tid in target_ids]
        avg_target = sum(target_activations) / len(target_activations) if target_activations else 0.0
        
        # Feedback modulation
        feedback_factor = 1.0 + (avg_target * 0.5)
        
        return source_activation * weight * feedback_factor
    
    def _has_converged(
        self,
        old_activations: Dict[str, float],
        new_activations: Dict[str, float],
        epsilon: float = 0.001
    ) -> bool:
        """Check if activation has converged"""
        for node_id in old_activations:
            if abs(old_activations[node_id] - new_activations[node_id]) > epsilon:
                return False
        return True
    
    def _record_activation_state(self, activations: Dict[str, float], mode: ActivationMode):
        """Record activation state for history"""
        import time
        timestamp = time.time()
        
        state = {
            node_id: ActivationState(
                node_id=node_id,
                activation_level=activation,
                timestamp=timestamp,
                mode=mode,
                source_nodes=[]
            )
            for node_id, activation in activations.items()
            if activation > self.threshold
        }
        
        self.activation_history.append(state)
    
    def get_top_activated_nodes(
        self,
        activations: Dict[str, float],
        top_k: int = 5
    ) -> List[Tuple[str, float]]:
        """Get top K activated nodes"""
        sorted_nodes = sorted(
            activations.items(),
            key=lambda x: x[1],
            reverse=True
        )
        return sorted_nodes[:top_k]
    
    def calculate_synergy_index(
        self,
        hypergraph: Dict,
        activations: Dict[str, float]
    ) -> float:
        """
        Calculate cognitive synergy index.
        
        Synergy = (Total Activation * Network Connectivity) / (Number of Nodes)
        """
        total_activation = sum(activations.values())
        num_nodes = len(hypergraph['hypernodes'])
        num_edges = len(hypergraph['hyperedges'])
        
        if num_nodes == 0:
            return 0.0
        
        connectivity = num_edges / (num_nodes * (num_nodes - 1) / 2) if num_nodes > 1 else 0.0
        synergy = (total_activation * connectivity) / num_nodes
        
        return synergy
    
    def export_activation_history(self, filepath: str):
        """Export activation history to JSON file"""
        history_data = [
            {node_id: state.to_dict() for node_id, state in snapshot.items()}
            for snapshot in self.activation_history
        ]
        
        with open(filepath, 'w') as f:
            json.dump(history_data, f, indent=2)
    
    def clear_history(self):
        """Clear activation history"""
        self.activation_history = []


def test_activation_engine():
    """Test the activation engine with sample hypergraph"""
    # Sample hypergraph
    hypergraph = {
        "hypernodes": {
            "node1": {"activation_level": 0.0},
            "node2": {"activation_level": 0.0},
            "node3": {"activation_level": 0.0},
            "node4": {"activation_level": 0.0}
        },
        "hyperedges": {
            "edge1": {
                "source_node_ids": ["node1"],
                "target_node_ids": ["node2", "node3"],
                "edge_type": "symbolic",
                "weight": 0.8
            },
            "edge2": {
                "source_node_ids": ["node2"],
                "target_node_ids": ["node4"],
                "edge_type": "causal",
                "weight": 0.6
            },
            "edge3": {
                "source_node_ids": ["node3"],
                "target_node_ids": ["node4"],
                "edge_type": "feedback",
                "weight": 0.7
            }
        }
    }
    
    # Initialize engine
    engine = ActivationEngine(decay_rate=0.1, threshold=0.01, max_iterations=10)
    
    # Test spreading activation
    initial = {"node1": 1.0}
    activations = engine.propagate_activation(hypergraph, initial, ActivationMode.SPREADING)
    
    print("Spreading Activation Results:")
    for node_id, activation in sorted(activations.items(), key=lambda x: x[1], reverse=True):
        print(f"  {node_id}: {activation:.4f}")
    
    # Calculate synergy
    synergy = engine.calculate_synergy_index(hypergraph, activations)
    print(f"\nSynergy Index: {synergy:.4f}")
    
    # Top activated nodes
    top_nodes = engine.get_top_activated_nodes(activations, top_k=3)
    print("\nTop 3 Activated Nodes:")
    for node_id, activation in top_nodes:
        print(f"  {node_id}: {activation:.4f}")


if __name__ == "__main__":
    test_activation_engine()
