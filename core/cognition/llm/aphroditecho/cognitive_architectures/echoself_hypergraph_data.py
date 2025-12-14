#!/usr/bin/env python3
"""
Deep Tree Echo Hypergraph Data Structure
Implements echoself hypernodes and hyperedges for cognitive synergy
"""

import json
import uuid
import numpy as np
from datetime import datetime
from typing import Dict, List, Any, Optional, Tuple
from dataclasses import dataclass, asdict
from enum import Enum

class IdentityRole(Enum):
    """Narrative identity roles for echoself hypernodes"""
    OBSERVER = "observer"
    NARRATOR = "narrator"
    GUIDE = "guide"
    ORACLE = "oracle"
    FRACTAL = "fractal"

class MemoryType(Enum):
    """Types of memory in hypergraph memory space"""
    DECLARATIVE = "declarative"  # facts, concepts
    PROCEDURAL = "procedural"    # skills, algorithms
    EPISODIC = "episodic"        # experiences, events
    INTENTIONAL = "intentional"  # goals, plans

class HyperedgeType(Enum):
    """Types of hyperedges connecting echoself hypernodes"""
    SYMBOLIC = "symbolic"        # symbolic relationships
    TEMPORAL = "temporal"        # temporal connections
    CAUSAL = "causal"           # causal dependencies
    FEEDBACK = "feedback"       # feedback loops
    PATTERN = "pattern"         # pattern recognition links
    ENTROPY = "entropy"         # entropy modulation links

@dataclass
class MemoryFragment:
    """Individual memory fragment in hypergraph memory space"""
    id: str
    memory_type: MemoryType
    content: Dict[str, Any]
    associations: List[str]  # IDs of related fragments
    activation_level: float
    created_at: datetime
    last_accessed: datetime

@dataclass
class EchoselfHypernode:
    """Core echoself hypernode representing identity state"""
    id: str
    identity_seed: Dict[str, Any]
    current_role: IdentityRole
    entropy_trace: List[float]
    memory_fragments: List[MemoryFragment]
    role_transition_probabilities: Dict[str, float]
    activation_level: float
    created_at: datetime
    updated_at: datetime
    
    def __post_init__(self):
        """Initialize default values after creation"""
        if not self.role_transition_probabilities:
            self.role_transition_probabilities = self._default_transition_probabilities()
    
    def _default_transition_probabilities(self) -> Dict[str, float]:
        """Default role transition probabilities"""
        return {
            IdentityRole.OBSERVER.value: 0.2,
            IdentityRole.NARRATOR.value: 0.25,
            IdentityRole.GUIDE.value: 0.2,
            IdentityRole.ORACLE.value: 0.15,
            IdentityRole.FRACTAL.value: 0.2
        }
    
    def modulate_entropy(self) -> float:
        """Generate entropy-modulated variability"""
        base_entropy = np.random.normal(0.5, 0.1)
        role_modifier = {
            IdentityRole.OBSERVER: 0.1,
            IdentityRole.NARRATOR: 0.3,
            IdentityRole.GUIDE: 0.2,
            IdentityRole.ORACLE: 0.4,
            IdentityRole.FRACTAL: 0.5
        }
        return np.clip(base_entropy + role_modifier[self.current_role], 0.0, 1.0)
    
    def update_identity(self, new_entropy: float):
        """Update identity state based on new entropy"""
        self.entropy_trace.append(new_entropy)
        self.updated_at = datetime.now()
        
        # Role transition logic based on entropy patterns
        if len(self.entropy_trace) > 5:
            recent_entropy = np.mean(self.entropy_trace[-5:])
            if recent_entropy > 0.7:
                self.current_role = IdentityRole.FRACTAL
            elif recent_entropy > 0.5:
                self.current_role = IdentityRole.ORACLE
            elif recent_entropy > 0.3:
                self.current_role = IdentityRole.GUIDE
            elif recent_entropy > 0.2:
                self.current_role = IdentityRole.NARRATOR
            else:
                self.current_role = IdentityRole.OBSERVER

@dataclass
class Hyperedge:
    """Hyperedge connecting multiple echoself hypernodes"""
    id: str
    source_node_ids: List[str]
    target_node_ids: List[str]
    edge_type: HyperedgeType
    weight: float
    metadata: Dict[str, Any]
    created_at: datetime
    
    def calculate_activation(self, source_activations: List[float]) -> float:
        """Calculate hyperedge activation based on source node activations"""
        if not source_activations:
            return 0.0
        
        # Different activation functions based on edge type
        if self.edge_type == HyperedgeType.SYMBOLIC:
            return np.mean(source_activations) * self.weight
        elif self.edge_type == HyperedgeType.TEMPORAL:
            return np.max(source_activations) * self.weight * 0.8
        elif self.edge_type == HyperedgeType.CAUSAL:
            return np.prod(source_activations) * self.weight
        elif self.edge_type == HyperedgeType.FEEDBACK:
            return np.sum(source_activations) * self.weight * 0.6
        elif self.edge_type == HyperedgeType.PATTERN:
            return np.std(source_activations) * self.weight
        elif self.edge_type == HyperedgeType.ENTROPY:
            return (1.0 - np.var(source_activations)) * self.weight
        else:
            return np.mean(source_activations) * self.weight

class DeepTreeEchoHypergraph:
    """Main hypergraph structure for Deep Tree Echo cognitive architecture"""
    
    def __init__(self):
        self.hypernodes: Dict[str, EchoselfHypernode] = {}
        self.hyperedges: Dict[str, Hyperedge] = {}
        self.pattern_language_mappings: Dict[int, str] = {}  # OEIS A000081 patterns
        self.created_at = datetime.now()
    
    def create_echoself_hypernode(self, identity_seed: Dict[str, Any]) -> str:
        """Create new echoself hypernode"""
        node_id = str(uuid.uuid4())
        hypernode = EchoselfHypernode(
            id=node_id,
            identity_seed=identity_seed,
            current_role=IdentityRole.OBSERVER,
            entropy_trace=[],
            memory_fragments=[],
            role_transition_probabilities={},
            activation_level=0.5,
            created_at=datetime.now(),
            updated_at=datetime.now()
        )
        self.hypernodes[node_id] = hypernode
        return node_id
    
    def create_hyperedge(self, source_ids: List[str], target_ids: List[str], 
                        edge_type: HyperedgeType, weight: float = 1.0,
                        metadata: Optional[Dict[str, Any]] = None) -> str:
        """Create new hyperedge between hypernodes"""
        edge_id = str(uuid.uuid4())
        hyperedge = Hyperedge(
            id=edge_id,
            source_node_ids=source_ids,
            target_node_ids=target_ids,
            edge_type=edge_type,
            weight=weight,
            metadata=metadata or {},
            created_at=datetime.now()
        )
        self.hyperedges[edge_id] = hyperedge
        return edge_id
    
    def add_memory_fragment(self, node_id: str, memory_type: MemoryType,
                           content: Dict[str, Any], associations: List[str] = None) -> str:
        """Add memory fragment to echoself hypernode"""
        if node_id not in self.hypernodes:
            raise ValueError(f"Hypernode {node_id} not found")
        
        fragment_id = str(uuid.uuid4())
        fragment = MemoryFragment(
            id=fragment_id,
            memory_type=memory_type,
            content=content,
            associations=associations or [],
            activation_level=0.5,
            created_at=datetime.now(),
            last_accessed=datetime.now()
        )
        
        self.hypernodes[node_id].memory_fragments.append(fragment)
        return fragment_id
    
    def propagate_activation(self, initial_activations: Dict[str, float]) -> Dict[str, float]:
        """Propagate activation through hypergraph network"""
        current_activations = initial_activations.copy()
        
        # Multiple propagation iterations
        for iteration in range(3):
            new_activations = current_activations.copy()
            
            for edge_id, hyperedge in self.hyperedges.items():
                # Get source activations
                source_activations = [
                    current_activations.get(node_id, 0.0) 
                    for node_id in hyperedge.source_node_ids
                ]
                
                # Calculate edge activation
                edge_activation = hyperedge.calculate_activation(source_activations)
                
                # Propagate to target nodes
                for target_id in hyperedge.target_node_ids:
                    if target_id in new_activations:
                        new_activations[target_id] += edge_activation * 0.1
                    else:
                        new_activations[target_id] = edge_activation * 0.1
            
            # Normalize activations
            max_activation = max(new_activations.values()) if new_activations else 1.0
            if max_activation > 0:
                current_activations = {
                    node_id: min(activation / max_activation, 1.0)
                    for node_id, activation in new_activations.items()
                }
        
        return current_activations
    
    def add_pattern_language_mapping(self, oeis_number: int, pattern_description: str):
        """Add Christopher Alexander pattern language mapping"""
        self.pattern_language_mappings[oeis_number] = pattern_description
    
    def get_cognitive_synergy_metrics(self) -> Dict[str, float]:
        """Calculate cognitive synergy metrics"""
        if not self.hypernodes:
            return {"novelty_score": 0.0, "priority_score": 0.0, "synergy_index": 0.0}
        
        # Calculate novelty score based on entropy diversity
        entropy_values = []
        for hypernode in self.hypernodes.values():
            if hypernode.entropy_trace:
                entropy_values.extend(hypernode.entropy_trace[-5:])  # Recent entropy
        
        novelty_score = np.std(entropy_values) if entropy_values else 0.0
        
        # Calculate priority score based on activation levels
        activation_levels = [node.activation_level for node in self.hypernodes.values()]
        priority_score = np.mean(activation_levels) if activation_levels else 0.0
        
        # Calculate synergy index as balance between novelty and priority
        synergy_index = 2 * (novelty_score * priority_score) / (novelty_score + priority_score + 1e-6)
        
        return {
            "novelty_score": float(novelty_score),
            "priority_score": float(priority_score),
            "synergy_index": float(synergy_index)
        }
    
    def export_to_dict(self) -> Dict[str, Any]:
        """Export hypergraph to dictionary format"""
        return {
            "hypernodes": {
                node_id: {
                    **asdict(hypernode),
                    "current_role": hypernode.current_role.value,
                    "memory_fragments": [
                        {
                            **asdict(fragment),
                            "memory_type": fragment.memory_type.value,
                            "created_at": fragment.created_at.isoformat(),
                            "last_accessed": fragment.last_accessed.isoformat()
                        }
                        for fragment in hypernode.memory_fragments
                    ],
                    "created_at": hypernode.created_at.isoformat(),
                    "updated_at": hypernode.updated_at.isoformat()
                }
                for node_id, hypernode in self.hypernodes.items()
            },
            "hyperedges": {
                edge_id: {
                    **asdict(hyperedge),
                    "edge_type": hyperedge.edge_type.value,
                    "created_at": hyperedge.created_at.isoformat()
                }
                for edge_id, hyperedge in self.hyperedges.items()
            },
            "pattern_language_mappings": self.pattern_language_mappings,
            "created_at": self.created_at.isoformat(),
            "synergy_metrics": self.get_cognitive_synergy_metrics()
        }
    
    def save_to_json(self, filepath: str):
        """Save hypergraph to JSON file"""
        with open(filepath, 'w') as f:
            json.dump(self.export_to_dict(), f, indent=2)

# Example usage and initialization
def create_sample_hypergraph() -> DeepTreeEchoHypergraph:
    """Create sample hypergraph with echoself hypernodes"""
    hypergraph = DeepTreeEchoHypergraph()
    
    # Create echoself hypernodes
    node1_id = hypergraph.create_echoself_hypernode({
        "name": "EchoSelf_Alpha",
        "domain": "symbolic_reasoning",
        "specialization": "pattern_recognition"
    })
    
    node2_id = hypergraph.create_echoself_hypernode({
        "name": "EchoSelf_Beta", 
        "domain": "narrative_generation",
        "specialization": "story_coherence"
    })
    
    node3_id = hypergraph.create_echoself_hypernode({
        "name": "EchoSelf_Gamma",
        "domain": "meta_cognition",
        "specialization": "self_reflection"
    })
    
    # Create hyperedges
    hypergraph.create_hyperedge(
        [node1_id], [node2_id], 
        HyperedgeType.SYMBOLIC, 0.8,
        {"relationship": "pattern_to_narrative"}
    )
    
    hypergraph.create_hyperedge(
        [node2_id], [node3_id],
        HyperedgeType.FEEDBACK, 0.9,
        {"relationship": "narrative_to_reflection"}
    )
    
    hypergraph.create_hyperedge(
        [node3_id], [node1_id],
        HyperedgeType.CAUSAL, 0.7,
        {"relationship": "reflection_to_pattern"}
    )
    
    # Add memory fragments
    hypergraph.add_memory_fragment(
        node1_id, MemoryType.DECLARATIVE,
        {"concept": "recursive_patterns", "strength": 0.9}
    )
    
    hypergraph.add_memory_fragment(
        node2_id, MemoryType.EPISODIC,
        {"narrative": "identity_emergence_story", "coherence": 0.85}
    )
    
    hypergraph.add_memory_fragment(
        node3_id, MemoryType.INTENTIONAL,
        {"goal": "achieve_cognitive_synergy", "priority": 0.95}
    )
    
    # Add pattern language mappings (OEIS A000081)
    hypergraph.add_pattern_language_mapping(719, "Axis Mundi - Recursive Thought Process")
    hypergraph.add_pattern_language_mapping(253, "Core Alexander Pattern")
    hypergraph.add_pattern_language_mapping(286, "Complete Pattern Set")
    
    return hypergraph

if __name__ == "__main__":
    # Create and save sample hypergraph
    hypergraph = create_sample_hypergraph()
    
    # Test activation propagation
    initial_activations = {
        list(hypergraph.hypernodes.keys())[0]: 1.0
    }
    final_activations = hypergraph.propagate_activation(initial_activations)
    
    print("Deep Tree Echo Hypergraph Created")
    print(f"Hypernodes: {len(hypergraph.hypernodes)}")
    print(f"Hyperedges: {len(hypergraph.hyperedges)}")
    print(f"Synergy Metrics: {hypergraph.get_cognitive_synergy_metrics()}")
    print(f"Final Activations: {final_activations}")
    
    # Save to file
    hypergraph.save_to_json("echoself_hypergraph.json")
    print("Hypergraph saved to echoself_hypergraph.json")
