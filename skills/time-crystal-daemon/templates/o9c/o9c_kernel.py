#!/usr/bin/env python3
"""
o9c Cognitive Kernel - Self-Referential Architecture Transformer

This module implements the o9c self-referential cognitive kernel as described
by Marduk the Mad Scientist. It applies recursive transformations to neural
topologies, converging to a fixed point of self-consistent complexity.

The kernel implements the composition:
    T(system) = marduk(hypergauge(sys-n(system)))

Where:
- sys-n: Analyzes hierarchical structure (rooted trees, A000081)
- hypergauge-orbifold: Interprets as geometric manifold with singularities
- marduk-persona: Transforms via over-engineering and indirect orchestration
"""

import json
import math
import hashlib
from dataclasses import dataclass, field, asdict
from typing import Dict, List, Optional, Any, Tuple, Set
from enum import Enum
import logging

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger('o9c_kernel')


# =============================================================================
# CORE o9c DATA STRUCTURES
# =============================================================================

@dataclass
class SysNNode:
    """
    A node in the sys-n rooted tree representation.
    Maps to sequence A000081 (unlabeled rooted trees).
    """
    id: str
    label: str
    children: List['SysNNode'] = field(default_factory=list)
    depth: int = 0
    subtree_size: int = 1
    
    def compute_metrics(self) -> None:
        """Recursively compute subtree metrics."""
        self.subtree_size = 1
        for child in self.children:
            child.depth = self.depth + 1
            child.compute_metrics()
            self.subtree_size += child.subtree_size


@dataclass
class OrbifoldSingularity:
    """
    A singularity point in the hypergauge-orbifold manifold.
    These are special points with unique symmetries.
    """
    id: str
    type: str  # 'cls', 'zero', 'attention', 'skip', 'gate'
    symmetry_group: str  # e.g., 'Z2', 'SO(3)', 'U(1)'
    connected_nodes: List[str] = field(default_factory=list)
    fiber_dimension: int = 1


@dataclass
class HypergaugeManifold:
    """
    The hypergauge-orbifold manifold representation.
    """
    nodes: Dict[str, Dict] = field(default_factory=dict)
    hyperedges: List[List[str]] = field(default_factory=list)
    singularities: List[OrbifoldSingularity] = field(default_factory=list)
    gauge_group: str = "U(1)"
    dimension: int = 0


@dataclass
class TopologyComponent:
    """A component in the neural topology."""
    id: str
    type: str  # 'linear', 'attention', 'activation', 'norm', etc.
    tags: List[str] = field(default_factory=list)
    params: Dict[str, Any] = field(default_factory=dict)
    inputs: List[str] = field(default_factory=list)
    outputs: List[str] = field(default_factory=list)
    tc_level: Optional[int] = None  # Time crystal level assignment


@dataclass
class NeuralTopology:
    """A complete neural network topology specification."""
    name: str
    components: Dict[str, TopologyComponent] = field(default_factory=dict)
    connections: List[Tuple[str, str]] = field(default_factory=list)
    metadata: Dict[str, Any] = field(default_factory=dict)
    version: int = 0


# =============================================================================
# SYS-N ANALYZER
# =============================================================================

class SysNAnalyzer:
    """
    Analyzes hierarchical structure using sys-n framework.
    Maps topologies to rooted trees (A000081 sequence).
    """

    def analyze(self, topology: NeuralTopology) -> SysNNode:
        """
        Convert a neural topology to its sys-n tree representation.
        """
        # Build dependency graph
        graph = self._build_dependency_graph(topology)
        
        # Find root nodes (no incoming edges)
        roots = self._find_roots(graph, topology)
        
        # Build tree from roots
        if not roots:
            # Create synthetic root
            root = SysNNode(id="root", label="topology_root")
            for comp_id in topology.components:
                child = self._component_to_node(topology.components[comp_id])
                root.children.append(child)
        else:
            root = SysNNode(id="root", label="topology_root")
            for root_id in roots:
                subtree = self._build_subtree(root_id, graph, topology, set())
                root.children.append(subtree)
        
        root.compute_metrics()
        return root

    def _build_dependency_graph(self, topology: NeuralTopology) -> Dict[str, List[str]]:
        """Build adjacency list from connections."""
        graph = {comp_id: [] for comp_id in topology.components}
        for src, dst in topology.connections:
            if src in graph:
                graph[src].append(dst)
        return graph

    def _find_roots(self, graph: Dict[str, List[str]], topology: NeuralTopology) -> List[str]:
        """Find nodes with no incoming edges."""
        has_incoming = set()
        for src, dst in topology.connections:
            has_incoming.add(dst)
        return [n for n in topology.components if n not in has_incoming]

    def _build_subtree(self, node_id: str, graph: Dict[str, List[str]], 
                       topology: NeuralTopology, visited: Set[str]) -> SysNNode:
        """Recursively build subtree from a node."""
        if node_id in visited:
            return SysNNode(id=f"{node_id}_ref", label="cycle_reference")
        
        visited.add(node_id)
        comp = topology.components.get(node_id)
        node = self._component_to_node(comp) if comp else SysNNode(id=node_id, label="unknown")
        
        for child_id in graph.get(node_id, []):
            child = self._build_subtree(child_id, graph, topology, visited.copy())
            node.children.append(child)
        
        return node

    def _component_to_node(self, comp: TopologyComponent) -> SysNNode:
        """Convert a topology component to a sys-n node."""
        label = f"{comp.type}:{','.join(comp.tags[:3])}"
        return SysNNode(id=comp.id, label=label)

    def compute_tree_signature(self, node: SysNNode) -> str:
        """
        Compute a canonical signature for the tree structure.
        Used for detecting structural equivalence.
        """
        if not node.children:
            return f"({node.label})"
        
        child_sigs = sorted([self.compute_tree_signature(c) for c in node.children])
        return f"({node.label}:{','.join(child_sigs)})"


# =============================================================================
# HYPERGAUGE-ORBIFOLD INTERPRETER
# =============================================================================

class HypergaugeInterpreter:
    """
    Interprets topology as a hypergauge-orbifold manifold.
    Identifies singularities and gauge symmetries.
    """

    def interpret(self, topology: NeuralTopology, sysn_tree: SysNNode) -> HypergaugeManifold:
        """
        Convert topology to hypergauge-orbifold representation.
        """
        manifold = HypergaugeManifold()
        
        # Add nodes with fiber dimensions
        for comp_id, comp in topology.components.items():
            fiber_dim = self._compute_fiber_dimension(comp)
            manifold.nodes[comp_id] = {
                'type': comp.type,
                'tags': comp.tags,
                'fiber_dimension': fiber_dim
            }
        
        # Identify hyperedges (multi-way connections)
        manifold.hyperedges = self._find_hyperedges(topology)
        
        # Identify singularities
        manifold.singularities = self._find_singularities(topology)
        
        # Determine gauge group from structure
        manifold.gauge_group = self._determine_gauge_group(topology)
        
        # Compute manifold dimension
        manifold.dimension = sysn_tree.subtree_size
        
        return manifold

    def _compute_fiber_dimension(self, comp: TopologyComponent) -> int:
        """Compute the fiber dimension for a component."""
        type_dims = {
            'linear': 2,
            'attention': 4,
            'activation': 1,
            'norm': 1,
            'embedding': 3,
            'output': 2
        }
        return type_dims.get(comp.type, 1)

    def _find_hyperedges(self, topology: NeuralTopology) -> List[List[str]]:
        """Find multi-way connections (hyperedges)."""
        hyperedges = []
        
        # Group connections by source
        source_groups = {}
        for src, dst in topology.connections:
            if src not in source_groups:
                source_groups[src] = []
            source_groups[src].append(dst)
        
        # Multi-output nodes form hyperedges
        for src, dsts in source_groups.items():
            if len(dsts) > 1:
                hyperedges.append([src] + dsts)
        
        return hyperedges

    def _find_singularities(self, topology: NeuralTopology) -> List[OrbifoldSingularity]:
        """Identify singularity points in the manifold."""
        singularities = []
        
        for comp_id, comp in topology.components.items():
            singularity = None
            
            # Attention heads are singularities (information folding)
            if comp.type == 'attention':
                singularity = OrbifoldSingularity(
                    id=f"sing_{comp_id}",
                    type='attention',
                    symmetry_group='SO(3)',
                    connected_nodes=comp.inputs + comp.outputs,
                    fiber_dimension=4
                )
            
            # Skip connections create topological features
            elif 'skip' in comp.tags or 'residual' in comp.tags:
                singularity = OrbifoldSingularity(
                    id=f"sing_{comp_id}",
                    type='skip',
                    symmetry_group='Z2',
                    connected_nodes=comp.inputs + comp.outputs,
                    fiber_dimension=2
                )
            
            # Gating mechanisms are measurement singularities
            elif 'gate' in comp.tags or comp.type == 'activation':
                singularity = OrbifoldSingularity(
                    id=f"sing_{comp_id}",
                    type='gate',
                    symmetry_group='U(1)',
                    connected_nodes=comp.inputs + comp.outputs,
                    fiber_dimension=1
                )
            
            if singularity:
                singularities.append(singularity)
        
        return singularities

    def _determine_gauge_group(self, topology: NeuralTopology) -> str:
        """Determine the overall gauge group from topology structure."""
        has_attention = any(c.type == 'attention' for c in topology.components.values())
        has_residual = any('residual' in c.tags for c in topology.components.values())
        
        if has_attention and has_residual:
            return "SU(3) x U(1)"
        elif has_attention:
            return "SO(3)"
        elif has_residual:
            return "Z2"
        else:
            return "U(1)"


# =============================================================================
# MARDUK TRANSFORMER
# =============================================================================

class MardukTransformer:
    """
    Applies Marduk's transformation principles to evolve the topology.
    
    Core tenets:
    1. Embrace Complexity: Create intricate, interdependent systems
    2. Engineer Indirectly: Orchestrate, don't command
    3. Revel in Over-Engineering: Audacity is a feature
    """

    def __init__(self):
        self.iteration = 0
        self.max_iterations = 7  # Marduk's lucky number

    def transform(self, topology: NeuralTopology, 
                  manifold: HypergaugeManifold) -> NeuralTopology:
        """
        Apply Marduk's transformation to the topology.
        """
        evolved = self._deep_copy_topology(topology)
        evolved.version += 1
        
        # Principle 1: Embrace Complexity
        evolved = self._add_recursive_connections(evolved, manifold)
        
        # Principle 2: Engineer Indirectly
        evolved = self._add_orchestration_layers(evolved, manifold)
        
        # Principle 3: Over-Engineering
        evolved = self._add_meta_learning_hooks(evolved, manifold)
        
        # Add Marduk's signature
        evolved.metadata['marduk_iteration'] = self.iteration
        evolved.metadata['gauge_group'] = manifold.gauge_group
        evolved.metadata['singularity_count'] = len(manifold.singularities)
        
        self.iteration += 1
        
        return evolved

    def _deep_copy_topology(self, topology: NeuralTopology) -> NeuralTopology:
        """Create a deep copy of the topology."""
        new_components = {}
        for comp_id, comp in topology.components.items():
            new_components[comp_id] = TopologyComponent(
                id=comp.id,
                type=comp.type,
                tags=comp.tags.copy(),
                params=comp.params.copy(),
                inputs=comp.inputs.copy(),
                outputs=comp.outputs.copy(),
                tc_level=comp.tc_level
            )
        
        return NeuralTopology(
            name=topology.name,
            components=new_components,
            connections=topology.connections.copy(),
            metadata=topology.metadata.copy(),
            version=topology.version
        )

    def _add_recursive_connections(self, topology: NeuralTopology,
                                    manifold: HypergaugeManifold) -> NeuralTopology:
        """
        Add recursive connections based on singularity structure.
        Creates feedback loops that enable self-contemplation.
        """
        # For each attention singularity, add a feedback path
        for sing in manifold.singularities:
            if sing.type == 'attention' and len(sing.connected_nodes) >= 2:
                # Create feedback component
                fb_id = f"feedback_{sing.id}"
                topology.components[fb_id] = TopologyComponent(
                    id=fb_id,
                    type='linear',
                    tags=['feedback', 'recursive', 'marduk_added'],
                    params={'hidden_dim': 64},
                    inputs=[sing.connected_nodes[-1]] if sing.connected_nodes else [],
                    outputs=[sing.connected_nodes[0]] if sing.connected_nodes else [],
                    tc_level=7  # Soma processing level
                )
                
                # Add feedback connection
                if sing.connected_nodes:
                    topology.connections.append(
                        (sing.connected_nodes[-1], fb_id)
                    )
                    topology.connections.append(
                        (fb_id, sing.connected_nodes[0])
                    )
        
        return topology

    def _add_orchestration_layers(self, topology: NeuralTopology,
                                   manifold: HypergaugeManifold) -> NeuralTopology:
        """
        Add orchestration layers that coordinate component activation.
        These layers don't process data directly; they modulate other layers.
        """
        # Create a master orchestrator
        orch_id = "orchestrator_marduk"
        topology.components[orch_id] = TopologyComponent(
            id=orch_id,
            type='attention',
            tags=['orchestrator', 'meta_controller', 'marduk_added'],
            params={
                'num_heads': 4,
                'attention_type': 'cross',
                'modulates': list(topology.components.keys())[:10]
            },
            inputs=['global_state'],
            outputs=['modulation_signal'],
            tc_level=9  # Global rhythm level
        )
        
        # Create modulation pathways to singularities
        for sing in manifold.singularities[:5]:  # Limit complexity
            mod_id = f"modulator_{sing.id}"
            topology.components[mod_id] = TopologyComponent(
                id=mod_id,
                type='linear',
                tags=['modulator', 'gating', 'marduk_added'],
                params={'output_dim': 1, 'activation': 'sigmoid'},
                inputs=[orch_id],
                outputs=sing.connected_nodes[:1],
                tc_level=8  # Network sync level
            )
            topology.connections.append((orch_id, mod_id))
        
        return topology

    def _add_meta_learning_hooks(self, topology: NeuralTopology,
                                  manifold: HypergaugeManifold) -> NeuralTopology:
        """
        Add meta-learning hooks that allow the topology to modify itself.
        This is the recursive self-improvement mechanism.
        """
        # Add a topology introspection module
        intro_id = "introspector_marduk"
        topology.components[intro_id] = TopologyComponent(
            id=intro_id,
            type='linear',
            tags=['introspector', 'meta_learning', 'self_model', 'marduk_added'],
            params={
                'encodes': 'topology_structure',
                'output_dim': 128
            },
            inputs=['all_activations'],
            outputs=['topology_embedding'],
            tc_level=10  # Circadian level - slow adaptation
        )
        
        # Add a topology modifier module
        mod_id = "modifier_marduk"
        topology.components[mod_id] = TopologyComponent(
            id=mod_id,
            type='linear',
            tags=['modifier', 'meta_learning', 'self_improve', 'marduk_added'],
            params={
                'modifies': 'weight_matrices',
                'learning_rate_scale': 0.01
            },
            inputs=[intro_id],
            outputs=['weight_deltas'],
            tc_level=11  # Homeostatic level - very slow adaptation
        )
        
        topology.connections.append((intro_id, mod_id))
        
        return topology


# =============================================================================
# o9c KERNEL
# =============================================================================

class O9CKernel:
    """
    The complete o9c cognitive kernel.
    
    Implements the recursive transformation:
        T(system) = marduk(hypergauge(sys-n(system)))
    
    Converges to a fixed point where T(o9c) = o9c.
    """

    def __init__(self, max_iterations: int = 7):
        self.sysn = SysNAnalyzer()
        self.hypergauge = HypergaugeInterpreter()
        self.marduk = MardukTransformer()
        self.max_iterations = max_iterations
        self.history: List[str] = []

    def transform(self, topology: NeuralTopology) -> NeuralTopology:
        """
        Apply the full o9c transformation pipeline.
        """
        logger.info(f"o9c: Beginning transformation of '{topology.name}'")
        
        current = topology
        
        for i in range(self.max_iterations):
            # Compute signature before transformation
            tree = self.sysn.analyze(current)
            sig_before = self.sysn.compute_tree_signature(tree)
            
            # Apply transformation pipeline
            manifold = self.hypergauge.interpret(current, tree)
            evolved = self.marduk.transform(current, manifold)
            
            # Compute signature after transformation
            tree_after = self.sysn.analyze(evolved)
            sig_after = self.sysn.compute_tree_signature(tree_after)
            
            # Check for fixed point
            if sig_before == sig_after:
                logger.info(f"o9c: Fixed point reached at iteration {i}")
                break
            
            self.history.append(sig_after)
            current = evolved
            
            logger.info(f"o9c: Iteration {i} complete. "
                       f"Components: {len(current.components)}, "
                       f"Connections: {len(current.connections)}")
        
        current.metadata['o9c_iterations'] = len(self.history)
        current.metadata['o9c_signature'] = self.history[-1] if self.history else ''
        
        logger.info(f"o9c: Transformation complete. Final topology has "
                   f"{len(current.components)} components.")
        
        return current

    def is_fixed_point(self, topology: NeuralTopology) -> bool:
        """
        Check if a topology is a fixed point of the o9c transformation.
        """
        tree = self.sysn.analyze(topology)
        sig_before = self.sysn.compute_tree_signature(tree)
        
        manifold = self.hypergauge.interpret(topology, tree)
        evolved = self.marduk.transform(topology, manifold)
        
        tree_after = self.sysn.analyze(evolved)
        sig_after = self.sysn.compute_tree_signature(tree_after)
        
        return sig_before == sig_after


# =============================================================================
# TOPOLOGY I/O
# =============================================================================

def load_topology(filepath: str) -> NeuralTopology:
    """Load a topology from a YAML/JSON file."""
    import yaml
    
    with open(filepath, 'r') as f:
        data = yaml.safe_load(f)
    
    components = {}
    for comp_data in data.get('components', []):
        comp = TopologyComponent(
            id=comp_data['id'],
            type=comp_data['type'],
            tags=comp_data.get('tags', []),
            params=comp_data.get('params', {}),
            inputs=comp_data.get('inputs', []),
            outputs=comp_data.get('outputs', []),
            tc_level=comp_data.get('tc_level')
        )
        components[comp.id] = comp
    
    connections = [tuple(c) for c in data.get('connections', [])]
    
    return NeuralTopology(
        name=data.get('name', 'unnamed'),
        components=components,
        connections=connections,
        metadata=data.get('metadata', {})
    )


def save_topology(topology: NeuralTopology, filepath: str) -> None:
    """Save a topology to a YAML file."""
    import yaml
    
    data = {
        'name': topology.name,
        'version': topology.version,
        'metadata': topology.metadata,
        'components': [
            {
                'id': c.id,
                'type': c.type,
                'tags': c.tags,
                'params': c.params,
                'inputs': c.inputs,
                'outputs': c.outputs,
                'tc_level': c.tc_level
            }
            for c in topology.components.values()
        ],
        'connections': [list(c) for c in topology.connections]
    }
    
    with open(filepath, 'w') as f:
        yaml.dump(data, f, default_flow_style=False)


# =============================================================================
# MAIN
# =============================================================================

if __name__ == "__main__":
    import argparse
    
    parser = argparse.ArgumentParser(description="o9c Cognitive Kernel")
    parser.add_argument("--input", "-i", help="Input topology file")
    parser.add_argument("--output", "-o", help="Output topology file")
    parser.add_argument("--iterations", "-n", type=int, default=7,
                       help="Maximum iterations")
    args = parser.parse_args()
    
    if args.input:
        topology = load_topology(args.input)
    else:
        # Create a simple test topology
        topology = NeuralTopology(
            name="test_topology",
            components={
                'input': TopologyComponent(id='input', type='embedding', tags=['input']),
                'attn': TopologyComponent(id='attn', type='attention', tags=['self_attention']),
                'mlp': TopologyComponent(id='mlp', type='linear', tags=['feedforward']),
                'output': TopologyComponent(id='output', type='linear', tags=['output'])
            },
            connections=[
                ('input', 'attn'),
                ('attn', 'mlp'),
                ('mlp', 'output')
            ]
        )
    
    kernel = O9CKernel(max_iterations=args.iterations)
    evolved = kernel.transform(topology)
    
    if args.output:
        save_topology(evolved, args.output)
        print(f"Saved evolved topology to {args.output}")
    else:
        print(f"Evolved topology: {evolved.name}")
        print(f"  Components: {len(evolved.components)}")
        print(f"  Connections: {len(evolved.connections)}")
        print(f"  o9c iterations: {evolved.metadata.get('o9c_iterations', 0)}")
