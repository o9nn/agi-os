#!/usr/bin/env python3
"""
Topology Weaver Integration for Time Crystal Daemon

This module implements the topology-weaver skill integration, which takes
parallel inputs from opencog-inferno-kernel and time-crystal-neuron to
generate contextually-tagged neural network topologies.

The weaver extracts terminology from both source skills and maps them
to neural architecture components using analogy patterns.
"""

import json
import re
from dataclasses import dataclass, field
from typing import Dict, List, Optional, Any, Tuple
from enum import Enum
import logging

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger('topology_weaver')


# =============================================================================
# TERMINOLOGY EXTRACTION
# =============================================================================

@dataclass
class Term:
    """A term extracted from a source context."""
    name: str
    domain: str  # 'opencog' or 'time_crystal'
    category: str  # 'entity', 'process', 'relation', 'property'
    description: str = ""
    related_terms: List[str] = field(default_factory=list)


@dataclass
class TerminologySet:
    """A collection of extracted terms."""
    terms: Dict[str, Term] = field(default_factory=dict)
    relations: List[Tuple[str, str, str]] = field(default_factory=list)  # (term1, relation, term2)
    clusters: Dict[str, List[str]] = field(default_factory=dict)  # semantic clusters


# OpenCog-Inferno-Kernel terminology
OPENCOG_TERMS = {
    # Entities
    'atom': Term('atom', 'opencog', 'entity', 'Basic unit of knowledge representation'),
    'atomspace': Term('atomspace', 'opencog', 'entity', 'Hypergraph knowledge store'),
    'node': Term('node', 'opencog', 'entity', 'Atom representing a concept'),
    'link': Term('link', 'opencog', 'entity', 'Atom representing a relation'),
    'truth_value': Term('truth_value', 'opencog', 'property', 'Probabilistic truth assignment'),
    'attention_value': Term('attention_value', 'opencog', 'property', 'Importance/STI/LTI'),
    
    # Processes
    'pattern_matching': Term('pattern_matching', 'opencog', 'process', 'Hypergraph query'),
    'pln_inference': Term('pln_inference', 'opencog', 'process', 'Probabilistic logic reasoning'),
    'moses_learning': Term('moses_learning', 'opencog', 'process', 'Program evolution'),
    'attention_allocation': Term('attention_allocation', 'opencog', 'process', 'Resource distribution'),
    'distributed_inference': Term('distributed_inference', 'opencog', 'process', 'Cluster reasoning'),
    
    # Relations
    'inheritance': Term('inheritance', 'opencog', 'relation', 'Is-a relationship'),
    'evaluation': Term('evaluation', 'opencog', 'relation', 'Predicate application'),
    'implication': Term('implication', 'opencog', 'relation', 'Logical implication'),
    'similarity': Term('similarity', 'opencog', 'relation', 'Conceptual similarity'),
}

# Time-Crystal-Neuron terminology
TIME_CRYSTAL_TERMS = {
    # Entities
    'time_crystal': Term('time_crystal', 'time_crystal', 'entity', 'Temporal oscillator'),
    'oscillator': Term('oscillator', 'time_crystal', 'entity', 'Periodic process'),
    'hierarchy_level': Term('hierarchy_level', 'time_crystal', 'entity', 'Temporal scale'),
    'phase': Term('phase', 'time_crystal', 'property', 'Oscillator state 0-1'),
    
    # Specific levels (from Nanobrain Fig 7.15)
    'quantum_resonance': Term('quantum_resonance', 'time_crystal', 'entity', 'Level 0: 1μs'),
    'protein_dynamics': Term('protein_dynamics', 'time_crystal', 'entity', 'Level 1: 8ms'),
    'ion_channel_gating': Term('ion_channel_gating', 'time_crystal', 'entity', 'Level 2: 26ms'),
    'membrane_dynamics': Term('membrane_dynamics', 'time_crystal', 'entity', 'Level 3: 52ms'),
    'axon_initial_segment': Term('axon_initial_segment', 'time_crystal', 'entity', 'Level 4: 110ms'),
    'dendritic_integration': Term('dendritic_integration', 'time_crystal', 'entity', 'Level 5: 160ms'),
    'synaptic_plasticity': Term('synaptic_plasticity', 'time_crystal', 'entity', 'Level 6: 250ms'),
    'soma_processing': Term('soma_processing', 'time_crystal', 'entity', 'Level 7: 330ms'),
    'network_synchronization': Term('network_synchronization', 'time_crystal', 'entity', 'Level 8: 500ms'),
    'global_rhythm': Term('global_rhythm', 'time_crystal', 'entity', 'Level 9: 1s'),
    'circadian_modulation': Term('circadian_modulation', 'time_crystal', 'entity', 'Level 10: 1min'),
    'homeostatic_regulation': Term('homeostatic_regulation', 'time_crystal', 'entity', 'Level 11: 1hr'),
    
    # Processes
    'phase_coupling': Term('phase_coupling', 'time_crystal', 'process', 'Oscillator synchronization'),
    'nested_periodicity': Term('nested_periodicity', 'time_crystal', 'process', 'Hierarchical timing'),
    'temporal_integration': Term('temporal_integration', 'time_crystal', 'process', 'Multi-scale fusion'),
}


def extract_terminology() -> TerminologySet:
    """
    Extract and merge terminology from both source skills.
    """
    term_set = TerminologySet()
    
    # Add OpenCog terms
    for name, term in OPENCOG_TERMS.items():
        term_set.terms[name] = term
    
    # Add Time Crystal terms
    for name, term in TIME_CRYSTAL_TERMS.items():
        term_set.terms[name] = term
    
    # Define cross-domain relations
    term_set.relations = [
        ('atom', 'operates_at', 'hierarchy_level'),
        ('attention_allocation', 'modulated_by', 'global_rhythm'),
        ('pln_inference', 'operates_at', 'dendritic_integration'),
        ('pattern_matching', 'operates_at', 'synaptic_plasticity'),
        ('moses_learning', 'operates_at', 'circadian_modulation'),
        ('distributed_inference', 'synchronized_by', 'network_synchronization'),
    ]
    
    # Define semantic clusters
    term_set.clusters = {
        'knowledge_representation': ['atom', 'atomspace', 'node', 'link', 'truth_value'],
        'cognitive_processes': ['pattern_matching', 'pln_inference', 'moses_learning', 'attention_allocation'],
        'temporal_structure': ['time_crystal', 'oscillator', 'phase', 'hierarchy_level'],
        'fast_processing': ['quantum_resonance', 'protein_dynamics', 'ion_channel_gating'],
        'medium_processing': ['membrane_dynamics', 'axon_initial_segment', 'dendritic_integration'],
        'slow_processing': ['soma_processing', 'network_synchronization', 'global_rhythm'],
        'adaptation': ['synaptic_plasticity', 'circadian_modulation', 'homeostatic_regulation'],
    }
    
    return term_set


# =============================================================================
# ANALOGY MAPPING
# =============================================================================

@dataclass
class AnalogyMapping:
    """A mapping from source term to neural component."""
    source_term: str
    component_type: str  # 'linear', 'attention', 'activation', 'norm', etc.
    tag: str
    rationale: str
    params: Dict[str, Any] = field(default_factory=dict)
    tc_level: Optional[int] = None


# Analogy patterns for OpenCog concepts
OPENCOG_ANALOGIES = {
    'atom': AnalogyMapping(
        'atom', 'embedding', 'discrete_feature',
        'Atoms are discrete, localized knowledge units',
        {'embedding_dim': 64}
    ),
    'atomspace': AnalogyMapping(
        'atomspace', 'attention', 'hypergraph_attention',
        'AtomSpace is a hypergraph with multi-way relations',
        {'num_heads': 8, 'attention_type': 'hypergraph'}
    ),
    'pattern_matching': AnalogyMapping(
        'pattern_matching', 'attention', 'pattern_matcher',
        'Pattern matching finds subgraph isomorphisms',
        {'num_heads': 4, 'attention_type': 'cross'},
        tc_level=6  # Synaptic plasticity
    ),
    'pln_inference': AnalogyMapping(
        'pln_inference', 'linear', 'probabilistic_reasoner',
        'PLN performs probabilistic logical inference',
        {'hidden_dim': 256, 'activation': 'softmax'},
        tc_level=5  # Dendritic integration
    ),
    'moses_learning': AnalogyMapping(
        'moses_learning', 'linear', 'program_evolver',
        'MOSES evolves programs via genetic search',
        {'hidden_dim': 512},
        tc_level=10  # Circadian modulation
    ),
    'attention_allocation': AnalogyMapping(
        'attention_allocation', 'attention', 'resource_allocator',
        'Attention allocation distributes cognitive resources',
        {'num_heads': 2, 'attention_type': 'self'},
        tc_level=9  # Global rhythm
    ),
    'truth_value': AnalogyMapping(
        'truth_value', 'linear', 'truth_encoder',
        'Truth values encode probabilistic confidence',
        {'output_dim': 2}  # strength, confidence
    ),
    'attention_value': AnalogyMapping(
        'attention_value', 'linear', 'importance_encoder',
        'Attention values encode importance/salience',
        {'output_dim': 3}  # STI, LTI, VLTI
    ),
}

# Analogy patterns for Time Crystal concepts
TIME_CRYSTAL_ANALOGIES = {
    'time_crystal': AnalogyMapping(
        'time_crystal', 'norm', 'temporal_oscillator',
        'Time crystals provide periodic normalization',
        {'norm_type': 'layer', 'periodic': True}
    ),
    'phase_coupling': AnalogyMapping(
        'phase_coupling', 'attention', 'phase_synchronizer',
        'Phase coupling synchronizes oscillators',
        {'num_heads': 2, 'attention_type': 'cross'}
    ),
    'nested_periodicity': AnalogyMapping(
        'nested_periodicity', 'linear', 'hierarchical_timer',
        'Nested periodicity creates multi-scale timing',
        {'hidden_dim': 128}
    ),
    # Level-specific mappings
    'quantum_resonance': AnalogyMapping(
        'quantum_resonance', 'activation', 'level_0_gate',
        'Fastest processing gate', {'activation': 'gelu'}, tc_level=0
    ),
    'protein_dynamics': AnalogyMapping(
        'protein_dynamics', 'linear', 'level_1_processor',
        'Protein-scale processing', {'hidden_dim': 32}, tc_level=1
    ),
    'dendritic_integration': AnalogyMapping(
        'dendritic_integration', 'attention', 'level_5_integrator',
        'Dendritic integration via attention', {'num_heads': 4}, tc_level=5
    ),
    'soma_processing': AnalogyMapping(
        'soma_processing', 'linear', 'level_7_processor',
        'Soma-level processing', {'hidden_dim': 256}, tc_level=7
    ),
    'network_synchronization': AnalogyMapping(
        'network_synchronization', 'attention', 'level_8_sync',
        'Network-wide synchronization', {'num_heads': 8}, tc_level=8
    ),
    'global_rhythm': AnalogyMapping(
        'global_rhythm', 'norm', 'level_9_rhythm',
        'Global rhythmic normalization', {'norm_type': 'layer'}, tc_level=9
    ),
    'circadian_modulation': AnalogyMapping(
        'circadian_modulation', 'linear', 'level_10_modulator',
        'Slow circadian modulation', {'hidden_dim': 64}, tc_level=10
    ),
    'homeostatic_regulation': AnalogyMapping(
        'homeostatic_regulation', 'linear', 'level_11_regulator',
        'Slowest homeostatic regulation', {'hidden_dim': 32}, tc_level=11
    ),
}


def get_analogy_mapping(term_name: str) -> Optional[AnalogyMapping]:
    """Get the analogy mapping for a term."""
    if term_name in OPENCOG_ANALOGIES:
        return OPENCOG_ANALOGIES[term_name]
    if term_name in TIME_CRYSTAL_ANALOGIES:
        return TIME_CRYSTAL_ANALOGIES[term_name]
    return None


# =============================================================================
# TOPOLOGY GENERATION
# =============================================================================

@dataclass
class TopologyComponent:
    """A component in the generated topology."""
    id: str
    type: str
    tags: List[str]
    params: Dict[str, Any]
    inputs: List[str]
    outputs: List[str]
    tc_level: Optional[int]


@dataclass
class GeneratedTopology:
    """A generated neural network topology."""
    name: str
    components: Dict[str, TopologyComponent]
    connections: List[Tuple[str, str]]
    metadata: Dict[str, Any]


class TopologyWeaver:
    """
    Weaves neural network topologies from extracted terminology.
    """

    def __init__(self):
        self.terminology = extract_terminology()
        self.component_counter = 0

    def weave(self, name: str = "woven_topology") -> GeneratedTopology:
        """
        Generate a complete topology from the merged terminology.
        """
        logger.info(f"Weaving topology '{name}' from {len(self.terminology.terms)} terms")
        
        components = {}
        connections = []
        
        # Create input embedding layer
        input_comp = self._create_component('input', 'embedding', ['input', 'discrete_feature'])
        components[input_comp.id] = input_comp
        
        # Create components for each semantic cluster
        cluster_outputs = {}
        for cluster_name, term_names in self.terminology.clusters.items():
            cluster_comp = self._weave_cluster(cluster_name, term_names)
            components.update(cluster_comp['components'])
            connections.extend(cluster_comp['connections'])
            cluster_outputs[cluster_name] = cluster_comp['output_id']
            
            # Connect input to cluster
            connections.append((input_comp.id, cluster_comp['input_id']))
        
        # Create cross-cluster attention (hypergraph attention)
        cross_attn = self._create_component(
            'cross_cluster_attention', 'attention',
            ['hypergraph_attention', 'cross_cluster'],
            params={'num_heads': 8, 'attention_type': 'cross'},
            tc_level=8
        )
        components[cross_attn.id] = cross_attn
        
        # Connect all cluster outputs to cross attention
        for cluster_name, output_id in cluster_outputs.items():
            connections.append((output_id, cross_attn.id))
        
        # Create output layer
        output_comp = self._create_component('output', 'linear', ['output', 'projection'])
        components[output_comp.id] = output_comp
        connections.append((cross_attn.id, output_comp.id))
        
        # Add metadata
        metadata = {
            'source_skills': ['opencog-inferno-kernel', 'time-crystal-neuron'],
            'term_count': len(self.terminology.terms),
            'cluster_count': len(self.terminology.clusters),
            'weaver_version': '1.0.0'
        }
        
        return GeneratedTopology(
            name=name,
            components=components,
            connections=connections,
            metadata=metadata
        )

    def _weave_cluster(self, cluster_name: str, term_names: List[str]) -> Dict:
        """Weave a sub-topology for a semantic cluster."""
        components = {}
        connections = []
        
        # Create cluster input
        cluster_input = self._create_component(
            f'{cluster_name}_input', 'linear',
            [cluster_name, 'cluster_input'],
            params={'hidden_dim': 128}
        )
        components[cluster_input.id] = cluster_input
        
        prev_id = cluster_input.id
        
        # Create component for each term in cluster
        for term_name in term_names:
            mapping = get_analogy_mapping(term_name)
            if mapping:
                comp = self._create_component(
                    f'{cluster_name}_{term_name}',
                    mapping.component_type,
                    [mapping.tag, cluster_name, term_name],
                    params=mapping.params,
                    tc_level=mapping.tc_level
                )
                components[comp.id] = comp
                connections.append((prev_id, comp.id))
                prev_id = comp.id
        
        # Create cluster output
        cluster_output = self._create_component(
            f'{cluster_name}_output', 'linear',
            [cluster_name, 'cluster_output'],
            params={'hidden_dim': 128}
        )
        components[cluster_output.id] = cluster_output
        connections.append((prev_id, cluster_output.id))
        
        return {
            'components': components,
            'connections': connections,
            'input_id': cluster_input.id,
            'output_id': cluster_output.id
        }

    def _create_component(self, name: str, comp_type: str, tags: List[str],
                          params: Dict[str, Any] = None,
                          tc_level: Optional[int] = None) -> TopologyComponent:
        """Create a topology component."""
        self.component_counter += 1
        comp_id = f"{name}_{self.component_counter}"
        
        return TopologyComponent(
            id=comp_id,
            type=comp_type,
            tags=tags,
            params=params or {},
            inputs=[],
            outputs=[],
            tc_level=tc_level
        )


# =============================================================================
# EXPORT FUNCTIONS
# =============================================================================

def topology_to_dict(topology: GeneratedTopology) -> Dict:
    """Convert topology to dictionary for serialization."""
    return {
        'name': topology.name,
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


def save_topology_yaml(topology: GeneratedTopology, filepath: str) -> None:
    """Save topology to YAML file."""
    import yaml
    
    data = topology_to_dict(topology)
    with open(filepath, 'w') as f:
        yaml.dump(data, f, default_flow_style=False)
    
    logger.info(f"Saved topology to {filepath}")


def save_topology_json(topology: GeneratedTopology, filepath: str) -> None:
    """Save topology to JSON file."""
    data = topology_to_dict(topology)
    with open(filepath, 'w') as f:
        json.dump(data, f, indent=2)
    
    logger.info(f"Saved topology to {filepath}")


# =============================================================================
# MAIN
# =============================================================================

if __name__ == "__main__":
    import argparse
    
    parser = argparse.ArgumentParser(description="Topology Weaver")
    parser.add_argument("--output", "-o", help="Output file path")
    parser.add_argument("--format", "-f", choices=['yaml', 'json'], default='yaml',
                       help="Output format")
    parser.add_argument("--name", "-n", default="woven_cognitive_topology",
                       help="Topology name")
    args = parser.parse_args()
    
    weaver = TopologyWeaver()
    topology = weaver.weave(name=args.name)
    
    print(f"Generated topology: {topology.name}")
    print(f"  Components: {len(topology.components)}")
    print(f"  Connections: {len(topology.connections)}")
    print(f"  Clusters: {len(topology.metadata.get('cluster_count', 0))}")
    
    if args.output:
        if args.format == 'yaml':
            save_topology_yaml(topology, args.output)
        else:
            save_topology_json(topology, args.output)
