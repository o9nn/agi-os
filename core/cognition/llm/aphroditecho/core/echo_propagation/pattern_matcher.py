"""
Pattern Matcher - Echo Propagation Pattern Recognition
Part of Deep Tree Echo Core

This module implements pattern recognition and matching algorithms for the hypergraph.
Patterns are identified based on structural similarity, activation patterns, and semantic content.
"""

import numpy as np
from typing import Dict, List, Set, Tuple, Optional
from dataclasses import dataclass
from collections import defaultdict
import json


@dataclass
class Pattern:
    """Represents a recognized pattern in the hypergraph"""
    pattern_id: str
    node_ids: List[str]
    edge_ids: List[str]
    pattern_type: str
    confidence: float
    metadata: Dict
    
    def to_dict(self) -> Dict:
        return {
            "pattern_id": self.pattern_id,
            "node_ids": self.node_ids,
            "edge_ids": self.edge_ids,
            "pattern_type": self.pattern_type,
            "confidence": self.confidence,
            "metadata": self.metadata
        }


class PatternMatcher:
    """
    Pattern recognition and matching for hypergraph structures.
    
    Supports multiple pattern types:
    - Structural patterns (graph motifs)
    - Activation patterns (temporal dynamics)
    - Semantic patterns (content similarity)
    - OEIS patterns (mathematical sequences)
    """
    
    def __init__(self, similarity_threshold: float = 0.7):
        """
        Initialize the pattern matcher.
        
        Args:
            similarity_threshold: Minimum similarity for pattern matching (0-1)
        """
        self.similarity_threshold = similarity_threshold
        self.recognized_patterns: List[Pattern] = []
        self.pattern_library: Dict[str, Pattern] = {}
    
    def find_structural_patterns(
        self,
        hypergraph: Dict,
        min_size: int = 2
    ) -> List[Pattern]:
        """
        Find structural patterns (graph motifs) in the hypergraph.
        
        Args:
            hypergraph: Hypergraph data structure
            min_size: Minimum pattern size (number of nodes)
        
        Returns:
            List of recognized structural patterns
        """
        patterns = []
        
        # Build adjacency structure
        adjacency = self._build_adjacency(hypergraph)
        
        # Find common motifs
        # Triangle pattern (3-cycle)
        triangles = self._find_triangles(adjacency)
        for i, triangle in enumerate(triangles):
            pattern = Pattern(
                pattern_id=f"triangle_{i}",
                node_ids=list(triangle),
                edge_ids=self._get_connecting_edges(hypergraph, triangle),
                pattern_type="triangle",
                confidence=1.0,
                metadata={"motif_type": "3-cycle"}
            )
            patterns.append(pattern)
        
        # Star pattern (hub and spokes)
        stars = self._find_stars(adjacency, min_spokes=3)
        for i, (hub, spokes) in enumerate(stars):
            pattern = Pattern(
                pattern_id=f"star_{i}",
                node_ids=[hub] + list(spokes),
                edge_ids=self._get_connecting_edges(hypergraph, [hub] + list(spokes)),
                pattern_type="star",
                confidence=1.0,
                metadata={"hub": hub, "num_spokes": len(spokes)}
            )
            patterns.append(pattern)
        
        # Chain pattern (linear sequence)
        chains = self._find_chains(adjacency, min_length=3)
        for i, chain in enumerate(chains):
            pattern = Pattern(
                pattern_id=f"chain_{i}",
                node_ids=chain,
                edge_ids=self._get_connecting_edges(hypergraph, chain),
                pattern_type="chain",
                confidence=1.0,
                metadata={"length": len(chain)}
            )
            patterns.append(pattern)
        
        self.recognized_patterns.extend(patterns)
        return patterns
    
    def find_activation_patterns(
        self,
        activation_history: List[Dict[str, float]],
        window_size: int = 3
    ) -> List[Pattern]:
        """
        Find temporal activation patterns in the history.
        
        Args:
            activation_history: List of activation snapshots
            window_size: Size of temporal window
        
        Returns:
            List of recognized activation patterns
        """
        patterns = []
        
        if len(activation_history) < window_size:
            return patterns
        
        # Sliding window over activation history
        for i in range(len(activation_history) - window_size + 1):
            window = activation_history[i:i + window_size]
            
            # Detect oscillation pattern
            if self._is_oscillating(window):
                active_nodes = set()
                for snapshot in window:
                    active_nodes.update(snapshot.keys())
                
                pattern = Pattern(
                    pattern_id=f"oscillation_{i}",
                    node_ids=list(active_nodes),
                    edge_ids=[],
                    pattern_type="oscillation",
                    confidence=0.8,
                    metadata={"window_start": i, "period": window_size}
                )
                patterns.append(pattern)
            
            # Detect cascade pattern
            if self._is_cascading(window):
                cascade_nodes = self._get_cascade_sequence(window)
                pattern = Pattern(
                    pattern_id=f"cascade_{i}",
                    node_ids=cascade_nodes,
                    edge_ids=[],
                    pattern_type="cascade",
                    confidence=0.9,
                    metadata={"window_start": i, "cascade_length": len(cascade_nodes)}
                )
                patterns.append(pattern)
        
        self.recognized_patterns.extend(patterns)
        return patterns
    
    def find_semantic_patterns(
        self,
        hypergraph: Dict,
        semantic_key: str = "identity_seed"
    ) -> List[Pattern]:
        """
        Find semantic patterns based on content similarity.
        
        Args:
            hypergraph: Hypergraph data structure
            semantic_key: Key in hypernode data containing semantic content
        
        Returns:
            List of recognized semantic patterns
        """
        patterns = []
        
        # Extract semantic content
        node_semantics = {}
        for node_id, node_data in hypergraph['hypernodes'].items():
            if semantic_key in node_data:
                node_semantics[node_id] = node_data[semantic_key]
        
        # Find clusters of similar content
        clusters = self._cluster_by_similarity(node_semantics)
        
        for i, cluster in enumerate(clusters):
            if len(cluster) >= 2:
                pattern = Pattern(
                    pattern_id=f"semantic_cluster_{i}",
                    node_ids=list(cluster),
                    edge_ids=self._get_connecting_edges(hypergraph, cluster),
                    pattern_type="semantic_cluster",
                    confidence=0.85,
                    metadata={"cluster_size": len(cluster)}
                )
                patterns.append(pattern)
        
        self.recognized_patterns.extend(patterns)
        return patterns
    
    def match_oeis_pattern(
        self,
        sequence: List[int],
        oeis_mappings: Dict[int, str]
    ) -> Optional[Pattern]:
        """
        Match a sequence to OEIS pattern database.
        
        Args:
            sequence: Integer sequence to match
            oeis_mappings: OEIS number to description mappings
        
        Returns:
            Matched pattern or None
        """
        # Simple matching based on sequence properties
        # In production, this would query OEIS API
        
        # Check for known sequences
        if len(sequence) >= 3:
            # Fibonacci-like
            if all(sequence[i] == sequence[i-1] + sequence[i-2] for i in range(2, len(sequence))):
                return Pattern(
                    pattern_id="oeis_fibonacci",
                    node_ids=[],
                    edge_ids=[],
                    pattern_type="oeis_sequence",
                    confidence=0.95,
                    metadata={"oeis_number": "A000045", "sequence": sequence}
                )
            
            # Powers of 2
            if all(sequence[i] == 2 ** i for i in range(len(sequence))):
                return Pattern(
                    pattern_id="oeis_powers_of_2",
                    node_ids=[],
                    edge_ids=[],
                    pattern_type="oeis_sequence",
                    confidence=0.95,
                    metadata={"oeis_number": "A000079", "sequence": sequence}
                )
        
        return None
    
    def _build_adjacency(self, hypergraph: Dict) -> Dict[str, Set[str]]:
        """Build adjacency list from hypergraph"""
        adjacency = defaultdict(set)
        
        for edge_data in hypergraph['hyperedges'].values():
            sources = edge_data['source_node_ids']
            targets = edge_data['target_node_ids']
            
            for source in sources:
                adjacency[source].update(targets)
        
        return dict(adjacency)
    
    def _find_triangles(self, adjacency: Dict[str, Set[str]]) -> List[Set[str]]:
        """Find triangle motifs (3-cycles)"""
        triangles = []
        nodes = list(adjacency.keys())
        
        for i, node1 in enumerate(nodes):
            for j, node2 in enumerate(nodes[i+1:], i+1):
                if node2 in adjacency.get(node1, set()):
                    for node3 in nodes[j+1:]:
                        if (node3 in adjacency.get(node1, set()) and
                            node3 in adjacency.get(node2, set())):
                            triangles.append({node1, node2, node3})
        
        return triangles
    
    def _find_stars(
        self,
        adjacency: Dict[str, Set[str]],
        min_spokes: int = 3
    ) -> List[Tuple[str, Set[str]]]:
        """Find star motifs (hub with spokes)"""
        stars = []
        
        for node, neighbors in adjacency.items():
            if len(neighbors) >= min_spokes:
                stars.append((node, neighbors))
        
        return stars
    
    def _find_chains(
        self,
        adjacency: Dict[str, Set[str]],
        min_length: int = 3
    ) -> List[List[str]]:
        """Find chain motifs (linear sequences)"""
        chains = []
        visited = set()
        
        def dfs_chain(node, chain):
            if node in visited:
                return
            
            visited.add(node)
            chain.append(node)
            
            neighbors = adjacency.get(node, set())
            unvisited_neighbors = [n for n in neighbors if n not in visited]
            
            if len(unvisited_neighbors) == 1:
                dfs_chain(unvisited_neighbors[0], chain)
            elif len(chain) >= min_length:
                chains.append(chain.copy())
        
        for node in adjacency:
            if node not in visited:
                dfs_chain(node, [])
        
        return [chain for chain in chains if len(chain) >= min_length]
    
    def _get_connecting_edges(
        self,
        hypergraph: Dict,
        node_ids: List[str]
    ) -> List[str]:
        """Get edges connecting the given nodes"""
        node_set = set(node_ids)
        connecting_edges = []
        
        for edge_id, edge_data in hypergraph['hyperedges'].items():
            sources = set(edge_data['source_node_ids'])
            targets = set(edge_data['target_node_ids'])
            
            if sources.issubset(node_set) and targets.issubset(node_set):
                connecting_edges.append(edge_id)
        
        return connecting_edges
    
    def _is_oscillating(self, window: List[Dict[str, float]]) -> bool:
        """Check if activation pattern is oscillating"""
        if len(window) < 3:
            return False
        
        # Check for alternating high/low activation
        for node_id in window[0].keys():
            values = [w.get(node_id, 0.0) for w in window]
            
            # Simple oscillation detection
            diffs = [values[i+1] - values[i] for i in range(len(values)-1)]
            sign_changes = sum(1 for i in range(len(diffs)-1) if diffs[i] * diffs[i+1] < 0)
            
            if sign_changes >= len(diffs) - 1:
                return True
        
        return False
    
    def _is_cascading(self, window: List[Dict[str, float]]) -> bool:
        """Check if activation pattern is cascading"""
        if len(window) < 2:
            return False
        
        # Check for progressive activation increase
        all_nodes = set()
        for snapshot in window:
            all_nodes.update(snapshot.keys())
        
        # Count newly activated nodes in each step
        activated_counts = []
        previously_active = set()
        
        for snapshot in window:
            newly_active = set(snapshot.keys()) - previously_active
            activated_counts.append(len(newly_active))
            previously_active.update(snapshot.keys())
        
        # Cascade if new nodes activate in each step
        return all(count > 0 for count in activated_counts)
    
    def _get_cascade_sequence(self, window: List[Dict[str, float]]) -> List[str]:
        """Get the sequence of nodes in a cascade"""
        sequence = []
        previously_active = set()
        
        for snapshot in window:
            newly_active = set(snapshot.keys()) - previously_active
            sequence.extend(sorted(newly_active))
            previously_active.update(snapshot.keys())
        
        return sequence
    
    def _cluster_by_similarity(
        self,
        node_semantics: Dict[str, Dict]
    ) -> List[Set[str]]:
        """Cluster nodes by semantic similarity"""
        clusters = []
        unclustered = set(node_semantics.keys())
        
        while unclustered:
            seed = unclustered.pop()
            cluster = {seed}
            seed_content = node_semantics[seed]
            
            to_check = list(unclustered)
            for node_id in to_check:
                similarity = self._calculate_similarity(
                    seed_content,
                    node_semantics[node_id]
                )
                
                if similarity >= self.similarity_threshold:
                    cluster.add(node_id)
                    unclustered.remove(node_id)
            
            clusters.append(cluster)
        
        return clusters
    
    def _calculate_similarity(self, content1: Dict, content2: Dict) -> float:
        """Calculate semantic similarity between two content dictionaries"""
        # Simple Jaccard similarity on keys
        keys1 = set(content1.keys())
        keys2 = set(content2.keys())
        
        if not keys1 and not keys2:
            return 1.0
        
        intersection = keys1 & keys2
        union = keys1 | keys2
        
        return len(intersection) / len(union) if union else 0.0
    
    def export_patterns(self, filepath: str):
        """Export recognized patterns to JSON file"""
        patterns_data = [pattern.to_dict() for pattern in self.recognized_patterns]
        
        with open(filepath, 'w') as f:
            json.dump(patterns_data, f, indent=2)
    
    def get_pattern_statistics(self) -> Dict:
        """Get statistics about recognized patterns"""
        pattern_types = defaultdict(int)
        total_confidence = 0.0
        
        for pattern in self.recognized_patterns:
            pattern_types[pattern.pattern_type] += 1
            total_confidence += pattern.confidence
        
        return {
            "total_patterns": len(self.recognized_patterns),
            "pattern_types": dict(pattern_types),
            "average_confidence": total_confidence / len(self.recognized_patterns) if self.recognized_patterns else 0.0
        }


if __name__ == "__main__":
    # Test pattern matcher
    matcher = PatternMatcher(similarity_threshold=0.7)
    
    # Sample hypergraph
    hypergraph = {
        "hypernodes": {
            "A": {"identity_seed": {"type": "symbolic"}},
            "B": {"identity_seed": {"type": "symbolic"}},
            "C": {"identity_seed": {"type": "narrative"}},
            "D": {"identity_seed": {"type": "symbolic"}},
        },
        "hyperedges": {
            "e1": {"source_node_ids": ["A"], "target_node_ids": ["B"]},
            "e2": {"source_node_ids": ["B"], "target_node_ids": ["C"]},
            "e3": {"source_node_ids": ["C"], "target_node_ids": ["A"]},
            "e4": {"source_node_ids": ["A"], "target_node_ids": ["D"]},
        }
    }
    
    # Find patterns
    structural = matcher.find_structural_patterns(hypergraph)
    semantic = matcher.find_semantic_patterns(hypergraph)
    
    print(f"Found {len(structural)} structural patterns")
    print(f"Found {len(semantic)} semantic patterns")
    print(f"\nPattern Statistics: {matcher.get_pattern_statistics()}")
