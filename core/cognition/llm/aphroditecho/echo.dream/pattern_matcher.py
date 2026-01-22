import logging
import random
from typing import Dict, List, Optional, Tuple
from database import db
from models_memory import MemoryNode, MemoryAssociation, PatternTemplate
logger = logging.getLogger(__name__)
class PatternMatcher:
    def __init__(self):
        self.similarity_threshold = 0.7
        self.basic_patterns = {'sequence': self._match_sequence_pattern, 'hierarchy': self._match_hierarchy_pattern, 'similarity': self._match_similarity_pattern, 'metaphor': self._match_metaphor_pattern, 'causality': self._match_causality_pattern}
        self.advanced_matchers = {'graph_isomorphism': self._match_graph_isomorphism, 'recursive_similarity': self._match_recursive_similarity, 'semantic_similarity': self._match_semantic_similarity}
        self.match_cache = {}
    def initialize_basic_patterns(self):
        from app import app
        with app.app_context():
            for pattern_type, matcher in self.basic_patterns.items():
                existing = PatternTemplate.query.filter_by(name=f'basic_{pattern_type}').first()
                if not existing:
                    structure = {'type': pattern_type, 'nodes': {'required': 2, 'max': 10}}
                    rules = {'matcher': pattern_type, 'threshold': self.similarity_threshold, 'params': {}}
                    description = self._get_pattern_description(pattern_type)
                    pattern = PatternTemplate(name=f'basic_{pattern_type}', pattern_type=pattern_type, description=description)
                    pattern.set_structure(structure)
                    pattern.set_rules(rules)
                    db.session.add(pattern)
            db.session.commit()
            logger.info('Basic pattern templates initialized')
    def _get_pattern_description(self, pattern_type: str) -> str:
        descriptions = {'sequence': 'Recognizes sequential patterns where items follow each other in a specific order.', 'hierarchy': 'Matches hierarchical structures with clear parent-child relationships.', 'similarity': 'Identifies patterns based on feature similarity across different contexts.', 'metaphor': 'Recognizes analogical mappings between different domains or contexts.', 'causality': 'Detects causal relationships and dependencies between events or concepts.'}
        return descriptions.get(pattern_type, f'Pattern template for {pattern_type} matching')
    def match_pattern(self, pattern_id: int, nodes: List[MemoryNode]) -> float:
        pattern = PatternTemplate.query.get(pattern_id)
        if not pattern:
            logger.error(f'Pattern template with ID {pattern_id} not found')
            return 0.0
        node_ids = sorted([node.id for node in nodes])
        node_ids_hash = hash(tuple(node_ids))
        if pattern_id in self.match_cache and node_ids_hash in self.match_cache[pattern_id]:
            return self.match_cache[pattern_id][node_ids_hash]
        rules = pattern.get_rules()
        if not rules or 'matcher' not in rules:
            logger.error(f'Pattern {pattern_id} has invalid rules: {rules}')
            return 0.0
        matcher_name = rules['matcher']
        matcher = None
        if matcher_name in self.basic_patterns:
            matcher = self.basic_patterns[matcher_name]
        elif matcher_name in self.advanced_matchers:
            matcher = self.advanced_matchers[matcher_name]
        else:
            logger.error(f'Unknown matcher: {matcher_name}')
            return 0.0
        rules.get('threshold', self.similarity_threshold)
        params = rules.get('params', {})
        try:
            match_score = matcher(nodes, pattern, **params)
            if pattern_id not in self.match_cache:
                self.match_cache[pattern_id] = {}
            self.match_cache[pattern_id][node_ids_hash] = match_score
            return match_score
        except Exception as e:
            logger.error(f'Error in pattern matcher {matcher_name}: {e}')
            return 0.0
    def find_matching_patterns(self, nodes: List[MemoryNode], threshold: Optional[float]=None) -> Dict[int, float]:
        if threshold is None:
            threshold = self.similarity_threshold
        patterns = PatternTemplate.query.all()
        matches = {}
        for pattern in patterns:
            match_score = self.match_pattern(pattern.id, nodes)
            if match_score >= threshold:
                matches[pattern.id] = match_score
        return dict(sorted(matches.items(), key=lambda x: x[1], reverse=True))
    def find_similar_memories(self, memory_id: int, limit: int=10, threshold: Optional[float]=None) -> List[Tuple[MemoryNode, float]]:
        if threshold is None:
            threshold = self.similarity_threshold
        memory = MemoryNode.query.get(memory_id)
        if not memory:
            logger.error(f'Memory node with ID {memory_id} not found')
            return []
        all_memories = MemoryNode.query.filter(MemoryNode.id != memory_id).all()
        similarities = []
        for other in all_memories:
            similarity = self._calculate_memory_similarity(memory, other)
            if similarity >= threshold:
                similarities.append((other, similarity))
        similarities.sort(key=lambda x: x[1], reverse=True)
        return similarities[:limit]
    def _calculate_memory_similarity(self, memory1: MemoryNode, memory2: MemoryNode) -> float:
        type_similarity = 1.0 if memory1.memory_type == memory2.memory_type else 0.5
        stage_diff = abs(memory1.consolidation_stage - memory2.consolidation_stage)
        stage_similarity = max(0.0, 1.0 - stage_diff / max(memory1.consolidation_stage, memory2.consolidation_stage, 1))
        valence_diff = abs(memory1.emotional_valence - memory2.emotional_valence)
        arousal_diff = abs(memory1.emotional_arousal - memory2.emotional_arousal)
        emotional_similarity = max(0.0, 1.0 - (valence_diff + arousal_diff) / 4.0)
        context1 = memory1.get_context()
        context2 = memory2.get_context()
        context_similarity = self._calculate_context_similarity(context1, context2)
        base_similarity = self._calculate_base_node_similarity(memory1.node_id, memory2.node_id)
        weights = {'type': 0.2, 'stage': 0.1, 'emotional': 0.2, 'context': 0.2, 'base': 0.3}
        weighted_similarity = weights['type'] * type_similarity + weights['stage'] * stage_similarity + weights['emotional'] * emotional_similarity + weights['context'] * context_similarity + weights['base'] * base_similarity
        return weighted_similarity
    def _calculate_context_similarity(self, context1: Dict, context2: Dict) -> float:
        if not context1 or not context2:
            return 0.5
        all_keys = set(context1.keys()).union(set(context2.keys()))
        if not all_keys:
            return 0.5
        matching_keys = 0
        matching_values = 0
        for key in all_keys:
            if key in context1 and key in context2:
                matching_keys += 1
                if context1[key] == context2[key]:
                    matching_values += 1
        key_similarity = matching_keys / len(all_keys)
        value_similarity = matching_values / max(1, matching_keys)
        return 0.6 * key_similarity + 0.4 * value_similarity
    def _calculate_base_node_similarity(self, node_id1: int, node_id2: int) -> float:
        from models import SelfReferentialNode, NodeConnection
        node1 = SelfReferentialNode.query.get(node_id1)
        node2 = SelfReferentialNode.query.get(node_id2)
        if not node1 or not node2:
            return 0.0
        type_similarity = 1.0 if node1.node_type == node2.node_type else 0.5
        connections1 = set()
        for conn in NodeConnection.query.filter((NodeConnection.source_id == node_id1) | (NodeConnection.target_id == node_id1)).all():
            other_id = conn.target_id if conn.source_id == node_id1 else conn.source_id
            connections1.add(other_id)
        connections2 = set()
        for conn in NodeConnection.query.filter((NodeConnection.source_id == node_id2) | (NodeConnection.target_id == node_id2)).all():
            other_id = conn.target_id if conn.source_id == node_id2 else conn.source_id
            connections2.add(other_id)
        all_connections = connections1.union(connections2)
        if not all_connections:
            connection_similarity = 0.5
        else:
            shared_connections = connections1.intersection(connections2)
            connection_similarity = len(shared_connections) / len(all_connections)
        expression_similarity = 0.5
        if node1.expression and node2.expression:
            expr1 = node1.expression.lower()
            expr2 = node2.expression.lower()
            words1 = set(expr1.split())
            words2 = set(expr2.split())
            common_words = words1.intersection(words2)
            all_words = words1.union(words2)
            if all_words:
                expression_similarity = len(common_words) / len(all_words)
        weights = {'type': 0.3, 'connection': 0.4, 'expression': 0.3}
        weighted_similarity = weights['type'] * type_similarity + weights['connection'] * connection_similarity + weights['expression'] * expression_similarity
        return weighted_similarity
    def _match_sequence_pattern(self, nodes: List[MemoryNode], pattern: PatternTemplate, **kwargs) -> float:
        if len(nodes) < 2:
            return 0.0
        sorted_nodes = sorted(nodes, key=lambda n: n.timestamp)
        sequence_score = 0.0
        associations_count = 0
        for i in range(len(sorted_nodes) - 1):
            node1 = sorted_nodes[i]
            node2 = sorted_nodes[i + 1]
            assoc = MemoryAssociation.query.filter((MemoryAssociation.source_id == node1.id) & (MemoryAssociation.target_id == node2.id) & MemoryAssociation.association_type.like('%temporal%')).first()
            if assoc:
                sequence_score += assoc.strength
                associations_count += 1
        if associations_count == 0:
            time_deltas = []
            for i in range(len(sorted_nodes) - 1):
                delta = (sorted_nodes[i + 1].timestamp - sorted_nodes[i].timestamp).total_seconds()
                time_deltas.append(delta)
            if time_deltas:
                avg_delta = sum(time_deltas) / len(time_deltas)
                variance = sum(((d - avg_delta) ** 2 for d in time_deltas)) / len(time_deltas)
                std_dev = variance ** 0.5
                if std_dev < avg_delta / 2:
                    return 0.8
                else:
                    return 0.5
            return 0.3
        return sequence_score / max(1, associations_count)
    def _match_hierarchy_pattern(self, nodes: List[MemoryNode], pattern: PatternTemplate, **kwargs) -> float:
        if len(nodes) < 2:
            return 0.0
        base_nodes = {}
        for node in nodes:
            base_nodes[node.id] = node.node_id
        from models import SelfReferentialNode
        hierarchy_score = 0.0
        hierarchy_count = 0
        for node_id, base_id in base_nodes.items():
            base_node = SelfReferentialNode.query.get(base_id)
            if not base_node:
                continue
            if base_node.parent_id and base_node.parent_id in base_nodes.values():
                hierarchy_score += 1.0
                hierarchy_count += 1
            children = SelfReferentialNode.query.filter_by(parent_id=base_id).all()
            for child in children:
                if child.id in base_nodes.values():
                    hierarchy_score += 1.0
                    hierarchy_count += 1
        if hierarchy_count == 0:
            hierarch_assocs = 0
            for i, node1 in enumerate(nodes):
                for node2 in nodes[i + 1:]:
                    assoc = MemoryAssociation.query.filter(((MemoryAssociation.source_id == node1.id) & (MemoryAssociation.target_id == node2.id) | (MemoryAssociation.source_id == node2.id) & (MemoryAssociation.target_id == node1.id)) & MemoryAssociation.association_type.like('%hierarch%')).first()
                    if assoc:
                        hierarchy_score += assoc.strength
                        hierarch_assocs += 1
            if hierarch_assocs > 0:
                return hierarchy_score / hierarch_assocs
            return 0.3
        max_possible = len(nodes) * 2
        return min(1.0, hierarchy_score / max_possible)
    def _match_similarity_pattern(self, nodes: List[MemoryNode], pattern: PatternTemplate, **kwargs) -> float:
        if len(nodes) < 2:
            return 0.0
        similarity_scores = []
        for i, node1 in enumerate(nodes):
            for node2 in nodes[i + 1:]:
                similarity = self._calculate_memory_similarity(node1, node2)
                similarity_scores.append(similarity)
        if similarity_scores:
            return sum(similarity_scores) / len(similarity_scores)
        return 0.0
    def _match_metaphor_pattern(self, nodes: List[MemoryNode], pattern: PatternTemplate, **kwargs) -> float:
        if len(nodes) < 4:
            return 0.0
        nodes_by_type = {}
        for node in nodes:
            if node.memory_type not in nodes_by_type:
                nodes_by_type[node.memory_type] = []
            nodes_by_type[node.memory_type].append(node)
        valid_types = [t for t, ns in nodes_by_type.items() if len(ns) >= 2]
        if len(valid_types) < 2:
            return 0.0
        type1, type2 = valid_types[:2]
        nodes1 = nodes_by_type[type1][:2]
        nodes2 = nodes_by_type[type2][:2]
        relation1 = self._calculate_memory_similarity(nodes1[0], nodes1[1])
        relation2 = self._calculate_memory_similarity(nodes2[0], nodes2[1])
        relation_diff = abs(relation1 - relation2)
        analogy_score = max(0.0, 1.0 - relation_diff)
        cross_similarity1 = self._calculate_memory_similarity(nodes1[0], nodes2[0])
        cross_similarity2 = self._calculate_memory_similarity(nodes1[1], nodes2[1])
        return 0.7 * analogy_score + 0.15 * cross_similarity1 + 0.15 * cross_similarity2
    def _match_causality_pattern(self, nodes: List[MemoryNode], pattern: PatternTemplate, **kwargs) -> float:
        if len(nodes) < 2:
            return 0.0
        sorted_nodes = sorted(nodes, key=lambda n: n.timestamp)
        causal_score = 0.0
        causal_count = 0
        for i in range(len(sorted_nodes) - 1):
            node1 = sorted_nodes[i]
            node2 = sorted_nodes[i + 1]
            assoc = MemoryAssociation.query.filter((MemoryAssociation.source_id == node1.id) & (MemoryAssociation.target_id == node2.id) & MemoryAssociation.association_type.like('%caus%')).first()
            if assoc:
                causal_score += assoc.strength
                causal_count += 1
        if causal_count == 0:
            emotions_change = []
            time_proximity = []
            for i in range(len(sorted_nodes) - 1):
                node1 = sorted_nodes[i]
                node2 = sorted_nodes[i + 1]
                valence_change = abs(node2.emotional_valence - node1.emotional_valence)
                arousal_change = abs(node2.emotional_arousal - node1.emotional_arousal)
                emotions_change.append(valence_change + arousal_change)
                delta = (node2.timestamp - node1.timestamp).total_seconds()
                proximity = max(0.0, 1.0 - min(1.0, delta / 3600))
                time_proximity.append(proximity)
            if emotions_change and time_proximity:
                emotion_score = sum(emotions_change) / len(emotions_change)
                proximity_score = sum(time_proximity) / len(time_proximity)
                return 0.6 * emotion_score + 0.4 * proximity_score
            return 0.2
        return causal_score / causal_count
    def _match_graph_isomorphism(self, nodes: List[MemoryNode], pattern: PatternTemplate, **kwargs) -> float:
        if len(nodes) < 2:
            return 0.0
        connections = 0
        for i, node1 in enumerate(nodes):
            for node2 in nodes[i + 1:]:
                assoc = MemoryAssociation.query.filter((MemoryAssociation.source_id == node1.id) & (MemoryAssociation.target_id == node2.id) | (MemoryAssociation.source_id == node2.id) & (MemoryAssociation.target_id == node1.id)).first()
                if assoc:
                    connections += 1
        max_connections = len(nodes) * (len(nodes) - 1) / 2
        connectivity = connections / max(1, max_connections)
        return min(0.8, 0.4 + 0.6 * connectivity)
    def _match_recursive_similarity(self, nodes: List[MemoryNode], pattern: PatternTemplate, **kwargs) -> float:
        if len(nodes) < 4:
            return 0.0
        recursive_score = random.uniform(0.4, 0.9)
        node_types = set((node.memory_type for node in nodes))
        type_diversity = len(node_types) / len(nodes)
        adjusted_score = recursive_score * (0.7 + 0.3 * type_diversity)
        return min(1.0, adjusted_score)
    def _match_semantic_similarity(self, nodes: List[MemoryNode], pattern: PatternTemplate, **kwargs) -> float:
        if len(nodes) < 2:
            return 0.0
        contexts = [node.get_context() for node in nodes]
        similarities = []
        for i, ctx1 in enumerate(contexts):
            for ctx2 in contexts[i + 1:]:
                sim = self._calculate_context_similarity(ctx1, ctx2)
                similarities.append(sim)
        if similarities:
            avg_similarity = sum(similarities) / len(similarities)
            return min(1.0, avg_similarity * 1.2)
        return 0.5
pattern_matcher = PatternMatcher()