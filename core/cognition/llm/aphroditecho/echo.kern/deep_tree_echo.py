import logging
from dataclasses import dataclass, field
from typing import List, Dict, Optional, Any, Tuple
import numpy as np
from collections import deque
from pathlib import Path
from ml_system import MLSystem
from emotional_dynamics import EmotionalDynamics, EmotionalState
from differential_emotion_theory import DifferentialEmotionSystem, DETState, DETEmotion
@dataclass
class SpatialContext:
    position: Tuple[float, float, float] = (0.0, 0.0, 0.0)
    orientation: Tuple[float, float, float] = (0.0, 0.0, 0.0)
    scale: float = 1.0
    depth: float = 1.0
    field_of_view: float = 90.0
    spatial_relations: Dict[str, Any] = field(default_factory=dict)
    spatial_memory: Dict[str, Any] = field(default_factory=dict)
@dataclass
class TreeNode:
    content: str
    echo_value: float = 0.0
    children: List['TreeNode'] = None
    parent: Optional['TreeNode'] = None
    metadata: Dict[str, Any] = None
    emotional_state: np.ndarray = None
    det_state: Optional[DETState] = None
    spatial_context: Optional[SpatialContext] = None
    def __post_init__(self):
        if self.children is None:
            self.children = []
        if self.metadata is None:
            self.metadata = {}
        if self.emotional_state is None:
            self.emotional_state = np.array([0.1] * 7)
        if self.det_state is None:
            self.det_state = None
        if self.spatial_context is None:
            self.spatial_context = SpatialContext()
class DeepTreeEcho:
    def __init__(self, echo_threshold: float=0.75, max_depth: int=10, use_julia: bool=True):
        self.logger = logging.getLogger(__name__)
        self.echo_threshold = echo_threshold
        self.max_depth = max_depth
        self.root = None
        self.ml_system = MLSystem()
        self.emotional_dynamics = EmotionalDynamics(use_julia=use_julia)
        self.det_system = DifferentialEmotionSystem(use_julia=use_julia)
        self.default_emotional_state = EmotionalState()
        self.spatial_awareness_enabled = True
        self.spatial_influence_factor = 0.15
        self.environment_map = {}
        try:
            from sensory_motor_simple import SensoryMotorSystem
            self.sensory_motor = SensoryMotorSystem()
            self.logger.info('Enhanced sensory motor system with 3D capabilities loaded')
        except ImportError:
            try:
                from sensory_motor import SensoryMotorSystem
                self.sensory_motor = SensoryMotorSystem()
                self.logger.info('Standard sensory motor system loaded')
            except ImportError:
                self.logger.warning('No sensory motor system available')
                self.sensory_motor = None
    def create_tree(self, content: str) -> TreeNode:
        initial_emotions = self.emotional_dynamics.content_to_emotion(content)
        self.root = TreeNode(content=content, emotional_state=initial_emotions)
        det_emotions = self.det_system.content_to_det_emotion(content)
        self.root.det_state = DETState(det_emotions=det_emotions)
        self.root.spatial_context = SpatialContext(position=(0.0, 0.0, 0.0), orientation=(0.0, 0.0, 0.0), field_of_view=110.0)
        return self.root
    def add_child(self, parent: TreeNode, content: str) -> TreeNode:
        child_emotions = self.emotional_dynamics.content_to_emotion(content)
        child = TreeNode(content=content, parent=parent, emotional_state=child_emotions)
        parent.children.append(child)
        det_emotions = self.det_system.content_to_det_emotion(content)
        child.det_state = DETState(det_emotions=det_emotions)
        if parent.spatial_context:
            relative_pos = (0.5, 0.2, 0.1)
            child.spatial_context = SpatialContext(position=(parent.spatial_context.position[0] + relative_pos[0], parent.spatial_context.position[1] + relative_pos[1], parent.spatial_context.position[2] + relative_pos[2]), orientation=parent.spatial_context.orientation, field_of_view=parent.spatial_context.field_of_view, depth=parent.spatial_context.depth + 0.1)
        child.echo_value = self.calculate_echo_value(child)
        return child
    def add_child_with_spatial_context(self, parent: TreeNode, content: str, position: Tuple[float, float, float]=None, orientation: Tuple[float, float, float]=None, depth: float=None) -> TreeNode:
        child = self.add_child(parent, content)
        if position:
            child.spatial_context.position = position
        if orientation:
            child.spatial_context.orientation = orientation
        if depth:
            child.spatial_context.depth = depth
        child.echo_value = self.calculate_echo_value(child)
        return child
    def calculate_echo_value(self, node: TreeNode) -> float:
        base_echo = len(node.content) / 1000
        unique_chars = len(set(node.content))
        complexity_factor = unique_chars / 128
        child_echo = 0
        if node.children:
            child_values = [child.echo_value for child in node.children]
            child_echo = np.mean(child_values) if child_values else 0
        depth_factor = 1 / (1 + self.get_node_depth(node))
        sibling_echo = 0
        if node.parent:
            sibling_values = [sibling.echo_value for sibling in node.parent.children if sibling != node]
            sibling_echo = np.mean(sibling_values) if sibling_values else 0
        historical_echo = node.metadata.get('historical_echo', 0)
        emotional_modifier = self.emotional_dynamics.emotion_to_echo_modifier(node.emotional_state)
        det_modifier = 0.0
        if node.det_state is not None:
            active_scripts = node.metadata.get('active_scripts', [])
            for script_name in active_scripts:
                if script_name in ['Exploration', 'Celebration', 'Orientation']:
                    det_modifier += 0.1
                elif script_name in ['Escape', 'Withdrawal', 'Atonement']:
                    det_modifier -= 0.1
            if 'valence' in node.det_state.cognitive_factors:
                det_modifier += node.det_state.cognitive_factors['valence'] * 0.1
            if 'arousal' in node.det_state.cognitive_factors:
                det_modifier += (node.det_state.cognitive_factors['arousal'] - 0.5) * 0.1
        spatial_modifier = 0.0
        if self.spatial_awareness_enabled and node.spatial_context:
            optimal_depth = 3.0
            depth_diff = abs(node.spatial_context.depth - optimal_depth)
            spatial_modifier -= depth_diff * 0.03
            fov_factor = (node.spatial_context.field_of_view - 90) / 90
            spatial_modifier += fov_factor * 0.05
            distance_from_center = np.sqrt(node.spatial_context.position[0] ** 2 + node.spatial_context.position[1] ** 2)
            spatial_modifier -= distance_from_center * 0.02
            spatial_modifier = max(-0.2, min(0.2, spatial_modifier))
        echo_value = 0.4 * base_echo + 0.2 * complexity_factor + 0.1 * child_echo + 0.1 * depth_factor + 0.1 * sibling_echo + 0.1 * historical_echo
        echo_value = min(1.0, max(0.0, echo_value + emotional_modifier + det_modifier + spatial_modifier * self.spatial_influence_factor))
        return echo_value
    def get_node_depth(self, node: TreeNode) -> int:
        if node is None:
            return -1
        depth = 0
        current = node
        while current.parent is not None:
            depth += 1
            current = current.parent
        return depth
    def visualize_in_3d_space(self) -> Dict[str, Any]:
        visualization_data = {'nodes': [], 'edges': [], 'spatial_info': {}}
        if self.root is None:
            return visualization_data
        queue = deque([(self.root, None)])
        node_id = 0
        id_map = {}
        while queue:
            node, parent_id = queue.popleft()
            current_id = node_id
            id_map[node] = current_id
            node_id += 1
            spatial_data = {}
            if node.spatial_context:
                spatial_data = {'position': node.spatial_context.position, 'orientation': node.spatial_context.orientation, 'depth': node.spatial_context.depth, 'fov': node.spatial_context.field_of_view}
            else:
                level = self.get_node_depth(node)
                spatial_data = {'position': (level * 2, current_id % 5 * 1.5, 0), 'orientation': (0, 0, 0), 'depth': level, 'fov': 90}
            node_data = {'id': current_id, 'content': node.content[:50] + ('...' if len(node.content) > 50 else ''), 'echo_value': node.echo_value, 'spatial': spatial_data}
            visualization_data['nodes'].append(node_data)
            if parent_id is not None:
                edge = {'source': parent_id, 'target': current_id, 'weight': node.echo_value}
                visualization_data['edges'].append(edge)
            for child in node.children:
                queue.append((child, current_id))
        visualization_data['spatial_info'] = {'bounds': {'x': [-10, 10], 'y': [-10, 10], 'z': [-10, 10]}, 'optimal_viewing_position': (5, 5, 5), 'echo_threshold': self.echo_threshold}
        return visualization_data
    def update_from_sensory_input(self):
        if not self.sensory_motor:
            self.logger.warning('No sensory motor system available for input')
            return False
        try:
            import asyncio
            input_data = asyncio.run(self.sensory_motor.process_all())
            if input_data.get('status') != 'processed':
                self.logger.info(f"Sensory input not processed: {input_data.get('reason', 'unknown reason')}")
                return False
            detected_objects = input_data.get('objects', [])
            if detected_objects:
                for obj in detected_objects:
                    obj_id = obj.get('id')
                    if obj_id:
                        self.environment_map[obj_id] = {'class': obj.get('class'), 'position': obj.get('position'), 'depth': obj.get('depth'), 'last_seen': obj.get('last_seen', 0)}
                if self.root:
                    for obj in detected_objects:
                        if obj.get('confidence', 0) > 0.85:
                            content = f"Detected {obj.get('class')} at depth {obj.get('depth'):.2f}"
                            position = (obj.get('position', {}).get('x', 0) / 1000, obj.get('position', {}).get('y', 0) / 1000, obj.get('depth', 1.0))
                            self.add_child_with_spatial_context(self.root, content, position=position, depth=obj.get('depth', 1.0))
            motion_data = input_data.get('motion', {})
            if motion_data and motion_data.get('motion_detected'):
                motion_content = f"Detected {motion_data.get('motion_count', 0)} motion regions"
                motion_child = self.add_child(self.root, motion_content)
                motion_child.metadata['motion_regions'] = motion_data.get('motion_regions', [])
            return True
        except Exception as e:
            self.logger.error(f'Error updating from sensory input: {str(e)}')
            return False
    def apply_spatial_dynamics(self, node: TreeNode=None):
        if node is None:
            node = self.root
        if node is None:
            return
        for child in node.children:
            if node.spatial_context and child.spatial_context:
                rel_x = child.spatial_context.position[0] - node.spatial_context.position[0]
                rel_y = child.spatial_context.position[1] - node.spatial_context.position[1]
                rel_z = child.spatial_context.position[2] - node.spatial_context.position[2]
                distance = np.sqrt(rel_x ** 2 + rel_y ** 2 + rel_z ** 2)
                child.spatial_context.spatial_relations['parent_distance'] = distance
                child.spatial_context.spatial_relations['parent_direction'] = (rel_x / distance if distance > 0 else 0, rel_y / distance if distance > 0 else 0, rel_z / distance if distance > 0 else 0)
                child.metadata['spatial_distance'] = distance
                optimal_distance = 1.0
                distance_factor = 1.0 - abs(distance - optimal_distance) / 2
                distance_factor = max(0.0, min(1.0, distance_factor))
                child.echo_value = 0.8 * child.echo_value + 0.2 * distance_factor
        for child in node.children:
            self.apply_spatial_dynamics(child)
    def simulate_det_dynamics(self, node: TreeNode, time_span: Tuple[float, float]=(0.0, 5.0)):
        if node is None or node.det_state is None:
            return
        updated_det_state = self.det_system.simulate_appraisal(node.det_state, time_span)
        node.det_state = updated_det_state
        active_scripts = self.det_system.identify_active_scripts(node.det_state)
        node.metadata['active_scripts'] = [script.name for script in active_scripts]
        responses = self.det_system.extract_behavioral_responses(node.det_state)
        node.metadata['behavioral_responses'] = responses
        core_emotions = self.det_system.map_det_to_core(node.det_state.det_emotions)
        node.emotional_state = core_emotions
        node.echo_value = self.calculate_echo_value(node)
        self.update_spatial_from_emotion(node)
    def update_spatial_from_emotion(self, node: TreeNode):
        if not node.det_state or not node.spatial_context:
            return
        joy = node.det_state.det_emotions[DETEmotion.JOY.value]
        interest = node.det_state.det_emotions[DETEmotion.INTEREST.value]
        base_fov = 90.0
        fov_modifier = (joy * 0.5 + interest * 0.5) * 40.0
        node.spatial_context.field_of_view = min(140.0, base_fov + fov_modifier)
        fear = node.det_state.det_emotions[DETEmotion.FEAR.value]
        anxiety = node.det_state.det_emotions[DETEmotion.ANXIETY.value]
        depth_modifier = (fear * 0.7 + anxiety * 0.3) * 2.0
        node.spatial_context.depth += depth_modifier
        anger = node.det_state.det_emotions[DETEmotion.ANGER.value]
        contempt = node.det_state.det_emotions[DETEmotion.CONTEMPT.value]
        current_pitch = node.spatial_context.orientation[0]
        pitch_modifier = (anger * 0.4 + contempt * 0.6) * 30.0
        new_pitch = min(45.0, current_pitch + pitch_modifier)
        node.spatial_context.orientation = (new_pitch, node.spatial_context.orientation[1], node.spatial_context.orientation[2])
    def inject_echo(self, source_node: TreeNode, target_node: TreeNode, strength: float=0.5):
        if source_node is None or target_node is None:
            return
        if source_node.echo_value < self.echo_threshold * 0.5:
            return
        emotional_similarity = 0.5
        if source_node.emotional_state is not None and target_node.emotional_state is not None:
            similarity = np.dot(source_node.emotional_state, target_node.emotional_state)
            similarity /= np.linalg.norm(source_node.emotional_state) * np.linalg.norm(target_node.emotional_state)
            emotional_similarity = (similarity + 1.0) / 2.0
        echo_boost = strength * emotional_similarity * source_node.echo_value * 0.3
        target_node.echo_value = min(1.0, target_node.echo_value + echo_boost)
        if 'echo_injections' not in target_node.metadata:
            target_node.metadata['echo_injections'] = []
        target_node.metadata['echo_injections'].append({'source': source_node.content[:50], 'strength': strength, 'similarity': emotional_similarity, 'boost': echo_boost})
    def propagate_echoes(self):
        if self.root is None:
            return
        self._update_all_echo_values(self.root)
        self._propagate_down(self.root)
        self._propagate_up(self.root)
        self._apply_echo_decay(self.root)
        if self.spatial_awareness_enabled:
            self.apply_spatial_dynamics()
    def _update_all_echo_values(self, node: TreeNode):
        node.echo_value = self.calculate_echo_value(node)
        for child in node.children:
            self._update_all_echo_values(child)
    def _propagate_down(self, node: TreeNode, depth: int=0):
        if node is None or depth > self.max_depth:
            return
        if node.echo_value >= self.echo_threshold:
            for child in node.children:
                propagation_factor = self._calculate_propagation_factor(node, child)
                echo_propagation = node.echo_value * propagation_factor
                child.echo_value = min(1.0, child.echo_value + echo_propagation)
                if 'echo_propagations' not in child.metadata:
                    child.metadata['echo_propagations'] = []
                child.metadata['echo_propagations'].append({'direction': 'down', 'from': 'parent', 'factor': propagation_factor, 'value': echo_propagation})
                self._propagate_down(child, depth + 1)
    def _propagate_up(self, node: TreeNode):
        if node is None:
            return
        for child in node.children:
            self._propagate_up(child)
        if node.parent and node.echo_value >= self.echo_threshold:
            propagation_factor = self._calculate_propagation_factor(node, node.parent) * 0.7
            echo_propagation = node.echo_value * propagation_factor
            node.parent.echo_value = min(1.0, node.parent.echo_value + echo_propagation)
            if 'echo_propagations' not in node.parent.metadata:
                node.parent.metadata['echo_propagations'] = []
            node.parent.metadata['echo_propagations'].append({'direction': 'up', 'from': 'child', 'factor': propagation_factor, 'value': echo_propagation})
    def _apply_echo_decay(self, node: TreeNode):
        if node is None:
            return
        decay_factor = 0.95
        node.metadata['historical_echo'] = node.echo_value
        node.echo_value = node.echo_value * decay_factor
        for child in node.children:
            self._apply_echo_decay(child)
    def _calculate_propagation_factor(self, source: TreeNode, target: TreeNode) -> float:
        base_factor = 0.3
        emotional_similarity = 0.5
        if source.emotional_state is not None and target.emotional_state is not None:
            similarity = np.dot(source.emotional_state, target.emotional_state)
            similarity /= np.linalg.norm(source.emotional_state) * np.linalg.norm(target.emotional_state)
            emotional_similarity = (similarity + 1.0) / 2.0
        content_similarity = 0.5
        if len(source.content) > 0 and len(target.content) > 0:
            src_words = set(source.content.lower().split())
            tgt_words = set(target.content.lower().split())
            if len(src_words) > 0 and len(tgt_words) > 0:
                shared = len(src_words.intersection(tgt_words))
                total = len(src_words.union(tgt_words))
                content_similarity = shared / total
        spatial_factor = 0.5
        if self.spatial_awareness_enabled and source.spatial_context and target.spatial_context:
            p1 = source.spatial_context.position
            p2 = target.spatial_context.position
            distance = np.sqrt((p1[0] - p2[0]) ** 2 + (p1[1] - p2[1]) ** 2 + (p1[2] - p2[2]) ** 2)
            spatial_factor = 1.0 / (1.0 + distance)
        propagation_factor = base_factor * (0.4 * emotional_similarity + 0.4 * content_similarity + 0.2 * spatial_factor)
        return propagation_factor
    def prune_weak_echoes(self):
        if self.root is None:
            return
        self._reset_weak_echoes(self.root)
    def _reset_weak_echoes(self, node: TreeNode):
        if node.echo_value < self.echo_threshold * 0.5:
            node.echo_value = 0.0
        for child in node.children:
            self._reset_weak_echoes(child)
    def analyze_echo_patterns(self) -> Dict[str, Any]:
        if self.root is None:
            return {'avg_echo': 0.0, 'max_echo': 0.0, 'resonant_nodes': 0, 'total_nodes': 0, 'depth': 0}
        all_nodes = self._collect_all_nodes(self.root)
        echo_values = [node.echo_value for node in all_nodes]
        avg_echo = np.mean(echo_values) if echo_values else 0.0
        max_echo = np.max(echo_values) if echo_values else 0.0
        resonant_nodes = sum((1 for echo in echo_values if echo >= self.echo_threshold))
        total_nodes = len(all_nodes)
        max_depth = max((self.get_node_depth(node) for node in all_nodes))
        return {'avg_echo': avg_echo, 'max_echo': max_echo, 'resonant_nodes': resonant_nodes, 'total_nodes': total_nodes, 'depth': max_depth}
    def _collect_all_nodes(self, node: TreeNode, nodes: List[TreeNode]=None) -> List[TreeNode]:
        if nodes is None:
            nodes = []
        nodes.append(node)
        for child in node.children:
            self._collect_all_nodes(child, nodes)
        return nodes
    def perform_recursive_introspection(self, repository_root: Optional[Path]=None, current_load: float=0.6, recent_activity: float=0.4) -> Dict[str, Any]:
        try:
            from echoself_introspection import EchoselfIntrospector
            introspector = EchoselfIntrospector(repository_root)
            cognitive_snapshot = introspector.get_cognitive_snapshot(current_load, recent_activity)
            prompt = introspector.inject_repo_input_into_prompt(current_load, recent_activity)
            integration_results = {'cognitive_snapshot': cognitive_snapshot, 'hypergraph_prompt': prompt[:1000] + '...' if len(prompt) > 1000 else prompt, 'echo_integration': None}
            if self.root is not None:
                introspection_content = f"Recursive Self-Introspection: {cognitive_snapshot['total_files_processed']} files, avg salience: {cognitive_snapshot['average_salience']:.3f}"
                introspection_node = self.add_child(self.root, introspection_content)
                introspection_node.echo_value = min(0.95, cognitive_snapshot['average_salience'] * 1.2)
                introspection_node.metadata.update({'type': 'recursive_introspection', 'cognitive_snapshot': cognitive_snapshot, 'attention_threshold': cognitive_snapshot['attention_threshold'], 'timestamp': cognitive_snapshot['timestamp']})
                integration_results['echo_integration'] = {'node_id': id(introspection_node), 'echo_value': introspection_node.echo_value, 'parent_echo': self.root.echo_value}
                self.logger.info(f'Integrated introspection node with echo value: {introspection_node.echo_value:.3f}')
            return integration_results
        except ImportError as e:
            self.logger.error(f'Could not import echoself_introspection: {e}')
            return {'error': 'Introspection module not available', 'cognitive_snapshot': None, 'hypergraph_prompt': None, 'echo_integration': None}
        except Exception as e:
            self.logger.error(f'Error during recursive introspection: {e}')
            return {'error': str(e), 'cognitive_snapshot': None, 'hypergraph_prompt': None, 'echo_integration': None}