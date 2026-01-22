import json
import time
from typing import Dict, Optional, Any
import sys
import os
sys.path.append(os.path.join(os.path.dirname(__file__), '..', 'echo.dream'))
sys.path.append('.')
try:
    from models_memory import MemoryNode as DreamMemoryNode, MemoryCycle
    from database import db
    HAS_DREAM_MODELS = True
except ImportError:
    HAS_DREAM_MODELS = False
    class DreamMemoryNode:
        pass
    class MemoryCycle:
        pass
from embodied_memory_system import EmbodiedContext, BodyState, BodyConfiguration, SpatialAnchor
class EmbodiedMemoryNode(DreamMemoryNode if HAS_DREAM_MODELS else object):
    if HAS_DREAM_MODELS:
        embodied_context_json = db.Column(db.Text)
        body_state = db.Column(db.String(32), default='neutral')
        spatial_position_x = db.Column(db.Float, default=0.0)
        spatial_position_y = db.Column(db.Float, default=0.0)
        spatial_position_z = db.Column(db.Float, default=0.0)
        spatial_anchor_type = db.Column(db.String(32), default='egocentric')
        spatial_activation = db.Column(db.Float, default=0.0)
        body_state_activation = db.Column(db.Float, default=0.0)
        sensory_activation = db.Column(db.Float, default=0.0)
    def __init__(self, *args, **kwargs):
        self.embodied_context = kwargs.pop('embodied_context', None)
        if HAS_DREAM_MODELS:
            super().__init__(*args, **kwargs)
            if self.embodied_context:
                self._update_embodied_fields()
        else:
            self.memory_type = kwargs.get('memory_type', 'episodic')
            self.activation_level = kwargs.get('activation_level', 0.0)
            self.content = kwargs.get('content', '')
    def _update_embodied_fields(self):
        if not self.embodied_context:
            return
        context = self.embodied_context
        self.embodied_context_json = json.dumps(context.to_dict())
        self.body_state = context.body_state.value
        self.spatial_position_x = context.body_config.position[0]
        self.spatial_position_y = context.body_config.position[1]
        self.spatial_position_z = context.body_config.position[2]
        self.spatial_anchor_type = context.spatial_anchor.value
    def get_embodied_context(self) -> Optional[EmbodiedContext]:
        if hasattr(self, 'embodied_context_json') and self.embodied_context_json:
            try:
                context_dict = json.loads(self.embodied_context_json)
                return EmbodiedContext.from_dict(context_dict)
            except (json.JSONDecodeError, KeyError) as e:
                print(f'Warning: Could not parse embodied context: {e}')
        return None
    def set_embodied_context(self, context: EmbodiedContext):
        self.embodied_context = context
        self._update_embodied_fields()
    def activate_embodied(self, current_context: EmbodiedContext, amount: float=0.5):
        if hasattr(super(), 'activate'):
            super().activate(amount)
        else:
            self.activation_level = min(1.0, self.activation_level + amount)
        stored_context = self.get_embodied_context()
        if stored_context:
            similarity = self._calculate_embodied_similarity(stored_context, current_context)
            self.spatial_activation = similarity * 0.3
            self.body_state_activation = (1.0 if stored_context.body_state == current_context.body_state else 0.3) * 0.2
            self.sensory_activation = self._calculate_sensory_similarity(stored_context, current_context) * 0.2
            embodied_boost = self.spatial_activation + self.body_state_activation + self.sensory_activation
            self.activation_level = min(1.0, self.activation_level + embodied_boost)
    @staticmethod
    def _calculate_embodied_similarity(context1: EmbodiedContext, context2: EmbodiedContext) -> float:
        similarity_factors = []
        pos1 = context1.body_config.position
        pos2 = context2.body_config.position
        spatial_distance = ((pos1[0] - pos2[0]) ** 2 + (pos1[1] - pos2[1]) ** 2 + (pos1[2] - pos2[2]) ** 2) ** 0.5
        spatial_similarity = 1.0 / (1.0 + spatial_distance)
        similarity_factors.append(spatial_similarity)
        if context1.body_state == context2.body_state:
            similarity_factors.append(1.0)
        else:
            similarity_factors.append(0.3)
        if context1.emotional_state and context2.emotional_state:
            emotional_sim = EmbodiedMemoryNode._cosine_similarity(context1.emotional_state, context2.emotional_state)
            similarity_factors.append(emotional_sim)
        return sum(similarity_factors) / len(similarity_factors)
    @staticmethod
    def _calculate_sensory_similarity(context1: EmbodiedContext, context2: EmbodiedContext) -> float:
        if not context1.sensory_input or not context2.sensory_input:
            return 0.0
        common_sensors = set(context1.sensory_input.keys()) & set(context2.sensory_input.keys())
        if not common_sensors:
            return 0.0
        similarities = []
        for sensor in common_sensors:
            val1 = context1.sensory_input[sensor]
            val2 = context2.sensory_input[sensor]
            if isinstance(val1, (int, float)) and isinstance(val2, (int, float)):
                similarities.append(1.0 - abs(val1 - val2))
        return sum(similarities) / len(similarities) if similarities else 0.0
    @staticmethod
    def _cosine_similarity(dict1: Dict[str, float], dict2: Dict[str, float]) -> float:
        if not dict1 or not dict2:
            return 0.0
        common_keys = set(dict1.keys()) & set(dict2.keys())
        if not common_keys:
            return 0.0
        vec1 = [dict1[k] for k in common_keys]
        vec2 = [dict2[k] for k in common_keys]
        dot_product = sum((a * b for a, b in zip(vec1, vec2)))
        norm1 = sum((a * a for a in vec1)) ** 0.5
        norm2 = sum((b * b for b in vec2)) ** 0.5
        if norm1 == 0 or norm2 == 0:
            return 0.0
        return dot_product / (norm1 * norm2)
class EmbodiedMemoryCycle(MemoryCycle if HAS_DREAM_MODELS else object):
    if HAS_DREAM_MODELS:
        body_state_filter = db.Column(db.String(32))
        spatial_scope_x = db.Column(db.Float)
        spatial_scope_y = db.Column(db.Float)
        spatial_scope_z = db.Column(db.Float)
        spatial_scope_radius = db.Column(db.Float, default=10.0)
        embodied_consolidation_factor = db.Column(db.Float, default=0.1)
        spatial_decay_factor = db.Column(db.Float, default=0.05)
    def __init__(self, *args, **kwargs):
        if HAS_DREAM_MODELS:
            super().__init__(*args, **kwargs)
        else:
            self.name = kwargs.get('name', 'embodied_cycle')
            self.cycle_type = kwargs.get('cycle_type', 'embodied')
            self.duration_ms = kwargs.get('duration_ms', 1000)
    def process_embodied_memories(self, current_context: EmbodiedContext, memories: list) -> Dict[str, Any]:
        processed_memories = 0
        consolidation_updates = 0
        spatial_activations = 0
        for memory in memories:
            if not isinstance(memory, EmbodiedMemoryNode):
                continue
            if self.body_state_filter:
                stored_context = memory.get_embodied_context()
                if stored_context and stored_context.body_state.value != self.body_state_filter:
                    continue
            if self._is_in_spatial_scope(memory, current_context):
                memory.activate_embodied(current_context, self.embodied_consolidation_factor)
                spatial_activations += 1
                if hasattr(memory, 'spatial_activation'):
                    distance = self._calculate_spatial_distance(memory, current_context)
                    if distance > self.spatial_scope_radius:
                        decay_amount = min(0.1, distance / 100.0 * self.spatial_decay_factor)
                        memory.activation_level = max(0.0, memory.activation_level - decay_amount)
                consolidation_updates += 1
            processed_memories += 1
        return {'processed_memories': processed_memories, 'consolidation_updates': consolidation_updates, 'spatial_activations': spatial_activations, 'current_body_state': current_context.body_state.value, 'current_position': current_context.body_config.position, 'processing_time': time.time()}
    def _is_in_spatial_scope(self, memory: EmbodiedMemoryNode, current_context: EmbodiedContext) -> bool:
        if not hasattr(self, 'spatial_scope_radius'):
            return True
        stored_context = memory.get_embodied_context()
        if not stored_context:
            return True
        distance = self._calculate_spatial_distance(memory, current_context)
        return distance <= self.spatial_scope_radius
    def _calculate_spatial_distance(self, memory: EmbodiedMemoryNode, current_context: EmbodiedContext) -> float:
        stored_context = memory.get_embodied_context()
        if not stored_context:
            return 0.0
        pos1 = stored_context.body_config.position
        pos2 = current_context.body_config.position
        return ((pos1[0] - pos2[0]) ** 2 + (pos1[1] - pos2[1]) ** 2 + (pos1[2] - pos2[2]) ** 2) ** 0.5
def create_embodied_memory_node(content: str, memory_type: str, embodied_context: EmbodiedContext, **kwargs) -> EmbodiedMemoryNode:
    memory = EmbodiedMemoryNode(memory_type=memory_type, embodied_context=embodied_context, content=content, **kwargs)
    if embodied_context.emotional_state:
        avg_emotion = sum(embodied_context.emotional_state.values()) / len(embodied_context.emotional_state)
        memory.activation_level = min(1.0, avg_emotion)
    return memory
def create_embodied_processing_cycle(name: str, body_state_filter: Optional[str]=None, spatial_scope: Optional[tuple]=None, **kwargs) -> EmbodiedMemoryCycle:
    cycle = EmbodiedMemoryCycle(name=name, cycle_type='embodied', **kwargs)
    if body_state_filter:
        cycle.body_state_filter = body_state_filter
    if spatial_scope:
        cycle.spatial_scope_x, cycle.spatial_scope_y, cycle.spatial_scope_z, cycle.spatial_scope_radius = spatial_scope
    return cycle
def migrate_dream_memories_to_embodied(dream_memories: list) -> list:
    embodied_memories = []
    for memory in dream_memories:
        embodied_context = EmbodiedContext(body_state=BodyState.NEUTRAL, body_config=BodyConfiguration(), spatial_anchor=SpatialAnchor.ALLOCENTRIC, emotional_state=getattr(memory, 'emotional_state', {'neutral': 0.5}))
        embodied_memory = EmbodiedMemoryNode(memory_type=getattr(memory, 'memory_type', 'episodic'), embodied_context=embodied_context, content=getattr(memory, 'content', ''), activation_level=getattr(memory, 'activation_level', 0.0))
        embodied_memories.append(embodied_memory)
    return embodied_memories
def setup_embodied_dream_integration():
    if not HAS_DREAM_MODELS:
        print('Warning: echo.dream models not available, skipping integration setup')
        return
    try:
        db.create_all()
        cycles = [create_embodied_processing_cycle(name='embodied_consolidation', duration_ms=5000, embodied_consolidation_factor=0.1, description='Consolidates embodied memories based on current context'), create_embodied_processing_cycle(name='spatial_memory_decay', body_state_filter='moving', duration_ms=10000, spatial_decay_factor=0.05, description='Decays memories distant from current position while moving'), create_embodied_processing_cycle(name='emotional_memory_boost', duration_ms=2000, embodied_consolidation_factor=0.2, description='Boosts memories with similar emotional context')]
        for cycle in cycles:
            db.session.add(cycle)
        db.session.commit()
        print('Embodied memory integration setup complete')
    except Exception as e:
        print(f'Error setting up embodied dream integration: {e}')
if __name__ == '__main__':
    print('=== Embodied Memory Dream Extensions Demo ===')
    test_context = EmbodiedContext(body_state=BodyState.LEARNING, body_config=BodyConfiguration(position=(1, 2, 3)), spatial_anchor=SpatialAnchor.EGOCENTRIC, emotional_state={'curiosity': 0.8, 'focus': 0.7})
    memory = create_embodied_memory_node(content='Learning about embodied cognition in the library', memory_type='episodic', embodied_context=test_context)
    print('Created embodied memory node:')
    print(f'  Content: {memory.content}')
    print(f'  Body state: {memory.embodied_context.body_state}')
    print(f'  Position: {memory.embodied_context.body_config.position}')
    similar_context = EmbodiedContext(body_state=BodyState.LEARNING, body_config=BodyConfiguration(position=(1.5, 2.2, 3.1)), spatial_anchor=SpatialAnchor.EGOCENTRIC, emotional_state={'curiosity': 0.75, 'focus': 0.65})
    initial_activation = memory.activation_level
    memory.activate_embodied(similar_context, 0.3)
    print(f'Activation change: {initial_activation:.3f} -> {memory.activation_level:.3f}')
    cycle = create_embodied_processing_cycle(name='test_cycle', body_state_filter='learning', spatial_scope=(1, 2, 3, 5.0), duration_ms=1000)
    results = cycle.process_embodied_memories(similar_context, [memory])
    print(f'Processing results: {results}')
    print('=== Demo completed successfully ===')