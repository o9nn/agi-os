import time
import logging
from typing import Dict, List, Optional, Any
import sys
import os
from pathlib import Path
sys.path.append(os.path.join(os.path.dirname(__file__), '..', 'echo.dash'))
sys.path.append('.')
try:
    from cognitive_architecture import CognitiveArchitecture, MemoryType, Memory
    from unified_echo_memory import HypergraphMemory, MemoryNode
    HAS_DASH_INTEGRATION = True
except ImportError:
    HAS_DASH_INTEGRATION = False
    class CognitiveArchitecture:
        pass
    class MemoryType:
        pass
    class Memory:
        pass
    class HypergraphMemory:
        pass
    class MemoryNode:
        pass
from embodied_memory_system import EmbodiedMemorySystem, EmbodiedContext, EmbodiedMemory, BodyState, BodyConfiguration, SpatialAnchor
logger = logging.getLogger(__name__)
class EmbodiedCognitiveArchitecture(CognitiveArchitecture if HAS_DASH_INTEGRATION else object):
    def __init__(self, use_unified_memory: bool=False, enable_embodied: bool=True, **kwargs):
        if HAS_DASH_INTEGRATION:
            super().__init__(use_unified_memory=use_unified_memory, **kwargs)
        else:
            self.memories = {}
            self.goals = []
            self.active_goals = []
        self.enable_embodied = enable_embodied
        if self.enable_embodied:
            embodied_storage = Path.home() / '.deep_tree_echo' / 'embodied_memory'
            self.embodied_memory_system = EmbodiedMemorySystem(storage_dir=str(embodied_storage), dtesn_integration=True)
            self.current_embodied_context = EmbodiedContext(body_state=BodyState.NEUTRAL, body_config=BodyConfiguration(), spatial_anchor=SpatialAnchor.EGOCENTRIC)
            self._setup_memory_bridges()
            logger.info('Embodied cognitive architecture initialized')
        else:
            self.embodied_memory_system = None
    def _setup_memory_bridges(self):
        self._migrated_memories = set()
        self._embodied_stats = {'total_embodied_memories': 0, 'spatial_memories': 0, 'emotional_memories': 0, 'body_state_memories': {}}
    def create_memory(self, content: str, memory_type: str, embodied_context: Optional[EmbodiedContext]=None, **kwargs) -> str:
        if HAS_DASH_INTEGRATION:
            traditional_memory_id = super().create_memory(content, memory_type, **kwargs)
        else:
            traditional_memory_id = f'trad_{int(time.time())}'
        if self.enable_embodied and self.embodied_memory_system:
            if embodied_context is None:
                embodied_context = self.current_embodied_context
            if isinstance(memory_type, str):
                try:
                    from embodied_memory_system import MemoryType as EmbodiedMemoryType
                    embodied_type = EmbodiedMemoryType(memory_type.lower())
                except ValueError:
                    embodied_type = EmbodiedMemoryType.EPISODIC
            else:
                embodied_type = memory_type
            embodied_memory_id = self.embodied_memory_system.create_memory(content, embodied_type, embodied_context)
            self._link_memories(traditional_memory_id, embodied_memory_id)
            self._update_embodied_stats(embodied_type, embodied_context)
            logger.debug(f'Created linked memories: {traditional_memory_id} <-> {embodied_memory_id}')
        return traditional_memory_id
    def retrieve_memories(self, query: str='', memory_type: Optional[str]=None, embodied_context: Optional[EmbodiedContext]=None, max_results: int=10, use_embodied: bool=True) -> List[Dict]:
        results = []
        if HAS_DASH_INTEGRATION and hasattr(super(), 'retrieve_memories'):
            traditional_results = super().retrieve_memories(query, memory_type, max_results)
            results.extend(traditional_results)
        if self.enable_embodied and self.embodied_memory_system and use_embodied:
            query_context = embodied_context or self.current_embodied_context
            embodied_type = None
            if memory_type:
                try:
                    from embodied_memory_system import MemoryType as EmbodiedMemoryType
                    embodied_type = EmbodiedMemoryType(memory_type.lower())
                except ValueError:
                    pass
            embodied_results = self.embodied_memory_system.retrieve_memories(query_context, embodied_type, max_results)
            for embodied_memory in embodied_results:
                memory_dict = {'id': embodied_memory.id, 'content': embodied_memory.content, 'memory_type': embodied_memory.memory_type.value, 'activation_level': embodied_memory.activation_level, 'embodied_context': embodied_memory.embodied_context.to_dict(), 'body_state': embodied_memory.embodied_context.body_state.value, 'spatial_position': embodied_memory.embodied_context.body_config.position, 'emotional_state': embodied_memory.embodied_context.emotional_state, 'creation_time': embodied_memory.creation_time, 'last_access_time': embodied_memory.last_access_time, 'access_count': embodied_memory.access_count, 'source': 'embodied'}
                results.append(memory_dict)
        results.sort(key=lambda x: x.get('activation_level', 0), reverse=True)
        return results[:max_results]
    def update_embodied_state(self, body_config: BodyConfiguration, body_state: Optional[BodyState]=None, emotional_state: Optional[Dict[str, float]]=None):
        if not self.enable_embodied or not self.embodied_memory_system:
            logger.warning('Embodied features not enabled')
            return
        self.embodied_memory_system.update_body_state(body_config, body_state)
        self.current_embodied_context = self.embodied_memory_system.current_context
        if emotional_state:
            self.embodied_memory_system.update_emotional_state(emotional_state)
        logger.info(f'Updated embodied state: {body_state}, position: {body_config.position}')
    def get_spatial_context_memories(self, radius: float=5.0) -> List[Dict]:
        if not self.enable_embodied or not self.embodied_memory_system:
            return []
        current_pos = self.current_embodied_context.body_config.position
        spatial_memories = self.embodied_memory_system.get_spatial_memories(current_pos, radius)
        memory_dicts = []
        for memory in spatial_memories:
            memory_dict = {'id': memory.id, 'content': memory.content, 'spatial_distance': memory._euclidean_distance(current_pos, memory.embodied_context.body_config.position), 'body_state': memory.embodied_context.body_state.value, 'activation_level': memory.activation_level, 'source': 'spatial_embodied'}
            memory_dicts.append(memory_dict)
        return memory_dicts
    def get_body_state_memories(self, body_state: BodyState) -> List[Dict]:
        if not self.enable_embodied or not self.embodied_memory_system:
            return []
        matching_memories = []
        for memory in self.embodied_memory_system.embodied_memories.values():
            if memory.embodied_context.body_state == body_state:
                memory_dict = {'id': memory.id, 'content': memory.content, 'body_state': body_state.value, 'activation_level': memory.activation_level, 'consolidation_level': memory.consolidation_level, 'spatial_position': memory.embodied_context.body_config.position, 'source': 'body_state_embodied'}
                matching_memories.append(memory_dict)
        matching_memories.sort(key=lambda x: x['activation_level'], reverse=True)
        return matching_memories
    def get_emotional_memories(self, emotional_query: Dict[str, float], similarity_threshold: float=0.7) -> List[Dict]:
        if not self.enable_embodied or not self.embodied_memory_system:
            return []
        matching_memories = []
        for memory in self.embodied_memory_system.embodied_memories.values():
            if memory.embodied_context.emotional_state:
                similarity = EmbodiedMemory._cosine_similarity(emotional_query, memory.embodied_context.emotional_state)
                if similarity >= similarity_threshold:
                    memory_dict = {'id': memory.id, 'content': memory.content, 'emotional_similarity': similarity, 'emotional_state': memory.embodied_context.emotional_state, 'activation_level': memory.activation_level, 'source': 'emotional_embodied'}
                    matching_memories.append(memory_dict)
        matching_memories.sort(key=lambda x: x['emotional_similarity'], reverse=True)
        return matching_memories
    def consolidate_embodied_memories(self) -> Dict[str, int]:
        if not self.enable_embodied or not self.embodied_memory_system:
            return {}
        stats = {'total_processed': 0, 'spatial_consolidated': 0, 'emotional_consolidated': 0, 'body_state_consolidated': 0}
        current_context = self.current_embodied_context
        for memory in self.embodied_memory_system.embodied_memories.values():
            stats['total_processed'] += 1
            spatial_distance = EmbodiedMemory._euclidean_distance(current_context.body_config.position, memory.embodied_context.body_config.position)
            if spatial_distance < 3.0:
                memory.consolidation_level = min(1.0, memory.consolidation_level + 0.1)
                stats['spatial_consolidated'] += 1
            if current_context.emotional_state and memory.embodied_context.emotional_state:
                emotional_similarity = EmbodiedMemory._cosine_similarity(current_context.emotional_state, memory.embodied_context.emotional_state)
                if emotional_similarity > 0.8:
                    memory.consolidation_level = min(1.0, memory.consolidation_level + 0.15)
                    stats['emotional_consolidated'] += 1
            if memory.embodied_context.body_state == current_context.body_state:
                memory.consolidation_level = min(1.0, memory.consolidation_level + 0.05)
                stats['body_state_consolidated'] += 1
        logger.info(f'Embodied memory consolidation completed: {stats}')
        return stats
    def get_embodied_statistics(self) -> Dict[str, Any]:
        if not self.enable_embodied or not self.embodied_memory_system:
            return {'embodied_enabled': False}
        base_stats = self.embodied_memory_system.get_stats()
        extended_stats = base_stats.copy()
        extended_stats.update({'embodied_enabled': True, 'current_body_state': self.current_embodied_context.body_state.value, 'current_position': self.current_embodied_context.body_config.position, 'current_emotional_state': self.current_embodied_context.emotional_state, 'migrated_memories': len(self._migrated_memories), 'integration_type': 'cognitive_architecture'})
        positions = []
        for memory in self.embodied_memory_system.embodied_memories.values():
            positions.append(memory.embodied_context.body_config.position)
        if positions:
            import numpy as np
            positions_array = np.array(positions)
            extended_stats['spatial_distribution'] = {'center': tuple(np.mean(positions_array, axis=0)), 'spread': tuple(np.std(positions_array, axis=0)), 'min_bounds': tuple(np.min(positions_array, axis=0)), 'max_bounds': tuple(np.max(positions_array, axis=0))}
        return extended_stats
    def migrate_traditional_memories(self) -> int:
        if not self.enable_embodied or not self.embodied_memory_system:
            return 0
        migrated_count = 0
        if hasattr(self, 'memories'):
            for memory_id, memory in self.memories.items():
                if memory_id not in self._migrated_memories:
                    embodied_context = EmbodiedContext(body_state=BodyState.NEUTRAL, body_config=BodyConfiguration(), spatial_anchor=SpatialAnchor.ALLOCENTRIC, emotional_state={'neutral': 0.5})
                    try:
                        from embodied_memory_system import MemoryType as EmbodiedMemoryType
                        embodied_type = EmbodiedMemoryType(memory.memory_type.value)
                    except (ValueError, AttributeError):
                        embodied_type = EmbodiedMemoryType.EPISODIC
                    embodied_id = self.embodied_memory_system.create_memory(memory.content, embodied_type, embodied_context)
                    self._link_memories(memory_id, embodied_id)
                    self._migrated_memories.add(memory_id)
                    migrated_count += 1
        logger.info(f'Migrated {migrated_count} traditional memories to embodied format')
        return migrated_count
    def _link_memories(self, traditional_id: str, embodied_id: str):
        if not hasattr(self, '_memory_links'):
            self._memory_links = {}
        self._memory_links[traditional_id] = embodied_id
    def _update_embodied_stats(self, memory_type, embodied_context):
        self._embodied_stats['total_embodied_memories'] += 1
        if embodied_context.body_config.position != (0, 0, 0):
            self._embodied_stats['spatial_memories'] += 1
        if embodied_context.emotional_state:
            self._embodied_stats['emotional_memories'] += 1
        body_state = embodied_context.body_state.value
        if body_state not in self._embodied_stats['body_state_memories']:
            self._embodied_stats['body_state_memories'][body_state] = 0
        self._embodied_stats['body_state_memories'][body_state] += 1
def create_embodied_cognitive_architecture(**kwargs) -> EmbodiedCognitiveArchitecture:
    return EmbodiedCognitiveArchitecture(use_unified_memory=kwargs.get('use_unified_memory', True), enable_embodied=kwargs.get('enable_embodied', True))
def enhance_existing_architecture(architecture) -> EmbodiedCognitiveArchitecture:
    embodied_arch = EmbodiedCognitiveArchitecture(enable_embodied=True)
    if hasattr(architecture, 'memories'):
        embodied_arch.memories = architecture.memories
    if hasattr(architecture, 'goals'):
        embodied_arch.goals = architecture.goals
    if hasattr(architecture, 'personality_traits'):
        embodied_arch.personality_traits = architecture.personality_traits
    embodied_arch.migrate_traditional_memories()
    logger.info('Enhanced existing cognitive architecture with embodied features')
    return embodied_arch
if __name__ == '__main__':
    print('=== Embodied Cognitive Architecture Demo ===')
    arch = create_embodied_cognitive_architecture()
    print(f'Embodied features enabled: {arch.enable_embodied}')
    if arch.enable_embodied:
        contexts = [EmbodiedContext(body_state=BodyState.LEARNING, body_config=BodyConfiguration(position=(0, 0, 0)), spatial_anchor=SpatialAnchor.EGOCENTRIC, emotional_state={'curiosity': 0.9, 'focus': 0.8}), EmbodiedContext(body_state=BodyState.MOVING, body_config=BodyConfiguration(position=(5, 3, 1)), spatial_anchor=SpatialAnchor.ALLOCENTRIC, emotional_state={'energy': 0.7, 'excitement': 0.6})]
        memory_ids = []
        for i, context in enumerate(contexts):
            memory_id = arch.create_memory(f'Embodied memory example {i}', 'episodic', context)
            memory_ids.append(memory_id)
            print(f'Created memory: {memory_id}')
        new_config = BodyConfiguration(position=(2, 1, 0.5))
        arch.update_embodied_state(new_config, BodyState.FOCUSED)
        print(f'Updated embodied state to: {BodyState.FOCUSED}')
        memories = arch.retrieve_memories(use_embodied=True, max_results=5)
        print(f'Retrieved {len(memories)} memories with embodied context')
        for memory in memories:
            if 'body_state' in memory:
                print(f"  - {memory['content']} (body state: {memory['body_state']})")
        spatial_memories = arch.get_spatial_context_memories(radius=3.0)
        print(f'Spatial memories in radius 3.0: {len(spatial_memories)}')
        consolidation_stats = arch.consolidate_embodied_memories()
        print(f'Consolidation completed: {consolidation_stats}')
        stats = arch.get_embodied_statistics()
        print('Embodied memory statistics:')
        for key, value in stats.items():
            if not isinstance(value, dict):
                print(f'  {key}: {value}')
    print('=== Demo completed successfully ===')