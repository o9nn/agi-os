import pytest
import time
import numpy as np
from unittest.mock import Mock, patch
from pathlib import Path
import tempfile
import shutil
import sys
sys.path.append('.')
from embodied_memory_system import EmbodiedMemorySystem, EmbodiedMemory, EmbodiedContext, BodyConfiguration, BodyState, SpatialAnchor, create_embodied_memory_bridge
sys.path.append('../echo.dash')
try:
    from unified_echo_memory import MemoryType
except ImportError:
    from enum import Enum
    class MemoryType(Enum):
        EPISODIC = 'episodic'
        PROCEDURAL = 'procedural'
        EMOTIONAL = 'emotional'
        SEMANTIC = 'semantic'
class TestBodyConfiguration:
    def test_body_configuration_creation(self):
        config = BodyConfiguration()
        assert config.position == (0.0, 0.0, 0.0)
        assert config.orientation == (0.0, 0.0, 0.0, 1.0)
        assert isinstance(config.joint_angles, dict)
        assert isinstance(config.timestamp, float)
    def test_body_configuration_serialization(self):
        config = BodyConfiguration(position=(1.0, 2.0, 3.0), orientation=(0.0, 0.0, 0.707, 0.707), joint_angles={'shoulder': 45.0, 'elbow': 30.0}, velocity=(0.5, 0.0, 0.0))
        config_dict = config.to_dict()
        assert config_dict['position'] == (1.0, 2.0, 3.0)
        assert config_dict['joint_angles']['shoulder'] == 45.0
        restored_config = BodyConfiguration.from_dict(config_dict)
        assert restored_config.position == config.position
        assert restored_config.joint_angles == config.joint_angles
class TestEmbodiedContext:
    def test_embodied_context_creation(self):
        body_config = BodyConfiguration(position=(1, 2, 3))
        context = EmbodiedContext(body_state=BodyState.LEARNING, body_config=body_config, spatial_anchor=SpatialAnchor.EGOCENTRIC, emotional_state={'arousal': 0.7, 'valence': 0.5}, sensory_input={'vision': [1, 2, 3], 'audio': [0.5]}, motor_output={'arm_move': [0.2, 0.3]})
        assert context.body_state == BodyState.LEARNING
        assert context.body_config.position == (1, 2, 3)
        assert context.spatial_anchor == SpatialAnchor.EGOCENTRIC
        assert context.emotional_state['arousal'] == 0.7
        assert 'vision' in context.sensory_input
        assert 'arm_move' in context.motor_output
    def test_embodied_context_serialization(self):
        context = EmbodiedContext(body_state=BodyState.MOVING, body_config=BodyConfiguration(position=(5, 4, 3)), spatial_anchor=SpatialAnchor.ALLOCENTRIC, emotional_state={'stress': 0.3})
        context_dict = context.to_dict()
        assert context_dict['body_state'] == 'moving'
        assert context_dict['spatial_anchor'] == 'allocentric'
        assert context_dict['body_config']['position'] == (5, 4, 3)
        restored_context = EmbodiedContext.from_dict(context_dict)
        assert restored_context.body_state == BodyState.MOVING
        assert restored_context.spatial_anchor == SpatialAnchor.ALLOCENTRIC
        assert restored_context.body_config.position == (5, 4, 3)
class TestEmbodiedMemory:
    def setup_method(self):
        self.test_context = EmbodiedContext(body_state=BodyState.LEARNING, body_config=BodyConfiguration(position=(1, 1, 1)), spatial_anchor=SpatialAnchor.EGOCENTRIC, emotional_state={'arousal': 0.6, 'valence': 0.4})
    def test_embodied_memory_creation(self):
        memory = EmbodiedMemory(id='test_memory_001', content='Learning about embodied cognition', memory_type=MemoryType.EPISODIC, embodied_context=self.test_context)
        assert memory.id == 'test_memory_001'
        assert memory.memory_type == MemoryType.EPISODIC
        assert memory.embodied_context.body_state == BodyState.LEARNING
        assert memory.activation_level == 0.0
        assert memory.consolidation_level == 0.0
    def test_embodied_memory_access(self):
        memory = EmbodiedMemory(id='test_memory_002', content='Walking in the park', memory_type=MemoryType.EPISODIC, embodied_context=self.test_context)
        initial_access_count = memory.access_count
        initial_activation = memory.activation_level
        memory.access()
        assert memory.access_count == initial_access_count + 1
        assert memory.last_access_time > 0
        similar_context = EmbodiedContext(body_state=BodyState.LEARNING, body_config=BodyConfiguration(position=(1.2, 1.1, 1.0)), spatial_anchor=SpatialAnchor.EGOCENTRIC, emotional_state={'arousal': 0.65, 'valence': 0.35})
        memory.access(similar_context)
        assert memory.activation_level > initial_activation
    def test_embodied_similarity_calculation(self):
        memory = EmbodiedMemory(id='test_memory_003', content='Test content', memory_type=MemoryType.EPISODIC, embodied_context=self.test_context)
        identical_context = EmbodiedContext(body_state=BodyState.LEARNING, body_config=BodyConfiguration(position=(1, 1, 1)), spatial_anchor=SpatialAnchor.EGOCENTRIC, emotional_state={'arousal': 0.6, 'valence': 0.4})
        similarity = memory._calculate_embodied_similarity(identical_context)
        assert similarity > 0.8
        different_context = EmbodiedContext(body_state=BodyState.RESTING, body_config=BodyConfiguration(position=(10, 10, 10)), spatial_anchor=SpatialAnchor.PROPRIOCEPTIVE, emotional_state={'arousal': 0.1, 'valence': 0.9})
        similarity = memory._calculate_embodied_similarity(different_context)
        assert similarity < 0.5
    def test_distance_calculations(self):
        pos1 = (0, 0, 0)
        pos2 = (3, 4, 0)
        distance = EmbodiedMemory._euclidean_distance(pos1, pos2)
        assert abs(distance - 5.0) < 0.001
    def test_cosine_similarity(self):
        emotion1 = {'arousal': 0.8, 'valence': 0.6}
        emotion2 = {'arousal': 0.7, 'valence': 0.5}
        similarity = EmbodiedMemory._cosine_similarity(emotion1, emotion2)
        assert 0 <= similarity <= 1
        identical_similarity = EmbodiedMemory._cosine_similarity(emotion1, emotion1)
        assert abs(identical_similarity - 1.0) < 0.001
        empty_similarity = EmbodiedMemory._cosine_similarity({}, emotion1)
        assert empty_similarity == 0.0
class TestEmbodiedMemorySystem:
    def setup_method(self):
        self.temp_dir = tempfile.mkdtemp()
        self.system = EmbodiedMemorySystem(storage_dir=self.temp_dir, dtesn_integration=False)
    def teardown_method(self):
        shutil.rmtree(self.temp_dir, ignore_errors=True)
    def test_system_initialization(self):
        assert len(self.system.embodied_memories) == 0
        assert len(self.system.working_memory) == 0
        assert self.system.max_working_memory == 7
        assert self.system.current_context.body_state == BodyState.NEUTRAL
        assert Path(self.temp_dir).exists()
    def test_memory_creation(self):
        context = EmbodiedContext(body_state=BodyState.LEARNING, body_config=BodyConfiguration(position=(2, 3, 4)), spatial_anchor=SpatialAnchor.EGOCENTRIC, emotional_state={'curiosity': 0.8})
        memory_id = self.system.create_memory('Learning about robotics', MemoryType.EPISODIC, context)
        assert memory_id in self.system.embodied_memories
        memory = self.system.embodied_memories[memory_id]
        assert memory.content == 'Learning about robotics'
        assert memory.memory_type == MemoryType.EPISODIC
        assert memory.embodied_context.body_state == BodyState.LEARNING
        assert memory_id in self.system.body_state_index[BodyState.LEARNING]
        assert memory_id in self.system.working_memory
    def test_memory_retrieval_by_context(self):
        contexts = [EmbodiedContext(body_state=BodyState.LEARNING, body_config=BodyConfiguration(position=(0, 0, 0)), emotional_state={'focus': 0.9}), EmbodiedContext(body_state=BodyState.MOVING, body_config=BodyConfiguration(position=(5, 5, 0)), emotional_state={'energy': 0.7}), EmbodiedContext(body_state=BodyState.RESTING, body_config=BodyConfiguration(position=(1, 1, 0)), emotional_state={'calm': 0.8})]
        memory_ids = []
        for i, context in enumerate(contexts):
            mem_id = self.system.create_memory(f'Memory content {i}', MemoryType.EPISODIC, context)
            memory_ids.append(mem_id)
        learning_query = EmbodiedContext(body_state=BodyState.LEARNING, body_config=BodyConfiguration(position=(0.5, 0.5, 0)), emotional_state={'focus': 0.85})
        results = self.system.retrieve_memories(learning_query, max_results=3)
        assert len(results) == 3
        assert results[0].embodied_context.body_state == BodyState.LEARNING
    def test_spatial_memory_retrieval(self):
        positions = [(0, 0, 0), (2, 1, 0), (10, 10, 0), (1, 1, 1)]
        memory_ids = []
        for i, pos in enumerate(positions):
            context = EmbodiedContext(body_state=BodyState.ACTIVE, body_config=BodyConfiguration(position=pos), spatial_anchor=SpatialAnchor.ALLOCENTRIC)
            mem_id = self.system.create_memory(f'Memory at position {pos}', MemoryType.EPISODIC, context)
            memory_ids.append(mem_id)
        nearby_memories = self.system.get_spatial_memories((0, 0, 0), radius=3.0)
        assert len(nearby_memories) == 3
        distances = []
        for memory in nearby_memories:
            pos = memory.embodied_context.body_config.position
            dist = EmbodiedMemory._euclidean_distance((0, 0, 0), pos)
            distances.append(dist)
        assert distances == sorted(distances)
    def test_episodic_memory_retrieval(self):
        contexts = [(MemoryType.EPISODIC, BodyState.LEARNING), (MemoryType.PROCEDURAL, BodyState.LEARNING), (MemoryType.EPISODIC, BodyState.MOVING), (MemoryType.EPISODIC, BodyState.LEARNING)]
        memory_ids = []
        for i, (mem_type, body_state) in enumerate(contexts):
            context = EmbodiedContext(body_state=body_state, body_config=BodyConfiguration(position=(i, 0, 0)))
            mem_id = self.system.create_memory(f'Memory {i}', mem_type, context)
            memory_ids.append(mem_id)
            time.sleep(0.01)
        episodic_memories = self.system.get_episodic_memories()
        assert len(episodic_memories) == 3
        learning_episodic = self.system.get_episodic_memories(body_state=BodyState.LEARNING)
        assert len(learning_episodic) == 2
        for i in range(len(learning_episodic) - 1):
            assert learning_episodic[i].creation_time >= learning_episodic[i + 1].creation_time
    def test_body_state_updates(self):
        new_config = BodyConfiguration(position=(5, 5, 5), orientation=(0, 0, 0.707, 0.707), velocity=(1, 0, 0))
        self.system.update_body_state(new_config, BodyState.MOVING)
        assert self.system.current_context.body_state == BodyState.MOVING
        assert self.system.current_context.body_config.position == (5, 5, 5)
        assert self.system.current_context.body_config.velocity == (1, 0, 0)
    def test_emotional_state_updates(self):
        emotional_context = EmbodiedContext(body_state=BodyState.STRESSED, body_config=BodyConfiguration(), emotional_state={'stress': 0.8, 'anxiety': 0.7})
        memory_id = self.system.create_memory('Stressful situation memory', MemoryType.EMOTIONAL, emotional_context)
        initial_consolidation = self.system.embodied_memories[memory_id].consolidation_level
        self.system.update_emotional_state({'stress': 0.75, 'anxiety': 0.65})
        updated_consolidation = self.system.embodied_memories[memory_id].consolidation_level
        assert updated_consolidation >= initial_consolidation
    def test_working_memory_management(self):
        for i in range(10):
            context = EmbodiedContext(body_state=BodyState.ACTIVE, body_config=BodyConfiguration(position=(i, 0, 0)))
            self.system.create_memory(f'Memory {i}', MemoryType.EPISODIC, context)
        assert len(self.system.working_memory) <= self.system.max_working_memory
        assert len(self.system.working_memory) == 7
    def test_memory_persistence(self):
        contexts = [EmbodiedContext(body_state=BodyState.LEARNING, body_config=BodyConfiguration(position=(1, 2, 3))), EmbodiedContext(body_state=BodyState.RESTING, body_config=BodyConfiguration(position=(4, 5, 6)))]
        memory_ids = []
        for i, context in enumerate(contexts):
            mem_id = self.system.create_memory(f'Persistent memory {i}', MemoryType.EPISODIC, context)
            memory_ids.append(mem_id)
        self.system.save_memories('test_memories.json')
        new_system = EmbodiedMemorySystem(storage_dir=self.temp_dir, dtesn_integration=False)
        new_system.load_memories('test_memories.json')
        assert len(new_system.embodied_memories) == 2
        for mem_id in memory_ids:
            loaded_memories = [m for m in new_system.embodied_memories.values() if m.content in [f'Persistent memory {i}' for i in range(2)]]
            assert len(loaded_memories) == 2
    def test_system_statistics(self):
        memory_types = [MemoryType.EPISODIC, MemoryType.PROCEDURAL, MemoryType.EMOTIONAL]
        body_states = [BodyState.LEARNING, BodyState.MOVING, BodyState.RESTING]
        for i, (mem_type, body_state) in enumerate(zip(memory_types, body_states)):
            context = EmbodiedContext(body_state=body_state, body_config=BodyConfiguration(position=(i, i, 0)))
            self.system.create_memory(f'Stats test memory {i}', mem_type, context)
        stats = self.system.get_stats()
        assert stats['total_memories'] == 3
        assert stats['working_memory_size'] == 3
        assert len(stats['memory_types']) == 3
        assert len(stats['body_states']) == 3
        assert 'average_activation' in stats
        assert 'average_consolidation' in stats
        assert stats['dtesn_integration'] is False
class TestDTESNIntegration:
    @patch('embodied_memory_system.HAS_DTESN_CORE', True)
    @patch('embodied_memory_system.PSystemEvolutionEngine')
    @patch('embodied_memory_system.ESNReservoir')
    @patch('embodied_memory_system.BSeriesTreeClassifier')
    def test_dtesn_initialization(self, mock_classifier, mock_esn, mock_psystem):
        mock_psystem.return_value = Mock()
        mock_esn.return_value = Mock()
        mock_classifier.return_value = Mock()
        system = EmbodiedMemorySystem(dtesn_integration=True)
        assert system.dtesn_integration is True
        assert hasattr(system, 'p_system')
        assert hasattr(system, 'esn')
        assert hasattr(system, 'tree_classifier')
    def test_context_encoding(self):
        system = EmbodiedMemorySystem(dtesn_integration=False)
        context = EmbodiedContext(body_state=BodyState.LEARNING, body_config=BodyConfiguration(position=(1.0, 2.0, 3.0), orientation=(0, 0, 0.707, 0.707), velocity=(0.5, 0, 0)), spatial_anchor=SpatialAnchor.EGOCENTRIC, emotional_state={'arousal': 0.8, 'valence': 0.6})
        encoding = system._encode_embodied_context(context)
        assert isinstance(encoding, np.ndarray)
        assert encoding.shape == (64,)
        learning_idx = list(BodyState).index(BodyState.LEARNING)
        assert encoding[learning_idx] == 1.0
        assert encoding[8] == 0.01
        assert encoding[9] == 0.02
        assert encoding[10] == 0.03
class TestMemoryBridge:
    def test_memory_bridge_creation(self):
        mock_echo_system = Mock()
        mock_echo_system.nodes = {'node1': Mock(content='Test memory 1', memory_type=MemoryType.EPISODIC, creation_time=time.time(), salience=0.7), 'node2': Mock(content='Test memory 2', memory_type=MemoryType.PROCEDURAL, creation_time=time.time(), salience=0.5)}
        embodied_system = create_embodied_memory_bridge(mock_echo_system)
        assert isinstance(embodied_system, EmbodiedMemorySystem)
        migrated_memories = [m for m in embodied_system.embodied_memories.values() if m.content in ['Test memory 1', 'Test memory 2']]
        assert len(migrated_memories) == 2
class TestPerformanceConstraints:
    def test_memory_creation_performance(self):
        system = EmbodiedMemorySystem(dtesn_integration=False)
        context = EmbodiedContext(body_state=BodyState.ACTIVE, body_config=BodyConfiguration())
        start_time = time.time()
        memory_id = system.create_memory('Performance test', MemoryType.EPISODIC, context)
        end_time = time.time()
        creation_time = (end_time - start_time) * 1000
        assert creation_time < 10.0
        assert memory_id in system.embodied_memories
    def test_memory_retrieval_performance(self):
        system = EmbodiedMemorySystem(dtesn_integration=False)
        for i in range(100):
            context = EmbodiedContext(body_state=BodyState.ACTIVE, body_config=BodyConfiguration(position=(i % 10, i % 7, i % 5)))
            system.create_memory(f'Performance memory {i}', MemoryType.EPISODIC, context)
        query_context = EmbodiedContext(body_state=BodyState.ACTIVE, body_config=BodyConfiguration(position=(5, 3, 2)))
        start_time = time.time()
        results = system.retrieve_memories(query_context, max_results=10)
        end_time = time.time()
        retrieval_time = (end_time - start_time) * 1000
        assert retrieval_time < 100.0
        assert len(results) <= 10
class TestAcceptanceCriteria:
    def setup_method(self):
        self.system = EmbodiedMemorySystem(dtesn_integration=False)
    def test_episodic_memory_body_state_integration(self):
        learning_context = EmbodiedContext(body_state=BodyState.LEARNING, body_config=BodyConfiguration(position=(0, 0, 0)))
        moving_context = EmbodiedContext(body_state=BodyState.MOVING, body_config=BodyConfiguration(position=(1, 1, 1)))
        learning_memory_id = self.system.create_memory('Learning about neural networks', MemoryType.EPISODIC, learning_context)
        moving_memory_id = self.system.create_memory('Walking to the library', MemoryType.EPISODIC, moving_context)
        learning_memory = self.system.embodied_memories[learning_memory_id]
        moving_memory = self.system.embodied_memories[moving_memory_id]
        assert learning_memory.embodied_context.body_state == BodyState.LEARNING
        assert moving_memory.embodied_context.body_state == BodyState.MOVING
        query_context = EmbodiedContext(body_state=BodyState.LEARNING, body_config=BodyConfiguration(position=(0.5, 0.5, 0.5)))
        retrieved_memories = self.system.retrieve_memories(query_context)
        assert retrieved_memories[0].embodied_context.body_state == BodyState.LEARNING
        assert 'Learning about neural networks' in retrieved_memories[0].content
    def test_spatial_memory_body_position_anchoring(self):
        positions = [(0, 0, 0), (5, 3, 1), (2, 7, 4)]
        memory_ids = []
        for i, pos in enumerate(positions):
            context = EmbodiedContext(body_state=BodyState.ACTIVE, body_config=BodyConfiguration(position=pos), spatial_anchor=SpatialAnchor.ALLOCENTRIC)
            mem_id = self.system.create_memory(f'Memory at position {pos}', MemoryType.EPISODIC, context)
            memory_ids.append(mem_id)
        nearby_origin = self.system.get_spatial_memories((0, 0, 0), radius=2.0)
        nearby_second = self.system.get_spatial_memories((5, 3, 1), radius=2.0)
        assert len(nearby_origin) >= 1
        assert len(nearby_second) >= 1
        origin_memory = nearby_origin[0]
        assert origin_memory.embodied_context.body_config.position == (0, 0, 0)
        assert origin_memory.embodied_context.spatial_anchor == SpatialAnchor.ALLOCENTRIC
    def test_emotional_memory_body_sensation_linking(self):
        emotional_contexts = [EmbodiedContext(body_state=BodyState.STRESSED, body_config=BodyConfiguration(), emotional_state={'stress': 0.9, 'tension': 0.8}, sensory_input={'proprioception': {'muscle_tension': 0.8}}), EmbodiedContext(body_state=BodyState.RESTING, body_config=BodyConfiguration(), emotional_state={'calm': 0.9, 'relaxation': 0.8}, sensory_input={'proprioception': {'muscle_tension': 0.2}})]
        stress_memory_id = self.system.create_memory('Stressful presentation', MemoryType.EMOTIONAL, emotional_contexts[0])
        calm_memory_id = self.system.create_memory('Peaceful meditation', MemoryType.EMOTIONAL, emotional_contexts[1])
        stress_memory = self.system.embodied_memories[stress_memory_id]
        calm_memory = self.system.embodied_memories[calm_memory_id]
        assert stress_memory.embodied_context.emotional_state['stress'] == 0.9
        assert stress_memory.embodied_context.sensory_input['proprioception']['muscle_tension'] == 0.8
        assert calm_memory.embodied_context.emotional_state['calm'] == 0.9
        assert calm_memory.embodied_context.sensory_input['proprioception']['muscle_tension'] == 0.2
        initial_stress_consolidation = stress_memory.consolidation_level
        self.system.update_emotional_state({'stress': 0.85, 'anxiety': 0.7})
        updated_stress_consolidation = stress_memory.consolidation_level
        assert updated_stress_consolidation >= initial_stress_consolidation
    def test_embodied_context_influences_retrieval(self):
        contexts_and_contents = [(EmbodiedContext(body_state=BodyState.LEARNING, body_config=BodyConfiguration(position=(0, 0, 0)), emotional_state={'curiosity': 0.9}), 'Studying machine learning'), (EmbodiedContext(body_state=BodyState.MOVING, body_config=BodyConfiguration(position=(10, 5, 0)), emotional_state={'energy': 0.8}), 'Running in the park'), (EmbodiedContext(body_state=BodyState.INTERACTING, body_config=BodyConfiguration(position=(2, 1, 0)), emotional_state={'social': 0.7}), 'Talking with friends')]
        memory_ids = []
        for context, content in contexts_and_contents:
            mem_id = self.system.create_memory(content, MemoryType.EPISODIC, context)
            memory_ids.append(mem_id)
        learning_query = EmbodiedContext(body_state=BodyState.LEARNING, body_config=BodyConfiguration(position=(1, 0, 0)), emotional_state={'curiosity': 0.8})
        learning_results = self.system.retrieve_memories(learning_query, max_results=3)
        assert 'Studying machine learning' in learning_results[0].content
        moving_query = EmbodiedContext(body_state=BodyState.MOVING, body_config=BodyConfiguration(position=(9, 4, 0)), emotional_state={'energy': 0.7})
        moving_results = self.system.retrieve_memories(moving_query, max_results=3)
        assert 'Running in the park' in moving_results[0].content
        social_query = EmbodiedContext(body_state=BodyState.INTERACTING, body_config=BodyConfiguration(position=(2, 2, 0)), emotional_state={'social': 0.6})
        social_results = self.system.retrieve_memories(social_query, max_results=3)
        assert 'Talking with friends' in social_results[0].content
@pytest.fixture
def temp_storage():
    temp_dir = tempfile.mkdtemp()
    yield temp_dir
    shutil.rmtree(temp_dir, ignore_errors=True)
@pytest.fixture
def embodied_memory_system(temp_storage):
    return EmbodiedMemorySystem(storage_dir=temp_storage, dtesn_integration=False)
def test_full_system_integration(embodied_memory_system):
    system = embodied_memory_system
    contexts = [EmbodiedContext(body_state=BodyState.LEARNING, body_config=BodyConfiguration(position=(0, 0, 0)), emotional_state={'focus': 0.8}), EmbodiedContext(body_state=BodyState.MOVING, body_config=BodyConfiguration(position=(5, 3, 2)), emotional_state={'energy': 0.7})]
    memory_ids = []
    for i, context in enumerate(contexts):
        mem_id = system.create_memory(f'Integration test memory {i}', MemoryType.EPISODIC, context)
        memory_ids.append(mem_id)
    system.update_body_state(BodyConfiguration(position=(1, 1, 1)), BodyState.FOCUSED)
    system.update_emotional_state({'concentration': 0.9})
    results = system.retrieve_memories(max_results=5)
    assert len(results) == 2
    spatial_results = system.get_spatial_memories((0, 0, 0), radius=2.0)
    assert len(spatial_results) >= 1
    system.save_memories()
    new_system = EmbodiedMemorySystem(storage_dir=system.storage_dir, dtesn_integration=False)
    new_system.load_memories()
    assert len(new_system.embodied_memories) == 2
    stats = system.get_stats()
    assert stats['total_memories'] == 2
    assert 'average_activation' in stats
if __name__ == '__main__':
    pytest.main([__file__, '-v'])