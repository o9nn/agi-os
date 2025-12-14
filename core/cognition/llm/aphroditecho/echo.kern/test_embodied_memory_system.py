#!/usr/bin/env python3
"""
Test Suite for Embodied Memory System

Comprehensive tests for Task 2.1.3 embodied memory implementation including:
- Episodic memory tied to body states
- Spatial memory anchored to body position
- Emotional memory linked to body sensations
- DTESN integration compliance
- Performance and real-time constraints
"""

import pytest
import time
import numpy as np
from unittest.mock import Mock, patch
from pathlib import Path
import tempfile
import shutil

# Import the embodied memory system
import sys
sys.path.append('.')
from embodied_memory_system import (
    EmbodiedMemorySystem, EmbodiedMemory, EmbodiedContext, BodyConfiguration,
    BodyState, SpatialAnchor, create_embodied_memory_bridge
)

# Mock DTESN imports for testing
sys.path.append('../echo.dash')
try:
    from unified_echo_memory import MemoryType
except ImportError:
    from enum import Enum
    class MemoryType(Enum):
        EPISODIC = "episodic"
        PROCEDURAL = "procedural"
        EMOTIONAL = "emotional"
        SEMANTIC = "semantic"

class TestBodyConfiguration:
    """Test body configuration and spatial representations"""
    
    def test_body_configuration_creation(self):
        """Test basic body configuration creation"""
        config = BodyConfiguration()
        assert config.position == (0.0, 0.0, 0.0)
        assert config.orientation == (0.0, 0.0, 0.0, 1.0)
        assert isinstance(config.joint_angles, dict)
        assert isinstance(config.timestamp, float)
    
    def test_body_configuration_serialization(self):
        """Test body configuration serialization and deserialization"""
        config = BodyConfiguration(
            position=(1.0, 2.0, 3.0),
            orientation=(0.0, 0.0, 0.707, 0.707),
            joint_angles={'shoulder': 45.0, 'elbow': 30.0},
            velocity=(0.5, 0.0, 0.0)
        )
        
        # Serialize to dict
        config_dict = config.to_dict()
        assert config_dict['position'] == (1.0, 2.0, 3.0)
        assert config_dict['joint_angles']['shoulder'] == 45.0
        
        # Deserialize back
        restored_config = BodyConfiguration.from_dict(config_dict)
        assert restored_config.position == config.position
        assert restored_config.joint_angles == config.joint_angles

class TestEmbodiedContext:
    """Test embodied context integration"""
    
    def test_embodied_context_creation(self):
        """Test embodied context creation with all components"""
        body_config = BodyConfiguration(position=(1, 2, 3))
        context = EmbodiedContext(
            body_state=BodyState.LEARNING,
            body_config=body_config,
            spatial_anchor=SpatialAnchor.EGOCENTRIC,
            emotional_state={'arousal': 0.7, 'valence': 0.5},
            sensory_input={'vision': [1, 2, 3], 'audio': [0.5]},
            motor_output={'arm_move': [0.2, 0.3]}
        )
        
        assert context.body_state == BodyState.LEARNING
        assert context.body_config.position == (1, 2, 3)
        assert context.spatial_anchor == SpatialAnchor.EGOCENTRIC
        assert context.emotional_state['arousal'] == 0.7
        assert 'vision' in context.sensory_input
        assert 'arm_move' in context.motor_output
    
    def test_embodied_context_serialization(self):
        """Test embodied context serialization with nested objects"""
        context = EmbodiedContext(
            body_state=BodyState.MOVING,
            body_config=BodyConfiguration(position=(5, 4, 3)),
            spatial_anchor=SpatialAnchor.ALLOCENTRIC,
            emotional_state={'stress': 0.3}
        )
        
        # Serialize
        context_dict = context.to_dict()
        assert context_dict['body_state'] == 'moving'
        assert context_dict['spatial_anchor'] == 'allocentric'
        assert context_dict['body_config']['position'] == (5, 4, 3)
        
        # Deserialize
        restored_context = EmbodiedContext.from_dict(context_dict)
        assert restored_context.body_state == BodyState.MOVING
        assert restored_context.spatial_anchor == SpatialAnchor.ALLOCENTRIC
        assert restored_context.body_config.position == (5, 4, 3)

class TestEmbodiedMemory:
    """Test embodied memory operations"""
    
    def setup_method(self):
        """Set up test fixtures"""
        self.test_context = EmbodiedContext(
            body_state=BodyState.LEARNING,
            body_config=BodyConfiguration(position=(1, 1, 1)),
            spatial_anchor=SpatialAnchor.EGOCENTRIC,
            emotional_state={'arousal': 0.6, 'valence': 0.4}
        )
    
    def test_embodied_memory_creation(self):
        """Test embodied memory creation with all fields"""
        memory = EmbodiedMemory(
            id="test_memory_001",
            content="Learning about embodied cognition",
            memory_type=MemoryType.EPISODIC,
            embodied_context=self.test_context
        )
        
        assert memory.id == "test_memory_001"
        assert memory.memory_type == MemoryType.EPISODIC
        assert memory.embodied_context.body_state == BodyState.LEARNING
        assert memory.activation_level == 0.0  # Default value
        assert memory.consolidation_level == 0.0  # Default value
    
    def test_embodied_memory_access(self):
        """Test memory access and activation updates"""
        memory = EmbodiedMemory(
            id="test_memory_002",
            content="Walking in the park",
            memory_type=MemoryType.EPISODIC,
            embodied_context=self.test_context
        )
        
        initial_access_count = memory.access_count
        initial_activation = memory.activation_level
        
        # Access without context
        memory.access()
        assert memory.access_count == initial_access_count + 1
        assert memory.last_access_time > 0
        
        # Access with similar context should boost activation
        similar_context = EmbodiedContext(
            body_state=BodyState.LEARNING,  # Same state
            body_config=BodyConfiguration(position=(1.2, 1.1, 1.0)),  # Similar position
            spatial_anchor=SpatialAnchor.EGOCENTRIC,
            emotional_state={'arousal': 0.65, 'valence': 0.35}  # Similar emotion
        )
        
        memory.access(similar_context)
        assert memory.activation_level > initial_activation
    
    def test_embodied_similarity_calculation(self):
        """Test embodied context similarity calculations"""
        memory = EmbodiedMemory(
            id="test_memory_003",
            content="Test content",
            memory_type=MemoryType.EPISODIC,
            embodied_context=self.test_context
        )
        
        # Identical context should have high similarity
        identical_context = EmbodiedContext(
            body_state=BodyState.LEARNING,
            body_config=BodyConfiguration(position=(1, 1, 1)),
            spatial_anchor=SpatialAnchor.EGOCENTRIC,
            emotional_state={'arousal': 0.6, 'valence': 0.4}
        )
        
        similarity = memory._calculate_embodied_similarity(identical_context)
        assert similarity > 0.8
        
        # Very different context should have low similarity
        different_context = EmbodiedContext(
            body_state=BodyState.RESTING,
            body_config=BodyConfiguration(position=(10, 10, 10)),
            spatial_anchor=SpatialAnchor.PROPRIOCEPTIVE,
            emotional_state={'arousal': 0.1, 'valence': 0.9}
        )
        
        similarity = memory._calculate_embodied_similarity(different_context)
        assert similarity < 0.5
    
    def test_distance_calculations(self):
        """Test spatial distance calculations"""
        pos1 = (0, 0, 0)
        pos2 = (3, 4, 0)  # Should be distance 5
        
        distance = EmbodiedMemory._euclidean_distance(pos1, pos2)
        assert abs(distance - 5.0) < 0.001
    
    def test_cosine_similarity(self):
        """Test emotional state cosine similarity"""
        emotion1 = {'arousal': 0.8, 'valence': 0.6}
        emotion2 = {'arousal': 0.7, 'valence': 0.5}
        
        similarity = EmbodiedMemory._cosine_similarity(emotion1, emotion2)
        assert 0 <= similarity <= 1
        
        # Identical emotions should have similarity 1
        identical_similarity = EmbodiedMemory._cosine_similarity(emotion1, emotion1)
        assert abs(identical_similarity - 1.0) < 0.001
        
        # Empty emotions should return 0
        empty_similarity = EmbodiedMemory._cosine_similarity({}, emotion1)
        assert empty_similarity == 0.0

class TestEmbodiedMemorySystem:
    """Test the complete embodied memory system"""
    
    def setup_method(self):
        """Set up test environment"""
        # Create temporary directory for test storage
        self.temp_dir = tempfile.mkdtemp()
        self.system = EmbodiedMemorySystem(
            storage_dir=self.temp_dir,
            dtesn_integration=False  # Disable for testing
        )
    
    def teardown_method(self):
        """Clean up test environment"""
        shutil.rmtree(self.temp_dir, ignore_errors=True)
    
    def test_system_initialization(self):
        """Test system initialization and configuration"""
        assert len(self.system.embodied_memories) == 0
        assert len(self.system.working_memory) == 0
        assert self.system.max_working_memory == 7  # Miller's Law
        assert self.system.current_context.body_state == BodyState.NEUTRAL
        assert Path(self.temp_dir).exists()
    
    def test_memory_creation(self):
        """Test memory creation with embodied context"""
        context = EmbodiedContext(
            body_state=BodyState.LEARNING,
            body_config=BodyConfiguration(position=(2, 3, 4)),
            spatial_anchor=SpatialAnchor.EGOCENTRIC,
            emotional_state={'curiosity': 0.8}
        )
        
        memory_id = self.system.create_memory(
            "Learning about robotics",
            MemoryType.EPISODIC,
            context
        )
        
        assert memory_id in self.system.embodied_memories
        memory = self.system.embodied_memories[memory_id]
        assert memory.content == "Learning about robotics"
        assert memory.memory_type == MemoryType.EPISODIC
        assert memory.embodied_context.body_state == BodyState.LEARNING
        
        # Check that indices were updated
        assert memory_id in self.system.body_state_index[BodyState.LEARNING]
        assert memory_id in self.system.working_memory
    
    def test_memory_retrieval_by_context(self):
        """Test context-based memory retrieval"""
        # Create memories with different contexts
        contexts = [
            EmbodiedContext(
                body_state=BodyState.LEARNING,
                body_config=BodyConfiguration(position=(0, 0, 0)),
                emotional_state={'focus': 0.9}
            ),
            EmbodiedContext(
                body_state=BodyState.MOVING,
                body_config=BodyConfiguration(position=(5, 5, 0)),
                emotional_state={'energy': 0.7}
            ),
            EmbodiedContext(
                body_state=BodyState.RESTING,
                body_config=BodyConfiguration(position=(1, 1, 0)),
                emotional_state={'calm': 0.8}
            )
        ]
        
        memory_ids = []
        for i, context in enumerate(contexts):
            mem_id = self.system.create_memory(
                f"Memory content {i}",
                MemoryType.EPISODIC,
                context
            )
            memory_ids.append(mem_id)
        
        # Query with learning context - should prioritize learning memory
        learning_query = EmbodiedContext(
            body_state=BodyState.LEARNING,
            body_config=BodyConfiguration(position=(0.5, 0.5, 0)),
            emotional_state={'focus': 0.85}
        )
        
        results = self.system.retrieve_memories(learning_query, max_results=3)
        assert len(results) == 3
        
        # First result should be the learning memory (most similar context)
        assert results[0].embodied_context.body_state == BodyState.LEARNING
    
    def test_spatial_memory_retrieval(self):
        """Test spatial-based memory retrieval"""
        # Create memories at different spatial locations
        positions = [(0, 0, 0), (2, 1, 0), (10, 10, 0), (1, 1, 1)]
        memory_ids = []
        
        for i, pos in enumerate(positions):
            context = EmbodiedContext(
                body_state=BodyState.ACTIVE,
                body_config=BodyConfiguration(position=pos),
                spatial_anchor=SpatialAnchor.ALLOCENTRIC
            )
            mem_id = self.system.create_memory(
                f"Memory at position {pos}",
                MemoryType.EPISODIC,
                context
            )
            memory_ids.append(mem_id)
        
        # Query for memories near origin
        nearby_memories = self.system.get_spatial_memories((0, 0, 0), radius=3.0)
        
        # Should find memories at (0,0,0), (2,1,0), and (1,1,1) but not (10,10,0)
        assert len(nearby_memories) == 3
        
        # Results should be sorted by proximity
        distances = []
        for memory in nearby_memories:
            pos = memory.embodied_context.body_config.position
            dist = EmbodiedMemory._euclidean_distance((0, 0, 0), pos)
            distances.append(dist)
        
        # Verify sorting (distances should be in ascending order)
        assert distances == sorted(distances)
    
    def test_episodic_memory_retrieval(self):
        """Test episodic memory retrieval with filtering"""
        # Create mix of memory types
        contexts = [
            (MemoryType.EPISODIC, BodyState.LEARNING),
            (MemoryType.PROCEDURAL, BodyState.LEARNING),
            (MemoryType.EPISODIC, BodyState.MOVING),
            (MemoryType.EPISODIC, BodyState.LEARNING)
        ]
        
        memory_ids = []
        for i, (mem_type, body_state) in enumerate(contexts):
            context = EmbodiedContext(
                body_state=body_state,
                body_config=BodyConfiguration(position=(i, 0, 0))
            )
            mem_id = self.system.create_memory(
                f"Memory {i}",
                mem_type,
                context
            )
            memory_ids.append(mem_id)
            
            # Add delay to ensure different timestamps
            time.sleep(0.01)
        
        # Get all episodic memories
        episodic_memories = self.system.get_episodic_memories()
        assert len(episodic_memories) == 3  # Only episodic memories
        
        # Get episodic memories while learning
        learning_episodic = self.system.get_episodic_memories(body_state=BodyState.LEARNING)
        assert len(learning_episodic) == 2  # Two episodic learning memories
        
        # Verify they're sorted by recency (newest first)
        for i in range(len(learning_episodic) - 1):
            assert learning_episodic[i].creation_time >= learning_episodic[i+1].creation_time
    
    def test_body_state_updates(self):
        """Test body state updates and their effects on memory"""
        
        # Update body configuration
        new_config = BodyConfiguration(
            position=(5, 5, 5),
            orientation=(0, 0, 0.707, 0.707),
            velocity=(1, 0, 0)
        )
        
        self.system.update_body_state(new_config, BodyState.MOVING)
        
        assert self.system.current_context.body_state == BodyState.MOVING
        assert self.system.current_context.body_config.position == (5, 5, 5)
        assert self.system.current_context.body_config.velocity == (1, 0, 0)
    
    def test_emotional_state_updates(self):
        """Test emotional state updates and memory consolidation"""
        # Create a memory with emotional content
        emotional_context = EmbodiedContext(
            body_state=BodyState.STRESSED,
            body_config=BodyConfiguration(),
            emotional_state={'stress': 0.8, 'anxiety': 0.7}
        )
        
        memory_id = self.system.create_memory(
            "Stressful situation memory",
            MemoryType.EMOTIONAL,
            emotional_context
        )
        
        initial_consolidation = self.system.embodied_memories[memory_id].consolidation_level
        
        # Update emotional state to similar emotions
        self.system.update_emotional_state({'stress': 0.75, 'anxiety': 0.65})
        
        # Check that similar emotional memories got consolidation boost
        updated_consolidation = self.system.embodied_memories[memory_id].consolidation_level
        assert updated_consolidation >= initial_consolidation
    
    def test_working_memory_management(self):
        """Test working memory capacity and management"""
        # Create more memories than working memory capacity
        for i in range(10):  # More than max_working_memory (7)
            context = EmbodiedContext(
                body_state=BodyState.ACTIVE,
                body_config=BodyConfiguration(position=(i, 0, 0))
            )
            self.system.create_memory(f"Memory {i}", MemoryType.EPISODIC, context)
        
        # Working memory should be limited to capacity
        assert len(self.system.working_memory) <= self.system.max_working_memory
        
        # Most recent memories should be in working memory
        assert len(self.system.working_memory) == 7
    
    def test_memory_persistence(self):
        """Test memory saving and loading"""
        # Create test memories
        contexts = [
            EmbodiedContext(
                body_state=BodyState.LEARNING,
                body_config=BodyConfiguration(position=(1, 2, 3))
            ),
            EmbodiedContext(
                body_state=BodyState.RESTING,
                body_config=BodyConfiguration(position=(4, 5, 6))
            )
        ]
        
        memory_ids = []
        for i, context in enumerate(contexts):
            mem_id = self.system.create_memory(
                f"Persistent memory {i}",
                MemoryType.EPISODIC,
                context
            )
            memory_ids.append(mem_id)
        
        # Save memories
        self.system.save_memories("test_memories.json")
        
        # Create new system and load memories
        new_system = EmbodiedMemorySystem(storage_dir=self.temp_dir, dtesn_integration=False)
        new_system.load_memories("test_memories.json")
        
        # Verify memories were loaded correctly
        assert len(new_system.embodied_memories) == 2
        for mem_id in memory_ids:
            # IDs might be different after loading, check by content
            loaded_memories = [m for m in new_system.embodied_memories.values() 
                             if m.content in [f"Persistent memory {i}" for i in range(2)]]
            assert len(loaded_memories) == 2
    
    def test_system_statistics(self):
        """Test system statistics and metrics"""
        # Create diverse memories
        memory_types = [MemoryType.EPISODIC, MemoryType.PROCEDURAL, MemoryType.EMOTIONAL]
        body_states = [BodyState.LEARNING, BodyState.MOVING, BodyState.RESTING]
        
        for i, (mem_type, body_state) in enumerate(zip(memory_types, body_states)):
            context = EmbodiedContext(
                body_state=body_state,
                body_config=BodyConfiguration(position=(i, i, 0))
            )
            self.system.create_memory(
                f"Stats test memory {i}",
                mem_type,
                context
            )
        
        stats = self.system.get_stats()
        
        assert stats['total_memories'] == 3
        assert stats['working_memory_size'] == 3
        assert len(stats['memory_types']) == 3
        assert len(stats['body_states']) == 3
        assert 'average_activation' in stats
        assert 'average_consolidation' in stats
        assert stats['dtesn_integration'] is False  # Disabled for testing

class TestDTESNIntegration:
    """Test DTESN integration functionality"""
    
    @patch('embodied_memory_system.HAS_DTESN_CORE', True)
    @patch('embodied_memory_system.PSystemEvolutionEngine')
    @patch('embodied_memory_system.ESNReservoir')
    @patch('embodied_memory_system.BSeriesTreeClassifier')
    def test_dtesn_initialization(self, mock_classifier, mock_esn, mock_psystem):
        """Test DTESN component initialization"""
        # Mock the DTESN components
        mock_psystem.return_value = Mock()
        mock_esn.return_value = Mock()
        mock_classifier.return_value = Mock()
        
        system = EmbodiedMemorySystem(dtesn_integration=True)
        
        assert system.dtesn_integration is True
        assert hasattr(system, 'p_system')
        assert hasattr(system, 'esn')
        assert hasattr(system, 'tree_classifier')
    
    def test_context_encoding(self):
        """Test embodied context encoding for neural processing"""
        system = EmbodiedMemorySystem(dtesn_integration=False)
        
        context = EmbodiedContext(
            body_state=BodyState.LEARNING,
            body_config=BodyConfiguration(
                position=(1.0, 2.0, 3.0),
                orientation=(0, 0, 0.707, 0.707),
                velocity=(0.5, 0, 0)
            ),
            spatial_anchor=SpatialAnchor.EGOCENTRIC,
            emotional_state={'arousal': 0.8, 'valence': 0.6}
        )
        
        encoding = system._encode_embodied_context(context)
        
        assert isinstance(encoding, np.ndarray)
        assert encoding.shape == (64,)  # Expected input size for ESN
        
        # Check that body state is encoded (one-hot)
        learning_idx = list(BodyState).index(BodyState.LEARNING)
        assert encoding[learning_idx] == 1.0
        
        # Check spatial encoding
        assert encoding[8] == 0.01   # x position normalized
        assert encoding[9] == 0.02   # y position normalized
        assert encoding[10] == 0.03  # z position normalized

class TestMemoryBridge:
    """Test integration bridge with existing Echo systems"""
    
    def test_memory_bridge_creation(self):
        """Test creating bridge with existing memory system"""
        # Mock existing Echo memory system
        mock_echo_system = Mock()
        mock_echo_system.nodes = {
            'node1': Mock(
                content='Test memory 1',
                memory_type=MemoryType.EPISODIC,
                creation_time=time.time(),
                salience=0.7
            ),
            'node2': Mock(
                content='Test memory 2',
                memory_type=MemoryType.PROCEDURAL,
                creation_time=time.time(),
                salience=0.5
            )
        }
        
        # Create bridge
        embodied_system = create_embodied_memory_bridge(mock_echo_system)
        
        assert isinstance(embodied_system, EmbodiedMemorySystem)
        
        # Check that memories were migrated
        migrated_memories = [m for m in embodied_system.embodied_memories.values() 
                           if m.content in ['Test memory 1', 'Test memory 2']]
        assert len(migrated_memories) == 2

class TestPerformanceConstraints:
    """Test real-time performance constraints"""
    
    def test_memory_creation_performance(self):
        """Test that memory creation meets timing constraints"""
        system = EmbodiedMemorySystem(dtesn_integration=False)
        
        context = EmbodiedContext(
            body_state=BodyState.ACTIVE,
            body_config=BodyConfiguration()
        )
        
        # Time memory creation (should be < 1ms for single memory)
        start_time = time.time()
        memory_id = system.create_memory("Performance test", MemoryType.EPISODIC, context)
        end_time = time.time()
        
        creation_time = (end_time - start_time) * 1000  # Convert to milliseconds
        assert creation_time < 10.0  # Should be under 10ms for single memory
        assert memory_id in system.embodied_memories
    
    def test_memory_retrieval_performance(self):
        """Test that memory retrieval meets timing constraints"""
        system = EmbodiedMemorySystem(dtesn_integration=False)
        
        # Create moderate number of memories
        for i in range(100):
            context = EmbodiedContext(
                body_state=BodyState.ACTIVE,
                body_config=BodyConfiguration(position=(i % 10, i % 7, i % 5))
            )
            system.create_memory(f"Performance memory {i}", MemoryType.EPISODIC, context)
        
        # Time retrieval operation
        query_context = EmbodiedContext(
            body_state=BodyState.ACTIVE,
            body_config=BodyConfiguration(position=(5, 3, 2))
        )
        
        start_time = time.time()
        results = system.retrieve_memories(query_context, max_results=10)
        end_time = time.time()
        
        retrieval_time = (end_time - start_time) * 1000  # Convert to milliseconds
        assert retrieval_time < 100.0  # Should be under 100ms for 100 memories
        assert len(results) <= 10

class TestAcceptanceCriteria:
    """Test acceptance criteria for Task 2.1.3"""
    
    def setup_method(self):
        """Set up test system"""
        self.system = EmbodiedMemorySystem(dtesn_integration=False)
    
    def test_episodic_memory_body_state_integration(self):
        """
        Acceptance Criteria: Episodic memory tied to body states
        """
        # Create episodic memories in different body states
        learning_context = EmbodiedContext(
            body_state=BodyState.LEARNING,
            body_config=BodyConfiguration(position=(0, 0, 0))
        )
        moving_context = EmbodiedContext(
            body_state=BodyState.MOVING,
            body_config=BodyConfiguration(position=(1, 1, 1))
        )
        
        learning_memory_id = self.system.create_memory(
            "Learning about neural networks",
            MemoryType.EPISODIC,
            learning_context
        )
        moving_memory_id = self.system.create_memory(
            "Walking to the library",
            MemoryType.EPISODIC,
            moving_context
        )
        
        # Verify memories are tied to body states
        learning_memory = self.system.embodied_memories[learning_memory_id]
        moving_memory = self.system.embodied_memories[moving_memory_id]
        
        assert learning_memory.embodied_context.body_state == BodyState.LEARNING
        assert moving_memory.embodied_context.body_state == BodyState.MOVING
        
        # Test retrieval influenced by body state
        query_context = EmbodiedContext(
            body_state=BodyState.LEARNING,
            body_config=BodyConfiguration(position=(0.5, 0.5, 0.5))
        )
        
        retrieved_memories = self.system.retrieve_memories(query_context)
        
        # Learning memory should be retrieved first due to body state match
        assert retrieved_memories[0].embodied_context.body_state == BodyState.LEARNING
        assert "Learning about neural networks" in retrieved_memories[0].content
    
    def test_spatial_memory_body_position_anchoring(self):
        """
        Acceptance Criteria: Spatial memory anchored to body position
        """
        # Create memories at specific body positions
        positions = [(0, 0, 0), (5, 3, 1), (2, 7, 4)]
        memory_ids = []
        
        for i, pos in enumerate(positions):
            context = EmbodiedContext(
                body_state=BodyState.ACTIVE,
                body_config=BodyConfiguration(position=pos),
                spatial_anchor=SpatialAnchor.ALLOCENTRIC  # Absolute positioning
            )
            mem_id = self.system.create_memory(
                f"Memory at position {pos}",
                MemoryType.EPISODIC,
                context
            )
            memory_ids.append(mem_id)
        
        # Test spatial retrieval around specific positions
        nearby_origin = self.system.get_spatial_memories((0, 0, 0), radius=2.0)
        nearby_second = self.system.get_spatial_memories((5, 3, 1), radius=2.0)
        
        # Should find position-specific memories
        assert len(nearby_origin) >= 1
        assert len(nearby_second) >= 1
        
        # Verify correct spatial anchoring
        origin_memory = nearby_origin[0]
        assert origin_memory.embodied_context.body_config.position == (0, 0, 0)
        assert origin_memory.embodied_context.spatial_anchor == SpatialAnchor.ALLOCENTRIC
    
    def test_emotional_memory_body_sensation_linking(self):
        """
        Acceptance Criteria: Emotional memory linked to body sensations
        """
        # Create emotional memories with different body sensations
        emotional_contexts = [
            EmbodiedContext(
                body_state=BodyState.STRESSED,
                body_config=BodyConfiguration(),
                emotional_state={'stress': 0.9, 'tension': 0.8},
                sensory_input={'proprioception': {'muscle_tension': 0.8}}
            ),
            EmbodiedContext(
                body_state=BodyState.RESTING,
                body_config=BodyConfiguration(),
                emotional_state={'calm': 0.9, 'relaxation': 0.8},
                sensory_input={'proprioception': {'muscle_tension': 0.2}}
            )
        ]
        
        stress_memory_id = self.system.create_memory(
            "Stressful presentation",
            MemoryType.EMOTIONAL,
            emotional_contexts[0]
        )
        calm_memory_id = self.system.create_memory(
            "Peaceful meditation",
            MemoryType.EMOTIONAL,
            emotional_contexts[1]
        )
        
        # Verify emotional memories are linked to body sensations
        stress_memory = self.system.embodied_memories[stress_memory_id]
        calm_memory = self.system.embodied_memories[calm_memory_id]
        
        assert stress_memory.embodied_context.emotional_state['stress'] == 0.9
        assert stress_memory.embodied_context.sensory_input['proprioception']['muscle_tension'] == 0.8
        assert calm_memory.embodied_context.emotional_state['calm'] == 0.9
        assert calm_memory.embodied_context.sensory_input['proprioception']['muscle_tension'] == 0.2
        
        # Test emotional state influence on memory consolidation
        initial_stress_consolidation = stress_memory.consolidation_level
        
        # Update to similar emotional state
        self.system.update_emotional_state({'stress': 0.85, 'anxiety': 0.7})
        
        # Stress memory should get consolidation boost
        updated_stress_consolidation = stress_memory.consolidation_level
        assert updated_stress_consolidation >= initial_stress_consolidation
    
    def test_embodied_context_influences_retrieval(self):
        """
        Acceptance Criteria: Memory retrieval influenced by embodied context
        """
        # Create memories with diverse embodied contexts
        contexts_and_contents = [
            (EmbodiedContext(
                body_state=BodyState.LEARNING,
                body_config=BodyConfiguration(position=(0, 0, 0)),
                emotional_state={'curiosity': 0.9}
            ), "Studying machine learning"),
            
            (EmbodiedContext(
                body_state=BodyState.MOVING,
                body_config=BodyConfiguration(position=(10, 5, 0)),
                emotional_state={'energy': 0.8}
            ), "Running in the park"),
            
            (EmbodiedContext(
                body_state=BodyState.INTERACTING,
                body_config=BodyConfiguration(position=(2, 1, 0)),
                emotional_state={'social': 0.7}
            ), "Talking with friends")
        ]
        
        memory_ids = []
        for context, content in contexts_and_contents:
            mem_id = self.system.create_memory(content, MemoryType.EPISODIC, context)
            memory_ids.append(mem_id)
        
        # Test retrieval with different query contexts
        
        # Query 1: Similar to learning context
        learning_query = EmbodiedContext(
            body_state=BodyState.LEARNING,
            body_config=BodyConfiguration(position=(1, 0, 0)),
            emotional_state={'curiosity': 0.8}
        )
        
        learning_results = self.system.retrieve_memories(learning_query, max_results=3)
        assert "Studying machine learning" in learning_results[0].content
        
        # Query 2: Similar to moving context  
        moving_query = EmbodiedContext(
            body_state=BodyState.MOVING,
            body_config=BodyConfiguration(position=(9, 4, 0)),
            emotional_state={'energy': 0.7}
        )
        
        moving_results = self.system.retrieve_memories(moving_query, max_results=3)
        assert "Running in the park" in moving_results[0].content
        
        # Query 3: Similar to social context
        social_query = EmbodiedContext(
            body_state=BodyState.INTERACTING,
            body_config=BodyConfiguration(position=(2, 2, 0)),
            emotional_state={'social': 0.6}
        )
        
        social_results = self.system.retrieve_memories(social_query, max_results=3)
        assert "Talking with friends" in social_results[0].content

# Integration test with pytest fixtures
@pytest.fixture
def temp_storage():
    """Provide temporary storage directory for tests"""
    temp_dir = tempfile.mkdtemp()
    yield temp_dir
    shutil.rmtree(temp_dir, ignore_errors=True)

@pytest.fixture
def embodied_memory_system(temp_storage):
    """Provide configured embodied memory system"""
    return EmbodiedMemorySystem(storage_dir=temp_storage, dtesn_integration=False)

def test_full_system_integration(embodied_memory_system):
    """Integration test for complete system functionality"""
    system = embodied_memory_system
    
    # Test complete workflow: create -> update -> retrieve -> save -> load
    
    # 1. Create diverse memories
    contexts = [
        EmbodiedContext(
            body_state=BodyState.LEARNING,
            body_config=BodyConfiguration(position=(0, 0, 0)),
            emotional_state={'focus': 0.8}
        ),
        EmbodiedContext(
            body_state=BodyState.MOVING,
            body_config=BodyConfiguration(position=(5, 3, 2)),
            emotional_state={'energy': 0.7}
        )
    ]
    
    memory_ids = []
    for i, context in enumerate(contexts):
        mem_id = system.create_memory(
            f"Integration test memory {i}",
            MemoryType.EPISODIC,
            context
        )
        memory_ids.append(mem_id)
    
    # 2. Update system state
    system.update_body_state(BodyConfiguration(position=(1, 1, 1)), BodyState.FOCUSED)
    system.update_emotional_state({'concentration': 0.9})
    
    # 3. Retrieve memories
    results = system.retrieve_memories(max_results=5)
    assert len(results) == 2
    
    # 4. Test spatial retrieval
    spatial_results = system.get_spatial_memories((0, 0, 0), radius=2.0)
    assert len(spatial_results) >= 1
    
    # 5. Test persistence
    system.save_memories()
    
    # 6. Load in new system instance
    new_system = EmbodiedMemorySystem(storage_dir=system.storage_dir, dtesn_integration=False)
    new_system.load_memories()
    
    assert len(new_system.embodied_memories) == 2
    
    # 7. Verify statistics
    stats = system.get_stats()
    assert stats['total_memories'] == 2
    assert 'average_activation' in stats

if __name__ == "__main__":
    # Run tests
    pytest.main([__file__, "-v"])