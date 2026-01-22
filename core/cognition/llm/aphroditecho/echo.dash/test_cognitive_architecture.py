import unittest
import logging
import sys
from unittest.mock import Mock, patch
from pathlib import Path
sys.path.insert(0, str(Path(__file__).parent))
try:
    from cognitive_architecture import CognitiveArchitecture, MemoryType, Memory, Goal, PersonalityTrait
    COGNITIVE_ARCH_AVAILABLE = True
except ImportError as e:
    COGNITIVE_ARCH_AVAILABLE = False
    print(f'Warning: Could not import cognitive_architecture: {e}')
class TestCognitiveArchitecture(unittest.TestCase):
    def setUp(self):
        logging.getLogger().setLevel(logging.CRITICAL)
    def test_import_cognitive_architecture(self):
        if not COGNITIVE_ARCH_AVAILABLE:
            self.skipTest('cognitive_architecture module not available')
        self.assertTrue(COGNITIVE_ARCH_AVAILABLE)
    @unittest.skipIf(not COGNITIVE_ARCH_AVAILABLE, 'cognitive_architecture not available')
    def test_memory_type_enum(self):
        expected_types = ['DECLARATIVE', 'PROCEDURAL', 'EPISODIC', 'INTENTIONAL', 'EMOTIONAL']
        for memory_type in expected_types:
            self.assertTrue(hasattr(MemoryType, memory_type))
            self.assertEqual(getattr(MemoryType, memory_type).name, memory_type)
    @unittest.skipIf(not COGNITIVE_ARCH_AVAILABLE, 'cognitive_architecture not available')
    def test_memory_creation(self):
        memory = Memory(content='Test memory content', memory_type=MemoryType.DECLARATIVE, timestamp=1234567890.0, emotional_valence=0.5, importance=0.8)
        self.assertEqual(memory.content, 'Test memory content')
        self.assertEqual(memory.memory_type, MemoryType.DECLARATIVE)
        self.assertEqual(memory.timestamp, 1234567890.0)
        self.assertEqual(memory.emotional_valence, 0.5)
        self.assertEqual(memory.importance, 0.8)
        self.assertIsInstance(memory.associations, set)
        self.assertIsInstance(memory.context, dict)
    @unittest.skipIf(not COGNITIVE_ARCH_AVAILABLE, 'cognitive_architecture not available')
    def test_goal_creation(self):
        goal = Goal(description='Test goal', priority=0.9, deadline=None, status='pending', progress=0.0)
        self.assertEqual(goal.description, 'Test goal')
        self.assertEqual(goal.priority, 0.9)
        self.assertIsNone(goal.deadline)
        self.assertEqual(goal.status, 'pending')
        self.assertEqual(goal.progress, 0.0)
        self.assertIsInstance(goal.subgoals, list)
        self.assertIsInstance(goal.context, dict)
        self.assertIsInstance(goal.dependencies, list)
    @unittest.skipIf(not COGNITIVE_ARCH_AVAILABLE, 'cognitive_architecture not available')
    def test_personality_trait_creation(self):
        trait = PersonalityTrait('openness', 0.75)
        self.assertEqual(trait.name, 'openness')
        self.assertEqual(trait.base_value, 0.75)
        self.assertEqual(trait.current_value, 0.75)
    @unittest.skipIf(not COGNITIVE_ARCH_AVAILABLE, 'cognitive_architecture not available')
    @patch('cognitive_architecture.EchoselfIntrospection')
    def test_cognitive_architecture_creation(self, mock_introspection):
        mock_introspection.return_value = Mock()
        try:
            cognitive_arch = CognitiveArchitecture()
            self.assertIsNotNone(cognitive_arch)
        except Exception as e:
            if 'No module named' in str(e):
                self.skipTest(f'Dependencies not available: {e}')
            else:
                raise
    @unittest.skipIf(not COGNITIVE_ARCH_AVAILABLE, 'cognitive_architecture not available')
    def test_memory_associations(self):
        memory = Memory(content='Test memory with associations', memory_type=MemoryType.EPISODIC, timestamp=1234567890.0)
        memory.associations.add('related_concept_1')
        memory.associations.add('related_concept_2')
        self.assertEqual(len(memory.associations), 2)
        self.assertIn('related_concept_1', memory.associations)
        self.assertIn('related_concept_2', memory.associations)
    @unittest.skipIf(not COGNITIVE_ARCH_AVAILABLE, 'cognitive_architecture not available')
    def test_goal_hierarchy(self):
        main_goal = Goal(description='Main goal', priority=0.9, deadline=None)
        subgoal1 = Goal(description='Subgoal 1', priority=0.8, deadline=None)
        subgoal2 = Goal(description='Subgoal 2', priority=0.7, deadline=None)
        main_goal.subgoals.append(subgoal1)
        main_goal.subgoals.append(subgoal2)
        self.assertEqual(len(main_goal.subgoals), 2)
        self.assertEqual(main_goal.subgoals[0].description, 'Subgoal 1')
        self.assertEqual(main_goal.subgoals[1].description, 'Subgoal 2')
    @unittest.skipIf(not COGNITIVE_ARCH_AVAILABLE, 'cognitive_architecture not available')
    def test_goal_dependencies(self):
        goal = Goal(description='Dependent goal', priority=0.8, deadline=None)
        goal.dependencies.append('dependency_1')
        goal.dependencies.append('dependency_2')
        self.assertEqual(len(goal.dependencies), 2)
        self.assertIn('dependency_1', goal.dependencies)
        self.assertIn('dependency_2', goal.dependencies)
    @unittest.skipIf(not COGNITIVE_ARCH_AVAILABLE, 'cognitive_architecture not available')
    def test_memory_context(self):
        memory = Memory(content='Memory with context', memory_type=MemoryType.PROCEDURAL, timestamp=1234567890.0)
        memory.context['location'] = 'test_location'
        memory.context['mood'] = 'positive'
        memory.context['tags'] = ['important', 'work']
        self.assertEqual(memory.context['location'], 'test_location')
        self.assertEqual(memory.context['mood'], 'positive')
        self.assertEqual(memory.context['tags'], ['important', 'work'])
    @unittest.skipIf(not COGNITIVE_ARCH_AVAILABLE, 'cognitive_architecture not available')
    def test_all_memory_types_valid(self):
        for memory_type in MemoryType:
            memory = Memory(content=f'Test {memory_type.name} memory', memory_type=memory_type, timestamp=1234567890.0)
            self.assertEqual(memory.memory_type, memory_type)
            self.assertIn(memory_type.name.lower(), memory_type.value)
def main():
    unittest.main(verbosity=2)
if __name__ == '__main__':
    main()