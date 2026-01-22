import unittest
import tempfile
import logging
from pathlib import Path
from cognitive_architecture import CognitiveArchitecture
class TestEchoselfIntegration(unittest.TestCase):
    def setUp(self):
        logging.disable(logging.CRITICAL)
        self.cognitive_arch = CognitiveArchitecture()
    def tearDown(self):
        logging.disable(logging.NOTSET)
    def test_introspection_system_initialization(self):
        self.assertIsNotNone(self.cognitive_arch.echoself_introspection)
    def test_recursive_introspection_execution(self):
        prompt = self.cognitive_arch.perform_recursive_introspection(current_cognitive_load=0.6, recent_activity_level=0.4)
        self.assertIsNotNone(prompt)
        self.assertIsInstance(prompt, str)
        self.assertIn('DeepTreeEcho', prompt)
        self.assertIn('Repository Hypergraph Analysis', prompt)
    def test_introspection_with_automatic_calculation(self):
        prompt = self.cognitive_arch.perform_recursive_introspection()
        self.assertIsNotNone(prompt)
        self.assertIsInstance(prompt, str)
    def test_introspection_metrics_retrieval(self):
        self.cognitive_arch.perform_recursive_introspection(0.5, 0.3)
        metrics = self.cognitive_arch.get_introspection_metrics()
        self.assertIsInstance(metrics, dict)
        self.assertIn('total_decisions', metrics)
        self.assertIn('hypergraph_nodes', metrics)
    def test_adaptive_goal_generation_with_introspection(self):
        goals = self.cognitive_arch.adaptive_goal_generation_with_introspection()
        self.assertIsInstance(goals, list)
        self.assertGreater(len(goals), 0)
        introspection_goals = [g for g in goals if 'introspection' in g.description.lower() or 'hypergraph' in g.description.lower()]
        self.assertGreater(len(introspection_goals), 0)
    def test_cognitive_load_calculation(self):
        load = self.cognitive_arch._calculate_current_cognitive_load()
        self.assertIsInstance(load, float)
        self.assertGreaterEqual(load, 0.1)
        self.assertLessEqual(load, 0.9)
    def test_recent_activity_calculation(self):
        activity = self.cognitive_arch._calculate_recent_activity()
        self.assertIsInstance(activity, float)
        self.assertGreaterEqual(activity, 0.1)
        self.assertLessEqual(activity, 1.0)
    def test_introspection_memory_storage(self):
        initial_memory_count = len(self.cognitive_arch.memories)
        self.cognitive_arch.perform_recursive_introspection(0.5, 0.3)
        self.assertGreater(len(self.cognitive_arch.memories), initial_memory_count)
        introspection_memories = [m for m in self.cognitive_arch.memories.values() if 'introspection' in m.content.lower()]
        self.assertGreater(len(introspection_memories), 0)
    def test_export_introspection_data(self):
        with tempfile.NamedTemporaryFile(suffix='.json', delete=False) as tmp:
            tmp_path = tmp.name
        try:
            self.cognitive_arch.perform_recursive_introspection(0.5, 0.3)
            success = self.cognitive_arch.export_introspection_data(tmp_path)
            self.assertTrue(success)
            self.assertTrue(Path(tmp_path).exists())
        finally:
            Path(tmp_path).unlink(missing_ok=True)
class TestIntrospectionEnhancedBehavior(unittest.TestCase):
    def setUp(self):
        logging.disable(logging.CRITICAL)
        self.cognitive_arch = CognitiveArchitecture()
    def tearDown(self):
        logging.disable(logging.NOTSET)
    def test_introspection_influences_personality(self):
        self.cognitive_arch.personality_traits['curiosity'].current_value
        self.cognitive_arch.perform_recursive_introspection()
        goals = self.cognitive_arch.adaptive_goal_generation_with_introspection()
        exploration_goals = [g for g in goals if 'explore' in g.description.lower() or 'analyze' in g.description.lower()]
        self.assertGreater(len(exploration_goals), 0)
    def test_recursive_feedback_loop(self):
        initial_memory_count = len(self.cognitive_arch.memories)
        for i in range(3):
            prompt = self.cognitive_arch.perform_recursive_introspection()
            self.assertIsNotNone(prompt)
            goals = self.cognitive_arch.adaptive_goal_generation_with_introspection()
            self.assertGreater(len(goals), 0)
        final_memory_count = len(self.cognitive_arch.memories)
        self.assertGreater(final_memory_count, initial_memory_count)
    def test_attention_allocation_adaptation(self):
        metrics_1 = self.cognitive_arch.get_introspection_metrics()
        for load in [0.3, 0.7, 0.5, 0.9, 0.2]:
            self.cognitive_arch.perform_recursive_introspection(load, 0.5)
        metrics_2 = self.cognitive_arch.get_introspection_metrics()
        self.assertGreater(metrics_2.get('total_decisions', 0), metrics_1.get('total_decisions', 0))
if __name__ == '__main__':
    unittest.main()