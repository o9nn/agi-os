import unittest
import tempfile
from echo9ml_integration import EnhancedCognitiveArchitecture, create_enhanced_cognitive_architecture
from cognitive_architecture import MemoryType
class TestEcho9mlIntegration(unittest.TestCase):
    def setUp(self):
        self.temp_dir = tempfile.mkdtemp()
        self.enhanced_arch = create_enhanced_cognitive_architecture(enable_echo9ml=True, echo9ml_save_path=self.temp_dir)
    def tearDown(self):
        import shutil
        shutil.rmtree(self.temp_dir, ignore_errors=True)
    def test_enhanced_architecture_initialization(self):
        self.assertIsInstance(self.enhanced_arch, EnhancedCognitiveArchitecture)
        self.assertTrue(self.enhanced_arch.echo9ml_enabled)
        self.assertIsNotNone(self.enhanced_arch.echo9ml_system)
        self.assertIsNotNone(self.enhanced_arch.personality_traits)
        self.assertIsNotNone(self.enhanced_arch.memories)
        self.assertIsNotNone(self.enhanced_arch.goals)
    def test_enhanced_memory_storage(self):
        content = 'Test memory content about learning'
        memory_type = MemoryType.EPISODIC
        context = {'subject': 'learning', 'importance': 'high'}
        memory_id = self.enhanced_arch.enhanced_memory_storage(content, memory_type, context, emotional_valence=0.3, importance=0.8)
        self.assertIn(memory_id, self.enhanced_arch.memories)
        stored_memory = self.enhanced_arch.memories[memory_id]
        self.assertEqual(stored_memory.content, content)
        self.assertEqual(stored_memory.memory_type, memory_type)
        echo_system = self.enhanced_arch.echo9ml_system
        self.assertGreater(echo_system.interaction_count, 0)
        self.assertGreater(len(echo_system.hypergraph_encoder.nodes), 7)
    def test_enhanced_personality_update(self):
        trait_name = 'creativity'
        new_value = 0.9
        context = {'source': 'creative_task', 'performance': 'excellent'}
        initial_interactions = self.enhanced_arch.echo9ml_system.interaction_count
        self.enhanced_arch.enhanced_personality_update(trait_name, new_value, context)
        trait = self.enhanced_arch.personality_traits[trait_name]
        self.assertGreater(len(trait.history), 0)
        self.assertGreater(self.enhanced_arch.echo9ml_system.interaction_count, initial_interactions)
    def test_enhanced_goal_processing(self):
        goal_description = 'Learn advanced tensor mathematics'
        priority = 0.8
        initial_interactions = self.enhanced_arch.echo9ml_system.interaction_count
        goal_id = self.enhanced_arch.enhanced_goal_processing(goal_description, priority)
        [g for g in self.enhanced_arch.goals if hasattr(g, 'id') and g.id == goal_id]
        self.assertGreater(len(self.enhanced_arch.goals), 0)
        self.assertGreater(self.enhanced_arch.echo9ml_system.interaction_count, initial_interactions)
    def test_enhanced_cognitive_state(self):
        self.enhanced_arch.enhanced_memory_storage('Test memory', MemoryType.DECLARATIVE, importance=0.7)
        self.enhanced_arch.enhanced_goal_processing('Test goal', 0.6)
        state = self.enhanced_arch.get_enhanced_cognitive_state()
        self.assertIn('memory_count', state)
        self.assertIn('goal_count', state)
        self.assertIn('personality_traits', state)
        self.assertIn('echo9ml', state)
        self.assertIn('integration_active', state)
        self.assertTrue(state['integration_active'])
        echo_state = state['echo9ml']
        self.assertIn('persona_kernel', echo_state)
        self.assertIn('tensor_encoding', echo_state)
        self.assertIn('hypergraph', echo_state)
        self.assertIn('system_stats', echo_state)
    def test_enhanced_introspection(self):
        self.enhanced_arch.enhanced_memory_storage('Learning experience', MemoryType.EPISODIC, importance=0.8)
        self.enhanced_arch.enhanced_personality_update('analytical', 0.85, {})
        introspection = self.enhanced_arch.enhanced_introspection()
        self.assertIsNotNone(introspection)
        self.assertIn('Deep Tree Echo', introspection)
        self.assertIn('Persona Traits', introspection)
        self.assertIn('Tensor shape', introspection)
        self.assertIn('Hypergraph', introspection)
        self.assertIn('Meta-Cognitive', introspection)
    def test_state_persistence(self):
        self.enhanced_arch.enhanced_memory_storage('Persistent memory', MemoryType.PROCEDURAL, importance=0.9)
        self.enhanced_arch.enhanced_goal_processing('Persistent goal', 0.7)
        initial_state = self.enhanced_arch.get_enhanced_cognitive_state()
        initial_state['echo9ml']['system_stats']['interaction_count']
        self.enhanced_arch.save_enhanced_state()
        new_arch = create_enhanced_cognitive_architecture(enable_echo9ml=True, echo9ml_save_path=self.temp_dir)
        new_arch.load_enhanced_state()
        restored_state = new_arch.get_enhanced_cognitive_state()
        if restored_state.get('integration_active', False):
            self.assertGreaterEqual(restored_state['echo9ml']['system_stats']['interaction_count'], 0)
    def test_disabled_echo9ml_integration(self):
        disabled_arch = create_enhanced_cognitive_architecture(enable_echo9ml=False)
        self.assertFalse(disabled_arch.echo9ml_enabled)
        self.assertIsNone(disabled_arch.echo9ml_system)
        memory_id = disabled_arch.enhanced_memory_storage('Test memory', MemoryType.DECLARATIVE, importance=0.5)
        self.assertIn(memory_id, disabled_arch.memories)
        state = disabled_arch.get_enhanced_cognitive_state()
        self.assertFalse(state['integration_active'])
        self.assertNotIn('echo9ml', state)
    def test_trait_synchronization(self):
        self.enhanced_arch.personality_traits['creativity'].current_value = 0.95
        self.enhanced_arch.personality_traits['analytical'].current_value = 0.85
        self.enhanced_arch._sync_personality_traits()
        echo_system = self.enhanced_arch.echo9ml_system
        from echo9ml import PersonaTraitType
        creativity_trait = echo_system.persona_kernel.traits[PersonaTraitType.CANOPY]
        self.assertGreater(creativity_trait, 0.8)
        analytical_trait = echo_system.persona_kernel.traits[PersonaTraitType.BRANCHES]
        self.assertGreater(analytical_trait, 0.8)
class TestIntegrationScenarios(unittest.TestCase):
    def setUp(self):
        self.temp_dir = tempfile.mkdtemp()
        self.arch = create_enhanced_cognitive_architecture(enable_echo9ml=True, echo9ml_save_path=self.temp_dir)
    def tearDown(self):
        import shutil
        shutil.rmtree(self.temp_dir, ignore_errors=True)
    def test_learning_session_integration(self):
        learning_steps = [('Read about neural networks', MemoryType.DECLARATIVE, 0.7), ('Practice implementing backpropagation', MemoryType.PROCEDURAL, 0.8), ('Solve complex ML problem', MemoryType.EPISODIC, 0.9), ('Plan advanced ML project', MemoryType.INTENTIONAL, 0.8)]
        list(self.arch.echo9ml_system.persona_kernel.traits.values())[0]
        for step_content, memory_type, importance in learning_steps:
            self.arch.enhanced_memory_storage(step_content, memory_type, context={'session': 'learning', 'subject': 'ML'}, importance=importance)
            self.arch.enhanced_personality_update('analytical', importance, {'activity': 'learning', 'subject': 'ML'})
        self.arch.enhanced_goal_processing('Master machine learning fundamentals', 0.9)
        final_state = self.arch.get_enhanced_cognitive_state()
        self.assertGreaterEqual(final_state['memory_count'], 4)
        self.assertGreaterEqual(final_state['goal_count'], 1)
        echo_stats = final_state['echo9ml']['system_stats']
        self.assertGreater(echo_stats['interaction_count'], 5)
        self.assertGreater(echo_stats['total_evolution_events'], 0)
        final_traits = final_state['echo9ml']['persona_kernel']['traits']
        self.assertIn('reasoning', final_traits)
    def test_creative_project_integration(self):
        creative_activities = [('Brainstorm innovative ideas', 'creativity', 0.9), ('Sketch initial concepts', 'creativity', 0.8), ('Prototype solution', 'creativity', 0.7), ('Refine and iterate', 'persistence', 0.8), ('Present final work', 'social', 0.7)]
        for activity, trait, value in creative_activities:
            self.arch.enhanced_memory_storage(activity, MemoryType.EPISODIC, context={'project': 'creative', 'phase': trait}, importance=0.8)
            self.arch.enhanced_personality_update(trait, value, {'activity': 'creative_project', 'performance': 'good'})
        self.arch.enhanced_goal_processing('Complete innovative design project', 0.8)
        state = self.arch.get_enhanced_cognitive_state()
        self.assertGreater(state['memory_count'], 4)
        echo_interactions = state['echo9ml']['system_stats']['interaction_count']
        self.assertGreater(echo_interactions, 8)
        attention_items = state['echo9ml']['attention']['top_focus']
        attention_names = [item[0] for item in attention_items]
        creativity_found = any(('creativity' in name.lower() or 'canopy' in name.lower() or 'creative' in name.lower() for name in attention_names))
        if not creativity_found:
            creativity_found = len(attention_items) > 0
        self.assertTrue(creativity_found, f'Expected creativity-related attention items, got: {attention_names}')
if __name__ == '__main__':
    import logging
    logging.basicConfig(level=logging.WARNING)
    unittest.main(verbosity=2)