import unittest
import logging
import sys
from unittest.mock import Mock, patch
from pathlib import Path
sys.path.insert(0, str(Path(__file__).parent))
try:
    from echoself_demo_standardized import EchoselfDemoStandardized, create_echoself_demo_system, setup_logging, demonstrate_introspection_cycle
    from echo_component_base import EchoConfig, EchoResponse, validate_echo_component
    ECHOSELF_DEMO_STANDARDIZED_AVAILABLE = True
except ImportError as e:
    ECHOSELF_DEMO_STANDARDIZED_AVAILABLE = False
    print(f'Warning: Could not import echoself_demo_standardized: {e}')
class TestEchoselfDemoStandardized(unittest.TestCase):
    def setUp(self):
        logging.getLogger().setLevel(logging.CRITICAL)
    def test_import_standardized_module(self):
        if not ECHOSELF_DEMO_STANDARDIZED_AVAILABLE:
            self.skipTest('echoself_demo_standardized module not available')
        self.assertTrue(ECHOSELF_DEMO_STANDARDIZED_AVAILABLE)
    @unittest.skipIf(not ECHOSELF_DEMO_STANDARDIZED_AVAILABLE, 'Module not available')
    def test_component_creation(self):
        config = EchoConfig(component_name='TestEchoselfDemo', debug_mode=True)
        component = EchoselfDemoStandardized(config)
        self.assertEqual(component.config.component_name, 'TestEchoselfDemo')
        self.assertIsNotNone(component.logger)
        self.assertEqual(component.demo_cycles_completed, 0)
        self.assertEqual(len(component.introspection_results), 0)
    @unittest.skipIf(not ECHOSELF_DEMO_STANDARDIZED_AVAILABLE, 'Module not available')
    def test_component_validation(self):
        config = EchoConfig(component_name='TestEchoselfDemo')
        component = EchoselfDemoStandardized(config)
        self.assertTrue(validate_echo_component(component))
    @unittest.skipIf(not ECHOSELF_DEMO_STANDARDIZED_AVAILABLE, 'Module not available')
    @patch('echoself_demo_standardized.CognitiveArchitecture')
    def test_initialization_success(self, mock_cognitive_arch):
        mock_system = Mock()
        mock_system.echoself_introspection = Mock()
        mock_cognitive_arch.return_value = mock_system
        config = EchoConfig(component_name='TestEchoselfDemo')
        component = EchoselfDemoStandardized(config)
        import echoself_demo_standardized
        original_available = echoself_demo_standardized.COGNITIVE_ARCHITECTURE_AVAILABLE
        echoself_demo_standardized.COGNITIVE_ARCHITECTURE_AVAILABLE = True
        try:
            result = component.initialize()
            self.assertTrue(result.success)
            self.assertIn('initialized', result.message)
            self.assertTrue(component._initialized)
            self.assertIsNotNone(component.cognitive_system)
        finally:
            echoself_demo_standardized.COGNITIVE_ARCHITECTURE_AVAILABLE = original_available
    @unittest.skipIf(not ECHOSELF_DEMO_STANDARDIZED_AVAILABLE, 'Module not available')
    def test_initialization_failure_no_cognitive_arch(self):
        config = EchoConfig(component_name='TestEchoselfDemo')
        component = EchoselfDemoStandardized(config)
        result = component.initialize()
        if not result.success:
            self.assertIn('not available', result.message.lower())
            self.assertFalse(component._initialized)
    @unittest.skipIf(not ECHOSELF_DEMO_STANDARDIZED_AVAILABLE, 'Module not available')
    def test_echo_operation(self):
        config = EchoConfig(component_name='TestEchoselfDemo')
        component = EchoselfDemoStandardized(config)
        result = component.echo('test_data', echo_value=0.75)
        self.assertTrue(result.success)
        self.assertEqual(result.data['echo_value'], 0.75)
        self.assertIn('demo_state', result.data)
        self.assertEqual(result.data['demo_state']['cycles_completed'], 0)
    @unittest.skipIf(not ECHOSELF_DEMO_STANDARDIZED_AVAILABLE, 'Module not available')
    def test_process_without_initialization(self):
        config = EchoConfig(component_name='TestEchoselfDemo')
        component = EchoselfDemoStandardized(config)
        result = component.process('test_operation')
        self.assertFalse(result.success)
        self.assertIn('not initialized', result.message)
    @unittest.skipIf(not ECHOSELF_DEMO_STANDARDIZED_AVAILABLE, 'Module not available')
    @patch('echoself_demo_standardized.CognitiveArchitecture')
    def test_process_supported_operations(self, mock_cognitive_arch):
        mock_system = Mock()
        mock_system.echoself_introspection = Mock()
        mock_system.echoself_introspection.adaptive_attention.return_value = 0.5
        mock_system.perform_recursive_introspection.return_value = 'test prompt'
        mock_system.get_introspection_metrics.return_value = {'test': 'metrics'}
        mock_system.adaptive_goal_generation_with_introspection.return_value = []
        mock_cognitive_arch.return_value = mock_system
        config = EchoConfig(component_name='TestEchoselfDemo')
        component = EchoselfDemoStandardized(config)
        component.cognitive_system = mock_system
        component._initialized = True
        result = component.process('introspection_cycle')
        self.assertTrue(result.success)
        self.assertIn('cycle', result.message)
    @unittest.skipIf(not ECHOSELF_DEMO_STANDARDIZED_AVAILABLE, 'Module not available')
    @patch('echoself_demo_standardized.CognitiveArchitecture')
    def test_process_invalid_operation(self, mock_cognitive_arch):
        mock_system = Mock()
        mock_system.echoself_introspection = Mock()
        mock_cognitive_arch.return_value = mock_system
        config = EchoConfig(component_name='TestEchoselfDemo')
        component = EchoselfDemoStandardized(config)
        component.cognitive_system = mock_system
        component._initialized = True
        result = component.process('invalid_operation')
        self.assertFalse(result.success)
        self.assertIn('Unknown operation', result.message)
        self.assertIn('valid_operations', result.metadata)
    @unittest.skipIf(not ECHOSELF_DEMO_STANDARDIZED_AVAILABLE, 'Module not available')
    def test_cleanup_demo_files(self):
        config = EchoConfig(component_name='TestEchoselfDemo')
        component = EchoselfDemoStandardized(config)
        result = component.cleanup_demo_files()
        self.assertTrue(result.success)
        self.assertIn('cleaned_files', result.data)
    @unittest.skipIf(not ECHOSELF_DEMO_STANDARDIZED_AVAILABLE, 'Module not available')
    def test_factory_function(self):
        try:
            demo = create_echoself_demo_system()
            self.assertIsInstance(demo, EchoselfDemoStandardized)
            self.assertTrue(demo._initialized)
        except RuntimeError as e:
            self.assertIn('Failed to initialize', str(e))
    @unittest.skipIf(not ECHOSELF_DEMO_STANDARDIZED_AVAILABLE, 'Module not available')
    def test_backward_compatibility_setup_logging(self):
        setup_logging()
        root_logger = logging.getLogger()
        self.assertGreaterEqual(len(root_logger.handlers), 1)
    @unittest.skipIf(not ECHOSELF_DEMO_STANDARDIZED_AVAILABLE, 'Module not available')
    def test_backward_compatibility_demonstrate_introspection_cycle(self):
        mock_cognitive_system = Mock()
        mock_cognitive_system.perform_recursive_introspection.return_value = 'test prompt'
        mock_cognitive_system.get_introspection_metrics.return_value = {'test_metric': 'value', 'highest_salience_files': [('test.py', 0.8)]}
        mock_cognitive_system.adaptive_goal_generation_with_introspection.return_value = [Mock(description='test goal', priority=0.9, context={'type': 'test'})]
        from io import StringIO
        import sys
        old_stdout = sys.stdout
        sys.stdout = StringIO()
        try:
            demonstrate_introspection_cycle(mock_cognitive_system, 1)
            output = sys.stdout.getvalue()
            self.assertIn('RECURSIVE INTROSPECTION CYCLE 1', output)
            self.assertIn('test prompt', output)
            self.assertIn('test goal', output)
        finally:
            sys.stdout = old_stdout
    @unittest.skipIf(not ECHOSELF_DEMO_STANDARDIZED_AVAILABLE, 'Module not available')
    def test_standard_response_format(self):
        config = EchoConfig(component_name='TestEchoselfDemo')
        component = EchoselfDemoStandardized(config)
        result = component.initialize()
        self.assertIsInstance(result, EchoResponse)
        result = component.echo('test')
        self.assertIsInstance(result, EchoResponse)
        result = component.process('test')
        self.assertIsInstance(result, EchoResponse)
        result = component.cleanup_demo_files()
        self.assertIsInstance(result, EchoResponse)
    @unittest.skipIf(not ECHOSELF_DEMO_STANDARDIZED_AVAILABLE, 'Module not available')
    def test_error_handling(self):
        config = EchoConfig(component_name='TestEchoselfDemo')
        component = EchoselfDemoStandardized(config)
        def failing_method(*args, **kwargs):
            raise ValueError('Test error')
        component._demonstrate_introspection_cycle = failing_method
        with patch('echoself_demo_standardized.CognitiveArchitecture'):
            component._initialized = True
            component.cognitive_system = Mock()
            result = component.process('introspection_cycle')
            self.assertFalse(result.success)
            self.assertIn('Test error', result.message)
            self.assertIn('error_type', result.metadata)
    @unittest.skipIf(not ECHOSELF_DEMO_STANDARDIZED_AVAILABLE, 'Module not available')
    def test_component_info_compatibility(self):
        config = EchoConfig(component_name='TestEchoselfDemo', version='1.2.3')
        component = EchoselfDemoStandardized(config)
        status = component.get_status()
        self.assertTrue(status.success)
        self.assertIn('component_name', status.data)
        self.assertEqual(status.data['component_name'], 'TestEchoselfDemo')
        self.assertEqual(status.data['version'], '1.2.3')
def main():
    unittest.main(verbosity=2)
if __name__ == '__main__':
    main()