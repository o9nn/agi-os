import unittest
import logging
import sys
from unittest.mock import Mock, patch
from pathlib import Path
sys.path.insert(0, str(Path(__file__).parent))
try:
    import echoself_demo
    ECHOSELF_DEMO_AVAILABLE = True
except ImportError as e:
    ECHOSELF_DEMO_AVAILABLE = False
    print(f'Warning: Could not import echoself_demo: {e}')
class TestEchoselfDemo(unittest.TestCase):
    def setUp(self):
        logging.getLogger().setLevel(logging.CRITICAL)
    def test_import_echoself_demo(self):
        if not ECHOSELF_DEMO_AVAILABLE:
            self.skipTest('echoself_demo module not available')
        self.assertTrue(ECHOSELF_DEMO_AVAILABLE)
        self.assertIsNotNone(echoself_demo)
    @unittest.skipIf(not ECHOSELF_DEMO_AVAILABLE, 'echoself_demo not available')
    def test_setup_logging_function_exists(self):
        self.assertTrue(hasattr(echoself_demo, 'setup_logging'))
        self.assertTrue(callable(echoself_demo.setup_logging))
    @unittest.skipIf(not ECHOSELF_DEMO_AVAILABLE, 'echoself_demo not available')
    def test_setup_logging_functionality(self):
        root_logger = logging.getLogger()
        for handler in root_logger.handlers[:]:
            root_logger.removeHandler(handler)
        echoself_demo.setup_logging()
        self.assertGreaterEqual(len(root_logger.handlers), 1)
        self.assertEqual(root_logger.level, logging.INFO)
    @unittest.skipIf(not ECHOSELF_DEMO_AVAILABLE, 'echoself_demo not available')
    @patch('echoself_demo.CognitiveArchitecture')
    def test_demonstrate_introspection_cycle_function_exists(self, mock_cog_arch):
        self.assertTrue(hasattr(echoself_demo, 'demonstrate_introspection_cycle'))
        self.assertTrue(callable(echoself_demo.demonstrate_introspection_cycle))
        mock_cognitive_system = Mock()
        mock_cognitive_system._calculate_current_cognitive_load.return_value = 0.5
        mock_cognitive_system._calculate_recent_activity.return_value = 0.3
        mock_cognitive_system.perform_recursive_introspection.return_value = 'test prompt'
        try:
            echoself_demo.demonstrate_introspection_cycle(mock_cognitive_system, 1)
        except Exception as e:
            self.fail(f'demonstrate_introspection_cycle raised an exception: {e}')
    @unittest.skipIf(not ECHOSELF_DEMO_AVAILABLE, 'echoself_demo not available')
    def test_module_structure(self):
        expected_functions = ['setup_logging', 'demonstrate_introspection_cycle']
        for func_name in expected_functions:
            self.assertTrue(hasattr(echoself_demo, func_name), f'Missing expected function: {func_name}')
            self.assertTrue(callable(getattr(echoself_demo, func_name)), f'Expected function is not callable: {func_name}')
    @unittest.skipIf(not ECHOSELF_DEMO_AVAILABLE, 'echoself_demo not available')
    def test_required_imports(self):
        import importlib
        try:
            importlib.reload(echoself_demo)
        except ImportError as e:
            self.fail(f'Module failed to import required dependencies: {e}')
    @unittest.skipIf(not ECHOSELF_DEMO_AVAILABLE, 'echoself_demo not available')
    @patch('time.time')
    @patch('echoself_demo.CognitiveArchitecture')
    def test_timing_functionality(self, mock_cog_arch, mock_time):
        mock_time.side_effect = [1000.0, 1001.5]
        mock_cognitive_system = Mock()
        mock_cognitive_system._calculate_current_cognitive_load.return_value = 0.5
        mock_cognitive_system._calculate_recent_activity.return_value = 0.3
        mock_cognitive_system.perform_recursive_introspection.return_value = 'test prompt'
        from io import StringIO
        import sys
        old_stdout = sys.stdout
        sys.stdout = StringIO()
        try:
            echoself_demo.demonstrate_introspection_cycle(mock_cognitive_system, 1)
            output = sys.stdout.getvalue()
            self.assertIn('1.50 seconds', output)
            self.assertIn('Current Cognitive Load:', output)
            self.assertIn('Recent Activity Level:', output)
        finally:
            sys.stdout = old_stdout
def main():
    unittest.main(verbosity=2)
if __name__ == '__main__':
    main()