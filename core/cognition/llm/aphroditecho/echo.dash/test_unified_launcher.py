import sys
import os
import tempfile
import unittest
import logging
from pathlib import Path
from unittest.mock import Mock, patch
sys.path.insert(0, str(Path(__file__).parent))
class TestUnifiedLauncher(unittest.TestCase):
    def setUp(self):
        self.temp_dir = tempfile.mkdtemp()
        self.original_cwd = os.getcwd()
    def tearDown(self):
        os.chdir(self.original_cwd)
    def test_launcher_config_creation(self):
        from unified_launcher import LauncherConfig, LaunchMode
        config = LauncherConfig(mode=LaunchMode.DEEP_TREE_ECHO, debug=True, gui=True, browser=True)
        self.assertEqual(config.mode, LaunchMode.DEEP_TREE_ECHO)
        self.assertTrue(config.debug)
        self.assertTrue(config.gui)
        self.assertTrue(config.browser)
        gui_config = LauncherConfig(mode=LaunchMode.GUI_STANDALONE, debug=False, no_activity=True)
        self.assertEqual(gui_config.mode, LaunchMode.GUI_STANDALONE)
        self.assertFalse(gui_config.debug)
        self.assertTrue(gui_config.no_activity)
    def test_launcher_mode_selection(self):
        from unified_launcher import UnifiedLauncher, LaunchMode
        launcher = UnifiedLauncher()
        args = Mock()
        args.mode = 'deep-tree-echo'
        mode = launcher._determine_mode(args)
        self.assertEqual(mode, LaunchMode.DEEP_TREE_ECHO)
        args.mode = 'gui'
        mode = launcher._determine_mode(args)
        self.assertEqual(mode, LaunchMode.GUI_DASHBOARD)
    @patch('memory_management.HypergraphMemory')
    def test_component_initialization(self, mock_memory):
        from unified_launcher import UnifiedLauncher, LauncherConfig, LaunchMode
        mock_memory_instance = Mock()
        mock_memory.return_value = mock_memory_instance
        launcher = UnifiedLauncher()
        config = LauncherConfig(mode=LaunchMode.GUI_STANDALONE)
        components = launcher._initialize_components(config)
        self.assertIsNotNone(components['memory'])
        mock_memory.assert_called_once()
    def test_backward_compatibility(self):
        from unified_launcher import create_config_from_args
        args = Mock()
        args.gui = True
        args.browser = True
        args.debug = False
        config = create_config_from_args('deep-tree-echo', args)
        self.assertTrue(config.gui)
        self.assertTrue(config.browser)
        self.assertFalse(config.debug)
        args = Mock()
        args.debug = True
        args.no_activity = True
        config = create_config_from_args('gui-standalone', args)
        self.assertTrue(config.debug)
        self.assertTrue(config.no_activity)
def run_tests():
    logging.getLogger().setLevel(logging.WARNING)
    loader = unittest.TestLoader()
    suite = loader.loadTestsFromTestCase(TestUnifiedLauncher)
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)
    return result.wasSuccessful()
if __name__ == '__main__':
    success = run_tests()
    sys.exit(0 if success else 1)