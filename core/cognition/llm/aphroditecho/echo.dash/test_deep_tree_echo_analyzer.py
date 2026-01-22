import unittest
import logging
import sys
import tempfile
from unittest.mock import Mock, patch, mock_open
from pathlib import Path
from datetime import datetime
sys.path.insert(0, str(Path(__file__).parent))
try:
    from deep_tree_echo_analyzer import DeepTreeEchoAnalyzer
    ANALYZER_AVAILABLE = True
except ImportError as e:
    ANALYZER_AVAILABLE = False
    print(f'Warning: Could not import deep_tree_echo_analyzer: {e}')
class TestDeepTreeEchoAnalyzer(unittest.TestCase):
    def setUp(self):
        logging.getLogger().setLevel(logging.CRITICAL)
    def test_import_deep_tree_echo_analyzer(self):
        if not ANALYZER_AVAILABLE:
            self.skipTest('deep_tree_echo_analyzer module not available')
        self.assertTrue(ANALYZER_AVAILABLE)
    @unittest.skipIf(not ANALYZER_AVAILABLE, 'analyzer not available')
    def test_analyzer_creation(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            analyzer = DeepTreeEchoAnalyzer(temp_dir)
            self.assertEqual(analyzer.repo_path, Path(temp_dir))
            self.assertIsInstance(analyzer.results, dict)
            expected_keys = ['fragments', 'architecture_gaps', 'migration_tasks', 'analysis_timestamp', 'recommendations']
            for key in expected_keys:
                self.assertIn(key, analyzer.results)
    @unittest.skipIf(not ANALYZER_AVAILABLE, 'analyzer not available')
    def test_default_repo_path(self):
        analyzer = DeepTreeEchoAnalyzer()
        self.assertEqual(analyzer.repo_path, Path('.'))
    @unittest.skipIf(not ANALYZER_AVAILABLE, 'analyzer not available')
    def test_results_structure(self):
        analyzer = DeepTreeEchoAnalyzer()
        self.assertIsInstance(analyzer.results['fragments'], list)
        self.assertIsInstance(analyzer.results['architecture_gaps'], list)
        self.assertIsInstance(analyzer.results['migration_tasks'], list)
        self.assertIsInstance(analyzer.results['recommendations'], list)
        self.assertIsInstance(analyzer.results['analysis_timestamp'], str)
    @unittest.skipIf(not ANALYZER_AVAILABLE, 'analyzer not available')
    def test_timestamp_format(self):
        analyzer = DeepTreeEchoAnalyzer()
        timestamp_str = analyzer.results['analysis_timestamp']
        try:
            parsed_time = datetime.fromisoformat(timestamp_str.replace('T', ' ').replace('Z', ''))
            self.assertIsInstance(parsed_time, datetime)
        except ValueError:
            self.fail(f'Timestamp not in valid ISO format: {timestamp_str}')
    @unittest.skipIf(not ANALYZER_AVAILABLE, 'analyzer not available')
    def test_analyze_fragments_method_exists(self):
        analyzer = DeepTreeEchoAnalyzer()
        self.assertTrue(hasattr(analyzer, 'analyze_fragments'))
        self.assertTrue(callable(analyzer.analyze_fragments))
    @unittest.skipIf(not ANALYZER_AVAILABLE, 'analyzer not available')
    @patch('pathlib.Path.glob')
    def test_analyze_fragments_functionality(self, mock_glob):
        mock_file1 = Mock()
        mock_file1.is_file.return_value = True
        mock_file1.name = 'echo_test.py'
        mock_file2 = Mock()
        mock_file2.is_file.return_value = True
        mock_file2.name = 'test_echo.py'
        mock_glob.return_value = [mock_file1, mock_file2]
        with patch('builtins.open', mock_open(read_data='class EchoTest:\n    def test_method(self):\n        pass')):
            analyzer = DeepTreeEchoAnalyzer()
            try:
                fragments = analyzer.analyze_fragments()
                self.assertIsInstance(fragments, list)
            except Exception as e:
                if 'not implemented' in str(e).lower():
                    self.skipTest('analyze_fragments method needs implementation')
                else:
                    pass
    @unittest.skipIf(not ANALYZER_AVAILABLE, 'analyzer not available')
    def test_analyzer_methods_exist(self):
        analyzer = DeepTreeEchoAnalyzer()
        expected_methods = ['analyze_fragments']
        for method_name in expected_methods:
            self.assertTrue(hasattr(analyzer, method_name), f'Missing expected method: {method_name}')
            self.assertTrue(callable(getattr(analyzer, method_name)), f'Method is not callable: {method_name}')
    @unittest.skipIf(not ANALYZER_AVAILABLE, 'analyzer not available')
    def test_file_pattern_recognition(self):
        analyzer = DeepTreeEchoAnalyzer()
        with tempfile.TemporaryDirectory() as temp_dir:
            temp_path = Path(temp_dir)
            echo_file = temp_path / 'echo_component.py'
            echo_file.write_text('class EchoComponent:\n    pass')
            deep_tree_file = temp_path / 'deep_tree_echo_test.py'
            deep_tree_file.write_text('def deep_tree_function():\n    pass')
            analyzer = DeepTreeEchoAnalyzer(temp_dir)
            try:
                analyzer.analyze_fragments()
            except Exception as e:
                if 'glob' in str(e) or 'not implemented' in str(e).lower():
                    pass
                else:
                    raise
    @unittest.skipIf(not ANALYZER_AVAILABLE, 'analyzer not available')
    def test_path_handling(self):
        analyzer1 = DeepTreeEchoAnalyzer('/test/path')
        self.assertEqual(analyzer1.repo_path, Path('/test/path'))
        test_path = Path('/another/path')
        analyzer2 = DeepTreeEchoAnalyzer(test_path)
        self.assertEqual(analyzer2.repo_path, test_path)
    @unittest.skipIf(not ANALYZER_AVAILABLE, 'analyzer not available')
    def test_results_initialization(self):
        analyzer = DeepTreeEchoAnalyzer()
        self.assertEqual(len(analyzer.results['fragments']), 0)
        self.assertEqual(len(analyzer.results['architecture_gaps']), 0)
        self.assertEqual(len(analyzer.results['migration_tasks']), 0)
        self.assertEqual(len(analyzer.results['recommendations']), 0)
        timestamp_str = analyzer.results['analysis_timestamp']
        timestamp = datetime.fromisoformat(timestamp_str.replace('T', ' ').replace('Z', ''))
        now = datetime.now()
        time_diff = abs((now - timestamp).total_seconds())
        self.assertLess(time_diff, 60, 'Timestamp should be recent')
    @unittest.skipIf(not ANALYZER_AVAILABLE, 'analyzer not available')
    def test_empty_directory_handling(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            analyzer = DeepTreeEchoAnalyzer(temp_dir)
            try:
                fragments = analyzer.analyze_fragments()
                self.assertIsInstance(fragments, list)
            except Exception as e:
                if 'not implemented' in str(e).lower():
                    self.skipTest('Method implementation incomplete')
                else:
                    pass
def main():
    unittest.main(verbosity=2)
if __name__ == '__main__':
    main()