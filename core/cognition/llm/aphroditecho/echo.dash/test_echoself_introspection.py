import unittest
import tempfile
import os
import json
from pathlib import Path
from echoself_introspection import EchoselfIntrospection, HypergraphNode
class TestEchoselfIntrospection(unittest.TestCase):
    def setUp(self):
        self.temp_dir = tempfile.mkdtemp()
        self.introspector = EchoselfIntrospection(self.temp_dir)
    def tearDown(self):
        import shutil
        shutil.rmtree(self.temp_dir, ignore_errors=True)
    def test_semantic_salience_scoring(self):
        self.assertGreater(self.introspector.semantic_salience('btree-psi.scm'), 0.95)
        self.assertGreater(self.introspector.semantic_salience('eva-model.py'), 0.9)
        self.assertGreater(self.introspector.semantic_salience('echoself.md'), 0.9)
        self.assertGreater(self.introspector.semantic_salience('cognitive_architecture.py'), 0.85)
        self.assertGreater(self.introspector.semantic_salience('README.md'), 0.85)
        self.assertLess(self.introspector.semantic_salience('test_something.py'), 0.8)
        self.assertLess(self.introspector.semantic_salience('config.json'), 0.7)
    def test_adaptive_attention_mechanism(self):
        high_load_threshold = self.introspector.adaptive_attention(current_load=0.9, recent_activity=0.3)
        low_load_threshold = self.introspector.adaptive_attention(current_load=0.1, recent_activity=0.3)
        self.assertGreater(high_load_threshold, low_load_threshold)
        high_activity_threshold = self.introspector.adaptive_attention(current_load=0.5, recent_activity=0.9)
        low_activity_threshold = self.introspector.adaptive_attention(current_load=0.5, recent_activity=0.1)
        self.assertLess(high_activity_threshold, low_activity_threshold)
    def test_hypergraph_node_creation(self):
        node = self.introspector.make_node('test.py', 'file', "print('hello')", ['link1', 'link2'])
        self.assertEqual(node.id, 'test.py')
        self.assertEqual(node.node_type, 'file')
        self.assertEqual(node.content, "print('hello')")
        self.assertEqual(node.links, ['link1', 'link2'])
        self.assertGreater(node.salience_score, 0)
        self.assertIsInstance(node.timestamp, float)
    def test_safe_file_reading(self):
        test_file = Path(self.temp_dir) / 'test.txt'
        large_file = Path(self.temp_dir) / 'large.txt'
        empty_file = Path(self.temp_dir) / 'empty.txt'
        test_file.write_text('test content', encoding='utf-8')
        content = self.introspector.safe_read_file(test_file)
        self.assertEqual(content, 'test content')
        large_content = 'x' * (self.introspector.MAX_FILE_SIZE + 1000)
        large_file.write_text(large_content, encoding='utf-8')
        content = self.introspector.safe_read_file(large_file)
        self.assertIn('File too large', content)
        empty_file.write_text('', encoding='utf-8')
        content = self.introspector.safe_read_file(empty_file)
        self.assertEqual(content, '[Empty file]')
        content = self.introspector.safe_read_file(Path('nonexistent.txt'))
        self.assertEqual(content, '[File not found]')
    def test_repo_file_listing(self):
        test_dir = Path(self.temp_dir)
        (test_dir / 'high_priority.py').write_text('# Important code')
        (test_dir / 'low_priority.txt').write_text('# Documentation')
        (test_dir / 'subdir').mkdir()
        (test_dir / 'subdir' / 'nested.py').write_text('# Nested code')
        high_threshold_files = self.introspector.repo_file_list(test_dir, 0.8)
        low_threshold_files = self.introspector.repo_file_list(test_dir, 0.3)
        self.assertLessEqual(len(high_threshold_files), len(low_threshold_files))
    def test_hypergraph_assembly(self):
        test_dir = Path(self.temp_dir)
        (test_dir / 'important.py').write_text('# Important Python code')
        (test_dir / 'doc.md').write_text('# Documentation')
        nodes = self.introspector.assemble_hypergraph_input(test_dir, 0.5)
        self.assertGreater(len(nodes), 0)
        self.assertIsInstance(nodes[0], HypergraphNode)
        for i in range(len(nodes) - 1):
            self.assertGreaterEqual(nodes[i].salience_score, nodes[i + 1].salience_score)
    def test_hypergraph_string_conversion(self):
        node1 = HypergraphNode('test1.py', 'file', 'content1')
        node2 = HypergraphNode('test2.py', 'file', 'content2')
        result = self.introspector.hypergraph_to_string([node1, node2])
        self.assertIn('test1.py', result)
        self.assertIn('test2.py', result)
        self.assertIn('content1', result)
        self.assertIn('content2', result)
        self.assertIn('(file', result)
    def test_prompt_generation(self):
        test_dir = Path(self.temp_dir)
        (test_dir / 'test.py').write_text("print('test')")
        introspector = EchoselfIntrospection(test_dir)
        prompt = introspector.inject_repo_input_into_prompt(0.5, 0.3)
        self.assertIn('DeepTreeEcho Recursive Self-Model Introspection', prompt)
        self.assertIn('Repository Hypergraph Analysis', prompt)
        self.assertIn('Cognitive architecture patterns', prompt)
    def test_attention_metrics(self):
        self.introspector.adaptive_attention(0.5, 0.3)
        self.introspector.adaptive_attention(0.7, 0.4)
        metrics = self.introspector.get_attention_metrics()
        self.assertIn('recent_average_threshold', metrics)
        self.assertIn('total_decisions', metrics)
        self.assertEqual(metrics['total_decisions'], 2)
    def test_hypergraph_export(self):
        self.introspector.hypergraph_nodes['test.py'] = HypergraphNode('test.py', 'file', 'content')
        export_path = os.path.join(self.temp_dir, 'export.json')
        self.introspector.export_hypergraph(export_path)
        self.assertTrue(os.path.exists(export_path))
        with open(export_path) as f:
            data = json.load(f)
        self.assertIn('nodes', data)
        self.assertIn('attention_history', data)
        self.assertIn('export_timestamp', data)
        self.assertEqual(len(data['nodes']), 1)
        self.assertEqual(data['nodes'][0]['id'], 'test.py')
class TestHypergraphNode(unittest.TestCase):
    def test_node_creation(self):
        node = HypergraphNode('test_id', 'test_type', 'test_content')
        self.assertEqual(node.id, 'test_id')
        self.assertEqual(node.node_type, 'test_type')
        self.assertEqual(node.content, 'test_content')
        self.assertEqual(node.links, [])
        self.assertEqual(node.metadata, {})
        self.assertEqual(node.salience_score, 0.0)
        self.assertIsInstance(node.timestamp, float)
    def test_node_with_all_fields(self):
        links = ['link1', 'link2']
        metadata = {'key': 'value'}
        node = HypergraphNode(id='test_id', node_type='test_type', content='test_content', links=links, metadata=metadata, salience_score=0.8)
        self.assertEqual(node.links, links)
        self.assertEqual(node.metadata, metadata)
        self.assertEqual(node.salience_score, 0.8)
import shutil
from pathlib import Path
from echoself_introspection import EchoselfIntrospector, SemanticSalienceAssessor, AdaptiveAttentionAllocator, RepositoryIntrospector, HypergraphNode
class TestSemanticSalienceAssessor(unittest.TestCase):
    def setUp(self):
        self.assessor = SemanticSalienceAssessor()
    def test_high_salience_files(self):
        high_salience_paths = ['eva-model.py', 'echoself.md', 'ARCHITECTURE.md']
        for path in high_salience_paths:
            salience = self.assessor.assess_semantic_salience(path)
            self.assertGreaterEqual(salience, 0.85, f'Path {path} should have high salience')
    def test_low_salience_files(self):
        low_salience_paths = ['.git/objects/abc123', 'node_modules/package/index.js']
        for path in low_salience_paths:
            salience = self.assessor.assess_semantic_salience(path)
            self.assertLess(salience, 0.2, f'Path {path} should have low salience')
    def test_default_salience(self):
        unknown_path = 'some_random_file.xyz'
        salience = self.assessor.assess_semantic_salience(unknown_path)
        self.assertEqual(salience, 0.5)
class TestAdaptiveAttentionAllocator(unittest.TestCase):
    def setUp(self):
        self.allocator = AdaptiveAttentionAllocator()
    def test_high_load_increases_threshold(self):
        low_load_threshold = self.allocator.adaptive_attention(0.2, 0.5)
        high_load_threshold = self.allocator.adaptive_attention(0.8, 0.5)
        self.assertGreater(high_load_threshold, low_load_threshold)
    def test_low_activity_increases_threshold(self):
        high_activity_threshold = self.allocator.adaptive_attention(0.5, 0.8)
        low_activity_threshold = self.allocator.adaptive_attention(0.5, 0.2)
        self.assertGreater(low_activity_threshold, high_activity_threshold)
    def test_threshold_bounds(self):
        min_threshold = self.allocator.adaptive_attention(0.0, 1.0)
        max_threshold = self.allocator.adaptive_attention(1.0, 0.0)
        self.assertGreaterEqual(min_threshold, 0.0)
        self.assertLessEqual(max_threshold, 1.0)
class TestRepositoryIntrospector(unittest.TestCase):
    def setUp(self):
        self.introspector = RepositoryIntrospector()
        self.test_dir = Path(tempfile.mkdtemp())
        (self.test_dir / 'README.md').write_text('# Test Repository')
        (self.test_dir / 'src').mkdir()
        (self.test_dir / 'src' / 'main.py').write_text("print('hello world')")
        (self.test_dir / 'test_file.py').write_text('def test(): pass')
        (self.test_dir / 'large_file.txt').write_text('x' * 60000)
        (self.test_dir / 'binary.pyc').write_bytes(b'\x00\x01\x02\x03')
    def tearDown(self):
        shutil.rmtree(self.test_dir)
    def test_file_validation(self):
        self.assertTrue(self.introspector.is_valid_file(self.test_dir / 'README.md'))
        self.assertTrue(self.introspector.is_valid_file(self.test_dir / 'src' / 'main.py'))
        self.assertFalse(self.introspector.is_valid_file(self.test_dir / 'large_file.txt'))
        self.assertFalse(self.introspector.is_valid_file(self.test_dir / 'binary.pyc'))
        self.assertFalse(self.introspector.is_valid_file(self.test_dir / 'nonexistent.txt'))
    def test_safe_file_reading(self):
        content = self.introspector.safe_read_file(self.test_dir / 'README.md')
        self.assertEqual(content, '# Test Repository')
        content = self.introspector.safe_read_file(self.test_dir / 'large_file.txt')
        self.assertIn('File too large', content)
        content = self.introspector.safe_read_file(self.test_dir / 'binary.pyc')
        self.assertIn('not accessible or binary', content)
    def test_repo_file_list_filtering(self):
        files_low = self.introspector.repo_file_list(self.test_dir, 0.3)
        files_high = self.introspector.repo_file_list(self.test_dir, 0.9)
        self.assertGreaterEqual(len(files_low), len(files_high))
        any(('readme' in str(f).lower() for f in files_high))
class TestEchoselfIntrospector(unittest.TestCase):
    def setUp(self):
        self.test_dir = Path(tempfile.mkdtemp())
        (self.test_dir / 'README.md').write_text('# Test Project\nDescription')
        (self.test_dir / 'echoself.md').write_text('# Echoself\nCognitive content')
        src_dir = self.test_dir / 'src'
        src_dir.mkdir()
        (src_dir / 'main.py').write_text('def main(): pass')
        self.introspector = EchoselfIntrospector(self.test_dir)
    def tearDown(self):
        shutil.rmtree(self.test_dir)
    def test_cognitive_snapshot(self):
        snapshot = self.introspector.get_cognitive_snapshot(current_load=0.6, recent_activity=0.4)
        self.assertIn('timestamp', snapshot)
        self.assertIn('attention_threshold', snapshot)
        self.assertIn('total_files_processed', snapshot)
        self.assertIn('nodes', snapshot)
        self.assertIsInstance(snapshot['nodes'], list)
        self.assertGreater(snapshot['total_files_processed'], 0)
        if snapshot['nodes']:
            node = snapshot['nodes'][0]
            self.assertIn('id', node)
            self.assertIn('type', node)
            self.assertIn('content', node)
            self.assertIn('salience', node)
    def test_prompt_generation(self):
        prompt = self.introspector.inject_repo_input_into_prompt(current_load=0.5, recent_activity=0.5)
        self.assertIn('DeepTreeEcho Prompt:', prompt)
        self.assertIn('(file', prompt)
    def test_attention_threshold_affects_processing(self):
        high_load_snapshot = self.introspector.get_cognitive_snapshot(current_load=0.9, recent_activity=0.1)
        low_load_snapshot = self.introspector.get_cognitive_snapshot(current_load=0.1, recent_activity=0.9)
        self.assertLessEqual(high_load_snapshot['total_files_processed'], low_load_snapshot['total_files_processed'])
class TestHypergraphNode(unittest.TestCase):
    def test_node_creation(self):
        node = HypergraphNode(id='test_file.py', node_type='file', content='def test(): pass', salience_score=0.8)
        self.assertEqual(node.id, 'test_file.py')
        self.assertEqual(node.node_type, 'file')
        self.assertEqual(node.content, 'def test(): pass')
        self.assertEqual(node.salience_score, 0.8)
        node_dict = node.to_dict()
        self.assertIn('id', node_dict)
        self.assertIn('type', node_dict)
        self.assertIn('content', node_dict)
        self.assertIn('salience', node_dict)
if __name__ == '__main__':
    unittest.main()