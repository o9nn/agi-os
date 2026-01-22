import json
import logging
import time
from pathlib import Path
from typing import Dict, List, Optional, Any, Tuple
from dataclasses import dataclass, field
logger = logging.getLogger(__name__)
@dataclass
class HypergraphNode:
    id: str
    node_type: str
    content: str
    links: List[str] = field(default_factory=list)
    metadata: Dict[str, Any] = field(default_factory=dict)
    salience_score: float = 0.0
    timestamp: float = field(default_factory=time.time)
    def to_dict(self) -> Dict[str, Any]:
        return {'id': self.id, 'type': self.node_type, 'content': self.content, 'links': self.links, 'metadata': self.metadata, 'salience': self.salience_score, 'timestamp': self.timestamp}
class EchoselfIntrospection:
    MAX_FILE_SIZE = 50000
    BASE_ATTENTION_THRESHOLD = 0.5
    def __init__(self, root_path: str='.'):
        self.logger = logging.getLogger(__name__)
        self.root_path = Path(root_path).resolve()
        self.hypergraph_nodes: Dict[str, HypergraphNode] = {}
        self.attention_history: List[Tuple[float, Dict[str, Any]]] = []
    def semantic_salience(self, path: str) -> float:
        path_str = str(path).lower()
        if 'btree-psi.scm' in path_str:
            return 0.98
        elif 'eva-model' in path_str or 'eva_model' in path_str:
            return 0.95
        elif 'eva-behavior' in path_str or 'eva_behavior' in path_str:
            return 0.92
        elif 'echoself' in path_str:
            return 0.95
        elif 'cognitive' in path_str:
            return 0.9
        elif 'deep_tree_echo' in path_str:
            return 0.88
        elif path_str.endswith('readme.md') or path_str.endswith('readme'):
            return 0.9
        elif 'architecture' in path_str:
            return 0.85
        elif 'components' in path_str:
            return 0.82
        elif 'data_flows' in path_str:
            return 0.8
        elif '/src/' in path_str or path_str.startswith('src/'):
            return 0.85
        elif '/core/' in path_str or path_str.startswith('core/'):
            return 0.9
        elif path_str.endswith('.py'):
            if 'test_' in path_str:
                return 0.7
            elif any((keyword in path_str for keyword in ['cognitive', 'emotional', 'memory', 'personality'])):
                return 0.85
            else:
                return 0.75
        elif path_str.endswith(('.json', '.yml', '.yaml', '.toml')):
            return 0.65
        elif path_str.endswith(('.md', '.txt', '.rst')):
            return 0.6
        elif 'btree.scm' in path_str and 'psi' not in path_str:
            return 0.7
        else:
            return 0.5
    def adaptive_attention(self, current_load: float, recent_activity: float) -> float:
        threshold = self.BASE_ATTENTION_THRESHOLD + current_load * 0.3 + (0.2 - recent_activity)
        self.attention_history.append((threshold, {'current_load': current_load, 'recent_activity': recent_activity, 'timestamp': time.time()}))
        if len(self.attention_history) > 1000:
            self.attention_history = self.attention_history[-500:]
        return max(0.1, min(0.95, threshold))
    def repo_file_list(self, root: Path, attention_threshold: float) -> List[Path]:
        files = []
        try:
            if root.is_file():
                if self.semantic_salience(str(root)) > attention_threshold:
                    files.append(root)
            elif root.is_dir():
                if root.name.startswith('.') and root.name not in ['.github', '.vscode', '.devcontainer']:
                    return files
                if root.name in ['__pycache__', 'node_modules', 'dist', 'build', 'target', '.git', 'browser_data', 'chrome_user_data']:
                    return files
                for child in root.iterdir():
                    files.extend(self.repo_file_list(child, attention_threshold))
        except (PermissionError, OSError) as e:
            self.logger.debug('Skipping %s: %s', root, e)
        return files
    def safe_read_file(self, path: Path) -> str:
        try:
            if not path.exists():
                return '[File not found]'
            file_size = path.stat().st_size
            if file_size == 0:
                return '[Empty file]'
            elif file_size > self.MAX_FILE_SIZE:
                return f'[File too large: {file_size} bytes, summarized or omitted]'
            try:
                with open(path, 'r', encoding='utf-8') as f:
                    return f.read()
            except UnicodeDecodeError:
                try:
                    with open(path, 'r', encoding='latin-1') as f:
                        content = f.read()
                        return f'[Binary/non-UTF-8 content, {len(content)} chars]'
                except (UnicodeDecodeError, IOError, OSError):
                    return '[Binary file content unavailable]'
        except (IOError, OSError, PermissionError) as e:
            self.logger.debug('Error reading %s: %s', path, e)
            return f'[Error reading file: {e}]'
    def make_node(self, node_id: str, node_type: str, content: str, links: List[str]=None) -> HypergraphNode:
        return HypergraphNode(id=node_id, node_type=node_type, content=content, links=links or [], salience_score=self.semantic_salience(node_id))
    def assemble_hypergraph_input(self, root: Path, attention_threshold: float) -> List[HypergraphNode]:
        nodes = []
        try:
            files = self.repo_file_list(root, attention_threshold)
            for file_path in files:
                content = self.safe_read_file(file_path)
                node = self.make_node(node_id=str(file_path), node_type='file', content=content)
                nodes.append(node)
        except (IOError, OSError, PermissionError) as e:
            self.logger.error('Error assembling hypergraph input: %s', str(e))
        return nodes
    def inject_repo_input_into_prompt(self, current_load: float=0.5, recent_activity: float=0.3) -> str:
        attention_threshold = self.adaptive_attention(current_load, recent_activity)
        nodes = self.assemble_hypergraph_input(self.root_path, attention_threshold)
        hypergraph_string = self.hypergraph_to_string(nodes)
        prompt = self.prompt_template(hypergraph_string)
        self.logger.info('Generated introspection prompt with %d files, attention threshold: %.3f', len(nodes), attention_threshold)
        return prompt
    def hypergraph_to_string(self, nodes: List[HypergraphNode]) -> str:
        parts = []
        for node in nodes:
            content = node.content
            if len(content) > 2000:
                content = content[:2000] + '\n... [content truncated]'
            parts.append(f'(file "{node.id}" """\n{content}\n""")')
        return '\n\n'.join(parts)
    def prompt_template(self, input_content: str) -> str:
        return f'DeepTreeEcho Recursive Self-Model Introspection:\n\nRepository Hypergraph Analysis:\n{input_content}\n\nPlease analyze this repository structure and content for:\n1. Cognitive architecture patterns\n2. Emergent self-model opportunities  \n3. Neural-symbolic integration points\n4. Attention allocation optimization\n5. Recursive improvement pathways\n'
    def get_attention_metrics(self) -> Dict[str, Any]:
        if not self.attention_history:
            return {'message': 'No attention history available'}
        recent_thresholds = [entry[0] for entry in self.attention_history[-10:]]
        recent_loads = [entry[1]['current_load'] for entry in self.attention_history[-10:]]
        return {'recent_average_threshold': sum(recent_thresholds) / len(recent_thresholds), 'recent_average_load': sum(recent_loads) / len(recent_loads), 'total_decisions': len(self.attention_history), 'hypergraph_nodes': len(self.hypergraph_nodes), 'highest_salience_files': [(node.id, node.salience_score) for node in sorted(self.hypergraph_nodes.values(), key=lambda n: n.salience_score, reverse=True)[:5]]}
    def export_hypergraph(self, output_path: str) -> None:
        export_data = {'nodes': [{'id': node.id, 'type': node.node_type, 'salience_score': node.salience_score, 'content_length': len(node.content), 'links': node.links, 'metadata': node.metadata, 'timestamp': node.timestamp} for node in self.hypergraph_nodes.values()], 'attention_history': [{'threshold': entry[0], 'context': entry[1]} for entry in self.attention_history[-100:]], 'export_timestamp': time.time()}
        with open(output_path, 'w') as f:
            json.dump(export_data, f, indent=2)
        self.logger.info('Exported hypergraph to %s', output_path)
class SemanticSalienceAssessor:
    def __init__(self):
        self.salience_patterns = [('btree-psi.scm', 0.98), ('eva-model', 0.95), ('echoself.md', 0.95), ('eva-behavior', 0.92), ('readme', 0.9), ('architecture.md', 0.9), ('deep_tree_echo', 0.85), ('components.md', 0.85), ('src/', 0.85), ('cognitive_', 0.8), ('memory_', 0.8), ('btree.scm', 0.7), ('.md', 0.7), ('.py', 0.6), ('test_', 0.5), ('__pycache__', 0.1), ('.git', 0.1), ('node_modules', 0.1)]
    def assess_semantic_salience(self, path: str) -> float:
        path_str = str(path).lower()
        for pattern, salience in self.salience_patterns:
            if pattern.lower() in path_str:
                return salience
        return 0.5
class AdaptiveAttentionAllocator:
    def __init__(self):
        self.base_threshold = 0.5
    def adaptive_attention(self, current_load: float, recent_activity: float) -> float:
        threshold = self.base_threshold + current_load * 0.3 + (0.2 - recent_activity)
        return max(0.0, min(1.0, threshold))
class RepositoryIntrospector:
    def __init__(self, max_file_size: int=50000, root_path: Path=None):
        self.max_file_size = max_file_size
        self.root_path = root_path or Path.cwd()
        self.logger = logging.getLogger(__name__)
        self.salience_assessor = SemanticSalienceAssessor()
        self.attention_allocator = AdaptiveAttentionAllocator()
        self.attention_history: List[Tuple[float, Dict[str, Any]]] = []
        self.hypergraph_nodes: Dict[str, HypergraphNode] = {}
    def is_valid_file(self, path: Path) -> bool:
        if not path.exists() or not path.is_file():
            return False
        binary_extensions = {'.pyc', '.so', '.dll', '.exe', '.bin', '.jpg', '.png', '.gif', '.pdf'}
        if path.suffix.lower() in binary_extensions:
            return False
        try:
            file_size = path.stat().st_size
            return file_size > 0 and file_size <= self.max_file_size
        except (OSError, IOError):
            return False
    def safe_read_file(self, path: Path) -> str:
        try:
            if not path.exists() or not path.is_file():
                return '[File not accessible]'
            file_size = path.stat().st_size
            if file_size > self.max_file_size:
                return f'[File too large: {file_size} bytes, summarized or omitted]'
            binary_extensions = {'.pyc', '.so', '.dll', '.exe', '.bin', '.jpg', '.png', '.gif', '.pdf'}
            if path.suffix.lower() in binary_extensions:
                return '[File not accessible or binary]'
            with open(path, 'r', encoding='utf-8', errors='ignore') as f:
                return f.read()
        except (IOError, OSError, UnicodeDecodeError) as e:
            logger.warning('Error reading file %s: %s', path, e)
            return f'[Error reading file: {e}]'
    def make_node(self, node_id: str, node_type: str, content: str, links: List[str]=None) -> HypergraphNode:
        salience = self.salience_assessor.assess_semantic_salience(node_id)
        return HypergraphNode(id=node_id, node_type=node_type, content=content, links=links or [], salience_score=salience)
    def repo_file_list(self, root: Path, attention_threshold: float) -> List[Path]:
        if not root.exists():
            return []
        if root.is_file():
            salience = self.salience_assessor.assess_semantic_salience(str(root))
            if salience > attention_threshold:
                return [root]
            else:
                return []
        files = []
        try:
            for item in root.iterdir():
                if item.name.startswith('.') and item.name not in {'.gitignore', '.env.example'}:
                    continue
                files.extend(self.repo_file_list(item, attention_threshold))
        except (OSError, PermissionError) as e:
            logger.warning('Error accessing directory %s: %s', root, e)
        return files
    def assemble_hypergraph_input(self, root: Path, attention_threshold: float) -> List[HypergraphNode]:
        files = self.repo_file_list(root, attention_threshold)
        nodes = []
        for file_path in files:
            content = self.safe_read_file(file_path)
            relative_path = str(file_path.relative_to(self.root_path))
            node = self.make_node(relative_path, 'file', content)
            nodes.append(node)
        nodes.sort(key=lambda n: n.salience_score, reverse=True)
        for node in nodes:
            self.hypergraph_nodes[node.id] = node
        self.logger.info('Assembled %d hypergraph nodes with threshold %.3f', len(nodes), attention_threshold)
        return nodes
    def hypergraph_to_string(self, nodes: List[HypergraphNode]) -> str:
        parts = []
        for node in nodes:
            content = node.content
            if len(content) > 2000:
                content = content[:2000] + '\n... [content truncated]'
            parts.append(f'(file "{node.id}" """\n{content}\n""")')
        return '\n\n'.join(parts)
    def prompt_template(self, input_content: str) -> str:
        return f'DeepTreeEcho Recursive Self-Model Introspection:\n\nRepository Hypergraph Analysis:\n{input_content}\n\nPlease analyze this repository structure and content for:\n1. Cognitive architecture patterns\n2. Emergent self-model opportunities  \n3. Neural-symbolic integration points\n4. Attention allocation optimization\n5. Recursive improvement pathways\n'
    def inject_repo_input_into_prompt(self, current_load: float=0.5, recent_activity: float=0.3) -> str:
        attention_threshold = self.attention_allocator.adaptive_attention(current_load, recent_activity)
        self.attention_history.append((attention_threshold, {'current_load': current_load, 'recent_activity': recent_activity, 'timestamp': time.time()}))
        if len(self.attention_history) > 1000:
            self.attention_history = self.attention_history[-500:]
        nodes = self.assemble_hypergraph_input(self.root_path, attention_threshold)
        hypergraph_string = self.hypergraph_to_string(nodes)
        prompt = self.prompt_template(hypergraph_string)
        self.logger.info('Generated introspection prompt with %d files, attention threshold: %.3f', len(nodes), attention_threshold)
        return prompt
    def get_attention_metrics(self) -> Dict[str, Any]:
        if not self.attention_history:
            return {'message': 'No attention history available'}
        recent_thresholds = [entry[0] for entry in self.attention_history[-10:]]
        recent_loads = [entry[1]['current_load'] for entry in self.attention_history[-10:]]
        return {'recent_average_threshold': sum(recent_thresholds) / len(recent_thresholds), 'recent_average_load': sum(recent_loads) / len(recent_loads), 'total_decisions': len(self.attention_history), 'hypergraph_nodes': len(self.hypergraph_nodes), 'highest_salience_files': [(node.id, node.salience_score) for node in sorted(self.hypergraph_nodes.values(), key=lambda n: n.salience_score, reverse=True)[:5]]}
    def export_hypergraph(self, output_path: str) -> None:
        export_data = {'nodes': [{'id': node.id, 'type': node.node_type, 'salience_score': node.salience_score, 'content_length': len(node.content), 'links': node.links, 'metadata': node.metadata, 'timestamp': node.timestamp} for node in self.hypergraph_nodes.values()], 'attention_history': [{'threshold': entry[0], 'context': entry[1]} for entry in self.attention_history[-100:]], 'export_timestamp': time.time()}
        with open(output_path, 'w') as f:
            json.dump(export_data, f, indent=2)
        self.logger.info('Exported hypergraph to %s', output_path)
def main():
    logging.basicConfig(level=logging.INFO)
    introspector = EchoselfIntrospection('.')
    prompt = introspector.inject_repo_input_into_prompt(current_load=0.6, recent_activity=0.4)
    print('=== ECHOSELF INTROSPECTION PROMPT ===')
    print(prompt[:1000] + '...' if len(prompt) > 1000 else prompt)
    metrics = introspector.get_attention_metrics()
    print('\n=== ATTENTION METRICS ===')
    for key, value in metrics.items():
        print(f'{key}: {value}')
    introspector.export_hypergraph('echoself_hypergraph.json')
class HypergraphStringSerializer:
    @staticmethod
    def hypergraph_to_string(nodes: List[HypergraphNode]) -> str:
        result = []
        for node in nodes:
            escaped_content = node.content.replace('"', '\\"').replace('\n', '\\n')[:1000]
            result.append(f'(file "{node.id}" "{escaped_content}")')
        return '\n'.join(result)
class EchoselfIntrospector:
    def __init__(self, repository_root: Optional[Path]=None):
        self.repository_root = repository_root or Path.cwd()
        self.introspector = RepositoryIntrospector(root_path=self.repository_root)
        self.serializer = HypergraphStringSerializer()
        self.attention_allocator = AdaptiveAttentionAllocator()
    def prompt_template(self, input_content: str) -> str:
        return f'DeepTreeEcho Prompt:\n{input_content}'
    def inject_repo_input_into_prompt(self, current_load: float=0.6, recent_activity: float=0.4) -> str:
        attention_threshold = self.attention_allocator.adaptive_attention(current_load, recent_activity)
        logger.info('Using attention threshold: %s', attention_threshold)
        nodes = self.introspector.assemble_hypergraph_input(self.repository_root, attention_threshold)
        logger.info('Assembled %d hypergraph nodes', len(nodes))
        hypergraph_string = self.serializer.hypergraph_to_string(nodes)
        return self.prompt_template(hypergraph_string)
    def get_cognitive_snapshot(self, current_load: float=0.6, recent_activity: float=0.4) -> Dict[str, Any]:
        attention_threshold = self.attention_allocator.adaptive_attention(current_load, recent_activity)
        nodes = self.introspector.assemble_hypergraph_input(self.repository_root, attention_threshold)
        total_files = len(nodes)
        avg_salience = sum((node.salience_score for node in nodes)) / total_files if total_files > 0 else 0
        high_salience_files = [node for node in nodes if node.salience_score > 0.8]
        return {'timestamp': time.time(), 'attention_threshold': attention_threshold, 'cognitive_load': current_load, 'recent_activity': recent_activity, 'total_files_processed': total_files, 'average_salience': avg_salience, 'high_salience_count': len(high_salience_files), 'nodes': [node.to_dict() for node in nodes], 'repository_root': str(self.repository_root)}
def main():
    introspector = EchoselfIntrospector()
    prompt = introspector.inject_repo_input_into_prompt(current_load=0.6, recent_activity=0.4)
    print('Generated prompt snippet:')
    print(prompt[:500] + '...' if len(prompt) > 500 else prompt)
    snapshot = introspector.get_cognitive_snapshot()
    print(f"\nCognitive snapshot: {snapshot['total_files_processed']} files, avg salience: {snapshot['average_salience']:.3f}")
if __name__ == '__main__':
    main()