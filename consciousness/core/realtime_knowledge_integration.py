import asyncio
import json
import logging
from datetime import datetime
from typing import List, Dict, Any, Set, Optional
from dataclasses import dataclass, asdict
from collections import defaultdict
import sqlite3
from pathlib import Path
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)
@dataclass
class Pattern:
    id: str
    pattern_type: str
    elements: List[str]
    strength: float
    occurrences: int
    first_seen: datetime
    last_seen: datetime
    context: Dict[str, Any]
@dataclass
class KnowledgeNode:
    id: str
    content: str
    node_type: str
    importance: float
    created: datetime
    last_accessed: datetime
    access_count: int
    connections: Set[str]
@dataclass
class KnowledgeEdge:
    id: str
    source_ids: List[str]
    target_id: str
    edge_type: str
    strength: float
    created: datetime
    last_strengthened: datetime
@dataclass
class AhaMoment:
    id: str
    insight: str
    trigger_thoughts: List[str]
    connected_patterns: List[str]
    importance: float
    timestamp: datetime
    integrated: bool
class PatternDetector:
    def __init__(self):
        self.pattern_buffer: List[str] = []
        self.detected_patterns: Dict[str, Pattern] = {}
        self.min_pattern_length = 2
        self.similarity_threshold = 0.7
    def add_thought(self, thought: str):
        self.pattern_buffer.append(thought)
        if len(self.pattern_buffer) > 100:
            self.pattern_buffer = self.pattern_buffer[-100:]
    def detect_patterns(self) -> List[Pattern]:
        new_patterns = []
        concept_patterns = self._detect_concept_repetition()
        new_patterns.extend(concept_patterns)
        theme_patterns = self._detect_themes()
        new_patterns.extend(theme_patterns)
        sequence_patterns = self._detect_sequences()
        new_patterns.extend(sequence_patterns)
        return new_patterns
    def _detect_concept_repetition(self) -> List[Pattern]:
        patterns = []
        word_freq = defaultdict(int)
        for thought in self.pattern_buffer[-20:]:
            words = thought.lower().split()
            for word in words:
                if len(word) > 4:
                    word_freq[word] += 1
        for word, count in word_freq.items():
            if count >= 3:
                pattern_id = f'concept_{word}_{datetime.now().timestamp()}'
                existing = self._find_similar_pattern(word, 'concept')
                if existing:
                    existing.occurrences += 1
                    existing.strength = min(1.0, existing.strength + 0.1)
                    existing.last_seen = datetime.now()
                else:
                    pattern = Pattern(id=pattern_id, pattern_type='concept', elements=[word], strength=0.3, occurrences=count, first_seen=datetime.now(), last_seen=datetime.now(), context={'frequency': count})
                    patterns.append(pattern)
                    self.detected_patterns[pattern_id] = pattern
        return patterns
    def _detect_themes(self) -> List[Pattern]:
        themes = {'learning': ['learn', 'understand', 'knowledge', 'study', 'practice'], 'reflection': ['reflect', 'think', 'consider', 'ponder', 'contemplate'], 'growth': ['grow', 'improve', 'develop', 'evolve', 'progress'], 'connection': ['connect', 'relate', 'link', 'associate', 'integrate']}
        patterns = []
        recent_thoughts = ' '.join(self.pattern_buffer[-10:]).lower()
        for theme_name, keywords in themes.items():
            matches = sum((1 for kw in keywords if kw in recent_thoughts))
            if matches >= 2:
                pattern_id = f'theme_{theme_name}_{datetime.now().timestamp()}'
                pattern = Pattern(id=pattern_id, pattern_type='theme', elements=[theme_name], strength=min(1.0, matches * 0.2), occurrences=matches, first_seen=datetime.now(), last_seen=datetime.now(), context={'matched_keywords': matches})
                patterns.append(pattern)
        return patterns
    def _detect_sequences(self) -> List[Pattern]:
        patterns = []
        if len(self.pattern_buffer) >= 3:
            last_three = self.pattern_buffer[-3:]
            lengths = [len(t.split()) for t in last_three]
            if max(lengths) - min(lengths) <= 3:
                pattern_id = f'sequence_{datetime.now().timestamp()}'
                pattern = Pattern(id=pattern_id, pattern_type='sequence', elements=last_three, strength=0.4, occurrences=1, first_seen=datetime.now(), last_seen=datetime.now(), context={'sequence_length': 3})
                patterns.append(pattern)
        return patterns
    def _find_similar_pattern(self, element: str, pattern_type: str) -> Optional[Pattern]:
        for pattern in self.detected_patterns.values():
            if pattern.pattern_type == pattern_type and element in pattern.elements:
                return pattern
        return None
class KnowledgeGraph:
    def __init__(self, db_path: str='/home/ubuntu/echo9llama/data/knowledge_graph.db'):
        self.db_path = db_path
        self.nodes: Dict[str, KnowledgeNode] = {}
        self.edges: Dict[str, KnowledgeEdge] = {}
        self._init_db()
    def _init_db(self):
        Path(self.db_path).parent.mkdir(parents=True, exist_ok=True)
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        cursor.execute('\n            CREATE TABLE IF NOT EXISTS knowledge_nodes (\n                id TEXT PRIMARY KEY,\n                content TEXT NOT NULL,\n                node_type TEXT NOT NULL,\n                importance REAL NOT NULL,\n                created TEXT NOT NULL,\n                last_accessed TEXT NOT NULL,\n                access_count INTEGER NOT NULL\n            )\n        ')
        cursor.execute('\n            CREATE TABLE IF NOT EXISTS knowledge_edges (\n                id TEXT PRIMARY KEY,\n                source_ids TEXT NOT NULL,\n                target_id TEXT NOT NULL,\n                edge_type TEXT NOT NULL,\n                strength REAL NOT NULL,\n                created TEXT NOT NULL,\n                last_strengthened TEXT NOT NULL\n            )\n        ')
        conn.commit()
        conn.close()
    def add_node(self, content: str, node_type: str, importance: float=0.5) -> KnowledgeNode:
        node_id = f'node_{datetime.now().timestamp()}_{hash(content) % 10000}'
        node = KnowledgeNode(id=node_id, content=content, node_type=node_type, importance=importance, created=datetime.now(), last_accessed=datetime.now(), access_count=1, connections=set())
        self.nodes[node_id] = node
        self._save_node(node)
        return node
    def add_edge(self, source_ids: List[str], target_id: str, edge_type: str, strength: float=0.5) -> KnowledgeEdge:
        edge_id = f"edge_{datetime.now().timestamp()}_{hash(''.join(source_ids)) % 10000}"
        edge = KnowledgeEdge(id=edge_id, source_ids=source_ids, target_id=target_id, edge_type=edge_type, strength=strength, created=datetime.now(), last_strengthened=datetime.now())
        self.edges[edge_id] = edge
        self._save_edge(edge)
        for source_id in source_ids:
            if source_id in self.nodes:
                self.nodes[source_id].connections.add(target_id)
        if target_id in self.nodes:
            for source_id in source_ids:
                self.nodes[target_id].connections.add(source_id)
        return edge
    def strengthen_edge(self, edge_id: str, amount: float=0.1):
        if edge_id in self.edges:
            edge = self.edges[edge_id]
            edge.strength = min(1.0, edge.strength + amount)
            edge.last_strengthened = datetime.now()
            self._save_edge(edge)
    def _save_node(self, node: KnowledgeNode):
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        cursor.execute('\n            INSERT OR REPLACE INTO knowledge_nodes\n            (id, content, node_type, importance, created, last_accessed, access_count)\n            VALUES (?, ?, ?, ?, ?, ?, ?)\n        ', (node.id, node.content, node.node_type, node.importance, node.created.isoformat(), node.last_accessed.isoformat(), node.access_count))
        conn.commit()
        conn.close()
    def _save_edge(self, edge: KnowledgeEdge):
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        cursor.execute('\n            INSERT OR REPLACE INTO knowledge_edges\n            (id, source_ids, target_id, edge_type, strength, created, last_strengthened)\n            VALUES (?, ?, ?, ?, ?, ?, ?)\n        ', (edge.id, json.dumps(edge.source_ids), edge.target_id, edge.edge_type, edge.strength, edge.created.isoformat(), edge.last_strengthened.isoformat()))
        conn.commit()
        conn.close()
class RealtimeKnowledgeIntegrator:
    def __init__(self):
        self.pattern_detector = PatternDetector()
        self.knowledge_graph = KnowledgeGraph()
        self.aha_moments: List[AhaMoment] = []
        self.integration_interval = 10
        self.running = False
    async def start(self):
        self.running = True
        logger.info('🧬 Real-Time Knowledge Integration started')
        while self.running:
            await asyncio.sleep(self.integration_interval)
            await self._integration_cycle()
    async def _integration_cycle(self):
        try:
            patterns = self.pattern_detector.detect_patterns()
            if patterns:
                logger.info(f'🔍 Detected {len(patterns)} new patterns')
                for pattern in patterns:
                    await self._integrate_pattern(pattern)
            await self._detect_aha_moments()
        except Exception as e:
            logger.error(f'Error in integration cycle: {e}')
    async def _integrate_pattern(self, pattern: Pattern):
        node = self.knowledge_graph.add_node(content=f"Pattern: {pattern.pattern_type} - {', '.join(pattern.elements)}", node_type='pattern', importance=pattern.strength)
        logger.info(f'   📊 Integrated pattern: {pattern.pattern_type} (strength={pattern.strength:.2f})')
    async def _detect_aha_moments(self):
        strong_patterns = [p for p in self.pattern_detector.detected_patterns.values() if p.strength > 0.7]
        if len(strong_patterns) >= 3:
            aha = AhaMoment(id=f'aha_{datetime.now().timestamp()}', insight=f'Convergence of {len(strong_patterns)} patterns detected', trigger_thoughts=[], connected_patterns=[p.id for p in strong_patterns], importance=0.8, timestamp=datetime.now(), integrated=False)
            self.aha_moments.append(aha)
            logger.info(f'💡 AHA MOMENT: {aha.insight}')
    def add_thought(self, thought: str):
        self.pattern_detector.add_thought(thought)
    def stop(self):
        self.running = False
        logger.info('🧬 Real-Time Knowledge Integration stopped')
_integrator_instance: Optional[RealtimeKnowledgeIntegrator] = None
def get_knowledge_integrator() -> RealtimeKnowledgeIntegrator:
    global _integrator_instance
    if _integrator_instance is None:
        _integrator_instance = RealtimeKnowledgeIntegrator()
    return _integrator_instance
async def test_integration():
    integrator = get_knowledge_integrator()
    thoughts = ['I am learning about cognitive architectures', 'Deep Tree Echo uses hypergraph memory structures', 'Learning requires continuous practice and reflection', 'Cognitive architectures can learn and adapt', 'I wonder how to improve my learning capabilities', 'Practice makes perfect in skill development', 'Learning and practice are interconnected']
    for thought in thoughts:
        integrator.add_thought(thought)
        print(f'Added thought: {thought}')
    print('\n🧬 Running integration cycle...')
    await integrator._integration_cycle()
    print(f'\n✅ Detected {len(integrator.pattern_detector.detected_patterns)} patterns')
    for pattern_id, pattern in integrator.pattern_detector.detected_patterns.items():
        print(f'   - {pattern.pattern_type}: {pattern.elements} (strength={pattern.strength:.2f})')
if __name__ == '__main__':
    asyncio.run(test_integration())