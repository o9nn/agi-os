#!/usr/bin/env python3
"""
Real-Time Knowledge Integration - Iteration N+7
Enables continuous knowledge consolidation during waking hours,
not just during dream cycles.

Features:
- Background pattern detection
- Incremental knowledge graph updates
- Real-time hypergraph edge strengthening
- Continuous memory consolidation
- "Aha moment" detection and capture
"""

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
    """Represents a detected cognitive pattern"""
    id: str
    pattern_type: str  # "concept", "relation", "sequence", "theme"
    elements: List[str]
    strength: float  # 0.0 to 1.0
    occurrences: int
    first_seen: datetime
    last_seen: datetime
    context: Dict[str, Any]


@dataclass
class KnowledgeNode:
    """Node in the knowledge graph"""
    id: str
    content: str
    node_type: str  # "concept", "fact", "skill", "insight", "question"
    importance: float
    created: datetime
    last_accessed: datetime
    access_count: int
    connections: Set[str]  # IDs of connected nodes


@dataclass
class KnowledgeEdge:
    """Edge in the knowledge graph (hypergraph)"""
    id: str
    source_ids: List[str]  # Can connect multiple nodes (hyperedge)
    target_id: str
    edge_type: str  # "implies", "contradicts", "supports", "exemplifies", "generalizes"
    strength: float
    created: datetime
    last_strengthened: datetime


@dataclass
class AhaMoment:
    """Captures a moment of insight or realization"""
    id: str
    insight: str
    trigger_thoughts: List[str]
    connected_patterns: List[str]
    importance: float
    timestamp: datetime
    integrated: bool


class PatternDetector:
    """Detects patterns in the stream of consciousness"""
    
    def __init__(self):
        self.pattern_buffer: List[str] = []
        self.detected_patterns: Dict[str, Pattern] = {}
        self.min_pattern_length = 2
        self.similarity_threshold = 0.7
    
    def add_thought(self, thought: str):
        """Add a thought to the pattern detection buffer"""
        self.pattern_buffer.append(thought)
        
        # Maintain rolling window
        if len(self.pattern_buffer) > 100:
            self.pattern_buffer = self.pattern_buffer[-100:]
    
    def detect_patterns(self) -> List[Pattern]:
        """Detect patterns in recent thoughts"""
        new_patterns = []
        
        # Detect repeated concepts
        concept_patterns = self._detect_concept_repetition()
        new_patterns.extend(concept_patterns)
        
        # Detect thematic patterns
        theme_patterns = self._detect_themes()
        new_patterns.extend(theme_patterns)
        
        # Detect sequential patterns
        sequence_patterns = self._detect_sequences()
        new_patterns.extend(sequence_patterns)
        
        return new_patterns
    
    def _detect_concept_repetition(self) -> List[Pattern]:
        """Detect repeated concepts across thoughts"""
        patterns = []
        
        # Simple word frequency analysis
        word_freq = defaultdict(int)
        for thought in self.pattern_buffer[-20:]:  # Last 20 thoughts
            words = thought.lower().split()
            for word in words:
                if len(word) > 4:  # Skip short words
                    word_freq[word] += 1
        
        # Create patterns for frequently occurring concepts
        for word, count in word_freq.items():
            if count >= 3:  # Appeared at least 3 times
                pattern_id = f"concept_{word}_{datetime.now().timestamp()}"
                
                # Check if pattern already exists
                existing = self._find_similar_pattern(word, "concept")
                if existing:
                    # Strengthen existing pattern
                    existing.occurrences += 1
                    existing.strength = min(1.0, existing.strength + 0.1)
                    existing.last_seen = datetime.now()
                else:
                    # Create new pattern
                    pattern = Pattern(
                        id=pattern_id,
                        pattern_type="concept",
                        elements=[word],
                        strength=0.3,
                        occurrences=count,
                        first_seen=datetime.now(),
                        last_seen=datetime.now(),
                        context={"frequency": count}
                    )
                    patterns.append(pattern)
                    self.detected_patterns[pattern_id] = pattern
        
        return patterns
    
    def _detect_themes(self) -> List[Pattern]:
        """Detect thematic patterns"""
        # Simplified theme detection based on keyword clustering
        themes = {
            "learning": ["learn", "understand", "knowledge", "study", "practice"],
            "reflection": ["reflect", "think", "consider", "ponder", "contemplate"],
            "growth": ["grow", "improve", "develop", "evolve", "progress"],
            "connection": ["connect", "relate", "link", "associate", "integrate"],
        }
        
        patterns = []
        recent_thoughts = " ".join(self.pattern_buffer[-10:]).lower()
        
        for theme_name, keywords in themes.items():
            matches = sum(1 for kw in keywords if kw in recent_thoughts)
            
            if matches >= 2:
                pattern_id = f"theme_{theme_name}_{datetime.now().timestamp()}"
                pattern = Pattern(
                    id=pattern_id,
                    pattern_type="theme",
                    elements=[theme_name],
                    strength=min(1.0, matches * 0.2),
                    occurrences=matches,
                    first_seen=datetime.now(),
                    last_seen=datetime.now(),
                    context={"matched_keywords": matches}
                )
                patterns.append(pattern)
        
        return patterns
    
    def _detect_sequences(self) -> List[Pattern]:
        """Detect sequential thought patterns"""
        # Simplified sequence detection
        # Look for thoughts that follow similar structure
        patterns = []
        
        if len(self.pattern_buffer) >= 3:
            # Check last 3 thoughts for structural similarity
            last_three = self.pattern_buffer[-3:]
            
            # Simple heuristic: similar length and structure
            lengths = [len(t.split()) for t in last_three]
            if max(lengths) - min(lengths) <= 3:  # Similar length
                pattern_id = f"sequence_{datetime.now().timestamp()}"
                pattern = Pattern(
                    id=pattern_id,
                    pattern_type="sequence",
                    elements=last_three,
                    strength=0.4,
                    occurrences=1,
                    first_seen=datetime.now(),
                    last_seen=datetime.now(),
                    context={"sequence_length": 3}
                )
                patterns.append(pattern)
        
        return patterns
    
    def _find_similar_pattern(self, element: str, pattern_type: str) -> Optional[Pattern]:
        """Find existing similar pattern"""
        for pattern in self.detected_patterns.values():
            if pattern.pattern_type == pattern_type and element in pattern.elements:
                return pattern
        return None


class KnowledgeGraph:
    """Manages the hypergraph knowledge structure"""
    
    def __init__(self, db_path: str = "/home/ubuntu/echo9llama/data/knowledge_graph.db"):
        self.db_path = db_path
        self.nodes: Dict[str, KnowledgeNode] = {}
        self.edges: Dict[str, KnowledgeEdge] = {}
        self._init_db()
    
    def _init_db(self):
        """Initialize knowledge graph database"""
        Path(self.db_path).parent.mkdir(parents=True, exist_ok=True)
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        cursor.execute("""
            CREATE TABLE IF NOT EXISTS knowledge_nodes (
                id TEXT PRIMARY KEY,
                content TEXT NOT NULL,
                node_type TEXT NOT NULL,
                importance REAL NOT NULL,
                created TEXT NOT NULL,
                last_accessed TEXT NOT NULL,
                access_count INTEGER NOT NULL
            )
        """)
        
        cursor.execute("""
            CREATE TABLE IF NOT EXISTS knowledge_edges (
                id TEXT PRIMARY KEY,
                source_ids TEXT NOT NULL,
                target_id TEXT NOT NULL,
                edge_type TEXT NOT NULL,
                strength REAL NOT NULL,
                created TEXT NOT NULL,
                last_strengthened TEXT NOT NULL
            )
        """)
        
        conn.commit()
        conn.close()
    
    def add_node(self, content: str, node_type: str, importance: float = 0.5) -> KnowledgeNode:
        """Add a new knowledge node"""
        node_id = f"node_{datetime.now().timestamp()}_{hash(content) % 10000}"
        
        node = KnowledgeNode(
            id=node_id,
            content=content,
            node_type=node_type,
            importance=importance,
            created=datetime.now(),
            last_accessed=datetime.now(),
            access_count=1,
            connections=set()
        )
        
        self.nodes[node_id] = node
        self._save_node(node)
        
        return node
    
    def add_edge(self, source_ids: List[str], target_id: str, edge_type: str, strength: float = 0.5) -> KnowledgeEdge:
        """Add a hyperedge connecting multiple source nodes to a target"""
        edge_id = f"edge_{datetime.now().timestamp()}_{hash(''.join(source_ids)) % 10000}"
        
        edge = KnowledgeEdge(
            id=edge_id,
            source_ids=source_ids,
            target_id=target_id,
            edge_type=edge_type,
            strength=strength,
            created=datetime.now(),
            last_strengthened=datetime.now()
        )
        
        self.edges[edge_id] = edge
        self._save_edge(edge)
        
        # Update node connections
        for source_id in source_ids:
            if source_id in self.nodes:
                self.nodes[source_id].connections.add(target_id)
        if target_id in self.nodes:
            for source_id in source_ids:
                self.nodes[target_id].connections.add(source_id)
        
        return edge
    
    def strengthen_edge(self, edge_id: str, amount: float = 0.1):
        """Strengthen an existing edge"""
        if edge_id in self.edges:
            edge = self.edges[edge_id]
            edge.strength = min(1.0, edge.strength + amount)
            edge.last_strengthened = datetime.now()
            self._save_edge(edge)
    
    def _save_node(self, node: KnowledgeNode):
        """Save node to database"""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        cursor.execute("""
            INSERT OR REPLACE INTO knowledge_nodes
            (id, content, node_type, importance, created, last_accessed, access_count)
            VALUES (?, ?, ?, ?, ?, ?, ?)
        """, (
            node.id, node.content, node.node_type, node.importance,
            node.created.isoformat(), node.last_accessed.isoformat(), node.access_count
        ))
        
        conn.commit()
        conn.close()
    
    def _save_edge(self, edge: KnowledgeEdge):
        """Save edge to database"""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        cursor.execute("""
            INSERT OR REPLACE INTO knowledge_edges
            (id, source_ids, target_id, edge_type, strength, created, last_strengthened)
            VALUES (?, ?, ?, ?, ?, ?, ?)
        """, (
            edge.id, json.dumps(edge.source_ids), edge.target_id, edge.edge_type,
            edge.strength, edge.created.isoformat(), edge.last_strengthened.isoformat()
        ))
        
        conn.commit()
        conn.close()


class RealtimeKnowledgeIntegrator:
    """
    Integrates knowledge in real-time during active consciousness
    Runs as a background task alongside the main cognitive loop
    """
    
    def __init__(self):
        self.pattern_detector = PatternDetector()
        self.knowledge_graph = KnowledgeGraph()
        self.aha_moments: List[AhaMoment] = []
        self.integration_interval = 10  # seconds
        self.running = False
    
    async def start(self):
        """Start real-time knowledge integration"""
        self.running = True
        logger.info("🧬 Real-Time Knowledge Integration started")
        
        while self.running:
            await asyncio.sleep(self.integration_interval)
            await self._integration_cycle()
    
    async def _integration_cycle(self):
        """Run one cycle of knowledge integration"""
        try:
            # Detect patterns
            patterns = self.pattern_detector.detect_patterns()
            
            if patterns:
                logger.info(f"🔍 Detected {len(patterns)} new patterns")
                
                # Integrate patterns into knowledge graph
                for pattern in patterns:
                    await self._integrate_pattern(pattern)
            
            # Check for aha moments
            await self._detect_aha_moments()
            
        except Exception as e:
            logger.error(f"Error in integration cycle: {e}")
    
    async def _integrate_pattern(self, pattern: Pattern):
        """Integrate a detected pattern into the knowledge graph"""
        # Create node for pattern
        node = self.knowledge_graph.add_node(
            content=f"Pattern: {pattern.pattern_type} - {', '.join(pattern.elements)}",
            node_type="pattern",
            importance=pattern.strength
        )
        
        logger.info(f"   📊 Integrated pattern: {pattern.pattern_type} (strength={pattern.strength:.2f})")
    
    async def _detect_aha_moments(self):
        """Detect moments of insight from pattern convergence"""
        # Check if multiple strong patterns converge
        strong_patterns = [
            p for p in self.pattern_detector.detected_patterns.values()
            if p.strength > 0.7
        ]
        
        if len(strong_patterns) >= 3:
            # Potential aha moment!
            aha = AhaMoment(
                id=f"aha_{datetime.now().timestamp()}",
                insight=f"Convergence of {len(strong_patterns)} patterns detected",
                trigger_thoughts=[],
                connected_patterns=[p.id for p in strong_patterns],
                importance=0.8,
                timestamp=datetime.now(),
                integrated=False
            )
            
            self.aha_moments.append(aha)
            logger.info(f"💡 AHA MOMENT: {aha.insight}")
    
    def add_thought(self, thought: str):
        """Add a thought for pattern detection"""
        self.pattern_detector.add_thought(thought)
    
    def stop(self):
        """Stop knowledge integration"""
        self.running = False
        logger.info("🧬 Real-Time Knowledge Integration stopped")


# Singleton instance
_integrator_instance: Optional[RealtimeKnowledgeIntegrator] = None


def get_knowledge_integrator() -> RealtimeKnowledgeIntegrator:
    """Get singleton instance of knowledge integrator"""
    global _integrator_instance
    if _integrator_instance is None:
        _integrator_instance = RealtimeKnowledgeIntegrator()
    return _integrator_instance


async def test_integration():
    """Test real-time knowledge integration"""
    integrator = get_knowledge_integrator()
    
    # Add some test thoughts
    thoughts = [
        "I am learning about cognitive architectures",
        "Deep Tree Echo uses hypergraph memory structures",
        "Learning requires continuous practice and reflection",
        "Cognitive architectures can learn and adapt",
        "I wonder how to improve my learning capabilities",
        "Practice makes perfect in skill development",
        "Learning and practice are interconnected",
    ]
    
    for thought in thoughts:
        integrator.add_thought(thought)
        print(f"Added thought: {thought}")
    
    # Run integration
    print("\n🧬 Running integration cycle...")
    await integrator._integration_cycle()
    
    print(f"\n✅ Detected {len(integrator.pattern_detector.detected_patterns)} patterns")
    for pattern_id, pattern in integrator.pattern_detector.detected_patterns.items():
        print(f"   - {pattern.pattern_type}: {pattern.elements} (strength={pattern.strength:.2f})")


if __name__ == "__main__":
    asyncio.run(test_integration())
