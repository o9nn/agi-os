import os
import json
import sqlite3
import numpy as np
from pathlib import Path
from datetime import datetime
from typing import Dict, List, Tuple, Any, Optional
from dataclasses import dataclass, asdict
import logging
try:
    import networkx as nx
    NETWORKX_AVAILABLE = True
except ImportError:
    NETWORKX_AVAILABLE = False
    print('⚠️  NetworkX not available - hypergraph features limited')
try:
    from sentence_transformers import SentenceTransformer
    EMBEDDINGS_AVAILABLE = True
except ImportError:
    EMBEDDINGS_AVAILABLE = False
    print('⚠️  Sentence Transformers not available - using simple embeddings')
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)
@dataclass
class Concept:
    id: str
    name: str
    concept_type: str
    properties: Dict[str, Any]
    embedding: Optional[np.ndarray] = None
    created_at: int = 0
    last_accessed: int = 0
    access_count: int = 0
    importance: float = 0.5
@dataclass
class Relation:
    source: str
    target: str
    relation_type: str
    strength: float = 1.0
    created_at: int = 0
    context: Dict[str, Any] = None
class HypergraphMemory:
    def __init__(self, db_path: str='data/hypergraph_memory.db', embedding_model: str='all-MiniLM-L6-v2'):
        self.db_path = db_path
        Path(db_path).parent.mkdir(parents=True, exist_ok=True)
        if NETWORKX_AVAILABLE:
            self.graph = nx.MultiDiGraph()
        else:
            self.graph = None
            logger.warning('NetworkX not available - graph operations disabled')
        if EMBEDDINGS_AVAILABLE:
            try:
                self.embedder = SentenceTransformer(embedding_model)
                logger.info(f'Loaded embedding model: {embedding_model}')
            except Exception as e:
                logger.warning(f'Failed to load embedding model: {e}')
                self.embedder = None
        else:
            self.embedder = None
        self._init_db()
        self._load_graph()
    def _init_db(self):
        conn = sqlite3.connect(self.db_path)
        conn.execute('\n            CREATE TABLE IF NOT EXISTS concepts (\n                id TEXT PRIMARY KEY,\n                name TEXT NOT NULL,\n                concept_type TEXT NOT NULL,\n                properties TEXT,\n                embedding BLOB,\n                created_at INTEGER,\n                last_accessed INTEGER,\n                access_count INTEGER,\n                importance REAL\n            )\n        ')
        conn.execute('\n            CREATE TABLE IF NOT EXISTS relations (\n                id INTEGER PRIMARY KEY AUTOINCREMENT,\n                source TEXT NOT NULL,\n                target TEXT NOT NULL,\n                relation_type TEXT NOT NULL,\n                strength REAL,\n                created_at INTEGER,\n                context TEXT,\n                FOREIGN KEY (source) REFERENCES concepts(id),\n                FOREIGN KEY (target) REFERENCES concepts(id)\n            )\n        ')
        conn.execute('CREATE INDEX IF NOT EXISTS idx_concept_type ON concepts(concept_type)')
        conn.execute('CREATE INDEX IF NOT EXISTS idx_relation_source ON relations(source)')
        conn.execute('CREATE INDEX IF NOT EXISTS idx_relation_target ON relations(target)')
        conn.execute('CREATE INDEX IF NOT EXISTS idx_relation_type ON relations(relation_type)')
        conn.commit()
        conn.close()
        logger.info(f'Initialized hypergraph memory database at {self.db_path}')
    def _load_graph(self):
        if not NETWORKX_AVAILABLE:
            return
        conn = sqlite3.connect(self.db_path)
        cursor = conn.execute('SELECT id, name, concept_type, properties FROM concepts')
        for row in cursor:
            concept_id, name, concept_type, properties_json = row
            properties = json.loads(properties_json) if properties_json else {}
            self.graph.add_node(concept_id, name=name, concept_type=concept_type, **properties)
        cursor = conn.execute('SELECT source, target, relation_type, strength, context FROM relations')
        for row in cursor:
            source, target, relation_type, strength, context_json = row
            context = json.loads(context_json) if context_json else {}
            self.graph.add_edge(source, target, relation=relation_type, strength=strength, **context)
        conn.close()
        logger.info(f'Loaded {self.graph.number_of_nodes()} concepts and {self.graph.number_of_edges()} relations')
    def add_concept(self, concept: Concept) -> bool:
        now = int(datetime.now().timestamp() * 1000)
        concept.created_at = now
        concept.last_accessed = now
        if self.embedder and concept.embedding is None:
            try:
                concept.embedding = self.embedder.encode(concept.name)
            except Exception as e:
                logger.warning(f'Failed to generate embedding: {e}')
        if NETWORKX_AVAILABLE:
            self.graph.add_node(concept.id, name=concept.name, concept_type=concept.concept_type, **concept.properties)
        conn = sqlite3.connect(self.db_path)
        try:
            embedding_blob = concept.embedding.tobytes() if concept.embedding is not None else None
            conn.execute('\n                INSERT OR REPLACE INTO concepts \n                (id, name, concept_type, properties, embedding, created_at, last_accessed, access_count, importance)\n                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)\n            ', (concept.id, concept.name, concept.concept_type, json.dumps(concept.properties), embedding_blob, concept.created_at, concept.last_accessed, concept.access_count, concept.importance))
            conn.commit()
            logger.info(f'Added concept: {concept.name} ({concept.concept_type})')
            return True
        except Exception as e:
            logger.error(f'Failed to add concept: {e}')
            return False
        finally:
            conn.close()
    def add_relation(self, relation: Relation) -> bool:
        now = int(datetime.now().timestamp() * 1000)
        relation.created_at = now
        if NETWORKX_AVAILABLE:
            self.graph.add_edge(relation.source, relation.target, relation=relation.relation_type, strength=relation.strength, **relation.context or {})
        conn = sqlite3.connect(self.db_path)
        try:
            conn.execute('\n                INSERT INTO relations (source, target, relation_type, strength, created_at, context)\n                VALUES (?, ?, ?, ?, ?, ?)\n            ', (relation.source, relation.target, relation.relation_type, relation.strength, relation.created_at, json.dumps(relation.context) if relation.context else None))
            conn.commit()
            logger.info(f'Added relation: {relation.source} --[{relation.relation_type}]--> {relation.target}')
            return True
        except Exception as e:
            logger.error(f'Failed to add relation: {e}')
            return False
        finally:
            conn.close()
    def find_related(self, concept_id: str, max_distance: int=2) -> List[Tuple[str, int]]:
        if not NETWORKX_AVAILABLE or concept_id not in self.graph:
            return []
        try:
            distances = nx.single_source_shortest_path_length(self.graph, concept_id, cutoff=max_distance)
            return [(node, dist) for node, dist in distances.items() if node != concept_id]
        except Exception as e:
            logger.error(f'Failed to find related concepts: {e}')
            return []
    def find_similar_concepts(self, query: str, top_k: int=5) -> List[Tuple[str, float]]:
        if not self.embedder:
            logger.warning('Embedder not available - cannot find similar concepts')
            return []
        try:
            query_embedding = self.embedder.encode(query)
            conn = sqlite3.connect(self.db_path)
            cursor = conn.execute('SELECT id, name, embedding FROM concepts WHERE embedding IS NOT NULL')
            similarities = []
            for row in cursor:
                concept_id, name, embedding_blob = row
                if embedding_blob:
                    embedding = np.frombuffer(embedding_blob, dtype=np.float32)
                    similarity = np.dot(query_embedding, embedding) / (np.linalg.norm(query_embedding) * np.linalg.norm(embedding))
                    similarities.append((concept_id, float(similarity)))
            conn.close()
            similarities.sort(key=lambda x: x[1], reverse=True)
            return similarities[:top_k]
        except Exception as e:
            logger.error(f'Failed to find similar concepts: {e}')
            return []
    def get_concept(self, concept_id: str) -> Optional[Concept]:
        conn = sqlite3.connect(self.db_path)
        cursor = conn.execute('SELECT id, name, concept_type, properties, embedding, created_at, last_accessed, access_count, importance FROM concepts WHERE id = ?', (concept_id,))
        row = cursor.fetchone()
        conn.close()
        if not row:
            return None
        concept_id, name, concept_type, properties_json, embedding_blob, created_at, last_accessed, access_count, importance = row
        embedding = None
        if embedding_blob:
            embedding = np.frombuffer(embedding_blob, dtype=np.float32)
        return Concept(id=concept_id, name=name, concept_type=concept_type, properties=json.loads(properties_json) if properties_json else {}, embedding=embedding, created_at=created_at, last_accessed=last_accessed, access_count=access_count, importance=importance)
    def update_access(self, concept_id: str):
        now = int(datetime.now().timestamp() * 1000)
        conn = sqlite3.connect(self.db_path)
        conn.execute('\n            UPDATE concepts \n            SET last_accessed = ?, access_count = access_count + 1\n            WHERE id = ?\n        ', (now, concept_id))
        conn.commit()
        conn.close()
    def get_memory_stats(self) -> Dict[str, Any]:
        conn = sqlite3.connect(self.db_path)
        cursor = conn.execute('\n            SELECT concept_type, COUNT(*) FROM concepts GROUP BY concept_type\n        ')
        concept_counts = dict(cursor.fetchall())
        cursor = conn.execute('SELECT COUNT(*) FROM relations')
        total_relations = cursor.fetchone()[0]
        cursor = conn.execute('\n            SELECT name, access_count FROM concepts \n            ORDER BY access_count DESC LIMIT 10\n        ')
        top_concepts = cursor.fetchall()
        conn.close()
        return {'total_concepts': sum(concept_counts.values()), 'concepts_by_type': concept_counts, 'total_relations': total_relations, 'top_accessed_concepts': top_concepts, 'graph_available': NETWORKX_AVAILABLE, 'embeddings_available': self.embedder is not None}
if __name__ == '__main__':
    memory = HypergraphMemory()
    memory.add_concept(Concept(id='wisdom_1', name='Wisdom is knowing that you know nothing', concept_type='declarative', properties={'source': 'Socrates', 'domain': 'philosophy'}))
    memory.add_concept(Concept(id='skill_1', name='Critical thinking', concept_type='procedural', properties={'category': 'cognitive', 'difficulty': 'intermediate'}))
    memory.add_relation(Relation(source='wisdom_1', target='skill_1', relation_type='requires', strength=0.8))
    stats = memory.get_memory_stats()
    print(json.dumps(stats, indent=2))