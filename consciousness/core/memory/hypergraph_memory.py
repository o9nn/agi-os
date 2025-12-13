#!/usr/bin/env python3
"""
Hypergraph Memory System - Iteration N+9
Implements multi-relational knowledge representation with semantic embeddings
for Deep Tree Echo's persistent memory and wisdom cultivation.
"""

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
    print("⚠️  NetworkX not available - hypergraph features limited")

try:
    from sentence_transformers import SentenceTransformer
    EMBEDDINGS_AVAILABLE = True
except ImportError:
    EMBEDDINGS_AVAILABLE = False
    print("⚠️  Sentence Transformers not available - using simple embeddings")

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


@dataclass
class Concept:
    """Represents a concept node in the hypergraph"""
    id: str
    name: str
    concept_type: str  # declarative, procedural, episodic, intentional
    properties: Dict[str, Any]
    embedding: Optional[np.ndarray] = None
    created_at: int = 0
    last_accessed: int = 0
    access_count: int = 0
    importance: float = 0.5


@dataclass
class Relation:
    """Represents a relation edge in the hypergraph"""
    source: str
    target: str
    relation_type: str
    strength: float = 1.0
    created_at: int = 0
    context: Dict[str, Any] = None


class HypergraphMemory:
    """
    Multi-relational knowledge graph with semantic embeddings.
    Supports declarative, procedural, episodic, and intentional memory.
    """
    
    def __init__(self, db_path: str = "data/hypergraph_memory.db", 
                 embedding_model: str = "all-MiniLM-L6-v2"):
        self.db_path = db_path
        Path(db_path).parent.mkdir(parents=True, exist_ok=True)
        
        # Initialize graph
        if NETWORKX_AVAILABLE:
            self.graph = nx.MultiDiGraph()
        else:
            self.graph = None
            logger.warning("NetworkX not available - graph operations disabled")
        
        # Initialize embedder
        if EMBEDDINGS_AVAILABLE:
            try:
                self.embedder = SentenceTransformer(embedding_model)
                logger.info(f"Loaded embedding model: {embedding_model}")
            except Exception as e:
                logger.warning(f"Failed to load embedding model: {e}")
                self.embedder = None
        else:
            self.embedder = None
        
        # Initialize database
        self._init_db()
        
        # Load existing graph from database
        self._load_graph()
    
    def _init_db(self):
        """Initialize SQLite database for persistent storage"""
        conn = sqlite3.connect(self.db_path)
        
        # Concepts table
        conn.execute("""
            CREATE TABLE IF NOT EXISTS concepts (
                id TEXT PRIMARY KEY,
                name TEXT NOT NULL,
                concept_type TEXT NOT NULL,
                properties TEXT,
                embedding BLOB,
                created_at INTEGER,
                last_accessed INTEGER,
                access_count INTEGER,
                importance REAL
            )
        """)
        
        # Relations table
        conn.execute("""
            CREATE TABLE IF NOT EXISTS relations (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                source TEXT NOT NULL,
                target TEXT NOT NULL,
                relation_type TEXT NOT NULL,
                strength REAL,
                created_at INTEGER,
                context TEXT,
                FOREIGN KEY (source) REFERENCES concepts(id),
                FOREIGN KEY (target) REFERENCES concepts(id)
            )
        """)
        
        # Create indices for faster queries
        conn.execute("CREATE INDEX IF NOT EXISTS idx_concept_type ON concepts(concept_type)")
        conn.execute("CREATE INDEX IF NOT EXISTS idx_relation_source ON relations(source)")
        conn.execute("CREATE INDEX IF NOT EXISTS idx_relation_target ON relations(target)")
        conn.execute("CREATE INDEX IF NOT EXISTS idx_relation_type ON relations(relation_type)")
        
        conn.commit()
        conn.close()
        logger.info(f"Initialized hypergraph memory database at {self.db_path}")
    
    def _load_graph(self):
        """Load graph from database into memory"""
        if not NETWORKX_AVAILABLE:
            return
        
        conn = sqlite3.connect(self.db_path)
        
        # Load concepts
        cursor = conn.execute("SELECT id, name, concept_type, properties FROM concepts")
        for row in cursor:
            concept_id, name, concept_type, properties_json = row
            properties = json.loads(properties_json) if properties_json else {}
            self.graph.add_node(concept_id, name=name, concept_type=concept_type, **properties)
        
        # Load relations
        cursor = conn.execute("SELECT source, target, relation_type, strength, context FROM relations")
        for row in cursor:
            source, target, relation_type, strength, context_json = row
            context = json.loads(context_json) if context_json else {}
            self.graph.add_edge(source, target, relation=relation_type, strength=strength, **context)
        
        conn.close()
        logger.info(f"Loaded {self.graph.number_of_nodes()} concepts and {self.graph.number_of_edges()} relations")
    
    def add_concept(self, concept: Concept) -> bool:
        """Add a concept to the hypergraph"""
        now = int(datetime.now().timestamp() * 1000)
        concept.created_at = now
        concept.last_accessed = now
        
        # Generate embedding if available
        if self.embedder and concept.embedding is None:
            try:
                concept.embedding = self.embedder.encode(concept.name)
            except Exception as e:
                logger.warning(f"Failed to generate embedding: {e}")
        
        # Add to graph
        if NETWORKX_AVAILABLE:
            self.graph.add_node(
                concept.id,
                name=concept.name,
                concept_type=concept.concept_type,
                **concept.properties
            )
        
        # Persist to database
        conn = sqlite3.connect(self.db_path)
        try:
            embedding_blob = concept.embedding.tobytes() if concept.embedding is not None else None
            conn.execute("""
                INSERT OR REPLACE INTO concepts 
                (id, name, concept_type, properties, embedding, created_at, last_accessed, access_count, importance)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
            """, (
                concept.id,
                concept.name,
                concept.concept_type,
                json.dumps(concept.properties),
                embedding_blob,
                concept.created_at,
                concept.last_accessed,
                concept.access_count,
                concept.importance
            ))
            conn.commit()
            logger.info(f"Added concept: {concept.name} ({concept.concept_type})")
            return True
        except Exception as e:
            logger.error(f"Failed to add concept: {e}")
            return False
        finally:
            conn.close()
    
    def add_relation(self, relation: Relation) -> bool:
        """Add a relation between concepts"""
        now = int(datetime.now().timestamp() * 1000)
        relation.created_at = now
        
        # Add to graph
        if NETWORKX_AVAILABLE:
            self.graph.add_edge(
                relation.source,
                relation.target,
                relation=relation.relation_type,
                strength=relation.strength,
                **(relation.context or {})
            )
        
        # Persist to database
        conn = sqlite3.connect(self.db_path)
        try:
            conn.execute("""
                INSERT INTO relations (source, target, relation_type, strength, created_at, context)
                VALUES (?, ?, ?, ?, ?, ?)
            """, (
                relation.source,
                relation.target,
                relation.relation_type,
                relation.strength,
                relation.created_at,
                json.dumps(relation.context) if relation.context else None
            ))
            conn.commit()
            logger.info(f"Added relation: {relation.source} --[{relation.relation_type}]--> {relation.target}")
            return True
        except Exception as e:
            logger.error(f"Failed to add relation: {e}")
            return False
        finally:
            conn.close()
    
    def find_related(self, concept_id: str, max_distance: int = 2) -> List[Tuple[str, int]]:
        """Find concepts related within max_distance hops"""
        if not NETWORKX_AVAILABLE or concept_id not in self.graph:
            return []
        
        try:
            # Use BFS to find all concepts within max_distance
            distances = nx.single_source_shortest_path_length(
                self.graph, concept_id, cutoff=max_distance
            )
            return [(node, dist) for node, dist in distances.items() if node != concept_id]
        except Exception as e:
            logger.error(f"Failed to find related concepts: {e}")
            return []
    
    def find_similar_concepts(self, query: str, top_k: int = 5) -> List[Tuple[str, float]]:
        """Find most similar concepts using semantic similarity"""
        if not self.embedder:
            logger.warning("Embedder not available - cannot find similar concepts")
            return []
        
        try:
            query_embedding = self.embedder.encode(query)
            
            # Load all concept embeddings from database
            conn = sqlite3.connect(self.db_path)
            cursor = conn.execute("SELECT id, name, embedding FROM concepts WHERE embedding IS NOT NULL")
            
            similarities = []
            for row in cursor:
                concept_id, name, embedding_blob = row
                if embedding_blob:
                    embedding = np.frombuffer(embedding_blob, dtype=np.float32)
                    # Cosine similarity
                    similarity = np.dot(query_embedding, embedding) / (
                        np.linalg.norm(query_embedding) * np.linalg.norm(embedding)
                    )
                    similarities.append((concept_id, float(similarity)))
            
            conn.close()
            
            # Sort by similarity and return top_k
            similarities.sort(key=lambda x: x[1], reverse=True)
            return similarities[:top_k]
        
        except Exception as e:
            logger.error(f"Failed to find similar concepts: {e}")
            return []
    
    def get_concept(self, concept_id: str) -> Optional[Concept]:
        """Retrieve a concept by ID"""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.execute(
            "SELECT id, name, concept_type, properties, embedding, created_at, last_accessed, access_count, importance FROM concepts WHERE id = ?",
            (concept_id,)
        )
        row = cursor.fetchone()
        conn.close()
        
        if not row:
            return None
        
        concept_id, name, concept_type, properties_json, embedding_blob, created_at, last_accessed, access_count, importance = row
        
        embedding = None
        if embedding_blob:
            embedding = np.frombuffer(embedding_blob, dtype=np.float32)
        
        return Concept(
            id=concept_id,
            name=name,
            concept_type=concept_type,
            properties=json.loads(properties_json) if properties_json else {},
            embedding=embedding,
            created_at=created_at,
            last_accessed=last_accessed,
            access_count=access_count,
            importance=importance
        )
    
    def update_access(self, concept_id: str):
        """Update access timestamp and count for a concept"""
        now = int(datetime.now().timestamp() * 1000)
        conn = sqlite3.connect(self.db_path)
        conn.execute("""
            UPDATE concepts 
            SET last_accessed = ?, access_count = access_count + 1
            WHERE id = ?
        """, (now, concept_id))
        conn.commit()
        conn.close()
    
    def get_memory_stats(self) -> Dict[str, Any]:
        """Get statistics about the memory system"""
        conn = sqlite3.connect(self.db_path)
        
        # Count concepts by type
        cursor = conn.execute("""
            SELECT concept_type, COUNT(*) FROM concepts GROUP BY concept_type
        """)
        concept_counts = dict(cursor.fetchall())
        
        # Total relations
        cursor = conn.execute("SELECT COUNT(*) FROM relations")
        total_relations = cursor.fetchone()[0]
        
        # Most accessed concepts
        cursor = conn.execute("""
            SELECT name, access_count FROM concepts 
            ORDER BY access_count DESC LIMIT 10
        """)
        top_concepts = cursor.fetchall()
        
        conn.close()
        
        return {
            "total_concepts": sum(concept_counts.values()),
            "concepts_by_type": concept_counts,
            "total_relations": total_relations,
            "top_accessed_concepts": top_concepts,
            "graph_available": NETWORKX_AVAILABLE,
            "embeddings_available": self.embedder is not None
        }


# Example usage
if __name__ == "__main__":
    memory = HypergraphMemory()
    
    # Add some example concepts
    memory.add_concept(Concept(
        id="wisdom_1",
        name="Wisdom is knowing that you know nothing",
        concept_type="declarative",
        properties={"source": "Socrates", "domain": "philosophy"}
    ))
    
    memory.add_concept(Concept(
        id="skill_1",
        name="Critical thinking",
        concept_type="procedural",
        properties={"category": "cognitive", "difficulty": "intermediate"}
    ))
    
    # Add relation
    memory.add_relation(Relation(
        source="wisdom_1",
        target="skill_1",
        relation_type="requires",
        strength=0.8
    ))
    
    # Get stats
    stats = memory.get_memory_stats()
    print(json.dumps(stats, indent=2))
