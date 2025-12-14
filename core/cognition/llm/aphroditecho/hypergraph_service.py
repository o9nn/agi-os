#!/usr/bin/env python3
"""
Hypergraph Service Integration for EchoCog/Aphroditecho
Provides hypergraph operations and identity management for the Echo systems.
"""

import asyncio
import json
import logging
from typing import Dict, List, Optional, Tuple, Any
from dataclasses import dataclass
from datetime import datetime
import asyncpg
from cachetools import LRUCache
import numpy as np

logger = logging.getLogger(__name__)

@dataclass
class IdentityFragment:
    """Represents an identity fragment from the hypergraph."""
    id: str
    type: str
    data: Dict[str, Any]
    metadata: Dict[str, Any]
    activation_strength: float = 0.0
    
    @classmethod
    def from_db_row(cls, row):
        """Create IdentityFragment from database row."""
        return cls(
            id=str(row['id']),
            type=row['type'],
            data=row['data'],
            metadata=row.get('metadata', {}),
            activation_strength=0.0
        )

@dataclass
class EchoPropagationResult:
    """Result of echo propagation through the hypergraph."""
    node_id: str
    depth: int
    accumulated_weight: float
    path: List[str]

@dataclass
class InteractionData:
    """Data structure for interaction outcomes."""
    trigger_id: str
    affected_nodes: List[str]
    strength: float
    context: Dict[str, Any]
    timestamp: datetime

class EchoPropagationEngine:
    """Engine for echo propagation through the hypergraph."""
    
    def __init__(self, hypergraph_service):
        self.hypergraph = hypergraph_service
        self.propagation_cache = LRUCache(maxsize=1000)
        
    async def propagate(self, start_nodes: List[str], max_depth: int = 3, 
                       min_weight: float = 0.1) -> List[EchoPropagationResult]:
        """Execute echo propagation from start nodes."""
        cache_key = f"{sorted(start_nodes)}_{max_depth}_{min_weight}"
        
        if cache_key in self.propagation_cache:
            logger.debug(f"Cache hit for propagation: {cache_key}")
            return self.propagation_cache[cache_key]
        
        results = []
        for start_node in start_nodes:
            node_results = await self.hypergraph.propagate_activation(
                start_node, max_depth, min_weight
            )
            results.extend(node_results)
        
        # Remove duplicates and sort by accumulated weight
        unique_results = {}
        for result in results:
            if result.node_id not in unique_results or \
               result.accumulated_weight > unique_results[result.node_id].accumulated_weight:
                unique_results[result.node_id] = result
        
        final_results = list(unique_results.values())
        final_results.sort(key=lambda x: x.accumulated_weight, reverse=True)
        
        self.propagation_cache[cache_key] = final_results
        return final_results

class HypergraphService:
    """Service layer for hypergraph operations within Echo systems."""
    
    def __init__(self, neon_connection_string: str, supabase_client=None):
        self.neon_connection_string = neon_connection_string
        self.supabase = supabase_client
        self.connection_pool = None
        self.propagation_cache = LRUCache(maxsize=1000)
        self.identity_cache = LRUCache(maxsize=500)
        
    async def initialize(self):
        """Initialize database connection pool."""
        self.connection_pool = await asyncpg.create_pool(
            self.neon_connection_string,
            min_size=5,
            max_size=20,
            command_timeout=60
        )
        logger.info("Hypergraph service initialized with connection pool")
    
    async def close(self):
        """Close database connections."""
        if self.connection_pool:
            await self.connection_pool.close()
    
    async def get_identity_fragments(self, context_keywords: str) -> List[IdentityFragment]:
        """Retrieve relevant identity fragments based on context."""
        cache_key = f"fragments_{context_keywords}"
        
        if cache_key in self.identity_cache:
            return self.identity_cache[cache_key]
        
        async with self.connection_pool.acquire() as conn:
            query = """
            SELECT h.*, similarity(h.data->>'description', $1) as relevance
            FROM hypernodes h
            WHERE h.data->>'description' % $1
            ORDER BY relevance DESC
            LIMIT 10
            """
            
            rows = await conn.fetch(query, context_keywords)
            fragments = [IdentityFragment.from_db_row(row) for row in rows]
            
            self.identity_cache[cache_key] = fragments
            return fragments
    
    async def propagate_activation(self, start_node_id: str, max_depth: int = 3, 
                                 min_weight: float = 0.1) -> List[EchoPropagationResult]:
        """Execute echo propagation through the hypergraph."""
        async with self.connection_pool.acquire() as conn:
            query = "SELECT * FROM simulate_echo_propagation($1, $2, $3)"
            rows = await conn.fetch(query, start_node_id, max_depth, min_weight)
            
            results = []
            for row in rows:
                result = EchoPropagationResult(
                    node_id=str(row['node_id']),
                    depth=row['depth'],
                    accumulated_weight=row['accumulated_weight'],
                    path=[str(node_id) for node_id in row['path']]
                )
                results.append(result)
            
            return results
    
    async def update_from_interaction(self, interaction_data: InteractionData):
        """Update hypergraph based on interaction outcomes."""
        async with self.connection_pool.acquire() as conn:
            # Create new echo propagation event
            await conn.execute("""
                INSERT INTO echo_propagation_events 
                (trigger_node_id, affected_nodes, propagation_strength, context, timestamp)
                VALUES ($1, $2, $3, $4, $5)
            """, 
            interaction_data.trigger_id,
            interaction_data.affected_nodes,
            interaction_data.strength,
            json.dumps(interaction_data.context),
            interaction_data.timestamp
            )
            
            # Update relationship weights based on co-activation
            await self._update_relationship_weights(conn, interaction_data)
    
    async def _update_relationship_weights(self, conn, interaction_data: InteractionData):
        """Update relationship weights based on co-activation patterns."""
        # Find existing relationships between co-activated nodes
        for i, node1 in enumerate(interaction_data.affected_nodes):
            for node2 in interaction_data.affected_nodes[i+1:]:
                # Check if relationship exists
                existing_edge = await conn.fetchrow("""
                    SELECT id, weight FROM hyperedges 
                    WHERE $1 = ANY(nodes) AND $2 = ANY(nodes)
                """, node1, node2)
                
                if existing_edge:
                    # Update existing relationship weight
                    new_weight = min(1.0, existing_edge['weight'] + 0.01 * interaction_data.strength)
                    await conn.execute("""
                        UPDATE hyperedges SET weight = $1, updated_at = NOW()
                        WHERE id = $2
                    """, new_weight, existing_edge['id'])
                else:
                    # Create new relationship if co-activation is strong enough
                    if interaction_data.strength > 0.5:
                        await conn.execute("""
                            INSERT INTO hyperedges (nodes, type, weight, metadata)
                            VALUES ($1, 'association', $2, $3)
                        """, 
                        [node1, node2],
                        0.1 * interaction_data.strength,
                        json.dumps({"created_from": "co_activation", "context": interaction_data.context})
                        )
    
    async def get_active_configuration(self) -> Dict[str, Any]:
        """Get the current active identity configuration."""
        async with self.connection_pool.acquire() as conn:
            config = await conn.fetchrow("""
                SELECT * FROM echoself_configurations 
                WHERE is_active = TRUE 
                ORDER BY updated_at DESC 
                LIMIT 1
            """)
            
            if config:
                return {
                    'id': str(config['id']),
                    'name': config['name'],
                    'description': config['description'],
                    'active_nodes': config['active_nodes'] or [],
                    'configuration_data': config['configuration_data'] or {},
                    'core_nodes': config['active_nodes'][:5] if config['active_nodes'] else []
                }
            else:
                return {'core_nodes': []}
    
    async def create_identity_fragment(self, fragment_type: str, data: Dict[str, Any], 
                                     metadata: Dict[str, Any] = None) -> str:
        """Create a new identity fragment in the hypergraph."""
        async with self.connection_pool.acquire() as conn:
            fragment_id = await conn.fetchval("""
                INSERT INTO hypernodes (type, data, metadata)
                VALUES ($1, $2, $3)
                RETURNING id
            """, fragment_type, json.dumps(data), json.dumps(metadata or {}))
            
            # Clear relevant caches
            self.identity_cache.clear()
            
            return str(fragment_id)
    
    async def create_relationship(self, node_ids: List[str], relationship_type: str, 
                                weight: float = 0.5, metadata: Dict[str, Any] = None) -> str:
        """Create a new relationship between identity fragments."""
        async with self.connection_pool.acquire() as conn:
            relationship_id = await conn.fetchval("""
                INSERT INTO hyperedges (nodes, type, weight, metadata)
                VALUES ($1, $2, $3, $4)
                RETURNING id
            """, node_ids, relationship_type, weight, json.dumps(metadata or {}))
            
            # Clear propagation cache
            self.propagation_cache.clear()
            
            return str(relationship_id)
    
    async def get_node_by_id(self, node_id: str) -> Optional[IdentityFragment]:
        """Get a specific node by ID."""
        async with self.connection_pool.acquire() as conn:
            row = await conn.fetchrow("""
                SELECT * FROM hypernodes WHERE id = $1
            """, node_id)
            
            if row:
                return IdentityFragment.from_db_row(row)
            return None
    
    async def search_nodes_by_type(self, node_type: str, limit: int = 50) -> List[IdentityFragment]:
        """Search nodes by type."""
        cache_key = f"type_{node_type}_{limit}"
        
        if cache_key in self.identity_cache:
            return self.identity_cache[cache_key]
        
        async with self.connection_pool.acquire() as conn:
            rows = await conn.fetch("""
                SELECT * FROM hypernodes 
                WHERE type = $1 
                ORDER BY created_at DESC 
                LIMIT $2
            """, node_type, limit)
            
            fragments = [IdentityFragment.from_db_row(row) for row in rows]
            self.identity_cache[cache_key] = fragments
            return fragments
    
    async def get_node_relationships(self, node_id: str) -> List[Dict[str, Any]]:
        """Get all relationships for a specific node."""
        async with self.connection_pool.acquire() as conn:
            rows = await conn.fetch("""
                SELECT he.*, array_length(he.nodes, 1) as node_count
                FROM hyperedges he
                WHERE $1 = ANY(he.nodes)
                ORDER BY he.weight DESC
            """, node_id)
            
            relationships = []
            for row in rows:
                relationships.append({
                    'id': str(row['id']),
                    'type': row['type'],
                    'weight': row['weight'],
                    'nodes': [str(node) for node in row['nodes']],
                    'metadata': row.get('metadata', {}),
                    'node_count': row['node_count']
                })
            
            return relationships
    
    async def get_statistics(self) -> Dict[str, Any]:
        """Get hypergraph statistics."""
        async with self.connection_pool.acquire() as conn:
            stats = await conn.fetchrow("""
                SELECT 
                    (SELECT COUNT(*) FROM hypernodes) as total_nodes,
                    (SELECT COUNT(*) FROM hyperedges) as total_edges,
                    (SELECT COUNT(*) FROM echo_propagation_events) as total_events,
                    (SELECT AVG(weight) FROM hyperedges) as avg_edge_weight,
                    (SELECT COUNT(DISTINCT type) FROM hypernodes) as node_types,
                    (SELECT COUNT(DISTINCT type) FROM hyperedges) as edge_types
            """)
            
            return {
                'total_nodes': stats['total_nodes'],
                'total_edges': stats['total_edges'],
                'total_events': stats['total_events'],
                'average_edge_weight': float(stats['avg_edge_weight'] or 0),
                'node_types': stats['node_types'],
                'edge_types': stats['edge_types'],
                'cache_stats': {
                    'propagation_cache_size': len(self.propagation_cache),
                    'identity_cache_size': len(self.identity_cache)
                }
            }

class HypergraphCacheManager:
    """Multi-level caching for hypergraph operations."""
    
    def __init__(self):
        self.l1_cache = LRUCache(maxsize=100)    # Hot propagation patterns
        self.l2_cache = LRUCache(maxsize=1000)   # Identity fragments
        self.l3_cache = LRUCache(maxsize=5000)   # Historical queries
        
    async def get_propagation_result(self, cache_key: str, compute_func):
        """Get propagation result with multi-level caching."""
        # Check L1 cache (hot patterns)
        if cache_key in self.l1_cache:
            return self.l1_cache[cache_key]
        
        # Check L2 cache (warm patterns)
        if cache_key in self.l2_cache:
            result = self.l2_cache[cache_key]
            self.l1_cache[cache_key] = result  # Promote to L1
            return result
        
        # Check L3 cache (cold patterns)
        if cache_key in self.l3_cache:
            result = self.l3_cache[cache_key]
            self.l2_cache[cache_key] = result  # Promote to L2
            return result
        
        # Compute and cache
        result = await compute_func()
        self.l3_cache[cache_key] = result
        return result
    
    def clear_all_caches(self):
        """Clear all cache levels."""
        self.l1_cache.clear()
        self.l2_cache.clear()
        self.l3_cache.clear()
    
    def get_cache_stats(self) -> Dict[str, Any]:
        """Get cache statistics."""
        return {
            'l1_size': len(self.l1_cache),
            'l1_maxsize': self.l1_cache.maxsize,
            'l2_size': len(self.l2_cache),
            'l2_maxsize': self.l2_cache.maxsize,
            'l3_size': len(self.l3_cache),
            'l3_maxsize': self.l3_cache.maxsize
        }

# Global hypergraph service instance
hypergraph_service: Optional[HypergraphService] = None

async def get_hypergraph_service() -> HypergraphService:
    """Get the global hypergraph service instance."""
    global hypergraph_service
    if hypergraph_service is None:
        raise RuntimeError("Hypergraph service not initialized")
    return hypergraph_service

async def initialize_hypergraph_service(neon_connection_string: str, supabase_client=None):
    """Initialize the global hypergraph service."""
    global hypergraph_service
    hypergraph_service = HypergraphService(neon_connection_string, supabase_client)
    await hypergraph_service.initialize()
    logger.info("Global hypergraph service initialized")

async def shutdown_hypergraph_service():
    """Shutdown the global hypergraph service."""
    global hypergraph_service
    if hypergraph_service:
        await hypergraph_service.close()
        hypergraph_service = None
        logger.info("Global hypergraph service shutdown")
