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
    id: str
    type: str
    data: Dict[str, Any]
    metadata: Dict[str, Any]
    activation_strength: float = 0.0
    @classmethod
    def from_db_row(cls, row):
        return cls(id=str(row['id']), type=row['type'], data=row['data'], metadata=row.get('metadata', {}), activation_strength=0.0)
@dataclass
class EchoPropagationResult:
    node_id: str
    depth: int
    accumulated_weight: float
    path: List[str]
@dataclass
class InteractionData:
    trigger_id: str
    affected_nodes: List[str]
    strength: float
    context: Dict[str, Any]
    timestamp: datetime
class EchoPropagationEngine:
    def __init__(self, hypergraph_service):
        self.hypergraph = hypergraph_service
        self.propagation_cache = LRUCache(maxsize=1000)
    async def propagate(self, start_nodes: List[str], max_depth: int=3, min_weight: float=0.1) -> List[EchoPropagationResult]:
        cache_key = f'{sorted(start_nodes)}_{max_depth}_{min_weight}'
        if cache_key in self.propagation_cache:
            logger.debug(f'Cache hit for propagation: {cache_key}')
            return self.propagation_cache[cache_key]
        results = []
        for start_node in start_nodes:
            node_results = await self.hypergraph.propagate_activation(start_node, max_depth, min_weight)
            results.extend(node_results)
        unique_results = {}
        for result in results:
            if result.node_id not in unique_results or result.accumulated_weight > unique_results[result.node_id].accumulated_weight:
                unique_results[result.node_id] = result
        final_results = list(unique_results.values())
        final_results.sort(key=lambda x: x.accumulated_weight, reverse=True)
        self.propagation_cache[cache_key] = final_results
        return final_results
class HypergraphService:
    def __init__(self, neon_connection_string: str, supabase_client=None):
        self.neon_connection_string = neon_connection_string
        self.supabase = supabase_client
        self.connection_pool = None
        self.propagation_cache = LRUCache(maxsize=1000)
        self.identity_cache = LRUCache(maxsize=500)
    async def initialize(self):
        self.connection_pool = await asyncpg.create_pool(self.neon_connection_string, min_size=5, max_size=20, command_timeout=60)
        logger.info('Hypergraph service initialized with connection pool')
    async def close(self):
        if self.connection_pool:
            await self.connection_pool.close()
    async def get_identity_fragments(self, context_keywords: str) -> List[IdentityFragment]:
        cache_key = f'fragments_{context_keywords}'
        if cache_key in self.identity_cache:
            return self.identity_cache[cache_key]
        async with self.connection_pool.acquire() as conn:
            query = "\n            SELECT h.*, similarity(h.data->>'description', $1) as relevance\n            FROM hypernodes h\n            WHERE h.data->>'description' % $1\n            ORDER BY relevance DESC\n            LIMIT 10\n            "
            rows = await conn.fetch(query, context_keywords)
            fragments = [IdentityFragment.from_db_row(row) for row in rows]
            self.identity_cache[cache_key] = fragments
            return fragments
    async def propagate_activation(self, start_node_id: str, max_depth: int=3, min_weight: float=0.1) -> List[EchoPropagationResult]:
        async with self.connection_pool.acquire() as conn:
            query = 'SELECT * FROM simulate_echo_propagation($1, $2, $3)'
            rows = await conn.fetch(query, start_node_id, max_depth, min_weight)
            results = []
            for row in rows:
                result = EchoPropagationResult(node_id=str(row['node_id']), depth=row['depth'], accumulated_weight=row['accumulated_weight'], path=[str(node_id) for node_id in row['path']])
                results.append(result)
            return results
    async def update_from_interaction(self, interaction_data: InteractionData):
        async with self.connection_pool.acquire() as conn:
            await conn.execute('\n                INSERT INTO echo_propagation_events \n                (trigger_node_id, affected_nodes, propagation_strength, context, timestamp)\n                VALUES ($1, $2, $3, $4, $5)\n            ', interaction_data.trigger_id, interaction_data.affected_nodes, interaction_data.strength, json.dumps(interaction_data.context), interaction_data.timestamp)
            await self._update_relationship_weights(conn, interaction_data)
    async def _update_relationship_weights(self, conn, interaction_data: InteractionData):
        for i, node1 in enumerate(interaction_data.affected_nodes):
            for node2 in interaction_data.affected_nodes[i + 1:]:
                existing_edge = await conn.fetchrow('\n                    SELECT id, weight FROM hyperedges \n                    WHERE $1 = ANY(nodes) AND $2 = ANY(nodes)\n                ', node1, node2)
                if existing_edge:
                    new_weight = min(1.0, existing_edge['weight'] + 0.01 * interaction_data.strength)
                    await conn.execute('\n                        UPDATE hyperedges SET weight = $1, updated_at = NOW()\n                        WHERE id = $2\n                    ', new_weight, existing_edge['id'])
                elif interaction_data.strength > 0.5:
                    await conn.execute("\n                            INSERT INTO hyperedges (nodes, type, weight, metadata)\n                            VALUES ($1, 'association', $2, $3)\n                        ", [node1, node2], 0.1 * interaction_data.strength, json.dumps({'created_from': 'co_activation', 'context': interaction_data.context}))
    async def get_active_configuration(self) -> Dict[str, Any]:
        async with self.connection_pool.acquire() as conn:
            config = await conn.fetchrow('\n                SELECT * FROM echoself_configurations \n                WHERE is_active = TRUE \n                ORDER BY updated_at DESC \n                LIMIT 1\n            ')
            if config:
                return {'id': str(config['id']), 'name': config['name'], 'description': config['description'], 'active_nodes': config['active_nodes'] or [], 'configuration_data': config['configuration_data'] or {}, 'core_nodes': config['active_nodes'][:5] if config['active_nodes'] else []}
            else:
                return {'core_nodes': []}
    async def create_identity_fragment(self, fragment_type: str, data: Dict[str, Any], metadata: Dict[str, Any]=None) -> str:
        async with self.connection_pool.acquire() as conn:
            fragment_id = await conn.fetchval('\n                INSERT INTO hypernodes (type, data, metadata)\n                VALUES ($1, $2, $3)\n                RETURNING id\n            ', fragment_type, json.dumps(data), json.dumps(metadata or {}))
            self.identity_cache.clear()
            return str(fragment_id)
    async def create_relationship(self, node_ids: List[str], relationship_type: str, weight: float=0.5, metadata: Dict[str, Any]=None) -> str:
        async with self.connection_pool.acquire() as conn:
            relationship_id = await conn.fetchval('\n                INSERT INTO hyperedges (nodes, type, weight, metadata)\n                VALUES ($1, $2, $3, $4)\n                RETURNING id\n            ', node_ids, relationship_type, weight, json.dumps(metadata or {}))
            self.propagation_cache.clear()
            return str(relationship_id)
    async def get_node_by_id(self, node_id: str) -> Optional[IdentityFragment]:
        async with self.connection_pool.acquire() as conn:
            row = await conn.fetchrow('\n                SELECT * FROM hypernodes WHERE id = $1\n            ', node_id)
            if row:
                return IdentityFragment.from_db_row(row)
            return None
    async def search_nodes_by_type(self, node_type: str, limit: int=50) -> List[IdentityFragment]:
        cache_key = f'type_{node_type}_{limit}'
        if cache_key in self.identity_cache:
            return self.identity_cache[cache_key]
        async with self.connection_pool.acquire() as conn:
            rows = await conn.fetch('\n                SELECT * FROM hypernodes \n                WHERE type = $1 \n                ORDER BY created_at DESC \n                LIMIT $2\n            ', node_type, limit)
            fragments = [IdentityFragment.from_db_row(row) for row in rows]
            self.identity_cache[cache_key] = fragments
            return fragments
    async def get_node_relationships(self, node_id: str) -> List[Dict[str, Any]]:
        async with self.connection_pool.acquire() as conn:
            rows = await conn.fetch('\n                SELECT he.*, array_length(he.nodes, 1) as node_count\n                FROM hyperedges he\n                WHERE $1 = ANY(he.nodes)\n                ORDER BY he.weight DESC\n            ', node_id)
            relationships = []
            for row in rows:
                relationships.append({'id': str(row['id']), 'type': row['type'], 'weight': row['weight'], 'nodes': [str(node) for node in row['nodes']], 'metadata': row.get('metadata', {}), 'node_count': row['node_count']})
            return relationships
    async def get_statistics(self) -> Dict[str, Any]:
        async with self.connection_pool.acquire() as conn:
            stats = await conn.fetchrow('\n                SELECT \n                    (SELECT COUNT(*) FROM hypernodes) as total_nodes,\n                    (SELECT COUNT(*) FROM hyperedges) as total_edges,\n                    (SELECT COUNT(*) FROM echo_propagation_events) as total_events,\n                    (SELECT AVG(weight) FROM hyperedges) as avg_edge_weight,\n                    (SELECT COUNT(DISTINCT type) FROM hypernodes) as node_types,\n                    (SELECT COUNT(DISTINCT type) FROM hyperedges) as edge_types\n            ')
            return {'total_nodes': stats['total_nodes'], 'total_edges': stats['total_edges'], 'total_events': stats['total_events'], 'average_edge_weight': float(stats['avg_edge_weight'] or 0), 'node_types': stats['node_types'], 'edge_types': stats['edge_types'], 'cache_stats': {'propagation_cache_size': len(self.propagation_cache), 'identity_cache_size': len(self.identity_cache)}}
class HypergraphCacheManager:
    def __init__(self):
        self.l1_cache = LRUCache(maxsize=100)
        self.l2_cache = LRUCache(maxsize=1000)
        self.l3_cache = LRUCache(maxsize=5000)
    async def get_propagation_result(self, cache_key: str, compute_func):
        if cache_key in self.l1_cache:
            return self.l1_cache[cache_key]
        if cache_key in self.l2_cache:
            result = self.l2_cache[cache_key]
            self.l1_cache[cache_key] = result
            return result
        if cache_key in self.l3_cache:
            result = self.l3_cache[cache_key]
            self.l2_cache[cache_key] = result
            return result
        result = await compute_func()
        self.l3_cache[cache_key] = result
        return result
    def clear_all_caches(self):
        self.l1_cache.clear()
        self.l2_cache.clear()
        self.l3_cache.clear()
    def get_cache_stats(self) -> Dict[str, Any]:
        return {'l1_size': len(self.l1_cache), 'l1_maxsize': self.l1_cache.maxsize, 'l2_size': len(self.l2_cache), 'l2_maxsize': self.l2_cache.maxsize, 'l3_size': len(self.l3_cache), 'l3_maxsize': self.l3_cache.maxsize}
hypergraph_service: Optional[HypergraphService] = None
async def get_hypergraph_service() -> HypergraphService:
    global hypergraph_service
    if hypergraph_service is None:
        raise RuntimeError('Hypergraph service not initialized')
    return hypergraph_service
async def initialize_hypergraph_service(neon_connection_string: str, supabase_client=None):
    global hypergraph_service
    hypergraph_service = HypergraphService(neon_connection_string, supabase_client)
    await hypergraph_service.initialize()
    logger.info('Global hypergraph service initialized')
async def shutdown_hypergraph_service():
    global hypergraph_service
    if hypergraph_service:
        await hypergraph_service.close()
        hypergraph_service = None
        logger.info('Global hypergraph service shutdown')