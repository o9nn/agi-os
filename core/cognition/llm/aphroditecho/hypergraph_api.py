#!/usr/bin/env python3
"""
Hypergraph API Integration for EchoCog/Aphroditecho
Provides REST API endpoints for hypergraph operations and identity management.
"""

import asyncio
import logging
from typing import Dict, List, Optional, Any
from datetime import datetime
from pydantic import BaseModel, Field
from fastapi import APIRouter, HTTPException, Depends, BackgroundTasks
from fastapi.responses import JSONResponse

from hypergraph_service import (
    get_hypergraph_service,
    HypergraphService,
    IdentityFragment,
    InteractionData,
    EchoPropagationResult
)

logger = logging.getLogger(__name__)

# Pydantic models for API requests and responses
class EchoPropagationRequest(BaseModel):
    """Request model for echo propagation."""
    start_nodes: List[str] = Field(..., description="List of starting node IDs")
    max_depth: int = Field(3, ge=1, le=10, description="Maximum propagation depth")
    min_weight: float = Field(0.1, ge=0.0, le=1.0, description="Minimum weight threshold")

class EchoPropagationResponse(BaseModel):
    """Response model for echo propagation."""
    propagated_nodes: List[Dict[str, Any]]
    activation_strength: float
    propagation_paths: List[List[str]]
    total_nodes: int
    execution_time: float

class IdentityContextRequest(BaseModel):
    """Request model for identity context."""
    context: str = Field(..., description="Context keywords or description")
    limit: int = Field(10, ge=1, le=50, description="Maximum number of fragments to return")

class IdentityContextResponse(BaseModel):
    """Response model for identity context."""
    context: str
    active_fragments: List[Dict[str, Any]]
    dominant_persona: Optional[str]
    confidence_score: float
    total_fragments: int

class HypergraphUpdateRequest(BaseModel):
    """Request model for hypergraph updates."""
    interaction_data: Dict[str, Any] = Field(..., description="Interaction outcome data")

class CreateFragmentRequest(BaseModel):
    """Request model for creating identity fragments."""
    type: str = Field(..., description="Fragment type (persona, skill, memory, belief, value, trait, goal, context)")
    data: Dict[str, Any] = Field(..., description="Fragment data")
    metadata: Optional[Dict[str, Any]] = Field(None, description="Optional metadata")

class CreateRelationshipRequest(BaseModel):
    """Request model for creating relationships."""
    node_ids: List[str] = Field(..., min_items=2, description="List of node IDs to connect")
    relationship_type: str = Field(..., description="Type of relationship")
    weight: float = Field(0.5, ge=0.0, le=1.0, description="Relationship weight")
    metadata: Optional[Dict[str, Any]] = Field(None, description="Optional metadata")

class HypergraphStatsResponse(BaseModel):
    """Response model for hypergraph statistics."""
    total_nodes: int
    total_edges: int
    total_events: int
    average_edge_weight: float
    node_types: int
    edge_types: int
    cache_stats: Dict[str, Any]

# Create API router
hypergraph_router = APIRouter(prefix="/v1/hypergraph", tags=["hypergraph"])

@hypergraph_router.post("/propagate", response_model=EchoPropagationResponse)
async def propagate_echo(
    request: EchoPropagationRequest,
    hypergraph: HypergraphService = Depends(get_hypergraph_service)
):
    """Trigger echo propagation in the hypergraph."""
    start_time = datetime.now()
    
    try:
        # Execute propagation for each start node
        all_results = []
        for start_node in request.start_nodes:
            results = await hypergraph.propagate_activation(
                start_node, request.max_depth, request.min_weight
            )
            all_results.extend(results)
        
        # Remove duplicates and sort by weight
        unique_results = {}
        for result in all_results:
            if result.node_id not in unique_results or \
               result.accumulated_weight > unique_results[result.node_id].accumulated_weight:
                unique_results[result.node_id] = result
        
        final_results = list(unique_results.values())
        final_results.sort(key=lambda x: x.accumulated_weight, reverse=True)
        
        # Calculate total activation strength
        total_activation = sum(r.accumulated_weight for r in final_results)
        
        # Extract propagation paths
        propagation_paths = [r.path for r in final_results[:10]]  # Top 10 paths
        
        # Convert results to dict format
        propagated_nodes = [
            {
                "node_id": r.node_id,
                "depth": r.depth,
                "accumulated_weight": r.accumulated_weight,
                "path": r.path
            }
            for r in final_results
        ]
        
        execution_time = (datetime.now() - start_time).total_seconds()
        
        return EchoPropagationResponse(
            propagated_nodes=propagated_nodes,
            activation_strength=total_activation,
            propagation_paths=propagation_paths,
            total_nodes=len(final_results),
            execution_time=execution_time
        )
        
    except Exception as e:
        logger.error(f"Error in echo propagation: {e}")
        raise HTTPException(status_code=500, detail=f"Echo propagation failed: {str(e)}")

@hypergraph_router.get("/identity/{context}", response_model=IdentityContextResponse)
async def get_identity_context(
    context: str,
    limit: int = 10,
    hypergraph: HypergraphService = Depends(get_hypergraph_service)
):
    """Get identity context for a given scenario."""
    try:
        # Get identity fragments for context
        fragments = await hypergraph.get_identity_fragments(context)
        
        # Limit results
        limited_fragments = fragments[:limit]
        
        # Convert fragments to dict format
        fragment_dicts = []
        for fragment in limited_fragments:
            fragment_dict = {
                "id": fragment.id,
                "type": fragment.type,
                "data": fragment.data,
                "metadata": fragment.metadata,
                "activation_strength": fragment.activation_strength
            }
            fragment_dicts.append(fragment_dict)
        
        # Determine dominant persona
        persona_fragments = [f for f in limited_fragments if f.type == 'persona']
        dominant_persona = None
        if persona_fragments:
            dominant = max(persona_fragments, key=lambda f: f.activation_strength)
            dominant_persona = dominant.data.get('name', 'unknown')
        
        # Calculate confidence score
        confidence_score = 0.5
        if limited_fragments:
            confidence_score = sum(f.activation_strength for f in limited_fragments) / len(limited_fragments)
        
        return IdentityContextResponse(
            context=context,
            active_fragments=fragment_dicts,
            dominant_persona=dominant_persona,
            confidence_score=confidence_score,
            total_fragments=len(fragments)
        )
        
    except Exception as e:
        logger.error(f"Error getting identity context: {e}")
        raise HTTPException(status_code=500, detail=f"Identity context retrieval failed: {str(e)}")

@hypergraph_router.post("/update")
async def update_hypergraph(
    request: HypergraphUpdateRequest,
    background_tasks: BackgroundTasks,
    hypergraph: HypergraphService = Depends(get_hypergraph_service)
):
    """Update hypergraph based on interaction outcomes."""
    try:
        # Convert request data to InteractionData
        interaction_data = InteractionData(
            trigger_id=request.interaction_data.get('trigger_id', ''),
            affected_nodes=request.interaction_data.get('affected_nodes', []),
            strength=request.interaction_data.get('strength', 0.5),
            context=request.interaction_data.get('context', {}),
            timestamp=datetime.now()
        )
        
        # Update hypergraph in background
        background_tasks.add_task(
            hypergraph.update_from_interaction,
            interaction_data
        )
        
        return {"status": "update_queued", "timestamp": datetime.now().isoformat()}
        
    except Exception as e:
        logger.error(f"Error updating hypergraph: {e}")
        raise HTTPException(status_code=500, detail=f"Hypergraph update failed: {str(e)}")

@hypergraph_router.post("/fragments", response_model=Dict[str, str])
async def create_identity_fragment(
    request: CreateFragmentRequest,
    hypergraph: HypergraphService = Depends(get_hypergraph_service)
):
    """Create a new identity fragment."""
    try:
        # Validate fragment type
        valid_types = ['persona', 'skill', 'memory', 'belief', 'value', 'trait', 'goal', 'context']
        if request.type not in valid_types:
            raise HTTPException(
                status_code=400, 
                detail=f"Invalid fragment type. Must be one of: {valid_types}"
            )
        
        # Create fragment
        fragment_id = await hypergraph.create_identity_fragment(
            request.type, request.data, request.metadata
        )
        
        return {
            "fragment_id": fragment_id,
            "type": request.type,
            "status": "created"
        }
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error creating identity fragment: {e}")
        raise HTTPException(status_code=500, detail=f"Fragment creation failed: {str(e)}")

@hypergraph_router.post("/relationships", response_model=Dict[str, str])
async def create_relationship(
    request: CreateRelationshipRequest,
    hypergraph: HypergraphService = Depends(get_hypergraph_service)
):
    """Create a new relationship between identity fragments."""
    try:
        # Validate relationship type
        valid_types = ['association', 'causality', 'contradiction', 'synergy', 'hierarchy', 'temporal', 'contextual']
        if request.relationship_type not in valid_types:
            raise HTTPException(
                status_code=400, 
                detail=f"Invalid relationship type. Must be one of: {valid_types}"
            )
        
        # Create relationship
        relationship_id = await hypergraph.create_relationship(
            request.node_ids, request.relationship_type, request.weight, request.metadata
        )
        
        return {
            "relationship_id": relationship_id,
            "type": request.relationship_type,
            "status": "created"
        }
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error creating relationship: {e}")
        raise HTTPException(status_code=500, detail=f"Relationship creation failed: {str(e)}")

@hypergraph_router.get("/fragments/{fragment_id}")
async def get_fragment_by_id(
    fragment_id: str,
    hypergraph: HypergraphService = Depends(get_hypergraph_service)
):
    """Get a specific identity fragment by ID."""
    try:
        fragment = await hypergraph.get_node_by_id(fragment_id)
        
        if not fragment:
            raise HTTPException(status_code=404, detail="Fragment not found")
        
        return {
            "id": fragment.id,
            "type": fragment.type,
            "data": fragment.data,
            "metadata": fragment.metadata,
            "activation_strength": fragment.activation_strength
        }
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error getting fragment: {e}")
        raise HTTPException(status_code=500, detail=f"Fragment retrieval failed: {str(e)}")

@hypergraph_router.get("/fragments/{fragment_id}/relationships")
async def get_fragment_relationships(
    fragment_id: str,
    hypergraph: HypergraphService = Depends(get_hypergraph_service)
):
    """Get all relationships for a specific fragment."""
    try:
        relationships = await hypergraph.get_node_relationships(fragment_id)
        
        return {
            "fragment_id": fragment_id,
            "relationships": relationships,
            "total_relationships": len(relationships)
        }
        
    except Exception as e:
        logger.error(f"Error getting fragment relationships: {e}")
        raise HTTPException(status_code=500, detail=f"Relationship retrieval failed: {str(e)}")

@hypergraph_router.get("/fragments/type/{fragment_type}")
async def get_fragments_by_type(
    fragment_type: str,
    limit: int = 50,
    hypergraph: HypergraphService = Depends(get_hypergraph_service)
):
    """Get identity fragments by type."""
    try:
        fragments = await hypergraph.search_nodes_by_type(fragment_type, limit)
        
        fragment_dicts = []
        for fragment in fragments:
            fragment_dict = {
                "id": fragment.id,
                "type": fragment.type,
                "data": fragment.data,
                "metadata": fragment.metadata,
                "activation_strength": fragment.activation_strength
            }
            fragment_dicts.append(fragment_dict)
        
        return {
            "fragment_type": fragment_type,
            "fragments": fragment_dicts,
            "total_fragments": len(fragment_dicts)
        }
        
    except Exception as e:
        logger.error(f"Error getting fragments by type: {e}")
        raise HTTPException(status_code=500, detail=f"Fragment search failed: {str(e)}")

@hypergraph_router.get("/stats", response_model=HypergraphStatsResponse)
async def get_hypergraph_statistics(
    hypergraph: HypergraphService = Depends(get_hypergraph_service)
):
    """Get hypergraph statistics and metrics."""
    try:
        stats = await hypergraph.get_statistics()
        
        return HypergraphStatsResponse(
            total_nodes=stats['total_nodes'],
            total_edges=stats['total_edges'],
            total_events=stats['total_events'],
            average_edge_weight=stats['average_edge_weight'],
            node_types=stats['node_types'],
            edge_types=stats['edge_types'],
            cache_stats=stats['cache_stats']
        )
        
    except Exception as e:
        logger.error(f"Error getting hypergraph statistics: {e}")
        raise HTTPException(status_code=500, detail=f"Statistics retrieval failed: {str(e)}")

@hypergraph_router.get("/health")
async def hypergraph_health_check(
    hypergraph: HypergraphService = Depends(get_hypergraph_service)
):
    """Health check for hypergraph service."""
    try:
        # Test database connection
        stats = await hypergraph.get_statistics()
        
        return {
            "status": "healthy",
            "timestamp": datetime.now().isoformat(),
            "database_connected": True,
            "total_nodes": stats['total_nodes'],
            "total_edges": stats['total_edges']
        }
        
    except Exception as e:
        logger.error(f"Hypergraph health check failed: {e}")
        return JSONResponse(
            status_code=503,
            content={
                "status": "unhealthy",
                "timestamp": datetime.now().isoformat(),
                "error": str(e)
            }
        )

# Utility endpoints for development and debugging
@hypergraph_router.post("/cache/clear")
async def clear_hypergraph_cache(
    hypergraph: HypergraphService = Depends(get_hypergraph_service)
):
    """Clear hypergraph caches (development endpoint)."""
    try:
        hypergraph.propagation_cache.clear()
        hypergraph.identity_cache.clear()
        
        return {
            "status": "cache_cleared",
            "timestamp": datetime.now().isoformat()
        }
        
    except Exception as e:
        logger.error(f"Error clearing cache: {e}")
        raise HTTPException(status_code=500, detail=f"Cache clear failed: {str(e)}")

@hypergraph_router.get("/cache/stats")
async def get_cache_statistics(
    hypergraph: HypergraphService = Depends(get_hypergraph_service)
):
    """Get cache statistics (development endpoint)."""
    try:
        return {
            "propagation_cache_size": len(hypergraph.propagation_cache),
            "propagation_cache_maxsize": hypergraph.propagation_cache.maxsize,
            "identity_cache_size": len(hypergraph.identity_cache),
            "identity_cache_maxsize": hypergraph.identity_cache.maxsize,
            "timestamp": datetime.now().isoformat()
        }
        
    except Exception as e:
        logger.error(f"Error getting cache stats: {e}")
        raise HTTPException(status_code=500, detail=f"Cache stats failed: {str(e)}")

# Export router for integration with main application
__all__ = ['hypergraph_router']
