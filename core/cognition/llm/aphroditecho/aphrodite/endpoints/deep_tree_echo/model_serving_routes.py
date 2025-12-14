"""
Model Serving Routes for Aphrodite Engine Integration.

This module provides FastAPI routes for the enhanced model serving infrastructure,
implementing Task 8.1.1 requirements with server-side rendering focus.

Routes:
- Model loading and caching management
- Zero-downtime model updates
- Resource allocation monitoring  
- Health checks and status reporting
"""

import asyncio
import json
import logging
import time
from typing import Any, Dict, List, Optional

from fastapi import APIRouter, HTTPException, Request, Response
from pydantic import BaseModel, Field

from .model_serving_manager import ModelServingManager

logger = logging.getLogger(__name__)


class ModelLoadRequest(BaseModel):
    """Request model for loading a model."""
    model_id: str = Field(..., description="Unique identifier for the model")
    version: str = Field(default="latest", description="Model version to load")
    force_reload: bool = Field(default=False, description="Force reload even if cached")


class ModelUpdateRequest(BaseModel):
    """Request model for zero-downtime model updates."""
    model_id: str = Field(..., description="Model to update")
    new_version: str = Field(..., description="New version to deploy")
    rollback_on_failure: bool = Field(default=True, description="Automatically rollback on failure")


class ModelRemoveRequest(BaseModel):
    """Request model for removing a model."""
    model_id: str = Field(..., description="Model to remove")
    version: Optional[str] = Field(default=None, description="Specific version to remove")


class ModelServingRoutes:
    """
    FastAPI routes for model serving management with server-side rendering focus.
    """
    
    def __init__(self, model_serving_manager: ModelServingManager):
        self.model_serving_manager = model_serving_manager
        self.router = APIRouter()
        self._setup_routes()
    
    def _setup_routes(self):
        """Setup all model serving routes."""
        
        @self.router.get("/model_serving/status")
        async def get_serving_status():
            """
            Get comprehensive model serving status.
            
            Returns server-rendered status information including:
            - Active models and versions
            - Performance metrics
            - Resource allocation
            - Health status
            """
            try:
                status = self.model_serving_manager.get_model_serving_status()
                
                # Enhance with server-side metadata
                status.update({
                    "server_rendered": True,
                    "timestamp": time.time(),
                    "serving_manager_active": True,
                    "api_version": "1.0"
                })
                
                return {
                    "status": "success",
                    "data": status,
                    "server_side_rendered": True
                }
                
            except Exception as e:
                logger.error(f"Failed to get serving status: {e}")
                raise HTTPException(
                    status_code=500,
                    detail=f"Failed to retrieve serving status: {str(e)}"
                )
        
        @self.router.post("/model_serving/load")
        async def load_model(request: ModelLoadRequest):
            """
            Load a model with comprehensive configuration.
            
            Implements server-side model loading with DTESN optimizations
            and engine integration.
            """
            try:
                start_time = time.time()
                
                # Check if model is already loaded (unless force reload)
                if not request.force_reload:
                    existing_allocation = await self.model_serving_manager.get_model_resource_allocation(
                        request.model_id
                    )
                    if existing_allocation:
                        logger.info(f"Model {request.model_id} already loaded, returning cached configuration")
                        return {
                            "status": "success",
                            "message": "Model already loaded",
                            "data": existing_allocation,
                            "cached": True,
                            "server_side_rendered": True
                        }
                
                # Load model asynchronously
                model_config = await self.model_serving_manager.load_model_async(
                    model_id=request.model_id,
                    version=request.version
                )
                
                load_time = (time.time() - start_time) * 1000
                
                return {
                    "status": "success",
                    "message": f"Model {request.model_id} loaded successfully",
                    "data": {
                        "model_config": model_config,
                        "load_time_ms": load_time,
                        "version": request.version,
                        "dtesn_optimized": model_config.get("dtesn_optimizations", {}).get("membrane_depth_optimization", False),
                        "engine_integrated": model_config.get("engine_integration", {}).get("available", False)
                    },
                    "server_side_rendered": True
                }
                
            except Exception as e:
                logger.error(f"Failed to load model {request.model_id}: {e}")
                raise HTTPException(
                    status_code=500,
                    detail=f"Failed to load model: {str(e)}"
                )
        
        @self.router.post("/model_serving/update")
        async def update_model_zero_downtime(request: ModelUpdateRequest):
            """
            Update model with zero-downtime deployment.
            
            Implements comprehensive zero-downtime updates with health monitoring
            and automatic rollback capabilities.
            """
            try:
                logger.info(f"Starting zero-downtime update for {request.model_id} -> {request.new_version}")
                
                # Perform zero-downtime update
                success = await self.model_serving_manager.update_model_zero_downtime(
                    model_id=request.model_id,
                    new_version=request.new_version
                )
                
                if success:
                    # Get updated model status
                    updated_allocation = await self.model_serving_manager.get_model_resource_allocation(
                        request.model_id
                    )
                    
                    return {
                        "status": "success",
                        "message": f"Zero-downtime update completed for {request.model_id}",
                        "data": {
                            "model_id": request.model_id,
                            "new_version": request.new_version,
                            "update_successful": True,
                            "resource_allocation": updated_allocation,
                            "zero_downtime": True
                        },
                        "server_side_rendered": True
                    }
                else:
                    return {
                        "status": "failed",
                        "message": f"Zero-downtime update failed for {request.model_id}",
                        "data": {
                            "model_id": request.model_id,
                            "new_version": request.new_version,
                            "update_successful": False,
                            "rollback_performed": request.rollback_on_failure
                        },
                        "server_side_rendered": True
                    }
                
            except Exception as e:
                logger.error(f"Zero-downtime update error for {request.model_id}: {e}")
                raise HTTPException(
                    status_code=500,
                    detail=f"Update failed: {str(e)}"
                )
        
        @self.router.get("/model_serving/models")
        async def list_models():
            """
            List all available models with their configurations.
            
            Returns comprehensive server-rendered model list with status information.
            """
            try:
                models = await self.model_serving_manager.list_available_models()
                
                return {
                    "status": "success",
                    "data": {
                        "models": models,
                        "total_count": len(models),
                        "healthy_count": sum(1 for m in models if m["status"] == "healthy"),
                        "dtesn_optimized_count": sum(1 for m in models if m["dtesn_optimized"]),
                        "engine_integrated_count": sum(1 for m in models if m["engine_integrated"])
                    },
                    "server_side_rendered": True
                }
                
            except Exception as e:
                logger.error(f"Failed to list models: {e}")
                raise HTTPException(
                    status_code=500,
                    detail=f"Failed to list models: {str(e)}"
                )
        
        @self.router.get("/model_serving/models/{model_id}")
        async def get_model_details(model_id: str):
            """
            Get detailed information about a specific model.
            
            Returns comprehensive model configuration and status.
            """
            try:
                model_allocation = await self.model_serving_manager.get_model_resource_allocation(model_id)
                
                if not model_allocation:
                    raise HTTPException(
                        status_code=404,
                        detail=f"Model {model_id} not found"
                    )
                
                # Get serving status for additional context
                serving_status = self.model_serving_manager.get_model_serving_status()
                model_health = serving_status.get("health_summary", {})
                
                return {
                    "status": "success",
                    "data": {
                        "model_allocation": model_allocation,
                        "health_summary": model_health,
                        "load_balancer_status": serving_status.get("load_balancer_status", {}).get(model_id),
                        "performance_context": serving_status.get("performance_metrics", {})
                    },
                    "server_side_rendered": True
                }
                
            except HTTPException:
                raise
            except Exception as e:
                logger.error(f"Failed to get model details for {model_id}: {e}")
                raise HTTPException(
                    status_code=500,
                    detail=f"Failed to get model details: {str(e)}"
                )
        
        @self.router.delete("/model_serving/models/{model_id}")
        async def remove_model(model_id: str, version: Optional[str] = None):
            """
            Remove a model from the serving infrastructure.
            
            Safely removes model with cleanup of all associated resources.
            """
            try:
                success = await self.model_serving_manager.remove_model(model_id, version)
                
                if success:
                    return {
                        "status": "success",
                        "message": f"Model {model_id} removed successfully",
                        "data": {
                            "model_id": model_id,
                            "version": version,
                            "removed": True
                        },
                        "server_side_rendered": True
                    }
                else:
                    return {
                        "status": "failed",
                        "message": f"Failed to remove model {model_id}",
                        "data": {
                            "model_id": model_id,
                            "version": version,
                            "removed": False
                        },
                        "server_side_rendered": True
                    }
                
            except Exception as e:
                logger.error(f"Failed to remove model {model_id}: {e}")
                raise HTTPException(
                    status_code=500,
                    detail=f"Failed to remove model: {str(e)}"
                )
        
        @self.router.get("/model_serving/health/{model_id}")
        async def get_model_health(model_id: str):
            """
            Get comprehensive health status for a specific model.
            
            Returns detailed health metrics and status information.
            """
            try:
                serving_status = self.model_serving_manager.get_model_serving_status()
                
                # Find model in health status
                health_info = serving_status.get("health_summary", {})
                load_balancer_info = serving_status.get("load_balancer_status", {}).get(model_id, {})
                
                # Get detailed model allocation for health context
                model_allocation = await self.model_serving_manager.get_model_resource_allocation(model_id)
                
                if not model_allocation:
                    raise HTTPException(
                        status_code=404,
                        detail=f"Model {model_id} not found in serving infrastructure"
                    )
                
                health_status = model_allocation.get("health_status", {})
                
                return {
                    "status": "success",
                    "data": {
                        "model_id": model_id,
                        "health_status": health_status,
                        "load_balancer_config": load_balancer_info,
                        "resource_health": {
                            "memory_allocated": model_allocation.get("memory_usage", {}),
                            "dtesn_optimizations": model_allocation.get("dtesn_optimizations", {}),
                            "engine_integration": model_allocation.get("engine_integration", {})
                        },
                        "overall_health_summary": health_info,
                        "health_check_timestamp": time.time()
                    },
                    "server_side_rendered": True
                }
                
            except HTTPException:
                raise
            except Exception as e:
                logger.error(f"Failed to get health for model {model_id}: {e}")
                raise HTTPException(
                    status_code=500,
                    detail=f"Failed to get model health: {str(e)}"
                )
        
        @self.router.get("/model_serving/metrics")
        async def get_performance_metrics():
            """
            Get comprehensive performance metrics for model serving.
            
            Returns server-rendered performance data and analytics.
            """
            try:
                serving_status = self.model_serving_manager.get_model_serving_status()
                performance_metrics = serving_status.get("performance_metrics", {})
                
                # Calculate additional derived metrics
                total_loads = performance_metrics.get("total_loads", 0)
                successful_loads = performance_metrics.get("successful_loads", 0)
                failed_loads = performance_metrics.get("failed_loads", 0)
                
                success_rate = (successful_loads / total_loads * 100) if total_loads > 0 else 0
                failure_rate = (failed_loads / total_loads * 100) if total_loads > 0 else 0
                
                enhanced_metrics = {
                    **performance_metrics,
                    "success_rate_percent": round(success_rate, 2),
                    "failure_rate_percent": round(failure_rate, 2),
                    "total_models_cached": serving_status.get("overview", {}).get("cached_models", 0),
                    "healthy_models": serving_status.get("health_summary", {}).get("healthy_models", 0),
                    "engine_integrated_models": serving_status.get("engine_integration", {}).get("integrated_models", 0)
                }
                
                return {
                    "status": "success",
                    "data": {
                        "performance_metrics": enhanced_metrics,
                        "resource_overview": serving_status.get("resource_allocation", {}),
                        "load_balancer_summary": serving_status.get("load_balancer_status", {}),
                        "metrics_timestamp": time.time()
                    },
                    "server_side_rendered": True
                }
                
            except Exception as e:
                logger.error(f"Failed to get performance metrics: {e}")
                raise HTTPException(
                    status_code=500,
                    detail=f"Failed to get performance metrics: {str(e)}"
                )
        
        @self.router.post("/model_serving/health_check/{model_id}")
        async def perform_health_check(model_id: str):
            """
            Perform on-demand health check for a specific model.
            
            Executes comprehensive health validation and returns detailed results.
            """
            try:
                # Get current model version
                model_allocation = await self.model_serving_manager.get_model_resource_allocation(model_id)
                
                if not model_allocation:
                    raise HTTPException(
                        status_code=404,
                        detail=f"Model {model_id} not found"
                    )
                
                version = model_allocation.get("version", "latest")
                
                # Perform health check
                health_result = await self.model_serving_manager._health_check_model(model_id, version)
                
                # Get updated health status
                serving_status = self.model_serving_manager.get_model_serving_status()
                health_details = serving_status.get("health_summary", {})
                
                return {
                    "status": "success",
                    "data": {
                        "model_id": model_id,
                        "version": version,
                        "health_check_passed": health_result,
                        "health_details": health_details,
                        "check_timestamp": time.time(),
                        "comprehensive_check": True
                    },
                    "server_side_rendered": True
                }
                
            except HTTPException:
                raise
            except Exception as e:
                logger.error(f"Health check failed for model {model_id}: {e}")
                raise HTTPException(
                    status_code=500,
                    detail=f"Health check failed: {str(e)}"
                )


def create_model_serving_routes(model_serving_manager: ModelServingManager) -> APIRouter:
    """
    Create and configure model serving routes.
    
    Args:
        model_serving_manager: Configured model serving manager instance
        
    Returns:
        Configured APIRouter with all model serving endpoints
    """
    route_handler = ModelServingRoutes(model_serving_manager)
    return route_handler.router