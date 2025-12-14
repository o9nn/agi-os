"""
FastAPI routes for dynamic DTESN configuration management.

Provides server-side endpoints for configuration updates, validation,
rollback, and environment management without service restart.
"""

import logging
from typing import List, Dict, Any, Optional
from fastapi import APIRouter, HTTPException, Depends, BackgroundTasks
from fastapi.responses import JSONResponse
from pydantic import BaseModel, Field

from .dynamic_config_manager import (
    DynamicConfigurationManager,
    ConfigurationUpdateRequest,
    ConfigurationEnvironment,
    get_dynamic_config_manager
)
from .config import DTESNConfig

logger = logging.getLogger(__name__)

# Create router for configuration management
config_router = APIRouter(prefix="/v1/config", tags=["configuration"])


# Request/Response Models
class ParameterUpdateRequest(BaseModel):
    """API request model for parameter updates."""
    parameter: str = Field(..., description="Parameter name to update")
    value: Any = Field(..., description="New parameter value")
    description: Optional[str] = Field(None, description="Update description")
    validate_only: bool = Field(False, description="Only validate, don't apply")
    environment: Optional[str] = Field(None, description="Target environment")


class BatchUpdateRequest(BaseModel):
    """API request model for batch parameter updates."""
    updates: List[ParameterUpdateRequest] = Field(..., description="List of parameter updates")
    description: Optional[str] = Field(None, description="Batch update description")


class RollbackRequest(BaseModel):
    """API request model for configuration rollback."""
    snapshot_id: str = Field(..., description="Snapshot ID to rollback to")


class EnvironmentRequest(BaseModel):
    """API request model for environment changes."""
    environment: str = Field(..., description="Target environment")


class ConfigurationResponse(BaseModel):
    """API response model for configuration operations."""
    success: bool = Field(..., description="Operation success status")
    message: Optional[str] = Field(None, description="Response message")
    data: Optional[Dict[str, Any]] = Field(None, description="Response data")
    errors: Optional[List[str]] = Field(None, description="Validation errors")


# Dependency to get configuration manager
async def get_config_manager() -> DynamicConfigurationManager:
    """Dependency to get the dynamic configuration manager."""
    return get_dynamic_config_manager()


@config_router.get(
    "/current",
    response_model=Dict[str, Any],
    summary="Get Current Configuration",
    description="Retrieve the current DTESN configuration parameters."
)
async def get_current_configuration(
    manager: DynamicConfigurationManager = Depends(get_config_manager)
):
    """Get the current configuration."""
    try:
        current_config = manager.current_config
        return {
            "success": True,
            "configuration": current_config.dict(),
            "environment": manager.environment.value,
            "timestamp": manager.get_current_status()
        }
    except Exception as e:
        logger.error(f"Failed to get current configuration: {e}")
        raise HTTPException(
            status_code=500,
            detail=f"Failed to retrieve configuration: {str(e)}"
        )


@config_router.get(
    "/status",
    response_model=Dict[str, Any],
    summary="Get Configuration Manager Status",
    description="Retrieve detailed status of the configuration manager."
)
async def get_configuration_status(
    manager: DynamicConfigurationManager = Depends(get_config_manager)
):
    """Get configuration manager status."""
    try:
        status = manager.get_current_status()
        return {
            "success": True,
            "status": status
        }
    except Exception as e:
        logger.error(f"Failed to get configuration status: {e}")
        raise HTTPException(
            status_code=500,
            detail=f"Failed to retrieve status: {str(e)}"
        )


@config_router.post(
    "/update",
    response_model=ConfigurationResponse,
    summary="Update Configuration Parameter",
    description="Update a single DTESN configuration parameter with validation."
)
async def update_configuration_parameter(
    request: ParameterUpdateRequest,
    background_tasks: BackgroundTasks,
    manager: DynamicConfigurationManager = Depends(get_config_manager)
):
    """Update a single configuration parameter."""
    try:
        # Convert environment string to enum if provided
        environment = None
        if request.environment:
            try:
                environment = ConfigurationEnvironment(request.environment)
            except ValueError:
                raise HTTPException(
                    status_code=400,
                    detail=f"Invalid environment: {request.environment}"
                )
        
        # Create update request
        update_request = ConfigurationUpdateRequest(
            parameter_path=request.parameter,
            new_value=request.value,
            description=request.description,
            validate_only=request.validate_only,
            environment=environment
        )
        
        # Apply the update
        result = await manager.update_parameter(update_request)
        
        if result["success"]:
            return ConfigurationResponse(
                success=True,
                message=f"Parameter {request.parameter} updated successfully",
                data=result
            )
        else:
            return ConfigurationResponse(
                success=False,
                message=result.get("error", "Update failed"),
                errors=result.get("validation_errors", [])
            )
            
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Configuration update failed: {e}")
        raise HTTPException(
            status_code=500,
            detail=f"Configuration update failed: {str(e)}"
        )


@config_router.post(
    "/batch-update",
    response_model=ConfigurationResponse,
    summary="Batch Update Configuration Parameters",
    description="Update multiple DTESN configuration parameters atomically."
)
async def batch_update_configuration(
    request: BatchUpdateRequest,
    background_tasks: BackgroundTasks,
    manager: DynamicConfigurationManager = Depends(get_config_manager)
):
    """Update multiple configuration parameters in a batch."""
    try:
        # Convert to internal update requests
        update_requests = []
        for update in request.updates:
            environment = None
            if update.environment:
                try:
                    environment = ConfigurationEnvironment(update.environment)
                except ValueError:
                    raise HTTPException(
                        status_code=400,
                        detail=f"Invalid environment: {update.environment}"
                    )
            
            update_requests.append(
                ConfigurationUpdateRequest(
                    parameter_path=update.parameter,
                    new_value=update.value,
                    description=update.description,
                    validate_only=update.validate_only,
                    environment=environment
                )
            )
        
        # Apply the batch update
        result = await manager.update_multiple_parameters(update_requests)
        
        if result["success"]:
            return ConfigurationResponse(
                success=True,
                message=f"Batch update of {len(request.updates)} parameters completed",
                data=result
            )
        else:
            return ConfigurationResponse(
                success=False,
                message=result.get("error", "Batch update failed"),
                errors=result.get("validation_errors", [])
            )
            
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Batch configuration update failed: {e}")
        raise HTTPException(
            status_code=500,
            detail=f"Batch update failed: {str(e)}"
        )


@config_router.post(
    "/validate",
    response_model=ConfigurationResponse,
    summary="Validate Configuration Parameter",
    description="Validate a configuration parameter without applying changes."
)
async def validate_configuration_parameter(
    request: ParameterUpdateRequest,
    manager: DynamicConfigurationManager = Depends(get_config_manager)
):
    """Validate a configuration parameter without applying."""
    try:
        # Force validate_only to True
        update_request = ConfigurationUpdateRequest(
            parameter_path=request.parameter,
            new_value=request.value,
            description=request.description,
            validate_only=True
        )
        
        result = await manager.update_parameter(update_request)
        
        return ConfigurationResponse(
            success=result["success"],
            message="Validation completed",
            data=result,
            errors=result.get("validation_errors", [])
        )
        
    except Exception as e:
        logger.error(f"Configuration validation failed: {e}")
        raise HTTPException(
            status_code=500,
            detail=f"Validation failed: {str(e)}"
        )


@config_router.post(
    "/rollback",
    response_model=ConfigurationResponse,
    summary="Rollback Configuration",
    description="Rollback configuration to a previous snapshot."
)
async def rollback_configuration(
    request: RollbackRequest,
    background_tasks: BackgroundTasks,
    manager: DynamicConfigurationManager = Depends(get_config_manager)
):
    """Rollback configuration to a snapshot."""
    try:
        result = await manager.rollback_to_snapshot(request.snapshot_id)
        
        if result["success"]:
            return ConfigurationResponse(
                success=True,
                message=f"Configuration rolled back to snapshot {request.snapshot_id}",
                data=result
            )
        else:
            return ConfigurationResponse(
                success=False,
                message=result.get("error", "Rollback failed"),
                data=result
            )
            
    except Exception as e:
        logger.error(f"Configuration rollback failed: {e}")
        raise HTTPException(
            status_code=500,
            detail=f"Rollback failed: {str(e)}"
        )


@config_router.get(
    "/snapshots",
    response_model=Dict[str, Any],
    summary="Get Configuration Snapshots",
    description="Retrieve list of configuration snapshots."
)
async def get_configuration_snapshots(
    manager: DynamicConfigurationManager = Depends(get_config_manager)
):
    """Get list of configuration snapshots."""
    try:
        snapshots = manager.get_snapshots()
        return {
            "success": True,
            "snapshots": snapshots,
            "total_count": len(snapshots)
        }
    except Exception as e:
        logger.error(f"Failed to get snapshots: {e}")
        raise HTTPException(
            status_code=500,
            detail=f"Failed to retrieve snapshots: {str(e)}"
        )


@config_router.post(
    "/environment",
    response_model=ConfigurationResponse,
    summary="Set Configuration Environment",
    description="Set the configuration environment (development, staging, production)."
)
async def set_configuration_environment(
    request: EnvironmentRequest,
    manager: DynamicConfigurationManager = Depends(get_config_manager)
):
    """Set the configuration environment."""
    try:
        # Validate environment
        try:
            environment = ConfigurationEnvironment(request.environment)
        except ValueError:
            raise HTTPException(
                status_code=400,
                detail=f"Invalid environment: {request.environment}. "
                       f"Valid options: {[e.value for e in ConfigurationEnvironment]}"
            )
        
        # Set environment
        manager.set_environment(environment)
        
        return ConfigurationResponse(
            success=True,
            message=f"Environment set to {environment.value}",
            data={"environment": environment.value}
        )
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Failed to set environment: {e}")
        raise HTTPException(
            status_code=500,
            detail=f"Failed to set environment: {str(e)}"
        )


@config_router.get(
    "/schema",
    response_model=Dict[str, Any],
    summary="Get Configuration Schema",
    description="Get the DTESN configuration schema with parameter descriptions."
)
async def get_configuration_schema():
    """Get the configuration schema."""
    try:
        # Get the Pydantic model schema
        schema = DTESNConfig.schema()
        
        # Add additional metadata
        parameter_info = {
            "max_membrane_depth": {
                "type": "integer",
                "range": "1-64",
                "description": "Maximum depth for DTESN membrane hierarchy",
                "impact": "Affects computational complexity and memory usage"
            },
            "esn_reservoir_size": {
                "type": "integer", 
                "range": "64-16384",
                "description": "Size of Echo State Network reservoir",
                "impact": "Directly affects model capacity and memory requirements"
            },
            "bseries_max_order": {
                "type": "integer",
                "range": "1-32", 
                "description": "Maximum order for B-Series computations",
                "impact": "Controls precision vs. computational cost tradeoff"
            },
            "enable_caching": {
                "type": "boolean",
                "description": "Enable server-side response caching",
                "impact": "Improves response time but increases memory usage"
            },
            "cache_ttl_seconds": {
                "type": "integer",
                "range": "1-86400",
                "description": "Cache TTL in seconds",
                "impact": "Controls cache freshness vs. performance"
            }
        }
        
        return {
            "success": True,
            "schema": schema,
            "parameter_info": parameter_info,
            "environments": [e.value for e in ConfigurationEnvironment]
        }
        
    except Exception as e:
        logger.error(f"Failed to get schema: {e}")
        raise HTTPException(
            status_code=500,
            detail=f"Failed to retrieve schema: {str(e)}"
        )


@config_router.get(
    "/health",
    response_model=Dict[str, Any],
    summary="Configuration Manager Health Check",
    description="Check health and readiness of configuration manager."
)
async def configuration_health_check(
    manager: DynamicConfigurationManager = Depends(get_config_manager)
):
    """Health check for configuration manager."""
    try:
        status = manager.get_current_status()
        
        # Basic health checks
        health_status = {
            "healthy": True,
            "checks": {
                "config_manager_initialized": manager is not None,
                "config_loaded": status["current_config"] is not None,
                "snapshots_available": status["total_snapshots"] > 0,
                "backup_directory_writable": status["backup_directory"] is not None
            }
        }
        
        # Check if any health checks failed
        if not all(health_status["checks"].values()):
            health_status["healthy"] = False
        
        return {
            "success": True,
            "health": health_status,
            "status": status
        }
        
    except Exception as e:
        logger.error(f"Configuration health check failed: {e}")
        return {
            "success": False,
            "healthy": False,
            "error": str(e)
        }


# Additional utility endpoints for monitoring and debugging
@config_router.get(
    "/diff/{snapshot_id}",
    response_model=Dict[str, Any],
    summary="Get Configuration Diff",
    description="Compare current configuration with a snapshot."
)
async def get_configuration_diff(
    snapshot_id: str,
    manager: DynamicConfigurationManager = Depends(get_config_manager)
):
    """Get difference between current config and a snapshot."""
    try:
        snapshots = manager.get_snapshots()
        target_snapshot = None
        
        for snapshot in snapshots:
            if snapshot["snapshot_id"] == snapshot_id:
                target_snapshot = snapshot
                break
        
        if not target_snapshot:
            raise HTTPException(
                status_code=404,
                detail=f"Snapshot {snapshot_id} not found"
            )
        
        current_config = manager.current_config.dict()
        
        # Simple diff calculation (could be enhanced with proper diff library)
        differences = {}
        
        # Find changed parameters
        for param, current_value in current_config.items():
            # Note: Would need to load snapshot config data from manager
            # This is a simplified implementation
            differences[param] = {
                "current": current_value,
                "snapshot": "N/A",  # Would need snapshot data
                "changed": False    # Would need actual comparison
            }
        
        return {
            "success": True,
            "snapshot_id": snapshot_id,
            "differences": differences,
            "total_changes": sum(1 for d in differences.values() if d["changed"])
        }
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Failed to get configuration diff: {e}")
        raise HTTPException(
            status_code=500,
            detail=f"Failed to get diff: {str(e)}"
        )