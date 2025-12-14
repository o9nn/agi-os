"""
OpenAI-compatible serving endpoint for dynamic model updates.

Provides REST API for online model parameter updates, versioning, 
and rollback capabilities with zero service interruption.
"""

import logging
from http import HTTPStatus
from typing import Any, Optional

import torch
from fastapi import HTTPException

from aphrodite.dynamic_model_manager import (
    DynamicModelManager,
    IncrementalUpdateRequest as DynamicIncrementalUpdateRequest,
    DynamicUpdateConfig
)
from aphrodite.endpoints.openai.protocol import (
    ErrorResponse,
    IncrementalUpdateRequest,
    ModelVersionRequest,
    ModelRollbackRequest,
    DynamicUpdateResponse,
    ModelVersionInfo,
    ModelVersionListResponse,
    ModelStatusResponse
)
from aphrodite.engine.protocol import EngineClient
from aphrodite.common.config import ModelConfig, LoRAConfig

logger = logging.getLogger(__name__)


class OpenAIServingDynamicUpdates:
    """
    OpenAI-compatible serving endpoint for dynamic model updates.
    
    Handles the routes:
    - /v1/models/incremental_update
    - /v1/models/create_version  
    - /v1/models/rollback
    - /v1/models/versions
    - /v1/models/status
    """
    
    def __init__(
        self,
        engine_client: EngineClient,
        model_config: ModelConfig,
        lora_config: Optional[LoRAConfig] = None,
        dynamic_config: Optional[DynamicUpdateConfig] = None
    ):
        self.engine_client = engine_client
        self.model_config = model_config
        self.lora_config = lora_config
        
        # Initialize dynamic model manager
        self.dynamic_manager = DynamicModelManager(
            engine_client=engine_client,
            model_config=model_config,
            lora_config=lora_config,
            config=dynamic_config
        )
        
        # Track if initialized
        self._initialized = False
    
    async def initialize(self) -> None:
        """Initialize the dynamic updates service."""
        if self._initialized:
            return
        
        try:
            # Create initial version checkpoint
            await self.dynamic_manager.create_initial_version("Initial model state")
            self._initialized = True
            logger.info("Dynamic updates service initialized successfully")
        except Exception as e:
            logger.error(f"Failed to initialize dynamic updates service: {e}")
            raise
    
    async def apply_incremental_update(
        self, 
        request: IncrementalUpdateRequest
    ) -> DynamicUpdateResponse:
        """Apply incremental parameter update to the model."""
        if not self._initialized:
            await self.initialize()
        
        try:
            # Convert request to internal format
            update_data = self._process_update_data(request.update_data)
            
            internal_request = DynamicIncrementalUpdateRequest(
                parameter_name=request.parameter_name,
                update_data=update_data,
                learning_rate=request.learning_rate,
                update_type=request.update_type,
                metadata=request.metadata
            )
            
            # Apply the update
            result = await self.dynamic_manager.apply_incremental_update(internal_request)
            
            if result["success"]:
                return DynamicUpdateResponse(
                    success=True,
                    message=f"Successfully applied incremental update to {request.parameter_name}",
                    data={
                        "update_id": result["update_id"],
                        "update_count": result.get("update_count"),
                        "pre_metrics": result.get("pre_metrics"),
                        "post_metrics": result.get("post_metrics")
                    }
                )
            else:
                return DynamicUpdateResponse(
                    success=False,
                    message=f"Failed to apply update: {result.get('reason', 'Unknown error')}",
                    data=result
                )
        
        except Exception as e:
            logger.error(f"Error applying incremental update: {e}")
            return DynamicUpdateResponse(
                success=False,
                message=f"Internal error: {str(e)}"
            )
    
    async def create_version(self, request: ModelVersionRequest) -> DynamicUpdateResponse:
        """Create a new model version checkpoint."""
        if not self._initialized:
            await self.initialize()
        
        try:
            version_id = await self.dynamic_manager.create_version(request.description)
            
            return DynamicUpdateResponse(
                success=True,
                message=f"Successfully created model version: {version_id}",
                data={"version_id": version_id}
            )
        
        except Exception as e:
            logger.error(f"Error creating model version: {e}")
            return DynamicUpdateResponse(
                success=False,
                message=f"Failed to create version: {str(e)}"
            )
    
    async def rollback_to_version(self, request: ModelRollbackRequest) -> DynamicUpdateResponse:
        """Rollback to a specific model version."""
        if not self._initialized:
            await self.initialize()
        
        try:
            result = await self.dynamic_manager.rollback_to_version(request.version_id)
            
            if result["success"]:
                return DynamicUpdateResponse(
                    success=True,
                    message=f"Successfully rolled back to version: {request.version_id}",
                    data=result
                )
            else:
                return DynamicUpdateResponse(
                    success=False,
                    message=f"Rollback failed: {result.get('reason', 'Unknown error')}",
                    data=result
                )
        
        except Exception as e:
            logger.error(f"Error rolling back to version {request.version_id}: {e}")
            return DynamicUpdateResponse(
                success=False,
                message=f"Rollback error: {str(e)}"
            )
    
    async def list_versions(self) -> ModelVersionListResponse:
        """List all available model versions."""
        if not self._initialized:
            await self.initialize()
        
        try:
            versions_data = self.dynamic_manager.list_versions()
            
            versions = [
                ModelVersionInfo(
                    version_id=v["version_id"],
                    timestamp=v["timestamp"],
                    description=v["description"],
                    is_active=v["is_active"],
                    performance_metrics=v["performance_metrics"]
                )
                for v in versions_data
            ]
            
            return ModelVersionListResponse(
                versions=versions,
                total_count=len(versions)
            )
        
        except Exception as e:
            logger.error(f"Error listing versions: {e}")
            raise HTTPException(
                status_code=HTTPStatus.INTERNAL_SERVER_ERROR,
                detail=f"Failed to list versions: {str(e)}"
            )
    
    async def get_status(self) -> ModelStatusResponse:
        """Get current status of dynamic model updates."""
        if not self._initialized:
            await self.initialize()
        
        try:
            status_data = self.dynamic_manager.get_status()
            
            return ModelStatusResponse(
                current_version=status_data["current_version"],
                total_versions=status_data["total_versions"],
                total_updates=status_data["total_updates"],
                config=status_data["config"],
                recent_performance=status_data["recent_performance"]
            )
        
        except Exception as e:
            logger.error(f"Error getting status: {e}")
            raise HTTPException(
                status_code=HTTPStatus.INTERNAL_SERVER_ERROR,
                detail=f"Failed to get status: {str(e)}"
            )
    
    def create_error_response(
        self,
        message: str,
        error_type: str = "DynamicUpdateError",
        status_code: int = HTTPStatus.BAD_REQUEST
    ) -> ErrorResponse:
        """Create standardized error response."""
        return ErrorResponse(
            message=message,
            type=error_type,
            code=status_code
        )
    
    # Private helper methods
    
    def _process_update_data(self, update_data: Any) -> torch.Tensor:
        """
        Process and validate update data, converting to tensor if needed.
        
        Args:
            update_data: Raw update data from request
            
        Returns:
            torch.Tensor: Processed update data
            
        Raises:
            ValueError: If update data is invalid
        """
        try:
            if isinstance(update_data, list):
                # Convert list to tensor
                return torch.tensor(update_data, dtype=torch.float32)
            elif isinstance(update_data, torch.Tensor):
                return update_data
            elif isinstance(update_data, (int, float)):
                # Single value
                return torch.tensor([update_data], dtype=torch.float32)
            else:
                # Try to convert generic data
                return torch.tensor(update_data, dtype=torch.float32)
        
        except Exception as e:
            raise ValueError(f"Invalid update data format: {e}")
    
    async def _validate_parameter_name(self, parameter_name: str) -> bool:
        """
        Validate that the parameter name exists in the model.
        
        Args:
            parameter_name: Name of the parameter to update
            
        Returns:
            bool: True if parameter exists, False otherwise
        """
        # This would integrate with the actual model to check parameter existence
        # For now, perform basic validation
        if not parameter_name or not isinstance(parameter_name, str):
            return False
        
        # Basic format validation
        if len(parameter_name) > 200 or not parameter_name.replace("_", "").replace(".", "").isalnum():
            return False
        
        return True