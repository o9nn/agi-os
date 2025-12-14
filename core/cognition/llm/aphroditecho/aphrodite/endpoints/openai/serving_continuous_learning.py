"""
OpenAI-Compatible Server-Side Continuous Learning Service.

Integrates continuous learning capabilities with OpenAI-compatible endpoints,
providing real-time model improvement from production data.
"""

import asyncio
import logging
from datetime import datetime
from typing import Any, Dict, List, Optional

from fastapi import Request, HTTPException, BackgroundTasks
from loguru import logger

from aphrodite.common.config import ModelConfig
from aphrodite.continuous_learning import (
    ContinuousLearningSystem, 
    ContinuousLearningConfig,
    InteractionData,
    ServerSideConfig
)
from aphrodite.dtesn_integration import DTESNDynamicIntegration
from aphrodite.dynamic_model_manager import DynamicModelManager
from aphrodite.endpoints.logger import RequestLogger
from aphrodite.endpoints.openai.protocol import ErrorResponse
from aphrodite.endpoints.openai.serving_engine import OpenAIServing
from aphrodite.endpoints.openai.serving_models import OpenAIServingModels
from aphrodite.engine.protocol import EngineClient


class OpenAIServingContinuousLearning(OpenAIServing):
    """
    OpenAI-compatible continuous learning service.
    
    Provides endpoints for managing and monitoring continuous learning
    in production environments with full server-side integration.
    """
    
    def __init__(self,
                 engine_client: EngineClient,
                 model_config: ModelConfig,
                 models: OpenAIServingModels,
                 *,
                 request_logger: Optional[RequestLogger],
                 continuous_learning_system: Optional[ContinuousLearningSystem] = None,
                 server_config: Optional[ServerSideConfig] = None):
        
        super().__init__(
            engine_client=engine_client,
            model_config=model_config, 
            models=models,
            request_logger=request_logger
        )
        
        self.server_config = server_config or ServerSideConfig()
        
        # Initialize continuous learning system if not provided
        if continuous_learning_system is None:
            self.continuous_learning_system = self._initialize_learning_system()
        else:
            self.continuous_learning_system = continuous_learning_system
        
        # Learning state
        self.learning_enabled = True
        self.learning_stats = {
            "total_learning_requests": 0,
            "successful_adaptations": 0,
            "failed_adaptations": 0,
            "last_adaptation": None
        }
        
        logger.info("OpenAI Continuous Learning service initialized")
    
    def _initialize_learning_system(self) -> ContinuousLearningSystem:
        """Initialize continuous learning system with server-optimized configuration."""
        
        # Server-optimized learning configuration
        learning_config = ContinuousLearningConfig(
            max_experiences=50000,  # Large buffer for server environment
            replay_batch_size=16,   # Smaller batches for production
            replay_frequency=20,    # Less frequent replay for stability
            learning_rate_base=self.server_config.max_learning_rate_production,
            learning_rate_decay=self.server_config.learning_rate_decay_production,
            enable_ewc=True,        # Always enable catastrophic forgetting prevention
            ewc_lambda=2000.0,      # Strong forgetting prevention for production
            consolidation_frequency=50,  # More frequent consolidation
            performance_threshold=0.6    # Higher threshold for production
        )
        
        # Initialize required components (mock implementations for demonstration)
        dynamic_manager = self._create_mock_dynamic_manager()
        dtesn_integration = self._create_mock_dtesn_integration()
        
        return ContinuousLearningSystem(
            dynamic_manager=dynamic_manager,
            dtesn_integration=dtesn_integration,
            config=learning_config
        )
    
    def _create_mock_dynamic_manager(self) -> DynamicModelManager:
        """Create mock dynamic model manager for demonstration."""
        
        class MockDynamicModelManager:
            async def apply_incremental_update(self, request):
                return {"success": True, "update_id": f"server_update_{datetime.now().isoformat()}"}
        
        return MockDynamicModelManager()
    
    def _create_mock_dtesn_integration(self) -> DTESNDynamicIntegration:
        """Create mock DTESN integration for demonstration."""
        
        class MockDTESNIntegration:
            async def adaptive_parameter_update(self, parameter_name, current_params, 
                                              target_gradient, performance_feedback):
                import torch
                # Mock adaptive update
                updated_params = current_params + target_gradient * 0.001
                metrics = {
                    "learning_type": "server_side_adaptive",
                    "learning_rate": 0.001,
                    "adaptation_strength": abs(performance_feedback) * 0.1,
                    "parameter_norm": torch.norm(updated_params).item() if hasattr(updated_params, 'norm') else 1.0
                }
                return updated_params, metrics
        
        return MockDTESNIntegration()
    
    async def learn_from_production_interaction(self,
                                              interaction_data: InteractionData,
                                              background_tasks: BackgroundTasks) -> Dict[str, Any]:
        """
        Learn from a production interaction with server-side processing.
        
        Args:
            interaction_data: The interaction to learn from
            background_tasks: FastAPI background tasks for async processing
            
        Returns:
            Learning result with metrics and status
        """
        
        if not self.learning_enabled:
            return {
                "success": False,
                "error": "Learning is currently disabled",
                "learning_enabled": False
            }
        
        self.learning_stats["total_learning_requests"] += 1
        
        try:
            # Process learning in background for production safety
            def background_learning_task():
                asyncio.create_task(self._background_learn_from_interaction(interaction_data))
            
            background_tasks.add_task(background_learning_task)
            
            return {
                "success": True,
                "message": "Learning queued for background processing",
                "interaction_id": interaction_data.interaction_id,
                "queued_at": datetime.now().isoformat(),
                "learning_enabled": self.learning_enabled
            }
            
        except Exception as e:
            logger.error(f"Failed to queue learning task: {e}")
            self.learning_stats["failed_adaptations"] += 1
            
            return {
                "success": False,
                "error": str(e),
                "interaction_id": interaction_data.interaction_id,
                "learning_enabled": self.learning_enabled
            }
    
    async def _background_learn_from_interaction(self, interaction_data: InteractionData):
        """Background task for learning from interaction."""
        
        try:
            result = await self.continuous_learning_system.learn_from_interaction(interaction_data)
            
            if result.get("success", False):
                self.learning_stats["successful_adaptations"] += 1
                self.learning_stats["last_adaptation"] = datetime.now().isoformat()
                logger.info(f"Successfully learned from interaction {interaction_data.interaction_id}")
            else:
                self.learning_stats["failed_adaptations"] += 1
                logger.warning(f"Failed to learn from interaction {interaction_data.interaction_id}: {result.get('error')}")
                
        except Exception as e:
            self.learning_stats["failed_adaptations"] += 1
            logger.error(f"Background learning failed for interaction {interaction_data.interaction_id}: {e}")
    
    async def get_learning_status(self) -> Dict[str, Any]:
        """Get comprehensive learning status."""
        
        # Get system statistics
        system_stats = self.continuous_learning_system.get_learning_stats()
        
        # Combine with service statistics
        status = {
            "service_info": {
                "learning_enabled": self.learning_enabled,
                "server_config": {
                    "background_learning_interval": self.server_config.background_learning_interval,
                    "max_learning_rate": self.server_config.max_learning_rate_production,
                    "enable_hot_swapping": self.server_config.enable_hot_swapping,
                    "enable_rollback": self.server_config.enable_rollback_on_failure
                }
            },
            "learning_statistics": {
                "service_stats": self.learning_stats,
                "system_stats": system_stats
            },
            "model_info": {
                "model_name": self.model_config.model,
                "model_type": getattr(self.model_config.hf_config, "model_type", "unknown"),
                "max_model_len": self.model_config.max_model_len
            },
            "timestamp": datetime.now().isoformat()
        }
        
        return status
    
    async def enable_learning(self) -> Dict[str, Any]:
        """Enable continuous learning."""
        
        self.learning_enabled = True
        
        logger.info("Continuous learning enabled")
        
        return {
            "success": True,
            "message": "Continuous learning enabled",
            "learning_enabled": True,
            "timestamp": datetime.now().isoformat()
        }
    
    async def disable_learning(self) -> Dict[str, Any]:
        """Disable continuous learning."""
        
        self.learning_enabled = False
        
        logger.info("Continuous learning disabled")
        
        return {
            "success": True,
            "message": "Continuous learning disabled", 
            "learning_enabled": False,
            "timestamp": datetime.now().isoformat()
        }
    
    async def reset_learning_state(self) -> Dict[str, Any]:
        """Reset learning state while preserving consolidated memory."""
        
        try:
            await self.continuous_learning_system.reset_learning_state()
            
            # Reset service statistics
            self.learning_stats = {
                "total_learning_requests": 0,
                "successful_adaptations": 0,
                "failed_adaptations": 0,
                "last_adaptation": None
            }
            
            logger.info("Learning state reset successfully")
            
            return {
                "success": True,
                "message": "Learning state reset (consolidated memory preserved)",
                "timestamp": datetime.now().isoformat()
            }
            
        except Exception as e:
            logger.error(f"Failed to reset learning state: {e}")
            
            return {
                "success": False,
                "error": str(e),
                "timestamp": datetime.now().isoformat()
            }
    
    async def get_learning_metrics(self) -> Dict[str, Any]:
        """Get detailed learning performance metrics."""
        
        system_stats = self.continuous_learning_system.get_learning_stats()
        
        metrics = {
            "overview": {
                "learning_enabled": self.learning_enabled,
                "total_interactions": system_stats.get("interaction_count", 0),
                "success_rate": (
                    system_stats.get("metrics", {}).get("successful_adaptations", 0) /
                    max(1, system_stats.get("interaction_count", 1))
                ),
                "current_learning_rate": system_stats.get("current_learning_rate", 0.0)
            },
            "performance": {
                "experience_count": system_stats.get("experience_count", 0),
                "consolidated_parameters": system_stats.get("consolidated_parameters", 0),
                "parameter_importance_tracked": system_stats.get("parameter_importance_count", 0)
            },
            "system_metrics": system_stats.get("metrics", {}),
            "performance_stats": system_stats.get("performance_stats", {}),
            "service_stats": self.learning_stats,
            "timestamp": datetime.now().isoformat()
        }
        
        return metrics
    
    async def trigger_manual_learning(self,
                                    prompt: str,
                                    response: str, 
                                    performance_feedback: float,
                                    interaction_type: str = "manual",
                                    metadata: Optional[Dict[str, Any]] = None) -> Dict[str, Any]:
        """
        Manually trigger learning from provided prompt/response pair.
        
        Useful for fine-tuning based on specific examples or user feedback.
        """
        
        if not self.learning_enabled:
            return {
                "success": False,
                "error": "Learning is currently disabled"
            }
        
        # Create interaction data
        interaction = InteractionData(
            interaction_id=f"manual_{datetime.now().isoformat()}",
            interaction_type=interaction_type,
            input_data={"prompt": prompt},
            output_data={"response": response},
            performance_feedback=performance_feedback,
            timestamp=datetime.now(),
            context_metadata={
                "source": "manual_trigger",
                **(metadata or {})
            },
            success=True
        )
        
        try:
            # Process learning immediately for manual triggers
            result = await self.continuous_learning_system.learn_from_interaction(interaction)
            
            if result.get("success", False):
                self.learning_stats["successful_adaptations"] += 1
                self.learning_stats["last_adaptation"] = datetime.now().isoformat()
                
                return {
                    "success": True,
                    "message": "Manual learning completed successfully",
                    "interaction_id": interaction.interaction_id,
                    "learning_result": {
                        "learning_time": result.get("learning_time", 0),
                        "current_learning_rate": result.get("current_learning_rate", 0),
                        "replay_triggered": result.get("replay_result") is not None,
                        "consolidation_triggered": result.get("consolidation_result") is not None
                    },
                    "timestamp": datetime.now().isoformat()
                }
            else:
                self.learning_stats["failed_adaptations"] += 1
                
                return {
                    "success": False,
                    "error": result.get("error", "Unknown learning failure"),
                    "interaction_id": interaction.interaction_id,
                    "timestamp": datetime.now().isoformat()
                }
                
        except Exception as e:
            self.learning_stats["failed_adaptations"] += 1
            logger.error(f"Manual learning failed: {e}")
            
            return {
                "success": False,
                "error": str(e),
                "interaction_id": interaction.interaction_id,
                "timestamp": datetime.now().isoformat()
            }


# Pydantic models for API endpoints
from pydantic import BaseModel, Field


class LearningInteractionRequest(BaseModel):
    """Request model for learning from interaction."""
    
    interaction_id: Optional[str] = Field(None, description="Unique interaction identifier")
    prompt: str = Field(..., description="Input prompt")
    response: str = Field(..., description="Generated response")
    performance_feedback: float = Field(..., ge=-1.0, le=1.0, description="Performance feedback (-1 to 1)")
    interaction_type: str = Field("chat_completion", description="Type of interaction")
    metadata: Optional[Dict[str, Any]] = Field(None, description="Additional metadata")


class LearningControlRequest(BaseModel):
    """Request model for learning control operations."""
    
    action: str = Field(..., description="Control action (enable/disable/reset)")
    parameters: Optional[Dict[str, Any]] = Field(None, description="Optional parameters")


class LearningResponse(BaseModel):
    """Response model for learning operations."""
    
    success: bool = Field(..., description="Operation success status")
    message: Optional[str] = Field(None, description="Response message")
    data: Optional[Dict[str, Any]] = Field(None, description="Response data")
    timestamp: str = Field(..., description="Operation timestamp")
    error: Optional[str] = Field(None, description="Error message if failed")