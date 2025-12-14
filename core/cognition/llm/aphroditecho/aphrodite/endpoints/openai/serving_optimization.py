"""
Server-Side Model Optimization Serving Endpoints.

FastAPI endpoints for server-side model optimization including compilation,
dynamic tuning, and ensemble management. Integrates with OpenAI-compatible
API infrastructure for seamless optimization control.
"""

import asyncio
import logging
import time
from typing import Dict, List, Optional, Any, Union
from fastapi import HTTPException, Request
from pydantic import BaseModel, Field
import torch

from aphrodite.common.config import ModelConfig
from aphrodite.engine.server_side_optimizer import (
    ServerSideOptimizer, 
    OptimizationConfig,
    ServerLoadMetrics
)
from aphrodite.engine.async_aphrodite import AsyncAphrodite
from aphrodite.endpoints.openai.protocol import ErrorResponse
from aphrodite.endpoints.logger import RequestLogger

logger = logging.getLogger(__name__)


class OptimizationRequest(BaseModel):
    """Request for optimization configuration changes."""
    enable_torch_compile: Optional[bool] = None
    compile_mode: Optional[str] = Field(None, pattern="^(default|reduce-overhead|max-autotune)$")
    enable_dynamic_tuning: Optional[bool] = None
    tuning_interval_sec: Optional[float] = Field(None, ge=1.0, le=300.0)
    load_threshold_high: Optional[float] = Field(None, ge=0.1, le=1.0)
    load_threshold_low: Optional[float] = Field(None, ge=0.0, le=0.9)
    enable_ensemble: Optional[bool] = None
    max_ensemble_size: Optional[int] = Field(None, ge=1, le=10)
    ensemble_strategy: Optional[str] = Field(
        None, 
        pattern="^(weighted_voting|best_of_n|adaptive)$"
    )


class OptimizationResponse(BaseModel):
    """Response containing optimization status."""
    success: bool
    message: str
    optimization_status: Dict[str, Any]
    timestamp: float


class LoadMetricsResponse(BaseModel):
    """Response containing current server load metrics."""
    timestamp: float
    cpu_usage_percent: float
    memory_usage_percent: float
    gpu_utilization: float
    active_requests: int
    queue_depth: int
    throughput_tokens_per_sec: float
    avg_latency_ms: float
    error_rate: float
    overall_load_score: float


class EnsembleStatusResponse(BaseModel):
    """Response containing ensemble status."""
    enabled: bool
    strategy: str
    model_count: int
    max_size: int
    models: List[Dict[str, Any]]


class PerformanceReportResponse(BaseModel):
    """Response containing performance report."""
    report_timestamp: float
    report_filepath: Optional[str]
    summary: Dict[str, Any]
    performance_grade: str


class OptimizationServingMixin:
    """
    Mixin providing server-side optimization endpoints for OpenAI serving.
    
    This integrates server-side model optimization capabilities with the
    existing OpenAI-compatible API server infrastructure.
    """
    
    def __init__(self):
        self.optimizer: Optional[ServerSideOptimizer] = None
        self.optimization_enabled = False
        
    def _setup_optimization_routes(self, app, engine: AsyncAphrodite):
        """Set up optimization routes in FastAPI app."""
        
        @app.post("/v1/optimization/configure")
        async def configure_optimization(
            request: OptimizationRequest,
            raw_request: Request
        ) -> OptimizationResponse:
            """Configure server-side optimization settings."""
            try:
                if not self.optimizer:
                    await self._initialize_optimizer(engine)
                
                # Update configuration
                config_updates = {}
                if request.enable_torch_compile is not None:
                    config_updates["enable_torch_compile"] = request.enable_torch_compile
                if request.compile_mode is not None:
                    config_updates["compile_mode"] = request.compile_mode
                if request.enable_dynamic_tuning is not None:
                    config_updates["enable_dynamic_tuning"] = request.enable_dynamic_tuning
                if request.tuning_interval_sec is not None:
                    config_updates["tuning_interval_sec"] = request.tuning_interval_sec
                if request.load_threshold_high is not None:
                    config_updates["load_threshold_high"] = request.load_threshold_high
                if request.load_threshold_low is not None:
                    config_updates["load_threshold_low"] = request.load_threshold_low
                if request.enable_ensemble is not None:
                    config_updates["enable_ensemble"] = request.enable_ensemble
                if request.max_ensemble_size is not None:
                    config_updates["max_ensemble_size"] = request.max_ensemble_size
                if request.ensemble_strategy is not None:
                    config_updates["ensemble_strategy"] = request.ensemble_strategy
                
                # Apply configuration updates
                for key, value in config_updates.items():
                    setattr(self.optimizer.config, key, value)
                
                # Restart optimization with new config if it was running
                if self.optimization_enabled:
                    await self.optimizer.stop_optimization()
                    await self.optimizer.start_optimization()
                
                status = self.optimizer.get_optimization_status()
                
                return OptimizationResponse(
                    success=True,
                    message=f"Updated {len(config_updates)} optimization settings",
                    optimization_status=status,
                    timestamp=time.time()
                )
                
            except Exception as e:
                logger.error(f"Failed to configure optimization: {e}")
                raise HTTPException(status_code=500, detail=str(e))
        
        @app.post("/v1/optimization/start")
        async def start_optimization(raw_request: Request) -> OptimizationResponse:
            """Start server-side optimization system."""
            try:
                if not self.optimizer:
                    await self._initialize_optimizer(engine)
                
                if not self.optimization_enabled:
                    await self.optimizer.start_optimization()
                    self.optimization_enabled = True
                    message = "Server-side optimization started"
                else:
                    message = "Server-side optimization already running"
                
                status = self.optimizer.get_optimization_status()
                
                return OptimizationResponse(
                    success=True,
                    message=message,
                    optimization_status=status,
                    timestamp=time.time()
                )
                
            except Exception as e:
                logger.error(f"Failed to start optimization: {e}")
                raise HTTPException(status_code=500, detail=str(e))
        
        @app.post("/v1/optimization/stop")
        async def stop_optimization(raw_request: Request) -> OptimizationResponse:
            """Stop server-side optimization system."""
            try:
                if self.optimizer and self.optimization_enabled:
                    await self.optimizer.stop_optimization()
                    self.optimization_enabled = False
                    message = "Server-side optimization stopped"
                else:
                    message = "Server-side optimization not running"
                
                status = self.optimizer.get_optimization_status() if self.optimizer else {}
                
                return OptimizationResponse(
                    success=True,
                    message=message,
                    optimization_status=status,
                    timestamp=time.time()
                )
                
            except Exception as e:
                logger.error(f"Failed to stop optimization: {e}")
                raise HTTPException(status_code=500, detail=str(e))
        
        @app.get("/v1/optimization/status")
        async def get_optimization_status(raw_request: Request) -> OptimizationResponse:
            """Get current optimization status."""
            try:
                if not self.optimizer:
                    status = {"enabled": False, "message": "Optimization not initialized"}
                else:
                    status = self.optimizer.get_optimization_status()
                    status["enabled"] = self.optimization_enabled
                
                return OptimizationResponse(
                    success=True,
                    message="Optimization status retrieved",
                    optimization_status=status,
                    timestamp=time.time()
                )
                
            except Exception as e:
                logger.error(f"Failed to get optimization status: {e}")
                raise HTTPException(status_code=500, detail=str(e))
        
        @app.get("/v1/optimization/metrics")
        async def get_load_metrics(raw_request: Request) -> LoadMetricsResponse:
            """Get current server load metrics."""
            try:
                if not self.optimizer:
                    raise HTTPException(status_code=400, detail="Optimization not initialized")
                
                # Collect current metrics
                load_metrics = self.optimizer.parameter_tuner.collect_load_metrics()
                
                return LoadMetricsResponse(
                    timestamp=load_metrics.timestamp,
                    cpu_usage_percent=load_metrics.cpu_usage_percent,
                    memory_usage_percent=load_metrics.memory_usage_percent,
                    gpu_utilization=load_metrics.gpu_utilization,
                    active_requests=load_metrics.active_requests,
                    queue_depth=load_metrics.queue_depth,
                    throughput_tokens_per_sec=load_metrics.throughput_tokens_per_sec,
                    avg_latency_ms=load_metrics.avg_latency_ms,
                    error_rate=load_metrics.error_rate,
                    overall_load_score=load_metrics.overall_load_score
                )
                
            except Exception as e:
                logger.error(f"Failed to get load metrics: {e}")
                raise HTTPException(status_code=500, detail=str(e))
        
        @app.get("/v1/optimization/ensemble")
        async def get_ensemble_status(raw_request: Request) -> EnsembleStatusResponse:
            """Get current ensemble status."""
            try:
                if not self.optimizer:
                    raise HTTPException(status_code=400, detail="Optimization not initialized")
                
                ensemble_status = self.optimizer.ensemble_manager.get_ensemble_status()
                
                return EnsembleStatusResponse(
                    enabled=ensemble_status["enabled"],
                    strategy=ensemble_status["strategy"],
                    model_count=ensemble_status["model_count"],
                    max_size=ensemble_status["max_size"],
                    models=ensemble_status["models"]
                )
                
            except Exception as e:
                logger.error(f"Failed to get ensemble status: {e}")
                raise HTTPException(status_code=500, detail=str(e))
        
        @app.post("/v1/optimization/ensemble/add_model")
        async def add_model_to_ensemble(
            model_id: str,
            initial_weight: float = 1.0,
            raw_request: Request = None
        ) -> OptimizationResponse:
            """Add a model to the ensemble."""
            try:
                if not self.optimizer:
                    raise HTTPException(status_code=400, detail="Optimization not initialized")
                
                # This would integrate with actual model loading
                # For now, we create a placeholder model instance
                placeholder_model = torch.nn.Linear(1, 1)  # Minimal placeholder
                
                self.optimizer.ensemble_manager.add_model_to_ensemble(
                    model_id=model_id,
                    model_instance=placeholder_model,
                    initial_weight=initial_weight,
                    performance_metric=1.0
                )
                
                ensemble_status = self.optimizer.ensemble_manager.get_ensemble_status()
                
                return OptimizationResponse(
                    success=True,
                    message=f"Model {model_id} added to ensemble",
                    optimization_status={"ensemble": ensemble_status},
                    timestamp=time.time()
                )
                
            except Exception as e:
                logger.error(f"Failed to add model to ensemble: {e}")
                raise HTTPException(status_code=500, detail=str(e))
        
        @app.get("/v1/optimization/report")
        async def get_performance_report(
            export_file: bool = False,
            raw_request: Request = None
        ) -> PerformanceReportResponse:
            """Get comprehensive performance optimization report."""
            try:
                if not self.optimizer:
                    raise HTTPException(status_code=400, detail="Optimization not initialized")
                
                # Generate report
                filepath = None
                if export_file:
                    report = self.optimizer.export_performance_report()
                    filepath = f"/tmp/server_optimization_report_{int(time.time())}.json"
                else:
                    report = self.optimizer.export_performance_report(filepath=None)
                
                # Calculate performance grade
                status = self.optimizer.get_optimization_status()
                recent_perf = status.get("recent_performance", {})
                
                # Simple grading based on success rate and latency
                success_rate = recent_perf.get("success_rate", 1.0)
                avg_latency = recent_perf.get("avg_latency_ms", 100.0)
                
                if success_rate >= 0.99 and avg_latency <= 100:
                    grade = "A+"
                elif success_rate >= 0.95 and avg_latency <= 200:
                    grade = "A"
                elif success_rate >= 0.90 and avg_latency <= 500:
                    grade = "B"
                elif success_rate >= 0.80:
                    grade = "C"
                else:
                    grade = "D"
                
                summary = {
                    "total_optimizations": len(report.get("performance_history", [])),
                    "current_strategy": status.get("current_strategy", {}).get("strategy", "unknown"),
                    "ensemble_models": status.get("ensemble_status", {}).get("model_count", 0),
                    "compilation_enabled": status.get("configuration", {}).get("torch_compile_enabled", False),
                    "success_rate": success_rate,
                    "avg_latency_ms": avg_latency
                }
                
                return PerformanceReportResponse(
                    report_timestamp=report["report_timestamp"],
                    report_filepath=filepath,
                    summary=summary,
                    performance_grade=grade
                )
                
            except Exception as e:
                logger.error(f"Failed to generate performance report: {e}")
                raise HTTPException(status_code=500, detail=str(e))
        
        @app.post("/v1/optimization/force_recompile")
        async def force_model_recompilation(
            model_id: str = "default",
            raw_request: Request = None
        ) -> OptimizationResponse:
            """Force recompilation of a specific model."""
            try:
                if not self.optimizer:
                    raise HTTPException(status_code=400, detail="Optimization not initialized")
                
                # Clear compiled model cache for the specified model
                if model_id in self.optimizer.model_compiler.compiled_models:
                    del self.optimizer.model_compiler.compiled_models[model_id]
                
                message = f"Model {model_id} marked for recompilation"
                status = self.optimizer.get_optimization_status()
                
                return OptimizationResponse(
                    success=True,
                    message=message,
                    optimization_status=status,
                    timestamp=time.time()
                )
                
            except Exception as e:
                logger.error(f"Failed to force recompilation: {e}")
                raise HTTPException(status_code=500, detail=str(e))
        
        logger.info("Server-side optimization routes configured")
    
    async def _initialize_optimizer(self, engine: AsyncAphrodite):
        """Initialize the server-side optimizer."""
        try:
            # Get model configuration from engine
            model_config = engine.engine.model_config
            
            # Create optimization configuration
            optimization_config = OptimizationConfig(
                enable_torch_compile=True,
                enable_dynamic_tuning=True,
                enable_ensemble=True,
                enable_performance_tracking=True,
                tuning_interval_sec=30.0,
                max_ensemble_size=3
            )
            
            # Initialize optimizer
            self.optimizer = ServerSideOptimizer(
                engine_client=engine,
                model_config=model_config,
                lora_config=None,  # Could be configured if LoRA is used
                optimization_config=optimization_config
            )
            
            logger.info("Server-side optimizer initialized")
            
        except Exception as e:
            logger.error(f"Failed to initialize optimizer: {e}")
            raise
    
    async def optimize_request_processing(
        self, 
        model_instance: torch.nn.Module,
        request_context: Dict[str, Any]
    ) -> torch.nn.Module:
        """Optimize model for request processing."""
        if not self.optimizer or not self.optimization_enabled:
            return model_instance
        
        try:
            # Get optimal model from ensemble or compile current model
            optimal_model = self.optimizer.select_optimal_model(request_context)
            if optimal_model is not None:
                return optimal_model
            
            # Fall back to compiling the current model
            return await self.optimizer.optimize_model_for_request(
                model_instance, 
                request_context
            )
            
        except Exception as e:
            logger.error(f"Model optimization failed: {e}")
            return model_instance
    
    def record_request_metrics(
        self,
        model_id: str,
        start_time: float,
        end_time: float,
        success: bool,
        additional_metrics: Optional[Dict[str, Any]] = None
    ):
        """Record request performance metrics for optimization."""
        if not self.optimizer or not self.optimization_enabled:
            return
        
        try:
            latency_ms = (end_time - start_time) * 1000.0
            
            self.optimizer.record_request_performance(
                model_id=model_id,
                latency_ms=latency_ms,
                success=success,
                additional_metrics=additional_metrics
            )
            
        except Exception as e:
            logger.error(f"Failed to record request metrics: {e}")


# Integration helper functions
async def setup_optimization_serving(app, engine: AsyncAphrodite) -> OptimizationServingMixin:
    """
    Set up server-side optimization serving endpoints.
    
    Args:
        app: FastAPI application instance
        engine: AsyncAphrodite engine instance
        
    Returns:
        OptimizationServingMixin instance for integration
    """
    optimization_mixin = OptimizationServingMixin()
    optimization_mixin._setup_optimization_routes(app, engine)
    return optimization_mixin


def create_optimized_request_context(
    model_name: str,
    request_id: str,
    user_preferences: Optional[Dict[str, Any]] = None,
    load_info: Optional[Dict[str, Any]] = None
) -> Dict[str, Any]:
    """
    Create request context for optimization decisions.
    
    Args:
        model_name: Name/ID of the model
        request_id: Unique request identifier
        user_preferences: User-specific preferences
        load_info: Current server load information
        
    Returns:
        Request context dictionary for optimization
    """
    context = {
        "model_id": model_name,
        "request_id": request_id,
        "timestamp": time.time(),
    }
    
    if user_preferences:
        context["user_preferences"] = user_preferences
    
    if load_info:
        context["load_info"] = load_info
    
    return context


class OptimizationMiddleware:
    """
    Middleware for integrating optimization with request processing.
    
    This middleware automatically applies server-side optimizations
    to requests and collects performance metrics.
    """
    
    def __init__(self, optimization_mixin: OptimizationServingMixin):
        self.optimization_mixin = optimization_mixin
    
    async def process_request(
        self,
        model_instance: torch.nn.Module,
        request_data: Dict[str, Any],
        process_func: callable
    ) -> Any:
        """
        Process request with optimization.
        
        Args:
            model_instance: Model to use for processing
            request_data: Request data
            process_func: Function to process the request
            
        Returns:
            Processing result
        """
        start_time = time.time()
        model_id = request_data.get("model", "default")
        request_id = request_data.get("request_id", f"req_{int(start_time * 1000)}")
        
        try:
            # Create request context
            request_context = create_optimized_request_context(
                model_name=model_id,
                request_id=request_id,
                user_preferences=request_data.get("user_preferences"),
                load_info={"timestamp": start_time}
            )
            
            # Optimize model for this request
            optimized_model = await self.optimization_mixin.optimize_request_processing(
                model_instance,
                request_context
            )
            
            # Process request with optimized model
            result = await process_func(optimized_model, request_data)
            
            # Record successful processing
            end_time = time.time()
            self.optimization_mixin.record_request_metrics(
                model_id=model_id,
                start_time=start_time,
                end_time=end_time,
                success=True,
                additional_metrics={
                    "request_id": request_id,
                    "optimization_applied": optimized_model is not model_instance,
                    "result_length": len(str(result)) if result else 0
                }
            )
            
            return result
            
        except Exception as e:
            # Record failed processing
            end_time = time.time()
            self.optimization_mixin.record_request_metrics(
                model_id=model_id,
                start_time=start_time,
                end_time=end_time,
                success=False,
                additional_metrics={
                    "request_id": request_id,
                    "error": str(e),
                    "error_type": type(e).__name__
                }
            )
            raise