"""
Server-Side Continuous Learning Middleware for Aphrodite Engine.

Integrates with the FastAPI server to collect production data, aggregate experiences,
and enable continuous model improvement without service disruption.
"""

import asyncio
import json
import logging
import time
import uuid
from collections import deque
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from typing import Any, Callable, Dict, List, Optional, Set

from fastapi import FastAPI, Request, Response
from starlette.middleware.base import BaseHTTPMiddleware
from starlette.types import ASGIApp

from aphrodite.continuous_learning import (
    ContinuousLearningSystem, 
    ContinuousLearningConfig,
    InteractionData,
    InteractionFeedback,
    ServerSideConfig
)
from aphrodite.dtesn_integration import DTESNDynamicIntegration
from aphrodite.dynamic_model_manager import DynamicModelManager

logger = logging.getLogger(__name__)


@dataclass
class ServerLearningMetrics:
    """Metrics for server-side learning performance."""
    total_requests: int = 0
    learning_requests: int = 0
    background_updates: int = 0
    failed_updates: int = 0
    avg_response_time: float = 0.0
    learning_success_rate: float = 0.0
    last_update: Optional[datetime] = None
    
    def update_request_metrics(self, response_time: float, learning_occurred: bool):
        """Update request-level metrics."""
        self.total_requests += 1
        self.avg_response_time = (
            (self.avg_response_time * (self.total_requests - 1) + response_time) / 
            self.total_requests
        )
        
        if learning_occurred:
            self.learning_requests += 1
            self.learning_success_rate = self.learning_requests / self.total_requests


class ServerSideDataCollector:
    """Collects and aggregates server-side interaction data for learning."""
    
    def __init__(self, config: ServerSideConfig):
        self.config = config
        self.interaction_buffer = deque(maxlen=10000)
        self.feedback_buffer = deque(maxlen=5000)
        self.quality_scores = deque(maxlen=1000)
        
        # Track request patterns
        self.request_patterns = {}
        self.user_sessions = {}
        
    def collect_interaction(self, request: Request, response: Response, 
                          response_time: float, metadata: Dict[str, Any] = None) -> Optional[InteractionData]:
        """Collect interaction data from request/response cycle."""
        
        try:
            # Extract request data
            request_data = self._extract_request_data(request)
            response_data = self._extract_response_data(response)
            
            # Calculate performance feedback
            performance_feedback = self._calculate_performance_feedback(
                request, response, response_time, metadata or {}
            )
            
            # Create interaction data
            interaction = InteractionData(
                interaction_id=f"server_{uuid.uuid4().hex}",
                interaction_type=self._determine_interaction_type(request),
                input_data=request_data,
                output_data=response_data,
                performance_feedback=performance_feedback,
                timestamp=datetime.now(),
                context_metadata={
                    "endpoint": str(request.url.path),
                    "method": request.method,
                    "response_time_ms": response_time,
                    "status_code": response.status_code,
                    **(metadata or {})
                },
                success=200 <= response.status_code < 300
            )
            
            # Buffer interaction for batch processing
            self.interaction_buffer.append(interaction)
            
            # Update quality tracking
            self.quality_scores.append(performance_feedback)
            
            return interaction
            
        except Exception as e:
            logger.error(f"Failed to collect interaction data: {e}")
            return None
    
    def _extract_request_data(self, request: Request) -> Dict[str, Any]:
        """Extract relevant data from request."""
        
        # Extract common request data
        data = {
            "method": request.method,
            "path": request.url.path,
            "headers": dict(request.headers),
            "query_params": dict(request.query_params)
        }
        
        # Extract body data for specific content types
        if hasattr(request.state, 'body_data'):
            body_data = request.state.body_data
            if isinstance(body_data, dict):
                # Extract key fields for learning
                data.update({
                    "prompt": body_data.get("prompt") or 
                             (body_data.get("messages", [{}])[-1].get("content", "") if body_data.get("messages") else ""),
                    "model": body_data.get("model"),
                    "max_tokens": body_data.get("max_tokens"),
                    "temperature": body_data.get("temperature"),
                    "stream": body_data.get("stream", False)
                })
        
        return data
    
    def _extract_response_data(self, response: Response) -> Dict[str, Any]:
        """Extract relevant data from response."""
        
        data = {
            "status_code": response.status_code,
            "headers": dict(response.headers)
        }
        
        # Extract response body if available and reasonable size
        if hasattr(response, 'body') and len(getattr(response, 'body', b'')) < 10000:
            try:
                body_str = response.body.decode('utf-8')
                body_data = json.loads(body_str)
                
                # Extract key response fields
                if "choices" in body_data:
                    choices = body_data["choices"]
                    if choices and len(choices) > 0:
                        choice = choices[0]
                        data["response_text"] = (
                            choice.get("message", {}).get("content") or 
                            choice.get("text", "")
                        )
                
                if "usage" in body_data:
                    data["usage"] = body_data["usage"]
                    
            except (UnicodeDecodeError, json.JSONDecodeError):
                # Skip malformed response data
                pass
        
        return data
    
    def _determine_interaction_type(self, request: Request) -> str:
        """Determine the type of interaction based on request."""
        
        path = request.url.path.lower()
        
        if "/chat/completions" in path:
            return "chat_completion"
        elif "/completions" in path:
            return "text_completion" 
        elif "/embeddings" in path:
            return "embedding"
        elif "/score" in path or "/rerank" in path:
            return "scoring"
        else:
            return "other"
    
    def _calculate_performance_feedback(self, request: Request, response: Response, 
                                      response_time: float, metadata: Dict[str, Any]) -> float:
        """Calculate performance feedback score (-1 to 1)."""
        
        # Start with neutral feedback
        feedback = 0.0
        
        # Response time feedback (faster is better)
        if response_time < 100:  # < 100ms is excellent
            feedback += 0.3
        elif response_time < 500:  # < 500ms is good
            feedback += 0.1
        elif response_time > 2000:  # > 2s is poor
            feedback -= 0.2
        
        # Status code feedback
        if 200 <= response.status_code < 300:
            feedback += 0.2
        elif 400 <= response.status_code < 500:
            feedback -= 0.3
        elif response.status_code >= 500:
            feedback -= 0.5
        
        # Model confidence feedback (if available)
        model_confidence = metadata.get("model_confidence")
        if model_confidence is not None:
            # High confidence gets positive feedback
            if model_confidence > 0.8:
                feedback += 0.2
            elif model_confidence < 0.5:
                feedback -= 0.1
        
        # User satisfaction feedback (if available)
        user_satisfaction = metadata.get("user_satisfaction")
        if user_satisfaction is not None:
            feedback += user_satisfaction * 0.3
        
        # Clamp to [-1, 1] range
        return max(-1.0, min(1.0, feedback))
    
    def get_aggregated_interactions(self, min_quality: float = None) -> List[InteractionData]:
        """Get aggregated interactions ready for learning."""
        
        min_quality = min_quality or self.config.interaction_quality_threshold
        
        # Filter high-quality interactions
        quality_interactions = [
            interaction for interaction in self.interaction_buffer
            if interaction.performance_feedback >= min_quality
        ]
        
        return quality_interactions
    
    def get_data_statistics(self) -> Dict[str, Any]:
        """Get statistics about collected data."""
        
        return {
            "total_interactions": len(self.interaction_buffer),
            "total_feedback": len(self.feedback_buffer),
            "avg_quality_score": sum(self.quality_scores) / max(1, len(self.quality_scores)),
            "interaction_types": self._count_interaction_types(),
            "quality_distribution": self._get_quality_distribution()
        }
    
    def _count_interaction_types(self) -> Dict[str, int]:
        """Count interactions by type."""
        
        type_counts = {}
        for interaction in self.interaction_buffer:
            interaction_type = interaction.interaction_type
            type_counts[interaction_type] = type_counts.get(interaction_type, 0) + 1
        
        return type_counts
    
    def _get_quality_distribution(self) -> Dict[str, int]:
        """Get distribution of quality scores."""
        
        distribution = {"high": 0, "medium": 0, "low": 0}
        
        for score in self.quality_scores:
            if score >= 0.5:
                distribution["high"] += 1
            elif score >= 0.0:
                distribution["medium"] += 1
            else:
                distribution["low"] += 1
        
        return distribution


class BackgroundLearningProcessor:
    """Background processor for continuous learning from server-side data."""
    
    def __init__(self, 
                 continuous_learning_system: ContinuousLearningSystem,
                 data_collector: ServerSideDataCollector,
                 config: ServerSideConfig):
        
        self.learning_system = continuous_learning_system
        self.data_collector = data_collector
        self.config = config
        
        # Background processing state
        self.is_running = False
        self.background_task = None
        self.learning_lock = asyncio.Lock()
        
        # Metrics
        self.metrics = ServerLearningMetrics()
        
        # Track learning failures for rollback decisions
        self.recent_failures = deque(maxlen=10)
        
    async def start_background_processing(self):
        """Start background learning processor."""
        
        if self.is_running:
            logger.warning("Background learning processor already running")
            return
        
        self.is_running = True
        self.background_task = asyncio.create_task(self._background_learning_loop())
        
        logger.info("Started background learning processor")
    
    async def stop_background_processing(self):
        """Stop background learning processor."""
        
        self.is_running = False
        
        if self.background_task:
            self.background_task.cancel()
            try:
                await self.background_task
            except asyncio.CancelledError:
                pass
        
        logger.info("Stopped background learning processor")
    
    async def _background_learning_loop(self):
        """Main background learning loop."""
        
        logger.info(f"Background learning loop started with interval {self.config.background_learning_interval}s")
        
        while self.is_running:
            try:
                await asyncio.sleep(self.config.background_learning_interval)
                
                if not self.is_running:
                    break
                
                # Process accumulated interactions
                await self._process_interactions_batch()
                
            except asyncio.CancelledError:
                break
            except Exception as e:
                logger.error(f"Error in background learning loop: {e}")
                await asyncio.sleep(5)  # Brief pause before retry
        
        logger.info("Background learning loop stopped")
    
    async def _process_interactions_batch(self):
        """Process a batch of interactions for learning."""
        
        async with self.learning_lock:
            try:
                # Get high-quality interactions
                interactions = self.data_collector.get_aggregated_interactions()
                
                if len(interactions) < self.config.min_interactions_for_learning:
                    logger.debug(f"Insufficient interactions for learning: {len(interactions)} < {self.config.min_interactions_for_learning}")
                    return
                
                logger.info(f"Processing batch of {len(interactions)} interactions for learning")
                
                # Apply production safety constraints
                original_config = self._apply_production_constraints()
                
                # Process interactions
                successful_updates = 0
                failed_updates = 0
                
                for interaction in interactions[-50:]:  # Process last 50 interactions
                    try:
                        result = await self.learning_system.learn_from_interaction(interaction)
                        
                        if result.get('success', False):
                            successful_updates += 1
                        else:
                            failed_updates += 1
                            self.recent_failures.append(datetime.now())
                        
                    except Exception as e:
                        logger.error(f"Failed to process interaction {interaction.interaction_id}: {e}")
                        failed_updates += 1
                        self.recent_failures.append(datetime.now())
                
                # Update metrics
                self.metrics.background_updates += 1
                self.metrics.failed_updates += failed_updates
                self.metrics.last_update = datetime.now()
                
                # Restore original configuration
                self._restore_original_config(original_config)
                
                # Check if rollback is needed
                if self.config.enable_rollback_on_failure:
                    await self._check_rollback_conditions()
                
                logger.info(f"Batch processing complete: {successful_updates} successful, {failed_updates} failed")
                
            except Exception as e:
                logger.error(f"Error processing interactions batch: {e}")
                self.metrics.failed_updates += 1
    
    def _apply_production_constraints(self) -> Dict[str, Any]:
        """Apply production safety constraints and return original config."""
        
        # Store original configuration
        original_config = {
            'learning_rate': self.learning_system.current_learning_rate,
            'ewc_lambda': self.learning_system.config.ewc_lambda,
            'replay_frequency': self.learning_system.config.replay_frequency
        }
        
        # Apply conservative production settings
        self.learning_system.current_learning_rate = min(
            self.learning_system.current_learning_rate,
            self.config.max_learning_rate_production
        )
        
        # Increase EWC strength for safety
        self.learning_system.config.ewc_lambda *= 2.0
        
        # Reduce replay frequency in production
        self.learning_system.config.replay_frequency *= 2
        
        logger.debug("Applied production safety constraints for learning")
        
        return original_config
    
    def _restore_original_config(self, original_config: Dict[str, Any]):
        """Restore original configuration after learning."""
        
        self.learning_system.current_learning_rate = original_config['learning_rate']
        self.learning_system.config.ewc_lambda = original_config['ewc_lambda']
        self.learning_system.config.replay_frequency = original_config['replay_frequency']
        
        logger.debug("Restored original learning configuration")
    
    async def _check_rollback_conditions(self):
        """Check if model rollback is needed based on recent failures."""
        
        # Count recent failures (last 10 minutes)
        now = datetime.now()
        recent_failures_count = sum(
            1 for failure_time in self.recent_failures
            if (now - failure_time).total_seconds() < 600
        )
        
        # Rollback if too many recent failures
        if recent_failures_count >= 5:
            logger.warning(f"High failure rate detected: {recent_failures_count} failures in last 10 minutes")
            
            # TODO: Implement model rollback logic
            # await self._rollback_to_stable_version()
    
    def get_learning_metrics(self) -> Dict[str, Any]:
        """Get comprehensive learning metrics."""
        
        return {
            "server_metrics": {
                "total_requests": self.metrics.total_requests,
                "learning_requests": self.metrics.learning_requests,
                "background_updates": self.metrics.background_updates,
                "failed_updates": self.metrics.failed_updates,
                "avg_response_time": self.metrics.avg_response_time,
                "learning_success_rate": self.metrics.learning_success_rate,
                "last_update": self.metrics.last_update.isoformat() if self.metrics.last_update else None
            },
            "data_statistics": self.data_collector.get_data_statistics(),
            "learning_statistics": self.learning_system.get_learning_stats(),
            "recent_failures": len(self.recent_failures),
            "system_status": {
                "background_processing": self.is_running,
                "learning_lock_acquired": self.learning_lock.locked()
            }
        }


class ContinuousLearningMiddleware(BaseHTTPMiddleware):
    """
    FastAPI middleware for server-side continuous learning integration.
    
    Collects production data, aggregates experiences, and enables 
    continuous model improvement without service disruption.
    """
    
    def __init__(self, 
                 app: ASGIApp,
                 continuous_learning_system: ContinuousLearningSystem,
                 config: Optional[ServerSideConfig] = None):
        
        super().__init__(app)
        
        self.config = config or ServerSideConfig()
        self.continuous_learning_system = continuous_learning_system
        
        # Initialize components
        self.data_collector = ServerSideDataCollector(self.config)
        self.background_processor = BackgroundLearningProcessor(
            continuous_learning_system=continuous_learning_system,
            data_collector=self.data_collector,
            config=self.config
        )
        
        # Learning endpoints to monitor
        self.learning_endpoints = {
            "/v1/chat/completions",
            "/v1/completions", 
            "/v1/embeddings",
            "/score",
            "/rerank"
        }
        
        logger.info("ContinuousLearningMiddleware initialized")
    
    async def dispatch(self, request: Request, call_next: Callable) -> Response:
        """Process request and collect learning data."""
        
        # Check if this endpoint should be monitored
        should_monitor = any(
            endpoint in str(request.url.path) 
            for endpoint in self.learning_endpoints
        ) and self.config.enable_request_monitoring
        
        if not should_monitor:
            return await call_next(request)
        
        # Record start time
        start_time = time.time()
        
        # Store request body for learning (if small enough)
        if request.method == "POST":
            try:
                body = await request.body()
                if len(body) < 50000:  # Reasonable size limit
                    request.state.body_data = json.loads(body)
                    
                    # Re-create request with consumed body
                    from starlette.requests import Request as StarletteRequest
                    request = StarletteRequest(
                        scope=request.scope,
                        receive=lambda: {"type": "http.request", "body": body}
                    )
            except (json.JSONDecodeError, UnicodeDecodeError):
                pass  # Skip malformed requests
        
        # Process request
        try:
            response = await call_next(request)
            response_time = (time.time() - start_time) * 1000  # Convert to milliseconds
            
            # Collect interaction data
            if self.config.enable_request_monitoring:
                interaction = self.data_collector.collect_interaction(
                    request=request,
                    response=response, 
                    response_time=response_time
                )
                
                # Update metrics
                self.background_processor.metrics.update_request_metrics(
                    response_time=response_time,
                    learning_occurred=interaction is not None
                )
            
            return response
            
        except Exception as e:
            # Handle request failures
            response_time = (time.time() - start_time) * 1000
            
            logger.error(f"Request failed: {e}")
            
            # Still collect failure data for learning
            if self.config.enable_request_monitoring:
                # Create a mock error response for data collection
                from starlette.responses import JSONResponse
                error_response = JSONResponse(
                    content={"error": str(e)}, 
                    status_code=500
                )
                
                self.data_collector.collect_interaction(
                    request=request,
                    response=error_response,
                    response_time=response_time,
                    metadata={"error": True, "exception": str(e)}
                )
            
            raise  # Re-raise the original exception
    
    async def startup(self):
        """Start background processing."""
        await self.background_processor.start_background_processing()
    
    async def shutdown(self):
        """Stop background processing."""
        await self.background_processor.stop_background_processing()
    
    def get_learning_status(self) -> Dict[str, Any]:
        """Get current learning status and metrics."""
        return self.background_processor.get_learning_metrics()


# Convenience function to set up continuous learning middleware
def setup_continuous_learning_middleware(
    app: FastAPI,
    continuous_learning_system: ContinuousLearningSystem,
    config: Optional[ServerSideConfig] = None
) -> ContinuousLearningMiddleware:
    """
    Set up continuous learning middleware on FastAPI application.
    
    Args:
        app: FastAPI application instance
        continuous_learning_system: Configured continuous learning system
        config: Optional server-side configuration
    
    Returns:
        ContinuousLearningMiddleware instance
    """
    
    middleware = ContinuousLearningMiddleware(
        app=app,
        continuous_learning_system=continuous_learning_system,
        config=config
    )
    
    # Add middleware to application
    app.add_middleware(ContinuousLearningMiddleware, 
                      continuous_learning_system=continuous_learning_system,
                      config=config)
    
    # Add startup and shutdown events
    @app.on_event("startup")
    async def startup_continuous_learning():
        await middleware.startup()
    
    @app.on_event("shutdown") 
    async def shutdown_continuous_learning():
        await middleware.shutdown()
    
    # Add learning status endpoint
    @app.get("/v1/learning/status")
    async def get_learning_status():
        """Get continuous learning system status."""
        return middleware.get_learning_status()
    
    logger.info("Continuous learning middleware configured for FastAPI application")
    
    return middleware