"""
A/B Testing Middleware for Aphrodite Engine
Phase 8 - SSR-Focused MLOps & Production Observability

Implements server-side A/B testing framework for model variants with:
- Traffic splitting and performance comparison
- Automated rollback mechanisms for underperforming models
- Integration with DTESN cache and AAR orchestration systems
"""
import json
import time
import hashlib
from datetime import datetime, timezone
from typing import Dict, Any, Optional, List, Tuple, AsyncGenerator
from dataclasses import dataclass, asdict
from pathlib import Path


from fastapi import Request, Response
from starlette.middleware.base import BaseHTTPMiddleware
from starlette.responses import JSONResponse
from loguru import logger

from aphrodite.common.sampling_params import SamplingParams
from aphrodite.common.outputs import RequestOutput


@dataclass
class ABTestConfig:
    """Configuration for A/B testing framework"""
    enabled: bool = False
    traffic_split_percent: float = 10.0  # Percentage of traffic to variant B
    model_a_name: str = ""  # Stable model
    model_b_name: str = ""  # Canary model
    test_duration_minutes: int = 60
    success_criteria: Dict[str, float] = None
    failure_criteria: Dict[str, float] = None
    auto_rollback: bool = True
    metrics_window_size: int = 1000  # Number of requests for rolling metrics
    
    def __post_init__(self):
        if self.success_criteria is None:
            self.success_criteria = {
                "max_error_rate_increase_percent": 50.0,
                "max_latency_increase_percent": 20.0,
                "min_improvement_threshold": 5.0
            }
        if self.failure_criteria is None:
            self.failure_criteria = {
                "max_error_rate_percent": 5.0,
                "max_latency_ms": 5000.0,
                "max_consecutive_errors": 10
            }


@dataclass
class ABTestMetrics:
    """Metrics tracking for A/B test variants"""
    request_count: int = 0
    error_count: int = 0
    total_latency_ms: float = 0.0
    successful_requests: int = 0
    last_updated: str = ""
    
    @property
    def error_rate(self) -> float:
        if self.request_count == 0:
            return 0.0
        return (self.error_count / self.request_count) * 100.0
    
    @property
    def avg_latency_ms(self) -> float:
        if self.successful_requests == 0:
            return 0.0
        return self.total_latency_ms / self.successful_requests


@dataclass
class ABTestResult:
    """Result of an A/B test execution"""
    test_id: str
    status: str  # "running", "completed", "failed", "rolled_back"
    decision: str  # "promote_b", "keep_a", "rollback", "inconclusive"
    metrics_a: ABTestMetrics
    metrics_b: ABTestMetrics
    start_time: str
    end_time: Optional[str] = None
    reason: str = ""


class ABTestingManager:
    """Core A/B testing management system"""
    
    def __init__(self, config_path: Optional[str] = None):
        self.config = self._load_config(config_path)
        self.active_test: Optional[Dict[str, Any]] = None
        self.metrics_a = ABTestMetrics()
        self.metrics_b = ABTestMetrics()
        self.test_start_time: Optional[float] = None
        self.request_history: List[Tuple[str, float, bool]] = []  # (variant, latency, success)
        
    def _load_config(self, config_path: Optional[str]) -> ABTestConfig:
        """Load A/B testing configuration from file or environment"""
        if config_path and Path(config_path).exists():
            try:
                with open(config_path) as f:
                    config_data = json.load(f)
                return ABTestConfig(**config_data)
            except Exception as e:
                logger.warning(f"Failed to load A/B test config from {config_path}: {e}")
        
        # Default configuration
        return ABTestConfig()
    
    def should_use_variant_b(self, request: Request) -> bool:
        """Determine if request should use variant B based on traffic splitting"""
        if not self.config.enabled or not self.active_test:
            return False
            
        # Use consistent hash-based splitting for session stickiness
        user_id = self._extract_user_identifier(request)
        hash_value = int(hashlib.md5(user_id.encode()).hexdigest(), 16)
        return (hash_value % 100) < self.config.traffic_split_percent
    
    def _extract_user_identifier(self, request: Request) -> str:
        """Extract user identifier for consistent traffic splitting"""
        # Try session ID, API key, or IP address
        user_id = (
            request.headers.get("x-session-id") or
            request.headers.get("authorization", "").split()[-1] if request.headers.get("authorization") else "" or
            request.client.host if request.client else ""
        )
        return user_id or "anonymous"
    
    async def start_ab_test(self, model_a: str, model_b: str) -> str:
        """Start a new A/B test"""
        test_id = f"ab_test_{int(time.time())}"
        
        self.active_test = {
            "test_id": test_id,
            "model_a": model_a,
            "model_b": model_b,
            "status": "running",
            "start_time": datetime.now(timezone.utc).isoformat()
        }
        
        self.config.model_a_name = model_a
        self.config.model_b_name = model_b
        self.config.enabled = True
        self.test_start_time = time.time()
        
        # Reset metrics
        self.metrics_a = ABTestMetrics()
        self.metrics_b = ABTestMetrics()
        self.request_history.clear()
        
        logger.info(f"Started A/B test {test_id}: {model_a} vs {model_b}")
        return test_id
    
    async def stop_ab_test(self, reason: str = "manual_stop") -> ABTestResult:
        """Stop the current A/B test and return results"""
        if not self.active_test:
            raise ValueError("No active A/B test to stop")
        
        test_result = self._evaluate_test_results(reason)
        
        self.config.enabled = False
        self.active_test = None
        
        logger.info(f"Stopped A/B test: {test_result.decision} - {test_result.reason}")
        return test_result
    
    def record_request_metrics(self, variant: str, latency_ms: float, success: bool):
        """Record metrics for a request"""
        metrics = self.metrics_a if variant == "a" else self.metrics_b
        
        metrics.request_count += 1
        if success:
            metrics.successful_requests += 1
            metrics.total_latency_ms += latency_ms
        else:
            metrics.error_count += 1
        
        metrics.last_updated = datetime.now(timezone.utc).isoformat()
        
        # Maintain rolling history
        self.request_history.append((variant, latency_ms, success))
        if len(self.request_history) > self.config.metrics_window_size * 2:
            self.request_history = self.request_history[-self.config.metrics_window_size:]
    
    def _evaluate_test_results(self, reason: str = "completed") -> ABTestResult:
        """Evaluate A/B test results and make decision"""
        if not self.active_test:
            raise ValueError("No active test to evaluate")
        
        decision = "inconclusive"
        evaluation_reason = reason
        
        if self.metrics_a.request_count > 50 and self.metrics_b.request_count > 50:
            # Check failure criteria first
            if (self.metrics_b.error_rate > self.config.failure_criteria["max_error_rate_percent"] or
                self.metrics_b.avg_latency_ms > self.config.failure_criteria["max_latency_ms"]):
                decision = "rollback"
                evaluation_reason = "Variant B failed quality criteria"
            
            # Check success criteria
            elif self._meets_success_criteria():
                decision = "promote_b"
                evaluation_reason = "Variant B meets success criteria"
            
            # Check if variant A is significantly better
            elif self._variant_a_significantly_better():
                decision = "keep_a"
                evaluation_reason = "Variant A performs better"
        
        return ABTestResult(
            test_id=self.active_test["test_id"],
            status="completed",
            decision=decision,
            metrics_a=self.metrics_a,
            metrics_b=self.metrics_b,
            start_time=self.active_test["start_time"],
            end_time=datetime.now(timezone.utc).isoformat(),
            reason=evaluation_reason
        )
    
    def _meets_success_criteria(self) -> bool:
        """Check if variant B meets success criteria"""
        error_rate_improvement = ((self.metrics_a.error_rate - self.metrics_b.error_rate) / 
                                max(self.metrics_a.error_rate, 0.1)) * 100
        
        latency_improvement = ((self.metrics_a.avg_latency_ms - self.metrics_b.avg_latency_ms) / 
                             max(self.metrics_a.avg_latency_ms, 1.0)) * 100
        
        return (
            error_rate_improvement >= self.config.success_criteria["min_improvement_threshold"] or
            latency_improvement >= self.config.success_criteria["min_improvement_threshold"]
        )
    
    def _variant_a_significantly_better(self) -> bool:
        """Check if variant A is significantly better than variant B"""
        error_rate_degradation = ((self.metrics_b.error_rate - self.metrics_a.error_rate) / 
                                max(self.metrics_a.error_rate, 0.1)) * 100
        
        latency_degradation = ((self.metrics_b.avg_latency_ms - self.metrics_a.avg_latency_ms) / 
                             max(self.metrics_a.avg_latency_ms, 1.0)) * 100
        
        return (
            error_rate_degradation > self.config.success_criteria["max_error_rate_increase_percent"] or
            latency_degradation > self.config.success_criteria["max_latency_increase_percent"]
        )
    
    def get_test_status(self) -> Optional[Dict[str, Any]]:
        """Get current test status and metrics"""
        if not self.active_test:
            return None
        
        elapsed_minutes = (time.time() - self.test_start_time) / 60 if self.test_start_time else 0
        
        return {
            "test_id": self.active_test["test_id"],
            "status": self.active_test["status"],
            "elapsed_minutes": elapsed_minutes,
            "traffic_split_percent": self.config.traffic_split_percent,
            "metrics": {
                "variant_a": asdict(self.metrics_a),
                "variant_b": asdict(self.metrics_b)
            },
            "models": {
                "variant_a": self.config.model_a_name,
                "variant_b": self.config.model_b_name
            }
        }


class ABTestingMiddleware(BaseHTTPMiddleware):
    """FastAPI middleware for A/B testing model variants"""
    
    def __init__(self, app, ab_manager: ABTestingManager):
        super().__init__(app)
        self.ab_manager = ab_manager
    
    async def dispatch(self, request: Request, call_next):
        """Process request through A/B testing logic"""
        # Skip A/B testing for non-inference endpoints
        if not self._is_inference_request(request):
            return await call_next(request)
        
        # Determine which variant to use
        use_variant_b = self.ab_manager.should_use_variant_b(request)
        variant = "b" if use_variant_b else "a"
        
        # Add variant information to request state
        request.state.ab_test_variant = variant
        request.state.ab_test_enabled = self.ab_manager.config.enabled
        
        # Process request and measure metrics
        start_time = time.time()
        response = None
        success = False
        
        try:
            response = await call_next(request)
            success = response.status_code < 400
        except Exception as e:
            logger.error(f"Request failed in A/B test variant {variant}: {e}")
            success = False
            raise
        finally:
            # Record metrics
            if self.ab_manager.active_test:
                latency_ms = (time.time() - start_time) * 1000
                self.ab_manager.record_request_metrics(variant, latency_ms, success)
        
        # Add A/B test headers for observability
        if response and self.ab_manager.config.enabled:
            response.headers["X-AB-Test-Variant"] = variant
            response.headers["X-AB-Test-ID"] = self.ab_manager.active_test.get("test_id", "")
        
        return response
    
    def _is_inference_request(self, request: Request) -> bool:
        """Check if request is an inference endpoint that should be A/B tested"""
        inference_paths = {
            "/v1/chat/completions",
            "/v1/completions", 
            "/v1/embeddings",
            "/generate"  # Legacy endpoint
        }
        return request.url.path in inference_paths


# Global A/B testing manager instance
ab_testing_manager = ABTestingManager()


def get_ab_testing_manager() -> ABTestingManager:
    """Get the global A/B testing manager instance"""
    return ab_testing_manager