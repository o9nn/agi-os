"""
Server-Side Model Optimization System for Aphrodite Engine.

Implements server-side model compilation, load-based parameter tuning, 
and ensemble serving for improved reliability and performance under 
varying server conditions as part of Phase 8 SSR-focused MLOps.
"""

import asyncio
import logging
import time
from concurrent.futures import ThreadPoolExecutor
from dataclasses import dataclass
from typing import Dict, List, Optional, Any, Callable, Union
import threading
import psutil
import torch
import torch.nn as nn
from pathlib import Path

from aphrodite.common.config import ModelConfig, LoRAConfig
from aphrodite.engine.dynamic_model_manager import DynamicModelManager, DynamicUpdateConfig
from aphrodite.engine.protocol import EngineClient
from aphrodite.endpoints.deep_tree_echo.monitoring import metrics_collector, alert_manager
from aphrodite.endpoints.deep_tree_echo.performance_integration import IntegratedDataPipelineMonitor

logger = logging.getLogger(__name__)


@dataclass
class ServerLoadMetrics:
    """Server load and performance metrics."""
    timestamp: float
    cpu_usage_percent: float
    memory_usage_percent: float
    gpu_utilization: float
    active_requests: int
    queue_depth: int
    throughput_tokens_per_sec: float
    avg_latency_ms: float
    error_rate: float
    
    @property
    def overall_load_score(self) -> float:
        """Calculate normalized load score (0-1, where 1 is maximum load)."""
        # Weight different metrics based on their impact on performance
        cpu_weight = 0.3
        memory_weight = 0.25
        gpu_weight = 0.3
        queue_weight = 0.15
        
        cpu_score = min(1.0, self.cpu_usage_percent / 100.0)
        memory_score = min(1.0, self.memory_usage_percent / 100.0)
        gpu_score = min(1.0, self.gpu_utilization / 100.0)
        queue_score = min(1.0, self.queue_depth / 100.0)  # Assume 100 is high queue depth
        
        return (cpu_weight * cpu_score + 
                memory_weight * memory_score + 
                gpu_weight * gpu_score + 
                queue_weight * queue_score)


@dataclass
class OptimizationConfig:
    """Configuration for server-side optimization."""
    # Compilation optimization settings
    enable_torch_compile: bool = True
    compile_mode: str = "default"  # "default", "reduce-overhead", "max-autotune"
    compile_fullgraph: bool = False
    
    # Load-based tuning settings
    enable_dynamic_tuning: bool = True
    tuning_interval_sec: float = 30.0
    load_threshold_high: float = 0.8  # Optimize for performance when load > 80%
    load_threshold_low: float = 0.3   # Optimize for quality when load < 30%
    
    # Model ensemble settings
    enable_ensemble: bool = True
    max_ensemble_size: int = 3
    ensemble_strategy: str = "weighted_voting"  # "weighted_voting", "best_of_n", "adaptive"
    
    # Performance monitoring
    enable_performance_tracking: bool = True
    metrics_history_size: int = 1000
    
    # Auto-scaling settings
    enable_auto_scaling: bool = True
    scale_up_threshold: float = 0.85
    scale_down_threshold: float = 0.4
    min_batch_size: int = 1
    max_batch_size: int = 32


class ModelCompiler:
    """Handles server-side model compilation for optimization."""
    
    def __init__(self, config: OptimizationConfig):
        self.config = config
        self.compiled_models: Dict[str, torch.nn.Module] = {}
        self.compilation_stats: Dict[str, Dict[str, Any]] = {}
        
    def compile_model(self, model: torch.nn.Module, model_id: str) -> torch.nn.Module:
        """Compile model for optimized inference."""
        if not self.config.enable_torch_compile:
            logger.info(f"Model compilation disabled for {model_id}")
            return model
            
        try:
            logger.info(f"Starting compilation for model {model_id} with mode: {self.config.compile_mode}")
            start_time = time.time()
            
            # Configure compilation options based on server load
            current_load = self._get_current_load_score()
            
            if current_load > 0.8:
                # High load: optimize for maximum performance
                compile_mode = "max-autotune"
                dynamic = False
            elif current_load < 0.3:
                # Low load: optimize for flexibility
                compile_mode = "default"
                dynamic = True
            else:
                # Medium load: balanced approach
                compile_mode = self.config.compile_mode
                dynamic = not self.config.compile_fullgraph
            
            compiled_model = torch.compile(
                model,
                mode=compile_mode,
                fullgraph=self.config.compile_fullgraph,
                dynamic=dynamic
            )
            
            compilation_time = time.time() - start_time
            
            # Store compiled model and stats
            self.compiled_models[model_id] = compiled_model
            self.compilation_stats[model_id] = {
                "compilation_time": compilation_time,
                "compile_mode": compile_mode,
                "dynamic": dynamic,
                "timestamp": time.time(),
                "load_score_at_compilation": current_load
            }
            
            logger.info(f"Successfully compiled model {model_id} in {compilation_time:.2f}s")
            return compiled_model
            
        except Exception as e:
            logger.error(f"Failed to compile model {model_id}: {e}")
            # Return original model if compilation fails
            return model
    
    def get_compiled_model(self, model_id: str) -> Optional[torch.nn.Module]:
        """Get compiled model if available."""
        return self.compiled_models.get(model_id)
    
    def _get_current_load_score(self) -> float:
        """Get current server load score."""
        # Integration with monitoring system
        metrics = metrics_collector.get_current_metrics()
        
        # Calculate load score from available metrics
        cpu_usage = psutil.cpu_percent()
        memory_usage = psutil.virtual_memory().percent
        
        # Approximate GPU utilization (would need actual GPU monitoring)
        gpu_util = 50.0  # Placeholder
        
        load_metrics = ServerLoadMetrics(
            timestamp=time.time(),
            cpu_usage_percent=cpu_usage,
            memory_usage_percent=memory_usage,
            gpu_utilization=gpu_util,
            active_requests=metrics.active_requests,
            queue_depth=0,  # Would need queue monitoring
            throughput_tokens_per_sec=metrics.throughput_rps * 100,  # Approximate
            avg_latency_ms=metrics.avg_response_time_ms,
            error_rate=metrics.error_rate
        )
        
        return load_metrics.overall_load_score


class LoadBasedParameterTuner:
    """Dynamically tunes model parameters based on server load."""
    
    def __init__(self, config: OptimizationConfig, model_manager: DynamicModelManager):
        self.config = config
        self.model_manager = model_manager
        self.load_history: List[ServerLoadMetrics] = []
        self.parameter_adjustments: Dict[str, Any] = {}
        self.tuning_lock = threading.RLock()
        
        # Performance vs Quality trade-off parameters
        self.performance_params = {
            "max_tokens": 512,      # Reduced for speed
            "temperature": 0.7,     # More focused responses
            "top_k": 40,           # More selective
            "repetition_penalty": 1.1
        }
        
        self.quality_params = {
            "max_tokens": 2048,     # Full context for quality
            "temperature": 0.8,     # More creative responses
            "top_k": 100,          # More diverse
            "repetition_penalty": 1.0
        }
        
    def collect_load_metrics(self) -> ServerLoadMetrics:
        """Collect current server load metrics."""
        # Get system metrics
        cpu_usage = psutil.cpu_percent(interval=1.0)
        memory = psutil.virtual_memory()
        
        # Get metrics from monitoring system
        monitoring_metrics = metrics_collector.get_current_metrics()
        
        # Placeholder GPU utilization (would need actual GPU monitoring)
        try:
            import pynvml
            pynvml.nvmlInit()
            handle = pynvml.nvmlDeviceGetHandleByIndex(0)
            gpu_util = pynvml.nvmlDeviceGetUtilizationRates(handle).gpu
        except:
            gpu_util = 50.0  # Fallback estimate
        
        metrics = ServerLoadMetrics(
            timestamp=time.time(),
            cpu_usage_percent=cpu_usage,
            memory_usage_percent=memory.percent,
            gpu_utilization=gpu_util,
            active_requests=monitoring_metrics.active_requests,
            queue_depth=max(0, monitoring_metrics.active_requests - 10),  # Estimate queue
            throughput_tokens_per_sec=monitoring_metrics.throughput_rps * 100,
            avg_latency_ms=monitoring_metrics.avg_response_time_ms,
            error_rate=monitoring_metrics.error_rate
        )
        
        # Store metrics history
        with self.tuning_lock:
            self.load_history.append(metrics)
            if len(self.load_history) > self.config.metrics_history_size:
                self.load_history = self.load_history[-self.config.metrics_history_size:]
        
        return metrics
    
    def determine_optimization_strategy(self, load_metrics: ServerLoadMetrics) -> Dict[str, Any]:
        """Determine optimization strategy based on load."""
        load_score = load_metrics.overall_load_score
        
        if load_score >= self.config.load_threshold_high:
            # High load: optimize for performance
            strategy = "performance"
            params = self.performance_params.copy()
            
            # Additional performance optimizations for very high load
            if load_score >= 0.9:
                params["max_tokens"] = min(256, params["max_tokens"])
                params["top_k"] = min(20, params["top_k"])
                
        elif load_score <= self.config.load_threshold_low:
            # Low load: optimize for quality
            strategy = "quality"
            params = self.quality_params.copy()
            
        else:
            # Medium load: balanced approach
            strategy = "balanced"
            # Interpolate between performance and quality parameters
            performance_weight = (load_score - self.config.load_threshold_low) / (
                self.config.load_threshold_high - self.config.load_threshold_low
            )
            quality_weight = 1.0 - performance_weight
            
            params = {}
            for key in self.performance_params:
                perf_val = self.performance_params[key]
                qual_val = self.quality_params[key]
                
                if isinstance(perf_val, (int, float)):
                    params[key] = perf_val * performance_weight + qual_val * quality_weight
                else:
                    params[key] = perf_val if performance_weight > 0.5 else qual_val
        
        return {
            "strategy": strategy,
            "parameters": params,
            "load_score": load_score,
            "timestamp": load_metrics.timestamp
        }
    
    async def apply_parameter_adjustments(self, optimization: Dict[str, Any]) -> bool:
        """Apply parameter adjustments to the model."""
        try:
            strategy = optimization["strategy"]
            params = optimization["parameters"]
            
            logger.info(f"Applying {strategy} optimization strategy with load score: {optimization['load_score']:.2f}")
            
            # Store current adjustments
            with self.tuning_lock:
                self.parameter_adjustments = optimization
            
            # Apply adjustments through model manager
            # Note: This would integrate with actual model parameter updates
            # For now, we log the intended adjustments
            for param_name, value in params.items():
                logger.debug(f"Setting {param_name} = {value}")
            
            return True
            
        except Exception as e:
            logger.error(f"Failed to apply parameter adjustments: {e}")
            return False
    
    def get_current_strategy(self) -> Optional[Dict[str, Any]]:
        """Get current optimization strategy."""
        with self.tuning_lock:
            return self.parameter_adjustments.copy() if self.parameter_adjustments else None


class ModelEnsembleManager:
    """Manages model ensemble serving for improved reliability."""
    
    def __init__(self, config: OptimizationConfig):
        self.config = config
        self.ensemble_models: List[Dict[str, Any]] = []
        self.ensemble_weights: Dict[str, float] = {}
        self.performance_history: Dict[str, List[float]] = {}
        self.ensemble_lock = threading.RLock()
        
    def add_model_to_ensemble(
        self, 
        model_id: str, 
        model_instance: Any, 
        initial_weight: float = 1.0,
        performance_metric: float = 1.0
    ):
        """Add a model to the ensemble."""
        if not self.config.enable_ensemble:
            return
            
        with self.ensemble_lock:
            # Check ensemble size limit
            if len(self.ensemble_models) >= self.config.max_ensemble_size:
                # Remove lowest performing model
                self._remove_worst_performer()
            
            model_info = {
                "model_id": model_id,
                "model_instance": model_instance,
                "added_timestamp": time.time(),
                "request_count": 0,
                "total_latency": 0.0,
                "error_count": 0,
                "last_used": time.time()
            }
            
            self.ensemble_models.append(model_info)
            self.ensemble_weights[model_id] = initial_weight
            self.performance_history[model_id] = [performance_metric]
            
            logger.info(f"Added model {model_id} to ensemble (size: {len(self.ensemble_models)})")
    
    def select_model_for_request(self, request_context: Dict[str, Any]) -> Optional[Dict[str, Any]]:
        """Select best model from ensemble for a request."""
        if not self.config.enable_ensemble or not self.ensemble_models:
            return None
            
        with self.ensemble_lock:
            if self.config.ensemble_strategy == "weighted_voting":
                return self._select_weighted_model(request_context)
            elif self.config.ensemble_strategy == "best_of_n":
                return self._select_best_model()
            elif self.config.ensemble_strategy == "adaptive":
                return self._select_adaptive_model(request_context)
            else:
                # Default to random selection
                return self.ensemble_models[0] if self.ensemble_models else None
    
    def _select_weighted_model(self, request_context: Dict[str, Any]) -> Dict[str, Any]:
        """Select model using weighted selection based on performance."""
        import random
        
        # Calculate selection probabilities based on weights and performance
        total_weight = 0.0
        model_probs = []
        
        for model_info in self.ensemble_models:
            model_id = model_info["model_id"]
            weight = self.ensemble_weights.get(model_id, 1.0)
            
            # Adjust weight based on recent performance
            if model_id in self.performance_history and self.performance_history[model_id]:
                recent_perf = sum(self.performance_history[model_id][-10:]) / len(self.performance_history[model_id][-10:])
                weight *= recent_perf
            
            # Penalize models with high error rates
            if model_info["request_count"] > 0:
                error_rate = model_info["error_count"] / model_info["request_count"]
                weight *= (1.0 - min(0.9, error_rate))
            
            model_probs.append(weight)
            total_weight += weight
        
        if total_weight <= 0:
            return self.ensemble_models[0]
        
        # Normalize probabilities
        model_probs = [p / total_weight for p in model_probs]
        
        # Select model using weighted random selection
        rand_val = random.random()
        cumulative_prob = 0.0
        
        for i, prob in enumerate(model_probs):
            cumulative_prob += prob
            if rand_val <= cumulative_prob:
                return self.ensemble_models[i]
        
        return self.ensemble_models[-1]  # Fallback
    
    def _select_best_model(self) -> Dict[str, Any]:
        """Select the best performing model."""
        best_model = None
        best_score = -1.0
        
        for model_info in self.ensemble_models:
            model_id = model_info["model_id"]
            
            # Calculate performance score
            score = 1.0
            
            if model_info["request_count"] > 0:
                # Factor in latency (lower is better)
                avg_latency = model_info["total_latency"] / model_info["request_count"]
                latency_score = max(0.1, 1.0 - (avg_latency / 1000.0))  # Normalize assuming 1s is poor
                
                # Factor in error rate (lower is better)
                error_rate = model_info["error_count"] / model_info["request_count"]
                error_score = 1.0 - min(0.9, error_rate)
                
                score = latency_score * error_score
            
            if score > best_score:
                best_score = score
                best_model = model_info
        
        return best_model or self.ensemble_models[0]
    
    def _select_adaptive_model(self, request_context: Dict[str, Any]) -> Dict[str, Any]:
        """Adaptively select model based on request characteristics and current load."""
        # Get current load
        load_metrics = ServerLoadMetrics(
            timestamp=time.time(),
            cpu_usage_percent=psutil.cpu_percent(),
            memory_usage_percent=psutil.virtual_memory().percent,
            gpu_utilization=50.0,  # Placeholder
            active_requests=10,     # Placeholder
            queue_depth=0,
            throughput_tokens_per_sec=100.0,
            avg_latency_ms=100.0,
            error_rate=0.01
        )
        
        load_score = load_metrics.overall_load_score
        
        if load_score > 0.8:
            # High load: prefer fastest model
            return min(
                self.ensemble_models, 
                key=lambda m: m["total_latency"] / max(1, m["request_count"])
            )
        elif load_score < 0.3:
            # Low load: prefer highest quality model (lowest error rate)
            return min(
                self.ensemble_models,
                key=lambda m: m["error_count"] / max(1, m["request_count"])
            )
        else:
            # Medium load: use weighted selection
            return self._select_weighted_model(request_context)
    
    def _remove_worst_performer(self):
        """Remove the worst performing model from ensemble."""
        if len(self.ensemble_models) <= 1:
            return
            
        worst_model = None
        worst_score = float('inf')
        
        for model_info in self.ensemble_models:
            model_id = model_info["model_id"]
            
            # Calculate performance score (higher is better, so we want lowest for removal)
            score = 1.0
            
            if model_info["request_count"] > 0:
                avg_latency = model_info["total_latency"] / model_info["request_count"]
                error_rate = model_info["error_count"] / model_info["request_count"]
                
                # Combined score (lower is worse)
                score = avg_latency + (error_rate * 1000)  # Weight errors heavily
            
            if score < worst_score:
                worst_score = score
                worst_model = model_info
        
        if worst_model:
            model_id = worst_model["model_id"]
            self.ensemble_models.remove(worst_model)
            if model_id in self.ensemble_weights:
                del self.ensemble_weights[model_id]
            if model_id in self.performance_history:
                del self.performance_history[model_id]
            
            logger.info(f"Removed worst performing model {model_id} from ensemble")
    
    def update_model_performance(
        self, 
        model_id: str, 
        latency_ms: float, 
        success: bool
    ):
        """Update performance metrics for a model."""
        with self.ensemble_lock:
            # Find model in ensemble
            model_info = None
            for info in self.ensemble_models:
                if info["model_id"] == model_id:
                    model_info = info
                    break
            
            if model_info:
                model_info["request_count"] += 1
                model_info["total_latency"] += latency_ms
                model_info["last_used"] = time.time()
                
                if not success:
                    model_info["error_count"] += 1
                
                # Update performance history
                if model_id in self.performance_history:
                    performance_score = 1.0 / max(0.1, latency_ms / 100.0)  # Inverse latency
                    if not success:
                        performance_score *= 0.1  # Heavy penalty for errors
                    
                    self.performance_history[model_id].append(performance_score)
                    
                    # Keep only recent history
                    if len(self.performance_history[model_id]) > 100:
                        self.performance_history[model_id] = self.performance_history[model_id][-100:]
    
    def get_ensemble_status(self) -> Dict[str, Any]:
        """Get current ensemble status."""
        with self.ensemble_lock:
            status = {
                "enabled": self.config.enable_ensemble,
                "strategy": self.config.ensemble_strategy,
                "model_count": len(self.ensemble_models),
                "max_size": self.config.max_ensemble_size,
                "models": []
            }
            
            for model_info in self.ensemble_models:
                model_id = model_info["model_id"]
                model_status = {
                    "model_id": model_id,
                    "weight": self.ensemble_weights.get(model_id, 1.0),
                    "request_count": model_info["request_count"],
                    "avg_latency_ms": (
                        model_info["total_latency"] / model_info["request_count"] 
                        if model_info["request_count"] > 0 else 0
                    ),
                    "error_rate": (
                        model_info["error_count"] / model_info["request_count"] 
                        if model_info["request_count"] > 0 else 0
                    ),
                    "last_used": model_info["last_used"],
                    "added_timestamp": model_info["added_timestamp"]
                }
                status["models"].append(model_status)
            
            return status


class ServerSideOptimizer:
    """
    Main server-side optimization system integrating all optimization components.
    
    Provides comprehensive model optimization including compilation, dynamic tuning,
    and ensemble serving for optimal performance under varying server conditions.
    """
    
    def __init__(
        self,
        engine_client: EngineClient,
        model_config: ModelConfig,
        lora_config: Optional[LoRAConfig] = None,
        optimization_config: Optional[OptimizationConfig] = None
    ):
        self.engine_client = engine_client
        self.model_config = model_config
        self.lora_config = lora_config
        self.config = optimization_config or OptimizationConfig()
        
        # Initialize components
        self.model_compiler = ModelCompiler(self.config)
        
        # Initialize dynamic model manager
        dynamic_config = DynamicUpdateConfig(
            enable_incremental_learning=True,
            enable_versioning=True,
            auto_rollback_threshold=0.1,
            checkpoint_interval=100
        )
        self.model_manager = DynamicModelManager(
            engine_client, model_config, lora_config, dynamic_config
        )
        
        self.parameter_tuner = LoadBasedParameterTuner(self.config, self.model_manager)
        self.ensemble_manager = ModelEnsembleManager(self.config)
        
        # Optimization state
        self._optimization_task: Optional[asyncio.Task] = None
        self._is_running = False
        self._performance_metrics: List[Dict[str, Any]] = []
        self._optimization_lock = threading.RLock()
        
        logger.info("ServerSideOptimizer initialized with configuration")
    
    async def start_optimization(self):
        """Start the server-side optimization system."""
        if self._is_running:
            return
        
        logger.info("Starting server-side optimization system")
        
        # Initialize model manager
        await self.model_manager.create_initial_version("Initial optimized version")
        
        # Start optimization loop
        self._is_running = True
        self._optimization_task = asyncio.create_task(self._optimization_loop())
        
        logger.info("Server-side optimization system started")
    
    async def stop_optimization(self):
        """Stop the server-side optimization system."""
        if not self._is_running:
            return
        
        logger.info("Stopping server-side optimization system")
        
        self._is_running = False
        if self._optimization_task:
            self._optimization_task.cancel()
            try:
                await self._optimization_task
            except asyncio.CancelledError:
                pass
        
        logger.info("Server-side optimization system stopped")
    
    async def _optimization_loop(self):
        """Main optimization loop."""
        while self._is_running:
            try:
                # Collect current metrics
                load_metrics = self.parameter_tuner.collect_load_metrics()
                
                # Determine optimization strategy
                optimization = self.parameter_tuner.determine_optimization_strategy(load_metrics)
                
                # Apply parameter adjustments
                await self.parameter_tuner.apply_parameter_adjustments(optimization)
                
                # Store performance metrics
                with self._optimization_lock:
                    perf_data = {
                        "timestamp": time.time(),
                        "load_metrics": load_metrics.__dict__,
                        "optimization": optimization,
                        "ensemble_status": self.ensemble_manager.get_ensemble_status()
                    }
                    self._performance_metrics.append(perf_data)
                    
                    # Keep only recent metrics
                    if len(self._performance_metrics) > self.config.metrics_history_size:
                        self._performance_metrics = self._performance_metrics[-self.config.metrics_history_size:]
                
                # Log periodic status
                if len(self._performance_metrics) % 20 == 0:  # Every ~10 minutes (30s interval * 20)
                    self._log_optimization_status(load_metrics, optimization)
                
                # Wait for next optimization cycle
                await asyncio.sleep(self.config.tuning_interval_sec)
                
            except Exception as e:
                logger.error(f"Error in optimization loop: {e}")
                await asyncio.sleep(5.0)  # Brief pause before retry
    
    def _log_optimization_status(self, load_metrics: ServerLoadMetrics, optimization: Dict[str, Any]):
        """Log current optimization status."""
        logger.info(
            f"Optimization Status - "
            f"Load: {load_metrics.overall_load_score:.2f}, "
            f"Strategy: {optimization['strategy']}, "
            f"CPU: {load_metrics.cpu_usage_percent:.1f}%, "
            f"Memory: {load_metrics.memory_usage_percent:.1f}%, "
            f"Active Requests: {load_metrics.active_requests}, "
            f"Ensemble Models: {len(self.ensemble_manager.ensemble_models)}"
        )
    
    async def optimize_model_for_request(
        self, 
        model_instance: torch.nn.Module, 
        request_context: Dict[str, Any]
    ) -> torch.nn.Module:
        """Optimize model for a specific request."""
        model_id = request_context.get("model_id", "default")
        
        # Check if we have a compiled version
        compiled_model = self.model_compiler.get_compiled_model(model_id)
        if compiled_model is not None:
            return compiled_model
        
        # Compile model if not already compiled
        compiled_model = self.model_compiler.compile_model(model_instance, model_id)
        
        # Add to ensemble if enabled
        if self.config.enable_ensemble:
            self.ensemble_manager.add_model_to_ensemble(
                model_id, 
                compiled_model,
                initial_weight=1.0,
                performance_metric=1.0
            )
        
        return compiled_model
    
    def select_optimal_model(self, request_context: Dict[str, Any]) -> Optional[Any]:
        """Select optimal model for request using ensemble."""
        if not self.config.enable_ensemble:
            return None
        
        selected_model_info = self.ensemble_manager.select_model_for_request(request_context)
        if selected_model_info:
            return selected_model_info["model_instance"]
        
        return None
    
    def record_request_performance(
        self, 
        model_id: str, 
        latency_ms: float, 
        success: bool,
        additional_metrics: Optional[Dict[str, Any]] = None
    ):
        """Record performance metrics for a request."""
        # Update ensemble performance
        if self.config.enable_ensemble:
            self.ensemble_manager.update_model_performance(model_id, latency_ms, success)
        
        # Store additional metrics
        if additional_metrics:
            with self._optimization_lock:
                metric_entry = {
                    "timestamp": time.time(),
                    "model_id": model_id,
                    "latency_ms": latency_ms,
                    "success": success,
                    **additional_metrics
                }
                self._performance_metrics.append(metric_entry)
    
    def get_optimization_status(self) -> Dict[str, Any]:
        """Get comprehensive optimization status."""
        with self._optimization_lock:
            current_strategy = self.parameter_tuner.get_current_strategy()
            
            status = {
                "running": self._is_running,
                "configuration": {
                    "torch_compile_enabled": self.config.enable_torch_compile,
                    "dynamic_tuning_enabled": self.config.enable_dynamic_tuning,
                    "ensemble_enabled": self.config.enable_ensemble,
                    "auto_scaling_enabled": self.config.enable_auto_scaling
                },
                "current_strategy": current_strategy,
                "compilation_stats": self.model_compiler.compilation_stats,
                "ensemble_status": self.ensemble_manager.get_ensemble_status(),
                "performance_metrics_count": len(self._performance_metrics),
                "model_manager_status": self.model_manager.get_status()
            }
            
            # Add recent performance summary
            if self._performance_metrics:
                recent_metrics = self._performance_metrics[-10:]
                status["recent_performance"] = {
                    "avg_latency_ms": sum(
                        m.get("latency_ms", 0) for m in recent_metrics 
                        if "latency_ms" in m
                    ) / max(1, len([m for m in recent_metrics if "latency_ms" in m])),
                    "success_rate": sum(
                        1 for m in recent_metrics 
                        if m.get("success", True)
                    ) / max(1, len(recent_metrics)),
                    "last_optimization": recent_metrics[-1]["timestamp"] if recent_metrics else None
                }
            
            return status
    
    def export_performance_report(self, filepath: Optional[str] = None) -> Dict[str, Any]:
        """Export comprehensive performance optimization report."""
        if not filepath:
            timestamp = int(time.time())
            filepath = f"/tmp/server_optimization_report_{timestamp}.json"
        
        with self._optimization_lock:
            report = {
                "report_timestamp": time.time(),
                "optimization_config": self.config.__dict__,
                "status": self.get_optimization_status(),
                "performance_history": self._performance_metrics[-500:],  # Last 500 entries
                "model_versions": self.model_manager.list_versions(),
            }
        
        # Save report
        import json
        try:
            with open(filepath, 'w') as f:
                json.dump(report, f, indent=2, default=str)
            logger.info(f"Optimization report exported to: {filepath}")
        except Exception as e:
            logger.error(f"Failed to export optimization report: {e}")
        
        return report