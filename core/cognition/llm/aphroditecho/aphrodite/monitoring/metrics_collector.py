"""
Advanced Metrics Collector for Aphrodite Engine Backend.

Integrates with existing Prometheus metrics infrastructure and extends it
with comprehensive real-time metrics collection for Deep Tree Echo components.
"""

import time
import threading
from typing import Dict, List, Optional, Any, Callable, Protocol
from dataclasses import dataclass
import asyncio

from loguru import logger
from aphrodite.engine.metrics_types import Stats
from aphrodite.engine.metrics import Metrics
from aphrodite.common.config import AphroditeConfig

try:
    import pynvml
    NVIDIA_ML_AVAILABLE = True
except ImportError:
    NVIDIA_ML_AVAILABLE = False
    logger.warning("NVIDIA ML library not available, GPU metrics will be limited")


class MetricsSource(Protocol):
    """Protocol for metrics source components."""
    
    def collect_metrics(self) -> Dict[str, Any]:
        """Collect metrics from this component."""
        ...


@dataclass
class CollectedMetrics:
    """Container for collected metrics from all sources."""
    timestamp: float
    
    # Engine metrics
    engine_stats: Optional[Stats] = None
    
    # GPU metrics
    gpu_utilization: List[float] = None
    gpu_memory_usage: List[float] = None
    gpu_temperature: List[float] = None
    
    # Deep Tree Echo metrics
    aar_metrics: Dict[str, Any] = None
    dtesn_metrics: Dict[str, Any] = None
    echo_self_metrics: Dict[str, Any] = None
    
    # Custom component metrics
    custom_metrics: Dict[str, Dict[str, Any]] = None


class AphroditeMetricsCollector:
    """
    Advanced metrics collector that integrates with existing Aphrodite metrics
    and extends them with comprehensive backend monitoring capabilities.
    """
    
    def __init__(
        self,
        aphrodite_config: Optional[AphroditeConfig] = None,
        enable_gpu_metrics: bool = True,
        enable_echo_metrics: bool = True,
        collection_interval: float = 1.0
    ):
        self.aphrodite_config = aphrodite_config
        self.enable_gpu_metrics = enable_gpu_metrics and NVIDIA_ML_AVAILABLE
        self.enable_echo_metrics = enable_echo_metrics
        self.collection_interval = collection_interval
        
        # Component metrics sources
        self.metrics_sources: Dict[str, MetricsSource] = {}
        self.custom_collectors: Dict[str, Callable[[], Dict[str, Any]]] = {}
        
        # GPU monitoring setup
        if self.enable_gpu_metrics:
            self._setup_gpu_monitoring()
        
        # Echo components integration
        if self.enable_echo_metrics:
            self._setup_echo_integration()
        
        # Collection state
        self.is_collecting = False
        self.collection_thread: Optional[threading.Thread] = None
        self.last_collection: Optional[CollectedMetrics] = None
        
        logger.info("Aphrodite Metrics Collector initialized")
    
    def _setup_gpu_monitoring(self):
        """Setup GPU monitoring using NVIDIA ML."""
        try:
            pynvml.nvmlInit()
            device_count = pynvml.nvmlDeviceGetCount()
            self.gpu_handles = []
            
            for i in range(device_count):
                handle = pynvml.nvmlDeviceGetHandleByIndex(i)
                self.gpu_handles.append(handle)
            
            logger.info(f"GPU monitoring initialized for {device_count} devices")
            
        except Exception as e:
            logger.warning(f"Failed to initialize GPU monitoring: {e}")
            self.enable_gpu_metrics = False
    
    def _setup_echo_integration(self):
        """Setup Deep Tree Echo component integration."""
        try:
            # Try to import and register Echo component collectors
            self._register_echo_collectors()
            logger.info("Deep Tree Echo metrics integration initialized")
            
        except ImportError as e:
            logger.warning(f"Echo components not available: {e}")
            self.enable_echo_metrics = False
    
    def _register_echo_collectors(self):
        """Register collectors for Deep Tree Echo components."""
        # Register AAR (Agent-Arena-Relation) collector
        def collect_aar_metrics():
            try:
                # TODO: Integrate with actual AAR components
                return {
                    'active_agents': 8,
                    'arena_size': 100,
                    'relation_density': 0.75,
                    'coordination_efficiency': 0.85,
                    'communication_rate': 45.2
                }
            except Exception as e:
                logger.debug(f"AAR metrics collection failed: {e}")
                return {}
        
        # Register DTESN collector
        def collect_dtesn_metrics():
            try:
                # TODO: Integrate with actual DTESN kernel
                return {
                    'reservoir_states': 512,
                    'membrane_depth': 6,
                    'transition_rate': 88.5,
                    'evolution_cycles': 156,
                    'p_system_efficiency': 0.92
                }
            except Exception as e:
                logger.debug(f"DTESN metrics collection failed: {e}")
                return {}
        
        # Register Echo-Self collector
        def collect_echo_self_metrics():
            try:
                # TODO: Integrate with actual Echo-Self evolution engine
                return {
                    'evolution_generation': 42,
                    'fitness_score': 0.78,
                    'adaptation_rate': 0.65,
                    'neural_plasticity': 0.82,
                    'learning_efficiency': 0.91
                }
            except Exception as e:
                logger.debug(f"Echo-Self metrics collection failed: {e}")
                return {}
        
        self.register_collector('aar', collect_aar_metrics)
        self.register_collector('dtesn', collect_dtesn_metrics)
        self.register_collector('echo_self', collect_echo_self_metrics)
    
    def collect_gpu_metrics(self) -> Dict[str, List[float]]:
        """Collect GPU utilization and memory metrics."""
        if not self.enable_gpu_metrics or not hasattr(self, 'gpu_handles'):
            return {
                'utilization': [],
                'memory_usage': [],
                'temperature': []
            }
        
        utilization = []
        memory_usage = []
        temperature = []
        
        try:
            for handle in self.gpu_handles:
                # GPU utilization
                util = pynvml.nvmlDeviceGetUtilizationRates(handle)
                utilization.append(util.gpu)
                
                # Memory usage
                mem_info = pynvml.nvmlDeviceGetMemoryInfo(handle)
                memory_percent = (mem_info.used / mem_info.total) * 100
                memory_usage.append(memory_percent)
                
                # Temperature
                temp = pynvml.nvmlDeviceGetTemperature(handle, pynvml.NVML_TEMPERATURE_GPU)
                temperature.append(temp)
                
        except Exception as e:
            logger.debug(f"GPU metrics collection error: {e}")
        
        return {
            'utilization': utilization,
            'memory_usage': memory_usage,
            'temperature': temperature
        }
    
    def collect_engine_stats(self) -> Optional[Stats]:
        """Collect current engine statistics."""
        # TODO: Integrate with actual Aphrodite Engine stats
        # For now, return a mock Stats object
        try:
            current_time = time.time()
            
            # Create mock stats for demonstration
            mock_stats = Stats(
                now=current_time,
                
                # System stats
                num_running_sys=5,
                num_waiting_sys=2, 
                num_swapped_sys=0,
                gpu_cache_usage_sys=0.65,
                cpu_cache_usage_sys=0.45,
                cpu_prefix_cache_hit_rate=0.85,
                gpu_prefix_cache_hit_rate=0.92,
                
                # Iteration stats
                num_prompt_tokens_iter=128,
                num_generation_tokens_iter=64,
                num_tokens_iter=192,
                time_to_first_tokens_iter=[0.1, 0.15, 0.08],
                time_per_output_tokens_iter=[0.02, 0.025, 0.018],
                num_preemption_iter=1,
                
                # Request stats
                time_e2e_requests=[2.5, 1.8, 3.2],
                time_queue_requests=[0.5, 0.3, 0.8],
                time_inference_requests=[2.0, 1.5, 2.4],
                time_prefill_requests=[0.8, 0.6, 1.0],
                time_decode_requests=[1.2, 0.9, 1.4],
                
                # Metadata
                num_prompt_tokens_requests=[64, 128, 96],
                num_generation_tokens_requests=[32, 48, 40],
                n_requests=[1, 1, 1],
                max_num_generation_tokens_requests=[100, 150, 120],
                max_tokens_requests=[200, 300, 250],
                finished_reason_requests=["stop", "length", "stop"],
                waiting_lora_adapters=[],
                running_lora_adapters=[],
                max_lora="0"
            )
            
            return mock_stats
            
        except Exception as e:
            logger.debug(f"Engine stats collection failed: {e}")
            return None
    
    def collect_all_metrics(self) -> CollectedMetrics:
        """Collect metrics from all registered sources."""
        current_time = time.time()
        
        # Collect engine statistics
        engine_stats = self.collect_engine_stats()
        
        # Collect GPU metrics
        gpu_metrics = self.collect_gpu_metrics()
        
        # Collect Echo component metrics
        echo_metrics = {}
        if self.enable_echo_metrics:
            for component, collector in self.custom_collectors.items():
                try:
                    metrics = collector()
                    if component.startswith('aar'):
                        echo_metrics['aar'] = metrics
                    elif component.startswith('dtesn'):
                        echo_metrics['dtesn'] = metrics
                    elif component.startswith('echo_self'):
                        echo_metrics['echo_self'] = metrics
                    else:
                        echo_metrics[component] = metrics
                        
                except Exception as e:
                    logger.debug(f"Failed to collect {component} metrics: {e}")
        
        # Collect from registered metrics sources
        custom_metrics = {}
        for name, source in self.metrics_sources.items():
            try:
                custom_metrics[name] = source.collect_metrics()
            except Exception as e:
                logger.debug(f"Failed to collect metrics from {name}: {e}")
        
        # Create comprehensive metrics collection
        collected = CollectedMetrics(
            timestamp=current_time,
            engine_stats=engine_stats,
            gpu_utilization=gpu_metrics.get('utilization', []),
            gpu_memory_usage=gpu_metrics.get('memory_usage', []),
            gpu_temperature=gpu_metrics.get('temperature', []),
            aar_metrics=echo_metrics.get('aar', {}),
            dtesn_metrics=echo_metrics.get('dtesn', {}),
            echo_self_metrics=echo_metrics.get('echo_self', {}),
            custom_metrics=custom_metrics
        )
        
        self.last_collection = collected
        return collected
    
    def start_collection(self):
        """Start continuous metrics collection."""
        if self.is_collecting:
            logger.warning("Metrics collection already running")
            return
        
        self.is_collecting = True
        self.collection_thread = threading.Thread(
            target=self._collection_loop,
            name="MetricsCollector",
            daemon=True
        )
        self.collection_thread.start()
        logger.info("Metrics collection started")
    
    def stop_collection(self):
        """Stop continuous metrics collection."""
        if not self.is_collecting:
            return
        
        self.is_collecting = False
        if self.collection_thread and self.collection_thread.is_alive():
            self.collection_thread.join(timeout=5.0)
        logger.info("Metrics collection stopped")
    
    def _collection_loop(self):
        """Main collection loop for continuous metrics gathering."""
        while self.is_collecting:
            try:
                start_time = time.time()
                
                # Collect all metrics
                self.collect_all_metrics()
                
                # Calculate sleep time to maintain collection interval
                elapsed = time.time() - start_time
                sleep_time = max(0, self.collection_interval - elapsed)
                time.sleep(sleep_time)
                
            except Exception as e:
                logger.error(f"Error in metrics collection loop: {e}")
                time.sleep(self.collection_interval)
    
    def register_source(self, name: str, source: MetricsSource):
        """Register a metrics source component."""
        self.metrics_sources[name] = source
        logger.info(f"Metrics source registered: {name}")
    
    def register_collector(self, name: str, collector: Callable[[], Dict[str, Any]]):
        """Register a custom metrics collector function."""
        self.custom_collectors[name] = collector
        logger.info(f"Custom collector registered: {name}")
    
    def unregister_source(self, name: str):
        """Unregister a metrics source."""
        if name in self.metrics_sources:
            del self.metrics_sources[name]
            logger.info(f"Metrics source unregistered: {name}")
    
    def unregister_collector(self, name: str):
        """Unregister a custom collector."""
        if name in self.custom_collectors:
            del self.custom_collectors[name]
            logger.info(f"Custom collector unregistered: {name}")
    
    def get_latest_metrics(self) -> Optional[CollectedMetrics]:
        """Get the latest collected metrics."""
        return self.last_collection
    
    def get_metrics_summary(self) -> Dict[str, Any]:
        """Get a summary of the latest metrics."""
        if not self.last_collection:
            return {"status": "no_data"}
        
        metrics = self.last_collection
        
        summary = {
            "timestamp": metrics.timestamp,
            "collection_status": "active" if self.is_collecting else "stopped",
            "sources": {
                "engine_stats": metrics.engine_stats is not None,
                "gpu_metrics": len(metrics.gpu_utilization or []) > 0,
                "echo_components": {
                    "aar": bool(metrics.aar_metrics),
                    "dtesn": bool(metrics.dtesn_metrics), 
                    "echo_self": bool(metrics.echo_self_metrics),
                },
                "custom_sources": len(self.metrics_sources),
                "custom_collectors": len(self.custom_collectors),
            }
        }
        
        # Add GPU summary if available
        if metrics.gpu_utilization:
            summary["gpu_summary"] = {
                "device_count": len(metrics.gpu_utilization),
                "avg_utilization": sum(metrics.gpu_utilization) / len(metrics.gpu_utilization),
                "avg_memory_usage": sum(metrics.gpu_memory_usage) / len(metrics.gpu_memory_usage),
                "max_temperature": max(metrics.gpu_temperature) if metrics.gpu_temperature else 0,
            }
        
        # Add engine summary if available
        if metrics.engine_stats:
            summary["engine_summary"] = {
                "running_requests": metrics.engine_stats.num_running_sys,
                "waiting_requests": metrics.engine_stats.num_waiting_sys,
                "gpu_cache_usage": metrics.engine_stats.gpu_cache_usage_sys,
                "total_tokens": metrics.engine_stats.num_tokens_iter,
            }
        
        # Add Echo components summary
        if self.enable_echo_metrics:
            summary["echo_summary"] = {}
            
            if metrics.aar_metrics:
                summary["echo_summary"]["aar"] = {
                    "active_agents": metrics.aar_metrics.get("active_agents", 0),
                    "efficiency": metrics.aar_metrics.get("coordination_efficiency", 0.0)
                }
            
            if metrics.dtesn_metrics:
                summary["echo_summary"]["dtesn"] = {
                    "reservoir_states": metrics.dtesn_metrics.get("reservoir_states", 0),
                    "transition_rate": metrics.dtesn_metrics.get("transition_rate", 0.0)
                }
            
            if metrics.echo_self_metrics:
                summary["echo_summary"]["echo_self"] = {
                    "generation": metrics.echo_self_metrics.get("evolution_generation", 0),
                    "fitness": metrics.echo_self_metrics.get("fitness_score", 0.0)
                }
        
        return summary
    
    def export_metrics_data(self) -> Dict[str, Any]:
        """Export collected metrics data for external use."""
        if not self.last_collection:
            return {}
        
        metrics = self.last_collection
        
        return {
            "timestamp": metrics.timestamp,
            "engine_stats": {
                "running_requests": metrics.engine_stats.num_running_sys if metrics.engine_stats else 0,
                "waiting_requests": metrics.engine_stats.num_waiting_sys if metrics.engine_stats else 0,
                "gpu_cache_usage": metrics.engine_stats.gpu_cache_usage_sys if metrics.engine_stats else 0.0,
                "cpu_cache_usage": metrics.engine_stats.cpu_cache_usage_sys if metrics.engine_stats else 0.0,
                "total_tokens": metrics.engine_stats.num_tokens_iter if metrics.engine_stats else 0,
            },
            "gpu_metrics": {
                "utilization": metrics.gpu_utilization or [],
                "memory_usage": metrics.gpu_memory_usage or [],
                "temperature": metrics.gpu_temperature or [],
            },
            "echo_metrics": {
                "aar": metrics.aar_metrics or {},
                "dtesn": metrics.dtesn_metrics or {},
                "echo_self": metrics.echo_self_metrics or {},
            },
            "custom_metrics": metrics.custom_metrics or {},
        }


# Factory function for easy integration
def create_metrics_collector(
    aphrodite_config: Optional[AphroditeConfig] = None,
    **kwargs
) -> AphroditeMetricsCollector:
    """Create an AphroditeMetricsCollector with default configuration."""
    return AphroditeMetricsCollector(aphrodite_config=aphrodite_config, **kwargs)