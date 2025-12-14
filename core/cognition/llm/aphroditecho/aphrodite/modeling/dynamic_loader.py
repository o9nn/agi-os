"""
Dynamic Model Loader for Aphrodite Engine Integration.

Provides dynamic model loading, unloading, and real-time model switching 
capabilities for seamless integration with existing Aphrodite infrastructure.
"""

import threading
import time
from typing import Dict, Any, Optional
from dataclasses import dataclass

import torch
import torch.nn as nn
from loguru import logger

from aphrodite.common.config import (
    LoadConfig, ModelConfig, AphroditeConfig, DeviceConfig, ParallelConfig
)
from aphrodite.modeling.model_loader.base_loader import BaseModelLoader
from aphrodite.modeling.model_loader import (
    get_model_loader, register_model_loader
)


@dataclass
class ModelResourceUsage:
    """Resource usage tracking for loaded models."""
    memory_mb: float
    gpu_memory_mb: float
    cpu_cores: float
    load_time_ms: float
    last_access_time: float
    request_count: int
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            'memory_mb': self.memory_mb,
            'gpu_memory_mb': self.gpu_memory_mb,
            'cpu_cores': self.cpu_cores,
            'load_time_ms': self.load_time_ms,
            'last_access_time': self.last_access_time,
            'request_count': self.request_count
        }


@dataclass
class LoadedModelInfo:
    """Information about a loaded model."""
    model_name: str
    model: nn.Module
    config: ModelConfig
    aphrodite_config: AphroditeConfig
    loader: BaseModelLoader
    load_time: float
    resource_usage: ModelResourceUsage
    is_active: bool = True
    
    def update_access(self):
        """Update last access time and increment request count."""
        self.resource_usage.last_access_time = time.time()
        self.resource_usage.request_count += 1


class DynamicModelLoader:
    """
    Dynamic model loader with support for multiple models and real-time
    switching.
    
    Features:
    - Dynamic loading and unloading of models
    - Resource management and tracking
    - Model eviction policies
    - Real-time model switching
    - Integration with existing Aphrodite infrastructure
    """
    
    def __init__(
        self,
        max_models: int = 5,
        memory_limit_gb: float = 16.0,
        eviction_policy: str = "lru"  # "lru", "fifo", "memory_pressure"
    ):
        self.max_models = max_models
        self.memory_limit_bytes = int(memory_limit_gb * 1024 * 1024 * 1024)
        self.eviction_policy = eviction_policy
        
        # Thread-safe model storage
        self._loaded_models: Dict[str, LoadedModelInfo] = {}
        self._model_lock = threading.RLock()
        
        # Resource tracking
        self._total_memory_used = 0
        self._resource_monitor = ResourceMonitor()
        
        # Request routing
        self._active_model = None
        self._request_router = RequestRouter()
        
        logger.info(f"DynamicModelLoader initialized: max_models={max_models}, "
                   f"memory_limit_gb={memory_limit_gb}, "
                   f"eviction_policy={eviction_policy}")
    
    async def load_model(
        self,
        model_name: str,
        model_config: ModelConfig,
        aphrodite_config: Optional[AphroditeConfig] = None,
        force_reload: bool = False
    ) -> bool:
        """
        Load a model dynamically with resource management.
        
        Args:
            model_name: Unique identifier for the model
            model_config: Model configuration
            aphrodite_config: Aphrodite engine configuration
            force_reload: Force reload even if model exists
            
        Returns:
            bool: True if model loaded successfully
        """
        with self._model_lock:
            # Check if model already loaded
            if model_name in self._loaded_models and not force_reload:
                logger.info(f"Model {model_name} already loaded")
                self._loaded_models[model_name].update_access()
                return True
            
            # Check resource constraints
            if not await self._check_resource_constraints():
                logger.warning("Resource constraints not met, attempting eviction")
                await self._evict_models_if_needed()
            
            try:
                start_time = time.time()
                
                # Create default Aphrodite config if not provided
                if aphrodite_config is None:
                    aphrodite_config = self._create_default_aphrodite_config(
                        model_config
                    )
                
                # Get appropriate model loader
                loader = get_model_loader(aphrodite_config.load_config)
                
                # Download model if needed
                loader.download_model(model_config)
                
                # Load model
                model = loader.load_model(aphrodite_config, model_config)
                
                load_time = (time.time() - start_time) * 1000  # Convert to ms
                
                # Calculate resource usage
                resource_usage = self._calculate_resource_usage(model, load_time)
                
                # Store loaded model info
                model_info = LoadedModelInfo(
                    model_name=model_name,
                    model=model,
                    config=model_config,
                    aphrodite_config=aphrodite_config,
                    loader=loader,
                    load_time=time.time(),
                    resource_usage=resource_usage
                )
                
                self._loaded_models[model_name] = model_info
                self._total_memory_used += resource_usage.memory_mb * 1024 * 1024
                
                logger.info(
                    f"Model {model_name} loaded successfully in "
                    f"{load_time:.1f}ms, using "
                    f"{resource_usage.memory_mb:.1f}MB memory"
                )
                
                return True
                
            except Exception as e:
                logger.error(f"Failed to load model {model_name}: {e}")
                return False
    
    async def unload_model(self, model_name: str) -> bool:
        """
        Unload a model and free resources.
        
        Args:
            model_name: Name of model to unload
            
        Returns:
            bool: True if model unloaded successfully
        """
        with self._model_lock:
            if model_name not in self._loaded_models:
                logger.warning(f"Model {model_name} not found for unloading")
                return False
            
            try:
                model_info = self._loaded_models[model_name]
                
                # Mark as inactive first
                model_info.is_active = False
                
                # Free memory
                del model_info.model
                torch.cuda.empty_cache() if torch.cuda.is_available() else None
                
                # Update memory tracking
                self._total_memory_used -= model_info.resource_usage.memory_mb * 1024 * 1024
                
                # Remove from loaded models
                del self._loaded_models[model_name]
                
                logger.info(f"Model {model_name} unloaded successfully")
                return True
                
            except Exception as e:
                logger.error(f"Failed to unload model {model_name}: {e}")
                return False
    
    async def switch_active_model(self, model_name: str) -> bool:
        """
        Switch the active model for inference.
        
        Args:
            model_name: Name of model to make active
            
        Returns:
            bool: True if switch successful
        """
        with self._model_lock:
            if model_name not in self._loaded_models:
                logger.error(f"Cannot switch to model {model_name}: not loaded")
                return False
            
            model_info = self._loaded_models[model_name]
            if not model_info.is_active:
                logger.error(f"Cannot switch to model {model_name}: not active")
                return False
            
            # Update access tracking
            model_info.update_access()
            
            # Set as active model
            old_active = self._active_model
            self._active_model = model_name
            
            logger.info(f"Switched active model from {old_active} to {model_name}")
            return True
    
    def get_active_model(self) -> Optional[LoadedModelInfo]:
        """Get the currently active model."""
        with self._model_lock:
            if self._active_model and self._active_model in self._loaded_models:
                return self._loaded_models[self._active_model]
            return None
    
    def get_loaded_models(self) -> Dict[str, Dict[str, Any]]:
        """Get information about all loaded models."""
        with self._model_lock:
            return {
                name: {
                    'model_name': info.model_name,
                    'config': info.config.__dict__,
                    'load_time': info.load_time,
                    'resource_usage': info.resource_usage.to_dict(),
                    'is_active': info.is_active,
                    'is_current_active': name == self._active_model
                }
                for name, info in self._loaded_models.items()
            }
    
    def get_resource_usage(self) -> Dict[str, Any]:
        """Get current resource usage statistics."""
        with self._model_lock:
            return {
                'total_models_loaded': len(self._loaded_models),
                'total_memory_used_mb': self._total_memory_used / (1024 * 1024),
                'memory_limit_mb': self.memory_limit_bytes / (1024 * 1024),
                'memory_utilization': self._total_memory_used / self.memory_limit_bytes,
                'active_model': self._active_model,
                'models': {
                    name: info.resource_usage.to_dict()
                    for name, info in self._loaded_models.items()
                }
            }
    
    async def _check_resource_constraints(self) -> bool:
        """Check if current resource usage is within limits."""
        # Check model count limit
        if len(self._loaded_models) >= self.max_models:
            return False
        
        # Check memory limit
        if self._total_memory_used >= self.memory_limit_bytes * 0.9:  # 90% threshold
            return False
        
        return True
    
    async def _evict_models_if_needed(self) -> bool:
        """Evict models based on configured eviction policy."""
        if not self._loaded_models:
            return True
        
        models_to_evict = []
        
        if self.eviction_policy == "lru":
            # Least Recently Used
            sorted_models = sorted(
                self._loaded_models.items(),
                key=lambda x: x[1].resource_usage.last_access_time
            )
            models_to_evict = [name for name, _ in sorted_models[:1]]  # Evict oldest
            
        elif self.eviction_policy == "fifo":
            # First In, First Out
            sorted_models = sorted(
                self._loaded_models.items(),
                key=lambda x: x[1].load_time
            )
            models_to_evict = [name for name, _ in sorted_models[:1]]  # Evict oldest loaded
            
        elif self.eviction_policy == "memory_pressure":
            # Evict largest memory consumers
            sorted_models = sorted(
                self._loaded_models.items(),
                key=lambda x: x[1].resource_usage.memory_mb,
                reverse=True
            )
            models_to_evict = [name for name, _ in sorted_models[:1]]  # Evict largest
        
        # Evict selected models
        evicted_count = 0
        for model_name in models_to_evict:
            if await self.unload_model(model_name):
                evicted_count += 1
        
        logger.info(f"Evicted {evicted_count} models using {self.eviction_policy} policy")
        return evicted_count > 0
    
    def _create_default_aphrodite_config(
        self, model_config: ModelConfig
    ) -> AphroditeConfig:
        """Create default AphroditeConfig if not provided."""
        return AphroditeConfig(
            model_config=model_config,
            load_config=LoadConfig(),
            device_config=DeviceConfig(),
            parallel_config=ParallelConfig()
        )
    
    def _calculate_resource_usage(
        self, 
        model: nn.Module, 
        load_time_ms: float
    ) -> ModelResourceUsage:
        """Calculate resource usage for a loaded model."""
        # Calculate model memory usage
        memory_mb = 0.0
        gpu_memory_mb = 0.0
        
        try:
            # Calculate parameter memory
            param_bytes = sum(
                p.numel() * p.element_size() for p in model.parameters()
            )
            memory_mb = param_bytes / (1024 * 1024)
            
            # Check GPU memory if available
            if torch.cuda.is_available():
                gpu_memory_mb = torch.cuda.memory_allocated() / (1024 * 1024)
                
        except Exception as e:
            logger.warning(f"Failed to calculate model memory usage: {e}")
        
        return ModelResourceUsage(
            memory_mb=memory_mb,
            gpu_memory_mb=gpu_memory_mb,
            cpu_cores=1.0,  # Default estimate
            load_time_ms=load_time_ms,
            last_access_time=time.time(),
            request_count=0
        )


class ResourceMonitor:
    """Monitor system resource usage."""
    
    def __init__(self):
        self._monitoring = False
    
    def start_monitoring(self):
        """Start resource monitoring."""
        self._monitoring = True
        logger.info("Resource monitoring started")
    
    def stop_monitoring(self):
        """Stop resource monitoring."""
        self._monitoring = False
        logger.info("Resource monitoring stopped")


class RequestRouter:
    """Route requests to appropriate models."""
    
    def __init__(self):
        self._routing_rules = {}
    
    def add_routing_rule(self, pattern: str, model_name: str):
        """Add a routing rule for requests."""
        self._routing_rules[pattern] = model_name
        logger.info(f"Added routing rule: {pattern} -> {model_name}")
    
    def route_request(self, request_data: Dict[str, Any]) -> Optional[str]:
        """Route a request to appropriate model."""
        # Simple routing based on request patterns
        # This can be extended with more sophisticated routing logic
        return None


# Register the dynamic loader with Aphrodite's model loader registry
@register_model_loader("dynamic")
class AphroditeDynamicModelLoader(BaseModelLoader):
    """Aphrodite-compatible dynamic model loader."""
    
    def __init__(self, load_config: LoadConfig):
        super().__init__(load_config)
        self._dynamic_loader = DynamicModelLoader()
    
    def download_model(self, model_config: ModelConfig) -> None:
        """Download model using default loader."""
        from aphrodite.modeling.model_loader.default_loader import DefaultModelLoader
        default_loader = DefaultModelLoader(self.load_config)
        default_loader.download_model(model_config)
    
    def load_weights(self, model: nn.Module, model_config: ModelConfig) -> None:
        """Load weights using default loader."""
        from aphrodite.modeling.model_loader.default_loader import DefaultModelLoader
        default_loader = DefaultModelLoader(self.load_config)
        default_loader.load_weights(model, model_config)
    
    def load_model(
        self, aphrodite_config: AphroditeConfig, model_config: ModelConfig
    ) -> nn.Module:
        """Load model through dynamic loader."""
        # This integrates with the dynamic loading system
        model_name = f"{model_config.model}_{id(model_config)}"
        
        # Use asyncio.run for now - in production this would be better handled
        # by the calling context being async
        import asyncio
        try:
            loop = asyncio.get_event_loop()
        except RuntimeError:
            loop = asyncio.new_event_loop()
            asyncio.set_event_loop(loop)
        
        success = loop.run_until_complete(
            self._dynamic_loader.load_model(
                model_name, model_config, aphrodite_config
            )
        )
        
        if success:
            model_info = self._dynamic_loader.get_loaded_models()[model_name]
            return model_info['model']
        else:
            # Fallback to standard loading
            return super().load_model(aphrodite_config, model_config)


__all__ = [
    "DynamicModelLoader",
    "ModelResourceUsage", 
    "LoadedModelInfo",
    "AphroditeDynamicModelLoader",
    "ResourceMonitor",
    "RequestRouter"
]