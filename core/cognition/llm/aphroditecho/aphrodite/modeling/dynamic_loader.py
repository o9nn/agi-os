import threading
import time
from typing import Dict, Any, Optional
from dataclasses import dataclass
import torch
import torch.nn as nn
from loguru import logger
from aphrodite.common.config import LoadConfig, ModelConfig, AphroditeConfig, DeviceConfig, ParallelConfig
from aphrodite.modeling.model_loader.base_loader import BaseModelLoader
from aphrodite.modeling.model_loader import get_model_loader, register_model_loader
@dataclass
class ModelResourceUsage:
    memory_mb: float
    gpu_memory_mb: float
    cpu_cores: float
    load_time_ms: float
    last_access_time: float
    request_count: int
    def to_dict(self) -> Dict[str, Any]:
        return {'memory_mb': self.memory_mb, 'gpu_memory_mb': self.gpu_memory_mb, 'cpu_cores': self.cpu_cores, 'load_time_ms': self.load_time_ms, 'last_access_time': self.last_access_time, 'request_count': self.request_count}
@dataclass
class LoadedModelInfo:
    model_name: str
    model: nn.Module
    config: ModelConfig
    aphrodite_config: AphroditeConfig
    loader: BaseModelLoader
    load_time: float
    resource_usage: ModelResourceUsage
    is_active: bool = True
    def update_access(self):
        self.resource_usage.last_access_time = time.time()
        self.resource_usage.request_count += 1
class DynamicModelLoader:
    def __init__(self, max_models: int=5, memory_limit_gb: float=16.0, eviction_policy: str='lru'):
        self.max_models = max_models
        self.memory_limit_bytes = int(memory_limit_gb * 1024 * 1024 * 1024)
        self.eviction_policy = eviction_policy
        self._loaded_models: Dict[str, LoadedModelInfo] = {}
        self._model_lock = threading.RLock()
        self._total_memory_used = 0
        self._resource_monitor = ResourceMonitor()
        self._active_model = None
        self._request_router = RequestRouter()
        logger.info(f'DynamicModelLoader initialized: max_models={max_models}, memory_limit_gb={memory_limit_gb}, eviction_policy={eviction_policy}')
    async def load_model(self, model_name: str, model_config: ModelConfig, aphrodite_config: Optional[AphroditeConfig]=None, force_reload: bool=False) -> bool:
        with self._model_lock:
            if model_name in self._loaded_models and (not force_reload):
                logger.info(f'Model {model_name} already loaded')
                self._loaded_models[model_name].update_access()
                return True
            if not await self._check_resource_constraints():
                logger.warning('Resource constraints not met, attempting eviction')
                await self._evict_models_if_needed()
            try:
                start_time = time.time()
                if aphrodite_config is None:
                    aphrodite_config = self._create_default_aphrodite_config(model_config)
                loader = get_model_loader(aphrodite_config.load_config)
                loader.download_model(model_config)
                model = loader.load_model(aphrodite_config, model_config)
                load_time = (time.time() - start_time) * 1000
                resource_usage = self._calculate_resource_usage(model, load_time)
                model_info = LoadedModelInfo(model_name=model_name, model=model, config=model_config, aphrodite_config=aphrodite_config, loader=loader, load_time=time.time(), resource_usage=resource_usage)
                self._loaded_models[model_name] = model_info
                self._total_memory_used += resource_usage.memory_mb * 1024 * 1024
                logger.info(f'Model {model_name} loaded successfully in {load_time:.1f}ms, using {resource_usage.memory_mb:.1f}MB memory')
                return True
            except Exception as e:
                logger.error(f'Failed to load model {model_name}: {e}')
                return False
    async def unload_model(self, model_name: str) -> bool:
        with self._model_lock:
            if model_name not in self._loaded_models:
                logger.warning(f'Model {model_name} not found for unloading')
                return False
            try:
                model_info = self._loaded_models[model_name]
                model_info.is_active = False
                del model_info.model
                torch.cuda.empty_cache() if torch.cuda.is_available() else None
                self._total_memory_used -= model_info.resource_usage.memory_mb * 1024 * 1024
                del self._loaded_models[model_name]
                logger.info(f'Model {model_name} unloaded successfully')
                return True
            except Exception as e:
                logger.error(f'Failed to unload model {model_name}: {e}')
                return False
    async def switch_active_model(self, model_name: str) -> bool:
        with self._model_lock:
            if model_name not in self._loaded_models:
                logger.error(f'Cannot switch to model {model_name}: not loaded')
                return False
            model_info = self._loaded_models[model_name]
            if not model_info.is_active:
                logger.error(f'Cannot switch to model {model_name}: not active')
                return False
            model_info.update_access()
            old_active = self._active_model
            self._active_model = model_name
            logger.info(f'Switched active model from {old_active} to {model_name}')
            return True
    def get_active_model(self) -> Optional[LoadedModelInfo]:
        with self._model_lock:
            if self._active_model and self._active_model in self._loaded_models:
                return self._loaded_models[self._active_model]
            return None
    def get_loaded_models(self) -> Dict[str, Dict[str, Any]]:
        with self._model_lock:
            return {name: {'model_name': info.model_name, 'config': info.config.__dict__, 'load_time': info.load_time, 'resource_usage': info.resource_usage.to_dict(), 'is_active': info.is_active, 'is_current_active': name == self._active_model} for name, info in self._loaded_models.items()}
    def get_resource_usage(self) -> Dict[str, Any]:
        with self._model_lock:
            return {'total_models_loaded': len(self._loaded_models), 'total_memory_used_mb': self._total_memory_used / (1024 * 1024), 'memory_limit_mb': self.memory_limit_bytes / (1024 * 1024), 'memory_utilization': self._total_memory_used / self.memory_limit_bytes, 'active_model': self._active_model, 'models': {name: info.resource_usage.to_dict() for name, info in self._loaded_models.items()}}
    async def _check_resource_constraints(self) -> bool:
        if len(self._loaded_models) >= self.max_models:
            return False
        if self._total_memory_used >= self.memory_limit_bytes * 0.9:
            return False
        return True
    async def _evict_models_if_needed(self) -> bool:
        if not self._loaded_models:
            return True
        models_to_evict = []
        if self.eviction_policy == 'lru':
            sorted_models = sorted(self._loaded_models.items(), key=lambda x: x[1].resource_usage.last_access_time)
            models_to_evict = [name for name, _ in sorted_models[:1]]
        elif self.eviction_policy == 'fifo':
            sorted_models = sorted(self._loaded_models.items(), key=lambda x: x[1].load_time)
            models_to_evict = [name for name, _ in sorted_models[:1]]
        elif self.eviction_policy == 'memory_pressure':
            sorted_models = sorted(self._loaded_models.items(), key=lambda x: x[1].resource_usage.memory_mb, reverse=True)
            models_to_evict = [name for name, _ in sorted_models[:1]]
        evicted_count = 0
        for model_name in models_to_evict:
            if await self.unload_model(model_name):
                evicted_count += 1
        logger.info(f'Evicted {evicted_count} models using {self.eviction_policy} policy')
        return evicted_count > 0
    def _create_default_aphrodite_config(self, model_config: ModelConfig) -> AphroditeConfig:
        return AphroditeConfig(model_config=model_config, load_config=LoadConfig(), device_config=DeviceConfig(), parallel_config=ParallelConfig())
    def _calculate_resource_usage(self, model: nn.Module, load_time_ms: float) -> ModelResourceUsage:
        memory_mb = 0.0
        gpu_memory_mb = 0.0
        try:
            param_bytes = sum((p.numel() * p.element_size() for p in model.parameters()))
            memory_mb = param_bytes / (1024 * 1024)
            if torch.cuda.is_available():
                gpu_memory_mb = torch.cuda.memory_allocated() / (1024 * 1024)
        except Exception as e:
            logger.warning(f'Failed to calculate model memory usage: {e}')
        return ModelResourceUsage(memory_mb=memory_mb, gpu_memory_mb=gpu_memory_mb, cpu_cores=1.0, load_time_ms=load_time_ms, last_access_time=time.time(), request_count=0)
class ResourceMonitor:
    def __init__(self):
        self._monitoring = False
    def start_monitoring(self):
        self._monitoring = True
        logger.info('Resource monitoring started')
    def stop_monitoring(self):
        self._monitoring = False
        logger.info('Resource monitoring stopped')
class RequestRouter:
    def __init__(self):
        self._routing_rules = {}
    def add_routing_rule(self, pattern: str, model_name: str):
        self._routing_rules[pattern] = model_name
        logger.info(f'Added routing rule: {pattern} -> {model_name}')
    def route_request(self, request_data: Dict[str, Any]) -> Optional[str]:
        return None
@register_model_loader('dynamic')
class AphroditeDynamicModelLoader(BaseModelLoader):
    def __init__(self, load_config: LoadConfig):
        super().__init__(load_config)
        self._dynamic_loader = DynamicModelLoader()
    def download_model(self, model_config: ModelConfig) -> None:
        from aphrodite.modeling.model_loader.default_loader import DefaultModelLoader
        default_loader = DefaultModelLoader(self.load_config)
        default_loader.download_model(model_config)
    def load_weights(self, model: nn.Module, model_config: ModelConfig) -> None:
        from aphrodite.modeling.model_loader.default_loader import DefaultModelLoader
        default_loader = DefaultModelLoader(self.load_config)
        default_loader.load_weights(model, model_config)
    def load_model(self, aphrodite_config: AphroditeConfig, model_config: ModelConfig) -> nn.Module:
        model_name = f'{model_config.model}_{id(model_config)}'
        import asyncio
        try:
            loop = asyncio.get_event_loop()
        except RuntimeError:
            loop = asyncio.new_event_loop()
            asyncio.set_event_loop(loop)
        success = loop.run_until_complete(self._dynamic_loader.load_model(model_name, model_config, aphrodite_config))
        if success:
            model_info = self._dynamic_loader.get_loaded_models()[model_name]
            return model_info['model']
        else:
            return super().load_model(aphrodite_config, model_config)
__all__ = ['DynamicModelLoader', 'ModelResourceUsage', 'LoadedModelInfo', 'AphroditeDynamicModelLoader', 'ResourceMonitor', 'RequestRouter']