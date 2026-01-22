import asyncio
import logging
import time
from typing import Dict, Any, List, Optional, Callable, TYPE_CHECKING
from dataclasses import dataclass
import threading
import weakref
try:
    from ..core.adaptive_architecture import AdaptiveArchitectureFramework, PerformanceMetrics, ArchitectureMutation
    from ..core.interfaces import Individual
    from .aphrodite_bridge import AphroditeBridge
except ImportError:
    try:
        from core.adaptive_architecture import AdaptiveArchitectureFramework, PerformanceMetrics, ArchitectureMutation
        from core.interfaces import Individual
        from integration.aphrodite_bridge import AphroditeBridge
    except ImportError:
        pass
if TYPE_CHECKING:
    pass
logger = logging.getLogger(__name__)
@dataclass
class InferenceMetrics:
    request_latency_ms: float
    tokens_generated: int
    generation_time_ms: float
    memory_used_mb: float
    batch_size: int
    sequence_length: int
    model_load_time_ms: float = 0.0
    def to_performance_metrics(self) -> PerformanceMetrics:
        throughput = self.tokens_generated / (self.generation_time_ms / 1000.0) if self.generation_time_ms > 0 else 0.0
        return PerformanceMetrics(latency_ms=self.request_latency_ms, throughput_tokens_per_sec=throughput, memory_usage_mb=self.memory_used_mb, accuracy_score=0.8, inference_time_ms=self.generation_time_ms)
class ModelTopologyAdapter:
    def __init__(self):
        self.supported_modifications = {'layer_scaling': self._scale_layer_dimensions, 'attention_heads': self._adjust_attention_heads, 'hidden_size': self._adjust_hidden_size, 'intermediate_size': self._adjust_intermediate_size, 'layer_removal': self._remove_layers, 'layer_addition': self._add_layers}
        self.modification_history: List[Dict[str, Any]] = []
        self._lock = threading.RLock()
    def can_modify_architecture(self, model_config: Dict[str, Any]) -> bool:
        required_params = ['hidden_size', 'num_attention_heads', 'num_hidden_layers']
        return all((param in model_config for param in required_params))
    def apply_mutation_to_model_config(self, model_config: Dict[str, Any], mutation: ArchitectureMutation) -> Dict[str, Any]:
        with self._lock:
            modified_config = model_config.copy()
            try:
                if mutation.mutation_type in self.supported_modifications:
                    modifier_func = self.supported_modifications[mutation.mutation_type]
                    modified_config = modifier_func(modified_config, mutation)
                    modification_record = {'timestamp': time.time(), 'mutation_type': mutation.mutation_type, 'original_config': model_config, 'modified_config': modified_config, 'parameters': mutation.parameters, 'expected_impact': mutation.expected_impact}
                    self.modification_history.append(modification_record)
                    logger.info(f'Applied {mutation.mutation_type} mutation to model config')
                else:
                    logger.warning(f'Unsupported mutation type: {mutation.mutation_type}')
            except Exception as e:
                logger.error(f'Failed to apply mutation {mutation.mutation_type}: {e}')
                return model_config
            return modified_config
    def _scale_layer_dimensions(self, config: Dict[str, Any], mutation: ArchitectureMutation) -> Dict[str, Any]:
        scale_factor = mutation.parameters.get('scale_factor', 1.0)
        if 'hidden_size' in config:
            original_size = config['hidden_size']
            new_size = max(64, int(original_size * scale_factor))
            if 'num_attention_heads' in config:
                heads = config['num_attention_heads']
                new_size = new_size // heads * heads
            config['hidden_size'] = new_size
        if 'intermediate_size' in config and scale_factor != 1.0:
            original_size = config['intermediate_size']
            config['intermediate_size'] = max(64, int(original_size * scale_factor))
        return config
    def _adjust_attention_heads(self, config: Dict[str, Any], mutation: ArchitectureMutation) -> Dict[str, Any]:
        new_heads = mutation.parameters.get('num_heads')
        if new_heads and 'num_attention_heads' in config:
            config['num_attention_heads']
            if 'hidden_size' in config:
                hidden_size = config['hidden_size']
                head_dim = hidden_size // new_heads
                if head_dim >= 32:
                    config['num_attention_heads'] = new_heads
                else:
                    logger.warning(f'Cannot set {new_heads} heads with hidden_size {hidden_size}')
        return config
    def _adjust_hidden_size(self, config: Dict[str, Any], mutation: ArchitectureMutation) -> Dict[str, Any]:
        new_size = mutation.parameters.get('hidden_size')
        if new_size and 'hidden_size' in config:
            if 'num_attention_heads' in config:
                heads = config['num_attention_heads']
                new_size = new_size // heads * heads
            config['hidden_size'] = max(64, new_size)
            if 'intermediate_size' in config:
                ratio = new_size / config['hidden_size']
                config['intermediate_size'] = max(64, int(config['intermediate_size'] * ratio))
        return config
    def _adjust_intermediate_size(self, config: Dict[str, Any], mutation: ArchitectureMutation) -> Dict[str, Any]:
        new_size = mutation.parameters.get('intermediate_size')
        if new_size and 'intermediate_size' in config:
            config['intermediate_size'] = max(64, new_size)
        return config
    def _remove_layers(self, config: Dict[str, Any], mutation: ArchitectureMutation) -> Dict[str, Any]:
        layers_to_remove = mutation.parameters.get('num_layers', 1)
        if 'num_hidden_layers' in config:
            current_layers = config['num_hidden_layers']
            new_layers = max(1, current_layers - layers_to_remove)
            config['num_hidden_layers'] = new_layers
        return config
    def _add_layers(self, config: Dict[str, Any], mutation: ArchitectureMutation) -> Dict[str, Any]:
        layers_to_add = mutation.parameters.get('num_layers', 1)
        if 'num_hidden_layers' in config:
            current_layers = config['num_hidden_layers']
            new_layers = min(48, current_layers + layers_to_add)
            config['num_hidden_layers'] = new_layers
        return config
class InferenceHookManager:
    def __init__(self, adaptive_framework: AdaptiveArchitectureFramework):
        self.adaptive_framework = adaptive_framework
        self.active_hooks: Dict[str, Callable] = {}
        self.metrics_collectors: List[Callable] = []
        self._hook_lock = threading.RLock()
        self.inference_count = 0
        self.total_inference_time = 0.0
        self.last_performance_check = time.time()
    def register_inference_hook(self, hook_name: str, hook_func: Callable) -> None:
        with self._hook_lock:
            self.active_hooks[hook_name] = hook_func
            logger.debug(f'Registered inference hook: {hook_name}')
    def unregister_inference_hook(self, hook_name: str) -> None:
        with self._hook_lock:
            if hook_name in self.active_hooks:
                del self.active_hooks[hook_name]
                logger.debug(f'Unregistered inference hook: {hook_name}')
    def add_metrics_collector(self, collector_func: Callable[[InferenceMetrics], None]) -> None:
        self.metrics_collectors.append(collector_func)
    async def pre_inference_hook(self, request_data: Dict[str, Any]) -> Dict[str, Any]:
        start_time = time.time()
        with self._hook_lock:
            for hook_name, hook_func in self.active_hooks.items():
                try:
                    if asyncio.iscoroutinefunction(hook_func):
                        request_data = await hook_func(request_data) or request_data
                    else:
                        request_data = hook_func(request_data) or request_data
                except Exception as e:
                    logger.error(f'Error in pre-inference hook {hook_name}: {e}')
        request_data['_hook_start_time'] = start_time
        return request_data
    async def post_inference_hook(self, request_data: Dict[str, Any], inference_result: Dict[str, Any]) -> Dict[str, Any]:
        end_time = time.time()
        start_time = request_data.get('_hook_start_time', end_time)
        inference_time = (end_time - start_time) * 1000
        metrics = InferenceMetrics(request_latency_ms=inference_time, tokens_generated=inference_result.get('tokens_generated', 0), generation_time_ms=inference_result.get('generation_time_ms', inference_time), memory_used_mb=inference_result.get('memory_usage_mb', 0), batch_size=request_data.get('batch_size', 1), sequence_length=request_data.get('sequence_length', 0))
        self.inference_count += 1
        self.total_inference_time += inference_time
        performance_metrics = metrics.to_performance_metrics()
        self.adaptive_framework.add_performance_metrics(performance_metrics)
        for collector in self.metrics_collectors:
            try:
                collector(metrics)
            except Exception as e:
                logger.error(f'Error in metrics collector: {e}')
        with self._hook_lock:
            for hook_name, hook_func in self.active_hooks.items():
                try:
                    if asyncio.iscoroutinefunction(hook_func):
                        inference_result = await hook_func(inference_result) or inference_result
                    else:
                        inference_result = hook_func(inference_result) or inference_result
                except Exception as e:
                    logger.error(f'Error in post-inference hook {hook_name}: {e}')
        if time.time() - self.last_performance_check > 30:
            await self._periodic_performance_check()
            self.last_performance_check = time.time()
        return inference_result
    async def _periodic_performance_check(self) -> None:
        if self.inference_count == 0:
            return
        avg_inference_time = self.total_inference_time / self.inference_count
        logger.debug(f'Performance check: {self.inference_count} inferences, avg time: {avg_inference_time:.2f}ms')
        self.inference_count = 0
        self.total_inference_time = 0.0
class AphroditeAdaptiveIntegration:
    def __init__(self, adaptive_framework: AdaptiveArchitectureFramework, aphrodite_bridge: Optional[AphroditeBridge]=None):
        self.adaptive_framework = adaptive_framework
        self.aphrodite_bridge = aphrodite_bridge or AphroditeBridge()
        self.topology_adapter = ModelTopologyAdapter()
        self.hook_manager = InferenceHookManager(adaptive_framework)
        self.current_model_config: Optional[Dict[str, Any]] = None
        self.original_model_config: Optional[Dict[str, Any]] = None
        self.model_modification_count = 0
        self.is_integrated = False
        self._integration_lock = threading.RLock()
        self._model_runner_refs: List[weakref.ReferenceType] = []
        logger.info('AphroditeAdaptiveIntegration initialized')
    async def integrate_with_aphrodite(self, model_config: Dict[str, Any]) -> bool:
        with self._integration_lock:
            if self.is_integrated:
                logger.warning('Already integrated with Aphrodite Engine')
                return True
            try:
                self.original_model_config = model_config.copy()
                self.current_model_config = model_config.copy()
                if not self.topology_adapter.can_modify_architecture(model_config):
                    logger.warning('Model architecture cannot be modified - limited functionality')
                if not self.aphrodite_bridge.is_initialized():
                    model_name = model_config.get('model_name', 'default')
                    bridge_initialized = self.aphrodite_bridge.initialize(model_name)
                    if not bridge_initialized:
                        logger.error('Aphrodite bridge initialization failed - cannot proceed with real integration')
                        raise RuntimeError('Failed to initialize real Aphrodite Engine components')
                await self._setup_inference_hooks()
                await self.adaptive_framework.start_adaptive_monitoring()
                self.is_integrated = True
                logger.info('Successfully integrated with Aphrodite Engine')
                return True
            except Exception as e:
                logger.error(f'Failed to integrate with Aphrodite Engine: {e}')
                return False
    async def _setup_inference_hooks(self) -> None:
        async def performance_monitoring_hook(data: Dict[str, Any]) -> Dict[str, Any]:
            data['performance_start_time'] = time.time()
            return data
        def metrics_collector(metrics: InferenceMetrics) -> None:
            logger.debug(f'Inference metrics: latency={metrics.request_latency_ms:.2f}ms, throughput={metrics.tokens_generated / (metrics.generation_time_ms / 1000.0):.1f} tokens/s')
        self.hook_manager.register_inference_hook('performance_monitor', performance_monitoring_hook)
        self.hook_manager.add_metrics_collector(metrics_collector)
        logger.debug('Inference hooks set up')
    async def apply_architecture_adaptation(self, mutation: ArchitectureMutation) -> bool:
        with self._integration_lock:
            if not self.is_integrated:
                logger.error('Not integrated with Aphrodite Engine')
                return False
            if self.current_model_config is None:
                logger.error('No current model configuration available')
                return False
            try:
                new_config = self.topology_adapter.apply_mutation_to_model_config(self.current_model_config, mutation)
                if new_config == self.current_model_config:
                    logger.warning('Mutation resulted in no configuration change')
                    return False
                self.current_model_config = new_config
                self.model_modification_count += 1
                logger.info(f'Architecture adaptation applied (#{self.model_modification_count}): {mutation.mutation_type}')
                return True
            except Exception as e:
                logger.error(f'Failed to apply architecture adaptation: {e}')
                return False
    def get_model_configuration_status(self) -> Dict[str, Any]:
        with self._integration_lock:
            return {'is_integrated': self.is_integrated, 'modification_count': self.model_modification_count, 'current_config': self.current_model_config, 'original_config': self.original_model_config, 'can_modify_architecture': self.topology_adapter.can_modify_architecture(self.current_model_config) if self.current_model_config else False, 'adaptation_status': self.adaptive_framework.get_adaptation_status()}
    def get_modification_history(self) -> List[Dict[str, Any]]:
        return self.topology_adapter.modification_history.copy()
    async def hook_inference_request(self, request_data: Dict[str, Any]) -> Dict[str, Any]:
        return await self.hook_manager.pre_inference_hook(request_data)
    async def hook_inference_response(self, request_data: Dict[str, Any], response_data: Dict[str, Any]) -> Dict[str, Any]:
        return await self.hook_manager.post_inference_hook(request_data, response_data)
    async def shutdown_integration(self) -> None:
        with self._integration_lock:
            if not self.is_integrated:
                return
            try:
                await self.adaptive_framework.stop_adaptive_monitoring()
                self.hook_manager.active_hooks.clear()
                self.hook_manager.metrics_collectors.clear()
                self._model_runner_refs.clear()
                self.is_integrated = False
                logger.info('Aphrodite integration shutdown completed')
            except Exception as e:
                logger.error(f'Error during integration shutdown: {e}')
class AdaptiveModelLoader:
    def __init__(self, integration: AphroditeAdaptiveIntegration):
        self.integration = integration
        self.loaded_models: Dict[str, Dict[str, Any]] = {}
        self._loader_lock = threading.RLock()
    async def load_adaptive_model(self, model_name: str, model_config: Dict[str, Any], enable_adaptation: bool=True) -> bool:
        with self._loader_lock:
            try:
                self.loaded_models[model_name] = {'config': model_config.copy(), 'loaded_at': time.time(), 'adaptation_enabled': enable_adaptation, 'modification_count': 0}
                if enable_adaptation:
                    integration_success = await self.integration.integrate_with_aphrodite(model_config)
                    if not integration_success:
                        logger.warning(f'Adaptation integration failed for model {model_name}')
                logger.info(f'Adaptive model loaded: {model_name}')
                return True
            except Exception as e:
                logger.error(f'Failed to load adaptive model {model_name}: {e}')
                return False
    async def reload_model_with_adaptation(self, model_name: str, mutation: ArchitectureMutation) -> bool:
        with self._loader_lock:
            if model_name not in self.loaded_models:
                logger.error(f'Model {model_name} not found')
                return False
            try:
                model_info = self.loaded_models[model_name]
                current_config = model_info['config']
                adapted_config = self.integration.topology_adapter.apply_mutation_to_model_config(current_config, mutation)
                model_info['config'] = adapted_config
                model_info['modification_count'] += 1
                success = await self.integration.apply_architecture_adaptation(mutation)
                if success:
                    logger.info(f'Model {model_name} reloaded with adaptation')
                else:
                    logger.warning(f'Adaptation failed for model {model_name}')
                return success
            except Exception as e:
                logger.error(f'Failed to reload model {model_name} with adaptation: {e}')
                return False
    def get_loaded_models(self) -> Dict[str, Dict[str, Any]]:
        with self._loader_lock:
            return {name: info.copy() for name, info in self.loaded_models.items()}
    async def unload_model(self, model_name: str) -> bool:
        with self._loader_lock:
            if model_name not in self.loaded_models:
                logger.warning(f'Model {model_name} not found')
                return False
            try:
                del self.loaded_models[model_name]
                logger.info(f'Model {model_name} unloaded')
                return True
            except Exception as e:
                logger.error(f'Failed to unload model {model_name}: {e}')
                return False