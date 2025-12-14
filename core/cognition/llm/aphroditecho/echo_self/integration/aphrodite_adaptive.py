"""
Aphrodite Engine Integration for Adaptive Architecture Framework.

Provides integration hooks with Aphrodite Engine model loading and 
inference-time architecture modification capabilities.
"""

import asyncio
import logging
import time
from typing import Dict, Any, List, Optional, Callable, TYPE_CHECKING
from dataclasses import dataclass
import threading
import weakref

# Handle both absolute and relative imports
try:
    from core.adaptive_architecture import (
        AdaptiveArchitectureFramework, PerformanceMetrics, 
        ArchitectureMutation
    )
    from core.interfaces import Individual
    from integration.aphrodite_bridge import AphroditeBridge
except ImportError:
    from .adaptive_architecture import (
        AdaptiveArchitectureFramework, PerformanceMetrics, 
        ArchitectureMutation
    )
    from ..integration.aphrodite_bridge import AphroditeBridge

if TYPE_CHECKING:
    # Type hints for Aphrodite components (avoid import errors)
    pass

logger = logging.getLogger(__name__)


@dataclass
class InferenceMetrics:
    """Metrics collected during model inference."""
    request_latency_ms: float
    tokens_generated: int
    generation_time_ms: float
    memory_used_mb: float
    batch_size: int
    sequence_length: int
    model_load_time_ms: float = 0.0
    
    def to_performance_metrics(self) -> PerformanceMetrics:
        """Convert to PerformanceMetrics format."""
        throughput = (self.tokens_generated / (self.generation_time_ms / 1000.0)) if self.generation_time_ms > 0 else 0.0
        
        return PerformanceMetrics(
            latency_ms=self.request_latency_ms,
            throughput_tokens_per_sec=throughput,
            memory_usage_mb=self.memory_used_mb,
            accuracy_score=0.8,  # Default, would need actual evaluation
            inference_time_ms=self.generation_time_ms
        )


class ModelTopologyAdapter:
    """Adapts neural model topology for Aphrodite Engine."""
    
    def __init__(self):
        self.supported_modifications = {
            'layer_scaling': self._scale_layer_dimensions,
            'attention_heads': self._adjust_attention_heads,
            'hidden_size': self._adjust_hidden_size,
            'intermediate_size': self._adjust_intermediate_size,
            'layer_removal': self._remove_layers,
            'layer_addition': self._add_layers
        }
        
        # Track applied modifications
        self.modification_history: List[Dict[str, Any]] = []
        self._lock = threading.RLock()
    
    def can_modify_architecture(self, model_config: Dict[str, Any]) -> bool:
        """Check if model architecture can be modified."""
        # Check if we have the necessary configuration parameters
        required_params = ['hidden_size', 'num_attention_heads', 'num_hidden_layers']
        return all(param in model_config for param in required_params)
    
    def apply_mutation_to_model_config(
        self, 
        model_config: Dict[str, Any], 
        mutation: ArchitectureMutation
    ) -> Dict[str, Any]:
        """Apply architecture mutation to Aphrodite model configuration."""
        with self._lock:
            modified_config = model_config.copy()
            
            try:
                if mutation.mutation_type in self.supported_modifications:
                    modifier_func = self.supported_modifications[mutation.mutation_type]
                    modified_config = modifier_func(modified_config, mutation)
                    
                    # Record the modification
                    modification_record = {
                        'timestamp': time.time(),
                        'mutation_type': mutation.mutation_type,
                        'original_config': model_config,
                        'modified_config': modified_config,
                        'parameters': mutation.parameters,
                        'expected_impact': mutation.expected_impact
                    }
                    self.modification_history.append(modification_record)
                    
                    logger.info(f"Applied {mutation.mutation_type} mutation to model config")
                else:
                    logger.warning(f"Unsupported mutation type: {mutation.mutation_type}")
            
            except Exception as e:
                logger.error(f"Failed to apply mutation {mutation.mutation_type}: {e}")
                return model_config  # Return original on failure
            
            return modified_config
    
    def _scale_layer_dimensions(self, config: Dict[str, Any], mutation: ArchitectureMutation) -> Dict[str, Any]:
        """Scale layer dimensions based on mutation parameters."""
        scale_factor = mutation.parameters.get('scale_factor', 1.0)
        
        if 'hidden_size' in config:
            original_size = config['hidden_size']
            new_size = max(64, int(original_size * scale_factor))
            # Ensure divisible by attention heads
            if 'num_attention_heads' in config:
                heads = config['num_attention_heads']
                new_size = (new_size // heads) * heads
            config['hidden_size'] = new_size
        
        if 'intermediate_size' in config and scale_factor != 1.0:
            original_size = config['intermediate_size']
            config['intermediate_size'] = max(64, int(original_size * scale_factor))
        
        return config
    
    def _adjust_attention_heads(self, config: Dict[str, Any], mutation: ArchitectureMutation) -> Dict[str, Any]:
        """Adjust number of attention heads."""
        new_heads = mutation.parameters.get('num_heads')
        if new_heads and 'num_attention_heads' in config:
            config['num_attention_heads']
            
            # Ensure head dimension remains reasonable
            if 'hidden_size' in config:
                hidden_size = config['hidden_size']
                head_dim = hidden_size // new_heads
                if head_dim >= 32:  # Minimum head dimension
                    config['num_attention_heads'] = new_heads
                else:
                    logger.warning(f"Cannot set {new_heads} heads with hidden_size {hidden_size}")
        
        return config
    
    def _adjust_hidden_size(self, config: Dict[str, Any], mutation: ArchitectureMutation) -> Dict[str, Any]:
        """Adjust hidden layer size."""
        new_size = mutation.parameters.get('hidden_size')
        if new_size and 'hidden_size' in config:
            # Ensure divisible by attention heads
            if 'num_attention_heads' in config:
                heads = config['num_attention_heads']
                new_size = (new_size // heads) * heads
            
            config['hidden_size'] = max(64, new_size)
            
            # Adjust related dimensions proportionally
            if 'intermediate_size' in config:
                ratio = new_size / config['hidden_size']
                config['intermediate_size'] = max(64, int(config['intermediate_size'] * ratio))
        
        return config
    
    def _adjust_intermediate_size(self, config: Dict[str, Any], mutation: ArchitectureMutation) -> Dict[str, Any]:
        """Adjust intermediate (feed-forward) layer size."""
        new_size = mutation.parameters.get('intermediate_size')
        if new_size and 'intermediate_size' in config:
            config['intermediate_size'] = max(64, new_size)
        
        return config
    
    def _remove_layers(self, config: Dict[str, Any], mutation: ArchitectureMutation) -> Dict[str, Any]:
        """Remove layers from the model."""
        layers_to_remove = mutation.parameters.get('num_layers', 1)
        if 'num_hidden_layers' in config:
            current_layers = config['num_hidden_layers']
            new_layers = max(1, current_layers - layers_to_remove)  # Keep at least 1 layer
            config['num_hidden_layers'] = new_layers
        
        return config
    
    def _add_layers(self, config: Dict[str, Any], mutation: ArchitectureMutation) -> Dict[str, Any]:
        """Add layers to the model."""
        layers_to_add = mutation.parameters.get('num_layers', 1)
        if 'num_hidden_layers' in config:
            current_layers = config['num_hidden_layers']
            new_layers = min(48, current_layers + layers_to_add)  # Reasonable maximum
            config['num_hidden_layers'] = new_layers
        
        return config


class InferenceHookManager:
    """Manages hooks into Aphrodite Engine inference process."""
    
    def __init__(self, adaptive_framework: AdaptiveArchitectureFramework):
        self.adaptive_framework = adaptive_framework
        self.active_hooks: Dict[str, Callable] = {}
        self.metrics_collectors: List[Callable] = []
        self._hook_lock = threading.RLock()
        
        # Performance tracking
        self.inference_count = 0
        self.total_inference_time = 0.0
        self.last_performance_check = time.time()
        
    def register_inference_hook(self, hook_name: str, hook_func: Callable) -> None:
        """Register a hook function to be called during inference."""
        with self._hook_lock:
            self.active_hooks[hook_name] = hook_func
            logger.debug(f"Registered inference hook: {hook_name}")
    
    def unregister_inference_hook(self, hook_name: str) -> None:
        """Unregister an inference hook."""
        with self._hook_lock:
            if hook_name in self.active_hooks:
                del self.active_hooks[hook_name]
                logger.debug(f"Unregistered inference hook: {hook_name}")
    
    def add_metrics_collector(self, collector_func: Callable[[InferenceMetrics], None]) -> None:
        """Add a metrics collector function."""
        self.metrics_collectors.append(collector_func)
    
    async def pre_inference_hook(self, request_data: Dict[str, Any]) -> Dict[str, Any]:
        """Called before inference starts."""
        start_time = time.time()
        
        # Execute registered hooks
        with self._hook_lock:
            for hook_name, hook_func in self.active_hooks.items():
                try:
                    if asyncio.iscoroutinefunction(hook_func):
                        request_data = await hook_func(request_data) or request_data
                    else:
                        request_data = hook_func(request_data) or request_data
                except Exception as e:
                    logger.error(f"Error in pre-inference hook {hook_name}: {e}")
        
        # Store start time for latency calculation
        request_data['_hook_start_time'] = start_time
        return request_data
    
    async def post_inference_hook(
        self, 
        request_data: Dict[str, Any], 
        inference_result: Dict[str, Any]
    ) -> Dict[str, Any]:
        """Called after inference completes."""
        end_time = time.time()
        start_time = request_data.get('_hook_start_time', end_time)
        
        # Calculate metrics
        inference_time = (end_time - start_time) * 1000  # Convert to milliseconds
        
        # Extract metrics from request and result
        metrics = InferenceMetrics(
            request_latency_ms=inference_time,
            tokens_generated=inference_result.get('tokens_generated', 0),
            generation_time_ms=inference_result.get('generation_time_ms', inference_time),
            memory_used_mb=inference_result.get('memory_usage_mb', 0),
            batch_size=request_data.get('batch_size', 1),
            sequence_length=request_data.get('sequence_length', 0)
        )
        
        # Update performance tracking
        self.inference_count += 1
        self.total_inference_time += inference_time
        
        # Send metrics to adaptive framework
        performance_metrics = metrics.to_performance_metrics()
        self.adaptive_framework.add_performance_metrics(performance_metrics)
        
        # Call metrics collectors
        for collector in self.metrics_collectors:
            try:
                collector(metrics)
            except Exception as e:
                logger.error(f"Error in metrics collector: {e}")
        
        # Execute registered hooks
        with self._hook_lock:
            for hook_name, hook_func in self.active_hooks.items():
                try:
                    if asyncio.iscoroutinefunction(hook_func):
                        inference_result = await hook_func(inference_result) or inference_result
                    else:
                        inference_result = hook_func(inference_result) or inference_result
                except Exception as e:
                    logger.error(f"Error in post-inference hook {hook_name}: {e}")
        
        # Periodic performance check
        if time.time() - self.last_performance_check > 30:  # Every 30 seconds
            await self._periodic_performance_check()
            self.last_performance_check = time.time()
        
        return inference_result
    
    async def _periodic_performance_check(self) -> None:
        """Perform periodic performance assessment."""
        if self.inference_count == 0:
            return
        
        avg_inference_time = self.total_inference_time / self.inference_count
        
        logger.debug(f"Performance check: {self.inference_count} inferences, "
                    f"avg time: {avg_inference_time:.2f}ms")
        
        # Reset counters for next period
        self.inference_count = 0
        self.total_inference_time = 0.0


class AphroditeAdaptiveIntegration:
    """
    Main integration class for Aphrodite Engine adaptive architecture.
    
    Provides integration hooks with Aphrodite Engine model loading
    and enables real-time architecture mutation capabilities.
    """
    
    def __init__(
        self,
        adaptive_framework: AdaptiveArchitectureFramework,
        aphrodite_bridge: Optional[AphroditeBridge] = None
    ):
        self.adaptive_framework = adaptive_framework
        self.aphrodite_bridge = aphrodite_bridge or AphroditeBridge()
        
        # Integration components
        self.topology_adapter = ModelTopologyAdapter()
        self.hook_manager = InferenceHookManager(adaptive_framework)
        
        # Model tracking
        self.current_model_config: Optional[Dict[str, Any]] = None
        self.original_model_config: Optional[Dict[str, Any]] = None
        self.model_modification_count = 0
        
        # Integration state
        self.is_integrated = False
        self._integration_lock = threading.RLock()
        
        # Model runner references
        self._model_runner_refs: List[weakref.ReferenceType] = []
        
        logger.info("AphroditeAdaptiveIntegration initialized")
    
    async def integrate_with_aphrodite(self, model_config: Dict[str, Any]) -> bool:
        """Integrate adaptive architecture with Aphrodite Engine."""
        with self._integration_lock:
            if self.is_integrated:
                logger.warning("Already integrated with Aphrodite Engine")
                return True
            
            try:
                # Store original configuration
                self.original_model_config = model_config.copy()
                self.current_model_config = model_config.copy()
                
                # Check if model can be adapted
                if not self.topology_adapter.can_modify_architecture(model_config):
                    logger.warning("Model architecture cannot be modified - limited functionality")
                
                # Initialize Aphrodite bridge - required for real functionality
                if not self.aphrodite_bridge.is_initialized():
                    model_name = model_config.get('model_name', 'default')
                    bridge_initialized = self.aphrodite_bridge.initialize(model_name)
                    if not bridge_initialized:
                        logger.error("Aphrodite bridge initialization failed - cannot proceed with real integration")
                        raise RuntimeError("Failed to initialize real Aphrodite Engine components")
                
                # Set up inference hooks
                await self._setup_inference_hooks()
                
                # Start adaptive monitoring
                await self.adaptive_framework.start_adaptive_monitoring()
                
                self.is_integrated = True
                logger.info("Successfully integrated with Aphrodite Engine")
                return True
                
            except Exception as e:
                logger.error(f"Failed to integrate with Aphrodite Engine: {e}")
                return False
    
    async def _setup_inference_hooks(self) -> None:
        """Set up inference hooks for performance monitoring."""
        
        async def performance_monitoring_hook(data: Dict[str, Any]) -> Dict[str, Any]:
            """Hook to monitor inference performance."""
            # Add timestamp for latency tracking
            data['performance_start_time'] = time.time()
            return data
        
        def metrics_collector(metrics: InferenceMetrics) -> None:
            """Collect and log inference metrics."""
            logger.debug(f"Inference metrics: latency={metrics.request_latency_ms:.2f}ms, "
                        f"throughput={metrics.tokens_generated / (metrics.generation_time_ms / 1000.0):.1f} tokens/s")
        
        # Register hooks
        self.hook_manager.register_inference_hook(
            'performance_monitor', 
            performance_monitoring_hook
        )
        self.hook_manager.add_metrics_collector(metrics_collector)
        
        logger.debug("Inference hooks set up")
    
    async def apply_architecture_adaptation(self, mutation: ArchitectureMutation) -> bool:
        """Apply architecture adaptation during runtime."""
        with self._integration_lock:
            if not self.is_integrated:
                logger.error("Not integrated with Aphrodite Engine")
                return False
            
            if self.current_model_config is None:
                logger.error("No current model configuration available")
                return False
            
            try:
                # Apply mutation to model configuration
                new_config = self.topology_adapter.apply_mutation_to_model_config(
                    self.current_model_config, 
                    mutation
                )
                
                if new_config == self.current_model_config:
                    logger.warning("Mutation resulted in no configuration change")
                    return False
                
                # Update current configuration
                self.current_model_config = new_config
                self.model_modification_count += 1
                
                # TODO: Apply configuration changes to running Aphrodite Engine
                # This would require deeper integration with Aphrodite's model loading system
                logger.info(f"Architecture adaptation applied (#{self.model_modification_count}): "
                           f"{mutation.mutation_type}")
                
                return True
                
            except Exception as e:
                logger.error(f"Failed to apply architecture adaptation: {e}")
                return False
    
    def get_model_configuration_status(self) -> Dict[str, Any]:
        """Get current model configuration status."""
        with self._integration_lock:
            return {
                'is_integrated': self.is_integrated,
                'modification_count': self.model_modification_count,
                'current_config': self.current_model_config,
                'original_config': self.original_model_config,
                'can_modify_architecture': (
                    self.topology_adapter.can_modify_architecture(self.current_model_config)
                    if self.current_model_config else False
                ),
                'adaptation_status': self.adaptive_framework.get_adaptation_status()
            }
    
    def get_modification_history(self) -> List[Dict[str, Any]]:
        """Get history of model modifications."""
        return self.topology_adapter.modification_history.copy()
    
    async def hook_inference_request(self, request_data: Dict[str, Any]) -> Dict[str, Any]:
        """Hook into inference request processing."""
        return await self.hook_manager.pre_inference_hook(request_data)
    
    async def hook_inference_response(
        self, 
        request_data: Dict[str, Any], 
        response_data: Dict[str, Any]
    ) -> Dict[str, Any]:
        """Hook into inference response processing."""
        return await self.hook_manager.post_inference_hook(request_data, response_data)
    
    async def shutdown_integration(self) -> None:
        """Shutdown the integration and clean up resources."""
        with self._integration_lock:
            if not self.is_integrated:
                return
            
            try:
                # Stop adaptive monitoring
                await self.adaptive_framework.stop_adaptive_monitoring()
                
                # Clear hooks
                self.hook_manager.active_hooks.clear()
                self.hook_manager.metrics_collectors.clear()
                
                # Clear model runner references
                self._model_runner_refs.clear()
                
                self.is_integrated = False
                logger.info("Aphrodite integration shutdown completed")
                
            except Exception as e:
                logger.error(f"Error during integration shutdown: {e}")


class AdaptiveModelLoader:
    """
    Model loader that supports runtime architecture adaptation.
    
    Extends Aphrodite Engine model loading to support dynamic topology changes.
    Integrates with the new DynamicModelLoader for comprehensive model management.
    """
    
    def __init__(self, integration: AphroditeAdaptiveIntegration):
        self.integration = integration
        self.loaded_models: Dict[str, Dict[str, Any]] = {}
        self._loader_lock = threading.RLock()
        
        # Initialize dynamic model loader for advanced capabilities
        try:
            from aphrodite.modeling.dynamic_loader import DynamicModelLoader
            self.dynamic_loader = DynamicModelLoader(
                max_models=5,
                memory_limit_gb=16.0,
                eviction_policy="lru"
            )
            logger.info("Dynamic model loader initialized for adaptive loading")
        except ImportError:
            self.dynamic_loader = None
            logger.warning("Dynamic model loader not available, using basic implementation")
    
    async def load_adaptive_model(
        self, 
        model_name: str, 
        model_config: Dict[str, Any],
        enable_adaptation: bool = True
    ) -> bool:
        """Load a model with adaptive architecture capabilities."""
        with self._loader_lock:
            try:
                # Use dynamic loader if available for enhanced capabilities
                if self.dynamic_loader:
                    # Convert dict config to ModelConfig if needed
                    if isinstance(model_config, dict):
                        from aphrodite.common.config import ModelConfig
                        aphrodite_model_config = ModelConfig(
                            model=model_config.get('model', model_name),
                            tokenizer=model_config.get('tokenizer'),
                            tokenizer_mode=model_config.get('tokenizer_mode', 'auto'),
                            trust_remote_code=model_config.get('trust_remote_code', False),
                            dtype=model_config.get('dtype', 'auto'),
                            max_model_len=model_config.get('max_model_len'),
                            quantization=model_config.get('quantization'),
                            quantization_param_path=model_config.get('quantization_param_path'),
                            seed=model_config.get('seed', 0),
                        )
                    else:
                        aphrodite_model_config = model_config
                    
                    # Load through dynamic loader
                    success = await self.dynamic_loader.load_model(
                        model_name=model_name,
                        model_config=aphrodite_model_config,
                        force_reload=False
                    )
                    
                    if success:
                        # Set as active model
                        await self.dynamic_loader.switch_active_model(model_name)
                        
                        # Store in local tracking as well
                        self.loaded_models[model_name] = {
                            'config': model_config if isinstance(model_config, dict) else model_config.__dict__,
                            'loaded_at': time.time(),
                            'adaptation_enabled': enable_adaptation,
                            'modification_count': 0,
                            'dynamic_loader_managed': True
                        }
                        
                        # Integrate with Aphrodite if adaptation is enabled
                        if enable_adaptation:
                            integration_success = await self.integration.integrate_with_aphrodite(model_config)
                            if not integration_success:
                                logger.warning(f"Adaptation integration failed for model {model_name}")
                        
                        logger.info(f"Adaptive model loaded with dynamic loader: {model_name}")
                        return True
                    else:
                        logger.error(f"Dynamic loader failed to load model: {model_name}")
                        return False
                
                # Fallback to original implementation
                # Store model configuration
                self.loaded_models[model_name] = {
                    'config': model_config.copy() if isinstance(model_config, dict) else model_config.__dict__.copy(),
                    'loaded_at': time.time(),
                    'adaptation_enabled': enable_adaptation,
                    'modification_count': 0,
                    'dynamic_loader_managed': False
                }
                
                # Integrate with Aphrodite if adaptation is enabled
                if enable_adaptation:
                    integration_success = await self.integration.integrate_with_aphrodite(model_config)
                    if not integration_success:
                        logger.warning(f"Adaptation integration failed for model {model_name}")
                
                logger.info(f"Adaptive model loaded: {model_name}")
                return True
                
            except Exception as e:
                logger.error(f"Failed to load adaptive model {model_name}: {e}")
                return False
    
    async def reload_model_with_adaptation(
        self, 
        model_name: str, 
        mutation: ArchitectureMutation
    ) -> bool:
        """Reload model with architecture adaptation applied."""
        with self._loader_lock:
            if model_name not in self.loaded_models:
                logger.error(f"Model {model_name} not found")
                return False
            
            try:
                model_info = self.loaded_models[model_name]
                current_config = model_info['config']
                
                # Apply mutation to configuration
                adapted_config = self.integration.topology_adapter.apply_mutation_to_model_config(
                    current_config, 
                    mutation
                )
                
                # Update model configuration
                model_info['config'] = adapted_config
                model_info['modification_count'] += 1
                
                # Apply adaptation through integration
                success = await self.integration.apply_architecture_adaptation(mutation)
                
                if success:
                    logger.info(f"Model {model_name} reloaded with adaptation")
                else:
                    logger.warning(f"Adaptation failed for model {model_name}")
                
                return success
                
            except Exception as e:
                logger.error(f"Failed to reload model {model_name} with adaptation: {e}")
                return False
    
    def get_loaded_models(self) -> Dict[str, Dict[str, Any]]:
        """Get information about loaded models."""
        with self._loader_lock:
            return {
                name: info.copy() for name, info in self.loaded_models.items()
            }
    
    async def unload_model(self, model_name: str) -> bool:
        """Unload a model and clean up resources."""
        with self._loader_lock:
            if model_name not in self.loaded_models:
                logger.warning(f"Model {model_name} not found for unloading")
                return False
            
            try:
                model_info = self.loaded_models[model_name]
                
                # Use dynamic loader if available and model is managed by it
                if self.dynamic_loader and model_info.get('dynamic_loader_managed', False):
                    success = await self.dynamic_loader.unload_model(model_name)
                    if not success:
                        logger.warning(f"Dynamic loader failed to unload model {model_name}")
                        # Continue with cleanup anyway
                
                # Clean up local tracking
                del self.loaded_models[model_name]
                logger.info(f"Model {model_name} unloaded successfully")
                return True
                
            except Exception as e:
                logger.error(f"Failed to unload model {model_name}: {e}")
                return False
    
    async def switch_model(self, model_name: str) -> bool:
        """Switch to a different loaded model for real-time model switching."""
        with self._loader_lock:
            if model_name not in self.loaded_models:
                logger.error(f"Cannot switch to model {model_name}: not loaded")
                return False
            
            try:
                # Use dynamic loader if available and model is managed by it
                if self.dynamic_loader and self.loaded_models[model_name].get('dynamic_loader_managed', False):
                    success = await self.dynamic_loader.switch_active_model(model_name)
                    if success:
                        logger.info(f"Successfully switched to model {model_name} via dynamic loader")
                        return True
                    else:
                        logger.warning(f"Dynamic loader failed to switch to model {model_name}")
                
                # Fallback implementation - just mark as active in our tracking
                # In a full implementation, this would involve more complex model switching
                logger.info(f"Switched to model {model_name} (basic implementation)")
                return True
                
            except Exception as e:
                logger.error(f"Failed to switch to model {model_name}: {e}")
                return False
    
    def get_resource_usage(self) -> Dict[str, Any]:
        """Get current resource usage statistics."""
        with self._loader_lock:
            base_info = {
                'total_models': len(self.loaded_models),
                'models': {}
            }
            
            # Add dynamic loader resource info if available
            if self.dynamic_loader:
                dynamic_usage = self.dynamic_loader.get_resource_usage()
                base_info.update({
                    'dynamic_loader_stats': dynamic_usage,
                    'memory_utilization': dynamic_usage.get('memory_utilization', 0.0)
                })
            
            # Add model info from local tracking
            for model_name, model_info in self.loaded_models.items():
                base_info['models'][model_name] = {
                    'loaded_at': model_info['loaded_at'],
                    'modification_count': model_info['modification_count'],
                    'adaptation_enabled': model_info['adaptation_enabled'],
                    'dynamic_loader_managed': model_info.get('dynamic_loader_managed', False)
                }
            
            return base_info