"""
Model Variant Management System
Phase 8 - SSR-Focused MLOps & Production Observability

Manages multiple model variants for A/B testing with automated rollback mechanisms.
Integrates with existing Aphrodite Engine architecture and DTESN systems.
"""
import asyncio
import time
import threading
from dataclasses import dataclass, field
from typing import Dict, Any, Optional, List, Union, AsyncGenerator
from concurrent.futures import ThreadPoolExecutor
from contextlib import asynccontextmanager

from loguru import logger

from aphrodite.engine.async_aphrodite import AsyncAphrodite
from aphrodite.engine.args_tools import AsyncEngineArgs
from aphrodite.common.sampling_params import SamplingParams
from aphrodite.common.outputs import RequestOutput


@dataclass
class ModelVariant:
    """Represents a model variant for A/B testing"""
    name: str
    model_path: str
    engine: Optional[AsyncAphrodite] = None
    is_loaded: bool = False
    load_time: Optional[float] = None
    error_count: int = 0
    request_count: int = 0
    last_used: Optional[float] = None
    health_score: float = 1.0  # 0.0 to 1.0, where 1.0 is perfect health
    
    def update_health_score(self):
        """Update health score based on error rate and performance"""
        if self.request_count > 0:
            error_rate = self.error_count / self.request_count
            # Health score decreases with error rate
            self.health_score = max(0.0, 1.0 - (error_rate * 2))  # Error rate > 50% = 0 health
        self.last_used = time.time()


class ModelVariantManager:
    """Manages multiple model variants for A/B testing"""
    
    def __init__(self, base_engine_args: AsyncEngineArgs, max_concurrent_models: int = 2):
        self.base_engine_args = base_engine_args
        self.max_concurrent_models = max_concurrent_models
        self.variants: Dict[str, ModelVariant] = {}
        self.active_variant: Optional[str] = None
        self.loading_lock = asyncio.Lock()
        self._executor = ThreadPoolExecutor(max_workers=2, thread_name_prefix="ModelVariant")
        
    async def register_variant(self, name: str, model_path: str, preload: bool = False) -> bool:
        """Register a new model variant"""
        if name in self.variants:
            logger.warning(f"Model variant {name} already registered")
            return False
        
        variant = ModelVariant(name=name, model_path=model_path)
        self.variants[name] = variant
        
        logger.info(f"Registered model variant: {name} -> {model_path}")
        
        if preload:
            return await self.load_variant(name)
        
        return True
    
    async def load_variant(self, name: str) -> bool:
        """Load a model variant into memory"""
        if name not in self.variants:
            logger.error(f"Model variant {name} not registered")
            return False
        
        variant = self.variants[name]
        if variant.is_loaded:
            logger.info(f"Model variant {name} already loaded")
            return True
        
        async with self.loading_lock:
            # Check if we need to unload other variants due to memory constraints
            loaded_variants = [v for v in self.variants.values() if v.is_loaded]
            if len(loaded_variants) >= self.max_concurrent_models:
                # Unload least recently used variant
                lru_variant = min(loaded_variants, key=lambda v: v.last_used or 0)
                await self.unload_variant(lru_variant.name)
            
            try:
                logger.info(f"Loading model variant: {name}")
                start_time = time.time()
                
                # Create engine args specific to this variant
                variant_args = self._create_variant_args(variant.model_path)
                
                # Load the engine
                engine = AsyncAphrodite.from_engine_args(variant_args)
                
                variant.engine = engine
                variant.is_loaded = True
                variant.load_time = time.time() - start_time
                variant.last_used = time.time()
                
                logger.info(f"Successfully loaded variant {name} in {variant.load_time:.2f}s")
                return True
                
            except Exception as e:
                logger.error(f"Failed to load model variant {name}: {e}")
                variant.error_count += 1
                variant.update_health_score()
                return False
    
    async def unload_variant(self, name: str) -> bool:
        """Unload a model variant from memory"""
        if name not in self.variants:
            logger.error(f"Model variant {name} not registered")
            return False
        
        variant = self.variants[name]
        if not variant.is_loaded:
            logger.info(f"Model variant {name} not loaded")
            return True
        
        try:
            logger.info(f"Unloading model variant: {name}")
            
            # Cleanup engine resources
            if variant.engine:
                # Note: AsyncAphrodite doesn't have explicit cleanup method in current implementation
                # In production, you'd want to properly cleanup GPU memory and resources
                variant.engine = None
            
            variant.is_loaded = False
            variant.last_used = None
            
            logger.info(f"Successfully unloaded variant {name}")
            return True
            
        except Exception as e:
            logger.error(f"Failed to unload model variant {name}: {e}")
            return False
    
    async def get_variant_engine(self, name: str) -> Optional[AsyncAphrodite]:
        """Get the engine for a specific variant, loading if necessary"""
        if name not in self.variants:
            logger.error(f"Model variant {name} not registered")
            return None
        
        variant = self.variants[name]
        
        # Load variant if not already loaded
        if not variant.is_loaded:
            success = await self.load_variant(name)
            if not success:
                return None
        
        variant.last_used = time.time()
        return variant.engine
    
    async def generate_with_variant(
        self,
        variant_name: str,
        prompt: str,
        sampling_params: SamplingParams,
        request_id: Optional[str] = None
    ) -> AsyncGenerator[RequestOutput, None]:
        """Generate text using a specific model variant"""
        
        variant = self.variants.get(variant_name)
        if not variant:
            raise ValueError(f"Model variant {variant_name} not registered")
        
        engine = await self.get_variant_engine(variant_name)
        if not engine:
            raise RuntimeError(f"Failed to load model variant {variant_name}")
        
        try:
            variant.request_count += 1
            
            # Generate using the variant engine
            async for output in engine.generate(prompt, sampling_params, request_id):
                yield output
            
            # Update health score on successful generation
            variant.update_health_score()
            
        except Exception as e:
            variant.error_count += 1
            variant.update_health_score()
            logger.error(f"Generation failed for variant {variant_name}: {e}")
            raise
    
    def _create_variant_args(self, model_path: str) -> AsyncEngineArgs:
        """Create engine args for a specific model variant"""
        # Copy base args and override model path
        variant_args = AsyncEngineArgs(
            model=model_path,
            tokenizer=self.base_engine_args.tokenizer or model_path,
            tokenizer_mode=self.base_engine_args.tokenizer_mode,
            trust_remote_code=self.base_engine_args.trust_remote_code,
            download_dir=self.base_engine_args.download_dir,
            load_format=self.base_engine_args.load_format,
            config_format=self.base_engine_args.config_format,
            dtype=self.base_engine_args.dtype,
            kv_cache_dtype=self.base_engine_args.kv_cache_dtype,
            quantization_param_path=self.base_engine_args.quantization_param_path,
            seed=self.base_engine_args.seed,
            max_model_len=self.base_engine_args.max_model_len,
            worker_use_ray=self.base_engine_args.worker_use_ray,
            pipeline_parallel_size=self.base_engine_args.pipeline_parallel_size,
            tensor_parallel_size=self.base_engine_args.tensor_parallel_size,
            max_parallel_loading_workers=self.base_engine_args.max_parallel_loading_workers,
            enable_prefix_caching=self.base_engine_args.enable_prefix_caching,
            disable_custom_all_reduce=self.base_engine_args.disable_custom_all_reduce,
            quantization=self.base_engine_args.quantization,
            enforce_eager=self.base_engine_args.enforce_eager,
            max_context_len_to_capture=self.base_engine_args.max_context_len_to_capture,
            max_seq_len_to_capture=self.base_engine_args.max_seq_len_to_capture,
            disable_custom_all_reduce=self.base_engine_args.disable_custom_all_reduce,
            tokenizer_pool_size=self.base_engine_args.tokenizer_pool_size,
            tokenizer_pool_type=self.base_engine_args.tokenizer_pool_type,
            tokenizer_pool_extra_config=self.base_engine_args.tokenizer_pool_extra_config,
            enable_lora=self.base_engine_args.enable_lora,
            max_loras=self.base_engine_args.max_loras,
            max_lora_rank=self.base_engine_args.max_lora_rank,
            enable_prompt_adapter=self.base_engine_args.enable_prompt_adapter,
            max_prompt_adapters=self.base_engine_args.max_prompt_adapters,
            max_prompt_adapter_token=self.base_engine_args.max_prompt_adapter_token,
            fully_sharded_loras=self.base_engine_args.fully_sharded_loras,
            device=self.base_engine_args.device,
            ray_workers_use_nsight=self.base_engine_args.ray_workers_use_nsight,
            num_gpu_blocks_override=self.base_engine_args.num_gpu_blocks_override,
            num_lookahead_slots=self.base_engine_args.num_lookahead_slots,
            model_loader_extra_config=self.base_engine_args.model_loader_extra_config,
            preemption_mode=self.base_engine_args.preemption_mode,
            served_model_name=f"{variant_args.served_model_name or model_path}-variant" if hasattr(self.base_engine_args, 'served_model_name') else None,
            speculative_model=self.base_engine_args.speculative_model,
            speculative_model_quantization=self.base_engine_args.speculative_model_quantization,
            qlora_adapter_name_or_path=self.base_engine_args.qlora_adapter_name_or_path,
            othermodel_name=model_path,  # Use variant model path
            gpu_memory_utilization=self.base_engine_args.gpu_memory_utilization,
            max_num_batched_tokens=self.base_engine_args.max_num_batched_tokens,
            max_num_seqs=self.base_engine_args.max_num_seqs,
            max_logprobs=self.base_engine_args.max_logprobs,
            disable_log_stats=self.base_engine_args.disable_log_stats,
            revision=self.base_engine_args.revision,
            code_revision=self.base_engine_args.code_revision,
            rope_scaling=self.base_engine_args.rope_scaling,
            rope_theta=self.base_engine_args.rope_theta,
            tokenizer_revision=self.base_engine_args.tokenizer_revision,
            max_cpu_loras=getattr(self.base_engine_args, 'max_cpu_loras', None),
            disable_sliding_window=getattr(self.base_engine_args, 'disable_sliding_window', False),
            use_v2_block_manager=getattr(self.base_engine_args, 'use_v2_block_manager', True),
            enable_chunked_prefill=getattr(self.base_engine_args, 'enable_chunked_prefill', None),
        )
        
        return variant_args
    
    def get_variant_stats(self) -> Dict[str, Dict[str, Any]]:
        """Get statistics for all registered variants"""
        stats = {}
        for name, variant in self.variants.items():
            stats[name] = {
                "name": variant.name,
                "model_path": variant.model_path,
                "is_loaded": variant.is_loaded,
                "load_time": variant.load_time,
                "error_count": variant.error_count,
                "request_count": variant.request_count,
                "last_used": variant.last_used,
                "health_score": variant.health_score,
                "error_rate": (variant.error_count / variant.request_count) if variant.request_count > 0 else 0.0
            }
        return stats
    
    async def health_check_all_variants(self) -> Dict[str, bool]:
        """Perform health check on all loaded variants"""
        results = {}
        
        for name, variant in self.variants.items():
            if not variant.is_loaded:
                results[name] = False
                continue
            
            try:
                # Simple health check - try to generate a short response
                test_params = SamplingParams(temperature=0.0, max_tokens=1)
                async for _ in self.generate_with_variant(name, "Test", test_params):
                    pass
                results[name] = True
            except Exception as e:
                logger.error(f"Health check failed for variant {name}: {e}")
                results[name] = False
                variant.error_count += 1
                variant.update_health_score()
        
        return results
    
    async def cleanup(self):
        """Cleanup all resources"""
        for name in list(self.variants.keys()):
            await self.unload_variant(name)
        
        if self._executor:
            self._executor.shutdown(wait=True)


# Global model variant manager instance
_model_variant_manager: Optional[ModelVariantManager] = None


def initialize_model_variant_manager(base_engine_args: AsyncEngineArgs, max_concurrent_models: int = 2):
    """Initialize the global model variant manager"""
    global _model_variant_manager
    _model_variant_manager = ModelVariantManager(base_engine_args, max_concurrent_models)


def get_model_variant_manager() -> Optional[ModelVariantManager]:
    """Get the global model variant manager instance"""
    return _model_variant_manager