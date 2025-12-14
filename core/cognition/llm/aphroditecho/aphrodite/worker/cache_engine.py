"""CacheEngine class for managing the KV cache."""
from typing import List

import torch

from aphrodite.attention import get_attn_backend
from aphrodite.common.config import (CacheConfig, DeviceConfig, ModelConfig,
                                     ParallelConfig)
from aphrodite.utils import (STR_DTYPE_TO_TORCH_DTYPE, LayerBlockType,
                                    get_dtype_size, is_pin_memory_available)
from aphrodite.worker.memory_pool import get_memory_pool


class CacheEngine:
    """Manages the KV cache.

    This class is responsible for initializing and managing the GPU and CPU KV
    caches. It also provides methods for performing KV cache operations, such
    as swapping and copying.
    """

    def __init__(
        self,
        cache_config: CacheConfig,
        model_config: ModelConfig,
        parallel_config: ParallelConfig,
        device_config: DeviceConfig,
    ) -> None:
        self.cache_config = cache_config
        self.model_config = model_config
        self.parallel_config = parallel_config
        self.device_config = device_config

        self.head_size = model_config.get_head_size()
        # Models like Jamba, have mixed typed layers, E.g Mamba
        self.num_attention_layers = model_config.get_num_layers_by_block_type(
            parallel_config, LayerBlockType.attention)
        self.num_kv_heads = model_config.get_num_kv_heads(parallel_config)

        self.block_size = cache_config.block_size
        self.num_gpu_blocks = cache_config.num_gpu_blocks
        if self.num_gpu_blocks:
            self.num_gpu_blocks //= parallel_config.pipeline_parallel_size
        self.num_cpu_blocks = cache_config.num_cpu_blocks
        if self.num_cpu_blocks:
            self.num_cpu_blocks //= parallel_config.pipeline_parallel_size

        if cache_config.cache_dtype == "auto":
            self.dtype = model_config.dtype
        else:
            self.dtype = STR_DTYPE_TO_TORCH_DTYPE[cache_config.cache_dtype]

        # Get attention backend.
        self.attn_backend = get_attn_backend(self.head_size,
                                             model_config.dtype,
                                             cache_config.cache_dtype,
                                             self.block_size,
                                             model_config.is_attention_free,
                                             use_mla=model_config.use_mla)

        # Initialize memory pool for optimized allocation
        # Estimate total cache size for pool sizing
        total_cache_size = self._estimate_total_cache_size()
        self.memory_pool = get_memory_pool(
            max_pool_size=max(total_cache_size * 2, 1024 * 1024 * 1024),  # At least 1GB
            enable_dtesn=True
        )

        # Initialize the cache with memory pool optimization.
        self.gpu_cache = self._allocate_kv_cache(
            self.num_gpu_blocks, self.device_config.device_type)
        self.cpu_cache = self._allocate_kv_cache(self.num_cpu_blocks, "cpu")
        
        # Track allocated tensors for cleanup
        self._allocated_tensors = []

    def _allocate_kv_cache(
        self,
        num_blocks: int,
        device: str,
    ) -> List[torch.Tensor]:
        """Allocates KV cache on the specified device using optimized memory pool."""
        if num_blocks == 0:
            return []
            
        kv_cache_generic_shape = self.attn_backend.get_kv_cache_shape(
            num_blocks, self.block_size, self.num_kv_heads, self.head_size)
        pin_memory = is_pin_memory_available() if device == "cpu" else False
        kv_cache: List[torch.Tensor] = []
        try:
            kv_cache_stride_order = self.attn_backend.get_kv_cache_stride_order(
            )
        except (AttributeError, NotImplementedError):
            kv_cache_stride_order = tuple(range(len(kv_cache_generic_shape)))

        # The allocation respects the backend-defined stride order to ensure
        # the semantic remains consistent for each backend. We first obtain the
        # generic kv cache shape and then permute it according to the stride
        # order which could result in a non-contiguous tensor.
        kv_cache_allocation_shape = tuple(kv_cache_generic_shape[i]
                                          for i in kv_cache_stride_order)

        # Calculate total elements needed
        total_elements = 1
        for dim in kv_cache_allocation_shape:
            total_elements *= dim

        for layer_idx in range(self.num_attention_layers):
            # Use memory pool for allocation to enable reuse
            try:
                layer_kv_cache = self.memory_pool.allocate(
                    size=total_elements,
                    dtype=self.dtype,
                    device=device,
                    requires_grad=False
                )
                
                # Reshape to required shape and apply stride order
                layer_kv_cache = layer_kv_cache.view(kv_cache_allocation_shape)
                layer_kv_cache = layer_kv_cache.permute(*kv_cache_stride_order)
                
                # Pin memory if required and supported
                if pin_memory and device == "cpu":
                    layer_kv_cache = layer_kv_cache.pin_memory()
                
                # Initialize with zeros (null block requirement)
                layer_kv_cache.zero_()
                
            except Exception as e:
                # Fallback to standard allocation if memory pool fails
                layer_kv_cache = torch.zeros(
                    kv_cache_allocation_shape,
                    dtype=self.dtype,
                    pin_memory=pin_memory,
                    device=device).permute(*kv_cache_stride_order)

            kv_cache.append(layer_kv_cache)
            self._allocated_tensors.append(layer_kv_cache)
            
        return kv_cache
    
    def _estimate_total_cache_size(self) -> int:
        """Estimate total cache size in bytes for memory pool sizing."""
        if not hasattr(self, 'head_size'):
            return 1024 * 1024 * 1024  # 1GB default
            
        # Calculate single block size
        single_block_size = self.get_cache_block_size(
            self.cache_config, self.model_config, self.parallel_config)
        
        # Total size = (GPU blocks + CPU blocks) * block size + overhead
        total_blocks = (self.num_gpu_blocks or 0) + (self.num_cpu_blocks or 0)
        overhead_factor = 1.5  # 50% overhead for fragmentation and pooling
        
        return int(total_blocks * single_block_size * overhead_factor)

    def swap_in(self, src_to_dst: torch.Tensor) -> None:
        for i in range(self.num_attention_layers):
            self.attn_backend.swap_blocks(self.cpu_cache[i], self.gpu_cache[i],
                                          src_to_dst)

    def swap_out(self, src_to_dst: torch.Tensor) -> None:
        for i in range(self.num_attention_layers):
            self.attn_backend.swap_blocks(self.gpu_cache[i], self.cpu_cache[i],
                                          src_to_dst)

    def copy(self, src_to_dsts: torch.Tensor) -> None:
        self.attn_backend.copy_blocks(self.gpu_cache, src_to_dsts)

    def get_memory_usage_stats(self) -> dict:
        """Get detailed memory usage statistics."""
        stats = {
            'cache_engine': {
                'gpu_blocks': self.num_gpu_blocks,
                'cpu_blocks': self.num_cpu_blocks,
                'block_size': self.block_size,
                'dtype': str(self.dtype),
                'attention_layers': self.num_attention_layers,
                'allocated_tensors': len(self._allocated_tensors)
            }
        }
        
        # Add memory pool stats if available
        if hasattr(self, 'memory_pool'):
            stats['memory_pool'] = self.memory_pool.get_memory_stats()
        
        return stats
    
    def cleanup_cache(self):
        """Clean up allocated cache tensors through memory pool."""
        if hasattr(self, 'memory_pool'):
            for tensor in self._allocated_tensors:
                try:
                    self.memory_pool.deallocate(tensor)
                except Exception:
                    # Ignore errors during cleanup
                    pass
        
        self._allocated_tensors.clear()
    
    def __del__(self):
        """Cleanup when cache engine is destroyed."""
        if hasattr(self, '_allocated_tensors'):
            self.cleanup_cache()

    @staticmethod
    def get_cache_block_size(
        cache_config: CacheConfig,
        model_config: ModelConfig,
        parallel_config: ParallelConfig,
    ) -> int:
        head_size = model_config.get_head_size()
        num_heads = model_config.get_num_kv_heads(parallel_config)
        num_attention_layers = model_config.get_num_layers_by_block_type(
            parallel_config, LayerBlockType.attention)

        if cache_config.cache_dtype == "auto":
            dtype = model_config.dtype
        else:
            dtype = STR_DTYPE_TO_TORCH_DTYPE[cache_config.cache_dtype]

        key_cache_entry = num_heads * head_size

        # For MLA there is no value cache, since the latent vector
        # is joint keys and values.
        value_cache_entry = key_cache_entry if not model_config.use_mla else 0
        total = num_attention_layers * cache_config.block_size * \
            (key_cache_entry + value_cache_entry)

        dtype_size = get_dtype_size(dtype)
        return dtype_size * total
