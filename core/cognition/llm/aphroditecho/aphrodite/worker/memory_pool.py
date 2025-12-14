"""Memory Pool Management for Optimized Backend Processing.

This module implements efficient memory pooling for KV cache blocks and sampling
parameters to achieve 30% memory usage reduction under load.

Integrates with DTESN architecture for hierarchical memory allocation following
OEIS A000081 patterns for optimal performance.
"""

import gc
import threading
import time
import weakref
from collections import defaultdict, deque
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional, Set, Tuple, Union

import torch
from loguru import logger

# DTESN integration for hierarchical memory pools
try:
    import sys
    import os
    sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..', 'echo.kern'))
    from oeis_a000081_enumerator import create_enhanced_validator
    _HAS_DTESN = True
except ImportError:
    _HAS_DTESN = False


@dataclass
class MemoryBlockInfo:
    """Information about a memory block in the pool."""
    size: int
    dtype: torch.dtype
    device: str
    allocated_time: float
    last_used: float
    ref_count: int = 0
    is_pinned: bool = False
    
    def __post_init__(self):
        if self.last_used == 0:
            self.last_used = self.allocated_time


@dataclass  
class PoolStats:
    """Statistics for memory pool performance tracking."""
    total_allocated_bytes: int = 0
    total_freed_bytes: int = 0
    peak_memory_usage: int = 0
    current_memory_usage: int = 0
    allocation_count: int = 0
    deallocation_count: int = 0
    cache_hits: int = 0
    cache_misses: int = 0
    pool_efficiency: float = 0.0
    
    def update_efficiency(self):
        """Update pool efficiency metric."""
        total_requests = self.cache_hits + self.cache_misses
        if total_requests > 0:
            self.pool_efficiency = self.cache_hits / total_requests
    

class MemoryPool:
    """
    Efficient memory pool for reusable tensor allocations.
    
    Implements hierarchical pooling based on DTESN OEIS A000081 patterns
    for optimal memory layout and access patterns.
    """
    
    def __init__(self, 
                 max_pool_size: int = 1024 * 1024 * 1024,  # 1GB default
                 enable_dtesn: bool = True,
                 cleanup_interval: float = 60.0):  # 1 minute
        """
        Initialize memory pool with DTESN-aware allocation strategies.
        
        Args:
            max_pool_size: Maximum memory pool size in bytes
            enable_dtesn: Enable DTESN hierarchical allocation patterns
            cleanup_interval: Cleanup interval in seconds
        """
        self.max_pool_size = max_pool_size
        self.enable_dtesn = enable_dtesn and _HAS_DTESN
        self.cleanup_interval = cleanup_interval
        
        # Memory pool storage organized by size buckets
        self._free_blocks: Dict[Tuple[int, torch.dtype, str], deque] = defaultdict(deque)
        self._allocated_blocks: Dict[id, MemoryBlockInfo] = {}
        self._block_registry: Dict[torch.Tensor, MemoryBlockInfo] = weakref.WeakKeyDictionary()
        
        # Statistics tracking
        self.stats = PoolStats()
        
        # Thread safety
        self._lock = threading.RLock()
        
        # DTESN hierarchical levels (based on OEIS A000081)
        self._dtesn_levels = [1, 1, 2, 4, 9, 20, 48] if self.enable_dtesn else []
        self._level_pools: Dict[int, Dict] = {}
        
        # Cleanup management
        self._last_cleanup = time.time()
        self._cleanup_threshold = 0.8  # Cleanup when 80% full
        
        if self.enable_dtesn:
            self._init_dtesn_pools()
    
    def _init_dtesn_pools(self):
        """Initialize DTESN hierarchical memory pools."""
        try:
            if _HAS_DTESN:
                validator = create_enhanced_validator()
                # Create hierarchical pools based on OEIS A000081 structure
                for level, count in enumerate(self._dtesn_levels):
                    self._level_pools[level] = {
                        'size_per_block': self.max_pool_size // (sum(self._dtesn_levels) * 8),
                        'blocks': deque(maxlen=count * 4),  # 4x overallocation for efficiency
                        'allocated': 0,
                        'peak_usage': 0
                    }
                logger.info(f"Initialized DTESN memory pools with {len(self._dtesn_levels)} levels")
        except Exception as e:
            logger.warning(f"Failed to initialize DTESN pools: {e}")
            self.enable_dtesn = False
    
    def allocate(self, 
                 size: int, 
                 dtype: torch.dtype = torch.float32,
                 device: str = "cuda",
                 requires_grad: bool = False) -> torch.Tensor:
        """
        Allocate tensor from memory pool with optimal reuse strategy.
        
        Args:
            size: Number of elements (not bytes)  
            dtype: Tensor data type
            device: Target device
            requires_grad: Whether tensor requires gradient computation
            
        Returns:
            Allocated tensor from pool or new allocation
        """
        with self._lock:
            # Calculate actual memory size in bytes
            element_size = torch.tensor([], dtype=dtype).element_size()
            memory_size = size * element_size
            
            # Check for periodic cleanup
            if time.time() - self._last_cleanup > self.cleanup_interval:
                self._cleanup_unused_blocks()
            
            # Try to find reusable block
            pool_key = (memory_size, dtype, device)
            tensor = self._try_reuse_block(pool_key, size, requires_grad)
            
            if tensor is not None:
                self.stats.cache_hits += 1
                self.stats.update_efficiency()
                return tensor
            
            # Allocate new tensor if no reusable block found
            self.stats.cache_misses += 1
            tensor = self._allocate_new_tensor(size, dtype, device, requires_grad)
            
            # Register the new allocation
            self._register_allocation(tensor, memory_size, device)
            
            self.stats.update_efficiency()
            return tensor
    
    def _try_reuse_block(self, 
                        pool_key: Tuple[int, torch.dtype, str], 
                        size: int,
                        requires_grad: bool) -> Optional[torch.Tensor]:
        """Try to reuse an existing block from the pool."""
        memory_size, dtype, device = pool_key
        
        # Check exact size match first
        if pool_key in self._free_blocks and self._free_blocks[pool_key]:
            tensor_ref = self._free_blocks[pool_key].popleft()
            if tensor_ref() is not None:
                tensor = tensor_ref()
                tensor.requires_grad_(requires_grad)
                tensor.zero_()  # Clear previous data
                
                # Update usage tracking
                if tensor in self._block_registry:
                    self._block_registry[tensor].last_used = time.time()
                    self._block_registry[tensor].ref_count += 1
                
                return tensor
        
        # Try to find compatible larger block (up to 2x size)
        for (block_size, block_dtype, block_device), block_deque in self._free_blocks.items():
            if (block_dtype == dtype and block_device == device and 
                memory_size <= block_size <= memory_size * 2 and block_deque):
                
                tensor_ref = block_deque.popleft()
                if tensor_ref() is not None:
                    tensor = tensor_ref()
                    # Create a view of the appropriate size
                    resized_tensor = tensor.view(size)
                    resized_tensor.requires_grad_(requires_grad)
                    resized_tensor.zero_()
                    
                    # Update tracking
                    if tensor in self._block_registry:
                        self._block_registry[tensor].last_used = time.time()
                        self._block_registry[tensor].ref_count += 1
                    
                    return resized_tensor
        
        return None
    
    def _allocate_new_tensor(self, 
                           size: int,
                           dtype: torch.dtype,
                           device: str, 
                           requires_grad: bool) -> torch.Tensor:
        """Allocate a new tensor with memory pressure management."""
        element_size = torch.tensor([], dtype=dtype).element_size()
        memory_size = size * element_size
        
        # Check memory pressure and cleanup if needed
        if self.stats.current_memory_usage + memory_size > self.max_pool_size:
            self._force_cleanup()
        
        # Use DTESN-aware allocation if enabled
        if self.enable_dtesn:
            tensor = self._allocate_dtesn_aware(size, dtype, device, requires_grad)
        else:
            tensor = self._allocate_standard(size, dtype, device, requires_grad)
        
        return tensor
    
    def _allocate_dtesn_aware(self, 
                            size: int, 
                            dtype: torch.dtype,
                            device: str,
                            requires_grad: bool) -> torch.Tensor:
        """Allocate using DTESN hierarchical pattern for optimal memory layout."""
        element_size = torch.tensor([], dtype=dtype).element_size()
        memory_size = size * element_size
        
        # Find appropriate DTESN level based on allocation size
        level = min(len(self._dtesn_levels) - 1, 
                   max(0, int(memory_size.bit_length()) - 20))  # Rough size bucketing
        
        if level in self._level_pools:
            level_info = self._level_pools[level]
            level_info['allocated'] += memory_size
            level_info['peak_usage'] = max(level_info['peak_usage'], level_info['allocated'])
        
        # Use optimized allocation with memory alignment
        if device == "cuda" and torch.cuda.is_available():
            # Ensure CUDA memory alignment for optimal access patterns
            aligned_size = ((size + 7) // 8) * 8  # 8-element alignment
            tensor = torch.empty(aligned_size, dtype=dtype, device=device)
            tensor = tensor[:size]  # View to original size
        else:
            tensor = torch.empty(size, dtype=dtype, device=device)
        
        tensor.requires_grad_(requires_grad)
        return tensor
    
    def _allocate_standard(self, 
                         size: int,
                         dtype: torch.dtype, 
                         device: str,
                         requires_grad: bool) -> torch.Tensor:
        """Standard tensor allocation fallback."""
        tensor = torch.empty(size, dtype=dtype, device=device)
        tensor.requires_grad_(requires_grad)
        return tensor
    
    def _register_allocation(self, tensor: torch.Tensor, memory_size: int, device: str):
        """Register a new allocation in tracking structures."""
        current_time = time.time()
        
        # Create memory block info
        block_info = MemoryBlockInfo(
            size=memory_size,
            dtype=tensor.dtype,
            device=device,
            allocated_time=current_time,
            last_used=current_time,
            ref_count=1,
            is_pinned=tensor.is_pinned() if hasattr(tensor, 'is_pinned') else False
        )
        
        # Register in tracking structures
        self._allocated_blocks[id(tensor)] = block_info
        self._block_registry[tensor] = block_info
        
        # Update statistics
        self.stats.allocation_count += 1
        self.stats.total_allocated_bytes += memory_size
        self.stats.current_memory_usage += memory_size
        self.stats.peak_memory_usage = max(self.stats.peak_memory_usage, 
                                         self.stats.current_memory_usage)
    
    def deallocate(self, tensor: torch.Tensor, force: bool = False):
        """
        Return tensor to pool for reuse or deallocate if pool is full.
        
        Args:
            tensor: Tensor to deallocate
            force: Force immediate deallocation without pooling
        """
        with self._lock:
            if tensor not in self._block_registry:
                return  # Not from this pool
            
            block_info = self._block_registry[tensor]
            block_info.ref_count -= 1
            
            # Only deallocate when ref_count reaches 0
            if block_info.ref_count > 0:
                return
            
            memory_size = block_info.size
            
            if force or self._should_force_deallocate(tensor, block_info):
                # Force deallocation
                self._force_deallocate(tensor, block_info)
            else:
                # Return to pool for reuse
                self._return_to_pool(tensor, block_info)
            
            # Update statistics
            self.stats.deallocation_count += 1
            self.stats.current_memory_usage -= memory_size
            self.stats.total_freed_bytes += memory_size
    
    def _should_force_deallocate(self, tensor: torch.Tensor, block_info: MemoryBlockInfo) -> bool:
        """Determine if tensor should be forcefully deallocated instead of pooled."""
        # Force deallocate if pool is over capacity
        if self.stats.current_memory_usage > self.max_pool_size * self._cleanup_threshold:
            return True
        
        # Force deallocate very large tensors to prevent pool bloat
        if block_info.size > self.max_pool_size // 10:  # > 10% of pool size
            return True
        
        # Force deallocate if tensor hasn't been used recently
        if time.time() - block_info.last_used > self.cleanup_interval * 2:
            return True
        
        return False
    
    def _return_to_pool(self, tensor: torch.Tensor, block_info: MemoryBlockInfo):
        """Return tensor to appropriate pool for reuse."""
        pool_key = (block_info.size, block_info.dtype, block_info.device)
        
        # Use weak reference to prevent memory leaks
        tensor_ref = weakref.ref(tensor)
        self._free_blocks[pool_key].append(tensor_ref)
        
        # Maintain pool size limits
        max_blocks_per_bucket = 16
        while len(self._free_blocks[pool_key]) > max_blocks_per_bucket:
            old_ref = self._free_blocks[pool_key].popleft()
            if old_ref() is not None:
                self._force_deallocate_ref(old_ref)
    
    def _force_deallocate(self, tensor: torch.Tensor, block_info: MemoryBlockInfo):
        """Force deallocation of tensor and cleanup tracking."""
        # Remove from tracking
        if id(tensor) in self._allocated_blocks:
            del self._allocated_blocks[id(tensor)]
        
        if tensor in self._block_registry:
            del self._block_registry[tensor]
        
        # Update DTESN level tracking
        if self.enable_dtesn:
            memory_size = block_info.size
            level = min(len(self._dtesn_levels) - 1, 
                       max(0, int(memory_size.bit_length()) - 20))
            if level in self._level_pools:
                self._level_pools[level]['allocated'] -= memory_size
        
        # Force garbage collection of the tensor
        del tensor
    
    def _force_deallocate_ref(self, tensor_ref: weakref.ref):
        """Force deallocate from weak reference."""
        tensor = tensor_ref()
        if tensor is not None and tensor in self._block_registry:
            block_info = self._block_registry[tensor]
            self._force_deallocate(tensor, block_info)
    
    def _cleanup_unused_blocks(self):
        """Clean up unused blocks to free memory."""
        current_time = time.time()
        cleanup_threshold = current_time - self.cleanup_interval
        
        # Clean up free blocks that haven't been used recently
        for pool_key, block_deque in list(self._free_blocks.items()):
            cleaned_blocks = deque()
            
            while block_deque:
                tensor_ref = block_deque.popleft()
                tensor = tensor_ref()
                
                if tensor is not None:
                    if tensor in self._block_registry:
                        block_info = self._block_registry[tensor]
                        if block_info.last_used > cleanup_threshold:
                            cleaned_blocks.append(tensor_ref)
                        else:
                            self._force_deallocate(tensor, block_info)
            
            self._free_blocks[pool_key] = cleaned_blocks
        
        self._last_cleanup = current_time
        
        # Force garbage collection after cleanup
        gc.collect()
        if torch.cuda.is_available():
            torch.cuda.empty_cache()
    
    def _force_cleanup(self):
        """Force aggressive cleanup when memory pressure is high."""
        logger.info("Memory pool under pressure, forcing cleanup...")
        
        # First pass: cleanup old unused blocks
        self._cleanup_unused_blocks()
        
        # Second pass: if still over limit, force deallocate some free blocks
        if self.stats.current_memory_usage > self.max_pool_size * 0.9:
            blocks_freed = 0
            for pool_key, block_deque in list(self._free_blocks.items()):
                # Keep only most recently used blocks  
                keep_count = max(1, len(block_deque) // 4)
                
                while len(block_deque) > keep_count and block_deque:
                    tensor_ref = block_deque.popleft()
                    tensor = tensor_ref()
                    if tensor is not None and tensor in self._block_registry:
                        block_info = self._block_registry[tensor]
                        self._force_deallocate(tensor, block_info)
                        blocks_freed += 1
            
            logger.info(f"Force cleanup freed {blocks_freed} blocks")
    
    def get_memory_stats(self) -> Dict[str, Any]:
        """Get comprehensive memory pool statistics."""
        stats_dict = {
            'pool_stats': {
                'total_allocated_mb': self.stats.total_allocated_bytes / (1024 * 1024),
                'total_freed_mb': self.stats.total_freed_bytes / (1024 * 1024),
                'current_usage_mb': self.stats.current_memory_usage / (1024 * 1024),
                'peak_usage_mb': self.stats.peak_memory_usage / (1024 * 1024),
                'pool_efficiency': self.stats.pool_efficiency,
                'allocation_count': self.stats.allocation_count,
                'deallocation_count': self.stats.deallocation_count,
                'cache_hit_rate': (self.stats.cache_hits / 
                                  max(1, self.stats.cache_hits + self.stats.cache_misses))
            },
            'pool_state': {
                'free_block_count': sum(len(deque) for deque in self._free_blocks.values()),
                'allocated_block_count': len(self._allocated_blocks),
                'pool_buckets': len(self._free_blocks),
                'max_pool_size_mb': self.max_pool_size / (1024 * 1024),
                'utilization': self.stats.current_memory_usage / self.max_pool_size
            }
        }
        
        # Add DTESN-specific stats if enabled
        if self.enable_dtesn and self._level_pools:
            dtesn_stats = {}
            for level, pool_info in self._level_pools.items():
                dtesn_stats[f'level_{level}'] = {
                    'allocated_mb': pool_info['allocated'] / (1024 * 1024),
                    'peak_usage_mb': pool_info['peak_usage'] / (1024 * 1024),
                    'utilization': pool_info['allocated'] / max(1, pool_info['size_per_block'])
                }
            stats_dict['dtesn_levels'] = dtesn_stats
        
        return stats_dict
    
    def clear_pool(self):
        """Clear all pooled memory and reset statistics."""
        with self._lock:
            # Force deallocate all free blocks
            for pool_key, block_deque in self._free_blocks.items():
                while block_deque:
                    tensor_ref = block_deque.popleft()
                    tensor = tensor_ref()
                    if tensor is not None and tensor in self._block_registry:
                        block_info = self._block_registry[tensor]
                        self._force_deallocate(tensor, block_info)
            
            # Clear all tracking structures
            self._free_blocks.clear()
            self._allocated_blocks.clear()
            self._block_registry.clear()
            
            # Reset DTESN pools
            if self.enable_dtesn:
                for level_info in self._level_pools.values():
                    level_info['allocated'] = 0
                    level_info['blocks'].clear()
            
            # Reset statistics but preserve configuration
            old_stats = self.stats
            self.stats = PoolStats()
            
            # Force cleanup
            gc.collect()
            if torch.cuda.is_available():
                torch.cuda.empty_cache()
            
            logger.info("Memory pool cleared and reset")


# Global memory pool instance
_global_memory_pool: Optional[MemoryPool] = None
_pool_lock = threading.Lock()


def get_memory_pool(max_pool_size: Optional[int] = None,
                   enable_dtesn: bool = True) -> MemoryPool:
    """
    Get or create the global memory pool instance.
    
    Args:
        max_pool_size: Override default pool size  
        enable_dtesn: Enable DTESN hierarchical allocation
        
    Returns:
        Global memory pool instance
    """
    global _global_memory_pool
    
    with _pool_lock:
        if _global_memory_pool is None:
            pool_size = max_pool_size or (1024 * 1024 * 1024)  # 1GB default
            _global_memory_pool = MemoryPool(
                max_pool_size=pool_size,
                enable_dtesn=enable_dtesn
            )
            logger.info(f"Initialized global memory pool with size {pool_size / (1024**3):.2f} GB")
        
        return _global_memory_pool


def reset_memory_pool():
    """Reset the global memory pool."""
    global _global_memory_pool
    
    with _pool_lock:
        if _global_memory_pool is not None:
            _global_memory_pool.clear_pool()
            _global_memory_pool = None