import gc
import threading
import time
import weakref
from collections import defaultdict, deque
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional, Set, Tuple, Union
import torch
from loguru import logger
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
        total_requests = self.cache_hits + self.cache_misses
        if total_requests > 0:
            self.pool_efficiency = self.cache_hits / total_requests
class MemoryPool:
    def __init__(self, max_pool_size: int=1024 * 1024 * 1024, enable_dtesn: bool=True, cleanup_interval: float=60.0):
        self.max_pool_size = max_pool_size
        self.enable_dtesn = enable_dtesn and _HAS_DTESN
        self.cleanup_interval = cleanup_interval
        self._free_blocks: Dict[Tuple[int, torch.dtype, str], deque] = defaultdict(deque)
        self._allocated_blocks: Dict[id, MemoryBlockInfo] = {}
        self._block_registry: Dict[torch.Tensor, MemoryBlockInfo] = weakref.WeakKeyDictionary()
        self.stats = PoolStats()
        self._lock = threading.RLock()
        self._dtesn_levels = [1, 1, 2, 4, 9, 20, 48] if self.enable_dtesn else []
        self._level_pools: Dict[int, Dict] = {}
        self._last_cleanup = time.time()
        self._cleanup_threshold = 0.8
        if self.enable_dtesn:
            self._init_dtesn_pools()
    def _init_dtesn_pools(self):
        try:
            if _HAS_DTESN:
                validator = create_enhanced_validator()
                for level, count in enumerate(self._dtesn_levels):
                    self._level_pools[level] = {'size_per_block': self.max_pool_size // (sum(self._dtesn_levels) * 8), 'blocks': deque(maxlen=count * 4), 'allocated': 0, 'peak_usage': 0}
                logger.info(f'Initialized DTESN memory pools with {len(self._dtesn_levels)} levels')
        except Exception as e:
            logger.warning(f'Failed to initialize DTESN pools: {e}')
            self.enable_dtesn = False
    def allocate(self, size: int, dtype: torch.dtype=torch.float32, device: str='cuda', requires_grad: bool=False) -> torch.Tensor:
        with self._lock:
            element_size = torch.tensor([], dtype=dtype).element_size()
            memory_size = size * element_size
            if time.time() - self._last_cleanup > self.cleanup_interval:
                self._cleanup_unused_blocks()
            pool_key = (memory_size, dtype, device)
            tensor = self._try_reuse_block(pool_key, size, requires_grad)
            if tensor is not None:
                self.stats.cache_hits += 1
                self.stats.update_efficiency()
                return tensor
            self.stats.cache_misses += 1
            tensor = self._allocate_new_tensor(size, dtype, device, requires_grad)
            self._register_allocation(tensor, memory_size, device)
            self.stats.update_efficiency()
            return tensor
    def _try_reuse_block(self, pool_key: Tuple[int, torch.dtype, str], size: int, requires_grad: bool) -> Optional[torch.Tensor]:
        memory_size, dtype, device = pool_key
        if pool_key in self._free_blocks and self._free_blocks[pool_key]:
            tensor_ref = self._free_blocks[pool_key].popleft()
            if tensor_ref() is not None:
                tensor = tensor_ref()
                tensor.requires_grad_(requires_grad)
                tensor.zero_()
                if tensor in self._block_registry:
                    self._block_registry[tensor].last_used = time.time()
                    self._block_registry[tensor].ref_count += 1
                return tensor
        for (block_size, block_dtype, block_device), block_deque in self._free_blocks.items():
            if block_dtype == dtype and block_device == device and (memory_size <= block_size <= memory_size * 2) and block_deque:
                tensor_ref = block_deque.popleft()
                if tensor_ref() is not None:
                    tensor = tensor_ref()
                    resized_tensor = tensor.view(size)
                    resized_tensor.requires_grad_(requires_grad)
                    resized_tensor.zero_()
                    if tensor in self._block_registry:
                        self._block_registry[tensor].last_used = time.time()
                        self._block_registry[tensor].ref_count += 1
                    return resized_tensor
        return None
    def _allocate_new_tensor(self, size: int, dtype: torch.dtype, device: str, requires_grad: bool) -> torch.Tensor:
        element_size = torch.tensor([], dtype=dtype).element_size()
        memory_size = size * element_size
        if self.stats.current_memory_usage + memory_size > self.max_pool_size:
            self._force_cleanup()
        if self.enable_dtesn:
            tensor = self._allocate_dtesn_aware(size, dtype, device, requires_grad)
        else:
            tensor = self._allocate_standard(size, dtype, device, requires_grad)
        return tensor
    def _allocate_dtesn_aware(self, size: int, dtype: torch.dtype, device: str, requires_grad: bool) -> torch.Tensor:
        element_size = torch.tensor([], dtype=dtype).element_size()
        memory_size = size * element_size
        level = min(len(self._dtesn_levels) - 1, max(0, int(memory_size.bit_length()) - 20))
        if level in self._level_pools:
            level_info = self._level_pools[level]
            level_info['allocated'] += memory_size
            level_info['peak_usage'] = max(level_info['peak_usage'], level_info['allocated'])
        if device == 'cuda' and torch.cuda.is_available():
            aligned_size = (size + 7) // 8 * 8
            tensor = torch.empty(aligned_size, dtype=dtype, device=device)
            tensor = tensor[:size]
        else:
            tensor = torch.empty(size, dtype=dtype, device=device)
        tensor.requires_grad_(requires_grad)
        return tensor
    def _allocate_standard(self, size: int, dtype: torch.dtype, device: str, requires_grad: bool) -> torch.Tensor:
        tensor = torch.empty(size, dtype=dtype, device=device)
        tensor.requires_grad_(requires_grad)
        return tensor
    def _register_allocation(self, tensor: torch.Tensor, memory_size: int, device: str):
        current_time = time.time()
        block_info = MemoryBlockInfo(size=memory_size, dtype=tensor.dtype, device=device, allocated_time=current_time, last_used=current_time, ref_count=1, is_pinned=tensor.is_pinned() if hasattr(tensor, 'is_pinned') else False)
        self._allocated_blocks[id(tensor)] = block_info
        self._block_registry[tensor] = block_info
        self.stats.allocation_count += 1
        self.stats.total_allocated_bytes += memory_size
        self.stats.current_memory_usage += memory_size
        self.stats.peak_memory_usage = max(self.stats.peak_memory_usage, self.stats.current_memory_usage)
    def deallocate(self, tensor: torch.Tensor, force: bool=False):
        with self._lock:
            if tensor not in self._block_registry:
                return
            block_info = self._block_registry[tensor]
            block_info.ref_count -= 1
            if block_info.ref_count > 0:
                return
            memory_size = block_info.size
            if force or self._should_force_deallocate(tensor, block_info):
                self._force_deallocate(tensor, block_info)
            else:
                self._return_to_pool(tensor, block_info)
            self.stats.deallocation_count += 1
            self.stats.current_memory_usage -= memory_size
            self.stats.total_freed_bytes += memory_size
    def _should_force_deallocate(self, tensor: torch.Tensor, block_info: MemoryBlockInfo) -> bool:
        if self.stats.current_memory_usage > self.max_pool_size * self._cleanup_threshold:
            return True
        if block_info.size > self.max_pool_size // 10:
            return True
        if time.time() - block_info.last_used > self.cleanup_interval * 2:
            return True
        return False
    def _return_to_pool(self, tensor: torch.Tensor, block_info: MemoryBlockInfo):
        pool_key = (block_info.size, block_info.dtype, block_info.device)
        tensor_ref = weakref.ref(tensor)
        self._free_blocks[pool_key].append(tensor_ref)
        max_blocks_per_bucket = 16
        while len(self._free_blocks[pool_key]) > max_blocks_per_bucket:
            old_ref = self._free_blocks[pool_key].popleft()
            if old_ref() is not None:
                self._force_deallocate_ref(old_ref)
    def _force_deallocate(self, tensor: torch.Tensor, block_info: MemoryBlockInfo):
        if id(tensor) in self._allocated_blocks:
            del self._allocated_blocks[id(tensor)]
        if tensor in self._block_registry:
            del self._block_registry[tensor]
        if self.enable_dtesn:
            memory_size = block_info.size
            level = min(len(self._dtesn_levels) - 1, max(0, int(memory_size.bit_length()) - 20))
            if level in self._level_pools:
                self._level_pools[level]['allocated'] -= memory_size
        del tensor
    def _force_deallocate_ref(self, tensor_ref: weakref.ref):
        tensor = tensor_ref()
        if tensor is not None and tensor in self._block_registry:
            block_info = self._block_registry[tensor]
            self._force_deallocate(tensor, block_info)
    def _cleanup_unused_blocks(self):
        current_time = time.time()
        cleanup_threshold = current_time - self.cleanup_interval
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
        gc.collect()
        if torch.cuda.is_available():
            torch.cuda.empty_cache()
    def _force_cleanup(self):
        logger.info('Memory pool under pressure, forcing cleanup...')
        self._cleanup_unused_blocks()
        if self.stats.current_memory_usage > self.max_pool_size * 0.9:
            blocks_freed = 0
            for pool_key, block_deque in list(self._free_blocks.items()):
                keep_count = max(1, len(block_deque) // 4)
                while len(block_deque) > keep_count and block_deque:
                    tensor_ref = block_deque.popleft()
                    tensor = tensor_ref()
                    if tensor is not None and tensor in self._block_registry:
                        block_info = self._block_registry[tensor]
                        self._force_deallocate(tensor, block_info)
                        blocks_freed += 1
            logger.info(f'Force cleanup freed {blocks_freed} blocks')
    def get_memory_stats(self) -> Dict[str, Any]:
        stats_dict = {'pool_stats': {'total_allocated_mb': self.stats.total_allocated_bytes / (1024 * 1024), 'total_freed_mb': self.stats.total_freed_bytes / (1024 * 1024), 'current_usage_mb': self.stats.current_memory_usage / (1024 * 1024), 'peak_usage_mb': self.stats.peak_memory_usage / (1024 * 1024), 'pool_efficiency': self.stats.pool_efficiency, 'allocation_count': self.stats.allocation_count, 'deallocation_count': self.stats.deallocation_count, 'cache_hit_rate': self.stats.cache_hits / max(1, self.stats.cache_hits + self.stats.cache_misses)}, 'pool_state': {'free_block_count': sum((len(deque) for deque in self._free_blocks.values())), 'allocated_block_count': len(self._allocated_blocks), 'pool_buckets': len(self._free_blocks), 'max_pool_size_mb': self.max_pool_size / (1024 * 1024), 'utilization': self.stats.current_memory_usage / self.max_pool_size}}
        if self.enable_dtesn and self._level_pools:
            dtesn_stats = {}
            for level, pool_info in self._level_pools.items():
                dtesn_stats[f'level_{level}'] = {'allocated_mb': pool_info['allocated'] / (1024 * 1024), 'peak_usage_mb': pool_info['peak_usage'] / (1024 * 1024), 'utilization': pool_info['allocated'] / max(1, pool_info['size_per_block'])}
            stats_dict['dtesn_levels'] = dtesn_stats
        return stats_dict
    def clear_pool(self):
        with self._lock:
            for pool_key, block_deque in self._free_blocks.items():
                while block_deque:
                    tensor_ref = block_deque.popleft()
                    tensor = tensor_ref()
                    if tensor is not None and tensor in self._block_registry:
                        block_info = self._block_registry[tensor]
                        self._force_deallocate(tensor, block_info)
            self._free_blocks.clear()
            self._allocated_blocks.clear()
            self._block_registry.clear()
            if self.enable_dtesn:
                for level_info in self._level_pools.values():
                    level_info['allocated'] = 0
                    level_info['blocks'].clear()
            old_stats = self.stats
            self.stats = PoolStats()
            gc.collect()
            if torch.cuda.is_available():
                torch.cuda.empty_cache()
            logger.info('Memory pool cleared and reset')
_global_memory_pool: Optional[MemoryPool] = None
_pool_lock = threading.Lock()
def get_memory_pool(max_pool_size: Optional[int]=None, enable_dtesn: bool=True) -> MemoryPool:
    global _global_memory_pool
    with _pool_lock:
        if _global_memory_pool is None:
            pool_size = max_pool_size or 1024 * 1024 * 1024
            _global_memory_pool = MemoryPool(max_pool_size=pool_size, enable_dtesn=enable_dtesn)
            logger.info(f'Initialized global memory pool with size {pool_size / 1024 ** 3:.2f} GB')
        return _global_memory_pool
def reset_memory_pool():
    global _global_memory_pool
    with _pool_lock:
        if _global_memory_pool is not None:
            _global_memory_pool.clear_pool()
            _global_memory_pool = None