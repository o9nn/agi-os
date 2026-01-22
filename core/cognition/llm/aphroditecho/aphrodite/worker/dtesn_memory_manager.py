import math
import threading
import time
from collections import defaultdict, deque
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional, Tuple, Union
import torch
from loguru import logger
try:
    import sys
    import os
    sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..', 'echo.kern'))
    from oeis_a000081_enumerator import create_enhanced_validator
    from memory_layout_validator import DTESNMemoryValidator
    from embodied_memory_system import EmbodiedMemorySystem, MemoryType
    _HAS_DTESN_CORE = True
    logger.info('DTESN core components loaded successfully')
except ImportError as e:
    logger.warning(f'DTESN core components not available: {e}')
    _HAS_DTESN_CORE = False
    class MemoryType:
        EPISODIC = 'episodic'
        SEMANTIC = 'semantic'
        PROCEDURAL = 'procedural'
@dataclass
class DTESNMemoryBlock:
    level: int
    membrane_id: int
    size: int
    allocated_time: float
    tensor: Optional[torch.Tensor] = None
    ref_count: int = 0
    oeis_compliant: bool = False
    def __post_init__(self):
        if self.allocated_time == 0:
            self.allocated_time = time.time()
@dataclass
class DTESNLevelInfo:
    level: int
    expected_membranes: int
    membrane_size: int
    allocated_membranes: int = 0
    total_allocated_bytes: int = 0
    peak_usage: int = 0
    blocks: Dict[int, DTESNMemoryBlock] = field(default_factory=dict)
class DTESNMemoryManager:
    OEIS_A000081 = [1, 1, 1, 2, 4, 9, 20, 48, 115, 286, 719, 1842]
    def __init__(self, total_memory_limit: int=8 * 1024 * 1024 * 1024, max_hierarchy_depth: int=8, enable_embodied_memory: bool=True):
        self.total_memory_limit = total_memory_limit
        self.max_hierarchy_depth = min(max_hierarchy_depth, len(self.OEIS_A000081))
        self.enable_embodied_memory = enable_embodied_memory and _HAS_DTESN_CORE
        self.levels: Dict[int, DTESNLevelInfo] = {}
        self._init_dtesn_levels()
        self.allocated_blocks: Dict[str, DTESNMemoryBlock] = {}
        self.free_blocks: Dict[Tuple[int, int], deque] = defaultdict(deque)
        self.memory_pressure_threshold = 0.85
        self.stats = {'total_allocations': 0, 'total_deallocations': 0, 'memory_consolidations': 0, 'oeis_compliant_allocations': 0, 'peak_memory_usage': 0, 'current_memory_usage': 0, 'allocation_failures': 0}
        self._lock = threading.RLock()
        self.dtesn_validator = None
        self.embodied_memory = None
        if _HAS_DTESN_CORE:
            self._init_dtesn_components()
    def _init_dtesn_levels(self):
        base_membrane_size = self.total_memory_limit // (sum(self.OEIS_A000081[:self.max_hierarchy_depth]) * 4)
        for level in range(self.max_hierarchy_depth):
            membrane_count = self.OEIS_A000081[level] if level < len(self.OEIS_A000081) else 1
            level_membrane_size = base_membrane_size * 2 ** max(0, level - 2)
            self.levels[level] = DTESNLevelInfo(level=level, expected_membranes=membrane_count, membrane_size=level_membrane_size)
        logger.info(f'Initialized {self.max_hierarchy_depth} DTESN levels with OEIS A000081 structure')
    def _init_dtesn_components(self):
        try:
            self.dtesn_validator = DTESNMemoryValidator()
            if self.enable_embodied_memory:
                self.embodied_memory = EmbodiedMemorySystem(storage_dir='dtesn_aphrodite_memory', max_working_memory=7, dtesn_integration=True)
            logger.info('DTESN components initialized successfully')
        except Exception as e:
            logger.error(f'Failed to initialize DTESN components: {e}')
            self.dtesn_validator = None
            self.embodied_memory = None
    def allocate_tensor(self, size: Union[int, Tuple[int, ...]], dtype: torch.dtype=torch.float32, device: str='cuda', requires_grad: bool=False, memory_type: str='procedural') -> Optional[torch.Tensor]:
        with self._lock:
            if isinstance(size, (list, tuple)):
                total_elements = math.prod(size)
                tensor_shape = size
            else:
                total_elements = size
                tensor_shape = (size,)
            element_size = torch.tensor([], dtype=dtype).element_size()
            memory_bytes = total_elements * element_size
            target_level = self._determine_allocation_level(memory_bytes, memory_type)
            tensor = self._try_reuse_block(target_level, memory_bytes, tensor_shape, dtype, device)
            if tensor is not None:
                if requires_grad:
                    tensor.requires_grad_(True)
                return tensor
            return self._allocate_new_block(target_level, memory_bytes, tensor_shape, dtype, device, requires_grad, memory_type)
    def _determine_allocation_level(self, memory_bytes: int, memory_type: str) -> int:
        size_level = min(self.max_hierarchy_depth - 1, max(0, int(math.log2(memory_bytes)) - 20))
        type_adjustments = {'episodic': 0, 'semantic': 1, 'procedural': -1, 'emotional': 2}
        adjustment = type_adjustments.get(memory_type, 0)
        target_level = max(0, min(self.max_hierarchy_depth - 1, size_level + adjustment))
        return target_level
    def _try_reuse_block(self, level: int, memory_bytes: int, tensor_shape: Tuple[int, ...], dtype: torch.dtype, device: str) -> Optional[torch.Tensor]:
        for size_key, block_deque in self.free_blocks.items():
            block_level, block_size = size_key
            if block_level == level and memory_bytes <= block_size <= memory_bytes * 2 and block_deque:
                block = block_deque.popleft()
                if block.tensor is not None:
                    if block.tensor.dtype == dtype and str(block.tensor.device) == device and (block.tensor.numel() >= math.prod(tensor_shape)):
                        tensor = block.tensor.view(tensor_shape)
                        tensor.zero_()
                        block.ref_count += 1
                        block_id = f'reused_{level}_{int(time.time() * 1000000)}'
                        self.allocated_blocks[block_id] = block
                        return tensor
        return None
    def _allocate_new_block(self, level: int, memory_bytes: int, tensor_shape: Tuple[int, ...], dtype: torch.dtype, device: str, requires_grad: bool, memory_type: str) -> Optional[torch.Tensor]:
        if self._check_memory_pressure():
            self._consolidate_memory()
        level_info = self.levels[level]
        if level_info.allocated_membranes >= level_info.expected_membranes:
            if level + 1 < self.max_hierarchy_depth:
                return self._allocate_new_block(level + 1, memory_bytes, tensor_shape, dtype, device, requires_grad, memory_type)
            else:
                self._force_memory_cleanup()
                if level_info.allocated_membranes >= level_info.expected_membranes:
                    self.stats['allocation_failures'] += 1
                    return None
        try:
            aligned_shape = self._align_tensor_shape(tensor_shape, level)
            tensor = torch.empty(aligned_shape, dtype=dtype, device=device)
            if aligned_shape != tensor_shape:
                tensor = tensor.view(tensor_shape)
            if requires_grad:
                tensor.requires_grad_(True)
            membrane_id = level_info.allocated_membranes
            block = DTESNMemoryBlock(level=level, membrane_id=membrane_id, size=memory_bytes, allocated_time=time.time(), tensor=tensor, ref_count=1, oeis_compliant=True)
            block_id = f'new_{level}_{membrane_id}_{int(time.time() * 1000000)}'
            self.allocated_blocks[block_id] = block
            level_info.allocated_membranes += 1
            level_info.total_allocated_bytes += memory_bytes
            level_info.peak_usage = max(level_info.peak_usage, level_info.total_allocated_bytes)
            level_info.blocks[membrane_id] = block
            self.stats['total_allocations'] += 1
            self.stats['current_memory_usage'] += memory_bytes
            self.stats['peak_memory_usage'] = max(self.stats['peak_memory_usage'], self.stats['current_memory_usage'])
            if block.oeis_compliant:
                self.stats['oeis_compliant_allocations'] += 1
            if self.embodied_memory:
                self._record_embodied_allocation(block, memory_type)
            return tensor
        except Exception as e:
            logger.error(f'Failed to allocate tensor at DTESN level {level}: {e}')
            self.stats['allocation_failures'] += 1
            return None
    def _align_tensor_shape(self, shape: Tuple[int, ...], level: int) -> Tuple[int, ...]:
        if level == 0:
            return shape
        alignment_factor = 2 ** min(level, 4)
        aligned_shape = list(shape)
        if aligned_shape:
            last_dim = aligned_shape[-1]
            aligned_last = (last_dim + alignment_factor - 1) // alignment_factor * alignment_factor
            aligned_shape[-1] = aligned_last
        return tuple(aligned_shape)
    def deallocate_tensor(self, tensor: torch.Tensor, force: bool=False):
        with self._lock:
            block_id = None
            block = None
            for bid, blk in self.allocated_blocks.items():
                if blk.tensor is tensor or (hasattr(blk.tensor, 'data_ptr') and hasattr(tensor, 'data_ptr') and (blk.tensor.data_ptr() == tensor.data_ptr())):
                    block_id = bid
                    block = blk
                    break
            if block is None:
                return
            block.ref_count -= 1
            if block.ref_count > 0:
                return
            if force or self._should_force_deallocate(block):
                self._force_deallocate_block(block_id, block)
            else:
                self._return_block_to_pool(block_id, block)
    def _should_force_deallocate(self, block: DTESNMemoryBlock) -> bool:
        if self.stats['current_memory_usage'] > self.total_memory_limit * self.memory_pressure_threshold:
            return True
        if time.time() - block.allocated_time > 3600:
            return True
        if block.size > self.total_memory_limit // 20:
            return True
        return False
    def _return_block_to_pool(self, block_id: str, block: DTESNMemoryBlock):
        if block_id in self.allocated_blocks:
            del self.allocated_blocks[block_id]
        pool_key = (block.level, block.size)
        self.free_blocks[pool_key].append(block)
        max_free_blocks = 8
        while len(self.free_blocks[pool_key]) > max_free_blocks:
            old_block = self.free_blocks[pool_key].popleft()
            self._force_deallocate_block(f'cleanup_{int(time.time())}', old_block)
    def _force_deallocate_block(self, block_id: str, block: DTESNMemoryBlock):
        level_info = self.levels[block.level]
        level_info.total_allocated_bytes -= block.size
        level_info.allocated_membranes = max(0, level_info.allocated_membranes - 1)
        if block.membrane_id in level_info.blocks:
            del level_info.blocks[block.membrane_id]
        self.stats['total_deallocations'] += 1
        self.stats['current_memory_usage'] -= block.size
        if block_id in self.allocated_blocks:
            del self.allocated_blocks[block_id]
        block.tensor = None
    def _check_memory_pressure(self) -> bool:
        current_usage = self.stats['current_memory_usage']
        return current_usage > self.total_memory_limit * self.memory_pressure_threshold
    def _consolidate_memory(self):
        consolidation_count = 0
        for level in range(self.max_hierarchy_depth):
            level_blocks = [(k, v) for k, v in self.free_blocks.items() if k[0] == level]
            if len(level_blocks) > 4:
                for pool_key, block_deque in level_blocks:
                    while len(block_deque) > 2:
                        old_block = block_deque.popleft()
                        self._force_deallocate_block(f'consolidate_{int(time.time())}', old_block)
                        consolidation_count += 1
        self.stats['memory_consolidations'] += consolidation_count
        if consolidation_count > 0:
            logger.info(f'Consolidated {consolidation_count} memory blocks')
    def _force_memory_cleanup(self):
        logger.warning('DTESN memory manager under severe pressure, forcing cleanup')
        freed_count = 0
        for pool_key, block_deque in list(self.free_blocks.items()):
            while block_deque:
                block = block_deque.popleft()
                self._force_deallocate_block(f'force_cleanup_{int(time.time())}', block)
                freed_count += 1
        self.free_blocks.clear()
        logger.warning(f'Force cleanup freed {freed_count} blocks')
    def _record_embodied_allocation(self, block: DTESNMemoryBlock, memory_type: str):
        if not self.embodied_memory:
            return
        try:
            content = f'DTESN allocation: level={block.level}, membrane={block.membrane_id}, size={block.size}'
            mem_type = getattr(MemoryType, memory_type.upper(), MemoryType.PROCEDURAL)
            self.embodied_memory.create_memory(content, mem_type)
        except Exception as e:
            logger.debug(f'Failed to record embodied allocation: {e}')
    def get_memory_stats(self) -> Dict[str, Any]:
        with self._lock:
            stats = {'global_stats': self.stats.copy(), 'memory_usage': {'total_limit_mb': self.total_memory_limit / (1024 * 1024), 'current_usage_mb': self.stats['current_memory_usage'] / (1024 * 1024), 'peak_usage_mb': self.stats['peak_memory_usage'] / (1024 * 1024), 'utilization': self.stats['current_memory_usage'] / self.total_memory_limit, 'pressure_threshold': self.memory_pressure_threshold}, 'dtesn_levels': {}, 'free_pool_stats': {'total_free_blocks': sum((len(deque) for deque in self.free_blocks.values())), 'free_pools': len(self.free_blocks)}}
            for level, level_info in self.levels.items():
                stats['dtesn_levels'][f'level_{level}'] = {'expected_membranes': level_info.expected_membranes, 'allocated_membranes': level_info.allocated_membranes, 'utilization': level_info.allocated_membranes / max(1, level_info.expected_membranes), 'total_allocated_mb': level_info.total_allocated_bytes / (1024 * 1024), 'peak_usage_mb': level_info.peak_usage / (1024 * 1024), 'active_blocks': len(level_info.blocks), 'oeis_term': self.OEIS_A000081[level] if level < len(self.OEIS_A000081) else 0}
            if self.embodied_memory:
                stats['embodied_memory'] = self.embodied_memory.get_stats()
            if self.dtesn_validator:
                is_valid, errors = self.dtesn_validator.validate_full_layout()
                stats['dtesn_validation'] = {'layout_valid': is_valid, 'error_count': len(errors) if not is_valid else 0}
            return stats
    def clear_all_memory(self):
        with self._lock:
            for block_id, block in list(self.allocated_blocks.items()):
                self._force_deallocate_block(block_id, block)
            for pool_key, block_deque in self.free_blocks.items():
                while block_deque:
                    block = block_deque.popleft()
                    self._force_deallocate_block(f'clear_all_{int(time.time())}', block)
            self.allocated_blocks.clear()
            self.free_blocks.clear()
            for level_info in self.levels.values():
                level_info.allocated_membranes = 0
                level_info.total_allocated_bytes = 0
                level_info.blocks.clear()
            self.stats = {'total_allocations': 0, 'total_deallocations': 0, 'memory_consolidations': 0, 'oeis_compliant_allocations': 0, 'peak_memory_usage': 0, 'current_memory_usage': 0, 'allocation_failures': 0}
            logger.info('DTESN memory manager cleared and reset')
_global_dtesn_manager: Optional[DTESNMemoryManager] = None
_dtesn_lock = threading.Lock()
def get_dtesn_memory_manager(total_memory_limit: Optional[int]=None, max_hierarchy_depth: int=8) -> DTESNMemoryManager:
    global _global_dtesn_manager
    with _dtesn_lock:
        if _global_dtesn_manager is None:
            memory_limit = total_memory_limit or 8 * 1024 * 1024 * 1024
            _global_dtesn_manager = DTESNMemoryManager(total_memory_limit=memory_limit, max_hierarchy_depth=max_hierarchy_depth, enable_embodied_memory=_HAS_DTESN_CORE)
            logger.info(f'Initialized global DTESN memory manager (limit={memory_limit / 1024 ** 3:.2f}GB, depth={max_hierarchy_depth})')
        return _global_dtesn_manager
def reset_dtesn_memory_manager():
    global _global_dtesn_manager
    with _dtesn_lock:
        if _global_dtesn_manager is not None:
            _global_dtesn_manager.clear_all_memory()
            _global_dtesn_manager = None