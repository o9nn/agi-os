import dataclasses
import gc
import os
from contextlib import contextmanager
from typing import Any, Callable, Optional, Union
import torch
from aphrodite.utils import is_pin_memory_available
def find_loaded_library(lib_name) -> Optional[str]:
    found_line = None
    with open('/proc/self/maps') as f:
        for line in f:
            if lib_name in line:
                found_line = line
                break
    if found_line is None:
        return None
    start = found_line.index('/')
    path = found_line[start:].strip()
    filename = path.split('/')[-1]
    assert filename.rpartition('.so')[0].startswith(lib_name), f'Unexpected filename: {filename} for library {lib_name}'
    return path
cumem_available = False
try:
    from aphrodite.cumem_allocator import init_module, python_create_and_map, python_unmap_and_release
    from aphrodite.distributed.device_communicators.cuda_wrapper import CudaRTLibrary
    lib_name = find_loaded_library('cumem_allocator')
    libcudart = CudaRTLibrary()
    cumem_available = True
except ModuleNotFoundError:
    init_module = None
    python_create_and_map = None
    python_unmap_and_release = None
    CudaRTLibrary = None
    lib_name = None
    libcudart = None
HandleType = tuple[int, int, int, int]
@dataclasses.dataclass
class AllocationData:
    handle: HandleType
    tag: str
    cpu_backup_tensor: Optional[torch.Tensor] = None
def create_and_map(allocation_handle: HandleType) -> None:
    python_create_and_map(*allocation_handle)
def unmap_and_release(allocation_handle: HandleType) -> None:
    python_unmap_and_release(*allocation_handle)
def get_pluggable_allocator(python_malloc_fn: Callable[[int], int], python_free_func: Callable[[int, int], None]) -> torch.cuda.memory.CUDAPluggableAllocator:
    init_module(python_malloc_fn, python_free_func)
    new_alloc = torch.cuda.memory.CUDAPluggableAllocator(lib_name, 'my_malloc', 'my_free')
    return new_alloc
@contextmanager
def use_memory_pool_with_allocator(python_malloc_fn: Callable[[int], int], python_free_func: Callable[[int, int], None]) -> None:
    new_alloc = get_pluggable_allocator(python_malloc_fn, python_free_func)
    mem_pool = torch.cuda.memory.MemPool(new_alloc._allocator)
    with torch.cuda.memory.use_mem_pool(mem_pool):
        yield (mem_pool, new_alloc)
class CuMemAllocator:
    instance: 'CuMemAllocator' = None
    default_tag: str = 'default'
    @staticmethod
    def get_instance() -> 'CuMemAllocator':
        assert cumem_available, 'cumem allocator is not available'
        if CuMemAllocator.instance is None:
            CuMemAllocator.instance = CuMemAllocator()
        return CuMemAllocator.instance
    def __init__(self):
        conf = os.environ.get('PYTORCH_CUDA_ALLOC_CONF', '')
        assert 'expandable_segments:True' not in conf, 'Expandable segments are not compatible with memory pool. Please track https://github.com/pytorch/pytorch/issues/147851 for the latest updates.'
        self.pointer_to_data: dict[int, AllocationData] = {}
        self.current_tag: str = CuMemAllocator.default_tag
        self.allocator_and_pools: dict[str, Any] = {}
    def python_malloc_callback(self, allocation_handle: HandleType) -> None:
        py_d_mem = allocation_handle[2]
        self.pointer_to_data[py_d_mem] = AllocationData(allocation_handle, self.current_tag)
        return
    def python_free_callback(self, ptr: int) -> HandleType:
        data = self.pointer_to_data.pop(ptr)
        if data.cpu_backup_tensor is not None:
            data.cpu_backup_tensor = None
        return data.handle
    def sleep(self, offload_tags: Optional[Union[tuple[str, ...], str]]=None) -> None:
        if offload_tags is None:
            offload_tags = (CuMemAllocator.default_tag,)
        elif isinstance(offload_tags, str):
            offload_tags = (offload_tags,)
        assert isinstance(offload_tags, tuple)
        for ptr, data in self.pointer_to_data.items():
            handle = data.handle
            if data.tag in offload_tags:
                size_in_bytes = handle[1]
                cpu_backup_tensor = torch.empty(size_in_bytes, dtype=torch.uint8, device='cpu', pin_memory=is_pin_memory_available())
                cpu_ptr = cpu_backup_tensor.data_ptr()
                libcudart.cudaMemcpy(cpu_ptr, ptr, size_in_bytes)
                data.cpu_backup_tensor = cpu_backup_tensor
            unmap_and_release(handle)
        gc.collect()
        torch.cuda.empty_cache()
    def wake_up(self, tags: Optional[list[str]]=None) -> None:
        for ptr, data in self.pointer_to_data.items():
            if tags is None or data.tag in tags:
                handle = data.handle
                create_and_map(handle)
                if data.cpu_backup_tensor is not None:
                    cpu_backup_tensor = data.cpu_backup_tensor
                    if cpu_backup_tensor is not None:
                        size_in_bytes = cpu_backup_tensor.numel() * cpu_backup_tensor.element_size()
                        cpu_ptr = cpu_backup_tensor.data_ptr()
                        libcudart.cudaMemcpy(ptr, cpu_ptr, size_in_bytes)
                        data.cpu_backup_tensor = None
    @contextmanager
    def use_memory_pool(self, tag: Optional[str]=None):
        if tag is None:
            tag = CuMemAllocator.default_tag
        assert isinstance(tag, str)
        old_tag = self.current_tag
        self.current_tag = tag
        with use_memory_pool_with_allocator(self.python_malloc_callback, self.python_free_callback) as data:
            self.allocator_and_pools[tag] = data
            yield
            self.current_tag = old_tag
    def get_current_usage(self) -> int:
        sum_bytes: int = 0
        for ptr, data in self.pointer_to_data.items():
            handle = data.handle
            sum_bytes += handle[1]
        return sum_bytes