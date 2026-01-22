import asyncio
import time
from abc import ABC, abstractmethod
from typing import Any, Awaitable, Callable, Dict, List, Optional, Set, Tuple, Union
from functools import cached_property
import torch.nn as nn
from loguru import logger
from typing_extensions import TypeVar
import aphrodite.platforms
from aphrodite.common.config import AphroditeConfig
from aphrodite.common.sequence import ExecuteModelRequest, PoolerOutput
from aphrodite.utils import make_async
from aphrodite.lora.request import LoRARequest
from aphrodite.modeling.layers.sampler import SamplerOutput
from aphrodite.worker.worker_base import WorkerBase
from aphrodite.tasks import SupportedTask
_R = TypeVar('_R', default=Any)
class ExecutorBase(ABC):
    uses_ray: bool
    supports_pp: bool = False
    def __init__(self, aphrodite_config: AphroditeConfig) -> None:
        self.aphrodite_config = aphrodite_config
        self.model_config = aphrodite_config.model_config
        self.cache_config = aphrodite_config.cache_config
        self.lora_config = aphrodite_config.lora_config
        self.load_config = aphrodite_config.load_config
        self.parallel_config = aphrodite_config.parallel_config
        self.scheduler_config = aphrodite_config.scheduler_config
        self.device_config = aphrodite_config.device_config
        self.speculative_config = aphrodite_config.speculative_config
        self.observability_config = aphrodite_config.observability_config
        self._init_executor()
        self.is_sleeping = False
        self.sleeping_tags: set[str] = set()
    @abstractmethod
    def _init_executor(self) -> None:
        raise NotImplementedError
    @abstractmethod
    def collective_rpc(self, method: Union[str, Callable[..., _R]], timeout: Optional[float]=None, args: Tuple=(), kwargs: Optional[Dict[str, Any]]=None) -> List[_R]:
        raise NotImplementedError
    def determine_num_available_blocks(self) -> Tuple[int, int]:
        results = self.collective_rpc('determine_num_available_blocks')
        a = min([r[0] for r in results])
        b = min([r[1] for r in results])
        return (a, b)
    def initialize_cache(self, num_gpu_blocks: int, num_cpu_blocks) -> None:
        logger.info('# {} blocks: {}, # CPU blocks: {}', aphrodite.platforms.current_platform.device_name, num_gpu_blocks, num_cpu_blocks)
        max_concurrency = num_gpu_blocks * self.cache_config.block_size / self.model_config.max_model_len
        logger.info('Maximum concurrency for {} tokens per request: {:.2f}x', self.model_config.max_model_len, max_concurrency)
        self.cache_config.num_gpu_blocks = num_gpu_blocks
        self.cache_config.num_cpu_blocks = num_cpu_blocks
        self.collective_rpc('initialize_cache', args=(num_gpu_blocks, num_cpu_blocks))
    def apply_model(self, func: Callable[[nn.Module], _R]) -> list[_R]:
        def rpc_func(worker: WorkerBase) -> _R:
            return func(worker.get_model())
        return self.collective_rpc(rpc_func)
    @cached_property
    def supported_tasks(self) -> tuple[SupportedTask, ...]:
        output = self.collective_rpc('get_supported_tasks')
        return output[0]
    def execute_model(self, execute_model_req: ExecuteModelRequest) -> Optional[List[Union[SamplerOutput, PoolerOutput]]]:
        output = self.collective_rpc('execute_model', args=(execute_model_req,))
        return output[0]
    def stop_remote_worker_execution_loop(self) -> None:
        return
    def add_lora(self, lora_request: LoRARequest) -> bool:
        assert lora_request.lora_int_id > 0, 'lora_id must be greater than 0.'
        return all(self.collective_rpc('add_lora', args=(lora_request,)))
    def remove_lora(self, lora_id: int) -> bool:
        assert lora_id > 0, 'lora_id must be greater than 0.'
        return all(self.collective_rpc('remove_lora', args=(lora_id,)))
    def pin_lora(self, lora_id: int) -> bool:
        assert lora_id > 0, 'lora_id must be greater than 0.'
        return all(self.collective_rpc('pin_lora', args=(lora_id,)))
    def list_loras(self) -> Set[int]:
        sets = self.collective_rpc('list_loras')
        for s in sets:
            assert s == sets[0], 'All workers should have the same LORAs.'
        return sets[0]
    def start_profile(self) -> None:
        self.collective_rpc('start_profile')
    def stop_profile(self) -> None:
        self.collective_rpc('stop_profile')
    def sleep(self, level: int=1):
        if self.is_sleeping:
            logger.warning('Executor is already sleeping.')
            return
        time_before_sleep = time.perf_counter()
        self.collective_rpc('sleep', kwargs=dict(level=level))
        time_after_sleep = time.perf_counter()
        self.sleeping_tags = {'weights', 'kv_cache'}
        self.is_sleeping = True
        logger.info('It took {:.6f} seconds to fall asleep.', time_after_sleep - time_before_sleep)
    def wake_up(self, tags: Optional[list[str]]=None):
        if not self.is_sleeping:
            logger.warning('Executor is not sleeping.')
            return
        if tags:
            for tag in tags:
                if tag not in self.sleeping_tags:
                    logger.warning('Tag {} is not in sleeping tags {}', tag, self.sleeping_tags)
                    return
        time_before_wakeup = time.perf_counter()
        self.collective_rpc('wake_up', kwargs=dict(tags=tags))
        time_after_wakeup = time.perf_counter()
        logger.info('It took {:.6f} seconds to wake up tags {}.', time_after_wakeup - time_before_wakeup, tags if tags is not None else self.sleeping_tags)
        if tags:
            for tag in tags:
                self.sleeping_tags.remove(tag)
        else:
            self.sleeping_tags.clear()
        if not self.sleeping_tags:
            self.is_sleeping = False
    def save_sharded_state(self, path: str, pattern: Optional[str]=None, max_size: Optional[int]=None) -> None:
        self.collective_rpc('save_sharded_state', kwargs=dict(path=path, pattern=pattern, max_size=max_size))
    @abstractmethod
    def check_health(self) -> None:
        raise NotImplementedError
    def shutdown(self) -> None:
        return
    def __del__(self):
        self.shutdown()
    async def execute_model_async(self, execute_model_req: ExecuteModelRequest) -> List[SamplerOutput]:
        output = await make_async(self.execute_model)(execute_model_req)
        return output
    async def stop_remote_worker_execution_loop_async(self) -> None:
        return
    async def check_health_async(self) -> None:
        self.check_health()
class DistributedExecutorBase(ExecutorBase):
    def __init__(self, *args, **kwargs):
        self.parallel_worker_tasks: Optional[Union[Any, Awaitable[Any]]] = None
        super().__init__(*args, **kwargs)
    def execute_model(self, execute_model_req: ExecuteModelRequest) -> List[SamplerOutput]:
        if self.parallel_worker_tasks is None:
            self.parallel_worker_tasks = self._run_workers('start_worker_execution_loop', async_run_tensor_parallel_workers_only=True)
        driver_outputs = self._driver_execute_model(execute_model_req)
        assert driver_outputs is not None
        return driver_outputs
    def stop_remote_worker_execution_loop(self) -> None:
        if self.parallel_worker_tasks is None:
            return
        self._driver_execute_model(execute_model_req=None)
        parallel_worker_tasks = self.parallel_worker_tasks
        self.parallel_worker_tasks = None
        self._wait_for_tasks_completion(parallel_worker_tasks)
    @abstractmethod
    def _driver_execute_model(self, execute_model_req: Optional[ExecuteModelRequest]) -> Optional[List[SamplerOutput]]:
        raise NotImplementedError
    def collective_rpc(self, method: Union[str, Callable], timeout: Optional[float]=None, args: Tuple=(), kwargs: Optional[Dict]=None) -> List[Any]:
        return self._run_workers(method, *args, **kwargs or {})
    @abstractmethod
    def _run_workers(self, method: Union[str, Callable], *args, async_run_tensor_parallel_workers_only: bool=False, max_concurrent_workers: Optional[int]=None, **kwargs) -> Any:
        raise NotImplementedError
    @abstractmethod
    def _wait_for_tasks_completion(self, parallel_worker_tasks: Any) -> None:
        raise NotImplementedError
    async def execute_model_async(self, execute_model_req: ExecuteModelRequest) -> List[SamplerOutput]:
        if self.parallel_worker_tasks is None:
            self.parallel_worker_tasks = asyncio.create_task(self._start_worker_execution_loop())
        return await self._driver_execute_model_async(execute_model_req)
    async def stop_remote_worker_execution_loop_async(self) -> None:
        if self.parallel_worker_tasks is None:
            return
        await self._driver_execute_model_async()
        parallel_worker_tasks = self.parallel_worker_tasks
        self.parallel_worker_tasks = None
        await parallel_worker_tasks
    @abstractmethod
    async def _driver_execute_model_async(self, execute_model_req: Optional[ExecuteModelRequest]=None) -> List[SamplerOutput]:
        raise NotImplementedError
    @abstractmethod
    async def _start_worker_execution_loop(self):
        raise NotImplementedError