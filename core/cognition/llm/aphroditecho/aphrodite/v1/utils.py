import argparse
import multiprocessing
import time
import weakref
from collections.abc import Sequence
from multiprocessing import connection
from multiprocessing.process import BaseProcess
from typing import TYPE_CHECKING, Any, Callable, Generic, Optional, TypeVar, Union, overload
import torch
from loguru import logger
from aphrodite.usage.usage_lib import UsageContext, is_usage_stats_enabled, usage_message
from aphrodite.utils import get_open_port, get_open_zmq_ipc_path, get_tcp_uri, kill_process_tree
if TYPE_CHECKING:
    from aphrodite.v1.engine.coordinator import DPCoordinator
    from aphrodite.v1.engine.utils import CoreEngineActorManager, CoreEngineProcManager
T = TypeVar('T')
class ConstantList(Generic[T], Sequence):
    def __init__(self, x: list[T]) -> None:
        self._x = x
    def append(self, item):
        raise TypeError('Cannot append to a constant list')
    def extend(self, item):
        raise TypeError('Cannot extend a constant list')
    def insert(self, item):
        raise TypeError('Cannot insert into a constant list')
    def pop(self, item):
        raise TypeError('Cannot pop from a constant list')
    def remove(self, item):
        raise TypeError('Cannot remove from a constant list')
    def clear(self):
        raise TypeError('Cannot clear a constant list')
    def index(self, item: T, start: int=0, stop: Optional[int]=None) -> int:
        return self._x.index(item, start, stop if stop is not None else len(self._x))
    @overload
    def __getitem__(self, item: int) -> T:
        ...
    @overload
    def __getitem__(self, s: slice, /) -> list[T]:
        ...
    def __getitem__(self, item: Union[int, slice]) -> Union[T, list[T]]:
        return self._x[item]
    @overload
    def __setitem__(self, item: int, value: T):
        ...
    @overload
    def __setitem__(self, s: slice, value: T, /):
        ...
    def __setitem__(self, item: Union[int, slice], value: Union[T, list[T]]):
        raise TypeError('Cannot set item in a constant list')
    def __delitem__(self, item):
        raise TypeError('Cannot delete item from a constant list')
    def __iter__(self):
        return iter(self._x)
    def __contains__(self, item):
        return item in self._x
    def __len__(self):
        return len(self._x)
    def __repr__(self):
        return f'ConstantList({self._x})'
def get_engine_client_zmq_addr(local_only: bool, host: str, port: int=0) -> str:
    return get_open_zmq_ipc_path() if local_only else get_tcp_uri(host, port or get_open_port())
class APIServerProcessManager:
    def __init__(self, target_server_fn: Callable, listen_address: str, sock: Any, args: argparse.Namespace, num_servers: int, input_addresses: list[str], output_addresses: list[str], stats_update_address: Optional[str]=None):
        self.listen_address = listen_address
        self.sock = sock
        self.args = args
        spawn_context = multiprocessing.get_context('spawn')
        self.processes: list[BaseProcess] = []
        for i, in_addr, out_addr in zip(range(num_servers), input_addresses, output_addresses):
            client_config = {'input_address': in_addr, 'output_address': out_addr, 'client_count': num_servers, 'client_index': i}
            if stats_update_address is not None:
                client_config['stats_update_address'] = stats_update_address
            proc = spawn_context.Process(target=target_server_fn, name=f'ApiServer_{i}', args=(listen_address, sock, args, client_config))
            self.processes.append(proc)
            proc.start()
        logger.info('Started {} API server processes', len(self.processes))
        self._finalizer = weakref.finalize(self, shutdown, self.processes)
    def close(self) -> None:
        self._finalizer()
def wait_for_completion_or_failure(api_server_manager: APIServerProcessManager, engine_manager: Optional[Union['CoreEngineProcManager', 'CoreEngineActorManager']]=None, coordinator: Optional['DPCoordinator']=None) -> None:
    from aphrodite.v1.engine.utils import CoreEngineActorManager, CoreEngineProcManager
    try:
        logger.info('Waiting for API servers to complete ...')
        sentinel_to_proc: dict[Any, BaseProcess] = {proc.sentinel: proc for proc in api_server_manager.processes}
        if coordinator:
            sentinel_to_proc[coordinator.proc.sentinel] = coordinator.proc
        actor_run_refs = []
        if isinstance(engine_manager, CoreEngineProcManager):
            for proc in engine_manager.processes:
                sentinel_to_proc[proc.sentinel] = proc
        elif isinstance(engine_manager, CoreEngineActorManager):
            actor_run_refs = engine_manager.get_run_refs()
        while sentinel_to_proc or actor_run_refs:
            ready_sentinels: list[Any] = connection.wait(sentinel_to_proc, timeout=5)
            for sentinel in ready_sentinels:
                proc = sentinel_to_proc.pop(sentinel)
                if proc.exitcode != 0:
                    raise RuntimeError(f'Process {proc.name} (PID: {proc.pid}) died with exit code {proc.exitcode}')
            if actor_run_refs:
                import ray
                _, actor_run_refs = ray.wait(actor_run_refs, timeout=5)
    except KeyboardInterrupt:
        logger.info('Received KeyboardInterrupt, shutting down API servers...')
    except Exception as e:
        logger.exception('Exception occurred while running API servers: {}', str(e))
        raise
    finally:
        logger.info('Terminating remaining processes ...')
        api_server_manager.close()
        if coordinator:
            coordinator.close()
        if engine_manager:
            engine_manager.close()
def shutdown(procs: list[BaseProcess]):
    for proc in procs:
        if proc.is_alive():
            proc.terminate()
    deadline = time.monotonic() + 5
    for proc in procs:
        remaining = deadline - time.monotonic()
        if remaining <= 0:
            break
        if proc.is_alive():
            proc.join(remaining)
    for proc in procs:
        if proc.is_alive() and (pid := proc.pid) is not None:
            kill_process_tree(pid)
def copy_slice(from_tensor: torch.Tensor, to_tensor: torch.Tensor, length: int) -> torch.Tensor:
    return to_tensor[:length].copy_(from_tensor[:length], non_blocking=True)
def report_usage_stats(aphrodite_config, usage_context: UsageContext=UsageContext.ENGINE_CONTEXT) -> None:
    if not is_usage_stats_enabled():
        return
    from aphrodite.modeling.model_loader import get_architecture_class_name
    usage_message.report_usage(get_architecture_class_name(aphrodite_config.model_config), usage_context, extra_kvs={'dtype': str(aphrodite_config.model_config.dtype), 'tensor_parallel_size': aphrodite_config.parallel_config.tensor_parallel_size, 'block_size': aphrodite_config.cache_config.block_size, 'gpu_memory_utilization': aphrodite_config.cache_config.gpu_memory_utilization, 'quantization': aphrodite_config.model_config.quantization, 'kv_cache_dtype': str(aphrodite_config.cache_config.cache_dtype), 'enable_lora': bool(aphrodite_config.lora_config), 'enable_prefix_caching': aphrodite_config.cache_config.enable_prefix_caching, 'enforce_eager': aphrodite_config.model_config.enforce_eager, 'disable_custom_all_reduce': aphrodite_config.parallel_config.disable_custom_all_reduce})